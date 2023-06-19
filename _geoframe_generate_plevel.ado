*! version 1.0.1  19jun2023  Ben Jann

program _geoframe_generate_plevel
    version 17
    syntax [name] [if] [in] [, replace noset ]
    if "`namelist'"=="" local namelist _PLEVEL
    local cframe `"`c(frame)'"'
    geoframe get type, l(type)
    marksample touse
    if "`type'"=="shape" local shpframe `"`cframe'"'
    else {
        geoframe get shpframe, local(shpframe) strict
        geoframe get linkname, local(lnkvar) strict
        frame `shpframe' {
            qui frget `touse' = `touse', from(`lnkvar')
            qui replace `touse' = 0 if `touse'>=.
        }
    }
    frame `shpframe' {
        local newvar 1               // variable does not exist
        capt confirm new variable `namelist'
        if _rc==1 exit _rc
        if _rc {
            capt confirm numeric variable `namelist'
            if _rc==1 exit _rc
            if _rc    local newvar 2 // variables exists and is string
            else      local newvar 0 // variables exists and is numeric
        }
        if "`replace'"!="" {
            if `newvar'==0 {
                local newvar 2       // replace variable even if conformable
            }
        }
        else {
            if `newvar'==2 {
                di as err "variable {bf:`namelist'} already exists and is"/*
                    */ " string; cannot update"
                exit 110
            }
        }
        geoframe get id, l(ID) strict
        geoframe get coordinates, l(XY) strict
        geoframe get pid, l(PID)
        if "`PID'"=="" {
            tempvar PID
            qui geoframe gen pid `PID', noset
        }
        tempname PL
        qui gen byte `PL' = 0
        mata: _st_plevel("`touse'")
        qui count if `PL'!=0 & (`ID'!=`ID'[_n-1] | `PID'!=`PID'[_n-1])
        di as txt "(`r(N)' nested polygons found)"
        if `newvar' {
            if `newvar'==2 drop `namelist'
            rename `PL' `namelist'
            di as txt "(variable {bf:`namelist'} added to frame {bf:`shpframe'})"
        }
        else {
            qui replace `namelist' = `PL' if `touse'
            di as txt "(variable {bf:`namelist'} updated in frame {bf:`shpframe'})"
        }
        if "`set'"=="" geoframe set plevel `namelist'
    }
end

version 17
mata:
mata set matastrict on

struct unit {
    real scalar n       // number of polygons within unit
    pointer (struct polygon) vector p
}

struct polygon {
    real scalar a, b    // data range of polygon
    real scalar l       // plot level
    real matrix XY
    real scalar xmin, xmax, ymin, ymax
    real colvector d    // list of descendants
}

void _st_plevel(string scalar touse)
{
    real scalar    i, j
    real colvector ID, PID, PL
    real matrix    XY
    pointer (struct unit) vector u

    // collect units and polygons
    st_view(ID=.,  ., st_local("ID"), touse)
    st_view(PID=., ., st_local("PID"), touse)
    st_view(XY=.,  ., st_local("XY"), touse)
    u = _read_units(ID, PID, XY)
    // determine within unit plot levels
    i = length(u)
    for (;i;i--) _plevel_within_u(*u[i])
    // update plot levels based on between unit comparisons
    i = length(u)
    for (;i;i--) {
        for (j=i-1; j; j--) _plevel_between_u(*u[i], *u[j])
    }
    // store plevel
    st_view(PL=.,  ., st_local("PL"), touse)
    i = length(u)
    for (;i;i--) _write_plevel(PL, *u[i])
}

pointer (struct unit) vector _read_units(real colvector ID,
    real colvector PID, real matrix XY)
{
    real scalar    i
    real colvector a, b
    pointer (struct unit) vector u
    
    a = selectindex(_mm_unique_tag(ID))
    b = selectindex(_mm_unique_tag(ID, 1))
    i = rows(a)
    u = J(i,1,NULL)
    for (;i;i--) u[i] = &_read_unit(a[i], b[i], PID, XY)
    return(u)
}

struct unit scalar _read_unit(real scalar a, real scalar b,
    real colvector ID, real matrix XY)
{
    real scalar        i
    real colvector     i1, i2, id
    struct unit scalar u
    
    id  = ID[|a \ b|]
    i1  = selectindex(_mm_unique_tag(id))    :- 1
    i2  = selectindex(_mm_unique_tag(id, 1)) :- 1
    u.n = i = rows(i1)
    u.p = J(u.n, 1, NULL)
    for (;i;i--) {
        u.p[i] = &_read_polygon(a+i1[i], a+i2[i], XY)
    }
    return(u)
}

struct polygon scalar _read_polygon(real scalar a, real scalar b,
    real matrix XY)
{
    real matrix minmax
    struct polygon scalar p
    
    p.a  = a
    p.b  = b
    p.l  = 0
    p.XY = XY[|a,1 \ b,2|]
    p.XY = select(p.XY, !rowmissing(p.XY)) // exclude missing points
    minmax = colminmax(p.XY)
    p.xmin = minmax[1,1]
    p.xmax = minmax[2,1]
    p.ymin = minmax[1,2]
    p.ymax = minmax[2,2]
    return(p)
}

void _write_plevel(real colvector PL, struct unit scalar u)
{
    real scalar i, a, b
    
    i = u.n
    for (;i;i--) {
        a = u.p[i]->a
        b = u.p[i]->b
        PL[|a \ b|] = J(b-a+1, 1, u.p[i]->l)
    }
}

void _plevel_within_u(struct unit scalar u)
{
    real scalar i, j
    pointer (struct polygon) scalar pi, pj

    i = u.n
    for (;i;i--) {
        pi = u.p[i]
        for (j=i-1; j; j--) {
            pj = u.p[j]
            if (pi->ymax < pj->ymin) continue
            if (pi->xmax < pj->xmin) continue
            if (pj->ymax < pi->ymin) continue
            if (pj->xmax < pi->xmin) continue
            if (!_amongdescendants(u, i, j)) {
                if (_notinside(pi->XY, pj->XY)) {
                    if (!_amongdescendants(u, j, i)) {
                        if (_notinside(pj->XY, pi->XY)) continue
                    }
                    // pj is inside pi
                    pj->l = pj->l + 1
                    pi->d = pi->d \ j
                    continue
                }
            }
            // pi is inside pj
            pi->l = pi->l + 1
            pj->d = pj->d \ i
        }
    }
}

real scalar _amongdescendants(struct unit scalar u, real scalar i,
    real scalar j)
{
    real scalar    ji
    real colvector d
    
    d = u.p[j]->d
    ji = rows(d)
    for (;ji;ji--) {
        if (i==d[ji]) return(1)
        if (_amongdescendants(u, i, d[ji])) return(1)
    }
    return(0)
}

void _plevel_between_u(struct unit scalar ui, struct unit scalar uj)
{
    real scalar i, j, l
    pointer (struct polygon) scalar pi, pj
    
    // update plot level
    i = ui.n
    for (;i;i--) {
        pi = ui.p[i]
        l  = pi->l
        j  = uj.n
        for (;j;j--) {
            pj = uj.p[j]
            if (mod(l,2)!=mod(pj->l, 2)) continue
            if (pi->ymax < pj->ymin) continue
            if (pi->xmax < pj->xmin) continue
            if (pj->ymax < pi->ymin) continue
            if (pj->xmax < pi->xmin) continue
            if (_notinside(pi->XY, pj->XY)) {
                if (_notinside(pj->XY, pi->XY)) continue
                // pj is in pi; increase plot level by 2
                pj->l = pj->l + 2
                continue
            }
            // pi is inside pj; increase plot level by 2
            pi->l = pi->l + 2
        }
    }
}

real scalar _notinside(real matrix xy, real matrix XY)
{   // check if at least one point of XY is outside XY
    real scalar i
    real scalar px, py

    i = rows(xy)
    for (;i;i--) {
        px = xy[i,1]; py = xy[i,2]
        if (!_pointinpolygon(px, py, XY)) return(1)
    }
    return(0)
}

// the following functions are based on
// https://rosettacode.org/wiki/Ray-casting_algorithm

real scalar _pointinpolygon(real scalar px, real scalar py, real matrix XY)
{
    real scalar i, c
    real scalar ax, ay, bx, by
    pragma unset bx
    pragma unset by
    
    c = 0
    i = rows(XY)
    ax = XY[i,1]; ay = XY[i,2]
    i--
    for (;i;i--) {
        swap(bx, ax); swap(by, ay)
        ax = XY[i,1]; ay = XY[i,2]
        if (ay>by) {
            if (_rayintersectseg(px, py, bx, by, ax, ay)) c++
        }
        else {
            if (_rayintersectseg(px, py, ax, ay, bx, by)) c++
        }
    }
    return(mod(c,2))
}

real scalar _rayintersectseg(real scalar px, real scalar py,
    real scalar ax, real scalar ay, real scalar bx, real scalar by)
{
    real scalar eps
    real scalar m_red, m_blue
    
    eps = 0.00001
    if (py==ay | py==by)    py = py + eps      // ok to modify permanently?
    if (py>by | py<ay)      return(0)
    if (px > max((ax, bx))) return(0)
    if (px < min((ax, bx))) return(1)
    if (ax!=bx) m_red  = (by - ay) / (bx - ax) // need to allow tolerance?
    if (ax!=px) m_blue = (py - ay) / (px - ax) // need to allow tolerance?
    return(m_blue >= m_red)
}

end


