*! version 1.0.1  19jun2023  Ben Jann

program _geoframe_spjoin
    version 17
    syntax [namelist(min=1 max=2)] [, COordinates(varlist min=2 max=2)/*
        */ replace noset ]
    gettoken shpframe namelist : namelist
    gettoken id       namelist : namelist
    if "`id'"=="" local id _id
    if "`replace'"=="" confirm new variable `id'
    if `"`coordinates'"'!="" local xy `coordinates'
    else geoframe get coordinates, l(xy) strict
    local frame `"`c(frame)'"'
    frame `shpframe' {
        geoframe get ID, l(ID) strict
        local type: type `ID'
        geoframe get coordinates, l(XY) strict
        geoframe get pid, l(PID)
        if "`PID'"=="" {
            tempvar PID
            qui geoframe gen pid `PID', noset
        }
        geoframe get pl, l(PL)
        if "`PL'"!="" {
            qui levelsof `PL', local(pl)
            local r = r(r)
            if !`r' local PL
            else mata: st_local("pl", invtokens(tokens(st_local("pl"))[`r'..1]))
        }
    }
    tempvar tmp
    qui gen `type' `tmp' = .
    if "`PL'"=="" {
        di as txt "({helpb geoframe##gen_plevel:plevel} not set;"/*
            */ " assuming that there are no nested polygons)"
        frame `shpframe': mata: _st_spjoin(`"`frame'"')
    }
    else {
        tempname touse
        qui gen byte `touse' = .
        frame `shpframe': qui gen byte `touse' = .
        foreach l of local pl {
            if mod(`l',2) continue // skip enclaves
            qui replace `touse' = `tmp'>=.
            frame `shpframe' {
                qui replace `touse' = `PL'==`l' 
                mata: _st_spjoin(`"`frame'"', "`touse'")
            }
        }
    }
    qui count if `tmp'>=.
    if `r(N)' {
        di as txt "({bf:`r(N)'} points not matched)"
    }
    capt confirm new variable `id'
    if _rc==1 exit _rc
    if _rc drop `id'
    rename `tmp' `id'
    if "`set'"=="" {
        geoframe set id `id'
        if "`coordinates'"!="" geoframe set coordinates `yx'
    }
    di as txt "(variable {bf:`id'} added to frame {bf:`frame'})"
end

version 17
mata:
mata set matastrict on

struct polygon {
    real scalar id
    real matrix XY
    real scalar xmin, xmax, ymin, ymax
}

void _st_spjoin(string scalar frame, | string scalar touse)
{
    real scalar    i
    real colvector ID, PID
    real matrix    XY
    pointer (struct polygon) vector p
    
    // collect shapes
    st_view(ID=.,  ., st_local("ID"), touse)
    st_view(PID=., ., st_local("PID"), touse)
    st_view(XY=.,  ., st_local("XY"), touse)
    p = _read_polygons(ID, PID, XY)
    ID = PID = XY = . // clear views
    // perform spjoin
    st_framecurrent(frame)
    stata("qui di") // this fixes an issue with st_framecurrent()
    st_view(XY,  ., st_local("xy"), touse)
    st_view(ID,  ., st_local("tmp"), touse)
    i = rows(XY)
    for (;i;i--) ID[i] = _spjoin(XY[i,1], XY[i,2], p)
}

pointer (struct polygon) vector _read_polygons(real colvector ID,
    real colvector PID, real matrix XY)
{
    real scalar    i
    real colvector a, b
    pointer (struct polygon) vector p
    
    a = selectindex(_mm_uniqrows_tag((ID,PID)))
    b = selectindex(_mm_uniqrows_tag((ID,PID), 1))
    i = rows(a)
    p = J(i,1,NULL)
    for (;i;i--) p[i] = &_read_polygon(a[i], b[i], ID[a[i]], XY)
    return(p)
}

struct polygon scalar _read_polygon(real scalar a, real scalar b,
    real scalar id, real matrix XY)
{
    real matrix minmax
    struct polygon scalar p
    
    p.id = id
    p.XY = XY[|a,1 \ b,2|]
    p.XY = select(p.XY, !rowmissing(p.XY)) // exclude missing points
    minmax = colminmax(p.XY)
    p.xmin = minmax[1,1]
    p.xmax = minmax[2,1]
    p.ymin = minmax[1,2]
    p.ymax = minmax[2,2]
    return(p)
}

real scalar _spjoin(real scalar px, real scalar py,
    pointer (struct polygon) vector p)
{
    real scalar i
    pointer (struct polygon) scalar pi
    
    i = length(p)
    for (;i;i--) {
        pi = p[i]
        if (px < pi->xmin) continue
        if (px > pi->xmax) continue
        if (py < pi->ymin) continue
        if (py > pi->ymax) continue
        if (_pointinpolygon(px, py, p[i]->XY)) return(p[i]->id)
    }
    return(.)
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


