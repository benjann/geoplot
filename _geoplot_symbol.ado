*! version 0.2.2  12jun2023  Ben Jann

program _geoplot_symbol
    version 17
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0, pars(", ")
    ***
    frame `frame' {
        // syntax
        qui syntax [varlist(default=none max=1 numeric)] [if] [in] [iw/] [,/*
            */ SHape(passthru) line/*
            */ SIze(str) OFFset(numlist max=2)/*
            */ ANGle(real 0) ratio(real 1) n(passthru)/*
            */ COLORVar(varname numeric) LEVels(str)/*
            */ COORdinates(varlist min=2 max=2)/*
            */ CENTRoids(str) area(str)/* (will be ignored)
            */ * ]
        // plottype
        if "`line'"!="" local plottype line
        else            local plottype area
        // parse shape() and n()
         _parse_shape, `n' `shape' // returns shape, arg, n
        // parse size etc.
        local scale .
        if `"`size'"'=="" local size .
        else if substr(`"`size'"',1,1)=="*" {
            local scale = substr(`"`size'"',2,.)
            local size .
        }
        capt n numlist `"`size' `scale'"', max(2) range(>=0) miss
        if _rc==1 exit 1
        if _rc {
            di as err "(error in size())"
            exit _rc
        }
        gettoken offset oangle : offset
        gettoken oangle: oangle
        if "`offset'"=="" local offset 0
        if "`oangle'"=="" local oangle 0
        // sample
        marksample touse
        tempname wvar
        if "`weight'"!="" {
            qui gen double `wvar' = abs(`exp') if `touse'
            markout `touse' `wvar'
            local wgt [iw=W]
        }
        else qui gen byte `wvar' = 1
        geoframe get id, local(ID)
        if "`ID'"!="" {
            // select one row per ID; assuming data is ordered by ID; assuming
            // Z and centroids are constant within ID
            qui replace `touse' = 0 if `ID'==`ID'[_n-1]
        }
        if "`coordinates'"!="" geoframe flip `coordinates', local(coordinates)
        else {
            geoframe get coordinates, strict flip local(coordinates)
            if `:list sizeof coordinates'!=2 {
                di as err "wrong number of coordinate variables"
                exit 498
            }
        }
        markout `touse' `coordinates' // include nonmissing coordinates only
        // extra variables to be copied
        if "`ID'"!="" {
            local ORG `ID' `wvar'
            local TGT _ID W
        }
        else {
            local ORG `wvar'
            local TGT W
        }
        if "`colorvar'"=="" local colorvar `varlist'
        if "`colorvar'"!="" {
            local zvar `colorvar'
            local options colorvar(ZVAR) `options'
            local ORG `ORG' `zvar'
            local TGT `TGT' ZVAR
            // get variable from levels(, weight())
            if `"`levels'"'!="" {
                _parse_levels `levels'
                local options levels(`levels') `options'
                if "`lwvar'"!="" {
                    local ORG `ORG' `lwvar'
                    local TGT `TGT' lW
                }
            }
        }
    }
    if `size'>=. {
        // get reference size of existing map
        su Y, meanonly
        local refsize = r(max) - r(min)
        su X, meanonly
        local refsize = min(`refsize', r(max) - r(min))
    }
    frame `frame' {
        // compute shapes and post coordinates into temporary frame
        tempname frame1
        frame create `frame1'
        mata: _compute_symbols("`frame1'", "`touse'", "`shape'",/*
            */ st_local("arg"), `n', `size', `scale', `angle', `ratio',/*
            */ `offset', `oangle')
    }
    ***
    __geoplot_layer 0 `plottype' `layer' `p' `frame1' `wgt', `options'
    c_local plot `plot'
    c_local p `p'
end

program _parse_shape
    syntax [, n(numlist int max=1 >0) SHape(str) ]
    if "`n'"=="" local n .
    // shape(numlist)
    capt numlist `"`shape'"'
    if _rc==1 exit 1
    if _rc==0 {
        c_local shape @numlist
        c_local arg   `r(numlist)'
        c_local n     .
        exit
    }
    // predefined shapes
    local SHAPES0 Triangle Square Pentagon HEXagon HEPtagon Octagon
    local shapes0 = strlower("`SHAPES0'")
    local SHAPES  `SHAPES0' Circle Arc SLice PENTAGRam HEXAGRam pin
    local shapes  = strlower("`SHAPES'")
    gettoken shape arg : shape
    local arg = strtrim(`"`arg'"')
    local 0, `shape'
    capt syntax [, `SHAPES' ]
    if _rc==1 exit 1
    if _rc==0 {
        local shape
        foreach s of local shapes {
            if "``s''"!="" {
                local shape `s'
                continue, break
            }
        }
        if "`shape'"=="" local shape circle // default
        else if `: list shape in shapes0' {
            local n: list posof "`shape'" in shapes
            local n = `n' + 2
            local shape circle
        }
        c_local shape `shape'
        c_local arg   `"`arg'"'
        c_local n     `n'
        exit
    }
    // matrix
    capt confirm matrix `shape'
    if _rc==1 exit 1
    if _rc==0 {
        c_local shape @matrix
        c_local arg   `shape'
        c_local n     .
        exit
    }
    // mata function (potentially)
    capt confirm name _`shape'
    if _rc==1 exit 1
    if _rc {
        di as err "invalid specification in shape()"
        exit 198
    }
    c_local shape `shape'
    c_local arg   `"`arg'"'
    c_local n     `n'
end

program _parse_levels
    _parse comma lhs 0 : 0
    syntax [, Weight(varname numeric) * ]
    if "`weight'"!="" {
        local options weight(lW) `options'
        c_local levels `"`lhs', `options'"'
        c_local lwvar `weight'
    }
end

version 17
mata:
mata set matastrict on

void _compute_symbols(string scalar frame, string scalar touse,
    string scalar shape, string scalar arg, real scalar n,
    real scalar size, real scalar scale, real scalar angle, real scalar ratio,
    real scalar off, real scalar oang)
{
    real scalar      i, a, b, s, haspl
    real matrix      YX, Z, yx, V
    real colvector   pl, PL
    string rowvector ORG, TGT
    pointer matrix   INFO
    pointer(real matrix function) scalar f
    
    // get origin data
    ORG  = tokens(st_local("ORG"))
    YX   = st_data(., st_local("coordinates"), touse)
    Z    = st_data(., ORG, touse)
    INFO = _fmtvl(ORG) // collect formats and labels
    i    = rows(YX)
    // obtain shape
    haspl = 0
    if (shape=="@numlist") {
        yx = strtoreal(tokens(arg))
        n  = length(yx) / 2
        if (n!=trunc(n)) {
            errprintf("shape({it:numlist}): even number of values required\n")
            exit(503)
        }
        yx = colshape(yx, 2)
    }
    else if (shape=="@matrix") {
        yx = st_matrix(arg)
        n  = length(yx) / 2
        if (n!=trunc(n)) {
            errprintf("shape({it:matname}): even number of values required\n")
            exit(503)
        }
        if (cols(yx)!=2) yx = colshape(yx', 2)
    }
    else {
        f = findexternal("__geoplot_symbol_"+shape+"()")
        if (f==NULL) {
            errprintf("function for symbol '%s' not found; error in shape()\n",
                shape)
            exit(111)
        }
        yx = (*f)(n, arg)
        if (cols(yx)>2) {
            haspl = 1
            pl = yx[,3]
            pl = pl[1] \ pl // duplicate 1st element
            yx = yx[,(1,2)]
        }
    }
    n = rows(yx)
    // determine size and apply to shape
    if (size<.) s = size
    else {
        s = min(mm_coldiff(colminmax(YX)))
        s = max((s,strtoreal(st_local("refsize"))))
        s = max((1, s * 0.03)) // 3% of min(yrange, xrange) of map
    }
    if (scale<.) s = s * scale
    yx = yx * s
    // apply ratio to shape
    if (ratio!=1) yx = yx[,1]*ratio, yx[,2]
    // apply angle to shape
    _rotate(yx, angle)
    // apply offset to centroids
    if (off) YX = YX :+ s*(off/100)*(sin(oang*pi()/180), cos(oang*pi()/180))
    // prepare destination data
    st_framecurrent(frame)
    TGT = tokens(st_local("TGT"))
    st_addobs(i * (n+1))
    st_view(V=., ., st_addvar("double", (tokens("_Y _X _CY _CX"), TGT)))
    _fmtvl(TGT, INFO)
    if (haspl) st_view(PL=., ., st_addvar("byte", "_PLEVEL"))
    // write shapes to destination data
    a = i * (n+1) + 1
    for (;i;i--) {
        b = a - 1
        a = b - n
        V[|a,1 \ b,.|] =
            (J(1,2,.) \ (YX[i,] :+ yx)), J(n+1, 1, (YX[i,], Z[i,]))
        if (haspl) PL[|a \ b|] = pl
    }
}

pointer matrix _fmtvl(string rowvector V, | pointer matrix INFO)
{
    real scalar      i
    string scalar    vl
    real colvector   values
    string colvector text
    pragma unset values
    pragma unset text
    
    i = length(V)
    if (args()==2) {
        for (;i;i--) {
            st_varformat(V[i], *INFO[i,1])
            st_varlabel(V[i], *INFO[i,2])
            if (INFO[i,3]!=NULL) {
                st_vlmodify(V[i], *((*INFO[i,3])[1]), *((*INFO[i,3])[2]))
                st_varvaluelabel(V[i], V[i])
            }
        }
        return
    }
    INFO = J(i,3,NULL)
    for (;i;i--) {
        INFO[i,1] = &st_varformat(V[i])
        INFO[i,2] = &st_varlabel(V[i])
        vl = st_varvaluelabel(V[i])
        if (vl=="") continue
        if (!st_vlexists(vl)) continue
        st_vlload(vl, values, text)
        INFO[i,3] = &(&values,&text)
    }
    return(INFO)
}

void _rotate(yx, angle)
{
    real scalar r
    
    if (angle==0) return
    r  = angle * pi() / 180
    yx = yx[,2] * sin(r) + yx[,1] * cos(r), yx[,2] * cos(r) - yx[,1] * sin(r)
}


real matrix __geoplot_symbol_circle(real scalar n, | string scalar arg)
{
    real colvector r
    pragma unused arg
    
    if (n>=.) n = 100
    r = .25 - .5/n // set angle such that shape has horizontal edge at bottom
    r = rangen(-r, 1-r, n+1) * (2 * pi())
    return((sin(r), cos(r)))
}

real matrix __geoplot_symbol_slice(real scalar n, string scalar arg)
{
    return((0,0) \ __geoplot_symbol_arc(n, arg) \ (0,0))
}

real matrix __geoplot_symbol_arc(real scalar n, string scalar arg)
{
    real scalar    angle
    real colvector r
    
    if (arg=="") angle = 180 // default
    else {
        angle = strtoreal(arg)
        if (angle<-360 | angle>360) {
            errprintf("{it:angle} must be in [-360,360]; error in shape()\n")
            exit(125)
        }
    }
    if (n>=.) n = max((2, ceil(abs(angle)/360 * 100)))
    r = rangen(0, angle/180, n) * pi()
    return((sin(r), cos(r)))
}

real matrix __geoplot_symbol_pentagram(real scalar n, string scalar arg)
{
    real matrix YX, yx
    pragma unused n
    pragma unused arg
    
    // outer pentagon
    YX = __geoplot_symbol_circle(5)
    // inner pentagon
    yx = __geoplot_symbol_circle(5) / (.5*(1+sqrt(5)))^2
    _rotate(yx, 180)
    // merge
    YX = colshape((YX[1::5,], (yx[4::5,] \ yx[1::3,])), 2)
    YX = YX \ YX[1,] // last point = first point
    return(YX)
}

real matrix __geoplot_symbol_hexagram(real scalar n, string scalar arg)
{
    real matrix YX, yx
    pragma unused n
    pragma unused arg
    
    // outer hexagon
    YX = __geoplot_symbol_circle(6)
    _rotate(YX, 90)
    // inner hexagon
    yx = __geoplot_symbol_circle(6) * sqrt(1/3)
    // merge
    YX = colshape(((YX[5::6,] \ YX[1::4,]), yx[1::6,]), 2)
    YX = YX \ YX[1,] // last point = first point
    return(YX)
}

real matrix __geoplot_symbol_pin(real scalar n, string scalar arg)
{
    real colvector r, h
    real matrix    yx
    
    if (arg=="") h = 1/3 // default
    else {
        h = strtoreal(arg)
        if (h<0 | h>1) {
            errprintf("{it:headsize} must be in [0,1]; error in shape()\n")
            exit(125)
        }
    }
    if (n>=.) n = max((4, ceil(h * 100)))
    r = rangen(-.25, .75, n+1) * (2 * pi())
    yx = h * (sin(r), cos(r))
    yx[,1] = yx[,1] :+ (2-h)
    yx = yx \ (0,0) \ (2-2*h,0)
    return(yx)
}

real matrix __geoplot_symbol_pin2(real scalar n, string scalar arg)
{
    real colvector r, a, c
    real matrix    yx0, yx1
    
    if (arg=="") a = .6 // default
    else {
        a = strtoreal(arg)
        if (a<0 | a>1) {
            errprintf("{it:headsize} must be in [0,1]; error in shape()\n")
            exit(125)
        }
    }
    if (n>=.) n = max((4, ceil(a * 100)))
    // outer polygon
    c = 2 - a
    r = rangen(-asin(a/c), pi()+asin(a/c), n+1)
    yx0 = a * (sin(r), cos(r))
    yx0[,1] = yx0[,1] :+ c
    yx0 = yx0 \ (0,0) \ yx0[1,] 
    yx0 = yx0, J(rows(yx0),1,0) // plevel=0
    // inner polygon
    r = rangen(0, 1, n+1) * (2 * pi())
    yx1 = (0.5*a) * (sin(r), cos(r))
    yx1[,1] = yx1[,1] :+ c
    yx1 = (.,.) \ yx1
    yx1 = yx1, J(rows(yx1),1,1) // plevel=1
    return(yx0 \ yx1)
}


end

