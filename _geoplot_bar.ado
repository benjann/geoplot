*! version 0.2.2  12jun2023  Ben Jann

program _geoplot_bar
    version 17
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0
    ***
    frame `frame' {
        // syntax
        qui syntax varlist(numeric) [if] [in] [iw/] [,/*
            */ asis ANGle(real 0)/*
            */ SIze(str) ratio(real 2) OFFset(numlist max=2)/*
            */ noLABel box BOX2(str)/*
            */ COLORVar(str) LEVels(str) cuts(str) DISCRete/* (will be ignored)
            */ COORdinates(varlist numeric min=1 max=2) /*
            */ CENTRoids(str) area(str)/* (will be ignored)
            */ * ]
        if `"`box'`box2'"'!="" _parse_sym, `box2'
        else                   local sym 0
        // parse size
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
        else {
            qui gen byte `wvar' = 1
        }
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
        // collect labels
        local lbls
        local i 0
        foreach v of local varlist {
            local ++i
            if "`label'"!="" local lbl
            else             local lbl: var lab `v'
            if `"`lbl'"'=="" local lbl `v'
            local lbls `lbls' `i' `"`lbl'"'
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
        // compute slices of pies and post coordinates into temporary frame
        tempname frame1 SIZE
        frame create `frame1'
        mata: _compute_bars("`frame1'", "`touse'", `size', `scale',/*
            */ `angle', `ratio', `offset', `oangle', "`asis'"!="")
            // => makes frame1 the current frame
        // add labels
        lab def Z `lbls'
        lab val Z Z
    }
    ***
    if `sym'==1 local ++p
    __geoplot_layer 0 area `layer' `p' `frame1' `wgt',/*
        */ colorvar(Z) lock discrete `options'
    ***
    if `sym' {
        local PLOT: copy local plot
        // pass weights, size, etc. through to box
        if `sym_wgt' local sym_wgt `wgt'
        else         local sym_wgt
        if `offset' {
            if `"`sym_offset'"'=="" {
                local sym_offset offset(`offset' `oangle')
            }
        }
        if `angle' {
            if `"`sym_angle'"'=="" {
                local sym_angle angle(`angle')
            }
        }
        if `ratio' {
            if `"`sym_ratio'"'=="" {
                local sym_ratio ratio(`ratio')
            }
        }
        if substr(`"`sym_size'"',1,1)=="*" {
            local sym_size = substr(`"`sym_size'"',2,.)
            capt n numlist `"`sym_size'"', max(1) range(>=0)
            if _rc==1 exit 1
            if _rc {
                di as err "(error in size())"
                exit _rc
            }
           local sym_size = `SIZE' * `sym_size'
        }
        else if `"`sym_size'"'=="" local sym_size = `SIZE'
        local sym_size size(`sym_size')
        if !`sym_hascol' {
            // use first color from main plot
            local sym_lcolor: char LAYER[Colors_`layer']
            gettoken sym_lcolor : sym_lcolor, quotes
            local sym_lcolor lcolor(`sym_lcolor')
        }
        // generate plot
        _geoplot_symbol . `p' `frame' `if' `in' `sym_wgt', shape(square)/*
            */ `sym_offset' `sym_angle' `sym_ratio' `sym_size' `sym_lcolor'/*
            */ `sym_opts'
        if `sym'==1 {
            local --p
            local plot `plot' `PLOT'
        }
        else {
            local plot `PLOT' `plot'
        }
    }
    ***
    c_local plot `plot'
    c_local p `p'
end

program _parse_sym
    syntax [, ABOve noWeight SIze(str) ratio(passthru) OFFset(passthru)/*
        */ ANGle(passthru) COLor(passthru) LColor(passthru)/*
        */ COLORVar(passthru)/* will be ignored
        */ * ]
    if "`above'"!="" c_local sym 2
    else             c_local sym 1
    c_local sym_size   `"`size'"'
    c_local sym_wgt    = "`weight'"==""
    c_local sym_hascol = `"`color'`lcolor'"'!=""
    c_local sym_offset `offset'
    c_local sym_ratio  `ratio'
    c_local sym_angle  `angle'
    c_local sym_opts   `color' `lcolor' `options'
end

version 17
mata:
mata set matastrict on

void _compute_bars(string scalar frame, string scalar touse, 
    real scalar size, real scalar scale, real scalar angle, real scalar ratio,
    real scalar off, real scalar oang, real scalar asis)
{
    real scalar    i, r, s, a
    real rowvector v
    real colvector w
    real matrix    YX, Z

    // get data
    YX = st_data(., st_local("coordinates"), touse)
    Z  = abs(st_data(., st_local("varlist"), touse)) // |Z|
    w  = st_data(., st_local("wvar"), touse)
    r  = rows(YX)
    // determine size
    if (size<.)  s = size
    else {
        s = min(mm_coldiff(colminmax(YX)))
        s = max((s,strtoreal(st_local("refsize"))))
        s = max((1, s * 0.02)) // 3% of min(yrange, xrange) of map
    }
    if (scale<.) s = s * scale
    st_numscalar(st_local("SIZE"), s)
    // apply offset
    if (off) YX = YX :+ s * (off/100) * (sin(oang*pi()/180), cos(oang*pi()/180))
    // prepare frame
    st_framecurrent(frame)
    v = st_addvar("double", ("_Y","_X","_CY","_CX", "Z", "W"))
    // generate coordinates of pies
    a = angle * pi() / 180
    for (i=1;i<=r;i++)
        _compute_bar(s, a, ratio, asis, v, Z[i,], YX[i,], w[i])
} 

void _compute_bar(real scalar s, real scalar a,
    real scalar ratio, real scalar asis, real rowvector v, real rowvector z,
    real rowvector yx, real scalar w)
{
    real scalar j, c
    real scalar z0
    
    if (asis) z = z / 50
    else      z = z / (sum(z)/2)
    z0 = -1
    c = cols(z)
    for (j=1;j<=c;j++)
        __compute_bar(s, a, ratio, v, j, z[j], yx, w, z0)
}

void __compute_bar(real scalar s, real scalar r, real scalar ratio,
    real rowvector v, real scalar j, real scalar z,
    real rowvector yx, real scalar w, real scalar z0) // z0 will be updated
{
    real scalar    a, z1
    real matrix    YX
    
    z1 = z0 + z
    YX = (sin(.25 * pi()) * s) * (
         (z0, z0, z1, z1, z0)' * ratio, // y
         (-1,  1,  1, -1, -1)')         // x
    if (r!=1) {
        YX = (YX[,2] * sin(r) + YX[,1] * cos(r)), // y
             (YX[,2] * cos(r) - YX[,1] * sin(r))  // x
    }
    YX = ((., .) \ yx :+ YX), J(6, 1, (yx, j, w))
    a = st_nobs() + 1
    st_addobs(6)
    st_store((a,a+5), v, YX)
    z0 = z1
}

end

