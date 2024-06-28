*! version 1.1.3  26jun2024  Ben Jann

program _geoplot_pie
    version 16.1
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0
    ***
    frame `frame' {
        // syntax
        qui syntax varlist(numeric) [if] [in] [iw/] [,/*
            */ polar EXPLode(str) asis REVerse mid ANGle(real 90)/*
            */ SIze(str) n(numlist int max=1 >0) OFFset(numlist max=2)/*
            */ noLABel OUTline OUTline2(str) wmax WMAX2(numlist max=1 >0)/*
            */ COLORVar(str) LEVels(str) cuts(str) DISCRete/* (will be ignored)
            */ COORdinates(varlist numeric min=1 max=2) /*
            */ CENTRoids(str) area(str)/* (will be ignored)
            */ * ]
        local options `wmax' `wmax2' `options'
        if "`n'"=="" local n 100
        _parse_explode `: list sizeof varlist' `explode'
        if `"`outline'`outline2'"'!="" _parse_sym, `outline2'
        else                           local sym 0
        // parse offset
        gettoken offset oangle : offset
        gettoken oangle: oangle
        if "`offset'"=="" local offset 0
        if "`oangle'"=="" local oangle 0
        // sample and weights
        marksample touse
        if "`coordinates'"!="" geoframe flip `coordinates', local(coord)
        else {
            geoframe get coordinates, strict flip local(coord)
            if `:list sizeof coord'!=2 {
                di as err "wrong number of coordinate variables"
                exit 498
            }
        }
        markout `touse' `coord' // include nonmissing coordinates only
        tempname wvar
        if "`weight'"!="" {
            qui gen double `wvar' = abs(`exp') if `touse'
            markout `touse' `wvar'
            if `"`wmax2'"'=="" {
                su `wvar' if `touse', meanonly
                if "`wmax'"!="" local wmax2 = r(max)
                else            local wmax2 = max(1, r(max)) 
            }
            qui replace `wvar' = `wvar' / `wmax2' if `touse'
            local wgt [iw=W]
            local wmaxopt wmax(1)
        }
        else {
            qui gen byte `wvar' = 1
            if "`polar'"!="" {
                local wgt [iw=W]
                local wmaxopt wmax(1)
            }
        }
        // size
        local scale 0
        if `"`size'"'=="" {
            local size 1
            local scale 1
        }
        else if substr(`"`size'"',1,1)=="*" {
            local size = substr(`"`size'"',2,.)
            local scale 1
        }
        tempvar SIZE
        qui gen double `SIZE' = (`size') if `touse'
        markout `touse' `SIZE'
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
    if `scale' {
        // get reference size of existing map
        su Y, meanonly
        local refsize = r(max) - r(min)
        su X, meanonly
        local refsize = min(`refsize', r(max) - r(min))
    }
    frame `frame' {
        // compute slices of pies and post coordinates into temporary frame
        tempname frame1
        frame create `frame1'
        mata: _compute_pies("`frame1'", "`touse'", "`polar'"!="",/*
            */ `scale', `angle', `n', `offset', `oangle',/*
            */ "`asis'"!="", "`reverse'"!="", "`mid'"!="")
            // => makes frame1 the current frame
        // add labels
        lab def Z `lbls'
        lab val Z Z
    }
    ***
    if `sym'==1 local ++p
    __geoplot_layer area `layer' `p' `frame1' `wgt',/*
        */ colorvar(Z) lock discrete `wmaxopt' `options'
    ***
    if `sym' {
        local PLOT: copy local plot
        // pass weights, size, coordinates, etc. through to outline
        if "`coordinates'"!="" local sym_coord coordinates(`coordinates')
        if `sym_wgt' & "`weight'"!="" {
            local sym_wgt [`weight'=`exp']
            if `"`sym_wmax'"'=="" {
                if "`wmax2'"!="" local sym_wmax wmax(`wmax2')
                else             local sym_wmax `wmax'
            }
        }
        else local sym_wgt
        if `offset' {
            if `"`sym_offset'"'=="" local sym_offset offset(`offset' `oangle')
        }
        if substr(`"`sym_size'"',1,1)=="*" local sym_size `"`SIZE' `sym_size'"'
        else if `"`sym_size'"'==""         local sym_size `SIZE'
        local sym_size size(`sym_size')
        if !`sym_hascol' {
            // use first color from main plot
            local sym_lcolor: char LAYER[z_colors_`layer']
            gettoken sym_lcolor : sym_lcolor, quotes
            local sym_lcolor lcolor(`sym_lcolor')
        }
        // generate plot
        local n0 = _N + 1
        _geoplot_symbol . `p' `frame' `if' `in' `sym_wgt', `sym_coord'/*
            */ shape(circle) `sym_offset' `sym_size' `sym_lcolor' `sym_wmax'/*
            */ `sym_opts'
        qui replace LAYER = `layer' in `n0'/l
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

program _parse_explode
    gettoken c 0 : 0
    capt numlist `"`0'"', min(1) max(1)
    if _rc==1 exit 1
    if _rc==0 {
        // single number
        local def `r(numlist)'
    }
    else {
        // id = # [ id = # ...]
        while (`"`0'"'!="") {
            gettoken id 0 : 0, parse("= ")
            gettoken eq   : 0, parse("= ")
            if `"`eq'"'=="=" gettoken eq 0 : 0, parse("= ") // remove "="
            gettoken e 0 : 0
            capt numlist `"`id'"', min(1) max(1) int range(>0)
            if _rc continue
            local id `r(numlist)'
            capt numlist `"`e'"', min(1) max(1)
            if _rc continue
            local e `r(numlist)'
            local e_`id' `e'
        }
        local def 0
    }
    local explode
    forv i=1/`c' {
        local e "`e_`i''"
        if "`e'"=="" local e `def'
        local explode `explode' `e'
    }
    c_local explode `explode'
end

program _parse_sym
    syntax [, below noWeight SIze(str) OFFset(passthru)/*
        */ wmax WMAX2(passthru) /*
        */ COLor(passthru) LColor(passthru)/*
        */ COLORVar(passthru)/* will be ignored
        */ * ]
    if "`below'"!="" c_local sym 1
    else             c_local sym 2
    c_local sym_wgt    = "`weight'"==""
    c_local sym_size   `"`size'"'
    c_local sym_offset `offset'
    c_local sym_wmax   `wmax' `wmax2'
    c_local sym_hascol = `"`color'`lcolor'"'!=""
    c_local sym_opts   `color' `lcolor' `options'
end

version 16.1
mata:
mata set matastrict on

void _compute_pies(string scalar frame, string scalar touse, real scalar polar,
    real scalar scale, real scalar a, real scalar n, real scalar off,
    real scalar oang, real scalar asis, real scalar rev, real scalar mid)
{
    real scalar    i, r, s
    real rowvector v, e
    real colvector w, S
    real matrix    YX, Z

    // get data
    e = strtoreal(tokens(st_local("explode")))
    YX = st_data(., st_local("coord"), touse)
    Z  = abs(st_data(., st_local("varlist"), touse)) // |Z|
    w  = st_data(., st_local("wvar"), touse)
    r  = rows(YX)
    // determine size
    st_view(S=., ., st_local("SIZE"), touse)
    if (scale) {
        s = min(mm_coldiff(colminmax(YX)))
        s = max((s,strtoreal(st_local("refsize"))))
        s = max((1, s * 0.03)) // 3% of min(yrange, xrange) of map
        S[.] = S * s // (write back to data for use by outline())
    }
    // apply offset
    if (off) YX = YX :+ S :*
        J(rows(S), 1, (off/100)*(sin(oang*pi()/180), cos(oang*pi()/180)))
    // prepare frame
    st_framecurrent(frame)
    v = st_addvar("double", ("_Y","_X","_CY","_CX", "Z", "W"))
    // generate coordinates of pies
    for (i=1;i<=r;i++) _compute_pie(polar, S[i], a, n, asis, rev, mid, e, v,
        Z[i,], YX[i,], w[i])
} 

void _compute_pie(real scalar polar, real scalar s, real scalar a,
    real scalar n, real scalar asis, real scalar rev, real scalar mid,
    real rowvector e, real rowvector v, real rowvector z, real rowvector yx,
    real scalar w)
{
    real scalar j, c
    real scalar z0
    
    if (asis) z = z / 100
    else      z = z / sum(z)
    if (mid & rev) z0 = -(a/360 - z[1]/2)
    else if (mid)  z0 = -(a/360 + z[1]/2)
    else           z0 = -(a/360)
    c = cols(z)
    for (j=1;j<=c;j++) {
        if (polar) _compute_slice(s, n, rev, e[j], v, j, 1/c, yx, c*z[j]*w, z0)
        else       _compute_slice(s, n, rev, e[j], v, j, z[j], yx, w, z0)
    }
}

void _compute_slice(real scalar s, real scalar n, real scalar rev,
    real scalar e, real rowvector v, real scalar j, real scalar z,
    real rowvector yx, real scalar w, real scalar z0) // z0 will be updated
{
    real scalar    r, a, dz
    real rowvector mid
    real matrix    YX
    
    if (rev) dz = -z
    else     dz = z
    if (e) { // explode
        mid = -(2*z0+dz) * pi()
        mid = yx + (s * e/100) * (sin(mid), cos(mid))
    }
    else mid = yx
    r  = max((2, round(z * n))) // number of points on arc (at least 2)
    YX = rangen(z0, z0 + dz, r) * (-2 * pi())
    YX = mid :+ s * (sin(YX), cos(YX))
    YX = (., .) \ mid \ YX \ mid
    YX = YX, J(r + 3, 1, (yx, j, w))
    a = st_nobs() + 1
    st_addobs(r + 3)
    st_store((a,a+r+2), v, YX)
    z0 = z0 + dz
}

end

