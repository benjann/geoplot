*! version 1.0.6  06jul2023  Ben Jann

program _geoplot_symbol
    version 17
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0, pars(", ")
    ***
    frame `frame' {
        // syntax
        qui syntax [varlist(default=none max=1 numeric fv)] [if] [in] [iw/] [,/*
            */ SHape(passthru) line/*
            */ SIze(str) OFFset(numlist max=2)/*
            */ ANGle(real 0) ratio(real 1) n(passthru)/*
            */ COLORVar(varname numeric fv) LEVels(str) DISCRete/*
            */ COORdinates(varlist numeric min=2 max=2)/*
            */ CENTRoids(str) area(str)/* (will be ignored)
            */ _frameonly(str)/* undocumented; used by geoframe symbol
            */ * ]
        local NOPLOT = `"`_frameonly'"'!=""
        if `NOPLOT' {
            gettoken frame1 _frameonly : _frameonly
            gettoken ID : _frameonly
        }
        else {
            // check zvar
            if `"`colorvar'"'!="" local zvar `colorvar'
            else                  local zvar `varlist'
            _parse_zvar `zvar' // returns zvar, discrete
            // plottype
            if "`line'"!="" local plottype line
            else            local plottype area
        }
        // parse shape() and n()
         _parse_shape, `n' `shape' // returns shape, arg, n
        // parse offset
        gettoken offset oangle : offset
        gettoken oangle: oangle
        if "`offset'"=="" local offset 0
        if "`oangle'"=="" local oangle 0
        // sample and weights
        marksample touse
        tempname wvar
        if "`weight'"!="" {
            qui gen double `wvar' = abs(`exp') if `touse'
            markout `touse' `wvar'
            local wgt [iw=W]
        }
        else qui gen byte `wvar' = 1
        if "`coordinates'"!="" geoframe flip `coordinates', local(coord)
        else {
            geoframe get coordinates, strict flip local(coord)
            if `:list sizeof coord'!=2 {
                di as err "wrong number of coordinate variables"
                exit 498
            }
        }
        markout `touse' `coord' // include nonmissing coordinates only
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
        capt unab SIZE: `size', max(1) // check whether single variable
        if _rc==1 exit 1
        if _rc {
            tempvar SIZE
            qui gen double `SIZE' = (`size') if `touse'
        }
        markout `touse' `SIZE'
        // extra variables to be copied
        local ORG `wvar'
        local TGT W
        if `NOPLOT' {
            if "`ID'"=="" {
                tempname ID
                qui gen byte `ID' = .
                qui replace `ID' = sum(`touse')
            }
            local ORG `ORG' `ID'
            local TGT `TGT' _ID
        }
        else if "`zvar'"!="" {
            local options `discrete' `options'
            local ZVAR ZVAR
            local ORG `ORG' `zvar'
            local TGT `TGT' `ZVAR'
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
    if `scale' {
        if `NOPLOT' local refsize 0
        else {
            // get reference size of existing map
            su Y, meanonly
            local refsize = r(max) - r(min)
            su X, meanonly
            local refsize = min(`refsize', r(max) - r(min))
        }
    }
    frame `frame' {
        // compute shapes and post coordinates into temporary frame
        if `"`frame1'"'=="" tempname frame1
        frame create `frame1'
        mata: _compute_symbols("`frame1'", "`touse'", "`shape'",/*
            */ st_local("arg"), `n', `scale', `angle', `ratio',/*
            */ `offset', `oangle')
    }
    if `NOPLOT' exit
    ***
    __geoplot_layer 0 `plottype' `layer' `p' `frame1' `ZVAR' `wgt', lock/*
        */ `options'
    c_local plot `plot'
    c_local p `p'
end

program _parse_zvar
    if "`0'"=="" exit
    if substr("`0'",1,2)=="i." {    // i.zvar
        c_local zvar = substr("`0'",3,.)
        c_local discrete discrete
        exit
    }
    capt confirm variable `0'
    if _rc==1 exit _rc
    if _rc {
        di as err `"`0' not allowed"'
        exit 198
    }
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
    real scalar scale, real scalar angle, real scalar ratio,
    real scalar off, real scalar oang)
{
    real scalar      i, a, b, s, haspl
    real matrix      YX, Z, yx, V
    real colvector   pl, PL, S
    string rowvector ORG, TGT, VTYP
    pointer matrix   INFO
    
    // get origin data
    ORG  = tokens(st_local("ORG"))
    YX   = st_data(., st_local("coord"), touse)
    Z    = st_data(., ORG, touse)
    INFO = _fmtvl(ORG) // collect formats and labels
    VTYP = J(1,i=length(ORG),"") // collect data types
    for (;i;i--) VTYP[i] = st_vartype(ORG[i]) 
    i = rows(YX)
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
        yx = geo_symbol(shape, n, arg)
        if (cols(yx)>2) {
            haspl = 1
            pl = yx[,3]
            pl = pl[1] \ pl // duplicate 1st element
        }
    }
    yx = yx[,(2,1)] // since input is (X,Y), not (Y,X)
    n = rows(yx)
    // determine size
    S = st_data(., st_local("SIZE"), touse)
    if (scale) {
        s = min(mm_coldiff(colminmax(YX)))
        s = max((s,strtoreal(st_local("refsize"))))
        s = max((1, s * 0.03)) // 3% of min(yrange, xrange) of map
        S = S * s
    }
    // apply ratio to shape
    if (ratio!=1) yx = yx[,1]*ratio, yx[,2]
    // apply angle to shape
    _geo_rotate(yx, -angle) // (_geo_rotate() expects XY, not YX)
    // apply offset to centroids
    if (off) YX = YX :+ S :*
        J(rows(S), 1, (off/100)*(sin(oang*pi()/180), cos(oang*pi()/180)))
    // prepare destination data
    st_framecurrent(frame)
    TGT = tokens(st_local("TGT"))
    st_addobs(i * (n+1))
    st_view(V=., ., st_addvar((J(1,4,"double"), VTYP),
                      (tokens("_Y _X _CY _CX"), TGT)))
    _fmtvl(TGT, INFO)
    if (haspl) st_view(PL=., ., st_addvar("byte", "_PLEVEL"))
    // write shapes to destination data
    a = i * (n+1) + 1
    for (;i;i--) {
        b = a - 1
        a = b - n
        V[|a,1 \ b,.|] =
            (J(1,2,.) \ (YX[i,] :+ yx*S[i])), J(n+1, 1, (YX[i,], Z[i,]))
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

end

