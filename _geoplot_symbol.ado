*! version 1.2.2  01aug2024  Ben Jann

program _geoplot_symbol
    version 16.1
    _parse comma lhs 0 : 0
    syntax [, _immediate _frameonly(passthru) * ]
    if "`_immediate'"!="" {
        __geoplot_symboli `lhs', `_frameonly' `options'
    }
    else {
        __geoplot_symbol `lhs', `_frameonly' `options'
    }
    if `"`_frameonly'"'!="" exit
    c_local plot `plot'
    c_local p `p'
end

program __geoplot_symboli
    gettoken layer 0 : 0
    gettoken p 0 : 0, parse(", ")
    _parse comma values 0 : 0
    syntax [, _frameonly(str) SIze(str) * ]
    local PLOT = `"`_frameonly'"'==""
    if `PLOT' tempname frame
    else gettoken frame _frameonly : _frameonly
    tempname SIZE RELSIZE
    if `PLOT' local mlab POS str1 LAB
    frame create `frame' byte _ID _CX _CY `SIZE' `RELSIZE' `mlab'
    frame `frame' {
        _symboli `"`size'"' `SIZE' `RELSIZE' `PLOT' `values'
        char _dta[GEOPLOT_sourcename] symbol
    }
    if `haslab' {
        local mlabel mlabel(LAB)
        if `haspos' local mlabvpos mlabvpos(POS)
    }
    if `"`shparg'"'!="" {
        local options shparg(`shparg') `options'
    }
    ___geoplot_symbol `layer' `p' `frame', size(`SIZE' `RELSIZE')/*
        */ _frameonly(`_frameonly') `mlabel' `mlabvpos' `options'
    if !`PLOT' exit
    c_local plot `plot'
    c_local p `p'
end

program _symboli
    gettoken Size 0 : 0
    gettoken SIZE 0 : 0
    gettoken RELSIZE 0 : 0
    gettoken PLOT 0 : 0
    // process size()
    if `"`Size'"'!="" {
        if substr(`"`Size'"',1,1)=="*" {
            local Size = substr(`"`Size'"',2,.)
            local Relsize 1
        }
        else local Relsize 0
        capt confirm number `Size'
        if _rc==1 exit 1
        else if _rc {
            di as error "size(): invalid syntax"
            exit 198
        }
    }
    else {
        local Size 1
        local Relsize 1
    }
    // get optional shape argument
    gettoken shparg : 0, match(paren)
    if "`paren'"=="(" {
        gettoken shparg 0 : 0, match(paren)
        c_local shparg `"`shparg'"'
    }
    // collect values and labels
    local xy X Y
    local n 0
    local HASLAB 0
    local HASPOS 0
    if `: list sizeof 0'==0 local 0
    while (`"`0'"'!="") {
        // #_x #_y ...
        foreach v of local xy {
            gettoken `v' 0 : 0, qed(q) match(p)
            if `q' | "`p'"!="" _symboli_err `PLOT'
            capt numlist `"``v''"', min(0) missingokay
            if _rc==1 exit 1
            else if _rc        _symboli_err `PLOT'
            if `"``v''"'==""   _symboli_err `PLOT'
        }
        qui set obs `++n'
        foreach v of local xy {
            qui replace _C`v' = ``v'' in `n'
        }
        // ... [[*]size] ...
        gettoken size : 0, bind
        if (substr(`"`size'"',1,1)+substr(`"`size'"',-1,1))=="[]" {
            gettoken size 0 : 0, bind
            local size = substr(`"`size'"',2,strlen(`"`size'"')-2)
            if substr(`"`size'"',1,1)=="*" {
                local size = substr(`"`size'"',2,.)
                qui replace `RELSIZE' = 1 in `n'
            }
            capt confirm number `size'
            if _rc==1 exit 1
            else if _rc _symboli_err `PLOT' "size"
            qui replace `SIZE' = (`size') in `n'
        }
        else {
            qui replace `SIZE' = `Size' in `n'
            if `Relsize' {
                qui replace `RELSIZE' = 1 in `n'
            }
        }
        if !`PLOT' continue
        // ... [(clockpos)] "label"
        local haslab 0
        local haspos 0
        gettoken pos : 0, qed(q) match(p)
        if "`p'"!="" { // ... (position) "label"
            gettoken pos 0 : 0, match(p)
            capt confirm integer number `pos'
            if _rc==1 exit 1
            else if _rc             _symboli_err `PLOT' "pos"
            if !inrange(`pos',0,12) _symboli_err `PLOT' "pos"
            gettoken lab : 0, qed(q)
            if !`q' local lab
            else gettoken lab 0 : 0
            local haslab 1
            local haspos 1
        }
        else if `q' { // ... "label"
            gettoken lab 0 : 0
            local pos .
            local haslab 1
        }
        if `haslab' {
            qui replace POS = `pos' in `n'
            qui replace LAB = `"`lab'"' in `n'
            local HASLAB 1
            if `haspos' local HASPOS 1
        }
    }
    qui replace _ID = _n
    c_local haslab `HASLAB'
    c_local haspos `HASPOS'
end

program _symboli_err
    gettoken ml  0 : 0
    gettoken err 0 : 0
    di as err "invalid immediate value; the syntax for immediate values is"
    di as err "  {it:#_x} {it:#_y} [{bf:[}[{bf:*}]{it:size}{bf:]}]" _c
    if `ml' di as err `" [{bf:(}{it:clockpos}{bf:)}] [{bf:"}{it:text}{bf:"}]"'
    else    di as err ""
    if "`err'"=="pos" {
        di as err "{it:clockpos} must be an integer between 0 and 12"
    }
    else if "`err'"=="size" {
        di as err "{it:size} must be numeric"
    }
    exit 198
end

program __geoplot_symbol
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0, pars(", ")
    frame `frame' {
        local sourcename: char _dta[GEOPLOT_sourcename]
        if `"`sourcename'"'=="" local sourcename `frame'
        // syntax
        syntax [anything] [if] [in] [iw/] [,/*
            */ SIze(str) MLabel(varname) MLABVposition(varname numeric)/*
            */ SELect(str asis) COLORVar(str) LEVels(str)/*
            */ COORdinates(varlist numeric min=2 max=2)/*
            */ CENTRoids(str) area(str)/* (will be ignored)
            */ _frameonly(str)/* undocumented; used by geoframe symbol
            */ * ]
        gettoken shparg : anything, match(paren)
        if "`paren'"=="(" {
            gettoken shparg anything : anything, match(paren)
            local options shparg(`shparg') `options'
        }
        _parse_zvar 0 varlist `anything'
        _parse_zvar 1 colorvar `colorvar'
        // mode (plot vs. data only) and name for tempframe
        local PLOT = `"`_frameonly'"'==""
        if `PLOT' tempname frame1
        else gettoken frame1 _frameonly : _frameonly
        local ORG
        local TGT
        // id and coordinates
        if !`PLOT' { // use existing id if called by geoframe
            geoframe get id, local(id)
            if `"`id'"'!="" {
                local ORG `ORG' `id'
                local TGT `TGT' _ID
            }
        }
        if "`coordinates'"=="" {
            geoframe get coordinates, strict local(coord)
            if `:list sizeof coord'>2 {
                mata: st_local("coord",/*
                    */ invtokens(tokens(st_local("coord"))[|1\2|]))
            }
            else if `:list sizeof coord'<2 {
                di as err "wrong number of coordinate variables"
                exit 498
            }
        }
        local ORG `ORG' `coord'
        local TGT `TGT' _CX _CY
        // parse size()
        tempname SIZE RELSIZE
        local ORG `ORG' `SIZE' `RELSIZE'
        local TGT `TGT' `SIZE' `RELSIZE'
        if `"`size'"'=="" {
            qui gen byte `SIZE' = 1
            qui gen byte `RELSIZE' = 1 // relative size
        }
        else if substr(`"`size'"',1,1)=="*" {
            local size = substr(`"`size'"',2,.)
            if `"`size'"'=="" exit 198
            qui gen double `SIZE' = (`size')
            qui gen byte `RELSIZE' = 1 // relative size
        }
        else {
            qui gen double `SIZE' = (`size')
            qui gen byte `RELSIZE' = . // absolute size
        }
        // collect variables from plot options
        if `PLOT' {
            // zvar
            if `"`colorvar'"'!="" local zvar `colorvar'
            else                  local zvar `varlist'
            if "`zvar'"!="" {
                if substr("`zvar'",1,2)=="i." {    // i.zvar
                    local zvar = substr("`zvar'",3,.)
                    local FV "i."
                }
                local ZVAR ZVAR
                local ORG `ORG' `zvar'
                local TGT `TGT' `ZVAR'
                // get variable from levels(, weight())
                if `"`levels'"'!="" {
                    _parse_levels `levels' // (inserts lW)
                    local options levels(`levels') `options'
                    if "`lwvar'"!="" {
                        local ORG `ORG' `lwvar'
                        local TGT `TGT' lW
                    }
                }
            }
            // mlabels
            if "`mlabel'"!="" {
                local ORG `ORG' `mlabel'
                local TGT `TGT' LAB
                local mlabel mlabel(LAB)
                if "`mlabvposition'"!="" {
                    local ORG `ORG' `mlabvposition'
                    local TGT `TGT' POS
                    local mlabvposition mlabvposition(POS)
                }
            }
            // weights
            if "`weight'"!="" {
                tempname wvar
                qui gen double `wvar' = abs(`exp')
                local wgt [iw=`wvar']
                local ORG `ORG' `wvar'
                local TGT `TGT' `wvar'
            }
            // select
            if `"`select'"'!="" {
                tempvar SELECT
                qui gen byte `SELECT' = 1
                qui replace `SELECT' = 0 if !(`select')
                local ORG `ORG' `SELECT'
                local TGT `TGT' `SELECT'
                local options select(`SELECT'==1) `options'
            }
        }
        // sample
        marksample touse, novarlist
        markout `touse' `coord' `wvar' `SIZE'
    }
    // create attribute frame
    frame create `frame1'
    frame `frame1' {
        qui geoframe append `frame' `ORG', target(`TGT') touse(`touse') fast
        if "`id'"=="" {
            qui gen byte _ID = .
            qui replace _ID = _n
            order _ID
        }
        char _dta[GEOPLOT_sourcename] `sourcename'
    }
    // create shape frame and plot command
    ___geoplot_symbol `layer' `p' `frame1' "`FV'`ZVAR'" "`wgt'",/*
        */ size(`SIZE' `RELSIZE') _frameonly(`_frameonly')/*
        */ `mlabel' `mlabvposition' `options'
    if !`PLOT' exit
    c_local plot `plot'
    c_local p `p'
end

program _parse_zvar // removes "i." from "i.zvar" if zvar is string
    gettoken opt 0 : 0
    gettoken lnm 0 : 0
    // nothing to do if empty
    local l: list sizeof 0
    if `l'==0 {
        c_local `lnm'
        exit
    }
    // must be single token
    if `l'>1 {
        if `opt' di as err "`lnm'(): " _c
        di as err "too many variables specified"
        exit 103
    }
    gettoken 0 : 0 // strip leading space
    // strip "i." if string
    if substr(`"`0'"',1,2)=="i." {
        local 00: copy local 0
        local 0 = substr(`"`0'"',3,.)
        capt syntax varname(str)
        if _rc==1 exit 1
        if _rc local 0: copy local 00
    }
    // parse varlist
    if `opt' {
        local 0 `", `lnm'(`0')"'
        syntax, `lnm'(varname fv)
        c_local `lnm' ``lnm''
        exit
    }
    syntax varname(fv)
    c_local `lnm' `varlist'
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

program ___geoplot_symbol
    _parse comma lhs 0 : 0
    gettoken layer lhs : lhs
    gettoken p     lhs : lhs
    gettoken frame lhs : lhs
    gettoken zvar  lhs : lhs
    gettoken wgt   lhs : lhs
    local mlopts MLabel MLABSTYle MLABPosition MLABVposition MLABGap MLABANGle/*
        */ MLABTextstyle MLABSize MLABColor MLABFormat MLABJUSTification/*
        */ mlabalign
    foreach opt of local mlopts {
        local MLOPTS `MLOPTS' `opt'(passthru)
    }
    local mlopts = strlower("`mlopts'")
    syntax [, size(str) _frameonly(str) shparg(str asis)/*
        */ tsize(numlist max=1 >=0)/* (undocumented; fix text size)
        */ SHape(passthru) n(passthru) OFFset(numlist max=2)/*
        */ ANGle(numlist max=1) ratio(numlist max=1) slant(numlist max=1)/*
        */ align(str) line SELect(str asis) `MLOPTS' * ]
    if `"`select'"'!="" {
        local options select(`select') `options'
        local select `" if (`select')"'
    }
    // size(), _frameonly(), shparg()
    gettoken size relsize : size
    gettoken relsize      : relsize
    local PLOT = `"`_frameonly'"'==""
    if `PLOT' tempname frame1
    else      local frame1 `_frameonly'
    if `"`shape'"'=="" {
        if `"`shparg'"'!="" local shape shape(`shparg')
    }
    // parse symbol options: size(), shape(), n(), offset(), align()...
    if `layer'<. {
        local SHAPE `shape' `n'
        if "`offset'"!="" local SHAPE `SHAPE' offset(`offset')
        if "`angle'"!=""  local SHAPE `SHAPE' angle(`angle')
        if "`ratio'"!=""  local SHAPE `SHAPE' ratio(`ratio')
        if "`slant'"!=""  local SHAPE `SHAPE' slant(`slant')
        if "`align'"!=""  local SHAPE `SHAPE' align(`align')
    }
    _parse_shape, `n' `shape' // returns shape, arg, n
    if "`shape'"=="@label" {
        if !`PLOT' {
            di as err `"{bf:shape("}{it:text}{bf:")}"'/*
                */ " not allowed with {bf:geoframe symbol}"
            exit 198
        }
        if `"`wgt'"'!="" {
            di as err `"weights not allowed with {bf:shape("}{it:text}{bf:")}"'
            exit 101
        }
    }
    gettoken offset oangle : offset
    gettoken oangle: oangle
    if "`offset'"=="" local offset 0
    if "`oangle'"=="" local oangle 0
    if "`angle'"==""  local angle 0
    if "`ratio'"==""  local ratio 1
    if "`slant'"==""  local slant 0
    _parse_align, `align'
    // plottype
    if "`line'"!="" local plottype line
    else            local plottype area
    // get range of underlying map (including positions of symbols)
    frame `frame' {
        su _CX`select', meanonly
        local xmin = r(min)
        local xmax = r(max)
        su _CY`select', meanonly
        local ymin = r(min)
        local ymax = r(max)
    }
    if `PLOT' {
        su X, meanonly
        local xmin = min(`xmin', r(min))
        local xmax = max(`xmax', r(max))
        su Y, meanonly
        local ymin = min(`ymin', r(min))
        local ymax = max(`ymax', r(max))
    }
    local REFSIZE = min(`xmax'-`xmin', `ymax'-`ymin')
    // write shapes to shape frame
    if "`shape'"!="@label" frame create `frame1'
    frame `frame' {
        tempname SIZE
        mata: _compute_symbols("`frame1'", "`shape'", st_local("arg"),/* 
            */ `n', `angle', `ratio', `slant', "`size'", "`relsize'",/*
            */ `REFSIZE', `offset', `oangle', "`align_lr'", "`align_bt'")
    }
    if !`PLOT' exit
    // generate plot
    if "`shape'"=="@label" {
        if "`align_lr'"=="left" {
            if      "`align_bt'"=="top"    local pos 5
            else if "`align_bt'"=="bottom" local pos 1
            else                           local pos 3
        }
        else if "`align_lr'"=="right" {
            if      "`align_bt'"=="top"    local pos 7
            else if "`align_bt'"=="bottom" local pos 11
            else                           local pos 9
        }
        else {
            if      "`align_bt'"=="top"    local pos 6
            else if "`align_bt'"=="bottom" local pos 12
            else                           local pos 0
        }
        if "`tsize'"=="" {
            local tsize = `SIZE' / `REFSIZE' * 200
            if (`tsize'>=.) local tsize 3
        }
        if `angle' local angle angle(`angle')
        else       local angle
        _geoplot_label `layer' `p' `frame' (`"`arg'"') `zvar', /*
            */ size(`tsize') gap(0) position(`pos') `angle' `options'
    }
    else {
        local tsize
        frame `frame': qui geoframe link `frame1'
        __geoplot_layer `plottype' `layer' `p' `frame' `zvar' `wgt',/*
            */ lforce lock `options'
    }
    if "`mlabel'"!="" {
        local PLOT: copy local plot
        if "`mlabposition'"=="" local mlabposition mlabposition(0)
        local MLOPTS
        foreach opt of local mlopts {
            local MLOPTS `MLOPTS' ``opt''
        }
        local n0 = _N + 1
        __geoplot_layer scatter . `p' `frame' `zvar',/*
            */ msymbol(i) msize(0) `MLOPTS' `options'
        qui replace LAYER = `layer' in `n0'/l
        local plot `PLOT' `plot'
    }
    c_local plot `plot'
    c_local p `p'
    if `layer'<. {
        char LAYER[symbol_`layer'] `SHAPE'
        char LAYER[symsize_`layer'] `=`SIZE''
        char LAYER[symtsize_`layer'] `tsize' // size for shape("text")
        char LAYER[layertype_`layer'] symbol
    }
end

program _parse_shape
    syntax [, n(numlist int max=1 >0) SHape(str asis) ]
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
    // label (text symbol)
    gettoken shape arg : shape, qed(label)
    local arg = strtrim(`"`arg'"')
    if `label' {
        c_local shape "@label"
        c_local arg   `"`shape'"' // ignoring arg
        c_local n     .
        exit
    }
    // predefined shapes
    local SHAPES0 Triangle Square Pentagon HEXagon HEPtagon Octagon
    local shapes0 = strlower("`SHAPES0'")
    local SHAPES  `SHAPES0' Circle Arc SLice star star6 PENTAGRam HEXAGRam pin/*
        */ Line pipe PLus x DIamond v ARRow FARRow BARRow FBARRow bar/*
        */ CRoss ASTerisk FASTerisk TRApezoid pin2 pin3
    local shapes  = strlower("`SHAPES'")
    local 0 , `shape'
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

program _parse_align
    syntax [, Left Right Bottom Top Center ]
    local lr `left' `right'
    if `: list sizeof lr'>1 {
        di as err "only one of {bf:left} and {bf:right} allowed in {bf:align()}"
        exit 198
    }
    if "`lr'"=="" local lr `center'
    local bt `bottom' `top'
    if `: list sizeof bt'>1 {
        di as err "only one of {bf:bottom} and {bf:top} allowed in {bf:align()}"
        exit 198
    }
    if "`bt'"=="" local bt `center'
    c_local align_lr `lr'
    c_local align_bt `bt'
end

version 16.1
mata:
mata set matastrict on

void _compute_symbols(string scalar frame,
    string scalar shape, string scalar arg, real scalar n,
    real scalar angle, real scalar ratio, real scalar slant,
    string scalar SIZE, string scalar RELSIZE, real scalar refsize,
    real scalar off, real scalar oang, string scalar align_lr,
    string scalar align_bt)
{
    real scalar      i, a, b, s, haspl
    real matrix      XY, xy, V
    real colvector   ID, pl, PL, S
    
    // get origin data
    st_view(XY=., ., "_CX _CY")
    // determine size
    if (refsize) s = refsize * 0.015 // default size: 1.5% of refsize
    else         s = 1               // or 1 if refsize is zero
    S = st_data(., SIZE) :* editmissing(st_data(., RELSIZE)*s, 1)
    st_numscalar(st_local("SIZE"), mean(S))
    // apply offset to centroids (and write back to data)
    if (off) XY[.,.] = XY :+ S :*
        J(rows(S), 1, (off/100)*(cos(oang*pi()/180), sin(oang*pi()/180)))
    if (shape=="@label") return // done (no shapes needed)
    // create shape
    haspl = 0
    if (shape=="@numlist") {
        xy = strtoreal(tokens(arg))
        n  = length(xy) / 2
        if (n!=trunc(n)) {
            errprintf("shape({it:numlist}): even number of values required\n")
            exit(503)
        }
        xy = colshape(xy, 2)
    }
    else if (shape=="@matrix") {
        xy = st_matrix(arg)
        n  = length(xy) / 2
        if (n!=trunc(n)) {
            errprintf("shape({it:matname}): even number of values required\n")
            exit(503)
        }
        if (cols(xy)!=2) xy = colshape(xy', 2)
    }
    else {
        xy = geo_symbol(shape, n, arg)
        if (cols(xy)>2) {
            haspl = 1
            pl = xy[,3]
            pl = pl[1] \ pl // duplicate 1st element
            xy = xy[,(1,2)]
        }
    }
    n = rows(xy)
    // align
    if      (align_lr=="left")   xy[,1] = xy[,1] :- min(xy[,1])
    else if (align_lr=="right")  xy[,1] = xy[,1] :- max(xy[,1])
    else if (align_lr=="center") xy[,1] = xy[,1] :- sum(minmax(xy[,1]))/2
    if      (align_bt=="bottom") xy[,2] = xy[,2] :- min(xy[,2])
    else if (align_bt=="top")    xy[,2] = xy[,2] :- max(xy[,2])
    else if (align_bt=="center") xy[,2] = xy[,2] :- sum(minmax(xy[,2]))/2
    // apply ratio, slant, and angle
    if (ratio!=1) xy[,2] = ratio * xy[,2]
    if (slant)    xy[,1] = xy[,1] + slant * xy[,2]
    _geo_rotate(xy, angle)
    // prepare destination data
    ID = st_data(., "_ID")
    i = rows(XY)
    st_framecurrent(frame)
    st_addobs(i * (n+1))
    st_view(V=., ., st_addvar("double", tokens("_ID _X _Y")))
    if (haspl) st_view(PL=., ., st_addvar("byte", "_PLEVEL"))
    // write shapes to destination data
    a = i * (n+1) + 1
    for (;i;i--) {
        b = a - 1
        a = b - n
        V[|a,1 \ b,.|] = J(n+1, 1, ID[i]), (J(1,2,.) \ (XY[i,] :+ xy*S[i]))
        if (haspl) PL[|a \ b|] = pl
    }
}

end

