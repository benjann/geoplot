*! version 1.2.7  08sep2024  Ben Jann

/*
    Syntax:

      __geoplot_layer <plottype> <layer> <p> <frame> ...

      where <plottype> is one of area, line, scatter, pcspike, ..., pcscatter
            <layer> is the index of the current layer
            <p> is the twoway plot counter
            <frame> is the name of the frame

    <layer> may be . to create a "hidden" layer, i.e. a layer that will not be
    available to legend().

    Outline of a program making used of __geoplot_layer:

    program _geoplot_mylayer
        version 16.1
        gettoken layer 0 : 0
        gettoken p 0 : 0
        gettoken frame 0 : 0
        ***
           core of program: parse input, do whatever operations are necessary,
           set plottype, myframe, myarguments, and myoptions
        ***
        __geoplot_layer `plottype' `layer' `p' `myframe' `myarguments', `myoptions'
        c_local plot `plot'
        c_local p `p'
    end

    The program can then be used as

        geoplot (mylayer frame ...) ...
*/

program __geoplot_layer
    version 16.1
    // setup
    gettoken plottype 0 : 0
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0, parse(" ,")
    local hasSHP 0
    local OPTS NOLEGEND LABel(str asis) GLoptions(str) Feature(passthru)/*
        */ box BOX2(str) SELect(str asis)
    local hasPLV 0
    local PLVopts
    local WGT
    local MLABopts
    local Zopts COLORVar(str)/*
        */ LEVels(str) cuts(str) DISCRete MISsing(str asis) /*
        */ COLor(passthru) LWidth(passthru) LPattern(passthru)
    local Zel color lwidth lpattern
    local YX Y X // coordinate variable names used in plot data
    if inlist(`"`plottype'"',"area","line") {
        local TYPE shape
        local hasSHP 1
        local OPTS `OPTS' SIze(str asis) lock wmax WMAX2(numlist max=1 >0)/*
            */ IFshp(str asis) COORdinates(namelist min=2 max=2) id(name)/*
            */ CENTRoids(varlist numeric min=2 max=2) area(varname numeric)/*
            */ LColor(passthru) lforce // lforce: do not not turn lines off
        local WGT [iw/]
        if `"`plottype'"'=="area" {
            local hasPLV 1
            local PLVopts PLevel(name) EColor(str asis) FColor(str asis)
            local Zopts `Zopts' FIntensity(passthru)
            local Zel `Zel' fintensity
        }
    }
    else {
        if substr(`"`plottype'"',1,2)=="pc" { // paired coordinate plot
            local TYPE pc
            local hasSHP 1
            local OPTS `OPTS' COORdinates(namelist min=4 max=4) noshp
            local YX  Y  X Y2 X2 // variable names used in plot data
        }
        else {  // scatter assumed
            local TYPE attribute
            local OPTS `OPTS' COORdinates(namelist min=2 max=2)/*
                */ wmax WMAX2(numlist max=1 >0)/*
                */ shp lock IFshp(str asis) id(name)/*
                */ CENTRoids(varlist numeric min=2 max=2)
            local WGT [iw/]
            local wgt "[aw=W]"
        }
        local MLABopts MLabel(varname) MLABVposition(varname numeric) /*
            */ MLABFormat(str) mlabi(str asis) mlabz
        local Zopts `Zopts' Msymbol(passthru) MSIZe(passthru) /*
            */ MSAngle(passthru) MLWidth(passthru) MLABSize(passthru) /*
            */ MLABANGle(passthru) MLABColor(passthru)
        local Zel `Zel' msymbol msize msangle mlwidth mlabsize mlabangle/*
            */ mlabcolor
    }
    // collect info from syntax, frame and possible shpframe
    frame `frame' {
        // syntax
        qui syntax [anything] [if] [in] `WGT'/*
            */ [, `OPTS' `Zopts' `PLVopts' `MLABopts' * ]
        // check zvar
        _parse_zvar 0 varlist `anything'
        _parse_zvar 1 colorvar `colorvar'
        if `"`colorvar'"'!="" local zvar `colorvar'
        else                  local zvar `varlist'
        if substr("`zvar'",1,2)=="i." {    // i.zvar
            local zvar = substr("`zvar'",3,.)
            local discrete 1
        }
        else local discrete = "`discrete'"!=""
        local hasZ = `"`zvar'"'!=""
        _parse_levels `levels' // => levels, method, l_wvar, l_min, l_max
        _parse_cuts `cuts' // => cuts, cutsismat
        _parse_label `label' // => label, nolabel, format, reverse
        // varia
        if `"`box2'"'!="" local box box
        local hasMLAB = (`"`mlabi'"'!="") + ("`mlabz'"!="") + ("`mlabel'"!="")
        if `hasMLAB'>1 {
            di as err "only one of mlabel(), mlabi(), and mlabz allowed"
            exit 198
        }
        _parse_size `size' // => size, s_max, s_scale
        local hasSIZE = `"`size'"'!=""
        if `"`fcolor'"'!="" mata: _get_colors("fcolor")
        // sample
        marksample touse, novarlist
        // feature
        if `"`feature'"'=="" geoframe get feature, l(feature)
        else                 _parse_feature, `feature'
        if `"`feature'"'=="water" {
            if `"`color'"'==""  local color color("135 206 235") // SkyBlue
            if `"`plottype'"'=="area" {
                if "`fintensity'"=="" local fintensity fintensity(50)
            }
        }
        // check shpframe and determine data type(s)
        if      "`shp'"=="shp"   local hasSHP 1 // scatter with option shp
        else if "`shp'"=="noshp" local hasSHP 0 // pc with option noshp
        if `hasSHP' {
            geoframe get shpframe, local(shpframe)
            local hasSHP = `"`shpframe'"'!=""
        }
        if `hasSHP' {
            geoframe get type, local(type)
            if `"`type'"'=="" local type attribute
            frame `shpframe' {
                if "`TYPE'"=="pc" local typeSHP pc // enforce pc
                else {
                    geoframe get type, local(typeSHP)
                    if `"`typeSHP'"'=="" local typeSHP shape
                }
            }
            local org `touse'
            local tgt `touse'
        }
        else {
            local shpframe `frame'
            if "`TYPE'"=="pc" local type pc // enforce pc
            else {
                geoframe get type, local(type)
                if `"`type'"'=="" local type `TYPE'
            }
            local typeSHP `"`type'"'
        }
        // handle ifshp()
        if strtrim(`"`ifshp'"')!="" {
            if `hasSHP' local SHPFRAME `shpframe'
            else        geoframe get shpframe, local(SHPFRAME)
            if `"`SHPFRAME'"'!="" {
                frame `SHPFRAME' { // tag units with nonzero selection
                    tempvar merge
                    qui gen byte `merge' = 1
                    qui replace  `merge' = 0 if !(`ifshp')
                    geoframe get id, local(SHPID) strict
                    mata: _set_one_if_any("`SHPID'" , "`merge'")
                }
                qui geoframe copy `SHPFRAME' `merge'
                qui replace `touse' = 0 if `touse' & `merge'!=1
                drop `merge'
            }
            else {
                qui replace `touse' = 0 if `touse' & !(`ifshp')
            }
        }
        // handle coordinates, PLV, and unit ID
        frame `shpframe' {
            if "`coordinates'"!="" geoframe flip `coordinates', local(yx)
            else geoframe get coordinates, strict flip local(yx) `typeSHP'
            local ORG `yx'
            local TGT `YX'
            _get_plevel `hasPLV' `hasZ' "`plevel'" `"`color'"' `"`fcolor'"'
            if `hasPLV' {
                local ORG `ORG' `plevel'
                local TGT `TGT' PLV
            }
        }
        // handle weights
        local hasWGT = "`weight'"!=""
        if `hasWGT' {
            tempname WVAR
            qui gen double `WVAR' = abs(`exp') if `touse'
            markout `touse' `WVAR' // excludes obs with missing weight!
            if `"`wmax2'"'=="" {
                su `WVAR' if `touse', meanonly
                if "`wmax'"!="" local wmax2 = r(max)
                else            local wmax2 = max(1, r(max)) 
            }
            qui replace `WVAR' = `WVAR' / `wmax2' if `touse'
            if `hasSHP' {
                local org `org' `WVAR'
                local tgt `tgt' `WVAR'
            }
            local ORG `ORG' `WVAR'
            local TGT `TGT' W
        }
        else local wgt
        // handle Z
        if `hasZ' {
            // tag first obs per ID if type is shape or pc and id is available
            // or if id() has been specified
            if inlist(`"`type'"',"shape","pc") & "`id'"=="" {
                geoframe get id, local(id)
            }
            if "`id'"!="" {
                tempname ztouse
                qui gen byte `ztouse' = `touse'
                qui replace `ztouse' = 0/*
                    */ if `id'==`id'[_n-1] & `touse'==`touse'[_n-1]
            }
            else local ztouse `touse'
            // categorize
            tempname ZVAR CUTS NOBS NMIS
            capt confirm string variable `zvar'
            if _rc==1 exit 1
            if _rc { // numeric zvar
                // determine cuts/levels
                mata: _z_cuts("`CUTS'", st_local("cuts"), `levels', `l_min',/*
                    */ `l_max', `discrete', "`zvar'", "`method'", "`l_wvar'",/*
                    */ "`cutsismat'", "`ztouse'") /* fills in CUTS and returns
                    levels */
                // categorize zvar
                qui gen `=cond(`levels'>32740,"long",/*
                    */cond(`levels'>100, "int", "byte"))' `ZVAR' = .
                mata: _z_categorize("`CUTS'", "`NOBS'", "`NMIS'", `levels',/*
                    */ `discrete', "`zvar'", "`ZVAR'", "`ztouse'") /* fills in
                       NOBS, NMIS, ZVAR and returns zindex */
                if `"`cuts'"'!="" { // check for unmatched obs
                    qui count if `ZVAR'>=. & `ztouse'
                    if r(N) local zvarnotcomplete 1
                }
                // collect labels/display format
                if `discrete' & "`nolabel'"=="" {
                    _z_labels `zvar' `levels' `CUTS' // => zlabels
                }
                local zfmt: format `zvar'
            }
            else { // string zvar
                local zvarstr 1
                local discrete 1
                local zfmt %8.0g
                if "`cutsismat'"!="" {
                    mata: st_local("cuts",/*
                        */ invtokens(strofreal(vec(st_matrix("`cuts'"))')))
                }
                capt numlist "`cuts'", int min(0) range(>0)
                if _rc==1 exit 1
                if _rc {
                    di as err "{it:zvar} is string: values in cuts()"/*
                        */ " must be positive and integer"
                    exit 126
                }
                local reverse = !`reverse' // flip default order in legend
                mata: _z_strvar("`CUTS'", st_local("cuts"), /*
                    */ "`NOBS'", "`NMIS'", "`zvar'", "`ZVAR'", "`ztouse'",/*
                    */ "`nolabel'"!="") /* fills in CUTS, NOBS, NMIS, ZVAR and
                       returns levels, zindex, zlabels, zvarnotcomplete */
            }
            // fill in remaining obs
            if "`touse'"!="`ztouse'" { 
                qui replace `ZVAR' = `ZVAR'[_n-1] if `touse' & !`ztouse'
            }
            if "`zvarnotcomplete'"=="1" {
                di as txt "(layer `layer': {it:zvar} contains values"/*
                    */ " not covered by cuts())"
            }
            if `hasSHP' {
                local org `org' `ZVAR'
                local tgt `tgt' `ZVAR'
            }
            local ORG `ORG' `ZVAR'
            local TGT `TGT' Z
        }
        else {
            if `hasMLAB' {
                if `"`mlabcolor'"'=="" & `"`color'"'!="" {
                    // colors() also sets mlabcolor()
                    local mlabcolor mlab`color'
                }
            }
            foreach el of local Zel {
                if `"``el''"'=="" continue
                local options ``el'' `options'
                local `=strupper("`el'")' `el'
                local `el'
            }
        }
        // handle size
        if `hasSIZE' {
            if "`area'"!="" local AREA `area'
            else {
                geoframe get area, local(AREA)
                if !`: list sizeof AREA' {
                    di as txt "layer `layer': area variable (original size) not"/*
                        */ " found; computing areas on the fly"
                    di as txt "generate/declare area variable using " /*
                        */ "{helpb geoframe} to avoid such extra computations"
                    tempname AREA
                    qui geoframe gen area `AREA', noset
                }
            }
            tempname SVAR
            qui gen double `SVAR' = abs(`size') / `AREA' if `touse'
            if `"`s_max'"'=="" {
                su `SVAR' if `touse', meanonly
                local s_max = r(max)
            }
            qui replace `SVAR' = `SVAR' * (`s_scale'/`s_max') if `touse'
            if `hasSHP' {
                local org `org' `SVAR'
                local tgt `tgt' `SVAR'
            }
            local ORG `ORG' `SVAR'
            local TGT `TGT' `SVAR' // tempvar only
        }
        // get centroids (used by weights and size() in area/line)
        if (`hasWGT' & ("`TYPE'"=="shape")) | `hasSIZE' | "`lock'"!="" {
            tempname CY CX
            if "`centroids'"!="" geoframe flip `centroids', local(cYX)
            else {
                geoframe get centroids, flip local(cYX)
                if !`: list sizeof cYX' {
                    di as txt "layer `layer': centroids not found;"/*
                        */ " computing centroids on the fly"
                    di as txt "generate/declare centroids using " /*
                        */ "{helpb geoframe} to avoid such extra computations"
                    qui geoframe gen centroids `CX' `CY', noset
                    local cYX `CY' `CX'
                }
            }
            if `hasSHP' {
                local org `org' `cYX'
                local tgt `tgt' `CY' `CX'
                local ORG `ORG' `CY' `CX'
            }
            else local ORG `ORG' `cYX'
            if "`lock'"!="" {
                local CY cY
                local CX cX
            }
            local TGT `TGT' `CY' `CX'
        }
        // handle marker labels
        if `hasMLAB' {
            if `"`mlabformat'"'!="" confirm format `mlabformat'
            if "`mlabvposition'"!="" {
                local ORG `ORG' `mlabvposition'
                local TGT `TGT' MLABPOS
            }
            tempname MLAB
            qui gen strL `MLAB' = ""
            if "`mlabel'"'!="" {
                mata: _generate_mlabels("`MLAB'", "`mlabel'", "`mlabformat'",/*
                    */ "`touse'")
            }
            if `hasSHP' {
                local org `org' `MLAB'
                local tgt `tgt' `MLAB'
            }
            local ORG `ORG' `MLAB'
            local TGT `TGT' MLAB
        }
        else local mlabcolor
        // remove obs with missing ZVAR (only if zvar is string)
        if "`zvarstr'`zvarnotcomplete'"=="11" {
            qui replace `touse' = 0 if `ZVAR'>=.
        }
        // select option
        if strtrim(`"`select'"')!="" {
            qui replace `touse' = 0 if `touse' & !(`select')
        }
        // inject colors
        _process_coloropts options, `lcolor' `options'
        if `"`fcolor'"'!="" local fcolor fcolor(`fcolor')
        if `"`gloptions'"'!="" {
            _process_coloropts gloptions, `gloptions'
        }
    }
    // copy relevant variables from unit frame into shpframe
    if `hasSHP' {
        frame `shpframe' {
            capt geoframe copy `frame' `org', target(`tgt') unique
            if _rc==1 exit 1
            if _rc { // try again with estimation sample only
                qui geoframe copy `frame' `org' if `touse', target(`tgt') unique
            }
            qui replace `touse' = 0 if `touse'>=.
            if strtrim(`"`ifshp'"')!="" {
                qui replace `touse' = 0 if `touse' & !(`ifshp')
            }
        }
    }
    // copy data into main frame
    local n0 = _N + 1
    qui geoframe append `shpframe' `ORG', target(`TGT') touse(`touse') raw fast
    local n1 = _N
    //local n1 = _N
    if `n1'<`n0' {
        if `layer'<. {
            char LAYER[hasz_`layer'] 0
            di as txt "(layer `layer' is empty)"
        }
        c_local plot
        c_local p `p'
        exit
    }
    local in in `n0'/`n1'
    qui replace LAYER = `layer' `in'
    // process size (area/line only)
    if `hasSIZE' {
        qui replace Y = `CY' + (Y-`CY') * sqrt(`SVAR') if `SVAR'<. `in'
        qui replace X = `CX' + (X-`CX') * sqrt(`SVAR') if `SVAR'<. `in'
    }
    // process weights (area/line only)
    if `hasWGT' & ("`TYPE'"=="shape") {
        qui replace Y = `CY' + (Y-`CY') * sqrt(W) if W<. & `CY'<. & `CX'<. `in'
        qui replace X = `CX' + (X-`CX') * sqrt(W) if W<. & `CY'<. & `CX'<. `in'
    }
    // prepare PLV
    if `hasPLV' {
        if `"`ecolor'"'=="" local ecolor white%100 // default for enclaves
        mata: _get_colors("ecolor")
        qui replace PLV = 0 if PLV>=. `in' // treat missing as 0
        qui levelsof PLV `in'
        local plevels `r(levels)'
    }
    else local plevels .
    // handle Z elements
    local opts
    if `hasZ' {
        // - process options
        if `levels' {
            local ZEL
            foreach el of local Zel {
                if "`el'"=="color"     continue
                if `"``el''"'==""      continue
                if "`el'"=="mlabcolor" _z_colors `discrete' `levels'/*
                    */ mlabcolor `mlabcolor'
                else _z_recycle `levels' `el', ``el'' // returns el, sizeofel
                if `sizeofel'==0 continue
                local `=strupper("`el'")' `el'
                if `sizeofel'==1 {
                    local opts `opts' `el'(``el'')
                    local `el'
                    continue
                }
                local ZEL `ZEL' `el'
            }
            _z_colors `discrete' `levels' color `color'
            if `sizeofel' {
                local COLOR color
                if `sizeofel'==1 {
                    local opts color(`color') `opts'
                    if `hasMLAB' & "`MLABCOLOR'"=="" {
                        // colors() also sets mlabcolor()
                        local opts `opts' mlabcolor(`color') 
                    }
                }
                else {
                    local ZEL color `ZEL'
                    if `hasMLAB' & "`MLABCOLOR'"=="" {
                        // colors() also sets mlabcolor()
                        local mlabcolor `"`color'"'
                        local ZEL `ZEL' mlabcolor
                    }
                }
            }
            if `"`color'"'=="" local color `"`mlabcolor'"'
        }
        // - process missing
        if `NMIS' {
            _z_parse_missing `plottype' `hasMLAB', `missing'
            if !`levels' local COLOR color // turn color on if only missing
        }
    }
    else local zindex 0
    // Set default options
    local opts0
    if `"`plottype'"'=="area" {
        local opts0 cmissing(n) nodropbase lalign(center)
        if "`COLOR'"=="" {
            if `"`lcolor'"'=="" local opts lcolor(gray) `opts'
            if `"`fcolor'"'=="" local opts fcolor(none) `opts'
        }
        if "`FINTENSITY'"=="" local opts finten(100) `opts'
        if "`LWIDTH'"=="" {
            if "`COLOR'"!="" & `"`fcolor'`lcolor'`lforce'"'=="" {
                local opts lwidth(0) lcolor(%0) `opts' // turn lines off
            }
            else local opts lwidth(.15) `opts'
        }
        if "`LPATTERN'"=="" local opts lpattern(solid) `opts'
    }
    else if "`plottype'"'=="line" {
        local opts0 cmissing(n)
        if "`COLOR'"=="" & `"`lcolor'"'=="" local opts lcolor(gray) `opts'
        if "`LWIDTH'"==""   local opts lwidth(.15) `opts'
        if "`LPATTERN'"=="" local opts lpattern(solid) `opts'
    }
    if `hasMLAB' {
        local opts0 `opts0' mlabel(MLAB)
        if "`mlabvposition'"!="" local opts0 `opts0' mlabvposition(MLABPOS)
    }
    // add box (below)
    if "`box'"!="" {
        _box `p' `n0' `n1' `"`TYPE'"', `box2' // p, plot
        qui replace LAYER = `layer' in `n1'/l
    }
    else local plot
    // compile plot
    if `hasWGT' local in inrange(_n,`n0',`n1')) | (_n<3)
    local GLOPTS
    local p0 = `p' + 1
    gettoken pl0 : plevels
    foreach pl of local plevels {
        local iff
        if `pl'<. {
            local iff PLV==`pl'
            local enclave = mod(`pl',2)
        }
        else local enclave 0
        if `hasZ' {
            foreach el of local ZEL {
                local EL = strupper("`el'")
                local `EL': copy local `el'
            }
        }
        foreach i of local zindex {
            local IFF `iff'
            if `hasZ' {
                if `i'==0 { // missing
                    local OPTS `opts'
                }
                else {
                    local OPTS
                    foreach el of local ZEL {
                        local EL = strupper("`el'")
                        gettoken tmp `EL' : `EL', quotes
                        local OPTS `OPTS' `el'(`tmp')
                    }
                    local OPTS `opts' `OPTS'
                }
                if `"`IFF'"'!="" local IFF `IFF' & Z==`i'
                else             local IFF Z==`i'
                if `pl'!=`pl0' {
                    // skip empty plots in additional layers
                    qui count if `IFF' in `n0'/`n1'
                    if r(N)==0 continue
                }
            }
            else local OPTS `opts'
            if `hasWGT' {
                if `"`IFF'"'!=""  local IFF if (`IFF' & `in'
                else              local IFF if (`in'
            }
            else if `"`IFF'"'!="" local IFF if `IFF' `in'
            else                  local IFF `in'
            local IFF `IFF' `wgt'
            if `enclave' local OPTS `OPTS' fcolor(`ecolor')
            else         local OPTS `OPTS' `fcolor'
            local OPTS `OPTS' `options'
            if `hasZ' {
                if `i'==0 { // missing
                    local OPTS `OPTS' `missing'
                }
            }
            if `layer'<. {
                if `pl'==`pl0' {
                    if `hasZ' & `i'==0 { // missing
                        local GLOPT `OPTS' `missing_glopts'
                    }
                    else {
                        local GLOPT `OPTS' `gloptions'
                    }
                    local GLOPTS `GLOPTS' (`GLOPT')
                }
            }
            local OPTS `opts0' `OPTS'
            if `"`OPTS'"'!="" {
                local OPTS , `OPTS'
            }
            local plot `plot' (`plottype' `YX' `IFF'`OPTS')
            local ++p
        }
        if `pl'==`pl0' local p1 `p'
    }
    numlist "`p0'/`p1'"
    local keys `r(numlist)'
    // compile legend labels
    if `hasZ' {
        if `"`format'"'=="" local format `zfmt'
        if `NMIS' {
            local labels `"`missing_lab'"'
            local Nobs = `NMIS'
            local labels: subinstr local labels "@n" "`Nobs'", all
            local space " "
        }
        else {
            local labels
            local space
        }
        if `discrete' {
            _label_separate `label' // => lab_keys, lab_lbls
            forv i = 1/`levels' {
                local val = `CUTS'[1,`i']
                local mid `: di `format' `val''
                gettoken lab zlabels : zlabels
                if `"`lab'"'=="" local lab `mid'
                mata: _get_lbl("`val'", "lab_keys", "lab_lbls", `"@lab"')
                local lbl: subinstr local lbl "@lab" `"`lab'"', all
                local lbl: subinstr local lbl "@lb"  "`mid'", all
                local lbl: subinstr local lbl "@ub"  "`mid'", all
                local lbl: subinstr local lbl "@mid" "`mid'", all
                local Nobs = `NOBS'[1,`i']
                local lbl: subinstr local lbl "@n" "`Nobs'", all
                local labels `"`labels'`space'`"`lbl'"'"'
                local space " "
            }
        }
        else {
            if `"`label'"'=="" local label 1 "[@lb,@ub]"
            _label_separate `label' // => lab_keys, lab_lbls
            forv i = 1/`levels' {
                local LB = `CUTS'[1,`i']
                local UB = `CUTS'[1,`i'+1]
                local mid `: di `format' (`UB'+`LB')/2'
                local lb  `: di `format' `LB''
                local ub  `: di `format' `UB''
                mata: _get_lbl("`i'", "lab_keys", "lab_lbls", `"(@lb,@ub]"')
                local lbl: subinstr local lbl "@lab" "`lb' - `ub'", all
                local lbl: subinstr local lbl "@lb" "`lb'", all
                local lbl: subinstr local lbl "@ub" "`ub'", all
                local lbl: subinstr local lbl "@mid" "`mid'", all
                local Nobs = `NOBS'[1,`i']
                local lbl: subinstr local lbl "@n" "`Nobs'", all
                local labels `"`labels'`space'`"`lbl'"'"'
                local space " "
            }
        }
    }
    else {
        if `"`label'"'=="" {
            frame `frame': local label: char _dta[GEOPLOT_sourcename]
            if `"`label'"'=="" local label `"`frame'"'
        }
        _add_quotes label `label'
        if `:list sizeof label'>1 local label `"`"`label'"'"'
        local labels `"`label'"'
    }
    // mlabi()/mlabz
    local mlabels
    if `"`mlabi'`mlabz'"'!="" {
        if "`mlabz'"!="" local MLBLS labels
        else             local MLBLS mlabi
        if `hasZ' {
            local space
            local MLABI
            foreach i of local zindex {
                if `"`MLABI'"'=="" { // recycle
                    local MLABI: copy local `MLBLS'
                }
                gettoken mlbi MLABI : MLABI
                qui replace MLAB = `"`mlbi'"' in `n0'/`n1' if Z==`i'
                local mlabels `"`mlabels'`space'`"`mlbi'"'"'
                local space " "
            }
        }
        else {
            gettoken mlbi : `MLBLS'
            qui replace MLAB = `"`mlbi'"' in `n0'/`n1'
            local mlabels `"`"`mlbi'"'"'
        }
    }
    else if `"`mlabel'"'!="" {
        if `layer'<. {
            foreach i of local zindex {
                local mlabels `mlabels' `mlabel'
            }
        }
    }
    // returns
    if `layer'<. {
        char LAYER[keys_`layer'] `keys'
        char LAYER[plottype_`layer'] `plottype'
        char LAYER[glopts_`layer'] `GLOPTS'
        char LAYER[labels_`layer'] `"`labels'"'
        char LAYER[mlabels_`layer'] `"`mlabels'"'
        char LAYER[nolegend_`layer'] `nolegend'
        char LAYER[hasz_`layer'] `hasZ'
        char LAYER[wmax_`layer'] `wmax2'
        if `hasZ' {
            local hasmis = `NMIS' > 0
            char LAYER[z_hasmis_`layer'] `hasmis'
            if `hasmis' {
                matrix `NOBS' = (`NMIS', `NOBS')
                local mleg = (1 + `missing_first'*2 + `missing_gap')/*
                    */ * (!`missing_nolab') 
                char LAYER[z_mleg_`layer'] `mleg'
                // 0 = omit missing in legend
                // 1 = place missing last without gap
                // 2 = place missing last with gap
                // 3 = place missing first without gap
                // 4 = place missing first with gap
            }
            local hascolors = `: list sizeof color' > 1
            char LAYER[z_hascol_`layer'] `hascolors'
            if `hasmis' local color `"`missing_color' `color'"'
            char LAYER[z_colors_`layer'] `"`color'"'
            char LAYER[z_discrete_`layer'] `discrete'
            char LAYER[z_reverse_`layer'] `reverse'
            char LAYER[z_format_`layer'] `zfmt'
            local TMP: char LAYER[CUTS]
            char LAYER[CUTS] // clear
            matrix rename `CUTS' `TMP'
            char LAYER[z_levels_`layer'] `TMP'
            local TMP: char LAYER[NOBS]
            char LAYER[NOBS] // clear
            matrix rename `NOBS' `TMP'
            char LAYER[z_nobs_`layer'] `TMP'
        }
    }
    c_local plot `plot'
    c_local p `p'
end

program _parse_zvar // parse [i.]zvar; remove "i." if zvar is string
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

program _parse_feature
    syntax [, feature(str) ]
    c_local feature `"`feature'"'
end

program _parse_size
    _parse comma size 0 : 0
    c_local size `"`size'"'
    if `"`size'"'=="" exit
    syntax [, Dmax(numlist max=1 >0) Scale(numlist max=1 >0) ]
    if "`scale'"=="" local scale 1
    c_local s_max `dmax'
    c_local s_scale `scale'
end

program _parse_levels
    if `"`0'"'=="" {
        c_local levels .
        c_local l_min .
        c_local l_max .
        exit
    }
    capt n __parse_levels `0'
    if _rc==1 exit _rc
    if _rc {
        di as err "(error in option levels())"
        exit _rc
    }
    c_local levels `levels'
    c_local method `method'
    c_local l_wvar `l_wvar'
    c_local l_min `l_min'
    c_local l_max `l_max'
end

program __parse_levels
    _parse comma n 0 : 0
    if `"`n'"'!="" {
        numlist `"`n'"', int min(0) max(1) range(>0)
        local n "`r(numlist)'"
    }
    else local n .
    local methods Quantiles Kmeans //Jenks
    syntax [, `methods' Weight(varname numeric)/*
        */ min(numlist max=1) max(numlist max=1) ]
    local methods = strlower("`methods'")
    foreach m of local methods {
        local method `method' ``m''
    }
    if `:list sizeof method'>1 {
        di as err "too many methods specified; only one method allowed"
        exit 198
    }
    if "`method'"=="kmeans" {
        if "`weight'"!="" {
            di as err "{bf:weight()} not allowed with method {bf:kmeans}"
            exit 198
        }
    }
    if "`min'"=="" local min .
    if "`max'"=="" local max .
    c_local levels `n'
    c_local method `method'
    c_local l_wvar `weight'
    c_local l_min `min'
    c_local l_max `max'
end

program _parse_cuts
    if `"`0'"'=="" exit
    capt numlist `"`0'"', min(0) missingok
    if _rc==1 exit 1
    if !_rc {
        local 0 `r(numlist)'
        c_local cuts: list uniq 0
        exit
    }
    if `: list sizeof 0'==1 {
        capt n confirm matrix `0'
        if _rc==1 exit 1
        if _rc {
            di as err "error in in {bf:cuts()}"
            exit _rc
        }
        c_local cutsismat cutsismat
        exit
    }
    di as err "invalid specification in {bf:cuts()}; must specify "/*
        */ "{it:numlist} or {it:matname}"
    exit 198
end

program _get_plevel
    args hasPLV hasZ plevel color fcolor
    if !`hasPLV' exit
    if !`hasZ' {
        // zvar not specified and no fill color
        if `"`color'`fcolor'"'==""            local hasPLV 0
        else if strtrim(`"`fcolor'"')=="none" local hasPLV 0
    }
    if `hasPLV' {
        if "`plevel'"=="" geoframe get plevel, l(plevel)
        if "`plevel'"=="" local hasPLV 0
    }
    c_local hasPLV `hasPLV'
    c_local plevel `plevel'
end

program _z_labels
    args var levels CUTS
    if !`levels' {
        c_local zlabels
        exit
    }
    local labname: value label `var'
    if `"`labname'"'=="" {
        mata: st_local("labels", invtokens(strofreal(st_matrix("`CUTS'"),/*
            */ "%18.0g")))
    }
    else {
        local labels
        local space
        forv i = 1/`levels' {
            local val = `CUTS'[1,`i']
            local lab: label `labname' `val', strict
            local labels `"`labels'`space'`"`lab'"'"'
            local space " "
        }
    }
    c_local zlabels `"`labels'"'
end

program _z_colors
    gettoken discrete 0 : 0
    gettoken k 0 : 0
    gettoken nm 0 : 0
    local 0 `", `0'"'
    syntax [, `nm'(str asis) ]
    mata: _z_color_parselastcomma("`nm'") // returns color and 0
    syntax [, class(passthru) n(passthru) Select(passthru) drop(passthru)/*
        */ IPolate(passthru) * ]
    if `"`n'`select'`drop'`ipolate'"'=="" local n n(`k')
    local options `n' `select' `drop' `ipolate' `options'
    if `discrete' local default_palette st
    else          local default_palette viridis
    local l: list sizeof color
    if `l'==0 {
        // use default palette if no palette specified
        local color `default_palette'
    }
    else if `l'==1 {
        // add default palette if single opacity/intensity specified
        if inlist(substr(`"`color'"',1,1),"%","*") {
            local color `"`default_palette'`color'"'
        }
    }
    if `"`class'"'=="" & `discrete' local class class(categorical)
    colorpalette `color', nograph `class' `options'
    local color `"`r(p)'"'
    local l: list sizeof color
    if `l'==0 {
        c_local `nm'
        c_local sizeofel 0
        exit
    }
    if `l'==1 {
        c_local `nm' `"`color'"'
        c_local sizeofel 1
        exit
    }
    if `l'!=`k' { // recycle colors if wrong number of colors
        colorpalette `color', nograph class(categorical) n(`k')
        local color `"`r(p)'"'
    }
    c_local `nm' `"`color'"'
    c_local sizeofel `k'
end

program _z_recycle
    _parse comma lhs 0 : 0
    gettoken k  lhs : lhs
    gettoken el lhs : lhs
    syntax [, `el'(str asis) ]
    loca opt: copy local `el'
    // try numlist
    capt numlist `"`opt'"'
    if _rc==1 _rc
    if _rc==0 {
        local opt `"`r(numlist)'"'
        if `: list sizeof opt'==2 {
            // generate range
            gettoken lb opt : opt
            gettoken ub opt : opt
            mata: st_local("opt",/*
                */ invtokens(strofreal(rangen(`lb', `ub', `k')', "%18.0g")))
        }
    }
    // expand
    local l: list sizeof opt
    if `l'==0 {
        c_local `el'
        c_local sizeofel 0
        exit
    }
    if `l'==1 {
        c_local `el' `"`opt'"'
        c_local sizeofel 1
        exit
    }
    mata: st_local("opt", invtokens(_vecrecycle(`k', tokens(st_local("opt")))))
    c_local `el' `"`opt'"'
    c_local sizeofel `k'
end

program _z_parse_missing
    gettoken plottype 0 : 0
    gettoken hasMLAB 0 : 0, parse(", ")
    syntax [, NOLABel LABel(str asis) GLoptions(str) first nogap/*
        */ COLor(str asis) MLABColor(passthru) * ]
    if `"`label'"'=="" local label `"no data"'
    _add_quotes label `label'
    if `:list sizeof label'>1 local label `"`"`label'"'"'
    _process_coloropts options, `mlabcolor' `options'
    mata: _get_colors("color")
    if `"`color'"'=="" local color gs14
    if `hasMLAB' {
        if `"`mlabcolor'"'=="" {
            local options mlabcolor(`color') `options'
        }
    }
    local options color(`color') `options'
    if `"`gloptions'"'!="" {
        _process_coloropts gloptions, `gloptions'
    }
    c_local missing `options'
    c_local missing_color `"`color'"'
    c_local missing_lab   `"`label'"'
    c_local missing_glopts `"`gloptions'"'
    c_local missing_nolab = "`nolabel'"!=""
    c_local missing_first = "`first'"!=""
    c_local missing_gap   = "`gap'"==""
end

program _parse_label
    _parse comma label 0 : 0
    syntax [, NOLabel Format(str) Reverse ]
    c_local label `"`label'"'
    c_local nolabel `nolabel'
    c_local format `format'
    c_local reverse = "`reverse'"!=""
end

program _label_separate
    // add "* =" if needed
    gettoken l : 0, quotes qed(hasquotes) parse("= ")
    if `hasquotes' local 0 `"* = `0'"'
    else {
        // check whether 1st token is numeric (possibly including wildcards)
        local l: subinstr local l "*" "", all
        local l: subinstr local l "?" "", all
        capt numlist `"`l'"', min(0) max(1) missingok
        if _rc==1 exit 1
        if _rc local 0 `"* = `"`0'"'"'
    }
    // parse list
    local keys
    local lbls
    while (1) {
        gettoken key 0 : 0, parse("= ")
        if `"`key'"'=="" continue, break
        local keys `"`keys' `key'"'
        gettoken eq : 0, parse("= ")
        if `"`eq'"'=="=" {
            gettoken eq 0 : 0, parse("= ")
        }
        //gettoken l : 0, quotes qed(hasquotes)
        local lbl
        local space
        local hasquotes 1
        local i 0
        while (`hasquotes') {
            local ++i
            gettoken l 0 : 0
            local lbl `"`lbl'`space'`"`l'"'"'
            local space " "
            gettoken l : 0, qed(hasquotes)
        }
        if `i'>1 local lbls `"`lbls' `"`lbl'"'"'
        else     local lbls `"`lbls' `lbl'"'
    }
    c_local lab_keys `"`keys'"'
    c_local lab_lbls `"`lbls'"'
end

program _add_quotes
    gettoken nm 0 : 0
    __add_quotes `0'
    c_local `nm' `"`0'"'
end

program __add_quotes
    if `"`0'"'=="" {
        c_local 0
        exit
    }
    gettoken tmp : 0, qed(hasquote)
    if !`hasquote' {
        local 0 `"`"`0'"'"'
    }
    c_local 0 `"`0'"'
end

program _process_coloropts // pass standard color options through ColrSpace
    _parse comma nm 0 : 0
    local opts COLor FColor LColor MColor MFColor MLColor MLABColor
    foreach o of local opts {
        local OPTS `OPTS' `o'(str asis)
    }
    syntax [, `OPTS' * ]
    local opts = strlower("`opts'")
    local OPTS
    foreach o of local opts {
        if `"``o''"'!="" {
            mata: _get_colors("`o'")
            local OPTS `OPTS' `o'(``o'')
        }
    }
    c_local `nm' `OPTS' `options'
end

program _box
    // syntax
    _parse comma args 0 : 0
    gettoken p      args : args
    gettoken n0     args : args
    gettoken n1     args : args
    gettoken TYPE   args : args
    syntax [, PADding(passthru) ROTate hull CIRcle n(passthru) ANGle(passthru)/*
        */ noADJust line box BOX2(passthru) SIze(passthru) COLORVar(passthru)/*
        */ REFine * ]
    if `"`box'`box2'`size'`colorvar'"'!="" {
        di as err "box(): invalid syntax"
        exit 198
    }
    local boxopts `padding' `rotate' `hull' `circle' `n' `angle' `adjust'
    // copy relevant data into new frame
    tempname XY
    frame create `XY'
    mata: _box_copy_XY("`XY'", `n0', `n1', "`TYPE'"=="pc")
    // generate frame containing box
    tempname BOX BOXSHP
    frame `XY': qui geoframe bbox `BOX' `BOXSHP', noshp `boxopts'
    if "`refine'"!="" {
        frame `BOX': qui geoframe refine, fast
    }
    // compile plot
    if "`line'"!="" local plottype line
    else            local plottype area
    __geoplot_layer `plottype' . `p' `BOX', `options'
    // returns
    c_local plot `plot'
    c_local p `p'
end

version 16.1
mata:
mata set matastrict on

void _set_one_if_any(string scalar id, string scalar touse)
{
    real scalar    i, a, b
    real colvector p
    real matrix    ID, TOUSE
    
    st_view(ID=., ., id)
    st_view(TOUSE=., ., touse)
    p = selectindex(_mm_unique_tag(ID))
    i = rows(p)
    a = rows(ID) + 1
    for (;i;i--) {
        b = a - 1; a = p[i]
        if (anyof(TOUSE[|a \ b|],1)) TOUSE[|a \ b|] = J(b - a + 1, 1, 1)
    }
}

transmorphic vector _vecrecycle(real scalar k, transmorphic vector x)
{   // x must have at least one element
    real scalar r, c
    
    r = rows(x); c = cols(x)
    if (r>c) return(J(ceil(k/c), 1, x)[|1\k|]) // => rowvector if x is 1x1
    return(J(1, ceil(k/c), x)[|1\k|])
}

void _z_cuts(string scalar CUTS, string scalar cuts, real scalar k,
    real scalar min, real scalar max, real scalar discrete, string scalar zvar,
    string scalar method, string scalar wvar, string scalar cutsismat,
    string scalar touse)
{
    real scalar    lb, ub
    real rowvector minmax
    real colvector C, X, w, p
    
    // CASE 1: cuts() specified
    if (cuts!="") {
        if (cutsismat!="") C = vec(st_matrix(cuts))
        else               C = strtoreal(tokens(cuts)')
        k = length(C)
        if (!discrete) {
            k = k - 1
            _sort(C,1)
        }
        st_matrix(CUTS, C')
        st_local("levels", strofreal(k))
        return
    }
    // CASE 2: discrete
    if (discrete) {
        C = mm_unique(st_data(., zvar, touse))
        C = select(C, C:<.) // remove missing codes
        if (length(C)==0) C = J(0,1,.) // select() may return 0x0
        st_matrix(CUTS, C')
        st_local("levels", strofreal(length(C)))
        return
    }
    // get data
    X = st_data(., zvar, touse)
    minmax = minmax(X)
    // SPECIAL CASE 1: no nonmissing data
    if (minmax==J(1,2,.)) {
        st_matrix(CUTS, J(1,0,.))
        st_local("levels", "0")
        return
    }
    // default number of levels
    if (k>=.) k = 5
    // SPECIAL CASE 2: no variance
    lb = minmax[1]; ub = minmax[2]
    if (min<lb)         lb = min
    if (max<. & max>ub) ub = max
    if (lb==ub) {
        st_matrix(CUTS, (lb, ub))
        st_local("levels", "1")
        return
    }
    // CASE 3: equidistant intervals
    if (method=="") {
        C = rangen(lb, ub, k+1) 
        st_matrix(CUTS, C')
        st_local("levels", strofreal(length(C)-1))
        return
    }
    // get weights
    if (wvar!="") {
        w = st_data(., wvar, touse)
        _editmissing(w, 0)
    }
    else w = 1
    // CASE 4: quantiles
    if (method=="quantiles") {
        p = selectindex(X:<.) // remove missings
        X = X[p]
        if (wvar!="") w = w[p]
        C = mm_quantile(X, w, rangen(0, 1, k+1))
    }
    // CASE 5: kmeans
    else if (method=="kmeans") {
        X = select(X, X:<.) // remove missings
        C = lb \ _z_cuts_kmeans(k, X)
    }
    else exit(error(3499))
    // return results from CASE 4 or 5
    C = _mm_unique(C)
    k = length(C)
    if (C[1]>lb) C[1] = lb // make sure that min is included
    if (C[k]<ub) C[k] = ub // make sure that max is included
    st_matrix(CUTS, C')
    st_local("levels", strofreal(k-1))
}

real colvector _z_cuts_kmeans(real scalar k, real colvector X)
{
    real colvector C, p
    string scalar  frame, cframe

    cframe = st_framecurrent()
    frame  = st_tempname()
    st_framecreate(frame)
    st_framecurrent(frame)
    (void) st_addvar("double", "X")
    st_addobs(rows(X))
    st_store(., "X", X)
    stata(sprintf("cluster kmeans X, k(%g) name(C) start(segments)", k))
    C = st_data(., "C")
    st_framecurrent(cframe)
    st_framedrop(frame) // (not really needed)
    // return sorted list of upper bounds of clusters
    p = order((C,X), (1,2))
    return(sort(select(X[p], _mm_unique_tag(C[p], 1)), 1))
}

void _z_categorize(string scalar cuts, string scalar nobs, string scalar nmis,
    real scalar k, real scalar discrete, string scalar zvar,
    string scalar ZVAR, string scalar touse)
{
    real scalar    i, n, m, c0, c1
    real rowvector C, N
    real colvector Z, T, p
    
    C = st_matrix(cuts)
    N = J(1, k, .)
    st_view(Z=., ., zvar, touse)
    st_view(T=., ., ZVAR, touse)
    if (k) {
        if (!discrete) c0 = C[k+1]
        for (i=k;i;i--) {
            if (discrete) p = selectindex(Z:==C[i])
            else {
                c1 = c0; c0 = C[i]
                if (i==1) p = selectindex(Z:>=c0 :& Z:<=c1)
                else      p = selectindex(Z:> c0 :& Z:<=c1)
            }
            N[i] = n = length(p)
            if (n) T[p] = J(n,1,i)
        }
    }
    p = selectindex(Z:>=. :& T:>=.)
    m = length(p)
    if (m) {
        T[p] = J(m,1,0) // set missings to 0
        st_local("zindex", invtokens(strofreal(0..k)))
    }
    else if (k) st_local("zindex", invtokens(strofreal(1..k)))
    else        st_local("zindex", "")
    st_matrix(nobs, N)
    st_numscalar(nmis, m)
}

void _z_strvar(string scalar CUTS, string scalar cuts,
    string scalar NOBS, string scalar nmis, string scalar zvar,
    string scalar ZVAR, string scalar touse, real scalar nolabel)
{
    real scalar      i, k, m, n
    real colvector   Z, T, C, N, p
    string colvector L
    
    // get levels
    st_sview(Z=., ., zvar, touse)
    L = mm_unique(Z)
    p = L:!=""
    m = rows(L) - sum(p)
    if (m) L = select(L, p)
    k = rows(L)
    if (k) C = 1::k
    else {
        L = J(0,1,"")
        C = J(0,1,.)
    }
    // select and reorder
    p = strtoreal(tokens(cuts)')
    if (length(p)) {
        p = select(p, p:<=k)
        if (length(p)) {; L = L[p]; C = C[p]; }
        else           {; L = J(0,1,""); C = J(0,1,.); }
        if (length(L)<k) {
            k = length(L)
            st_local("zvarnotcomplete","1")
        }
    }
    // generate ZVAR
    st_view(T=., .,
        st_addvar(k>32740 ? "long" : (k>100 ? "int" : "byte"), ZVAR), touse)
    N = J(k,1,.)
    for (i=k;i;i--) {
        p = selectindex(Z:==L[i])
        N[i] = n = length(p)
        if (n) T[p] = J(n,1,i)
    }
    // missings
    if (m) {
        T[.] = editmissing(T, 0)
        st_local("zindex", invtokens(strofreal(0..k)))
    }
    else if (k) st_local("zindex", invtokens(strofreal(1..k)))
    else        st_local("zindex", "")
    // further returns
    st_local("levels", strofreal(k))
    if (nolabel) st_local("zlabels", invtokens(strofreal(C')))
    else st_local("zlabels", invtokens(("`"+`"""'):+L':+(`"""'+"'")))
    st_matrix(CUTS, C')
    st_matrix(NOBS, N')
    st_numscalar(nmis, m)
}

void _z_color_parselastcomma(string scalar nm)
{
    real scalar      i, pi
    real rowvector   p
    string rowvector T
    transmorphic     t
    
    t = tokeninit("", ("/",","), (`""""', `"`""'"',"()"))
    tokenset(t, st_local(nm))
    T = tokengetall(t)
    p = selectindex(T:=="/")
    i = length(p)
    if (i) { // multiple palettes
        pi = p[i]
        if (pi==length(T)) pi = 0 // no opts
        else {
            p = selectindex(T[|pi+1\.|]:==",")
            if (length(p)) pi = pi + p[1]
            else           pi = 0 // no opts
        }
    }
    else { // single palette
        p = selectindex(T:==",")
        if (length(p)) pi = p[1]
        else           pi = 0 // no opts
    }
    if (pi) {
        if (pi==1) st_local("color", "") // nothing before comma
        else       st_local("color", strtrim(invtokens(T[|1\pi-1|],"")))
        st_local("0", invtokens(T[|pi\.|],""))
    }
    else { // no opts
        st_local("color", strtrim(invtokens(T,"")))
        st_local("0", "")
    }
}

void _generate_mlabels(string scalar var, string scalar VAR, string scalar fmt, 
    string scalar touse)
{
    real scalar      isstr
    real colvector   X
    string scalar    vl
    string colvector L

    isstr = st_isstrvar(VAR)
    if (isstr) L = st_sdata(., VAR, touse)
    else {
        X = st_data(., VAR, touse)
        if (!rows(X)) return
        vl = st_varvaluelabel(VAR)
        if (vl!="") {
            if (!st_vlexists(vl)) vl = ""
        }
        if (vl!="") {
            L = st_vlmap(vl, X)
            if (anyof(L, "")) {
                L = L + (L:=="") :* 
                    strofreal(X, fmt!="" ? fmt : st_varformat(VAR))
            }
        }
        else {
            L = strofreal(X, fmt!="" ? fmt : st_varformat(VAR))
        }
    }
    st_sstore(., var, touse, L)
}

void _get_colors(string scalar lname)
{   /* function assumes that global ColrSpace object "_GEOPLOT_ColrSpace_S"
       exists; maintaining a global is less work than initializing ColrSpace
       in each call */
    pointer (class ColrSpace scalar) scalar S
    
    S = findexternal("_GEOPLOT_ColrSpace_S")
    //if ((S = findexternal("_GEOPLOT_ColrSpace_S"))==NULL) S = &(ColrSpace())
    S->colors(st_local(lname))
    st_local(lname, S->colors())
}

void  _get_lbl(string scalar key, string scalar keys, string scalar lbls,
    string scalar def)
{
    real scalar   i, n
    string vector Keys
    
    Keys = tokens(st_local(keys))
    n = length(Keys)
    for (i=1;i<=n;i++) {
        if (strmatch(key, Keys[i])) break
    }
    if (i>n) st_local("lbl", def)
    else st_local("lbl", tokens(st_local(lbls))[i])
}

void _box_copy_XY(string scalar frame, real scalar n0, real scalar n1,
    real scalar pc)
{
    real matrix   XY
    string scalar cframe

    XY = st_data((n0,n1), ("X","Y"))
    if (pc) XY = XY \ st_data((n0,n1), ("X2","Y2"))
    cframe = st_framecurrent()
    st_framecurrent(frame)
    st_addobs(rows(XY))
    st_store(., st_addvar("double", ("_X","_Y")), XY)
    st_framecurrent(cframe)
}

end

