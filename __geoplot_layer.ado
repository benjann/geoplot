*! version 1.1.2  30sep2023  Ben Jann

/*
    Syntax:

    o case 1: layer using data from specified frame

          __geoplot_layer 0 <plottype> <layer> <p> <frame> ...

      where <plottype> is one of area, line, scatter, pcspike, ..., pcscatter
            <layer> is the index of the current layer
            <p> is the twoway plot counter
            <frame> is the name of the frame

    o case 2: layer with immediate arguments

          __geoplot_layer 1 <plottype> <layer> <p> <immediate_values> ...

      where <plottype> is one of scatteri, pci, pcarrowi
            <layer> is the index of the current layer
            <p> is the twoway plot counter
            <immediate_values> are the immediate coordinates

    In both cases, <layer> may be . to create a "hidden" layer, i.e. a layer
    that will not be available to legend().

    Outline of a (case 1) program making used of __geoplot_layer:

    program _geoplot_mylayer
        version 16.1
        gettoken layer 0 : 0
        gettoken p 0 : 0
        gettoken frame 0 : 0
        ***
           core of program: parse input, do whatever operations are necessary,
           set plottype, myframe, myarguments, and myoptions
        ***
        __geoplot_layer 0 `plottype' `layer' `p' `myframe' `myarguments', `myoptions'
        c_local plot `plot'
        c_local p `p'
    end

    The program can then be used as

        geoplot (mylayer frame ...) ...
*/

program __geoplot_layer
    version 16.1
    gettoken i 0 : 0
    if `i' {
        _layeri `0'
    }
    else {
        _layer `0'
    }
    c_local plot `plot'
    c_local p `p'
end

program _layeri
    gettoken plottype 0 : 0
    gettoken layer 0 : 0
    gettoken p 0 : 0
    _parse comma lhs 0 : 0
    syntax [, LABel(str asis) * ]
    _process_coloropts options, `options'
    local ++p
    if `layer'<. {
        _parse_label `label'
        if `"`label'"'=="" local label `"`plottype'"'
        _add_quotes label `label'
        if `:list sizeof label'>1 local label `"`"`label'"'"'
        char LAYER[hasz_`layer'] 0
        char LAYER[keys_`layer'] `p'
        char LAYER[labels_`layer'] `"`label'"'
    }
    c_local p `p'
    c_local plot (`plottype' `lhs', `options')
end

program _layer
    // setup
    gettoken plottype 0 : 0
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0, parse(" ,")
    local hasSHP 0
    local TYPE
    local OPTS NOLEGEND LABel(str asis) Feature(passthru) box BOX2(str)/*
        */ SELect(str asis)
    local hasPLV 0
    local PLVopts
    local WGT
    local MLABopts
    local Zopts COLORVar(varname numeric fv)/*
        */ LEVels(str) cuts(numlist sort min=2) DISCRete MISsing(str asis) /*
        */ COLor(passthru) LWidth(passthru) LPattern(passthru)
    local Zel color lwidth lpattern
    local YX Y X // coordinate variable names used in plot data
    if inlist(`"`plottype'"',"area","line") {
        local TYPE shape
        local hasSHP 1
        local OPTS `OPTS' SIze(str asis) lock wmax WMAX2(numlist max=1 >0)/*
            */ COORdinates(namelist min=2 max=2) id(name)/*
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
            local OPTS `OPTS' COORdinates(namelist min=4 max=4)
            local YX  Y  X Y2 X2 // variable names used in plot data
        }
        else {  // scatter assumed
            local TYPE unit
            local OPTS `OPTS' COORdinates(namelist min=2 max=2)/*
                */ wmax WMAX2(numlist max=1 >0)
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
        qui syntax [varlist(default=none max=1 numeric fv)] [if] [in] `WGT'/*
            */ [, `OPTS' `Zopts' `PLVopts' `MLABopts' * ]
        // check zvar
        if `"`colorvar'"'!="" local zvar `colorvar'
        else                  local zvar `varlist'
        _parse_zvar `zvar' // returns zvar, discrete
        local hasZ = `"`zvar'"'!=""
        // varia
        if `"`box2'"'!="" local box box
        local hasMLAB = (`"`mlabi'"'!="") + ("`mlabz'"!="") + ("`mlabel'"!="")
        if `hasMLAB'>1 {
            di as err "only one of mlabel(), mlabi(), and mlabz allowed"
            exit 198
        }
        local cuts: list uniq cuts
        _parse_size `size' // => size, s_max, s_scale
        local hasSIZE = `"`size'"'!=""
        _parse_levels `levels' // => levels, method, l_wvar
        if `"`fcolor'"'!="" mata: _get_colors("fcolor")
        _parse_label `label' // => label, nolabel, format, reverse
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
        // check shpframe
        if `hasSHP' {
            geoframe get shpframe, local(shpframe)
            local hasSHP = `"`shpframe'"'!=""
        }
        if `hasSHP' {
            local org `touse'
            local tgt `touse'
        }
        else local shpframe `frame'
        // handle coordinates, PLV, and unit ID
        frame `shpframe' {
            if "`TYPE'"=="pc" local typeSHP pc // enforce pc
            else {
                geoframe get type, local(typeSHP)
                if `"`typeSHP'"'=="" local typeSHP `TYPE'
            }
            if "`coordinates'"!="" geoframe flip `coordinates', local(yx)
            else geoframe get coordinates, strict flip local(yx) `typeSHP'
            local ORG `yx'
            local TGT `YX'
            _get_plevel `hasPLV' `hasZ' "`plevel'" `"`color'"' `"`fcolor'"'
            if `hasPLV' {
                local ORG `ORG' `plevel'
                local TGT `TGT' PLV
            }
            if "`TYPE'"=="shape" & "`id'"=="" geoframe get id, local(id)
        }
        // handle weights
        local hasWGT = "`weight'"!=""
        if `hasWGT' {
            tempname wvar
            qui gen double `wvar' = abs(`exp') if `touse'
            markout `touse' `wvar' // excludes obs with missing weight!
            if `"`wmax2'"'=="" {
                su `wvar' if `touse', meanonly
                if "`wmax'"!="" local wmax2 = r(max)
                else            local wmax2 = max(1, r(max)) 
            }
            qui replace `wvar' = `wvar' / `wmax2' if `touse'
            if `hasSHP' {
                tempname WVAR
                local org `org' `wvar'
                local tgt `tgt' `WVAR'
            }
            else local WVAR `wvar'
            local ORG `ORG' `WVAR'
            local TGT `TGT' W
        }
        else local wgt
        // handle Z
        if `hasZ' {
            local zfmt: format `zvar'
            tempname ZVAR
            if `hasSHP' {
                local org `org' `zvar'
                local tgt `tgt' `ZVAR'
                local Zvar `ZVAR'
            }
            else local Zvar `zvar'
            local ORG `ORG' `ZVAR'
            local TGT `TGT' Z
            if "`l_wvar'"!="" {
                if `hasSHP' {
                    tempname L_WVAR
                    local org `org' `l_wvar'
                    local tgt `tgt' `L_WVAR'
                }
                else local L_WVAR `l_wvar'
           }
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
                    tempvar AREA
                    qui geoframe gen area `AREA', noset
                }
            }
            tempname svar
            qui gen double `svar' = abs(`size') / `AREA' if `touse'
            if `"`s_max'"'=="" {
                su `svar' if `touse', meanonly
                local s_max = r(max)
            }
            qui replace `svar' = `svar' * (`s_scale'/`s_max') if `touse'
            if `hasSHP' {
                tempname SVAR
                local Svar `SVAR'
                local org `org' `svar'
                local tgt `tgt' `SVAR'
            }
            else {
                local SVAR `svar'
                tepname Svar
            }
            local ORG `ORG' `SVAR'
            local TGT `TGT' `Svar' // tempvar only
        }
        // get centroids (used by weights and size() in area/line)
        if (`hasWGT' & ("`TYPE'"=="shape")) | `hasSIZE' | "`lock'"!="" {
            if "`centroids'"!="" geoframe flip `centroids', local(cYX)
            else {
                geoframe get centroids, flip local(cYX)
                if !`: list sizeof cYX' {
                    di as txt "layer `layer': centroids not found;"/*
                        */ " computing centroids on the fly"
                    di as txt "generate/declare centroids using " /*
                        */ "{helpb geoframe} to avoid such extra computations"
                    tempvar tmp_CX tmp_CY
                    qui geoframe gen centroids `tmp_CX' `tmp_CY', noset
                    local cYX `tmp_CY' `tmp_CX'
                }
            }
            if `hasSHP' {
                tempname CY CX
                local org `org' `cYX'
                local tgt `tgt' `CY' `CX'
            }
            else {
                gettoken CY cYX : cYX
                gettoken CX cYX : cYX
            }
            if "`lock'"!="" {
                local cY cY
                local cX cX
            }
            else if `hasSHP' {
                local cY `CY'
                local cX `CX'
            }
            else {
                tempname cY cX
            }
            local ORG `ORG' `CY' `CX'
            local TGT `TGT' `cY' `cX'
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
            local ORG `ORG' `MLAB'
            local TGT `TGT' MLAB
        }
        else local mlabcolor
        // select option
        if `"`select'"'!="" {
            tempname touse2
            qui gen byte `touse2' = 0
            qui replace `touse2' = 1 if `touse' & (`select')
            if `hasSHP' {
                local org `org' `touse2'
                local tgt `tgt' `touse2'
            }
        }
        // inject colors
        _process_coloropts options, `lcolor' `options'
        if `"`fcolor'"'!="" local fcolor fcolor(`fcolor')
    }
    // put frames together and categorize zvar
    frame `shpframe' {
        if `hasSHP' {
            // copy relevant variables from unit frame into shpframe
            geoframe copy `frame' `org', target(`tgt') quietly
            qui replace `touse' = 0 if `touse'>=. 
        }
        // categorize zvar
        if `hasZ' {
            if "`id'"!="" {
                // tag first obs per ID
                tempname ztouse
                qui gen byte `ztouse' = `touse'
                qui replace `ztouse' = 0/*
                    */ if `id'==`id'[_n-1] & `touse'==`touse'[_n-1]
            }
            else local ztouse `touse'
            tempname CUTS NOBS NMIS
            mata: _z_cuts("`CUTS'", "`Zvar'", "`L_WVAR'", "`ztouse'") /*
                fills in CUTS, returns levels */
            if "`discrete'"!="" & "`nolabel'"=="" {
                _z_labels `Zvar' `levels' `CUTS' // => zlabels
            }
            _z_categorize `CUTS' `NOBS' `NMIS', levels(`levels') zvar(`Zvar')/*
                */ gen(`ZVAR') touse(`touse') ztouse(`ztouse') `discrete'
                /* returns zlevels discrete */
        }
        // select
        if `"`select'"'!="" local touse `touse2'
    }
    // copy data into main frame
    local n0 = _N + 1
    qui geoframe append `shpframe' `ORG', target(`TGT') touse(`touse')
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
        qui replace Y = `cY' + (Y-`cY') * sqrt(`Svar') if `Svar'<. `in'
        qui replace X = `cX' + (X-`cX') * sqrt(`Svar') if `Svar'<. `in'
    }
    // process weights (area/line only)
    if `hasWGT' & ("`TYPE'"=="shape") {
        qui replace Y = `cY' + (Y-`cY') * sqrt(W) if W<. & `cY'<. & `cX'<. `in'
        qui replace X = `cX' + (X-`cX') * sqrt(W) if W<. & `cY'<. & `cX'<. `in'
    }
    // prepare PLV
    if `hasPLV' {
        if `"`ecolor'"'=="" local ecolor white // default for enclaves
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
        if `NMIS' _z_parse_missing `plottype' `hasMLAB', `missing'
    }
    else local zlevels 0
    // Set default options
    if `"`plottype'"'=="area" {
        local opts cmissing(n) nodropbase lalign(center) `opts'
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
        local opts `opts' cmissing(n)
        if "`COLOR'"=="" & `"`lcolor'"'=="" local opts lcolor(gray) `opts'
        if "`LWIDTH'"==""   local opts lwidth(.15) `opts'
        if "`LPATTERN'"=="" local opts lpattern(solid) `opts'
    }
    if `hasMLAB' {
        local opts `opts' mlabel(MLAB)
        if "`mlabvposition'"!="" local opts `opts' mlabvposition(MLABPOS)
    }
    // add box (below)
    if "`box'"!="" {
        _box `p' `n0' `n1' `"`TYPE'"', `box2'
        qui replace LAYER = `layer' in `n1'/l
    }
    else local plot
    // compile plot
    if `hasWGT' local in inrange(_n,`n0',`n1')) | (_n<3)
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
        foreach i of local zlevels {
            local IFF `iff'
            if `hasZ' {
                if `i'==0 {
                    local OPTS `opts' `missing' // missing
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
            if `"`OPTS'"'!="" {
                local IFF `IFF', `OPTS'
            }
            local plot `plot' (`plottype' `YX' `IFF')
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
        if `"`label'"'=="" local label `"`frame'"'
        _add_quotes label `label'
        if `:list sizeof label'>1 local label `"`"`label'"'"'
        local labels `"`label'"'
    }
    // mlabi()/mlabz
    if `"`mlabi'`mlabz'"'!="" {
        if "`mlabz'"!="" local MLBLS labels
        else             local MLBLS mlabi
        if `hasZ' {
            local MLABI
            foreach i of local zlevels {
                if `"`MLABI'"'=="" { // recycle
                    local MLABI: copy local `MLBLS'
                }
                gettoken mlbi MLABI : MLABI
                qui replace MLAB = `"`mlbi'"' in `n0'/`n1' if Z==`i'
            }
        }
        else {
            gettoken mlbi : `MLBLS'
            qui replace MLAB = `"`mlbi'"' in `n0'/`n1'
        }
    }
    // returns
    if `layer'<. {
        char LAYER[keys_`layer'] `keys'
        char LAYER[labels_`layer'] `"`labels'"'
        char LAYER[nolegend_`layer'] `nolegend'
        char LAYER[hasz_`layer'] `hasZ'
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
    if `"`0'"'=="" exit
    capt n __parse_levels `0'
    if _rc==1 exit _rc
    if _rc {
        di as err "(error in option levels())"
        exit _rc
    }
    c_local levels `levels'
    c_local method `method'
    c_local l_wvar `l_wvar'
end

program __parse_levels
    _parse comma n 0 : 0
    if `"`n'"'!="" {
        numlist `"`n'"', int min(0) max(1) range(>0)
        local n "`r(numlist)'"
    }
    else local n .
    local methods Quantiles Kmeans //Jenks
    syntax [, `methods' Weight(varname numeric) ]
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
    c_local levels `n'
    c_local method `method'
    c_local l_wvar `weight'
end

program _z_categorize
    syntax anything, levels(str) zvar(str) gen(str)/*
        */ touse(str) ztouse(str) [ discrete ]
    gettoken CUTS anything : anything
    gettoken NOBS anything : anything
    gettoken NMIS anything : anything
    //local nobs
    local discrete = "`discrete'"!=""
    tempname tmp
    local dtype byte
    if      `levels'>32740 local dtype long
    else if `levels'>100   local dtype int
    qui gen `dtype' `tmp' = .
    mata: _z_categorize(`levels', `discrete', "`CUTS'", "`NOBS'", "`NMIS'",/*
        */ "`zvar'", "`tmp'", "`ztouse'")
    if "`touse'"!="`ztouse'" {
        // fill in remaining obs of shape units
        qui replace `tmp' = `tmp'[_n-1] if `touse' & !`ztouse'
    }
    if "`zvar'"=="`gen'" drop `zvar'
    rename `tmp' `gen'
    c_local zlevels `zlevels'
    c_local discrete `discrete'
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
    syntax [, NOEXPAND n(passthru) IPolate(passthru)/*
        */ OPacity(passthru) INtensity(passthru) * ]
    if "`noexpand'"=="" local noexpand noexpand
    if `"`n'`ipolate'"'=="" local n n(`k')
    if `"`color'"'!="" {
        mata: _parse_colorspec("color") // check for kw, *#, %#
        if "`color_is_kw'"!="" {
            c_local `nm' `"`color'"'
            c_local sizeofel 1
            exit
        }
        if "`color_is_op'"!="" {
            if `"`opacity'"'=="" {
                local opacity opacity(`color_is_op')
                local color
            }
        }
        if "`color_is_in'"!="" {
            if `"`intensity'"'=="" {
                local intensity intensity(`color_is_in')
                local color
            }
        }
    }
    if `"`color'"'=="" {
        if `discrete' local color Set1
        else          local color viridis
    }
    colorpalette `color', nograph `noexpand' `n' `ipolate'/*
        */ `opacity' `intensity' `options'
    local color `"`r(p)'"'
    local pclass `"`r(pclass)'"'
    if `"`pclass'"'=="" & `discrete' local pclass "categorical"
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
    if `l'!=`k' {
        // recycle or interpolate colors if wrong number of colors
        colorpalette `color', nograph n(`k') class(`pclass')
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
    syntax [, NOLABel LABel(str asis) first nogap COLor(str asis)/*
        */ MLABColor(passthru) * ]
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
    c_local missing `options'
    c_local missing_color `"`color'"'
    c_local missing_lab   `"`label'"'
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
        // check whether 1st token is integer (possibly including wildcards)
        local l: subinstr local l "*" "", all
        local l: subinstr local l "?" "", all
        if `"`l'"'=="" local l 0
        capt confirm integer number `l'
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
    syntax [, ROTate CIRcle hull PADding(passthru) n(passthru) line/*
        */ box BOX2(passthru) SIze(passthru) COLORVar(passthru) * ]
    if `"`box'`box2'`size'`colorvar'"'!="" {
        di as err "box(): invalid syntax"
        exit 198
    }
    // copy relevant data into new frame
    tempname XY
    frame create `XY'
    mata: _box_copy_XY("`XY'", `n0', `n1', "`TYPE'"=="pc")
    // generate frame containing box
    tempname BOX
    frame `XY': qui geoframe bbox `BOX', `rotate' `circle' `hull' `padding' `n'
    // compile plot
    if "`line'"!="" local plottype line
    else            local plottype area
    _layer `plottype' . `p' `BOX', `options'
    // returns
    c_local plot `plot'
    c_local p `p'
end

version 16.1
mata:
mata set matastrict on

transmorphic vector _vecrecycle(real scalar k, transmorphic vector x)
{   // x must have at least one element
    real scalar r, c
    
    r = rows(x); c = cols(x)
    if (r>c) return(J(ceil(k/c), 1, x)[|1\k|]) // => rowvector if x is 1x1
    return(J(1, ceil(k/c), x)[|1\k|])
}

void _z_cuts(string scalar CUTS, string scalar zvar, string scalar wvar,
    string scalar touse)
{
    string scalar  cuts, method
    real scalar    discrete, k, lb, ub
    real rowvector minmax
    real colvector C, X, w, p
    
    // CASE 1: cuts() specified
    discrete = st_local("discrete")!=""
    cuts     = st_local("cuts")
    if (cuts!="") { // cuts() specified
        C = strtoreal(tokens(cuts)')
        k = length(C)
        if (!discrete) k = k - 1
        st_matrix(CUTS, C')
        st_local("levels", strofreal(k))
        return
    }
    // CASE 2: discrete
    if (discrete) { // discrete specified
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
    // get requested number of levels and method
    k = strtoreal(st_local("levels"))
    if (k>=.) k = 5 // default number of levels
    method = st_local("method")
    // SPECIAL CASE 2: no variance
    lb = minmax[1]; ub = minmax[2]
    if (lb==ub) {
        st_matrix(CUTS, minmax)
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

void _z_categorize(real scalar k, real scalar discrete, string scalar cuts,
    string scalar nobs, string scalar nmis, string scalar zvar,
    string scalar ztmp, string scalar touse)
{
    real scalar    i, n, m, c0, c1
    real rowvector C, N
    real colvector Z, T, p
    
    C = st_matrix(cuts)
    N = J(1, k, .)
    st_view(Z=., ., zvar, touse)
    st_view(T=., ., ztmp, touse)
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
    p = selectindex(Z:>=.)
    m = length(p)
    if (m) {
        T[p] = J(m,1,0) // set missings to 0
        st_local("zlevels", invtokens(strofreal(0..k)))
    }
    else if (k) st_local("zlevels", invtokens(strofreal(1..k)))
    else        st_local("zlevels", "")
    st_matrix(nobs, N)
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

void _get_colors(string scalar lname, | string scalar lname2)
{   /* function assumes that global ColrSpace object "_GEOPLOT_ColrSpace_S"
       exists; maintaining a global is less work than initializing ColrSpace
       in each call */
    real scalar      i
    string scalar    c
    string rowvector C, kw1, kw2
    pointer (class ColrSpace scalar) scalar S
    
    if (args()<2) lname2 = lname
    kw1 = ("none", ".", "bg", "fg", "background", "foreground")
    kw2 = ("*", "%")
    S = findexternal("_GEOPLOT_ColrSpace_S")
    //if ((S = findexternal("_GEOPLOT_ColrSpace_S"))==NULL) S = &(ColrSpace())
    C = tokens(st_local(lname2))
    i = length(C)
    for (;i;i--) {
        c = C[i]
        if (anyof(kw1, c)) continue
        if (anyof(kw2, substr(c,1,1))) continue
        S->colors("`"+`"""' + C[i] + `"""'+"'")
        C[i] = S->colors()
    }
    st_local(lname, invtokens(C))
}

void _parse_colorspec(string scalar lname)
{
    real scalar      l
    string rowvector c
    
    c = tokens(st_local(lname))
    if (length(c)!=1) return
    if (anyof(("none", ".", "bg", "fg", "background", "foreground"), c)) {
        st_local("color_is_kw","1")
        return
    }
    if (substr(c,1,1)=="%") {
        l = strpos(c,"*")
        if (l) {
            st_local("color_is_op", substr(c,2,l-2))
            st_local("color_is_in", substr(c,l+1,.))
        }
        else st_local("color_is_op", substr(c,2,.))
        return
    }
    if (substr(c,1,1)=="*") {
        l = strpos(c,"%")
        if (l) {
            st_local("color_is_in", substr(c,2,l-2))
            st_local("color_is_op", substr(c,l+1,.))
        }
        else st_local("color_is_in", substr(c,2,.))
        return
    }
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

