*! version 1.1.5  17oct2023  Ben Jann

program geoframe, rclass
    version 16.1
    gettoken subcmd 0 : 0, parse(" ,")
    _parse_subcmd `subcmd'
    _geoframe_`subcmd' `macval(0)'
    if `"`local'"'!="" { // pass through returns (_geoframe_get, _geoframe_flip)
        c_local `local' `"`value'"'
    }
    if "`subcmd'"=="query" return add
end

program _parse_subcmd
    local l = strlen(`"`0'"')
    if      `"`0'"'=="get"                             local 0 get
    else if `"`0'"'=="set"                             local 0 set
    else if `"`0'"'=="flip"                            local 0 flip // undocumented
    else if `"`0'"'==substr("create",   1, max(2,`l')) local 0 create
    else if `"`0'"'==substr("link",     1, max(1,`l')) local 0 link
    else if `"`0'"'==substr("clean",    1, max(2,`l')) local 0 clean
    else if `"`0'"'==substr("select",   1, max(3,`l')) local 0 select
    else if `"`0'"'==substr("query",    1, max(1,`l')) local 0 query
    else if `"`0'"'==substr("describe", 1, max(1,`l')) local 0 describe
    else if `"`0'"'==substr("generate", 1, max(1,`l')) local 0 generate
    else if `"`0'"'==substr("bbox",     1, max(2,`l')) local 0 bbox
    else if `"`0'"'==substr("symbol",   1, max(3,`l')) local 0 symbol
    else if `"`0'"'==substr("rename",   1, max(3,`l')) local 0 rename
    else if `"`0'"'==substr("duplicate",1, max(3,`l')) local 0 duplicate
    else if `"`0'"'==substr("relink",   1, max(3,`l')) local 0 relink
    else if `"`0'"'==substr("unlink",   1, max(3,`l')) local 0 unlink
    else if `"`0'"'==substr("attach",   1, max(2,`l')) local 0 attach
    else if `"`0'"'==substr("detach",   1, max(3,`l')) local 0 detach
    else if `"`0'"'==substr("append",   1, max(2,`l')) local 0 append
    else if `"`0'"'==substr("translate",1, max(2,`l')) local 0 translate
    capt mata: assert(st_islmname(st_local("0")))
    if _rc==1 exit _rc
    if _rc {
        di as err `"`0' invalid subcommand"'
        exit 198
    }
    c_local subcmd `"`0'"'
end

program _geoframe_create
    // syntax
    syntax [anything(equalok)] [using] [, * ]
    if `:list sizeof using'==0 {
        if `:list sizeof anything'>1 {
            gettoken frame anything : anything
        }
        if `:list sizeof anything' {
            local anything `"using `macval(anything)'"'
        }
        local 0 `macval(frame)' `macval(anything)', `macval(options)'
    }
    syntax [name(id="frame name" name=frame)] [using/] [, replace/*
        */ type(str) id(name) sid(name) pid(name) PLevel(name)/*
        */ COordinates(namelist) CENtroids(namelist) area(name)/*
        */ Feature(str asis)/*
        */ noShpfile Shpfile2(str asis) nodrop /*
        */ noDEScribe noCURrent _mkfrlink(str) ]
    __geoframe_create_parse_type, `type'
    local isSHPread = `"`_mkfrlink'"'!=""
    local hasUSING = `"`macval(using)'"'!=""
    if `"`frame'"'=="" {
        if `hasUSING' {
            // use base name of file as frame name
            mata: st_local("frame",/*
                */ ustrtoname(pathrmsuffix(pathbasename(st_local("using")))))
        }
        else local frame `"`c(frame)'"' // use current frame
    }
    _parse comma shpf shpopts : shpfile2
    capt n __geoframe_create_parse_shpopts `shpopts'
    if _rc==1 exit 1
    if _rc {
        di as err "(error in {bf:shpfile()})"
        exit _rc
    }
    local hasSHP = `"`macval(shpf)'"'!=""
    if `hasSHP' {
        if "`shpfile'"!="" {
            di as err "{bf:noshpfile} and {bf:shpfile()} not both allowed"
            exit 198
        }
        if "`type'"!="`shape'" {
            di as err "only one of {bf:shape} and {bf:shpfile()} allowed"
            exit 198
        }
        capt n __geoframe_create_parse_shpf `macval(shpf)'
        if _rc==1 exit 1
        if _rc {
            di as err "(error in {bf:shpfile()})"
            exit _rc
        }
        mata: _add_path_and_sfx_to_shpf("SHP_using", "using")
    }
    
    // read data into frame and apply settings
    nobreak {
        if "`replace'"!="" & `hasUSING' {
            capt confirm new frame `frame'
            if _rc==110 { // frame already exists
                tempname tmpframe
                frame rename `frame' `tmpframe'
            }
            else if _rc exit _rc
        }
        if `hasUSING' frame create `frame'
        capture noisily break {
            frame `frame' {
                if `hasUSING' use `"`macval(using)'"'
                _geoframe_set id `id'
                __geoframe_create_type "`type'" `hasSHP' `"`coordinates'"'
                _geoframe_set type `type'
                _geoframe_set sid `sid'
                _geoframe_set pid `pid'
                _geoframe_set plevel `plevel'
                _geoframe_set centroids `centroids'
                _geoframe_set coordinates `coordinates'
                _geoframe_set area `area'
                _geoframe_set feature `feature'
                // check whether shapefile is available
                if "`type'"=="unit" {
                    if `hasSHP'==0 & "`shpfile'"=="" {
                        if `hasUSING' {
                            __geoframe_create_hasshpf `macval(using)'
                        }
                    }
                }
                // read shapefile and establish linkage
                if `hasSHP' {
                    di as txt `"(reading shapes from `macval(SHP_using)')"'
                    if "`SHP_frame'"=="" local SHP_frame "`frame'_shp"
                    else if "`SHP_frame'"=="`frame'" {
                        di as err "name for shape frame and"/*
                            */ " name for main frame may not be the same"
                        exit 498
                    }
                    _geoframe_create `SHP_frame' using `"`macval(SHP_using)'"'/*
                        */, type(shape) id(`SHP_id') sid(`SHP_sid') pid(`SHP_pid')/*
                        */  plevel(`SHP_plevel') coordinates(`SHP_coord')/*
                        */  feature(`SHP_feat') `replace'/*
                        */  nodescribe nocurrent _mkfrlink(`frame') `drop'
                }
                else if `isSHPread' {
                    if "`drop'"!="" local clean
                    else            local clean , clean
                    frame `_mkfrlink': _geoframe_link `frame'`clean'
                }
                else {
                    __geoframe_set_shpframe
                    __geoframe_set_linkname
                }
            }
        }
        if _rc {
            local rc = _rc // (because following commands seem to reset _rc)
            if `hasUSING' {
                frame drop `frame'
                if "`tmpframe'"!="" {
                    frame rename `tmpframe' `frame'
                }
            }
            exit `rc'
        }
    }
    
    // reporting and frame change
    if "`current'"=="" {
        if "`frame'"!=`"`c(frame)'"' {
            frame change `frame'
            di as txt "(current frame now {bf:`frame'})"
        }
    }
    if "`describe'"=="" _geoframe_describe `frame'
end

program __geoframe_create_parse_type
    syntax [, unit pc shape ]
    local type `unit' `pc' `shape'
    if `: list sizeof type'>1 {
        di as err "{bf:type()}: only one of {bf:unit}, {bf:shape}, and {bf:pc} allowed"
        exit 198
    }
    c_local type `type'
end

program __geoframe_create_parse_shpopts
    syntax [, id(name) sid(name) pid(name) PLevel(name)/*
        */ COordinates(namelist) Feature(str asis) ]
    c_local SHP_id      `"`id'"'
    c_local SHP_sid     `"`sid'"'
    c_local SHP_pid     `"`pid'"'
    c_local SHP_plevel  `"`plevel'"'
    c_local SHP_coord   `"`coordinates'"'
    c_local SHP_feat    `"`feature'"'
end

program __geoframe_create_parse_shpf
    syntax [anything(equalok)] [using]
    if `:list sizeof using'==0 {
        if `:list sizeof anything'>1 {
            gettoken frame anything : anything
        }
        local 0 `macval(frame)' using `macval(anything)'
    }
    syntax [name(id="frame name" name=frame)] using/
    c_local SHP_frame  `"`frame'"'
    c_local SHP_using  `"`macval(using)'"'
end

program __geoframe_create_hasshpf
    if `"`macval(0)'"'=="" exit
    mata: st_local("shpf", pathrmsuffix(st_local("0")) + "_shp.dta")
    capt confirm file `"`macval(shpf)'"'
    if _rc==1 exit 1
    if _rc exit
    c_local SHP_using `"`macval(shpf)'"'
    c_local hasSHP 1
end

program __geoframe_create_type
    args type hasSHP coord
    if "`type'"!="" exit // already set
    capt unab coord: `coord', min(4) max(4)
    if `: list sizeof coord'==4 local type "pc"   // 4 coordinates => pc
    else if `hasSHP'            local type "unit" // shpfile() => unit
    else {
        geoframe get id, local(id)
        if `"`id'"'!="" {
            capt isid `id', missok
            if _rc local type "shape"             // id not unique => shape
            else   local type "unit"              // id unique => unit
        }
        else local type "unit"                    // no id => unit
    }
    c_local type `type'
end

program _geoframe_link
    local frame `"`c(frame)'"'
    syntax name(id="shape frame" name=shpframe) [, CLean CLean2(str) ]
    if `"`shpframe'"'==`"`frame'"' {
        di as err "{it:shpframe} must be different from current frame"
        exit 498
    }
    confirm frame `shpframe'
    geoframe get shpframe, local(oldshpframe)
    if `"`oldshpframe'"'!="" geoframe unlink
    frame `shpframe' {
        geoframe get id, local(shpid)
        if `"`shpid'"'=="" {
            di as err "no ID variable defined in {it:shpframe}"
            exit 498
        }
        // create name for linkage variable
        local i 1
        while (1) {
            local lnkvar _GEOFRAME_lnkvar_`i'
            capt confirm new variable `lnkvar'
            if _rc==1 exit _rc
            if _rc local ++i        // name already exists
            else   continue, break  // name ok
        }
    }
    geoframe get id, local(id)
    if `"`id'"'=="" {
        di as err "no ID variable defined in current frame"
        exit 498
    }
    frame `shpframe': frlink m:1 `shpid', frame(`frame' `id') generate(`lnkvar')
    __geoframe_set_shpframe `shpframe'
    __geoframe_set_linkname `lnkvar'
    if `"`clean'`clean2'"'!="" {
        geoframe clean, quietly `clean2'
    }
    di as txt "(link to frame {bf:`shpframe'} added)"
end

program __geoframe_link_parse_clean
    syntax [, Shapes shp Units ]
    if "`shp'"!="" local shapes shapes
    c_local clean_shp  = "`shapes'`shp'"!=""
    c_local clean_unit = "`units'"!=""
end

program _geoframe_clean
    local frame `"`c(frame)'"'
    syntax [, Shapes shp Units NOEmpty Emptyonly quietly ] // quietly undocu
    if "`shp'"!="" local shapes shapes
    if "`shapes'`units'"=="" {
        local shapes shapes
        local units units
    }
    geoframe get shpframe, local(shpframe)
    if `"`shpframe'"'=="" {
        di as txt "(nothing to do; no link to shape frame found)"
        exit
    }
    geoframe get id, local(id) strict
    geoframe get linkname, local(lnkvar) strict
    frame `shpframe': geoframe get id, local(shpid) strict
    local lnkupdate 0
    // identify empty shapes (among the linked shapes)
    if "`noempty'"=="" {
        tempvar empty
        frame `shpframe' {
            tempvar empty
            qui gen `empty' = 0
            geoframe get coordinates, local(coord)
            if `"`coord'"'!="" {
                tempvar touse
                qui gen `touse' = `lnkvar'<.
                mata: _clean_empty("`empty'", "`shpid'", tokens(`"`coord'"'),/*
                    */ "`touse'")
            }
            else local Nempty 0
         }
    }
    else local empty
    // create reverse link from shape file to unit file
    if "`units'"!="" | "`noempty'"=="" {
        tempname tmpframe tag
        frame `shpframe' {
            qui gen byte `tag' = `lnkvar'<. & (_n==1 | `shpid'!=`shpid'[_n-1])
            frame put `shpid' `empty' if `tag', into(`tmpframe')
        }
        qui frlink 1:1 `id', frame(`tmpframe' `shpid')
    }
    // drop obs from shape file
    if "`shapes'"!="" & "`emptyonly'"=="" {
        frame `shpframe' {
            qui drop if `lnkvar'>=.
            if r(N_drop) {
                if r(N_drop)==1 local msg observation
                else            local msg observations
                local Ndrop `: di %9.0gc `r(N_drop)''
                di as txt "(dropped `Ndrop' unmatched `msg'"/*
                    */ " in frame {bf:`shpframe'})"
            }
            else {
                `quietly' di as txt /*
                    */ "(no unmatched observations in frame {bf:`shpframe'})"
            }
        }
    }
    // drop obs from unit file
    if "`units'"!="" & "`emptyonly'"=="" {
        qui drop if `tmpframe'>=.
        if r(N_drop) {
            local lnkupdate 1
            if r(N_drop)==1 local msg observation
            else            local msg observations
            local Ndrop `: di %9.0gc `r(N_drop)''
            di as txt "(dropped `Ndrop' unmatched `msg'"/*
                */ " in frame {bf:`frame'})"
        }
        else {
            `quietly' di as txt /*
                */ "(no unmatched observations in frame {bf:`frame'})"
        }
    }
    // drop empty shapes
    if "`noempty'"=="" {
        if `Nempty' {
            if "`shapes'"!="" {
                frame `shpframe' {
                    qui drop if `empty'==1
                    if `Nempty'==1 local msg shape
                    else           local msg shapes
                    di as txt "(dropped `Nempty' empty `msg'"/*
                        */ " in frame {bf:`shpframe'})"
                }
            }
            if "`units'"!="" {
                qui frget `empty' = `empty', from(`tmpframe')
                qui drop if `empty'==1
                if `Nempty'==1 local msg unit
                else           local msg units
                di as txt "(dropped `Nempty' empty-shape `msg'"/*
                    */ " in frame {bf:`frame'})"
            }
            local lnkupdate 1
        }
        else {
            `quietly' di as txt "(no empty shapes)"
        }
    }
    if `lnkupdate' qui geoframe link `shpframe'
end

program _geoframe_query
    gettoken fnc : 0, parse(" ,")
    if inlist(`"`fnc'"',"if","in") {
        local fnc ""
    }
    else {
        gettoken fnc 0 : 0, parse(" ,")
    }
    local fnc = strlower(`"`fnc'"')
    local l = strlen(`"`fnc'"')
    if `l'==0                                                local fnc n
    else if `"`fnc'"'=="n"                                   local fnc n
    else if `"`fnc'"''==substr("orientation", 1, max(2,`l')) local fnc orientation
    else if `"`fnc'"''==substr("direction", 1, max(3,`l'))   local fnc orientation
    else if `"`fnc'"''==substr("gtype", 1, max(2,`l'))       local fnc gtype
    else if `"`fnc'"''==substr("bbox", 1, max(2,`l'))        local fnc bbox
    else {
        capt mata: assert(st_islmname(st_local("fnc")))
        if _rc==1 exit 1
        if _rc {
            di as err `"`fnc': invalid function"'
            exit 198
        }
    }
    __geoframe_query_`fnc' `0'
end

program __geoframe_query_n, rclass
    syntax [if] [in]
    marksample touse
    geoframe get id, local(id) strict
    tempvar tmp
    qui gen `tmp' = _n==1 | `id'!=`id'[_n-1]
    su `tmp' if `touse', meanonly
    local units = r(sum)
    geoframe get type, l(type)
    if "`type'"!="shape" geoframe get shpframe, local(shpframe)
    if `"`shpframe'"'!="" {
        geoframe get linkname, local(lnkvar) strict
        tempvar PID
        qui geoframe generate pid `PID', noset
        frame `shpframe' {
            geoframe get id, local(id) strict
            qui frget `touse' = `touse', from(`lnkvar')
            qui replace `touse' = 0 if `touse'>=.
            tempvar tmp
            qui gen `tmp' = _n==1 | `id'!=`id'[_n-1]
            su `tmp' if `touse', meanonly
            local units_shp = r(sum)
            qui replace `tmp' = (`PID'!=`PID'[_n-1]) | `tmp'
            su `tmp' if `touse', meanonly
            local shapes = r(sum)
            su `PID' if `touse' & (_n==_N | `id'!=`id'[_n+1]), meanonly
            local min = r(min)
            local max = r(max)
            if `units_shp'<`units' local min 0
        }
    }
    else {
        geoframe get type, l(type)
        if "`type'"!="shape" {
            di as txt "(shape frame not found;"/*
                */ " treating current frame as shape frame)"
        }
        local units_shp `units'
        tempvar PID
        qui geoframe generate pid `PID', noset
        qui replace `tmp' = ( `PID'!=`PID'[_n-1]) | `tmp'
        su `tmp' if `touse', meanonly
        local shapes = r(sum)
        su `PID' if `touse' & (_n==_N | `id'!=`id'[_n+1]), meanonly
        local min = r(min)
        local max = r(max)
    }
    local avg = `shapes' / `units'
    di as txt ""
    di as txt "  Number of units               = " as res %9.0g `units'
    di as txt "  Number of shape items         = " as res %9.0g `shapes'
    di as txt "  Min number of items per unit  = " as res %9.0g `min'
    di as txt "  Avg number of items per unit  = " as res %9.0g `avg'
    di as txt "  Max number of items per unit  = " as res %9.0g `max'
    return scalar max = `max'
    return scalar avg = `avg'
    return scalar min = `min'
    return scalar items = `shapes'
    return scalar units  = `units'
end

program __geoframe_query_orientation
    ___geoframe_query_items orientation `0'
end

program __geoframe_query_gtype
    ___geoframe_query_items gtype `0'
end

program ___geoframe_query_items, rclass
    gettoken fcn 0 : 0
    syntax [if] [in]
    marksample touse
    local cframe `"`c(frame)'"'
    geoframe get type, l(type)
    if "`type'"=="shape" local shpframe `"`cframe'"'
    else {
        geoframe get shpframe, local(shpframe)
        if `"`shpframe'"'=="" {
            di as txt "(shape frame not found;"/*
                */ " treating current frame as shape frame)"
            local shpframe `"`cframe'"'
        }
        else {
            geoframe get linkname, local(lnkvar) strict
            frame `shpframe' {
                qui frget `touse' = `touse', from(`lnkvar')
                qui replace `touse' = 0 if `touse'>=.
            }
        }
    }
    frame `shpframe' {
        geoframe get id, local(ID)
        if `"`ID'"'=="" {
            tempvar ID
            qui gen byte `ID' = 1
        }
        tempvar PID
        qui geoframe generate pid `PID', noset
        geoframe get coord, local(XY) strict
        tempname R
        mata: _`fcn'("`R'", (`"`ID'"', "`PID'"), tokens(`"`XY'"'),/*
                */ "`touse'")
    }
    di as txt ""
    if "`fcn'"=="gtype" {
        di as txt "  Polygons        = " as res %9.0g `R'[1,1]
        di as txt "  Lines           = " as res %9.0g `R'[2,1]
        di as txt "  Points          = " as res %9.0g `R'[3,1]
        di as txt "  Empty items     = " as res %9.0g `R'[4,1]
        di as txt "  Number of items = " as res %9.0g/*
            */ `R'[1,1] + `R'[2,1] + `R'[3,1] + `R'[4,1]
        return scalar empty    = `R'[4,1]
        return scalar points   = `R'[3,1]
        return scalar lines    = `R'[2,1]
        return scalar polygons = `R'[1,1]
        return scalar items    = `R'[1,1] + `R'[2,1] + `R'[3,1] + `R'[4,1]
    }
    else {
        di as txt "  Clockwise (negative)        = " as res %9.0g `R'[1,1]
        di as txt "  Counterclockwise (positive) = " as res %9.0g `R'[2,1]
        di as txt "  Orientation undefined       = " as res %9.0g `R'[3,1]
        di as txt "  Number of shape items       = " as res %9.0g/*
            */ `R'[1,1] + `R'[2,1] + `R'[3,1]
        return scalar null  = `R'[3,1]
        return scalar pos   = `R'[2,1]
        return scalar neg   = `R'[1,1]
        return scalar items = `R'[1,1] + `R'[2,1] + `R'[3,1]
    }
end

program __geoframe_query_bbox, rclass
    syntax [if] [in] [, noShp ROTate CIRcle hull PADding(passthru)/*
        */ n(passthru) ANGle(passthru) noADJust ]
    // obtain bounding box
    tempname bbox
    qui geoframe bbox `bbox' `if' `in', `shp' `rotate' `circle' `hull'/*
        */ `padding' `n' `angle' `noadjust' 
    tempname BBOX LIMITS R
    frame `bbox' {
        mat `LIMITS' = J(1,4,.)
        mat coln `LIMITS' = xmin xmax ymin ymax
        su _X, meanonly
        mat `LIMITS'[1,1] = r(min)
        mat `LIMITS'[1,2] = r(max)
        su _Y, meanonly
        mat `LIMITS'[1,3] = r(min)
        mat `LIMITS'[1,4] = r(max)
        mkmat _X _Y in 2/l, matrix(`BBOX')
        mat coln `BBOX' = X Y
    }
    // display
    mat `R' = J(2,3,.)
    mat coln `R' = Minimum Maximum Midpoint
    mat rown `R' = X Y
    mat `R'[1,1] = `LIMITS'[1,1]
    mat `R'[1,2] = `LIMITS'[1,2]
    mat `R'[1,3] = (`LIMITS'[1,1] + `LIMITS'[1,2])/2
    mat `R'[2,1] = `LIMITS'[1,3]
    mat `R'[2,2] = `LIMITS'[1,4]
    mat `R'[2,3] = (`LIMITS'[1,3] + `LIMITS'[1,4])/2
    // display
    di _n as txt "Coordinates of bounding box saved in "/*
        */ "{stata mat list r(bbox):{bf:r(bbox)}}."
    di as txt "Limits of bounding box saved in "/*
        */ "{stata mat list r(limits):{bf:r(limits)}}."
    matlist `R', border(rows) rowtitle(Limits)
    // returns
    ret scalar ymid = `R'[2,3]
    ret scalar ymax = `R'[2,2]
    ret scalar ymin = `R'[2,1]
    ret scalar xmid = `R'[1,3]
    ret scalar xmax = `R'[1,2]
    ret scalar xmin = `R'[1,1]
    ret matrix limits = `LIMITS'
    ret matrix bbox = `BBOX'
end

program _geoframe_describe
    syntax [name(id="frame name" name=frame)] 
    if "`frame'"=="" local frame `"`c(frame)'"'
    // collect chars
    frame `frame' {
        local allchars type feat id coord centr area sid pid plevel shpf
        foreach char of local allchars {
            geoframe get `char', local(`char')
        }
    }
    // chars to print even if empty
    local chars type feat id coord
    if `"`type'"'=="shape"   local chars `chars' sid pid plevel
    else if `"`type'"'=="unit" {
                             local chars `chars' area shpf
                             local centr
    }
    else if `"`type'"'=="pc" local chars `chars' shpf
    else                     local chars `allchars'
    // process chars
    foreach char of local allchars {
        if "`char'"=="shpf" {
            if `"`shpf'"'=="" {
                if !`:list char in chars' continue
                local shpf "<none>"
            }
            else local shpf `"{stata geoframe describe `shpf':{bf:`shpf'}}"'
            continue
        }
        if `"``char''"'=="" {
            if !`:list char in chars' continue
            local `char' "<none>"
        }
        else local `char' `"{bf:``char''}"'
    }
    // number of obs
    frame `frame': qui count
    local nobs `:di %18.0gc r(N)'
    // display
    di
    __geoframe_describe_di "Frame name"             `"{bf:`frame'}"'
    __geoframe_describe_di "Frame type"             `"`type'"'
    __geoframe_describe_di "Feature type"           `"`feat'"'
    __geoframe_describe_di "Number of obs"          `"{bf:`nobs'}"'
    __geoframe_describe_di "Unit ID"                `"`id'"'
    __geoframe_describe_di "Coordinates"            `"`coord'"'
    __geoframe_describe_di "Centroids"              `"`centr'"'
    __geoframe_describe_di "Area"                   `"`area'"'
    __geoframe_describe_di "Within-unit sort ID"    `"`sid'"'
    __geoframe_describe_di "Within-unit polygon ID" `"`pid'"'
    __geoframe_describe_di "Plot level ID"          `"`plevel'"'
    __geoframe_describe_di "Linked shape frame"     `"`shpf'"'
end

program __geoframe_describe_di
    args lbl txt
    if `"`txt'"'=="" exit
    di as txt %22s `"`lbl'"' ": " `"`txt'"'
end

program _geoframe_select
    syntax [if] [in] [, into(namelist max=2) NOShp UNLink replace/*
        */ noDEScribe noCURrent ]
    local hasIF = `"`if'`in'"'!=""
    // get settings of current frame
    local frame `"`c(frame)'"'
    geoframe get shpframe, local(shpframe)
    local hasSHP = "`shpframe'"!=""
    // parse into()
    local newFRM 0
    local newSHP 0
    if "`into'"!="" {
        gettoken newname into : into
        local newFRM = "`newname'"!=`"`frame'"'
        if "`replace'"=="" confirm new frame `newname'
        if `hasSHP' {
            if "`newname'"==`"`shpframe'"' {
                di as err "{it:newname} must be different from"/*
                    */ " name of linked shape frame"
                exit 198
            }
            if `"`noshp'`unlink'"'=="" {
                gettoken newshpname : into
                if "`newshpname'"=="" local newshpname `newname'_shp
                local newSHP = "`newshpname'"!=`"`shpframe'"'
                if "`replace'"=="" confirm new frame `newshpname'
                if "`newshpname'"==`"`frame'"' {
                    di as err "{it:newshpname} must be different from"/*
                        */ " name of current frame"
                    exit 198
                }
                if "`newshpname'"=="`newname'" {
                    di as err "{it:newshpname} must be different from"/*
                        */ " {it:newname}"
                    exit 198
                }
            }
        }
    }
    if `newFRM'==0 local newname `frame'
    if `newSHP'==0 local newshpname `shpframe'
    // make copies of frames if necessary
    if `newFRM' {
        capt confirm new frame `newname'
        if _rc==1 exit 1
        if _rc frame drop `newname'
        frame copy `frame' `newname'
        // remove all linkage info in copied frame
        frame `newname' {
            __geoframe_set_shpframe
            __geoframe_set_linkname
            capt drop _GEOFRAME_lnkvar_*
        }
        di as txt "(new frame {bf:`newname'} created)"
    }
    if `newSHP' {
        capt confirm new frame `newshpname'
        if _rc==1 exit 1
        if _rc frame drop `newshpname'
        frame copy `shpframe' `newshpname'
        // remove all linkage info in copied frame
        frame `newshpname' {
            __geoframe_set_shpframe
            __geoframe_set_linkname
            capt drop _GEOFRAME_lnkvar_*
        }
        di as txt "(new shape frame {bf:`newshpname'} created)"
    }
    // apply selection and establish linkage
    local Ndrop 0
    local Ndropshp .
    frame `newname' {
        if `hasIF' {
            qui keep `if' `in'
            local Ndrop = r(N_drop)
        }
        if `hasSHP' {
            if "`unlink'"=="" {
                qui geoframe link `newshpname'
                if "`noshp'"=="" {
                    local Ndropshp 0
                    if `hasIF' {
                        geoframe get linkname, local(lnkvar)
                        frame `newshpname' {
                            qui keep if `lnkvar'<.
                            local Ndropshp = r(N_drop)
                        }
                    }
                }
            }
            else if `newFRM'==0 {
                geoframe unlink `shpframe'
            }
        }
    }
    // reporting and frame change
    if `hasIF' {
        if `Ndropshp'<. {
            if `Ndropshp'==1 local msg observation
            else             local msg observations
            local tmp `: di %9.0gc `Ndropshp''
            di as txt "(dropped `tmp' `msg' in frame {bf:`newshpname'})"
        }
        if `Ndrop'==1 local msg observation
        else          local msg observations
        local tmp `: di %9.0gc `Ndrop''
        di as txt "(dropped `tmp' `msg' in frame {bf:`newname'})"
    }
    if `newFRM' {
        if "`current'"=="" {
            frame change `newname'
            di as txt "(current frame now {bf:`newname'})"
        }
    }
    if "`describe'"=="" _geoframe_describe `newname'
end

program _geoframe_clip
    // syntax
    syntax anything(name=matname id="matname") [if] [in] [, * ]
    tempname MASK
    confirm matrix `matname'
    mat `MASK' = `matname'
    __geoframe_manipulate clip `if' `in', mask(`MASK') `options'
end

program _geoframe_rclip
    syntax anything(name=limits id="limits") [if] [in] [, * ]
    tempname MASK
    local ismat 0
    if `: list sizeof limits'==1 {
        capt confirm matrix `limits'
        if _rc==0 local ismat 1
    }
    if !`ismat' {
        numlist `"`limits'"', min(0) max(4) missingokay
        gettoken xmin limits : limits
        if "`xmin'"=="" local xmin .
        gettoken xmax limits : limits
        if "`xmax'"=="" local xmax .
        gettoken ymin limits : limits
        if "`ymin'"=="" local ymin .
        gettoken ymax limits : limits
        if "`ymax'"=="" local ymax .
        matrix `MASK' = (`xmin', `xmax', `ymin', `ymax')
    }
    else {
        matrix `MASK' = `limits : limits'
        if rowsof(`MASK')!=1 matrix `MASK' = vec(`MASK'')'
        local c: colsof `MASK'
        if `c'<4      matrix `MASK' = `MASK', J(1, 4-`c', .)
        else if `c'>4 matrix `MASK' = `MASK'[1,1..4]
    }
    if `MASK'[1,1]<. & `MASK'[1,2]<`MASK'[1,1] {
        di as err "{it:xmax} must be larger than {it:xmin}"
        esit 198
    }
    if `MASK'[1,3]<. & `MASK'[1,4]<`MASK'[1,3] {
        di as err "{it:ymax} must be larger than {it:ymin}"
        esit 198
    }
    __geoframe_manipulate clip `if' `in', mask(`MASK') rclip `options'
end

program _geoframe_simplify
    __geoframe_manipulate simplify `0'
end

program _geoframe_bshare
    __geoframe_manipulate bshare `0'
end

program __geoframe_manipulate
    // syntax
    gettoken subcmd 0 : 0
    local opts nodrop noDOTs into(namelist max=2) replace noCURrent
    if "`subcmd'"=="clip" {
        syntax [if] [in], mask(str) [ rclip/*
            */ Line noCLip STrict SPlit `opts' ]
    }
    else if "`subcmd'"=="simplify" {
        syntax [anything(id="delta" name=delta)] [if] [in] [,/*
            */ ABSolute JOINTly `opts' ]
        if "`delta'"!="" {
            numlist `"`delta'"', max(1) range(>=0)
            local delta `r(numlist)'
        }
    }
    else if "`subcmd'"=="bshare" {
        syntax [if] [in] [, NOT `opts' ]
    }
    else exit 198
    // mark sample
    marksample touse
    // into()
    if `"`into'"'!="" {
        gettoken frame : into
        geoframe select `if' `in', into(`into') `replace' nodescribe nocurrent 
    }
    else local frame `"`c(frame)'"'
    // check for shape frame
    frame `frame' {
        geoframe get shpframe, local(shpframe)
        local hasSHP = "`shpframe'"!=""
        if `hasSHP' {
            geoframe get id, local(id) strict
            geoframe get linkname, local(lnkvar) strict
            frame `shpframe' {
                geoframe get id, local(shpid) strict
                qui frget `touse' = `touse', from(`lnkvar')
                qui replace `touse' = 0 if `touse'>=.
            }
        }
        else local shpframe `frame'
    }
    // apply procedure
    frame `shpframe' {
        if "`subcmd'"=="clip" {
            ___geoframe_clip `touse' `mask' "`rclip'" "`line'" "`clip'"/*
                */ "`strict'" "`split'" "`drop'" "`dots'" // => Ndrop, Nadd
        } 
        else if "`subcmd'"=="simplify" {
            __geoframe_simplify `touse' "`delta'" "`absolute'" "`jointly'"/*
                */ "`drop'" "`dots'" // => Ndrop
        }
        else if "`subcmd'"=="bshare" {
            __geoframe_bshare `touse' "`not'" "`drop'" "`dots'" // => Ndrop
        }
        // update PID and shape_order (if set)
        geoframe get sid, local(sid)
        if `"`sid'"'!="" {
            geoframe get id, local(ID) strict
            qui replace `sid' = cond(_n==1 | `ID'!=`ID'[_n-1], 1, 1+`sid'[_n-1])
        }
        geoframe get pid, local(pid)
        if `"`pid'"'!="" {
            qui geoframe generate pid `pid', replace
        }
    }
    // eliminate dropped units in attribute frame
    if `hasSHP' & "`drop'"=="" {
        frame `frame' {
            drop `touse'
            _copy_from_shpframe `id' `shpid' `shpframe' `lnkvar' "`touse'"
            qui drop if `touse'>=.
            local Ndrop0 = r(N_drop)
            qui geoframe link `shpframe'
        }
    }
    // reporting and frame change
    if `Ndrop'==1 local msg observation
    else          local msg observations
    local tmp `: di %9.0gc `Ndrop''
    di as txt "(dropped `tmp' `msg' in frame {bf:`shpframe'})"
    if "`Nadd'"!="" {
        if `Nadd'==1 local msg observation
        else         local msg observations
        local tmp `: di %9.0gc `Nadd''
        di as txt "(added `tmp' `msg' in frame {bf:`shpframe'})"
    }
    if "`Ndrop0'"!="" {
        if `Ndrop0'==1 local msg observation
        else           local msg observations
        local tmp `: di %9.0gc `Ndrop0''
        di as txt "(dropped `tmp' `msg' in frame {bf:`frame'})"
    }
    if "`current'"=="" {
        if "`frame'"!=`"`c(frame)'"' {
            frame change `frame'
            di as txt "(current frame now {bf:`frame'})"
        }
    }
end

program ___geoframe_clip
    args touse MASK rclip line noclip strict split drop dots
    geoframe get coordinates, local(XY) strict
    gettoken X XY : XY
    gettoken Y XY : XY
    gettoken X2 XY : XY // pc
    gettoken Y2 XY : XY // pc
    // determine type of shapes and type of clipping
    tempname pid
    qui geoframe generate pid `pid', noset
    geoframe get id, local(id)
    if `"`id'"'=="" { // assume all obs belong to the same unit
        tempname id
        qui gen byte `id' = 1
    }
    if "`X2'"!="" local point point // assume point data if pc
    else {
        capt assert (`id'!=`id'[_n-1] | `pid'!=`pid'[_n-1]) if `touse'
        if _rc==1 exit _rc
        if _rc==0 local point point // assume point data if PID is unique
    }
    if "`point'"!="" local noclip noclip // point data implies noclip
    local poly 1 // 0 = individual observation, 1 = groups of observations
    if "`noclip'"!="" {
        if "`point'"!="" {
            if "`split'"!="" local poly 0
            else {
                capt assert (_n==1 | `id'!=`id'[_n-1]) if `touse'
                if _rc==1 exit _rc
                if _rc==0 local poly 0 // only one obs per unit
            }
        }
    }
    // identify points outside mask
    tempvar OUT
    if "`rclip'"!="" {
        qui gen byte `OUT' = 0 if `touse'
        qui replace `OUT' = . if (`X'>=. | `Y'>=.) & `touse'
        if `MASK'[1,1]<. qui replace `OUT' = 1 if `X'<`MASK'[1,1] & `OUT'==0
        if `MASK'[1,2]<. qui replace `OUT' = 1 if `X'>`MASK'[1,2] & `OUT'==0
        if `MASK'[1,3]<. qui replace `OUT' = 1 if `Y'<`MASK'[1,3] & `OUT'==0
        if `MASK'[1,4]<. qui replace `OUT' = 1 if `Y'>`MASK'[1,4] & `OUT'==0
    }
    else {
        qui gen byte `OUT' = .
    }
    // apply clipping
    if `poly' {
        if "`noclip'"!="" {
            mata: _noclip("`touse'", "`id'", "`pid'", "`rclip'"!="", "`OUT'",/*
                */ ("`X'","`Y'"), "`MASK'", "`strict'"!="", "`split'"!="",/*
                */ "`drop'"!="", "`dots'"=="")
        }
        else {
            mata: _clip("`touse'", "`id'", "`pid'", "`rclip'"!="", "`OUT'",/*
                */ ("`X'","`Y'"), "`MASK'", "`line'"!="", "`drop'"!="",/*
                */ "`dots'"=="")
        }
    }
    qui drop if `OUT' & `touse'
    c_local Ndrop = r(N_drop)
    c_local Nadd `Nadd'
end

program __geoframe_simplify
    args touse delta absolute jointly drop dots
    geoframe get id, local(ID)
    if `"`ID'"'=="" {
        tempvar ID
        qui gen byte `ID' = 1
    }
    tempvar OUT
    qui gen byte `OUT' = 0
    qui geoframe get coordinates, local(XY) strict
    gettoken X XY : XY
    gettoken Y XY : XY
    if "`delta'"=="" | "`absolute'"=="" {
        if "`delta'"=="" local delta 1
        su `X' if `touse', meanonly
        local xrange = r(max)-r(mean)
        su `Y' if `touse', meanonly
        local yrange = r(max)-r(mean)
        local delta = (`xrange'/1000) * (`yrange'/1000) / 2 * `delta'
    }
    local msg `: di %9.0g `delta''
    di as txt "(threshold = `msg')"
    mata: _simplify("`ID'", "`OUT'", ("`X'","`Y'"), "`touse'", `delta', /*
        */ "`jointly'"!="", "`drop'"!="", "`dots'"=="")
    qui drop if `OUT'
    c_local Ndrop = r(N_drop)
end

program __geoframe_bshare
    args touse not drop dots
    geoframe get id, local(ID)
    if `"`ID'"'=="" {
        tempvar ID
        qui gen byte `ID' = 1
    }
    tempvar OUT
    qui gen byte `OUT' = 0
    qui geoframe get coordinates, local(XY) strict
    gettoken X XY : XY
    gettoken Y XY : XY
    tempname PID
    qui geoframe gen pid `PID', noset
    mata: _bshare("`ID'", "`PID'", "`OUT'", ("`X'","`Y'"), "`touse'",/*
        */ "`not'"!="" ? -1 : 0, "`drop'"!="", "`dots'"=="")
    qui drop if `OUT'
    c_local Ndrop = r(N_drop)
end

/*
program __geoframe_clip
    // syntax
    syntax [if] [in], mask(str) [ rclip/*
        */ line noclip strict split nodrop /*
        */ into(namelist max=2) replace nocurrent ]
    // mark sample
    marksample touse
    // into()
    if `"`into'"'!="" {
        gettoken frame : into
        geoframe select `if' `in', into(`into') `replace' nodescribe nocurrent 
    }
    else local frame `"`c(frame)'"'
    // check for shape frame
    frame `frame' {
        geoframe get shpframe, local(shpframe)
        local hasSHP = "`shpframe'"!=""
        if `hasSHP' {
            geoframe get id, local(id) strict
            geoframe get linkname, local(lnkvar) strict
            frame `shpframe' {
                geoframe get id, local(shpid) strict
                qui frget `touse' = `touse', from(`lnkvar')
                qui replace `touse' = 0 if `touse'>=.
            }
        }
        else local shpframe `frame'
    }
    // apply clipping
    frame `shpframe' {
        di as txt "(clipping shapes ..." _c
        ___geoframe_clip `touse' `mask' "`rclip'" "`line'" "`clip'"/*
            */ "`strict'" "`split'" "`drop'" // => Ndrop, Nadd
        di as txt " done)"
    }
    // eliminate dropped units in attribute frame
    if `hasSHP' & "`drop'"=="" {
        frame `frame' {
            drop `touse'
            _copy_from_shpframe `id' `shpid' `shpframe' `lnkvar' "`touse'"
            qui drop if `touse'>=.
            local Ndrop0 = r(N_drop)
            qui geoframe link `shpframe'
        }
    }
    // reporting and frame change
    if `Ndrop'==1 local msg observation
    else          local msg observations
    local tmp `: di %9.0gc `Ndrop''
    di as txt "(dropped `tmp' `msg' in frame {bf:`shpframe'})"
    if "`Nadd'"!="" {
        if `Nadd'==1 local msg observation
        else         local msg observations
        local tmp `: di %9.0gc `Nadd''
        di as txt "(added `tmp' `msg' in frame {bf:`shpframe'})"
    }
    if "`Ndrop0'"!="" {
        if `Ndrop0'==1 local msg observation
        else           local msg observations
        local tmp `: di %9.0gc `Ndrop0''
        di as txt "(dropped `tmp' `msg' in frame {bf:`frame'})"
    }
    if "`current'"=="" {
        if "`frame'"!=`"`c(frame)'"' {
            frame change `frame'
            di as txt "(current frame now {bf:`frame'})"
        }
    }
end

program ___geoframe_clip
    args touse MASK rclip line noclip strict split drop
    geoframe get coordinates, local(XY) strict
    gettoken X XY : XY
    gettoken Y XY : XY
    gettoken X2 XY : XY // pc
    gettoken Y2 XY : XY // pc
    // determine type of shapes and type of clipping
    tempname pid
    qui geoframe generate pid `pid', noset
    geoframe get id, local(id)
    if `"`id'"'=="" { // assume all obs belong to the same unit
        tempname id
        qui gen byte `id' = 1
    }
    if "`X2'"!="" local point point // assume point data if pc
    else {
        capt assert (`id'!=`id'[_n-1] | `pid'!=`pid'[_n-1]) if `touse'
        if _rc==1 exit _rc
        if _rc==0 local point point // assume point data if PID is unique
    }
    if "`point'"!="" local noclip noclip // point data implies noclip
    local poly 1 // 0 = individual observation, 1 = groups of observations
    if "`noclip'"!="" {
        if "`point'"!="" {
            if "`split'"!="" local poly 0
            else {
                capt assert (_n==1 | `id'!=`id'[_n-1]) if `touse'
                if _rc==1 exit _rc
                if _rc==0 local poly 0 // only one obs per unit
            }
        }
    }
    // identify points outside mask
    tempvar OUT
    if "`rclip'"!="" {
        qui gen byte `OUT' = 0 if `touse'
        qui replace `OUT' = . if (`X'>=. | `Y'>=.) & `touse'
        if `MASK'[1,1]<. qui replace `OUT' = 1 if `X'<`MASK'[1,1] & `OUT'==0
        if `MASK'[1,2]<. qui replace `OUT' = 1 if `X'>`MASK'[1,2] & `OUT'==0
        if `MASK'[1,3]<. qui replace `OUT' = 1 if `Y'<`MASK'[1,3] & `OUT'==0
        if `MASK'[1,4]<. qui replace `OUT' = 1 if `Y'>`MASK'[1,4] & `OUT'==0
    }
    else {
        qui gen byte `OUT' = .
    }
    // apply clipping
    if `poly' {
        if "`noclip'"!="" {
            mata: _noclip("`touse'", "`id'", "`pid'", "`rclip'"!="", "`OUT'",/*
                */ ("`X'","`Y'"), "`MASK'", "`strict'"!="", "`split'"!="",/*
                */ "`drop'"!="")
        }
        else {
            mata: _clip("`touse'", "`id'", "`pid'", "`rclip'"!="", "`OUT'",/*
                */ ("`X'","`Y'"), "`MASK'", "`line'"!="", "`drop'"!="")
        }
    }
    qui drop if `OUT' & `touse'
    c_local Ndrop = r(N_drop)
    c_local Nadd `Nadd'
    // update PID and shape_order (if set)
    geoframe get sid, local(sid)
    if `"`sid'"'!="" {
        qui replace `sid' = cond(_n==1 | `id'!=`id'[_n-1], 1, 1 + `sid'[_n-1])
    }
    geoframe get pid, local(pid)
    if `"`pid'"'!="" {
        qui geoframe generate pid `pid', replace
    }
end
*/

/*
program _geoframe_simplify
    // syntax
    syntax [anything(id="delta" name=delta)] [if] [in] [,/*
        */ ABSolute JOINTly nodots nodrop/*
        */ into(namelist max=2) replace noCURrent ]
    if "`delta'"!="" {
        numlist `"`delta'"', max(1) range(>=0)
        local delta `r(numlist)'
    }
    // mark sample
    marksample touse
    // into()
    if `"`into'"'!="" {
        gettoken frame : into
        geoframe select `if' `in', into(`into') `replace' nodescribe nocurrent 
    }
    else local frame `"`c(frame)'"'
    // check for shape frame
    frame `frame' {
        geoframe get shpframe, local(shpframe)
        local hasSHP = "`shpframe'"!=""
        if `hasSHP' {
            geoframe get id, local(id) strict
            geoframe get linkname, local(lnkvar) strict
            frame `shpframe' {
                geoframe get id, local(shpid) strict
                qui frget `touse' = `touse', from(`lnkvar')
                qui replace `touse' = 0 if `touse'>=.
            }
        }
        else local shpframe `frame'
    }
    // generalize
    frame `shpframe' {
        geoframe get id, local(ID)
        if `"`ID'"'=="" {
            tempvar ID
            qui gen byte `ID' = 1
        }
        tempvar OUT
        qui gen byte `OUT' = 0
        qui geoframe get coordinates, local(XY) strict
        gettoken X XY : XY
        gettoken Y XY : XY
        if "`delta'"=="" | "`absolute'"=="" {
            if "`delta'"=="" local delta 1
            su `X' if `touse', meanonly
            local xrange = r(max)-r(mean)
            su `Y' if `touse', meanonly
            local yrange = r(max)-r(mean)
            local delta = (`xrange'/1000) * (`yrange'/1000) / 2 * `delta'
        }
        local msg `: di %9.0g `delta''
        di as txt "(threshold = `msg')"
        mata: _simplify("`ID'", "`OUT'", ("`X'","`Y'"), "`touse'", `delta', /*
            */ "`jointly'"!="", "`drop'"!="", "`dots'"=="")
        qui drop if `OUT'
        local Ndrop = r(N_drop)
        // update PID and shape_order (if set)
        geoframe get sid, local(sid)
        if `"`sid'"'!="" {
            qui replace `sid' = cond(_n==1 | `ID'!=`ID'[_n-1], 1, 1+`sid'[_n-1])
        }
        geoframe get pid, local(pid)
        if `"`pid'"'!="" {
            qui geoframe generate pid `pid', replace
        }
    }
    // eliminate dropped units in attribute frame
    if `hasSHP' & "`drop'"=="" {
        frame `frame' {
            drop `touse'
            _copy_from_shpframe `id' `shpid' `shpframe' `lnkvar' "`touse'"
            qui drop if `touse'>=.
            local Ndrop0 = r(N_drop)
            qui geoframe link `shpframe'
        }
    }
    // reporting and frame change
    if `Ndrop'==1 local msg observation
    else          local msg observations
    local tmp `: di %9.0gc `Ndrop''
    di as txt "(dropped `tmp' `msg' in frame {bf:`shpframe'})"
    if "`Ndrop0'"!="" {
        if `Ndrop0'==1 local msg observation
        else           local msg observations
        local tmp `: di %9.0gc `Ndrop0''
        di as txt "(dropped `tmp' `msg' in frame {bf:`frame'})"
    }
    if "`current'"=="" {
        if "`frame'"!=`"`c(frame)'"' {
            frame change `frame'
            di as txt "(current frame now {bf:`frame'})"
        }
    }
end
*/


/*
program _geoframe_bshare
    // syntax
    syntax [if] [in] [,/*
        */ nodots nodrop/*
        */ into(namelist max=2) replace noCURrent ]
    // mark sample
    marksample touse
    // into()
    if `"`into'"'!="" {
        gettoken frame : into
        geoframe select `if' `in', into(`into') `replace' nodescribe nocurrent 
    }
    else local frame `"`c(frame)'"'
    // check for shape frame
    frame `frame' {
        geoframe get shpframe, local(shpframe)
        local hasSHP = "`shpframe'"!=""
        if `hasSHP' {
            geoframe get id, local(id) strict
            geoframe get linkname, local(lnkvar) strict
            frame `shpframe' {
                geoframe get id, local(shpid) strict
                qui frget `touse' = `touse', from(`lnkvar')
                qui replace `touse' = 0 if `touse'>=.
            }
        }
        else local shpframe `frame'
    }
    // generalize
    frame `shpframe' {
        geoframe get id, local(ID)
        if `"`ID'"'=="" {
            tempvar ID
            qui gen byte `ID' = 1
        }
        tempvar OUT
        qui gen byte `OUT' = 0
        qui geoframe get coordinates, local(XY) strict
        gettoken X XY : XY
        gettoken Y XY : XY
        tempname PID
        qui geoframe gen pid `PID', noset
        mata: _bshare("`ID'", "`PID'", "`OUT'", ("`X'","`Y'"), "`touse'",/*
            */ "`drop'"!="", "`dots'"=="")
        qui drop if `OUT'
        local Ndrop = r(N_drop)
        // update PID and shape_order (if set)
        geoframe get sid, local(sid)
        if `"`sid'"'!="" {
            qui replace `sid' = cond(_n==1 | `ID'!=`ID'[_n-1], 1, 1+`sid'[_n-1])
        }
        geoframe get pid, local(pid)
        if `"`pid'"'!="" {
            qui geoframe generate pid `pid', replace
        }
    }
    // eliminate dropped units in attribute frame
    if `hasSHP' & "`drop'"=="" {
        frame `frame' {
            drop `touse'
            _copy_from_shpframe `id' `shpid' `shpframe' `lnkvar' "`touse'"
            qui drop if `touse'>=.
            local Ndrop0 = r(N_drop)
            qui geoframe link `shpframe'
        }
    }
    // reporting and frame change
    if `Ndrop'==1 local msg observation
    else          local msg observations
    local tmp `: di %9.0gc `Ndrop''
    di as txt "(dropped `tmp' `msg' in frame {bf:`shpframe'})"
    if "`Ndrop0'"!="" {
        if `Ndrop0'==1 local msg observation
        else           local msg observations
        local tmp `: di %9.0gc `Ndrop0''
        di as txt "(dropped `tmp' `msg' in frame {bf:`frame'})"
    }
    if "`current'"=="" {
        if "`frame'"!=`"`c(frame)'"' {
            frame change `frame'
            di as txt "(current frame now {bf:`frame'})"
        }
    }
end
*/
    
program _geoframe_generate
    gettoken fnc 0 : 0, parse(" ,")
    local fnc = strlower(`"`fnc'"')
    local l = strlen(`"`fnc'"')
    if      `"`fnc'"'==substr("centroids", 1, max(3,`l')) local fnc centroids
    else if `"`fnc'"'==substr("plevel", 1, max(2,`l'))    local fnc plevel
    else if `"`fnc'"'==substr("shpmatch", 1, max(3,`l'))  local fnc shpmatch
    else if `"`fnc'"'=="" {
        di as err `"{it:function} required"'
        exit 198
    }
    else {
        capt mata: assert(st_islmname(st_local("fnc")))
        if _rc==1 exit 1
        if _rc {
            di as err `"`fnc': invalid function"'
            exit 198
        }
    }
    _geoframe_generate_`fnc' `0'
end

program _geoframe_generate_plevel
    syntax [name] [if] [in] [, by(varname numeric) replace noset noDOTs ]
    if "`namelist'"=="" local namelist _PLEVEL
    local cframe `"`c(frame)'"'
    geoframe get type, l(type)
    marksample touse
    if "`by'"!="" markout `touse' `by'
    if "`type'"=="shape" {
        local shpframe `"`cframe'"'
        local BY `by'
    }
    else {
        geoframe get shpframe, local(shpframe) strict
        geoframe get linkname, local(lnkvar) strict
        frame `shpframe' {
            qui frget `touse' = `touse', from(`lnkvar')
            qui replace `touse' = 0 if `touse'>=.
            if "`by'"!="" {
                tempvar BY
                qui frget `BY' = `by', from(`lnkvar')
            }
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
        geoframe get id, l(ID)
        if `"`ID'"'=="" {
            di as txt "(ID not available;"/*
                */ " assuming all polygons belong to same unit)"
            tempname ID
            qui gen byte `ID' = 1
        }
        geoframe get coordinates, l(XY) strict
        geoframe get pid, l(PID)
        if `"`PID'"'=="" {
            tempvar PID
            qui geoframe gen pid `PID', noset
        }
        tempname PL
        qui gen double `PL' = .
        if "`BY'"!="" {
            qui levelsof `BY' if `touse', local(bylvls)
            foreach lvl of local bylvls {
                if "`dots'"=="" di as txt "(processing `by'=`lvl')"
                mata: _plevel("`dots'"!="", "`PL'", `"`ID'"', `"`PID'"',/*
                    */ `"`XY'"', "`touse'", "`BY'", `lvl') // returns N
                if `N'==1 local msg polygon
                else      local msg polygons
                if "`dots'"=="" di as txt "(`N' nested `msg' found)"
                else di as txt "(`by'=`lvl': `N' nested `msg' found)"
            }
        }
        else {
            mata: _plevel("`dots'"!="", "`PL'", `"`ID'"', `"`PID'"', `"`XY'"',/*
                */ "`touse'")
            if `N'==1 local msg polygon
            else      local msg polygons
            di as txt "(`N' nested `msg' found)"
        }
        qui compress `PL'
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

program _geoframe_generate_centroids
    syntax [namelist(min=2 max=2)] [, replace noset ]
    if "`namelist'"=="" local namelist _CX _CY
    if "`replace'"=="" {
        confirm new variable `namelist'
    }
    geoframe get type, l(type)
    local cframe `"`c(frame)'"'
    if "`type'"=="shape" {
        geoframe get id, l(ID0)
        if `"`ID0'"'=="" {
            di as txt "(ID not available;"/*
                */ " assuming all polygons belong to same unit)"
            tempname ID0
            qui gen byte `ID0' = 1
        }
        local shpframe `"`cframe'"'
        local hasSHP 0
    }
    else {
        geoframe get shpframe, local(shpframe) strict
        geoframe get linkname, local(lnkvar) strict
        geoframe get id, l(ID0) strict
        local hasSHP 1
    }
    frame `shpframe' {
        if !`hasSHP' local ID `ID0'
        else geoframe get id, l(ID) strict
        geoframe get coordinates, l(XY) strict
        tempvar CX CY
        qui gen double `CX' = .
        qui gen double `CY' = .
        mata: _centroid(("`CX'","`CY'"), `"`ID'"', `"`XY'"')
    }
    if `hasSHP' {
        _copy_from_shpframe `ID0' `ID' `shpframe' `lnkvar' "`CX' `CY'"
    }
    local TMP `CX' `CY'
    foreach var of local namelist {
        capt confirm new variable `var'
        if _rc==1 exit _rc
        if _rc drop `var'
        gettoken tmp TMP : TMP
        rename `tmp' `var'
    }
    if "`set'"=="" _geoframe_set centroids `namelist'
    di as txt "(variables {bf:`namelist'} added to frame {bf:`cframe'})"
end

program _geoframe_generate_area
    syntax [name] [, replace noset Scale(str) ]
    if "`namelist'"=="" local namelist _AREA
    if "`replace'"=="" {
        confirm new variable `namelist'
    }
    geoframe get type, l(type)
    local cframe `"`c(frame)'"'
    if "`type'"=="shape" {
        geoframe get id, l(ID0)
        if `"`ID0'"'=="" {
            di as txt "(ID not available;"/*
                */ " assuming all polygons belong to same unit)"
            tempname ID0
            qui gen byte `ID0' = 1
        }
        local shpframe `"`cframe'"'
        local hasSHP 0
    }
    else {
        geoframe get shpframe, local(shpframe) strict
        geoframe get linkname, local(lnkvar) strict
        geoframe get id, l(ID0) strict
        local hasSHP 1
    }
    frame `shpframe' {
        if !`hasSHP' local ID `ID0'
        else geoframe get id, l(ID) strict
        geoframe get coordinates, l(XY) strict
        tempvar AREA
        qui gen double `AREA' = .
        mata: _area("`AREA'", `"`ID'"', `"`XY'"')
    }
    if `hasSHP' {
        _copy_from_shpframe `ID0' `ID' `shpframe' `lnkvar' `AREA'
    }
    if `"`scale'"'!="" {
        qui replace `AREA' = `AREA' / (`scale')^2
    }
    capt confirm new variable `namelist'
    if _rc==1 exit _rc
    if _rc drop `namelist'
    rename `AREA' `namelist'
    if "`set'"=="" _geoframe_set area `namelist'
    di as txt "(variable {bf:`namelist'} added to frame {bf:`cframe'})"
end

program _copy_from_shpframe
    gettoken id 0 : 0
    gettoken shpid 0 : 0
    gettoken shpframe 0 : 0
    gettoken lnkvar 0 : 0
    gettoken varlist 0 : 0
    gettoken vlist 0 : 0
    if !`:list sizeof varlist' exit
    tempname tmpframe tag
    frame `shpframe' {
        qui gen byte `tag' = `lnkvar'<. & (_n==1 | `shpid'!=`shpid'[_n-1])
        frame put `shpid' `varlist' if `tag', into(`tmpframe')
    }
    qui frlink 1:1 `id', frame(`tmpframe' `shpid')
    if `"`vlist'"'=="" {
        foreach v of local varlist {
            local vlist `vlist' `v' = `v'
        }
    }
    qui frget `vlist', from(`tmpframe')
end

program _geoframe_generate_pid
    syntax [name] [, replace noset ]
    if "`namelist'"=="" local namelist _PID
    local cframe `"`c(frame)'"'
    geoframe get type, l(type)
    if "`type'"=="shape" local shpframe `"`cframe'"'
    else {
        geoframe get shpframe, local(shpframe)
        if `"`shpframe'"'=="" {
            di as txt "(shape frame not found;"/*
                */ " treating current frame as shape frame)"
            local shpframe `"`cframe'"'
        }
    }
    frame `shpframe' {
        if "`replace'"=="" {
            cap n confirm new variable `namelist'
            if _rc==1 exit 1
            if _rc {
                if "`shpframe'"!="`cframe'" {
                    di as err "in frame {bf:`shpframe'}"
                }
                exit _rc
            }
        }
        geoframe get id, l(ID)
        if `"`ID'"'=="" {
            di as txt "(ID not available;"/*
                */ " assuming all polygons belong to same unit)"
            tempname ID
            qui gen byte `ID' = 1
        }
        geoframe get coordinates, l(XY) strict
        tempvar PID
        qui gen double `PID' = .
        mata: _pid("`PID'", `"`ID'"', `"`XY'"')
        qui compress `PID'
        capt confirm new variable `namelist'
        if _rc==1 exit _rc
        if _rc drop `namelist'
        rename `PID' `namelist'
        if "`set'"=="" _geoframe_set pid `namelist'
        di as txt "(variable {bf:`namelist'} added to frame {bf:`shpframe'})"
    }
end

program _geoframe_generate_shpmatch
    syntax [name] [, replace ]
    if "`namelist'"=="" local namelist _SHPMATCH
    if "`replace'"=="" {
        confirm new variable `namelist'
    }
    geoframe get shpframe, local(shpframe) strict
    geoframe get id, local(id) strict
    geoframe get linkname, local(lnkvar) strict
    tempname tmpframe tag
    frame `shpframe' {
        geoframe get id, local(shpid) strict
        qui gen byte `tag' = `lnkvar'<. & (_n==1 | `shpid'!=`shpid'[_n-1])
        frame put `shpid' if `tag', into(`tmpframe')
    }
    qui frlink 1:1 `id', frame(`tmpframe' `shpid')
    tempvar SHPMATCH
    qui gen byte `SHPMATCH' = `tmpframe'<.
    capt confirm new variable `namelist'
    if _rc==1 exit _rc
    if _rc drop `namelist'
    rename `SHPMATCH' `namelist'
    di as txt "(variable {bf:`namelist'} added to frame {bf:`c(frame)'})"
end

program _geoframe_bbox
    syntax name(id="newname" name=newname) [if] [in] [, noShp/*
        */ by(varname numeric) ROTate CIRcle hull PADding(real 0)/*
        */ n(numlist int max=1 >0) ANGle(real 0) noADJust replace ]
    if "`hull'"!="" & "`circle'"!="" {
        di as err "only one of {bf:circle} and {bf:hull} allowed"
        exit 198
    }
    if "`hull'"!=""        local btype 3
    else if "`circle'"!="" local btype 2
    else if "`rotate'"!="" local btype 1
    else                   local btype 0
    if "`n'"=="" local n 100
    if "`replace'"=="" confirm new frame `newname'
    // mark sample
    marksample touse
    if "`by'"!="" markout `touse' `by'
    // find shapes
    local cframe `"`c(frame)'"'
    geoframe get id, l(ID0)
    if "`shp'"=="" {
        geoframe get type, l(type) 
        if "`type'"!="shape" geoframe get shpframe, local(shpframe)
    }
    if `"`shpframe'"'=="" {
        geoframe get coordinates, l(XY) strict
        markout `touse' `XY'
        local shpframe `"`cframe'"'
        local BY `by'
        local ID `ID0'
    }
    else {
        geoframe get linkname, local(lnkvar) strict
        frame `shpframe' {
            qui frget `touse' = `touse', from(`lnkvar')
            qui replace `touse' = 0 if `touse'>=.
            geoframe get coordinates, l(XY) strict
            markout `touse' `XY'
            geoframe get id, l(ID)
            if "`by'"!="" {
                if "`by'"=="`ID0'" & "`ID'"!="" local BY `ID'
                else {
                    tempvar BY
                    qui frget `BY' = `by', from(`lnkvar')
                }
            }
        }
    }
    // prepare new frame
    tempname newframe
    frame create `newframe' double(_ID _X _Y)
    // obtain boxes/MECs
    frame `shpframe' {
        if "`BY'"!="" {
            if "`BY'"=="`ID'" {
                mata: _bbox2("`newframe'", `"`XY'"', "`touse'",/*
                     */ `btype', `n', `padding', "`adjust'"=="",/*
                     */ `angle', "`BY'")
            }
            else {
                qui levelsof `BY' if `touse', local(bylvls)
                foreach lvl of local bylvls {
                    mata: _bbox1("`newframe'", `"`XY'"', "`touse'",/*
                         */ `btype', `n', `padding', "`adjust'"=="",/*
                         */ `angle', "`BY'", `lvl')
                }
            }
        }
        else {
            mata: _bbox1("`newframe'", `"`XY'"', "`touse'",/*
                 */ `btype', `n', `padding',"`adjust'"=="", `angle')
        }
    }
    // cleanup
    frame `newframe' {
        qui compress _ID
        __geoframe_set_type shape
        capt confirm new frame `newname'
        if _rc==1 exit 1
        if _rc frame drop `newname'
        frame rename `newframe' `newname'
        di as txt "(new frame "/*
            */ `"{stata geoframe describe `newname':{bf:`newname'}}"'/*
            */ " created)"
    }
end

program _geoframe_symbol
    syntax name(id="newname" name=name) [if] [in] [, * ]
    __geoframe_symbol `name' `if' `in', `options'
end

program _geoframe_symboli
    syntax anything(id="newname") [, * ]
    gettoken name anything : anything
    confirm name `name'
    numlist `"`anything'"', min(3)
    __geoframe_symbol `name' `r(numlist)', `options'
end

program __geoframe_symbol
    syntax anything [if] [in] [, replace/*
        */ SHape(passthru) SIze(passthru) OFFset(passthru) ANGle(passthru)/*
        */ ratio(passthru) n(passthru) ]
    gettoken newname anything : anything
    if "`replace'"=="" confirm new frame `newname'
    // generate symbols
    if `: list sizeof anything' {
        if `"`if'"'!="" {
            di as err "if not allowed"
            exit 101
        }
        if `"`in'"'!="" {
            di as err "in not allowed"
            exit 101
        }
        tempname frame
        frame create `frame' double(_X _Y SIZE)
        while (`"`anything'"'!="") {
            gettoken x anything : anything
            local x = real(`"`x'"')
            gettoken y anything : anything
            local y = real(`"`y'"')
            gettoken size anything : anything
            local size = real(`"`size'"')
            frame post `frame' (`x') (`y') (`size')
        }
        local size size(SIZE)
    }
    else {
        local frame `"`c(frame)'"'
        geoframe get id, l(ID)
    }
    tempname newframe
    _geoplot_symbol . . `frame' `if' `in', _frameonly(`newframe' `ID')/*
        */ `shape' `size' `offset' `angle' `ratio' `n'
    // cleanup
    frame `newframe' {
        drop W
        order _ID
        __geoframe_set_type shape
        capt confirm new frame `newname'
        if _rc==1 exit 1
        if _rc frame drop `newname'
        frame rename `newframe' `newname'
        di as txt "(new frame "/*
            */ `"{stata geoframe describe `newname':{bf:`newname'}}"'/*
            */ " created)"
    }
end

program _geoframe_collapse
    __geoframe_collapse 0 `0'
end

program _geoframe_contract
    __geoframe_collapse 1 `0'
end

program __geoframe_collapse
    gettoken contract 0 : 0
    local cframe `"`c(frame)'"'
    gettoken frame 0 : 0, parse(" ,")
    frame `frame' {
        local opts COordinates(passthru) SELect(passthru) id(varname)/*
            */ GENerate GENerate2(str)
        if `contract' {
            syntax [if] [in] [fw] [, `opts' * ]
        }
        else {
            syntax anything(equalok id="clist") [if] [in] [aw fw iw pw] [,/*
                */ `opts' cw ]
        }
        if `"`generate2'"'!="" local generate generate
        if "`id'"!="" {
            if `"`generate'"'!="" {
                di as err "generate() and id() not both allowed"
                exit 198
            }
            if `"`select'"'!="" {
                di as err "select() not allowed together with id()"
                exit 198
            }
            if `"`coordinates'"'!="" {
                di as err "coordinates() not allowed together with id()"
                exit 198
            }
            local ID `id'
        }
        else {
            if `"`generate'"'!="" {
                __geoframe_collapse_parse_gen `generate2' /*
                    returns generate, replace, set */
                local ID `generate'
                local novarnote
            }
            else {
                tempname ID
                local replace
                local set noset
                local novarnote novarnote
            }
            geoframe spjoin `cframe' `ID' `if' `in',/*
                */ `select' `coordinates' `replace' `set' `novarnote'
        }
    }
    tempname frame1
    frame copy `frame' `frame1'
    frame `frame1' {
        marksample touse
        qui keep if `touse' & `ID'<.
        if `contract' {
            contract `ID' [`weight'`exp'], `options'
        }
        else {
            collapse `anything' [`weight'`exp'], by(`ID') fast `cw'
        }
        geoframe get id, local(id)
        if `"`id'"'!="`ID'" {
            geoframe set id `ID'
        }
    }
    geoframe copy `frame1' *, exclude(`ID') quietly
    if `r(k)'==1 di as txt "(variable " _c
    else         di as txt "(variables " _c
    di as res r(newlist) as txt " added to frame {bf:`cframe'})"
end

program __geoframe_collapse_parse_gen
    syntax [name] [, replace noset ]
    if "`namelist'"=="" local namelist _ID // default
    c_local generate `namelist'
    c_local replace `replace'
    c_local set `set'
end

program _geoframe_spjoin
    syntax [namelist(min=1 max=2)] [if] [in] [, SELect(str asis) /*
        */ COordinates(varlist min=2 max=2) replace noset NOVARNOTE noDOTs ]
    marksample touse
    gettoken shpframe namelist : namelist
    gettoken id       namelist : namelist
    if "`id'"=="" local id _ID
    if "`replace'"=="" confirm new variable `id'
    if `"`coordinates'"'!="" local xy `coordinates'
    else geoframe get coordinates, l(xy) strict
    markout `touse' `xy'
    local frame `"`c(frame)'"'
    local TOUSE
    frame `shpframe' {
        // check whether shpframe is an attribute frame linked to a shape frame
        geoframe get type, l(type)
        if "`type'"!="shape" {
            geoframe get shpframe, local(shpframe2)
            if `"`shpframe2'"'!="" {
                tempvar TOUSE
                if `"`select'"'!="" {
                    qui gen byte `TOUSE' = 0
                    qui replace `TOUSE' = 1 if (`select')
                }
                else qui gen byte `TOUSE' = 1
                geoframe get linkname, local(lnkvar) strict
                frame `shpframe2' {
                    qui frget `TOUSE' = `TOUSE', from(`lnkvar')
                    qui replace `TOUSE' = 0 if `TOUSE'>=.
                }
                local shpframe `shpframe2'
            }
            else if `"`select'"'!="" {
                tempvar TOUSE
                qui gen byte `TOUSE' = 0
                qui replace `TOUSE' = 1 if (`select')
            }
        }
    }
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
        if "`PL'"=="" {
            di as txt "({helpb geoframe##gen_plevel:plevel} not set;"/*
                */ " assuming that there are no nested polygons)"
        }
    }
    tempvar Id
    qui gen `type' `Id' = .
    frame `shpframe': mata: _spjoin("`frame'", "`Id'", "`xy'", "`touse'",/*
        */ "`ID'", "`PID'", "`XY'", "`PL'", "`TOUSE'", "`dots'"!="")
    qui count if `Id'>=. & `touse'
    if `r(N)' {
        if r(N)==1 local msg point
        else       local msg points
        di as txt "({bf:`r(N)'} `msg' not matched)"
    }
    capt confirm new variable `id'
    if _rc==1 exit _rc
    if _rc drop `id'
    rename `Id' `id'
    if "`set'"=="" {
        geoframe set id `id'
    }
    if "`novarnote'"=="" {
        di as txt "(variable {bf:`id'} added to frame {bf:`frame'})"
    }
end

program _geoframe_set
    gettoken char 0 : 0, parse(" ,")
    __geoframe_set_char `char' // => char
    gettoken char arg : char
    if      "`char'"=="type"        __geoframe_set_type `0'
    else if "`char'"=="var"         __geoframe_set_var `arg' `0'
    else if "`char'"=="coordinates" __geoframe_set_coordinates `0'
    else if "`char'"=="centroids"   __geoframe_set_centroids `0'
    else if "`char'"=="type"        __geoframe_set_type `0'
    else if "`char'"=="shpframe" {
        di as err "'shpframe' only allowed with {bf:geoframe get}"
        exit 198
    }
    else if "`char'"=="linkname" {
        di as err "'linkname' only allowed with {bf:geoframe get}"
        exit 198
    }
    else char _dta[GEOFRAME_`char'] `0'
end

program __geoframe_set_type
    if !inlist(`"`0'"',"","unit","pc","shape") {
        di as err "type must be {bf:unit}, {bf:pc}, or {bf:shape}"
        exit 198
    }
    char _dta[GEOFRAME_type] `0'
end

program __geoframe_set_char
    local 0 = strlower(`"`0'"')
    local l = strlen(`"`0'"')
    if      `"`0'"'==substr("type", 1, max(1,`l'))        local 0 type
    else if `"`0'"'=="id"                                 local 0 var id
    else if `"`0'"'=="sid"                                local 0 var sid
    else if `"`0'"'=="pid"                                local 0 var pid
    else if `"`0'"'=="area"                               local 0 var area
    else if `"`0'"'==substr("plevel", 1, max(2,`l'))      local 0 var plevel
    else if `"`0'"'==substr("coordinates", 1, max(2,`l')) local 0 coordinates
    else if `"`0'"'==substr("centroids", 1, max(3,`l'))   local 0 centroids
    else if `"`0'"'==substr("feature", 1, max(1,`l'))     local 0 feature
    else if `"`0'"'==substr("shpframe", 1, max(1,`l'))    local 0 shpframe
    else if `"`0'"'=="linkname"                           local 0 linkname
    else {
        di as err `"'`0'' not allowed"'
        exit 198
    }
    c_local char `0'
end

program __geoframe_set_shpframe
    syntax [name(name=shpframe id="shape frame")]
    if "`shpframe'"=="" {
        char _dta[GEOFRAME_shpframe]
        exit
    }
    local cframe `"`c(frame)'"'
    if `"`cframe'"'=="`shpframe'" {
        di as err "{it:shpframe} may not be equal name of current frame"
        exit 498
    }
    confirm frame `shpframe'
    char _dta[GEOFRAME_shpframe] `shpframe'
end

program __geoframe_set_linkname
    syntax [name(name=lnkname id="linkname")]
    if "`lnkname'"=="" {
        char _dta[GEOFRAME_linkname]
        exit
    }
    char _dta[GEOFRAME_linkname] `lnkname'
end

program __geoframe_set_var
    gettoken var arg : 0
    local arg = strtrim(`"`arg'"')
    if `"`arg'"'!="" {
        capt unab arg: `arg', max(1)
        if _rc==1 exit _rc
        if _rc {
            confirm variable `arg'
            if `:list sizeof arg'>1 {
                di as err "too many variables specified"
                exit 198
            }
        }
        confirm numeric variable `arg', exact
        if "`var'"=="sid" local ARG shape_order
        else              local ARG = strupper("_`var'")
        if "`arg'"=="`ARG'" local arg // is default name; leave char empty
    }
    char _dta[GEOFRAME_`var'] `arg'
end

program __geoframe_set_coordinates
    _parse comma arg 0 : 0
    syntax [, unit pc shape ]
    local type `unit' `pc' `shape'
    if `: list sizeof type'>1 {
        di as err "only one of {bf:unit}, {bf:shape}, and {bf:pc} allowed"
        exit 198
    }
    if `"`arg'"'!="" {
        if `"`type'"'=="" {
            local type: char _dta[GEOFRAME_type]
        }
        if `"`type'"'=="pc" local n 4
        else                local n 2
        capt unab arg: `arg', min(`n') max(`n')
        if _rc==1 exit _rc
        if _rc {
            confirm variable `arg'
            if `:list sizeof arg'!=`n' {
                di as err "wrong number of variables specified; "/*
                    */ "must specify `n' variables"
                exit 198
            }
        }
        confirm numeric variable `arg', exact
        if `"`type'"'=="unit"    local ARG _CX _CY
        else if `"`type'"'=="pc" local ARG _X1 _Y1 _X2 _Y2
        else                     local ARG _X _Y
        if "`arg'"=="`ARG'" local arg // is default names; leave char empty
    }
    char _dta[GEOFRAME_coordinates] `arg'
end

program __geoframe_set_centroids
    local type: char _dta[GEOFRAME_type]
    if `"`type'"'=="unit" {
        __geoframe_set_coordinates `0'
        exit
    }
    local arg: copy local 0
    if `"`arg '"'!="" {
        capt unab arg: `arg', min(2) max(2)
        if _rc==1 exit _rc
        if _rc {
            confirm variable `arg'
            if `:list sizeof arg'!=2 {
                di as err "wrong number of variables specified; "/*
                    */ "must specify 2 variables"
                exit 198
            }
        }
        confirm numeric variable `arg', exact
        if "`arg'"=="_CX _CY" local arg // is default names; leave char empty
    }
    char _dta[GEOFRAME_centroids] `arg'
end

program _geoframe_get
    _parse comma char 0 : 0
    __geoframe_set_char `char' // => char
    gettoken char arg : char
    syntax [, Local(str) strict * ]
    if      "`char'"=="var"         __geoframe_get_var `arg', `options'
    else if "`char'"=="coordinates" __geoframe_get_coordinates, `options'
    else if "`char'"=="centroids"   __geoframe_get_centroids, `options'
    else                            local exp: char _dta[GEOFRAME_`char']
    if "`strict'"!="" {
        if `"`exp'"'=="" {
            if "`arg'"!="" local char `arg'
            di as err `"`char' not found; "' /*
                */ `"declare `char' using {helpb geoframe}"'
            exit 111
        }
    }
    if "`local'"!="" {
        c_local local `local'
        c_local value `"`exp'"'
    }
    else {
        di as txt `"`exp'"'
    }
end

program __geoframe_get_var
    syntax name [, * ]
    local exp: char _dta[GEOFRAME_`namelist']
    if `"`exp'"'=="" {
        // try default name
        if "`namelist'"=="sid" local VAR shape_order
        else                   local VAR = strupper("_`namelist'")
        capt confirm numeric variable `VAR', exact
        if _rc==1 exit _rc
        if _rc==0 {
            local exp `VAR'
        }
    }
    c_local exp `exp'
end

program __geoframe_get_coordinates
    syntax [, flip unit pc shape ]
    local type `unit' `pc' `shape'
    if `: list sizeof type'>1 {
        di as err "only one of {bf:unit}, {bf:shape}, and {bf:pc} allowed"
        exit 198
    }
    local exp: char _dta[GEOFRAME_coordinates]
    if `"`exp'"'=="" {
        // try default names
        if "`type'"=="" {
            local type: char _dta[GEOFRAME_type]
        }
        if `"`type'"'=="unit"    local VARS _CX _CY
        else if `"`type'"'=="pc" local VARS _X1 _Y1 _X2 _Y2
        else                     local VARS _X _Y
        capt confirm numeric variable `VARS', exact
        if _rc==1 exit _rc
        if _rc==0 local exp `VARS'
        else if inlist(`"`type'"',"","unit") { // check alternative names
            if `"`type'"'=="unit" local VARS _X _Y
            else                  local VARS _CX _CY
            capt confirm numeric variable `VARS', exact
            if _rc==1 exit _rc
            if _rc==0 local exp `VARS'
        }
    }
    if "`flip'"!="" geoframe flip `exp', local(exp)
    c_local exp `exp'
end

program __geoframe_get_centroids
    local type: char _dta[GEOFRAME_type]
    if `"`type'"'=="unit" {
        __geoframe_get_coordinates `0'
        c_local exp `exp'
        exit
    }
    syntax [, flip ]
    local exp: char _dta[GEOFRAME_centroids]
    if `"`exp'"'=="" {
        // try default names
        local VARS _CX _CY
        capt confirm numeric variable `VARS', exact
        if _rc==1 exit _rc
        if _rc==0 {
            local exp `VARS'
        }
    }
    if "`flip'"!="" geoframe flip `exp', local(exp)
    c_local exp `exp'
end

program _geoframe_flip
    _parse comma lhs 0 : 0
    syntax [, Local(str) ]
    mata: _flip("lhs")
    c_local local `local'
    c_local value `"`lhs'"'
end

program _geoframe_rename
    local cname `"`c(frame)'"'
    syntax name(id="newname" name=newname) [, replace ]
    if "`newname'"=="`cname'" {
        di as txt "({it:newname} equal to current name; nothing to do)"
        exit 198
    }
    if "`replace'"=="" confirm new frame `newname'
    capt confirm new frame `newname'
    if _rc==1 exit 1
    if _rc frame drop `newname'
    frame rename `cname' `newname'
    di as txt "(frame {bf:`cname'} renamed to {bf:`newname'})"
    frame `newname' {
        // update possible outgoing link
        geoframe get shpframe, local(shpframe)
        if `"`shpframe'"'!="" {
            local rc = `"`shpframe'"'=="`newname'"
            if `rc'==0 {
                capt confirm frame `shpframe'
                if _rc==1 exit 1
                if _rc local rc 1
            }
            if `rc' {
                __geoframe_set_shpframe
                __geoframe_set_linkname
                di as txt "(shape frame {bf:`shpframe'} no longer"/*
                    */ " exists; link removed)"
            }
            else {
                qui geoframe link `shpframe'
                di as txt "(link to shape frame {bf:`shpframe'} updated)"
            }
        }
        // update possible incoming links
        capt unab lnkvars: _GEOFRAME_lnkvar_*
        foreach lnkvar of local lnkvars {
            local unitframe: char `lnkvar'[frlink_fname2]
            local rc = `"`unitframe'"'=="`newname'"
            if `rc'==0 {
                capt confirm frame `unitframe'
                if _rc==1 exit
                if _rc local rc 1
            }
            if `rc' {
                drop `lnkvar'
                di as txt "(attribute frame {bf:`unitframe'} no longer"/*
                    */ " exists; link removed)"
                continue
            }
            frame `unitframe' {
                geoframe get shpframe, local(shpframe)
                if `"`shpframe'"'!="`cname'" continue // leaves linkvar in data
                __geoframe_set_shpframe `newname'
                __geoframe_set_linkname `lnkvar'
                di as txt "(link from attribute frame {bf:`unitframe'} updated)"
            }
        }
    }
end

program _geoframe_duplicate
    syntax namelist(id="newname" name=newname) [, * ]
    _geoframe_select, into(`newname') `options'
end

program _geoframe_relink
    geoframe get shpframe, local(shpframe)
    if `"`shpframe'"'=="" {
        di as txt "(no link to shape frame found)"
        exit
    }
    geoframe link `shpframe'
end

program _geoframe_unlink
    geoframe get shpframe, local(shpframe)
    geoframe get linkname, local(lnkvar)
    if `"`shpframe'"'=="" {
        di as txt "(no link to shape frame found)"
        exit
    }
    if `"`lnkvar'"'!="" {
        capt confirm frame `shpframe'
        if _rc==1 exit 1
        if _rc==0 {
            frame `shpframe': capt drop `lnkvar'
        }
    }
    __geoframe_set_shpframe
    __geoframe_set_linkname
    di as txt "(link to frame {bf:`shpframe'} removed)"
end

program _geoframe_attach
    if c(stata_version)<18 {
        di as err "{bf:geoframe attach} requires Stata 18"
        exit 9
    }
    gettoken frame 0 : 0, parse(" ,")
    if `"`frame'"'==`"`c(frame)'"' {
        di as err "{it:unitframe} must be different from current frame"
        exit 498
    }
    confirm frame `frame'
    frame `frame' {
        syntax [varlist] [, EXclude(varlist) ]
        geoframe get id, local(id)
        if `"`id'"'=="" {
            di as err "no ID variable defined in {it:unitframe}"
            exit 498
        }
        local keep `varlist'
        local keep: list keep - exclude
        local keep: list keep - id
        local keep: list keep - frame
        local keep: list uniq keep
    }
    local 0
    syntax [varlist]
    geoframe get id, local(id0)
    if `"`id0'"'=="" {
        di as err "no ID variable defined in current frame"
        exit 498
    }
    local keep: list keep - id0
    local varlist: list varlist - id0
    local varlist: list keep & varlist
    local k: list sizeof varlist
    if `k' {
        local keep: list keep - varlist
        if `k'>1 local msg "already exist"
        else     local msg "already exists"
        di as txt "(`varlist' `msg')"
    }
    if `: list sizeof keep'==0 {
        di as txt "(no variables to attach)"
        exit
    }
    foreach var of local keep {
        // use matched list so that variables staring with "_" are aliased
        local vlist `vlist' `var' = `var'
    }
    frlink m:1 `id0', frame(`frame' `id')
    fralias add `vlist', from(`frame')
end

program _geoframe_detach
    if c(stata_version)<18 {
        di as err "{bf:geoframe detach} requires Stata 18"
        exit 9
    }
    syntax name(name=frame id="unitframe")
    if "`frame'"==`"`c(frame)'"' {
        di as err "{it:unitframe} must be different from current frame"
        exit 498
    }
    qui frlink dir
    local frlink `"`r(vars)'"'
    if `: list frame in frlink'==0 {
        di as err "{bf:frlink} variable {bf:`frame'} not found"
        exit 111
    }
    qui fralias describe
    local aliaslist `"`r(varlist)'"'
    local droplist
    foreach v of local aliaslist {
        if `: isalias `v'' {
            if `"`: aliaslinkname `v''"'=="`frame'" {
                local droplist `droplist' `v'
            }
        }
    }
    drop `droplist' `frame'
    di as txt "({bf:`frame'} detached from {bf:`c(frame)'})"
end

program _geoframe_copy
    // syntax
    gettoken frame2 0 : 0, parse(" ,")
    local frame `"`c(frame)'"'
    if `"`frame2'"'==`"`frame'"' {
        di as err "{it:frame2} must be different from current frame"
        exit 498
    }
    confirm frame `frame2'
    frame `frame2' {
        syntax [varlist(default=none)] [, EXclude(varlist)/*
            */ TARget(namelist) id(namelist max=2)/*
            */ QUIETly ] // undodumented
        local varlist: list varlist - exclude
        geoframe get id, local(id2)
        geoframe get shpframe, local(shpframe2)
        if `"`shpframe2'"'!=`"`frame'"' local shpframe2
        gettoken ID  id : id
        gettoken ID2 id : id
        if "`ID2'"=="" local ID2 `ID'
        if "`ID2'"!="" confirm variable `ID2'
        else           local ID2 `id2'
        if "`ID2'"=="" {
            di as err "no ID variable found in frame {bf:´frame2'}"
            exit 111
        }
        local varlist: list varlist - ID2
    }
    geoframe get id, local(id)
    geoframe get shpframe, local(shpframe)
    if `"`shpframe'"'!=`"`frame2'"' local shpframe
    if "`ID'"!="" confirm variable `ID'
    else          local ID `id'
    if "`ID'"=="" {
        di as err "no ID variable found in the current frame"
        exit 111
    }
    // prepare varlist
    if `: list sizeof target'==0 {
        unab VARLIST: *
        local varlist: list varlist - VARLIST
    }
    if `: list sizeof varlist'==0 {
        `quietly' di as txt "(no variables to copy)"
        exit
    }
    local tgtvlist
    foreach var of local varlist {
        // use matched list so that variables staring with "_" are copied
        gettoken TGT target : target
        if "`TGT'"=="" local TGT `var'
        local tgtvlist `tgtvlist' `TGT'
        local vlist `vlist' `TGT' = `var'
    }
    confirm new variable `tgtvlist'
    // try existing link
    if "`ID'"=="`id'" & "`ID2'"=="`id2'" {
        if "`shpframe2'"!="" { // attribute -> shp
            frame `frame2': geoframe get linkname, local(lnkvar2)
            if `"`lnkvar2'"'!="" {
                capt frget `vlist', from(`lnkvar2')
                if _rc==1 exit 1
                if _rc==0 {
                    local k = r(k)
                    qui count if (_n==1 | `ID'!=`ID'[_n-1]) & `lnkvar2'>=.
                    `quietly' _geoframe_copy_di `frame2' `k' `r(N)'
                    exit
                }
            }
            local lnkvar2
        }
        if  "`shpframe'"!="" { // shp -> attribute
            geoframe get linkname, local(lnkvar)
            if `"`lnkvar'"'!="" {
                frame `frame2' {
                    tempvar merge
                    qui gen byte `merge' = 1
                }
                capt _copy_from_shpframe `ID' `ID2' `frame2' `lnkvar'/*
                    */ "`merge' `varlist'" "`merge' = `merge' `vlist'"
                if _rc==1 exit 1
                if _rc==0 {
                    local k = r(k)
                    qui count if `merge'>=.
                    `quietly' _geoframe_copy_di `frame2' `k' `r(N)'
                    exit
                }
            }
            local lnkvar2
        }
        if "`shpframe2'`shpframe'"!="" {
            `quietly' di as txt "(existing link failed;"/*
                */ " trying to link on the fly)"
        }
    }
    // cases
    capt isid `ID', missok
    if _rc==1 exit 1
    local uniq = _rc==0
    frame `frame2' {
        capt isid `ID2', missok
        if _rc==1 exit 1
        local uniq2 = _rc==0
    }
    // copy
    tempvar lnkvar
    if `uniq' & `uniq2' { // attribute -> attribute
        qui frlink 1:1 `ID', frame(`frame2' `ID2') generate(`lnkvar')
        qui frget `vlist', from(`lnkvar')
        local k = r(k)
        qui count if `lnkvar'>=.
        local Nmis = r(N)
    }
    else if `uniq2' { // attribute -> shp
        qui frlink m:1 `ID', frame(`frame2' `ID2') generate(`lnkvar')
        qui frget `vlist', from(`lnkvar')
        local k = r(k)
        qui count if (_n==1 | `ID'!=`ID'[_n-1]) & `lnkvar'>=.
        local Nmis = r(N)
    }
    else if `uniq' { // shp -> attribute
        frame `frame2' {
            tempvar merge
            qui gen byte `merge' = 1
            qui frlink m:1 `ID2', frame(`frame' `ID') generate(`lnkvar')
        }
        _copy_from_shpframe `ID' `ID2' `frame2' `lnkvar'/*
            */ "`merge' `varlist'" "`merge' = `merge' `vlist'"
        local k = r(k)
        qui count if `merge'>=.
        local Nmis = r(N)
    }
    else { // shp -> shp
        tempname tmpframe tag
        qui gen byte `tag' = _n==1 | `ID'!=`ID'[_n-1] // tag 1st obs per unit
        frame put `ID' if `tag', into(`tmpframe')
        frame `tmpframe' {
            frame `frame2' {
                tempvar merge
                qui gen byte `merge' = 1
                qui frlink m:1 `ID2', frame(`tmpframe' `ID') generate(`lnkvar')
            }
            _copy_from_shpframe `ID' `ID2' `frame2' `lnkvar'/*
                */ "`merge' `varlist'" "`merge' = `merge' `vlist'"
            qui count if `merge'>=.
            local Nmis = r(N)
        }
        qui frlink m:1 `ID', frame(`tmpframe' `ID') generate(`lnkvar')
        qui frget `vlist', from(`lnkvar')
        local k = r(k)
        foreach var of local tgtvlist {
            qui replace `var' = `var'[_n-1] if `tag'==0
        }
    }
    `quietly' _geoframe_copy_di `frame2' `k' `Nmis'
end

program _geoframe_copy_di
    args frame k Nmis
    if "`Nmis'"=="0"       local msg all units matched
    else if "`Nmis'"=="1"  local msg 1 unit unmatched
    else                   local msg `Nmis' units unmatched
    di as txt "(`msg')"
    if `k'==1 local msg variable
    else      local msg variables
    di as txt `"(`k' `msg' copied from frame {bf:`frame'})"'
end

program _geoframe_append
    gettoken frame 0 : 0, parse(" ,")
    confirm frame `frame'
    frame `frame' {
        syntax [varlist] [if] [in] [, EXclude(varlist) /*
            */ TARget(namelist) touse(varname numeric) ]
        if "`touse'"!="" {
            if `"`if'`in'"'!="" {
                di as err "touse() not allowed with if or in"
                exit 198
            }
        }
        else {
            marksample touse, novarlist
        }
        local varlist: list varlist - exclude
        local types
        foreach v of local varlist {
            local types `types' `:type `v''
        }
    }
    if `: list sizeof varlist'==0 {
        di as txt "(no variables to append)"
        exit
    }
    local addvars
    local recast
    local TARGET
    foreach v of local varlist {
        gettoken type types : types
        gettoken V target : target
        if `"`V'"'=="" local V `v'
        capt unab V : `V'
        if _rc==1 exit _rc
        if _rc {
            confirm name `V'
            local addvars `addvars' `type' `V'
            local TARGET `TARGET' `V'
            continue
        }
        local TARGET `TARGET' `V'
        if c(stata_version)>=18 {
            if `: isalias `V'' {
                di as err "`V' is an alias; may not append to alias variables"
                exit 498
            }
        }
        local TYPE: type `V'
        capt n _check_types `type' `TYPE' // may update type
        if _rc==1   exit _rc
        if _rc==109 exit _rc // type mismatch
        if _rc {
            local recast `recast' `type' `V'
        }
    }
    local dups: list dups TARGET
    if `"`dups'"'!="" {
        di as err "duplicates not allowed in list of target variables"
        exit 198
    }
    local K = c(k)
    while (`"`addvars'"'!="") {
        gettoken type addvars : addvars
        gettoken V    addvars : addvars
        mata: (void) st_addvar("`type'", "`V'")
    }
    while (`"`recast'"'!="") {
        gettoken type recast : recast
        gettoken V    recast : recast
        recast `type' `V'
    }
    local N = _N
    local cframe = c(frame)
    capt n mata: _append("`frame'", "`touse'",/*
        */ tokens(st_local("varlist")), tokens(st_local("TARGET")))
    if _rc {
        frame change `cframe'
        exit _rc
    }
    local N = _N - `N'
    di as txt "(`N' observations appended)"
    local K = c(k) - `K'
    if `K' di as txt "(`K' new variables created)"
end

program _check_types
    args type TYPE
    if "`type'"=="`TYPE'" exit
    local str = substr("`type'",1,3)=="str"
    local STR = substr("`TYPE'",1,3)=="str"
    if `str'!=`STR' {
        di as err "type mismatch; may not combine numeric and string"
        exit 109
    }
    if `str' {
        local num = substr("`type'",4,.)
        local NUM = substr("`TYPE'",4,.)
        if "`NUM'"=="L" exit
        if "`num'"=="L" exit 499 // => recast
        if `NUM'<`num'  exit 499 // => recast
        exit
    }
    if "`TYPE'"=="double" exit
    if "`TYPE'"=="float" {
        if "`type'"=="double" exit 499 // => recast
        if "`type'"=="long" {
            c_local type "double" // long + float => recast to double
            exit 499
        }
        exit
    }
    if "`TYPE'"=="long" {
        if "`type'"=="double" exit 499 // => recast
        if "`type'"=="float" {
            c_local type "double" // long + float => recast to double
            exit 499
        }
        exit
    }
    if "`TYPE'"=="int" {
        if "`type'"=="double" exit 499 // => recast
        if "`type'"=="float"  exit 499 // => recast
        if "`type'"=="long"   exit 499 // => recast
        exit
    }
    exit 499 // TYPE is bite => recast
end

program _geoframe_translate
    gettoken subcmd : 0, parse(" ,")
    local subcmd = strlower(`"`subcmd'"')
    if `"`subcmd'"'=="esri" gettoken subcmd 0 : 0, parse(" ,")
    __geoframe_translate_esri `0'
end

program __geoframe_translate_esri
    syntax [anything(equalok)] [using] [, * ]
    if `:list sizeof using'==0 {
        if `:list sizeof anything'>1 {
            gettoken outname anything : anything
        }
        if `:list sizeof anything' {
            local anything `"using `macval(anything)'"'
        }
        local 0 `macval(outname)' `macval(anything)', `macval(options)'
    }
    syntax [anything(name=outname)] using/ [, replace ]
    mata: _translate_using(st_local("using")) // => using path basename
    mata: _translate_outname(st_local("outname")) // => outpath outname
    if `"`outname'"'=="" local outname `"`basename'"'
    local hasoutpath: list sizeof outpath
    if `hasoutpath' {
        mata: st_local("shpfile", pathjoin(`"`outpath'"', `"`outname'"'))
    }
    else local shpfile `"`outname'"'
    local shpfile `"`"`shpfile'"'"'
    local shpfile: list clean shpfile 
    local pwd = c(pwd)
    nobreak {
        if `hasoutpath' local pwd = c(pwd)
        capture noisily break {
            if `hasoutpath' qui cd `"`outpath'"'
            di ""
            spshape2dta `"`using'"', saving(`"`outname'"') `replace'
            if `hasoutpath' di as txt `"  (in directory `outpath')"'
            else            di as txt "  (in current directory)"
        }
        local rc = _rc
        if `hasoutpath' qui cd `"`pwd'"'
        if `rc' exit `rc'
    }
    di _n as txt "  (type "/*
        */ `"{bf:{stata geoframe create `shpfile'}}"'/*
        */ " to load the data)"
end

version 16.1
mata:
mata set matastrict on

void _add_path_and_sfx_to_shpf(string scalar shpf, string scalar mainf)
{
    string scalar path, fn, SHPF
    pragma unset path
    pragma unset fn
    
    SHPF = st_local(shpf)
    if (pathsuffix(SHPF)=="") SHPF = SHPF + ".dta"
    pathsplit(SHPF, path, fn)
    if (path=="") { // shape file has no path
        pathsplit(st_local(mainf), path, fn)
        if (path!="") { // main file has path
            SHPF = pathjoin(path, SHPF)
        }
    }
    st_local(shpf, SHPF)
}

void _clean_empty(string scalar empty, string scalar id, string rowvector xy,
    string scalar touse)
{
    real scalar    i, nempty
    real colvector ID, E, a, b 
    real matrix    XY

    if (length(xy)>2) xy = xy[(1,2)] // ignore X2 Y2
    st_view(E=., ., empty, touse)
    st_view(ID=., ., id, touse)
    st_view(XY=., ., xy, touse)
    a = selectindex(_mm_unique_tag(ID))
    i = rows(a)
    if (i<=1) b = rows(ID)
    else      b = a[|2 \. |] :- 1 \ rows(ID)
    nempty = 0
    for (;i;i--) {
        if (all(rowmissing(XY[|a[i],1 \ b[i],2|]))) {
            E[|a[i] \ b[i]|] = J(b[i]-a[i]+1, 1, 1)
            nempty++
        }
    }
    st_local("Nempty", strofreal(nempty, "%18.0g"))
}

void _flip(string scalar nm)
{
    real scalar      i
    real rowvector   p
    string rowvector S
    
    S = tokens(st_local(nm))
    i = cols(S)
    p = J(1,i,i)
    if (mod(i,2)) i-- // skip last if uneven
    for (;i;i--) {
        p[i] = i - 1
        i--
        p[i] = i + 1
    }
    st_local(nm, invtokens(S[p]))
}

void _orientation(string scalar R, string rowvector id,
    string rowvector xy, string scalar touse)
{
    real scalar    i, n, dir, pos, neg, na
    real colvector a, b 
    real matrix    ID, XY

    st_view(ID=., ., id, touse)
    st_view(XY=., ., xy, touse)
    n = rows(ID)
    a = selectindex(_mm_uniqrows_tag(ID))
    i = rows(a)
    if (i<=1) b = n
    else      b = a[|2 \. |] :- 1 \ n
    pos = neg = na = 0
    for (;i;i--) {
        dir = geo_orientation(XY[|a[i],1 \ b[i],2|])
        if (dir==1)       pos++ 
        else if (dir==-1) neg++
        else              na++
    }
    st_matrix(R, neg \ pos \ na)
}

void _gtype(string scalar R, string rowvector id,
    string rowvector xy, string scalar touse)
{
    real scalar    i, n, ai, bi, poly, line, point, empty
    real colvector a, b 
    real matrix    ID, XY

    st_view(ID=., ., id, touse)
    st_view(XY=., ., xy, touse)
    n = rows(ID)
    a = selectindex(_mm_uniqrows_tag(ID))
    i = rows(a)
    if (i<=1) b = n
    else      b = a[|2 \. |] :- 1 \ n
    poly = line = point = empty = 0
    for (;i;i--) {
        ai = a[i]; bi = b[i] 
        if (ai==bi) {
            if (hasmissing(XY[ai,])) empty++
            else                     point++
            continue
        }
        if (hasmissing(XY[ai,])) ai++
        else                     bi--
        if (ai==bi)                point++ // only one valid obs
        else if ((ai+1)==bi)       line++  // only two valid obs
        else if (XY[ai,]==XY[bi,]) poly++
        else                       line++
    }
    st_matrix(R, poly \ line \ point \ empty)
}

void _noclip(string scalar touse,
    string scalar id, string scalar pid, real scalar rclip,
    string scalar out, string rowvector xy, string scalar mask,
    real scalar strict, real scalar split, real scalar nodrop,
    real scalar dots)
{
    real scalar    i, r, n
    real colvector a, b
    real colvector ID, PID, OUT, p
    real matrix    XY
    real rowvector ab

    st_view(ID=., ., id, touse)
    st_view(PID=., ., pid, touse)
    st_view(OUT=., ., out, touse)
    n = rows(ID)
    a = selectindex(_mm_uniqrows_tag((ID,PID)))
    i = rows(a)
    if (i<=1) b = n
    else      b = a[|2 \. |] :- 1 \ n
    if (dots) {
        printf("{txt}(clipping %g shape items ...", i)
        displayflush()
    }
    if (!rclip) {
        st_view(XY=., ., xy, touse)
        p = selectindex(!rowmissing(XY))
        OUT[p] = !__geo_clip_point(XY[p,],
                 _geo_clip_mask(st_matrix(mask), "matname"))
    }
    for (;i;i--) {
        ab = a[i] \ b[i]
        r  = b[i] - a[i] + 1
        if (anyof(OUT[|ab|], strict)) OUT[|ab|] = J(r, 1, strict)
        else                          OUT[|ab|] = J(r, 1, 1-strict)
    }
    if (!split) {
        a = selectindex(_mm_unique_tag(ID))
        i = rows(a)
        if (i<=1) b = n
        else      b = a[|2 \. |] :- 1 \ n
        for (;i;i--) {
            ab = a[i] \ b[i]
            r  = b[i] - a[i] + 1
            if (anyof(OUT[|ab|], strict)) OUT[|ab|] = J(r, 1, strict)
            else                          OUT[|ab|] = J(r, 1, 1-strict)
        }
    }
    if (nodrop) {
        if (rclip) st_view(XY=., ., xy, touse)
        if (split) {
            a = selectindex(_mm_unique_tag(ID))
            i = rows(a)
            if (i<=1) b = n
            else      b = a[|2 \. |] :- 1 \ n
        }
        for (;i;i--) {
            ab = a[i] \ b[i]
            r  = b[i] - a[i] + 1
            if (all(OUT[|ab|])) {
                OUT[a[i]] = 0
                XY[a[i],] = (.,.)
            }
        }
    }
    if (dots) printf(" done)\n")
}

struct _clip_expand_info {
    pointer rowvector XY // copy of clipped polygon
    pointer rowvector ab // range of original polygon in data
}

void _clip(string scalar touse,
    string scalar id, string scalar pid, real scalar rclip,
    string scalar out, string rowvector xy, string scalar mask,
    real scalar line, real scalar nodrop, real scalar dots)
{
    real scalar    i, n, r, run, dn, d0
    real colvector a, b
    real colvector ID, PID, OUT
    real rowvector ab
    real matrix    MASK, XY, XYi
    struct _clip_expand_info scalar I
    
    if (rclip) MASK = vec(st_matrix(mask))
    else       MASK = _geo_clip_mask(st_matrix(mask), "matname")
    st_view(ID=., ., id, touse)
    st_view(PID=., ., pid, touse)
    st_view(OUT=., ., out, touse)
    st_view(XY=., ., xy, touse)
    n = rows(OUT)
    a = selectindex(_mm_uniqrows_tag((ID,PID)))
    i = rows(a)
    if (i<=1) b = n
    else      b = a[|2 \. |] :- 1 \ n
    if (dots) {
        displayas("txt")
        printf("(clipping %g shape items)\n", i)
        d0 = _geo_progress_init("(")
        dn = i
    }
    for (;i;i--) {
        if (dots) _geo_progressdots(1-i/dn, d0)
        ab = a[i] \ b[i]
        r  = b[i] - a[i] + 1
        run = rclip ? anyof(OUT[|ab|], 1) : 1
        if (run) { // has points outside (or !rclip)
            run = rclip ? anyof(OUT[|ab|], 0) : 1
            if (run) { // has points inside (or !rclip)
                XYi = _geo_clip(XY[|ab, (1\2)|], MASK, line)
                if (rows(XYi)>r) {
                    // too many observations; handle later
                    OUT[|ab|] = J(r, 1, 0)
                    I.XY = I.XY, &XYi[.,.]
                    I.ab = I.ab, &ab[.]
                    continue
                }
                OUT[|ab|] = J(r, 1, 1)
                r = rows(XYi)
                if (!r) { // (should not happen if rclip)
                    if (nodrop) {
                        OUT[a[i]] = 0
                        XY[a[i],] = (.,.)
                    }
                    continue 
                }
                ab = ab[1] \ ab[1] + r - 1
                OUT[|ab|] = J(r, 1, 0)
                XY[|ab, (1\2)|] = XYi
                continue
            }
            OUT[|ab|] = J(r, 1, 1) // all points outside (rclip only)
            if (nodrop) {
                OUT[a[i]] = 0
                XY[a[i],] = (.,.)
            }
            continue
        }
        if (anyof(OUT[|ab|], 0)) OUT[|ab|] = J(r, 1, 0) // all in (rclip only)
        else {
            OUT[|ab|] = J(r, 1, 1) // all points missing (rclip only)
            if (nodrop) OUT[a[i]] = 0
        }
    }
    _clip_expand(touse, I.XY, I.ab, xy) // store polygons with additional points
    if (dots) {
        _geo_progressdots(1, d0)
        display(")")
    }
}

void _clip_expand(string scalar touse, pointer rowvector XY, 
    pointer rowvector ab, string rowvector xy)
{
    real scalar    l, i, r, n
    string scalar  p, s
    real colvector V
    
    l = length(ab)
    if (!l) return // nothing to do
    // expand data
    s = st_tempname()
    stata("generate double " + s + " = _n") // sort index
    p = st_tempname()
    stata("generate double " + p + " = 1") // count variable for expand
    st_view(V=.,., p, touse)
    n = 0
    for (i=l;i;i--) {
        r = rows(*XY[i]) - ((*ab[i])[2] - (*ab[i])[1] + 1) // additional obs
        V[(*ab[i])[2]] = r + 1 // expand last obs of original polygon
        *ab[i] = *ab[i] + (n \ n+r) // update range index
        n = n + r
    }
    stata("qui expand " + p)
    stata("sort " + s)
    st_local("Nadd", strofreal(n, "%18.0g"))
    // store clipped polygons
    st_view(V=., ., xy, touse)
    for (i=l;i;i--) {
        V[|*ab[i],(1\2)|] = *XY[i]
    }
}

void _simplify(string scalar id, string scalar out, string rowvector xy,
    string scalar touse, real scalar delta, real scalar jointly,
    real scalar nodrop, real scalar dots)
{
    real scalar    i, n, dn, d0
    real colvector a, b
    real colvector ID, OUT, B
    real matrix    XY
    real rowvector ab

    st_view(ID=., ., id, touse)
    st_view(OUT=., ., out, touse)
    st_view(XY=., ., xy, touse)
    if (jointly) B = geo_bshare(ID, geo_pid(ID, XY), XY, 1, !dots)
    n = rows(OUT)
    a = selectindex(_mm_unique_tag(ID))
    i = rows(a)
    if (i<=1) b = n
    else      b = a[|2 \. |] :- 1 \ n
    if (dots) {
        displayas("txt")
        printf("(simplifying shapes of %g units)\n", i)
        d0 = _geo_progress_init("(")
        dn = i
    }
    for (;i;i--) {
        ab = a[i] \ b[i]
        if (jointly) OUT[|ab|] = !geo_simplify(XY[|ab, (1\2)|], delta, B[|ab|])
        else         OUT[|ab|] = !geo_simplify(XY[|ab, (1\2)|], delta)
        if (nodrop) {
            if (all(OUT[|ab|])) {
                OUT[a[i]] = 0 // keep unit in data
                XY[a[i],] = (.,.)
            }
        }
        if (dots) _geo_progressdots(1-(i-1)/dn, d0)
    }
    if (dots) display(")")
}

void _bshare(string scalar id, string scalar pid, string scalar out,
    string rowvector xy, string scalar touse, real scalar rtype,
    real scalar nodrop, real scalar dots)
{
    real scalar    i, n
    real colvector a, b
    real colvector ID, PID, OUT
    real matrix    XY

    st_view(ID=., ., id, touse)
    st_view(PID=., ., pid, touse)
    st_view(OUT=., ., out, touse)
    st_view(XY=., ., xy, touse)
    // identify border points
    OUT[.] = !geo_bshare(ID, PID, XY, rtype, !dots)
    // lines may be disconnected at polygon origin; fix this
    n = rows(OUT)
    a = selectindex(_mm_uniqrows_tag((ID,PID)))
    i = rows(a)
    if (i<=1) b = n
    else      b = a[|2 \. |] :- 1 \ n
    for (;i;i--) {
        if (OUT[a[i]]) continue // all out
        _bshare_wrap(OUT, XY, a[i], b[i])
    }
    // separate lines by missings (and handle nodrop)
    a = selectindex(_mm_unique_tag(ID))
    i = rows(a)
    if (i<=1) b = n
    else      b = a[|2 \. |] :- 1 \ n
    for (;i;i--) {
        if (all(OUT[|a[i] \ b[i]|])) { // no border points
            if (nodrop) {
                OUT[a[i]] = 0
                XY[a[i],] = (.,.)
            }
        }
        else _bshare_addmis(OUT, XY, a[i], b[i])
    }
}

void _bshare_wrap(real colvector out, real matrix XY, real scalar a,
    real scalar b)
{
    real scalar i
    
    if (!any(out[|a\b|])) return // all in; nothing to do
    // select non-missing section
    for (;a<=b;a++) {
        if (!missing(XY[a,])) break
    }
    if (a>b) return // all missing
    for (;b;b--) {
        if (!missing(XY[b,])) break
    }
    // wrap around if needed
    if (XY[a,]!=XY[b,]) return // not a polygon
    if (out[a])         return // origin not within shared border
    for (i=a;i<=b;i++) { // find first outside point; make this the new origin
        if (out[i]) break
    }
    out[|a\b|]    = out[|i\b|]    \ out[|a+1\i|]
    XY[|a,1\b,2|] = XY[|i,1\b,2|] \ XY[|a+1,1\i,2|]
}

void _bshare_addmis(real colvector out, real matrix XY, real scalar a,
    real scalar b)
{
    real scalar mfirst
    
    if (!any(out[|a\b|])) return // all in; nothing to do
    // keep first obs if missing; else tag as missing-comes-last
    mfirst = hasmissing(XY[a,])
    if (mfirst) out[a++] = 0
    // skip to first inside point that is nonmissing
    for (;a<=b;a++) {
        if (!out[a]) {
            if (hasmissing(XY[a,])) out[a] = 1
            else break
        }
    }
    // now add missing before each segment
    for (++a;a<=b;a++) {
        if (hasmissing(XY[a,])) out[a] = 1
        else if (!out[a]) {
            if (out[a-1]) {
                out[a-1] = 0
                XY[a-1,] = (.,.)
            }
        }
    }
    // keep last point if missing-comes-last
    if (!mfirst) {
        if (hasmissing(XY[b,])) out[b] = 0
    }
}

void _centroid(string rowvector centr, string scalar id, string scalar xy)
{
    real colvector ID
    real matrix    XY
    
    st_view(ID=., ., id)
    st_view(XY=., ., xy)
    st_store(., centr, geo_centroid(1, ID, XY))
}

void _area(string scalar area, string scalar id, string scalar xy)
{
    real colvector ID
    real matrix    XY
    
    st_view(ID=., ., id)
    st_view(XY=., ., xy)
    st_store(., area, geo_area(1, ID, XY))
}

void _pid(string scalar pid, string scalar id, string scalar xy)
{
    real colvector ID
    real matrix    XY
    
    st_view(ID=., ., id)
    st_view(XY=., ., xy)
    st_store(., pid, geo_pid(ID, XY))
}

void _plevel(real scalar nodots, string scalar pl, string scalar id,
    string scalar pid, string scalar xy, string scalar touse,
    | string scalar BY, real scalar lvl)
{
    real colvector ID, PID, PL, p, L
    real matrix    XY
    
    if (BY!="") {
        p   = selectindex(st_data(., BY, touse):==lvl)
        ID  = st_data(., id,  touse)[p]
        PID = st_data(., pid, touse)[p]
        XY  = st_data(., xy,  touse)[p,]
        L = geo_plevel(1, ID, PID, XY, nodots)
        st_view(PL=., ., pl,  touse)
        PL[p,] = L
    }
    else {
        st_view(ID=.,  ., id,  touse)
        st_view(PID=., ., pid, touse)
        st_view(XY=.,  ., xy,  touse)
        L = geo_plevel(1, ID, PID, XY, nodots)
        st_store(., pl, touse, L)
    }
    st_local("N", strofreal(sum(L:!=0 :& _mm_uniqrows_tag((ID,PID))), "%18.0g"))
}

void _spjoin(string scalar frame, string scalar id1, string scalar xy1,
    string scalar touse1, string scalar id, string scalar pid, string scalar xy,
    string scalar pl, string scalar touse, real scalar nodots)
{
    real colvector ID, PID, PL
    real matrix    XY, XY1
    
    st_view(ID=.,  ., id, touse)
    st_view(PID=., ., pid, touse)
    st_view(XY=.,  ., xy, touse)
    if (pl!="") st_view(PL=., ., pl, touse)
    else        PL = J(0,1,.)
    st_framecurrent(frame)
    stata("*") // fixes an issue with frames and views; may become redundant
    st_view(XY1=., ., xy1, touse1)
    st_store(., id1, touse1, geo_spjoin(XY1, ID, PID, XY, PL, nodots))
}

void _bbox1(string scalar frame, string scalar xy, string scalar touse,
     real scalar btype, real scalar k, real scalar pad, real scalar adj,
     real scalar angle, | string scalar BY, real scalar id)
{
    string scalar  cframe
    real scalar    n, n0, n1
    real matrix    XY, PT
    
    // generate shape
    if (BY!="") XY = select(st_data(., xy, touse), st_data(., BY, touse):==id)
    else        st_view(XY=., ., xy, touse)
    if      (btype==3) PT = __bbox_hull(XY, pad)
    else if (btype==2) PT = __bbox_mec(XY, k, pad, adj, angle)
    else               PT = __bbox(XY, btype, pad)
    // store shape
    cframe = st_framecurrent()
    st_framecurrent(frame)
    n = rows(PT)
    n0 = st_nobs()
    st_addobs(n)
    n1 = n0 + n; n0 = n0 + 1
    st_store((n0,n1), "_ID", J(n, 1, id))
    st_store((n0,n1), ("_X","_Y"), PT)
    st_framecurrent(cframe)
}

void _bbox2(string scalar frame, string scalar xy, string scalar touse,
     real scalar btype, real scalar k, real scalar pad, real scalar adj,
     real scalar angle, string scalar BY)
{   // BY equal to ID; assuming data ordered by ID
    string scalar  cframe
    real scalar    n, n0, n1, i
    real matrix    XY, ID, a, b
    real colvector N
    pointer (real matrix) colvector PT

    // data
    st_view(XY=., ., xy, touse)
    st_view(ID=., ., BY, touse)
    a = selectindex(_mm_unique_tag(ID))
    n = rows(a)
    if (n<=1) b = rows(XY)
    else      b = a[|2 \. |] :- 1 \ rows(XY)
    // generate shapes
    N = J(n,1,.)
    PT = J(n,1,NULL)
    for (i=n;i;i--) {
        if      (btype==3) PT[i] = &__bbox_hull(XY[|a[i],1\b[i],.|], pad)
        else if (btype==2) PT[i] = &__bbox_mec(XY[|a[i],1\b[i],.|], k, pad,
                                    adj, angle)
        else               PT[i] = &__bbox(XY[|a[i],1\b[i],.|], btype, pad)
        N[i] = rows(*PT[i])
    }
    // store shapes
    cframe = st_framecurrent()
    st_framecurrent(frame)
    n1 = st_nobs()
    st_addobs(sum(N))
    for (i=1;i<=n;i++) {
        n0 = n1 + 1
        n1 = n0 + N[i] - 1
        st_store((n0,n1), "_ID", J(n1-n0+1, 1, ID[a[i]]))
        st_store((n0,n1), ("_X","_Y"), *PT[i])
    }
    st_framecurrent(cframe)
}

real matrix __bbox_hull(real matrix XY, real scalar pad)
{
    real rowvector c
    real matrix    xy
    
    xy = geo_hull(XY)
    if (pad) {
        c = _geo_centroid(xy)
        xy = (xy :- c) * (1 + pad/100) :+ c
    }
    return((.,.) \ xy)
}

real matrix __bbox_mec(real matrix XY, real scalar n, real scalar pad,
    real scalar adj, real scalar angle)
{
    real rowvector c
    real matrix    xy
    
    c = geo_welzl(XY)
    if (adj) c[3] = sqrt(c[3]^2 / (1-sin(pi()/n)^2))
    if (pad) c[3] = c[3] * (1 + pad/100)
    xy = _geo_symbol_circle(n)
    _geo_rotate(xy, angle)
    return((.,.) \ c[(1,2)] :+ c[3] * xy)
}

real matrix __bbox(real matrix XY, real scalar type, real scalar pad)
{
    real scalar xmid, ymid
    real matrix xy
    
    xy = geo_bbox(XY, type)
    if (pad) {
        xmid = sum(minmax(xy[,1])) / 2
        ymid = sum(minmax(xy[,2])) / 2
        xy = (xy :- (xmid,ymid)) * (1 + pad/100) :+  (xmid,ymid)
    }
    return((.,.) \ xy)
}

void _append(string scalar frame, string scalar touse, string rowvector vars,
    string rowvector VARS)
{
    real scalar       i, k, n, n0
    pointer rowvector X
    string scalar     cframe

    cframe = st_framecurrent()
    k = length(vars)
    X = J(1, k, NULL)
    st_framecurrent(frame)
    for (i=1;i<=k;i++) {
        if (st_isstrvar(vars[i])) X[i] = &st_sdata(., vars[i], touse)
        else                      X[i] =  &st_data(., vars[i], touse)
    }
    n = rows(*X[1])
    st_framecurrent(cframe)
    n0 = st_nobs()
    st_addobs(n)
    n = n0 + n; n0 = n0 + 1
    for (i=1;i<=k;i++) {
        if (st_isstrvar(VARS[i])) st_sstore((n0,n), VARS[i], *X[i])
        else                       st_store((n0,n), VARS[i], *X[i])
    }
}

void _translate_using(string scalar fn)
{
    string scalar    path, bn
    string colvector dir
    pragma unset path
    
    bn = pathbasename(fn)
    if (bn!="") {
        // check whether fn is indeed a file; using a workaround because mata's
        // fileexists() seems to return 1 also if fn is, in fact, a directory
        stata(sprintf(`"local FEXISTS = fileexists(`"%s"')"', fn))
        if (st_local("FEXISTS")=="0") {
            if (direxists(fn)) bn = ""
        }
        st_local("FEXISTS", "")
    }
    if (bn!="") { // [path/]filename specified
        pathsplit(fn, path, bn)
    }
    else { // only path specified
        path = fn
        if (!direxists(path)) {
            errprintf("directory %s does not exist", path)
            exit(601)
        }
        dir = dir(path, "files", "*.shp")
        if (!length(dir)) {
            errprintf("no ESRI shapefile found in directory %s\n", path)
            exit(601)
        }
        bn = dir[1] // use first match
        printf("{txt}(translating {bf:%s} from directory %s)\n",
            pathrmsuffix(bn), path)
    }
    if (!pathisabs(path)) path = pathjoin(pwd(), path) // make path absolute
    st_local("using", pathjoin(path, bn))
    st_local("path", path)
    st_local("basename", pathrmsuffix(bn))
}

void _translate_outname(fn)
{
    string scalar path, bn
    pragma unset path
    
    bn = pathbasename(fn)
    if (bn!="") { // [path/]filename specified
        pathsplit(fn, path, bn)
        bn = pathrmsuffix(bn)
    }
    else { // only path specified
        path = fn
        if (!direxists(path)) {
            errprintf("directory %s does not exist", path)
            exit(601)
        }
    }
    st_local("outname", bn)
    st_local("outpath", path)
}


end


