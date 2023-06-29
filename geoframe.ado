*! version 1.0.3  29jun2023  Ben Jann

program geoframe
    version 17
    gettoken subcmd 0 : 0, parse(" ,")
    _parse_subcmd `subcmd'
    _geoframe_`subcmd' `macval(0)'
    if `"`local'"'!="" { // pass through returns (_geoframe_get, _geoframe_flip)
        c_local `local' `"`value'"'
    }
end

program _parse_subcmd
    local l = strlen(`"`0'"')
    if      `"`0'"'=="get"                              local 0 get
    else if `"`0'"'=="set"                              local 0 set
    else if `"`0'"'=="flip"                             local 0 flip // undocumented
    else if `"`0'"'==substr("create",    1, max(2,`l')) local 0 create
    else if `"`0'"'==substr("link",      1, max(1,`l')) local 0 link
    else if `"`0'"'==substr("clean",     1, max(2,`l')) local 0 clean
    else if `"`0'"'==substr("select",    1, max(3,`l')) local 0 select
    else if `"`0'"'==substr("describe",  1, max(1,`l')) local 0 describe
    else if `"`0'"'==substr("generate",  1, max(1,`l')) local 0 generate
    else if `"`0'"'==substr("bbox",      1, max(2,`l')) local 0 bbox
    else if `"`0'"'==substr("rename",    1, max(3,`l')) local 0 rename
    else if `"`0'"'==substr("duplicate", 1, max(3,`l')) local 0 duplicate
    else if `"`0'"'==substr("relink",    1, max(3,`l')) local 0 relink
    else if `"`0'"'==substr("unlink",    1, max(3,`l')) local 0 unlink
    else if `"`0'"'==substr("attach",    1, max(2,`l')) local 0 attach
    else if `"`0'"'==substr("detach",    1, max(3,`l')) local 0 detach
    else if `"`0'"'==substr("append",    1, max(2,`l')) local 0 append
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
                // check whether shapefile is declared in characteristics
                if "`type'"=="unit" {        // only do this if type=unit
                    if `hasSHP'==0 {         // only if shpfile() not specified
                        if "`shpfile'"=="" { // only noshpfile not specified
                            __geoframe_create_hasshpf `"`macval(using)'"'
                        }
                    }
                }
                // read shapefile and establish linkage
                if `hasSHP' {
                    if "`SHP_frame'"=="" local SHP_frame "`frame'_shp"
                    else if "`SHP_frame'"=="`frame'" {
                        di as err "name for shape frame and"/*
                            */ " name for main frame may not be the same"
                        exit 498
                    }
                    _geoframe_create `SHP_frame' using `macval(SHP_using)'/*
                        */, type(shape) id(`SHP_id') sid(`SHP_sid') pid(`SHP_pid')/*
                        */  plevel(`SHP_plevel') coordinates(`SHP_coord')/*
                        */  feature(`SHP_feat') `replace'/*
                        */  nodescribe nocurrent _mkfrlink(`frame') `drop'
                }
                else if `isSHPread' {
                    if "`drop'"!="" local cleanshp
                    else            local cleanshp , clean(shp)
                    frame `_mkfrlink': _geoframe_link `frame'`cleanshp'
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
    args path
    local shpf: char _dta[sp__shp_dta]
    if `"`shpf'"'=="" exit // no shape file
    if `"`macval(path)'"'!="" mata: _add_path_to_shpf("shpf", "path")
    capt confirm file `"`macval(shpf)'"'
    if _rc==1 exit 1
    if _rc {
        di as txt `"(shape file '`macval(shpf)'' not found)"'
        exit
    }
    di as txt `"(reading shapes from `macval(shpf)')"'
    c_local SHP_using `"`"`macval(shpf)'"'"'
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
    di as txt "(link to frame {cmd:`shpframe'} added)"
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
            // tag shapes with only 1 obs
            qui gen byte `empty' = `lnkvar'<. &/*
                */ !inlist(`shpid', `shpid'[_n-1], `shpid'[_n+1])
            // check coordinates if available
            geoframe get coordinates, local(coord)
            foreach c of local coord {
                qui replace `empty' = 0 if `c'<.
            }
            // count empty shapes
            qui count if `empty'
            local Nempty = r(N)
        }
    }
    else local empty
    // create reverse link from shape file to unit file
    if "`units'"!="" | "`noempty'"=="" {
        tempname tmpframe tag
        frame `shpframe' {
            qui gen byte `tag' = `lnkvar'<. & `shpid'!=`shpid'[_n-1]
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
                di as txt "(dropped `Nempty' empty shape `msg'"/*
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
    frame `newname' {
        if `hasIF' {
            qui keep `if' `in'
            if r(N_drop)==1 local msg observation
            else            local msg observations
            local Ndrop `: di %9.0gc `r(N_drop)''
            di as txt "(dropped `Ndrop' `msg' in frame {bf:`newname'})"
        }
        if `hasSHP' {
            if "`unlink'"=="" {
                qui geoframe link `newshpname'
                if `hasIF' & "`noshp'"=="" {
                    geoframe get linkname, local(lnkvar)
                    frame `newshpname' {
                        qui keep if `lnkvar'<.
                        if r(N_drop)==1 local msg observation
                        else            local msg observations
                        local Ndrop `: di %9.0gc `r(N_drop)''
                        di as txt "(dropped `Ndrop' `msg' in shape frame"/*
                            */ " {bf:`newshpname'})"
                    }
                }
            }
            else if `newFRM'==0 {
                geoframe unlink `shpframe'
            }
        }
    }
    // frame change/reporting
    if `newFRM' {
        if "`current'"=="" {
            frame change `newname'
            di as txt "(current frame now {bf:`newname'})"
        }
    }
    if "`describe'"=="" _geoframe_describe `newname'
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

program _geoframe_generate
    gettoken fcn 0 : 0, parse(" ,=")
    local fcn = strlower(`"`fcn'"')
    local l = strlen(`"`fcn'"')
    if      `"`fcn'"'==substr("centroids", 1, max(3,`l')) local fcn centroids
    else if `"`fcn'"'==substr("plevel", 1, max(2,`l'))    local fcn plevel
    else {
        capt mata: assert(st_islmname(st_local("fcn")))
        if _rc==1 exit 1
        if _rc {
            di as err `"`fcn': invalid function"'
            exit 198
        }
    }
    _geoframe_generate_`fcn' `0'
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
    }
    else {
        geoframe get shpframe, local(shpframe) strict
        geoframe get id, l(ID0) strict
    }
    frame `shpframe' {
        if "`shpframe'"=="`cframe'" local ID `ID0'
        else geoframe get id, l(ID) strict
        geoframe get coordinates, l(XY) strict
        tempvar CX CY
        qui gen double `CX' = .
        qui gen double `CY' = .
        mata: _centroid(("`CX'","`CY'"), `"`ID'"', `"`XY'"')
        if "`shpframe'"!="`cframe'" {
            mata: _copy_by_ID("`cframe'", `"`ID0'"', `"`ID'"', "`CX' `CY'")
        }
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
    }
    else {
        geoframe get shpframe, local(shpframe) strict
        geoframe get id, l(ID0) strict
    }
    frame `shpframe' {
        if "`shpframe'"=="`cframe'" local ID `ID0'
        else geoframe get id, l(ID) strict
        geoframe get coordinates, l(XY) strict
        tempvar AREA
        qui gen double `AREA' = .
        mata: _area("`AREA'", `"`ID'"', `"`XY'"')
        if "`shpframe'"!="`cframe'" {
            mata: _copy_by_ID("`cframe'", `"`ID0'"', `"`ID'"', "`AREA'")
        }
    }
    if `"`scale'"'!="" {
        qui replace `AREA' = `AREA' * (`scale')
    }
    capt confirm new variable `namelist'
    if _rc==1 exit _rc
    if _rc drop `namelist'
    rename `AREA' `namelist'
    if "`set'"=="" _geoframe_set area `namelist'
    di as txt "(variable {bf:`namelist'} added to frame {bf:`cframe'})"
end

program _geoframe_generate_pid
    syntax [name] [, replace noset ]
    if "`namelist'"=="" local namelist _PID
    local cframe `"`c(frame)'"'
    geoframe get type, l(type)
    if "`type'"=="shape" local shpframe `"`cframe'"'
    else geoframe get shpframe, local(shpframe) strict
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

program _geoframe_generate_plevel
    syntax [name] [if] [in] [, by(varname numeric) replace noset ]
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
            tempvar TOUSE
            qui gen byte `TOUSE' = 0
            qui levelsof `BY' if `touse', local(bylvls)
            foreach lvl of local bylvls {
                qui replace `TOUSE' = `BY'==`lvl' if `touse'
                mata: _plevel("`PL'", `"`ID'"', `"`PID'"', `"`XY'"', "`TOUSE'")
                qui count if `PL' & (`ID'!=`ID'[_n-1] | `PID'!=`PID'[_n-1])/*
                    */ & `TOUSE'
                if `r(N)'==1 local msg polygon
                else         local msg polygons
                di as txt "(`by'=`lvl': `r(N)' nested `msg' found)"
            }
        }
        else {
            mata: _plevel("`PL'", `"`ID'"', `"`PID'"', `"`XY'"', "`touse'")
            qui count if `PL' & (`ID'!=`ID'[_n-1] | `PID'!=`PID'[_n-1])/*
                */ & `touse'
            if `r(N)'==1 local msg polygon
            else         local msg polygons
            di as txt "(`r(N)' nested `msg' found)"
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

program _geoframe_spjoin
    syntax [namelist(min=1 max=2)] [if] [in] [,/*
        */ COordinates(varlist min=2 max=2) replace noset ]
    marksample touse
    gettoken shpframe namelist : namelist
    gettoken id       namelist : namelist
    if "`id'"=="" local id _ID
    if "`replace'"=="" confirm new variable `id'
    if `"`coordinates'"'!="" local xy `coordinates'
    else geoframe get coordinates, l(xy) strict
    markout `touse' `coordinates'
    local frame `"`c(frame)'"'
    local TOUSE
    frame `shpframe' {
        // check whether shpframe is an attribute frame linked to a shape frame
        geoframe get type, l(type)
        if "`type'"!="shape" {
            geoframe get shpframe, local(shpframe2)
            if `"`shpframe2'"'!="" {
                tempvar TOUSE
                qui gen `TOUSE' = 1
                geoframe get linkname, local(lnkvar) strict
                frame `shpframe2' {
                    qui frget `TOUSE' = `TOUSE', from(`lnkvar')
                    qui replace `TOUSE' = 0 if `TOUSE'>=.
                }
                local shpframe `shpframe2'
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
        */ "`ID'", "`PID'", "`XY'", "`PL'", "`TOUSE'")
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
        if "`coordinates'"!="" geoframe set coordinates `xy'
    }
    di as txt "(variable {bf:`id'} added to frame {bf:`frame'})"
end

program _geoframe_bbox
    syntax name(id="newname" name=newname) [if] [in] [, by(varname numeric)/*
        */ CIRcle n(numlist int max=1 >0) PADding(real 0) replace ]
    if "`n'"=="" local n 100
    local mec = "`circle'"!=""
    if "`replace'"=="" confirm new frame `newname'
    // mark sample
    marksample touse
    if "`by'"!="" markout `touse' `by'
    // find shapes
    local cframe `"`c(frame)'"'
    geoframe get type, l(type)
    if "`type'"!="shape" geoframe get shpframe, local(shpframe)
    if `"`shpframe'"'=="" {
        geoframe get coordinates, l(XY) strict
        markout `touse' `XY'
        local shpframe `"`cframe'"'
        local BY `by'
    }
    else {
        geoframe get linkname, local(lnkvar) strict
        frame `shpframe' {
            qui frget `touse' = `touse', from(`lnkvar')
            qui replace `touse' = 0 if `touse'>=.
            geoframe get coordinates, l(XY) strict
            markout `touse' `XY'
            if "`by'"!="" {
                tempvar BY
                qui frget `BY' = `by', from(`lnkvar')
            }
        }
    }
    // prepare new frame
    tempname newframe
    frame create `newframe'
    frame `newframe' {
        qui gen double _ID = .
        qui gen double _X  = .
        qui gen double _Y  = .
    }
    // obtain boxes/MECs
    frame `shpframe' {
        if "`BY'"!="" {
            tempvar TOUSE
            qui gen byte `TOUSE' = 0
            qui levelsof `BY' if `touse', local(bylvls)
            foreach lvl of local bylvls {
                qui replace `TOUSE' = `BY'==`lvl' if `touse'
                mata: _bbox("`newframe'", `"`XY'"', "`TOUSE'", `lvl',/*
                     */ `mec', `n', `padding')
            }
        }
        else {
            mata: _bbox("`newframe'", `"`XY'"', "`touse'", .,/*
                 */ `mec', `n', `padding')
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
        if _rc==0 {
            local exp `VARS'
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
    di as txt "(link to frame {cmd:`shpframe'} removed)"
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
    gettoken frame 0 : 0, parse(" ,")
    local cframe `"`c(frame)'"'
    if `"`frame'"'==`"`cframe'"' {
        di as err "{it:unitframe} must be different from current frame"
        exit 498
    }
    confirm frame `frame'
    frame `frame' {
        syntax [varlist(default=none)] [, EXclude(varlist)/*
            */ TARget(namelist) QUIETly ] // quietly undodumented
        local varlist: list varlist - exclude
        geoframe get shpframe, local(shpframe) // check whether already linked
        if `"`shpframe'"'!=`"`cframe'"' local shpframe
    }
    if `"`quietly'"'=="" local noisily noisily
    if `: list sizeof varlist'==0 {
        `quietly' di as txt "(no variables to copy)"
        exit
    }
    foreach var of local varlist {
        // use matched list so that variables staring with "_" are copied
        gettoken TGT target : target
        if "`TGT'"=="" local TGT `var'
        local vlist `vlist' `TGT' = `var'
    }
    if `"`shpframe'"'!="" {
        frame `frame': geoframe get linkname, local(lnkvar)
        if `"`lnkvar'"'=="" {
            di as txt "(frame {bf:`frame'}: linkname not found;"/*
                */ " trying to link on the fly)"
        }
        else {
            capt `noisily' frget `vlist', from(`lnkvar')
            if _rc==459 {
                di as txt "existing link between {bf:`frame'} and"/*
                    */" {bf:`cframe'} seems broken; trying to link on the fly"
                di as txt "apply {helpb geoframe##relink:geoframe relink}"/*
                    */" to fix the link permanently"
                local lnkvar
            }
            else exit _rc
        }
    }
    frame `frame' {
        geoframe get id, local(id)
        if `"`id'"'=="" {
            di as err "no ID variable defined in frame {bf:`frame'}"
            exit 498
        }
    }
    geoframe get id, local(id0)
    if `"`id0'"'=="" {
        di as err "no ID variable defined in frame {bf:`cframe'}"
        exit 498
    }
    tempvar lnkvar
    `quietly' frlink m:1 `id0', frame(`frame' `id') generate(`lnkvar')
    `quietly' frget `vlist', from(`lnkvar')
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

version 17
mata:
mata set matastrict on

void _add_path_to_shpf(string scalar shpf, string scalar path0)
{
    string scalar path, fn
    
    pathsplit(st_local(path0), path="", fn="")
    st_local(shpf, pathjoin(path, st_local(shpf)))
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

string scalar _strreverse(string scalar s)
{
    real scalar      n
    string rowvector S
    
    S = tokens(s)
    n = length(S)
    if (!n) return(s)
    return(invtokens(S[n..1]))
}

void _copy_by_ID(string scalar frame, string scalar ID0,
    string scalar ID, string scalar X)
{   // X may contain multiple variables
    // assumptions: current frame is ordered by ID and X is constant within ID
    real scalar    i
    real colvector id, p
    real matrix    x
    transmorphic   A
    string scalar  cframe
    
    // get data
    id = st_data(., ID)
    p  = selectindex(_mm_unique_tag(id, 1))
    id = id[p]
    st_view(x=., p, tokens(X))
    // post data into asarray
    A = asarray_create("real")
    asarray_notfound(A, J(1,cols(x),.))
    for (i=rows(id); i; i--) asarray(A, id[i], x[i,])
    // change frame and copy data from asarray
    cframe = st_framecurrent()
    st_framecurrent(frame)
    id = st_data(., ID0)
    st_view(x, ., st_addvar("double", tokens(X)))
    for (i=rows(id); i; i--) x[i,] = asarray(A, id[i])
    st_framecurrent(cframe)
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

void _plevel(string scalar pl, string scalar id, string scalar pid,
    string scalar xy, string scalar touse)
{
    real colvector ID, PID
    real matrix    XY
    
    st_view(ID=.,  ., id,  touse)
    st_view(PID=., ., pid, touse)
    st_view(XY=.,  ., xy,  touse)
    st_store(., pl, touse, geo_plevel(1, ID, PID, XY))
}

void _spjoin(string scalar frame, string scalar id1, string scalar xy1,
    string scalar touse1, string scalar id, string scalar pid, string scalar xy,
    string scalar pl, string scalar touse)
{
    real colvector ID, PID, PL
    real matrix    XY, XY1
    
    st_view(ID=.,  ., id, touse)
    st_view(PID=., ., pid, touse)
    st_view(XY=.,  ., xy, touse)
    if (pl!="") st_view(PL=., ., pl, touse)
    else        PL = J(0,1,.)
    st_framecurrent(frame)
    st_view(XY1=., ., xy1, touse1)
    st_store(., id1, touse1, geo_spjoin(XY1, ID, PID, XY, PL))
}

void _bbox(string scalar frame, string scalar xy, string scalar touse,
     real scalar id, real scalar mec, real scalar k, real scalar pad)
{
    string scalar  cframe
    real scalar    n, n0, n1
    real matrix    XY, PT
    
    // generate shape
    st_view(XY=.,  ., xy, touse)
    if (mec) PT = __bbox_mec(XY, k, pad)
    else     PT = __bbox(XY, pad)
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

real matrix __bbox_mec(real matrix XY, real scalar n, real scalar pad)
{
    real rowvector c
    
    c = geo_welzl(XY)
    if (pad) c[3] = c[3] * (1 + pad/100)
    return((.,.) \ c[(1,2)] :+ c[3] * _geo_symbol_circle(n))
}

real matrix __bbox(real matrix XY, real scalar pad)
{
    real scalar xmid, ymid
    real matrix xy
    
    xy = colminmax(XY)
    if (pad) {
        xmid = (xy[2,1] + xy[1,1]) / 2
        ymid = (xy[2,2] + xy[1,2]) / 2
        xy = (xy :- (xmid,ymid)) * (1 + pad/100) :+  (xmid,ymid)
    }
    return((.,.) \
        (xy[2,1],xy[1,2]) \ // xmax,ymin
        (xy[2,1],xy[2,2]) \ // xmax,ymax
        (xy[1,1],xy[2,2]) \ // xmin,ymax
        (xy[1,1],xy[1,2]) \ // xmin,ymin
        (xy[2,1],xy[1,2]))  // xmax,ymin
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

end


