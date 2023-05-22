*! version 0.1.4  22may2023  Ben Jann

program geoframe
    version 17
    gettoken subcmd 0 : 0, parse(" ,")
    _parse_subcmd `subcmd'
    _geoframe_`subcmd' `macval(0)'
    if `"`local'"'!="" { // pass through returns from _geoframe_get
        c_local `local' `"`value'"'
    }
end

program _parse_subcmd
    local l = strlen(`"`0'"')
    if      `"`0'"'==substr("create", 1, max(2,`l'))    local 0 create
    else if `"`0'"'==substr("describe", 1, max(1,`l'))  local 0 describe
    else if `"`0'"'=="set"                              local 0 set
    else if `"`0'"'=="get"                              local 0 get
    else if `"`0'"'==substr("link", 1, max(1,`l'))      local 0 link
    else if `"`0'"'==substr("unlink", 3, max(1,`l'))    local 0 unlink
    else if `"`0'"'==substr("generate", 1, max(1,`l'))  local 0 generate
    else if `"`0'"'==substr("attach", 1, max(2,`l'))    local 0 attach
    else if `"`0'"'==substr("detach", 1, max(3,`l'))    local 0 detach
    else if `"`0'"'==substr("append", 1, max(2,`l'))    local 0 append
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
    syntax [name(id="framename" name=frame)] [using/] [, replace/*
        */ type(str) id(name) sid(name) pid(name) PLevel(name)/*
        */ COordinates(namelist)/*
        */ FEATure(str asis)/*
        */ noSHPfile SHPfile2(str asis)/*
        */ noDEScribe _checkfrlink(str) ]
    __geoframe_create_parse_type, `type'
    local isSHPread = `"`_checkfrlink'"'!=""
    if `isSHPread' local describe nodescribe
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
            qui _frame dir
            local framelist = r(contents)
            if `:list frame in framelist' {
                tempname tmpframe
                frame rename `frame' `tmpframe'
            }
        }
        if `hasUSING' frame create `frame'
        capture noisily break {
            frame `frame' {
                if `hasUSING' use `"`macval(using)'"'
                if !`hasSHP' & "`type'"!="shape" & "`shpfile'"=="" {
                    // check whether shape file is declared in characteristics
                    __geoframe_create_has_shpf `"`macval(using)'"'
                }
                _geoframe_set id `id'
                __geoframe_create_type "`type'" `hasSHP' `"`coordinates'"'
                _geoframe_set type `type'
                _geoframe_set sid `sid'
                _geoframe_set pid `pid'
                _geoframe_set plevel `plevel'
                _geoframe_set coordinates `coordinates'
                _geoframe_set feature `feature'
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
                        */  _checkfrlink(`frame')
                }
                else if `isSHPread' {
                    frame `_checkfrlink': _geoframe_link `frame'
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
    if !`isSHPread' {
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
        */ COordinates(namelist) FEATure(str asis) ]
    c_local SHP_id     `"`id'"'
    c_local SHP_sid    `"`sid'"'
    c_local SHP_pid    `"`pid'"'
    c_local SHP_plevel `"`plevel'"'
    c_local SHP_coord  `"`coordinates'"'
    c_local SHP_feat   `"`feature'"'
end

program __geoframe_create_parse_shpf
    syntax [anything(equalok)] [using]
    if `:list sizeof using'==0 {
        if `:list sizeof anything'>1 {
            gettoken frame anything : anything
        }
        local 0 `macval(frame)' using `macval(anything)'
    }
    syntax [name(id="framename" name=frame)] using/
    c_local SHP_frame  `"`frame'"'
    c_local SHP_using  `"`macval(using)'"'
end

program __geoframe_create_has_shpf
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
    if `: list sizeof coord'==4 local type "pc"
    else if `hasSHP'            local type "unit"
    else {
        geoframe get id, local(id)
        if `"`id'"'!="" {
            capt isid `id', missok
            if _rc local type "shape" // id not unique
            else   local type "unit"  // id unique
        }
        else local type "unit" // no id
    }
    c_local type `type'
end

program _geoframe_describe
    syntax [name(id="frame" name=frame)] 
    if "`frame'"=="" local frame `"`c(frame)'"'
    frame `frame' {
        geoframe get type, local(type)
        if `"`type'"'=="shape"     local chars feat id sid pid plevel
        else if `"`type'"'=="unit" local chars feat id shpf
        else if `"`type'"'=="pc"   local chars feat id shpf
        else                       local chars feat id sid pid plevel shpf
        foreach char of local chars {
            geoframe get `char', local(`char')
        }
        geoframe get coord, local(coord) `type'
    }
    local chars type `chars' coord
    foreach char in type feat id coord sid pid plevel shpf {
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
    di
    __geoframe_describe_di "Frame name"             `"{bf:`frame'}"'
    __geoframe_describe_di "Frame type"             `"`type'"'
    __geoframe_describe_di "Feature type"           `"`feat'"'
    __geoframe_describe_di "Unit ID"                `"`id'"'
    __geoframe_describe_di "Coordinates"            `"`coord'"'
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
    capt mata: assert(st_islmname(st_local("fcn")))
    if _rc==1 exit _rc
    if _rc {
        di as err `"`fcn' invalid procedure"'
        exit 198
    }
    _geoframe_generate_`fcn' `0'
end

program _geoframe_generate_pid
    syntax [name] [, replace ]
    if "`namelist'"=="" local namelist _PID
    if "`replace'"=="" {
        confirm new variable `namelist'
    }
    geoframe get id, l(ID)
    geoframe get coordinates, l(X)
    gettoken X : X
    tempvar tmp
    if (`X'[1]>=.) {
        // first coordinate missing: assuming polygons start with missing
        qui gen byte `tmp' = `X'>=. | `ID'!=`ID'[_n-1]
    }
    else {
        // first coordinate non-missing: assuming polygons end with missing
        qui gen byte `tmp' = `X'[_n-1]>=. | `ID'!=`ID'[_n-1]
    }
    qui replace `tmp' = `tmp' + `tmp'[_n-1] if `ID'==`ID'[_n-1]
    capt confirm new variable `namelist'
    if _rc==1 exit _rc
    if _rc drop `namelist'
    rename `tmp' `namelist'
    _geoframe_set pid `namelist'
    di as txt "(variable {bf:`namelist'} generated)"
end

program _geoframe_set
    gettoken char 0 : 0, parse(" ,")
    __geoframe_set_char `char' // => char
    gettoken char arg : char
    if      "`char'"=="type"        __geoframe_set_type `0'
    else if "`char'"=="var"         __geoframe_set_var `arg' `0'
    else if "`char'"=="coordinates" __geoframe_set_coordinates `0'
    else if "`char'"=="shpframe"    __geoframe_set_shpframe `0'
    else if "`char'"=="type"        __geoframe_set_type `0'
    else                            char _dta[GEOFRAME_`char'] `0'
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
    else if `"`0'"'==substr("plevel", 1, max(2,`l'))      local 0 var plevel
    else if `"`0'"'==substr("coordinates", 1, max(2,`l')) local 0 coordinates
    else if `"`0'"'==substr("feature", 1, max(4,`l'))     local 0 feature
    else if `"`0'"'==substr("shpframe", 1, max(3,`l'))    local 0 shpframe
    else {
        di as err `"'`0'' not allowed"'
        exit 198
    }
    c_local char `0'
end

program __geoframe_set_shpframe
    if !`:list sizeof 0' {
        char _dta[GEOFRAME_shpframe]
        exit
    }
    confirm name `0'
    local 0 `0'
    local cframe `"`c(frame)'"'
    if `"`cframe'"'==`"`0'"' {
        di as err "{it:shpframe} may not be equal name of current frame"
        exit 498
    }
    qui _frame dir
    local framelist = r(contents)
    if !`:list 0 in framelist' {
        di as err "frame {bf:`0'} not found"
        exit 111
    }
    char _dta[GEOFRAME_shpframe] `0'
end

program __geoframe_set_var
    gettoken var arg : 0
    local arg = strtrim(`"`arg'"')
    if `"`arg'"'=="" {
        if "`var'"=="oid" local VAR shape_order
        else              local VAR _`=strupper("`var'")'
        capt confirm numeric variable `VAR', exact
        if _rc==1 exit _rc
        if _rc==0 {
            local arg `VAR'
        }
    }
    else {
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
    if `"`type'"'=="" {
        local type: char _dta[GEOFRAME_type]
    }
    if `"`type'"'=="unit"    local VARS _CX _CY
    else if `"`type'"'=="pc" local VARS _X1 _Y1 _X2 _Y2
    else                     local VARS _X _Y
    local n: list sizeof VARS
    if `"`arg'"'!="" {
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
    }
    else {
        capt confirm numeric variable `VARS', exact
        if _rc==1 exit _rc
        if _rc==0 {
            local arg `VARS'
        }
    }
    char _dta[GEOFRAME_coordinates] `arg'
end

program _geoframe_get
    _parse comma char 0 : 0
    __geoframe_set_char `char' // => char
    gettoken char arg : char
    syntax [, Local(str) * ]
    if      "`char'"=="var"         __geoframe_get_var `arg', `options'
    else if "`char'"=="coordinates" __geoframe_get_coordinates, `options'
    else                            local exp: char _dta[GEOFRAME_`char']
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
        if "`var'"=="sid" local VAR shape_order
        else              local VAR _`=strupper("`var'")'
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
    if "`flip'"!="" {
        mata: st_local("exp",  _strreverse(st_local("exp")))
    }
    c_local exp `exp'
end

program _geoframe_link
    /*  syntax 1: test and register linking
            geoframe link <shpframe>
        syntax 2; generate linking variable for registered linking
            geoframe link, linkvar()                                */
    local frame `"`c(frame)'"'
    syntax [name(id="shpframe" name=shpframe)] [, linkvar(name) ]
    if "`shpframe'"!="" {
        if "`linkvar'"!="" {
            di as err "{bf:linkvar()} not allowed"
            exit 198
        }
        if `"`shpframe'"'==`"`frame'"' {
            di as err "{it:shpframe} must be different from current frame"
            exit 498
        }
    }
    else {
        if "`linkvar'"=="" {
            di as err "{it:shpframe} required"
            exit 198
        }
        geoframe get shpframe, local(shpframe)
        if `"`shpframe'"'=="" {
            di as err "no link to shape file found"
            exit 111
        }
    }
    frame `shpframe' {
        geoframe get id, local(shpid)
        if `"`shpid'"'=="" {
            di as err "no ID variable defined in {it:shpframe}"
            exit 498
        }
        if "`linkvar'"=="" tempvar lnkvar // create temporary link (check only)
        else               local lnkvar `linkvar'
    }
    geoframe get id, local(id)
    if `"`id'"'=="" {
        di as err "no ID variable defined in current frame"
        exit 498
    }
    frame `shpframe': frlink m:1 `shpid', frame(`frame' `id') generate(`lnkvar')
    if "`linkvar'"=="" {
        __geoframe_set_shpframe `shpframe'
    }
end

program _geoframe_unlink
    geoframe get shpframe, local(shpframe)
    if `"`shpframe'"'=="" {
        di as txt "(no link to shape frame found)"
        exit
    }
    __geoframe_set_shpframe
    di as txt "(link to shape frame removed)"
end

program _geoframe_attach
    if c(stata_version)<18 {
        di as err "{bf:geoframe attach} requires Stata 18"
        exit 9
    }
    gettoken frame 0 : 0, parse(" ,")
    confirm name `frame'
    if `"`frame'"'==`"`c(frame)'"' {
        di as err "{it:unitframe} must be different from current frame"
        exit 498
    }
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
    frlink m:1 `id0', frame(`frame' `id')
    fralias add `keep', from(`frame')
end

program _geoframe_detach
    if c(stata_version)<18 {
        di as err "{bf:geoframe detach} requires Stata 18"
        exit 9
    }
    syntax name(name=frame)
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
    confirm name `frame'
    if `"`frame'"'==`"`c(frame)'"' {
        di as err "{it:unitframe} must be different from current frame"
        exit 498
    }
    frame `frame' {
        syntax [varlist(default=none)] [, EXclude(varlist)/*
            */ TARget(namelist) ]
        geoframe get id, local(id)
        if `"`id'"'=="" {
            di as err "no ID variable defined in {it:unitframe}"
            exit 498
        }
        local varlist: list varlist - exclude
    }
    geoframe get id, local(id0)
    if `"`id0'"'=="" {
        di as err "no ID variable defined in current frame"
        exit 498
    }
    if `: list sizeof varlist'==0 {
        di as txt "(no variables to copy)"
        exit
    }
    if `"`target'"'!="" {
        foreach var of local varlist {
            gettoken TGT target : target
            if "`TGT'"=="" local TGT `var'
            local vlist `vlist' `TGT' = `var'
        }
        local varlist `vlist'
    }
    tempvar lnkvar
    frlink m:1 `id0', frame(`frame' `id') generate(`lnkvar')
    frget `varlist', from(`lnkvar')
end

program _geoframe_append
    gettoken frame 0 : 0, parse(" ,")
    confirm name `frame'
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
    if !`: list sizeof varlist' exit // nothing to do
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
        _geoframe_varinit `type' `V'
    }
    while (`"`recast'"'!="") {
        gettoken type recast : recast
        gettoken V    recast : recast
        recast `type' `V'
    }
    local N = _N
    local cframe = c(frame)
    capt n mata: _geoframe_append("`frame'", "`touse'",/*
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

program _geoframe_varinit
    gettoken type 0 : 0
    local isstr = substr("`type'",1,3)=="str"
    foreach v of local 0 {
        capt confirm variable `v', exact
        if _rc {
            if `isstr' local exp `""""'
            else       local exp .
            qui gen `type' `v' = `exp'
        }
    }
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

string scalar _strreverse(string scalar s)
{
    real scalar      n
    string rowvector S
    
    S = tokens(s)
    n = length(S)
    if (!n) return(s)
    return(invtokens(S[n..1]))
}

void _geoframe_append(string scalar frame, string scalar touse,
    string rowvector vars, string rowvector VARS)
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


