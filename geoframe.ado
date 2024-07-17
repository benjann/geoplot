*! version 1.2.7  17jul2024  Ben Jann

program geoframe, rclass
    version 16.1
    gettoken subcmd 0 : 0, parse(" ,")
    _parse_subcmd `subcmd'
    geoframe_`subcmd' `macval(0)'
    if `"`local'"'!="" { // pass through returns (geoframe_get, geoframe_flip)
        c_local `local' `"`value'"'
    }
    if inlist("`subcmd'","query","copy") return add
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
    else if `"`0'"'=="convert"                         local 0 translate
    else if `"`0'"'=="load"                            local 0 create
    capt mata: assert(st_islmname(st_local("0")))
    if _rc==1 exit _rc
    if _rc {
        di as err `"`0' invalid subcommand"'
        exit 198
    }
    c_local subcmd `"`0'"'
end

program geoframe_create
    _syntax_add_using `macval(0)'
    // if options only; declare current frame as geoframe
    _parse comma lhs rhs : 0
    if `"`macval(lhs)'"'=="" {
        _create_cur `macval(0)'
        exit
    }
    // explicit dtype
    syntax [anything(equalok)] using/ [, dta Dtype(str) * ]
    _create_parse_dtype, `dtype'
    if "`dta'"!="" {
        if !inlist("`dtype'","dta","") {
            di as err "dta and dtype() not both allowed"
            exit 198
        }
        _create `macval(lhs)', `macval(options)'
        exit
    }
    if "`dtype'"!="" {
        geoframe_import `dtype' `macval(lhs)', `macval(options)'
        exit
    }
    // look for .dta suffix in `using'
    mata: st_local("suffix", pathsuffix(st_local("using")))
    if `"`suffix'"'==".dta" {
        _create `macval(lhs)', `macval(options)'
        exit
    }
    // check whether file `using'.dta exists
    capt _create_confirmfile `macval(using)'.dta
    if _rc==1 exit 1
    if !_rc {
        _create `macval(lhs)', `macval(options)'
        exit
    }
    // else run geoframe import
    geoframe_import `macval(lhs)', `macval(options)'
end

program _create_parse_dtype
    syntax [, dta esri shp json GEOJson ]
    local dtype `dta' `estri' `shp' `json' `geojson'
    if `:list sizeof dtype'>1 {
        di as err "dtype(): only one keyword allowed"
        exit 198
    }
    c_local dtype `dtype'
end

program _create_confirmfile
    mata: st_local("rc", fileexists(st_local("0")) ? "0" : "601")
    if `rc' {
        di as err `"file `0' not found or not readable"'
        exit `rc'
    }
end

program _create
    // syntax
    syntax [name(name=frame)] [using/] [, replace Type(str)/*
        */ noDEScribe CURrent noShp Shp2(str asis) * ]
    // parse type()
    _create_parse_type, `type'
    // file and frame
    mata: st_local("using", st_local("using") +/*
        */ (pathsuffix(st_local("using"))!=".dta" ? ".dta" : ""))
    _create_confirmfile `macval(using)'
    if "`frame'"=="" {
        mata: st_local("frame",/*
            */ ustrtoname(pathrmsuffix(pathbasename(st_local("using")))))
    }
    // shape file
    if `"`shp2'"'!="" {
        if "`shp'"!="" {
            di as err "{bf:noshp} and {bf:shp()} not both allowed"
            exit 198
        }
        if inlist("`type'","shape","pc") {
            di as err "{bf:shp()} not allowed with {bf:type(`type')}"
            exit 198
        }
        local type "attribute"
    }
    if "`shp'"=="" & !inlist("`type'","shape","pc") {
        capt n _create_parse_shp "`frame'" `"`macval(using)'"'/*
            */ `"`macval(shp2)'"' // => shpframe, shp2
        if _rc==1 exit 1
        if _rc {
            di as err "(error in shp())"
            exit _rc
        }
    }
    // replace
    if "`replace'"=="" {
        confirm new frame `frame'
        if "`shpframe'"!="" confirm new frame `shpframe'
    }
    else {
        capt confirm new frame `frame'
        if _rc==110 tempname tmpframe
        else if _rc exit _rc
        if "`shpframe'"!="" {
            capt confirm new frame `shpframe'
            if _rc==110 tempname tmpshpframe
            else if _rc exit _rc
        }
    }
    // process
    nobreak {
        if "`tmpframe'"!="" frame rename `frame' `tmpframe'
        if "`shpframe'"!="" {
            if "`tmpshpframe'"!="" frame rename `shpframe' `tmpshpframe'
        }
        capture noisily break {
            _di_frame "(creating frame " `frame' `" from `macval(using)')"'
            frame create `frame'
            frame `frame' {
                // read file
                use `"`macval(using)'"'
                // drop old linkage information that may exist in the file
                _set_shpframe
                _set_linkname
                capt drop _GEOFRAME_lnkvar_*
                // apply options
                __create, type(`type') `shp2' `macval(options)'
            }
        }
        if _rc {
            local rc = _rc
            capt frame drop `frame'
            if "`tmpframe'"!="" frame rename `tmpframe' `frame'
            if "`shpframe'"!="" {
                capt frame drop `shpframe'
                if "`tmpshpframe'"!="" frame rename `tmpshpframe' `shpframe'
            }
            exit `rc'
        }
        // shpframe might not have been created
        if "`shpframe'"!="" {
            if "`tmpshpframe'"!="" {
                capt confirm frame `shpframe'
                if _rc frame rename `tmpshpframe' `shpframe'
            }
        }
    }
    _describe_and_make_current `frame' "`current'" "`describe'"
end

program _create_parse_type
    syntax [, Attribute pc Shape ]
    local type `attribute' `pc' `shape'
    if `: list sizeof type'>1 {
        di as err "{bf:type()}: only one of {bf:attribute}, {bf:shape},"/*
            */ " and {bf:pc} allowed"
        exit 198
    }
    c_local type `type'
end

program _create_parse_shp
    args frame0 using0 0
    _syntax_add_using `macval(0)'
    _parse comma 0 opts : 0
    syntax [name(id="shpname" name=frame)] [using/]
    if `"`macval(using)'"'!="" {
        mata: _create_parse_shp_using(st_local("using"), st_local("using0"))
        _create_confirmfile `macval(using)'
    }
    else {
        mata: st_local("using", pathrmsuffix(st_local("using0")) + "_shp.dta")
        capt _create_confirmfile `macval(using)'
        if _rc==1 exit 1
        if _rc {
            c_local shpframe
            c_local shp2
            exit
        }
    }
    if `"`frame'"'=="" local frame `frame0'_shp
    c_local shpframe `frame'
    c_local shp2 shp2(`frame' using `"`macval(using)'"'`opts')
end

program _create_cur
    syntax [, Type(str) noDEScribe noShp Shp2(str asis)/*
        */ CURrent * ] // ignoring current
    // parse type()
    _create_parse_type, `type'
    // shape file
    if `"`shp2'"'!="" {
        if "`shp'"!="" {
            di as err "{bf:noshp} and {bf:shp()} not both allowed"
            exit 198
        }
        if inlist("`type'","shape","pc") {
            di as err "{bf:shp()} not allowed with {bf:type(`type')}"
            exit 198
        }
        local type "attribute"
    }
    if "`shp'"=="" & !inlist("`type'","shape","pc") {
        capt n _create_cur_parse_shp `macval(shp2)'
        if _rc==1 exit 1
        if _rc {
            di as err "(error in shp())"
            exit _rc
        }
    }
    // process
    __create _cur, type(`type') `shp2' `macval(options)'
    if "`describe'"=="" geoframe_describe
end

program _create_cur_parse_shp
    _parse comma frame 0 : 0
    if `"`frame'"'=="" {
        _get shpframe, local(frame) // may already have link
        if `"`frame'"'=="" local frame `c(frame)'_shp
        capt confirm frame `frame'
        if _rc==1 exit 1
        if _rc local frame
    }
    else {
        confirm frame `frame'
        if "`frame'"==c(frame) {
            di as err "{it:shpframe} must be different from attribute frame"
            exit 498
        }
    }
    if "`frame'"!="" c_local shp2 shp2(`frame'`macval(0)')
    else             c_local shp2
end

program __create
    _parse comma _cur 0 : 0
    // syntax
    syntax [, Type(str) id(varname) COordinates(varlist)/*
        */ CENtroids(varlist min=2 max=2) area(varname) sid(varname)/*
        */ pid(varname) PLevel(varname) Feature(str asis)/*
        */ SHP2(str asis) nodrop noCLean ]
    if "`drop'`clean'"!="" local clean
    else                   local clean clean
    if `"`macval(shp2)'"'!="" local shp shp
    // determine type
    if "`type'"=="" _get type, local(type)
    if !inlist("`type'","attribute","shape","pc") local type
    _create_parse_coordinates "`type'" "`coordinates'" // may update type
    if "`type'"=="" & "`shp'"!="" local type "attribute" // shp available
    if "`type'"=="" _create_determine_type `id'
    if "`type'"!="attribute" local shp
    // process shape frame
    if "`shp'"!="" {
        capt n _create_shp`_cur' `"`feature'"' `macval(shp2)'
        if _rc==1 exit 1
        if _rc {
            di as err "(error in shp())"
            exit _rc
        }
    }
    // process current frame
    if `"`coordinates'"'!="" geoframe_set coordinates // clear setting
    geoframe_set type `type'
    if `"`feature'"'!=""     geoframe_set feature `feature'
    if `"`id'"'!=""          geoframe_set id `id'
    if `"`coordinates'"'!="" geoframe_set coordinates `coordinates'
    if `"`centroids'"'!=""   geoframe_set centroids `centroids'
    if `"`area'"'!=""        geoframe_set area `area'
    if `"`sid'"'!=""         geoframe_set sid `sid'
    if `"`pid'"'!=""         geoframe_set pid `pid'
    if `"`plevel'"'!=""      geoframe_set plevel `plevel'
    qui geoframe_unlink
    if "`shp'"!=""           geoframe_link `shpframe', quietly `clean'
end

program _create_parse_coordinates
    args type coordinates
    local n: list sizeof coordinates
    if !`n' exit
    if `"`type'"'=="" {
        if `n'==4 {
            c_local type "pc" // set type to pc if 4 coordinates
            exit
        }
        local k 2
    }
    else if "`type'"=="pc" local k 4
    else local k 2
    if `n'!=`k' {
        di as err "wrong number of variables specified in coordinates()" _c
        if `k'==4 di as err "; must specify 4 variables if type is {bf:pc}"
        else      di as err "; must specify 2 variables unless type is {bf:pc}"
        exit 198
    }
end

program _create_determine_type
    args id
    if `"`id'"'=="" _get id, local(id)
    if `"`id'"'!="" {
        capt isid `id', missok
        if _rc local type "shape"       // id not unique => shape
        else   local type "attribute"   // id unique => attribute
    }
    else local type "attribute"         // no id => attribute
    c_local type `type'
end

program _create_shp
    gettoken feature 0 : 0
    _parse comma 0 opts : 0
    syntax name(name=frame) using/
    _di_frame "(creating frame " `frame' `" from `macval(using)')"'
    frame create `frame'
    frame `frame' {
        // read file
        use `"`macval(using)'"'
        // drop old linkage information that may exist in the file
        _set_shpframe
        _set_linkname
        capt drop _GEOFRAME_lnkvar_*
        // apply options
        __create_shp `"`feature'"' `macval(opts)'
     }
     c_local shpframe `frame'
end

program _create_shp_cur
    gettoken feature 0 : 0
    _parse comma frame 0 : 0
    frame `frame': __create_shp `"`feature'"' `macval(0)'
    c_local shpframe `frame'
end

program __create_shp
    gettoken feature 0 : 0
    // syntax
    syntax [, Type(str) id(varname) COordinates(varlist)/*
        */ CENtroids(varlist min=2 max=2) area(varname) sid(varname)/*
        */ pid(varname) PLevel(varname) ]
    // determine type
    _create_parse_type, `type'
    if "`type'"=="attribute" {
        di as err "{bf:type(attribute)} not allowed with shape frame"
        exit 198
    }
    if "`type'"=="" {
        _get type, local(type)
        if `"`type'"'=="attribute" local type
    }
    _create_parse_coordinates "`type'" "`coordinates'" // may update type
    if "`type'"=="" local type "shape" // enforce shape if type empty
    // apply settings
    if `"`coordinates'"'!="" geoframe_set coordinates // clear setting
    geoframe_set type `type'
    if `"`feature'"'!=""     geoframe_set feature `feature'
    if `"`id'"'!=""          geoframe_set id `id'
    if `"`coordinates'"'!="" geoframe_set coordinates `coordinates'
    if `"`centroids'"'!=""   geoframe_set centroids `centroids'
    if `"`area'"'!=""        geoframe_set area `area'
    if `"`sid'"'!=""         geoframe_set sid `sid'
    if `"`pid'"'!=""         geoframe_set pid `pid'
    if `"`plevel'"'!=""      geoframe_set plevel `plevel'
end

program geoframe_save
    // syntax
    syntax [anything] [if] [in] [, replace IFshp(passthru) NOShp ]
    if "`noshp'"!="" {
        if `"`ifshp'"'!="" {
            di as err "ifshp() and noshp not both allowed"
            exit 198
        }
        local noshp unlink
    }
    if "`noshp'"=="" _get shpframe, local(shpframe)
    else             local shpframe
    // determine destination
    mata: _save_set_fname(st_local("anything")) // => fname
    if "`replace'"=="" {
        confirm new file `fname'.dta
        if `"`shpframe'"'!="" {
            confirm new file `fname'_shp.dta
        }
    }
    // copy data to tempframes
    tempname tframe 
    if `"`shpframe'"'!="" tempname tshpframe
    qui geoframe_select `if' `in', into(`tframe' `tshpframe') `ifshp' `noshp'
    // remove all linkage information and save
    frame `tframe' {
        _set_shpframe
        _set_linkname
        capt drop _GEOFRAME_lnkvar_*
        save `fname'.dta, `replace'
    }
    if `"`shpframe'"'!="" {
        frame `tshpframe' {
            _set_shpframe
            _set_linkname
            capt drop _GEOFRAME_lnkvar_*
            save `fname'_shp.dta, `replace'
        }
    }
end

program geoframe_use
    syntax anything(id="filename" everything) [, replace NOShp noDEScribe/*
        */ CURrent ]
    _create using `anything', noclean `replace' `noshp' `describe' `current'
end

program geoframe_describe
    syntax [name(id="frame name" name=frame)] 
    if "`frame'"=="" local frame = c(frame)
    // collect chars
    frame `frame' {
        local allchars type feat id coord centr area sid pid plevel shpf
        foreach char of local allchars {
            _get `char', local(`char')
        }
    }
    // chars to print even if empty
    local chars type feat id coord
    if `"`type'"'=="attribute" {
        local chars `chars' shpf
        local centr
    }
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
    _describe_di "Frame name"/*
        */ `"{bf:`frame'} [{stata frame change `frame':make current}]"'
    _describe_di "Frame type"             `"`type'"'
    _describe_di "Feature type"           `"`feat'"'
    _describe_di "Number of obs"          `"{bf:`nobs'}"'
    _describe_di "Unit ID"                `"`id'"'
    _describe_di "Coordinates"            `"`coord'"'
    _describe_di "Centroids"              `"`centr'"'
    _describe_di "Area"                   `"`area'"'
    _describe_di "Within-unit sort ID"    `"`sid'"'
    _describe_di "Within-unit item ID"    `"`pid'"'
    _describe_di "Plot level ID"          `"`plevel'"'
    _describe_di "Linked shape frame"     `"`shpf'"'
end

program _describe_di
    args lbl txt
    if `"`txt'"'=="" exit
    di as txt %22s `"`lbl'"' ": " `"`txt'"'
end

program geoframe_set
    gettoken char 0 : 0, parse(" ,")
    _set_parse_char `char' // => char
    gettoken char arg : char
    if      "`char'"=="type"        _set_type `0'
    else if "`char'"=="var"         _set_var `arg' `0'
    else if "`char'"=="coordinates" _set_coordinates `0'
    else if "`char'"=="centroids"   _set_centroids `0'
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

program _set_parse_char
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

program _set_type
    if !inlist(`"`0'"',"","attribute","pc","shape") {
        di as err "type must be {bf:attribute}, {bf:pc}, or {bf:shape}"
        exit 198
    }
    if "`0'"!="" {
        local XY: char _dta[GEOFRAME_coordinates]
        local n: list sizeof XY
        if `n' {
            if (inlist("`0'","attribute","shape") & `n'!=2)/*
                */ | ("`0'"=="pc" & `n'!=4) {
                di as txt "(current coordinates setting incompatible"/*
                    */ " with type {bf:`0'}; reset to default)"
                char _dta[GEOFRAME_coordinates]
            }
        }
    }
    char _dta[GEOFRAME_type] `0'
end

program _set_shpframe
    syntax [name(name=shpframe id="shape frame")]
    if "`shpframe'"=="" {
        char _dta[GEOFRAME_shpframe]
        exit
    }
    local cframe = c(frame)
    if `"`cframe'"'=="`shpframe'" {
        di as err "{it:shpframe} may not be equal name of current frame"
        exit 498
    }
    confirm frame `shpframe'
    char _dta[GEOFRAME_shpframe] `shpframe'
end

program _set_linkname
    syntax [name(name=lnkname id="linkname")]
    if "`lnkname'"=="" {
        char _dta[GEOFRAME_linkname]
        exit
    }
    char _dta[GEOFRAME_linkname] `lnkname'
end

program _set_var
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

program _set_coordinates
    _parse comma arg 0 : 0
    syntax [, Attribute pc Shape ]
    local type `attribute' `pc' `shape'
    if `: list sizeof type'>1 {
        di as err "only one of {bf:attribute}, {bf:shape}, and {bf:pc} allowed"
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
        if `"`type'"'=="attribute" local ARG _CX _CY
        else if `"`type'"'=="pc"   local ARG _X1 _Y1 _X2 _Y2
        else                       local ARG _X _Y
        if "`arg'"=="`ARG'" local arg // is default names; leave char empty
    }
    char _dta[GEOFRAME_coordinates] `arg'
end

program _set_centroids
    local type: char _dta[GEOFRAME_type]
    if `"`type'"'=="attribute" {
        _set_coordinates `0'
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

program geoframe_get
    _parse comma char 0 : 0
    _set_parse_char `char' // => char
    gettoken char arg : char
    syntax [, Local(str) strict * ]
    if      "`char'"=="var"         _get_var `arg', `options'
    else if "`char'"=="coordinates" _get_coordinates, `options'
    else if "`char'"=="centroids"   _get_centroids, `options'
    else if "`char'"=="shpframe"    _get_shpframe, `options'
    else if "`char'"=="linkname"    _get_linkname, `options'
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

program _get
    geoframe_get `0'
    c_local `local' `"`value'"'
end

program _get_var
    syntax name [, _OptionForBetterErrMsg ]
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

program _get_coordinates
    syntax [, flip Attribute pc Shape ]
    local type `attribute' `pc' `shape'
    if `: list sizeof type'>1 {
        di as err "only one of {bf:attribute}, {bf:shape}, and {bf:pc} allowed"
        exit 198
    }
    local exp: char _dta[GEOFRAME_coordinates]
    if `"`exp'"'=="" {
        // try default names
        if "`type'"=="" {
            local type: char _dta[GEOFRAME_type]
        }
        if `"`type'"'=="attribute" local VARS _CX _CY
        else if `"`type'"'=="pc"   local VARS _X1 _Y1 _X2 _Y2
        else                       local VARS _X _Y
        capt confirm numeric variable `VARS', exact
        if _rc==1 exit _rc
        if _rc==0 local exp `VARS'
        else if inlist(`"`type'"',"","attribute") { // check alternative names
            if `"`type'"'=="attribute" local VARS _X _Y
            else                       local VARS _CX _CY
            capt confirm numeric variable `VARS', exact
            if _rc==1 exit _rc
            if _rc==0 local exp `VARS'
        }
    }
    if "`flip'"!="" _flip `exp', local(exp)
    c_local exp `exp'
end

program _get_centroids
    local type: char _dta[GEOFRAME_type]
    if `"`type'"'=="attribute" {
        _get_coordinates `0'
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
    if "`flip'"!="" _flip `exp', local(exp)
    c_local exp `exp'
end

program _get_shpframe
    syntax [, _OptionForBetterErrMsg ]
    local type: char _dta[GEOFRAME_type]
    if inlist(`"`type'"',"shape","pc") c_local exp
    else c_local exp: char _dta[GEOFRAME_shpframe]
end

program _get_linkname
    syntax [, _OptionForBetterErrMsg ]
    local type: char _dta[GEOFRAME_type]
    if inlist(`"`type'"',"shape","pc") c_local exp
    else c_local exp: char _dta[GEOFRAME_linkname]
end

program geoframe_flip
    _parse comma lhs 0 : 0
    syntax [, Local(str) ]
    mata: _flip("lhs")
    c_local local `local'
    c_local value `"`lhs'"'
end

program _flip
    geoframe_flip `0'
    c_local `local' `"`value'"'
end

program geoframe_query
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
    else if `"`fnc'"''==substr("items", 1, max(4,`l'))       local fnc gtype
    else if `"`fnc'"''==substr("gtype", 1, max(2,`l'))       local fnc gtype
    else if `"`fnc'"''==substr("orientation", 1, max(2,`l')) local fnc dir
    else if `"`fnc'"''==substr("direction", 1, max(3,`l'))   local fnc dir
    else if `"`fnc'"''==substr("bbox", 1, max(2,`l'))        local fnc bbox
    else {
        capt mata: assert(st_islmname(st_local("fnc")))
        if _rc==1 exit 1
        if _rc {
            di as err `"`fnc': invalid function"'
            exit 198
        }
    }
    _query_`fnc' `0'
end

program _query_n
    __query_items n `0'
end

program _query_gtype
    __query_items gtype `0'
end

program _query_dir
    __query_items dir `0'
end

program __query_items, rclass
    gettoken fcn 0 : 0
    syntax [if] [in]
    marksample touse
    _get shpframe, local(shpframe)
    local hasSHP = `"`shpframe'"'!=""
    if `hasSHP' _markshapes `shpframe' `touse' `touse'
    else {
        _get type, local(type)
        if !inlist(`"`type'"',"shape","pc") {
            di as txt "(shape frame not found;"/*
                */ " treating current frame as shape frame)"
        }
        local shpframe = c(frame)
    }
    frame `shpframe' {
        _get id, local(ID)
        if `"`ID'"'=="" {
            tempvar ID
            qui gen byte `ID' = 1
        }
        _get coord, local(XY) strict
        tempname R
        mata: _query_`fcn'("`R'", `"`ID'"', "`XY'", "`touse'")
    }
    if `hasSHP' & "`fcn'"=="n" {
        tempname shptouse
        qui geoframe_copy `shpframe' `touse', target(`shptouse')
        qui count if `touse' & `shptouse'==1
        if r(N)>`R'[1,1] di as txt "(ignoring duplicate units)"
        qui count if `touse' & `shptouse'>=.
        if r(N) di as txt "(ignoring unmatched units)"
    }
    di as txt ""
    if "`fcn'"=="n" {
        di as txt "  Number of units              = " as res %9.0g `R'[1,1]
        di as txt "  Number of shape items        = " as res %9.0g `R'[2,1]
        di as txt "  Min number of items per unit = " as res %9.0g `R'[3,1]
        di as txt "  Avg number of items per unit = " as res %9.0g `R'[4,1]
        di as txt "  Max number of items per unit = " as res %9.0g `R'[5,1]
        return scalar max   = `R'[5,1]
        return scalar avg   = `R'[4,1]
        return scalar min   = `R'[3,1]
        return scalar items = `R'[2,1]
        return scalar units = `R'[3,1]
    }
    else if "`fcn'"=="gtype" {
        di as txt "  Polygon         = " as res %9.0g `R'[4,1]
        di as txt "  LineString      = " as res %9.0g `R'[3,1]
        di as txt "  Point           = " as res %9.0g `R'[2,1]
        di as txt "  Empty           = " as res %9.0g `R'[1,1]
        di as txt "  Number of items = " as res %9.0g/*
            */ `R'[1,1] + `R'[2,1] + `R'[3,1] + `R'[4,1]
        return scalar Empty      = `R'[1,1]
        return scalar Point      = `R'[2,1]
        return scalar LineString = `R'[3,1]
        return scalar Polygon    = `R'[4,1]
        return scalar items      = `R'[1,1] + `R'[2,1] + `R'[3,1] + `R'[4,1]
    }
    else if "`fcn'"=="dir" {
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

program _query_bbox, rclass
    syntax [if] [in] [, noShp PADding(passthru) ROTate hull CIRcle/*
        */ n(passthru) ANGle(passthru) noADJust ]
    // obtain bounding box
    tempname bbox bbox_shp
    qui geoframe_bbox `bbox' `bbox_shp' `if' `in',/*
        */ `shp' `rotate' `circle' `hull' `padding' `n' `angle' `noadjust' 
    tempname BBOX LIMITS R
    frame `bbox_shp' {
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

program geoframe_generate
    gettoken fnc 0 : 0, parse(" ,")
    local fnc = strlower(`"`fnc'"')
    local l = strlen(`"`fnc'"')
    if      `"`fnc'"'==substr("centroids", 1, max(3,`l'))   local fnc centroids
    else if `"`fnc'"'==substr("gtype", 1, max(2,`l'))       local fnc gtype
    else if `"`fnc'"'==substr("direction", 1, max(3,`l'))   local fnc dir
    else if `"`fnc'"'==substr("orientation", 1, max(2,`l')) local fnc dir
    else if `"`fnc'"'==substr("plevel", 1, max(2,`l'))      local fnc plevel
    else if `"`fnc'"'==substr("shpmatch", 1, max(3,`l'))    local fnc shpmatch
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
    _generate_`fnc' `0'
end

program _generate_centroids
    __generate_A centroids `0'
end

program _generate_area
    __generate_A area `0'
end

program __generate_A
    // syntax
    gettoken subcmd 0 : 0
    if "`subcmd'"=="centroids" {
        syntax [namelist(min=2 max=2)] [, replace noset ]
        if "`namelist'"=="" local namelist _CX _CY
        tempvar CX CY
        local TMP `CX' `CY'
    }
    else if "`subcmd'"=="area" {
        syntax [name] [, replace noset Scale(str) ]
        if "`namelist'"=="" local namelist _AREA
        tempvar TMP
    }
    else exit 198
    if "`replace'"=="" {
        confirm new variable `namelist'
    }
    // frames
    local cframe = c(frame)
    _get type, local(type)
    if !inlist(`"`type'"',"shape","pc") {
        _get shpframe, local(shpframe)
        if `"`shpframe'"'=="" {
            di as txt "(shape frame not found;"/*
                */ " treating current frame as shape frame)"
        }
    }
    if `"`shpframe'"'=="" {
        _get id, local(ID0)
        if `"`ID0'"'=="" {
            di as txt "(ID not available;"/*
                */ " assuming all items belong to same unit)"
            tempname ID0
            qui gen byte `ID0' = 1
        }
        local shpframe `"`cframe'"'
        local hasSHP 0
    }
    else {
        _get id, local(ID0) strict
        local hasSHP 1
    }
    // generate
    frame `shpframe' {
        if !`hasSHP' local ID `ID0'
        else _get id, local(ID) strict
        _get coordinates, local(XY) strict
        foreach var of local TMP {
            qui gen double `var' = .
        }
        mata: _generate_`subcmd'(tokens("`TMP'"), `"`ID'"', `"`XY'"')
    }
    if `hasSHP' {
        qui geoframe_copy `shpframe' `TMP'
    }
    if "`subcmd'"=="area" {
        if `"`scale'"'!="" {
            qui replace `TMP' = `TMP' / (`scale')^2
        }
    }
    // cleanup
    foreach var of local namelist {
        capt confirm new variable `var'
        if _rc==1 exit _rc
        if _rc drop `var'
        gettoken tmp TMP : TMP
        rename `tmp' `var'
    }
    if "`set'"=="" geoframe_set `subcmd' `namelist'
    if `: list sizeof namelist'>1 local msg variables
    else                          local msg variable
    _di_frame "(`msg' {bf:`namelist'} added to frame " `cframe' ")"
end

program _generate_pid
    __generate_B pid `0'
end

program _generate_gtype
    __generate_B gtype `0'
end

program _generate_dir
    __generate_B dir `0'
end

program __generate_B
    // syntax
    gettoken subcmd 0 : 0
    if "`subcmd'"=="pid" {
        syntax [name] [, replace noset ]
        if "`namelist'"=="" local namelist _PID
        local label nolabel
        local dtype long
        local idmsg 1
    }
    else if "`subcmd'"=="gtype" {
        syntax [name] [, replace nolabel ]
        if "`namelist'"=="" local namelist _GTYPE
        local set noset
        local dtype byte
        local idmsg 0
        local lbls 0 "Empty" 1 "Point" 2 "LineString" 3 "Polygon" 
    }
    else if "`subcmd'"=="dir" {
        syntax [name] [, replace nolabel ]
        if "`namelist'"=="" local namelist _DIR
        local set noset
        local dtype byte
        local idmsg 0
        local lbls -1 "Clockwise" 0 "Undefined" 1 "Counterclockwise"
    }
    else exit 198
    // frames
    local cframe = c(frame)
    _get type, local(type)
    if inlist(`"`type'"',"shape","pc") local shpframe `"`cframe'"'
    else {
        _get shpframe, local(shpframe)
        if `"`shpframe'"'=="" {
            di as txt "(shape frame not found;"/*
                */ " treating current frame as shape frame)"
            local shpframe `"`cframe'"'
        }
    }
    // generate
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
        _get id, local(ID)
        if `"`ID'"'=="" {
            if `idmsg' di as txt "(ID not available;"/*
                */ " assuming all shape items belong to same unit)"
            tempname ID
            qui gen byte `ID' = 1
        }
        _get coordinates, local(XY) strict
        // run routine
        tempvar TMP
        qui gen `dtype' `TMP' = .
        mata: _generate_`subcmd'("`TMP'", `"`ID'"', "`XY'")
        if "`dtype'"!="byte" qui compress `TMP'
        // cleanup
        capt confirm new variable `namelist'
        if _rc==1 exit _rc
        if _rc drop `namelist'
        rename `TMP' `namelist'
        if "`set'"=="" geoframe_set `subcmd' `namelist'
        if "`label'"=="" {
            label define `namelist' `lbls', replace
            label values `namelist' `namelist', nofix
        }
        _di_frame "(variable {bf:`namelist'} added to frame " `shpframe' ")"
    }
end

program _generate_plevel
    syntax [name] [if] [in] [, by(varname numeric) replace noset noDOTs ]
    if "`namelist'"=="" local namelist _PLEVEL
    local cframe = c(frame)
    marksample touse
    if "`by'"!="" markout `touse' `by'
    _get type, local(type)
    if !inlist(`"`type'"',"shape","pc") {
        _get shpframe, local(shpframe)
        if `"`shpframe'"'=="" {
            di as txt "(shape frame not found;"/*
                */ " treating current frame as shape frame)"
        }
    }
    if `"`shpframe'"'=="" {
        local shpframe `"`cframe'"'
        local BY `by'
    }
    else {
        _markshapes `shpframe' `touse' `touse'
        if "`by'"!="" {
            frame `shpframe' {
                tempvar BY
                capt geoframe_copy `cframe' `by', target(`BY') unique
                if _rc==1 exit 1
                if _rc { // try again with estimation sample only
                    qui geoframe_copy `cframe' `by' if `touse',/*
                        */ target(`BY') unique
                }
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
        _get id, local(ID)
        if `"`ID'"'=="" {
            di as txt "(ID not available;"/*
                */ " assuming all items belong to same unit)"
            tempname ID
            qui gen byte `ID' = 1
        }
        _get coordinates, local(XY) strict
        tempname PL
        qui gen long `PL' = .
        if "`BY'"!="" {
            qui levelsof `BY' if `touse', local(bylvls)
            foreach lvl of local bylvls {
                if "`dots'"=="" di as txt "(processing `by'=`lvl')"
                mata: _generate_plevel("`dots'"!="", "`PL'", `"`ID'"',/*
                    */ `"`XY'"', "`touse'", "`BY'", `lvl') // returns N
                if `N'==1 local msg item
                else      local msg items
                if "`dots'"=="" di as txt "(`N' nested `msg' found)"
                else di as txt "(`by'=`lvl': `N' nested `msg' found)"
            }
        }
        else {
            mata: _generate_plevel("`dots'"!="", "`PL'", `"`ID'"',/*
                */ `"`XY'"', "`touse'")
            if `N'==1 local msg item
            else      local msg items
            di as txt "(`N' nested `msg' found)"
        }
        qui compress `PL'
        if `newvar' {
            if `newvar'==2 drop `namelist'
            rename `PL' `namelist'
            _di_frame "(variable {bf:`namelist'} added to frame "/*
                */ `shpframe' ")"
        }
        else {
            qui replace `namelist' = `PL' if `touse'
            _di_frame "(variable {bf:`namelist'} updated in frame "/*
                */ `shpframe' ")"
        }
        if "`set'"=="" geoframe_set plevel `namelist'
    }
end

program _generate_shpmatch
    syntax [name] [, replace ]
    if "`namelist'"=="" local namelist _SHPMATCH
    if "`replace'"=="" {
        confirm new variable `namelist'
    }
    _get shpframe, local(shpframe) strict
    frame `shpframe' {
        tempvar touse
        qui gen byte `touse' = 1
    }
    qui geoframe_copy `shpframe' `touse'
    qui replace `touse' = `touse'<.
    capt confirm new variable `namelist'
    if _rc==1 exit _rc
    if _rc drop `namelist'
    rename `touse' `namelist'
    _di_frame "(variable {bf:`namelist'} added to frame " `"`c(frame)'"' ")"
end

program geoframe_select
    // syntax
    syntax [if] [in] [, IFshp(str asis) nodrop NOShp UNLink/*
        */ into(namelist max=2) replace CURrent ]
    if `"`ifshp'"'!="" {
        if "`noshp'"!="" {
            di as err "ifshp() and noshp not both allowed"
            exit 198
        }
        if "`unlink'"!="" {
            di as err "ifshp() and unlink not both allowed"
            exit 198
        }
    }
    if "`unlink'"!="" local noshp noshp
    local frame = c(frame)
    _get shpframe, local(shpframe0)
    if `"`shpframe0'"'=="" {
        if `"`ifshp'"'!="" {
            di as err "ifshp() not allowed; the current frame is not"/*
                */ " linked to a shape frame"
            exit 198
        }
    }
    if "`noshp'"=="" local shpframe `"`shpframe0'"'
    // parse into()
    if "`into'"!="" {
        tempname tname
        gettoken newname into : into
        if "`replace'"=="" confirm new frame `newname'
        if `"`shpframe'"'!="" {
            tempname tshpname
            gettoken newshpname : into
            if "`newshpname'"=="" local newshpname `newname'_shp
            else if "`newshpname'"=="`newname'" {
                di as err "{it:newshpname} must be different from"/*
                    */ " {it:newname}"
                exit 198
            }
            if "`replace'"=="" confirm new frame `newshpname'
        }
    }
    // mark samples
    if `"`if'`in'"'!="" marksample touse
    if `"`shpframe'"'!="" {
        if `"`if'`in'`ifshp'"'!="" {
            tempvar shptouse 
            _markshapes `shpframe' `shptouse' `touse'
            if `"`ifshp'"'!="" {
                frame `shpframe' {
                    qui replace `shptouse' = 0 if `shptouse' & !(`ifshp')
                    if "`drop'"=="" {
                        // tag units with nonzero selection
                        _get id, local(id) strict
                        tempvar merge
                        qui gen byte `merge' = `shptouse'
                        mata: _tag_unit_if_any("`id'" , "`merge'")
                    }
                }
                if "`drop'"=="" {
                    if "`touse'"=="" tempvar touse
                    else             drop `touse'
                    qui geoframe_copy `shpframe' `merge', target(`touse')
                    qui replace `touse' = 0 if `touse'>=.
                }
            }
        }
    }
    // copy frames if into()
    foreach shp in "" "shp" {
        if "`t`shp'name'"!="" {
            frame copy ``shp'frame' `t`shp'name'
            // remove all linkage info in copied frame
            frame `t`shp'name' {
                _set_shpframe
                _set_linkname
                capt drop _GEOFRAME_lnkvar_*
            }
        }
        else local t`shp'name ``shp'frame'
    }
    // drop observations and update linkage
    nobreak {
        foreach shp in "" "shp" {
            // drop obs
            local msg
            local N`shp'drop .
            if "``shp'touse'"!="" {
                frame `t`shp'name' {
                    qui keep if ``shp'touse'
                    local N`shp'drop = r(N_drop)
                    local msg `: di %9.0gc `N`shp'drop''
                    if `N`shp'drop'==1 local msg "`msg' observation"
                    else               local msg "`msg' observations"
                }
            }
            // rename frames
            if "`new`shp'name'"!="" {
                frame `t`shp'name' {
                    capt confirm new frame `new`shp'name'
                    if _rc frame drop `new`shp'name'
                    frame rename `t`shp'name' `new`shp'name'
                }
                if "`msg'"!="" local msg "; `msg' dropped"
                _di_frame "(frame " `new`shp'name' " created`msg')"
            }
            else if "`msg'"!="" {
                _di_frame "(dropped `msg' in frame " ``shp'frame' ")"
            }
        }
        // update linkage
        if "`newname'"=="" {
            if `"`shpframe'"'!="" {
                if "`unlink'"!=""             geoframe_unlink
                else if `Ndrop'<. & `Ndrop'>0 qui geoframe_relink
            }
        }
        else {
            if `"`shpframe'"'!="" {
                frame `newname': qui geoframe_link `newshpname'
            }
            else if `"`shpframe0'"'!="" & "`unlink'"=="" {
                if "`newname'"!=`"`shpframe0'"' {
                    frame `newname': geoframe_link `shpframe0'
                }
            }
        }
    }
    if "`newname'"!="" {
        _describe_and_make_current `newname' "`current'" nodescribe
    }
end

program geoframe_project
    syntax [anything(name=projection)] [if] [in] [, RADian/*
        */ xy(varlist numeric) nodrop noDOTs * ] // ignoring nodrop and nodots
    if mod(`:list sizeof xy',2) {
        di as err "xy(): number of variable must be even"
        exit 198
    }
    gettoken pname pargs : projection
    mata: st_local("pname", _geo_project_find(`"`pname'"'))
    _manipulate project `if' `in', `options'/*
        */ _args("`pname'" `"`pargs'"' "`radian'" "`xy'")
end

program geoframe_rescale
    syntax [anything] [if] [in] [, x0(str) y0(str)/*
        */ xy(varlist numeric) nodrop noDOTs * ] // ignoring nodrop and nodots
    if mod(`:list sizeof xy',2) {
        di as err "xy(): number of variable must be even"
        exit 198
    }
    if `: list sizeof anything'>2 error 198
    gettoken sx anything : anything
    gettoken sy anything : anything
    if `"`sx'"'=="" local sx 1
    if `"`sy'"'=="" local sy `"`sx'"'
    if `"`x0'"'=="" local x0 0
    if `"`y0'"'=="" local y0 0
    _manipulate rescale `if' `in', `options'/*
        */ _args(`"`sx'"' `"`sy'"' `"`x0'"' `"`y0'"' "`xy'")
end

program geoframe_clip
    syntax anything(name=matname id="matname") [if] [in] [,/*
        */ Line noCLip STrict noSPlit nodrop noDOTs * ]
    tempname MASK
    confirm matrix `matname'
    mat `MASK' = `matname'
    _manipulate clip `if' `in', `options'/*
        */ _args("" "`MASK'" "`line'" "`clip'" "`strict'" "`split'"/*
        */ "`drop'" "`dots'")
end

program geoframe_rclip
    syntax anything(name=limits id="limits") [if] [in] [,/*
        */ Line noCLip STrict noSPlit nodrop noDOTs * ]
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
    _manipulate clip `if' `in', `options'/*
        */ _args("rclip" "`MASK'" "`line'" "`clip'" "`strict'" "`split'"/*
        */ "`drop'" "`dots'")
end

program geoframe_simplify
    syntax [anything(id="delta" name=delta)] [if] [in] [,/*
        */ ABSolute JOINTly noREFine REFine2(str asis) nodrop noDOTs * ]
    if "`delta'"!="" {
        numlist `"`delta'"', max(1) range(>=0)
        local delta `r(numlist)'
    }
    if "`refine'"!="" {
        if `"`refine2'"'!="" {
            di as err "{bf:norefine} and {bf:refine()} not both allowed"
            exit 198
        }
    }
    else {
        capt n _simplify_parse_refine `refine2'
        if _rc==1 exit 1
        if _rc {
            di as err "(error in refine())"
            exit 198
        }
    }
    _manipulate simplify `if' `in', `options'/*
        */ _args("`delta'" "`absolute'" "`jointly'" "`drop'" "`dots'"/*
        */ `"`refine2'"')
end

program _simplify_parse_refine
    syntax [anything(id="delta" name=delta)] [, ABSolute ]
    if "`delta'"!="" {
        numlist `"`delta'"', max(1) range(>=0)
        local delta `r(numlist)'
    }
    c_local refine2 `""`delta'" "`absolute'""'
end

program geoframe_refine
    syntax [anything(id="delta" name=delta)] [if] [in] [,/*
        */ ABSolute nodrop noDOTs * ] // ignoring nodrop
    if "`delta'"!="" {
        numlist `"`delta'"', max(1) range(>=0)
        local delta `r(numlist)'
    }
    _manipulate refine `if' `in', `options'/*
        */ _args("`delta'" "`absolute'" "`dots'")
end

program _manipulate
    // syntax
    gettoken subcmd 0 : 0
    syntax [if] [in] [, NOShp IFshp(str asis) into(namelist max=2)/*
        */ replace CURrent fast _args(str asis) ]
    if "`noshp'"!="" {
        if !inlist("`subcmd'","project","rescale") {
            di as err "option {bf:noshp} not allowed"
            exit 198
        }
        if `"`ifshp'"'!="" {
            di as err "ifshp() and noshp not both allowed"
            exit 198
        }
    }
    local frame = c(frame)
    _get shpframe, local(shpframe)
    if `"`shpframe'"'=="" {
        if `"`ifshp'"'!="" {
            di as err "ifshp() not allowed; the current frame is not"/*
                */ " linked to a shape frame"
            exit 198
        }
        _get type, local(type)
        if !inlist(`"`type'"',"shape","pc") {
            if !inlist("`subcmd'","project","rescale") {
                di as txt "(shape frame not found;"/*
                    */ " treating current frame as shape frame)"
            }
            else if "`noshp'"=="" {
                di as txt "(shape frame not found;"/*
                    */ " manipulating current frame only)"
            }
        }
        local drop nodrop
    }
    else {
        if "`noshp'"!="" local shpframe
        else confirm frame `shpframe'
    }
    // parse into()
    if "`into'"!="" {
        gettoken newname into : into
        if "`replace'"=="" confirm new frame `newname'
        if "`shpframe'"!="" {
            gettoken newshpname : into
            if "`newshpname'"=="" local newshpname `newname'_shp
            else if "`newshpname'"=="`newname'" {
                di as err "{it:newshpname} must be different from"/*
                    */ " {it:newname}"
                exit 198
            }
            if "`replace'"=="" confirm new frame `newshpname'
        }
    }
    // backup frame(s)
    if "`fast'"=="" | "`newname'"!="" {
        preserve
        if "`shpframe'"!="" frame `shpframe': preserve
    }
    // mark samples
    marksample touse
    if "`shpframe'"!="" {
        _markshapes `shpframe' `touse' `touse'
        if `"`ifshp'"'!="" {
            frame `shpframe' {
                qui replace `touse' = 0 if `touse' & !(`ifshp')
                // tag units with nonzero selection
                _get id, local(id) strict
                tempvar merge
                qui gen byte `merge' = `touse'
                mata: _tag_unit_if_any("`id'" , "`merge'")
            }
            drop `touse'
            qui geoframe_copy `shpframe' `merge', target(`touse')
            qui replace `touse' = 0 if `touse'>=.
        }
    }
    // apply procedure
    local Ndrop 0
    local Nshpdrop 0
    local Nadd 0
    if inlist("`subcmd'","project","rescale") {
        _manipulate_`subcmd' `touse' `frame' "`shpframe'" `_args'
    }
    else {
        if "`shpframe'"!="" local shp shp
        else                local shp
        local drop nodrop // (will be updated by procedure)
        frame ``shp'frame' {
            // run procedure
            _manipulate_`subcmd' `touse' `_args'
            if "`N_drop'"!="" local N`shp'drop =`N`shp'drop' + `N_drop'
            if "`N_add'"!=""  local Nadd = `Nadd' + `N_add'
            // update PID and shape_order (if set)
            _get sid, local(sid)
            if `"`sid'"'!="" {
                _get id, local(ID) strict
                qui replace `sid' =/*
                    */ cond(_n==1 | `ID'!=`ID'[_n-1], 1, 1+`sid'[_n-1])
            }
            _get pid, local(pid)
            if `"`pid'"'!="" qui geoframe_generate pid `pid', replace
        }
        if "`drop'"=="" {
            if "`shpframe'"!="" {
                drop `touse'
                qui geoframe_copy `shpframe' `touse'
                qui drop if `touse'>=.
                local Ndrop = `Ndrop' + r(N_drop)
                if "`newname'"=="" qui geoframe_link `shpframe' /* will link
                    later in case of into() */
            }
        }
    }
    // copy frames if into(); else cancel preserve
    if "`newname'"!="" {
        // select obs and make copy
        foreach shp in "" "shp" {
            if "``shp'frame'"=="" continue
            frame ``shp'frame' {
                qui keep if `touse'
                local N`shp'drop = `N`shp'drop' + r(N_drop)
            }
            tempname t`shp'name
            frame copy ``shp'frame' `t`shp'name'
            frame `t`shp'name' {
                _set_shpframe
                _set_linkname
                capt drop _GEOFRAME_lnkvar_*
            }
        }
        // restore original frames
        restore
        if "`shpframe'"!="" frame `shpframe': restore
        // rename modified frames and update linkage
        foreach shp in "" "shp" {
            if "``shp'frame'"=="" continue
            frame `t`shp'name' {
                capt confirm new frame `new`shp'name'
                if _rc frame drop `new`shp'name'
                frame rename `t`shp'name' `new`shp'name'
                local `shp'frame `new`shp'name'
            }
        }
        if "`shpframe'"!="" {
            frame `frame': qui geoframe_link `shpframe'
        }
    }
    else if "`fast'"=="" {
        restore, not
        if "`shpframe'"!="" frame `shpframe': restore, not
    }
    // reporting
    foreach shp in "" "shp" {
        if `N`shp'drop' {
            if `N`shp'drop'==1 local msg observation
            else               local msg observations
            local tmp `: di %9.0gc `N`shp'drop''
            local dmsg`shp' "`tmp' `msg'"
        }
    }
    if `Nadd' {
        if `Nadd'==1 local msg observation
        else         local msg observations
        local tmp `: di %9.0gc `Nadd''
        if "`shpframe'"!="" local shp shp
        else                local shp
        local amsg`shp' "`tmp' `msg'"
    }
    if "`newname'"!="" {
        foreach shp in "" "shp" {
            if "``shp'frame'"=="" continue
            local msg
            if "`dmsg`shp''"!="" local msg "; `dmsg`shp'' dropped"
            if "`amsg`shp''"!="" local msg "`msg'; `amsg`shp'' added"
            _di_frame "(frame " ``shp'frame' " created`msg')"
        }
    }
    else {
        if "`dmsg'"!=""    _di_frame "(dropped `dmsg' in frame " `frame' ")"
        if "`amsg'"!=""    _di_frame "(added `amsg' in frame " `frame' ")"
        if "`dmsgshp'"!="" _di_frame "(dropped `dmsgshp' in frame " `shpframe' ")"
        if "`amsgshp'"!="" _di_frame "(added `amsgshp' in frame " `shpframe' ")"
    }
    if "`newname'"!="" {
        _describe_and_make_current `newname' "`current'" nodescribe
    }
end

program _manipulate_project
    args touse frame shpframe pname pargs radian xy
    di as txt "(applying {bf:`pname'} projection)"
    // project
    foreach shp in "" shp {
        if `"``shp'frame'"'=="" continue
        frame ``shp'frame' {
            _get coordinates, local(vars)
            if "`shp'"=="" local vars `vars' `xy' // current frame; add xy()
            local XY`shp' `vars'
            local TMP`shp'
            if `"`vars'"'=="" {
                _di_frame "(no coordinates found in frame " ``shp'frame' ")"
            }
            else {
                while (`"`vars'"'!="") {
                    gettoken X vars : vars
                    gettoken Y vars : vars
                    tempname Xtmp Ytmp
                    local TMP`shp' `TMP`shp'' `Xtmp' `Ytmp'
                    qui gen double `Xtmp' = `X' if `touse'
                    qui gen double `Ytmp' = `Y' if `touse'
                    qui count if `touse'
                    if r(N)==0 continue // no obs
                    mata: _project("`touse'", "`Xtmp' `Ytmp'", "`pname'",/*
                        */"`pargs'", "`radian'")
                }
            }
        }
    }
    // cleanup
    foreach shp in "" shp {
        if `"``shp'frame'"'=="" continue
        frame ``shp'frame' {
            foreach x of local XY`shp' {
                gettoken xtmp TMP`shp' : TMP`shp'
                qui replace `x' = `xtmp' if `touse'
            }
        }
    }
end

program _manipulate_rescale
    args touse frame shpframe sx sy x0 y0 xy
    di as txt "(applying rescaling: "/*
        */ `"X' = (X - (`x0')) * `sx'"'/*
        */ "; "/*
        */ `"Y' = (Y - (`y0')) * `sy'"'/*
        */ ")"
    // project
    foreach shp in "" shp {
        if `"``shp'frame'"'=="" continue
        frame ``shp'frame' {
            _get coordinates, local(vars)
            if "`shp'"=="" local vars `vars' `xy' // current frame; add xy()
            local XY`shp' `vars'
            local TMP`shp'
            if `"`vars'"'=="" {
                _di_frame "(no coordinates found in frame " ``shp'frame' ")"
            }
            else {
                while (`"`vars'"'!="") {
                    gettoken X vars : vars
                    gettoken Y vars : vars
                    tempname Xtmp Ytmp
                    local TMP`shp' `TMP`shp'' `Xtmp' `Ytmp'
                    qui gen double `Xtmp' = (`sx')*(`X'-(`x0')) if `touse'
                    qui gen double `Ytmp' = (`sy')*(`Y'-(`y0')) if `touse'
                }
            }
        }
    }
    // cleanup
    foreach shp in "" shp {
        if `"``shp'frame'"'=="" continue
        frame ``shp'frame' {
            foreach x of local XY`shp' {
                gettoken xtmp TMP`shp' : TMP`shp'
                qui replace `x' = `xtmp' if `touse'
            }
        }
    }
end

program _manipulate_clip
    args touse rclip MASK line noclip strict nosplit drop dots
    if "`noclip'"!="" {
        if "`strict'"!="" {
            if "`nosplit'"!="" local method 5
            else               local method 3
        }
        else {
            if "`nosplit'"!="" local method 4
            else               local method 2
        }
    }
    else if "`line'"!=""       local method 1
    else                       local method 0
    _get coordinates, local(XY) strict
    _get id, local(ID)
    if `"`ID'"'=="" {
        tempvar ID
        qui gen byte `ID' = 1
    }
    tempvar OUT
    qui gen byte `OUT' = 0
    mata: _clip("`ID'", "`OUT'", tokens("`XY'"), "`touse'", "`MASK'",/*
        */ "`rclip'"!="", `method', "`drop'"!="", "`dots'"=="")
    qui drop if `OUT' & `touse'
    c_local N_drop = r(N_drop)
    c_local N_add `Nadd'
    c_local drop `drop'
end

program _manipulate_simplify
    args touse delta absolute jointly drop dots refine
    _get coordinates, local(XY) strict
    if `: list sizeof XY'==4 {
        di as err "four coordinate variables found in frame"/*
            */ " {bf:`c(frame)'}" _n "paired-coordinates data not"/*
            */ " supported by {bf:geoframe simplify}"
        exit 499
    }
    gettoken X XY : XY
    gettoken Y XY : XY
    _get id, local(ID)
    if `"`ID'"'=="" {
        tempvar ID
        qui gen byte `ID' = 1
    }
    tempvar OUT
    qui gen byte `OUT' = 0
    if "`delta'"=="" | "`absolute'"=="" {
        if "`delta'"=="" local delta 1
        su `X' if `touse', meanonly
        local xrange = r(max)-r(min)
        su `Y' if `touse', meanonly
        local yrange = r(max)-r(min)
        local delta = (`xrange'/2000) * (`yrange'/2000) / 2 * `delta'
    }
    local msg `: di %9.0g `delta''
    di as txt "(simplification threshold = `msg')"
    mata: _simplify("`ID'", "`OUT'", ("`X'","`Y'"), "`touse'", `delta', /*
        */ "`jointly'"!="", "`drop'"!="", "`dots'"=="")
    qui drop if `OUT'
    c_local N_drop = r(N_drop)
    c_local drop `drop'
    // apply refinement
    if `"`refine'"'=="" exit
    _manipulate_refine `touse' `refine' "`dots'"
    c_local N_add `N_add'
end

program _manipulate_refine
    args touse delta absolute dots
    _get coordinates, local(XY) strict
    if `: list sizeof XY'==4 {
        di as err "four coordinate variables found in frame"/*
            */ " {bf:`c(frame)'}" _n "paired-coordinates data not"/*
            */ " supported by {bf:geoframe refine}"
        exit 499
    }
    gettoken X XY : XY
    gettoken Y XY : XY
    _get id, local(ID)
    if `"`ID'"'=="" {
        tempvar ID
        qui gen byte `ID' = 1
    }
    if "`delta'"=="" | "`absolute'"=="" {
        if "`delta'"=="" local delta 1
        su `X' if `touse', meanonly
        local xrange = r(max)-r(min)
        su `Y' if `touse', meanonly
        local yrange = r(max)-r(min)
        local delta = sqrt(`xrange'^2 + `yrange'^2) / 200 / `delta'
        if `delta'==0 local delta .
    }
    local msg `: di %9.0g `delta''
    di as txt "(refinement threshold = `msg')"
    mata: _refine("`ID'", ("`X'","`Y'"), "`touse'", `delta', "`dots'"=="")
    c_local N_add `Nadd'
end

program geoframe_grid
    _grid 0 `0'
end

program geoframe_tissot
    _grid 1 `0'
end

program _grid
    gettoken tissot 0 : 0
    if `tissot' local opts r(numlist max=1 >0)
    else        local opts noEXtend mesh
    syntax namelist(id="newname" name=newnames max=2) [if] [in] [,/*
        */ x(str) y(str) `opts' tight PADding(str)/*
        */ RADian n(numlist int max=1 >0)/*
        */ coordinates(namelist) /* undocumented
        */ noShp replace CURrent ]
    if "`n'"=="" local n 100
    if `"`padding'"'!="" local tight tight
    mata: _geo_parse_marginexp("padding", `"`padding'"')
    gettoken newname newnames : newnames
    gettoken newshpname : newnames
    if "`newshpname'"=="" local newshpname "`newname'_shp"
    if "`replace'"=="" {
        confirm new frame `newname'
        confirm new frame `newshpname'
    }
    if `tissot' {
        if "`radian'"!="" local tl = _pi/2
        else              local tl = 90
        _grid_parse "range(>0)" ""                    X `x'
        _grid_parse "range(>0)" "range(>-`tl' <`tl')" Y `y'
        if "`r'"!="" {
            tempname R
            scalar `R' = `r'
        }
        local extend noextend
        local mesh
    }
    else { // grid
        _grid_parse "range(>=0)" "" X `x'
        _grid_parse "range(>=0)" "" Y `y'
    }
    // process x()/y() numlists
    foreach x in X Y {
        if "``x'list'"=="" continue
        tempname `x'LIST
        mata: st_matrix("``x'LIST'", strtoreal(tokens(st_local("`x'list")))')
    }
    // get data range if needed
    if "`XLIST'"=="" | "`YLIST'"=="" | "`extend'"=="" {
        marksample touse
        // find shapes
        local cframe = c(frame)
        if "`shp'"=="" {
            _get shpframe, local(shpframe)
            if `"`shpframe'"'=="" {
                _get type, local(type)
                if !inlist(`"`type'"',"shape","pc") {
                    di as txt "(shape frame not found;"/*
                        */ " treating current frame as shape frame)"
                }
            }
        }
        if `"`shpframe'"'=="" {
            if `"`coordinates'"'!="" local XY `coordinates'
            else _get coordinates, local(XY) strict
            markout `touse' `XY'
            local shpframe `"`cframe'"'
        }
        else {
            _markshapes `shpframe' `touse' `touse'
            frame `shpframe' {
                if `"`coordinates'"'!="" local XY `coordinates'
                else _get coordinates, local(XY) strict
                markout `touse' `XY'
            }
        }
        // obtain data range
        frame `shpframe' {
            gettoken X XY : XY
            gettoken Y XY : XY
            local PAD: copy local padding
            foreach x in X Y {
                tempname `x'min `x'max
                su ``x'' if `touse', meanonly
                gettoken pad PAD : PAD
                scalar ``x'min' = r(min) - (r(max)-r(min))*(`pad'/100)
                gettoken pad PAD : PAD
                scalar ``x'max' = r(max) + (r(max)-r(min))*(`pad'/100)
            }
            if `"`XY'"'!="" { // pc-data
                gettoken X XY : XY
                gettoken Y XY : XY
                local PAD: copy local padding
                foreach x in X Y {
                    su ``x'' if `touse', meanonly
                    gettoken pad PAD : PAD
                    scalar ``x'min' = min(``x'min',/*
                        */ r(min) - (r(max)-r(min))*(`pad'/100))
                    gettoken pad PAD : PAD
                    scalar ``x'max' = max(``x'max',/*
                        */ r(max) + (r(max)-r(min))*(`pad'/100))
                }
            }
            if `tissot' { // restrict Y
                if "`radian'"!="" {
                    local tl (_pi/2)
                    local msg radians
                }
                else {
                    local tl 90
                    local msg degrees
                }
                if `Ymax'<-`tl' | `Ymin'>`tl' {
                    di as error "coordinates do not seem to be in `msg'"/*
                        */"; cannot compute Tissot's indicatrices"
                    exit 499
                }
                scalar `Ymin' = max(`Ymin', -`tl')
                scalar `Ymax' = min(`Ymax',  `tl')
            }
        }
    }
    // determine grid points if not provided
    local ndef = 7 - 2*`tissot' // default
    if "`X_n'"=="" & "`Y_n'"=="" local Y_n `ndef'
    tempname tmp
    foreach x in Y X {
        if `"``x'LIST'"'!="" continue
        // determine number
        if "``x'_n'"=="" {
            if "`x'"=="X" {
                local y Y
                if `Y_n'==0 local Yn `ndef'
                else        local Yn `Y_n'
            }
            else {
                local y X
                if `X_n'==0 local Y_n `ndef'
                else        local Yn  `X_n'
            }
        }
        if "``x'_n'"=="" {
            scalar `tmp' = (``y'max' - ``y'min') / (`Yn' - 1 + 2*`tissot')
            local `x'_n = round((``x'max' - ``x'min') / `tmp') + 1 - 2*`tissot'
            if ``x'_n'<1 | ``x'_n'>=. local `x'_n 1
            local `x'_n = max(``x'_n',min(`Yn',3))
        }
        // determine positions
        if ``x'_n'<1 continue
        tempname `x'LIST
        mata: _grid_pos("``x'LIST'", ``x'_n', `tissot',/*
            */ "`tight'"!="", "`radian'"!="", st_numscalar("``x'min'"),/*
            */ st_numscalar("``x'max'"))
        // display info
        di as txt "(``x'_n' position" _c
        if ``x'_n'>1 di "s at " ``x'LIST'[1,1]/*
            */ "(" ``x'LIST'[2,1]-``x'LIST'[1,1] ")" ``x'LIST'[``x'_n',1] _c
        else di " at " ``x'LIST'[1,1] _c
        di " on `x' axis)"
    }
    // determine radius (tissot)
    if `tissot' {
        if "`r'"=="" {
            tempname R
            scalar `R' = min((`XLIST'[`X_n',1]-`XLIST'[1,1])/(`X_n'-1),/*
                          */ (`YLIST'[`Y_n',1]-`YLIST'[1,1])/(`Y_n'-1)) / 4.3
            if `R'>=. { // fallback if X_n and Y_n equal 1
                scalar `R' = cond("`radian'"!="", _pi/180, 1) * 30 
            }
        }
        scalar `tmp' = min(`tl' - `YLIST'[`Y_n',1], `YLIST'[1,1] + `tl')
        if `R'>`tmp' {
            scalar `R' = `tmp'
            if "`r'"!="" {
                di as txt "(requested radius too large; reset to " `R' ")"
            }
        }
        if "`r'"=="" di as txt "(radius set to " `R' ")"
    }
    // add min and max (grid)
    else {
        foreach x in X Y {
            if !``x'_n' {
                tempname `x'LIST
            }
            else if "`extend'"=="" {
                scalar ``x'min' = min(``x'min',``x'LIST'[1,1])
                scalar ``x'max' = max(``x'max',``x'LIST'[``x'_n',1])
            }
            else {
                tempname `x'min `x'max
                scalar ``x'min' = ``x'LIST'[1,1]
                scalar ``x'max' = ``x'LIST'[``x'_n',1]
            }
            if "`mesh'"!="" & ``x'_n'>2 {
                matrix ``x'LIST' = ``x'LIST'[2..``x'_n'-1,1]
                local `x'_n = ``x'_n'-2
            }
            if !``x'_n' {
                matrix ``x'LIST' = ``x'min' \ ``x'max'
            }
            else {
                matrix ``x'LIST' = ``x'min' \ ``x'LIST' \ ``x'max'
            }
        }
    }
    // prepare new frame
    tempname newframe newshpframe
    if `tissot' local vlist double(_ID _CX _CY)
    else        local vlist double(_ID _CX _CY xmin ymin xmax ymax) byte(axis)
    frame create `newframe' `vlist'
    frame create `newshpframe' double(_ID _X _Y)
    // create grid
    frame `newframe' {
        if `tissot' {
            mata: _tissot("`newshpframe'", `n', "`radian'"!="",/*
                */ st_numscalar("`R'"),/*
                */ st_matrix("`XLIST'"), st_matrix("`YLIST'"))
        }
        else { // grid
            lab def axis 1 "X" 2 "Y"
            lab val axis axis
            mata: _grid("`newshpframe'", `n',/*
                */ st_matrix("`XLIST'"), st_matrix("`YLIST'"))
        }
    }
    // cleanup
    frame `newshpframe' {
        qui compress _ID
        _set_type shape
        capt confirm new frame `newshpname'
        if _rc==1 exit 1
        if _rc frame drop `newshpname'
        frame rename `newshpframe' `newshpname'
    }
    frame `newframe' {
        qui compress _ID
        _set_type attribute
        if `tissot' char _dta[GEOFRAME_feature] tissot
        else        char _dta[GEOFRAME_feature] grid
        capt confirm new frame `newname'
        if _rc==1 exit 1
        if _rc frame drop `newname'
        frame rename `newframe' `newname'
        qui geoframe_link `newshpname'
    }
    _di_frame "(frame " `newname' " created)"
    _di_frame "(frame " `newshpname' " created)"
    _describe_and_make_current `newname' "`current'" nodescribe
end

program _grid_parse
    gettoken opt1 0 : 0
    gettoken opt2 0 : 0
    gettoken nm 0 : 0
    local 0 = strtrim(`"`0'"')
    if `"`0'"'=="" {
        local n ""
    }
    else if substr(`"`0'"',1,1)=="#" {
        local 0 = substr(`"`0'"',2,.)
        numlist `"`0'"', int min(1) max(1) `opt1'
        local n `r(numlist)'
    }
    else {
        numlist `"`0'"', sort `opt2'
        local x `r(numlist)'
        local x: list uniq x
        local n: list sizeof x
    }
    c_local `nm'_n `n'
    c_local `nm'list `x'
end

program geoframe_bbox
    syntax namelist(id="newname" name=newnames max=2) [if] [in] [, noShp/*
        */ by(varname numeric) PADding(real 0) hull ROTate CIRcle/*
        */ n(numlist int max=1 >0) ANGle(real 0) noADJust replace CURrent ]
    gettoken newname newnames : newnames
    gettoken newshpname : newnames
    if "`newshpname'"=="" local newshpname "`newname'_shp"
    if "`hull'"!="" & "`circle'"!="" {
        di as err "only one of {bf:circle} and {bf:hull} allowed"
        exit 198
    }
    if "`hull'"!=""        local btype 3
    else if "`circle'"!="" local btype 2
    else if "`rotate'"!="" local btype 1
    else                   local btype 0
    if "`n'"=="" local n 100
    if "`replace'"=="" {
        confirm new frame `newname'
        confirm new frame `newshpname'
    }
    // mark sample
    marksample touse
    if "`by'"!="" markout `touse' `by'
    // find shapes
    local cframe = c(frame)
    _get id, local(ID0)
    if "`shp'"=="" {
        _get shpframe, local(shpframe)
        if `"`shpframe'"'=="" {
            _get type, local(type)
            if !inlist(`"`type'"',"shape","pc") {
                di as txt "(shape frame not found;"/*
                    */ " treating current frame as shape frame)"
            }
        }
    }
    if `"`shpframe'"'=="" {
        _get coordinates, local(XY) strict
        markout `touse' `XY'
        local shpframe `"`cframe'"'
        local BY `by'
        local ID `ID0'
    }
    else {
        _markshapes `shpframe' `touse' `touse'
        frame `shpframe' {
            _get coordinates, local(XY) strict
            markout `touse' `XY'
            _get id, local(ID)
            if "`by'"!="" {
                if "`by'"=="`ID0'" & "`ID'"!="" local BY `ID'
                else {
                    tempvar BY
                    capt geoframe_copy `cframe' `by', target(`BY') unique
                    if _rc==1 exit 1
                    if _rc { // try again with estimation sample only
                        qui geoframe_copy `cframe' `by' if `touse',/*
                            */ target(`BY') unique
                    }
                }
            }
        }
    }
    // prepare new frame
    tempname newframe newshpframe
    frame create `newframe' double(_ID)
    frame create `newshpframe' double(_ID _X _Y)
    // obtain boxes/MECs
    frame `shpframe' {
        mata: _bbox("`newshpframe'", `"`XY'"', "`touse'", `btype', `n',/*
            */ `padding', "`adjust'"=="", `angle', "`BY'", "`ID'")
        frame `newshpframe' {
            qui compress _ID
            _set_type shape
        }
    }
    frame `newframe' {
        qui geoframe append `newshpframe' _ID if _n==1 | _ID!=_ID[_n-1]
        _set_type attribute
        qui geoframe_link `newshpframe'
        qui geoframe_generate centroids
        qui geoframe_unlink
    }
    // cleanup
    frame `newshpframe' {
        capt confirm new frame `newshpname'
        if _rc==1 exit 1
        if _rc frame drop `newshpname'
        frame rename `newshpframe' `newshpname'
    }
    frame `newframe' {
        capt confirm new frame `newname'
        if _rc==1 exit 1
        if _rc frame drop `newname'
        frame rename `newframe' `newname'
        qui geoframe_link `newshpname'
    }
    _di_frame "(frame " `newname' " created)"
    _di_frame "(frame " `newshpname' " created)"
    _describe_and_make_current `newname' "`current'" nodescribe
end

program geoframe_bshare
    syntax namelist(id="newname" name=newnames max=2) [if] [in] [, noShp/*
        */ NOT UNIQue ENDpoints noDOTs replace CURrent ]
    gettoken newname newnames : newnames
    gettoken newshpname : newnames
    if "`newshpname'"=="" local newshpname "`newname'_shp"
    local rtype `not' `unique' `endpoints'
    if `:list sizeof rtype'>1 {
        di as err "only one of {bf:not}, {bf:unique}, and {bf:endpoints} allowed"
        exit 198
    }
    if "`not'"!=""            local rtype 2
    else if "`unique'"!=""    local rtype 0
    else if "`endpoints'"!="" local rtype 3
    else                      local rtype 1
    if "`replace'"=="" {
        confirm new frame `newname'
        confirm new frame `newshpname'
    }
    // mark sample and find shapes
    marksample touse
    local cframe = c(frame)
    if "`shp'"=="" {
        _get shpframe, local(shpframe)
        if `"`shpframe'"'=="" {
            _get type, local(type)
            if !inlist(`"`type'"',"shape","pc") {
                di as txt "(shape frame not found;"/*
                    */ " treating current frame as shape frame)"
            }
        }
    }
    if `"`shpframe'"'=="" local shpframe `"`cframe'"'
    else _markshapes `shpframe' `touse' `touse'
    // prepare new frame
    tempname newframe newshpframe
    frame create `newframe' double(_ID)
    if `rtype'==0 local idvars _ID1 _ID2
    frame create `newshpframe' double(_ID `idvars' _X _Y)
    // obtain boxes/MECs
    frame `shpframe' {
        _get coordinates, local(XY) strict
        if `: list sizeof XY'==4 {
            di as err "four coordinate variables found in frame"/*
                */ " {bf:`shpframe'}" _n "paired-coordinates data not"/*
                */ " supported by {bf:geoframe bshare}"
            exit 499
        }
        _get id, local(ID)
        mata: _bshare("`newshpframe'", "`ID'", `"`XY'"', "`touse'",/*
            */ `rtype', "`dots'"!="")
        qui compress _ID `idvars'
        _set_type shape
    }
    frame `newframe' {
        qui geoframe append `newshpframe' _ID if _n==1 | _ID!=_ID[_n-1]
        _set_type attribute
        qui geoframe_link `newshpframe'
        qui geoframe_generate centroids
        qui geoframe_unlink
    }
    // cleanup
    frame `newshpframe' {
        capt confirm new frame `newshpname'
        if _rc==1 exit 1
        if _rc frame drop `newshpname'
        frame rename `newshpframe' `newshpname'
    }
    frame `newframe' {
        capt confirm new frame `newname'
        if _rc==1 exit 1
        if _rc frame drop `newname'
        frame rename `newframe' `newname'
        qui geoframe_link `newshpname'
    }
    _di_frame "(frame " `newname' " created)"
    _di_frame "(frame " `newshpname' " created)"
    _describe_and_make_current `newname' "`current'" nodescribe
end

program geoframe_symbol
    syntax namelist(id="newname" name=newnames max=2) [if] [in] [, * ]
    _symbol `newnames' `if' `in', `options'
end

program geoframe_symboli
    syntax anything(id="newname") [, * ]
    gettoken name    anything : anything
    gettoken shpname          : anything
    confirm name `name'
    capt confirm name `shpname'
    if _rc==1 exit _rc
    if _rc local shpname "`name'_shp"           // 2nd element is not a name
    else   gettoken shpname anything : anything // 2nd element is a name
    _symbol `name' `shpname' `anything', _immediate `options'
end

program _symbol
    syntax anything [if] [in] [, _immediate replace/*
        */ SHape(passthru) SIze(passthru) OFFset(passthru) ANGle(passthru)/*
        */ ratio(passthru) n(passthru) CURrent ]
    gettoken newname     anything : anything
    gettoken newshpname  anything : anything
    if "`newshpname'"=="" local newshpname "`newname'_shp"
    if "`replace'"=="" {
        confirm new frame `newname'
        confirm new frame `newshpname'
    }
    local cframe = c(frame)
    // generate symbols
    tempname newframe newshpframe
    if "`_immediate'"!="" {
        if `"`if'"'!="" {
            di as err "if not allowed"
            exit 101
        }
        if `"`in'"'!="" {
            di as err "in not allowed"
            exit 101
        }
        _geoplot_symbol . . `anything', _immediate/*
            */ _frameonly(`newframe' `newshpframe')/*
            */ `shape' `size' `offset' `angle' `ratio' `n'
    }
    else {
        _geoplot_symbol . . `cframe' `if' `in',/*
            */ _frameonly(`newframe' `newshpframe')/*
            */ `shape' `size' `offset' `angle' `ratio' `n'
    }
    // cleanup
    frame `newshpframe' {
        _set_type shape
        capt confirm new frame `newshpname'
        if _rc==1 exit 1
        if _rc frame drop `newshpname'
        frame rename `newshpframe' `newshpname'
    }
    frame `newframe' {
        _set_type attribute
        capt confirm new frame `newname'
        if _rc==1 exit 1
        if _rc frame drop `newname'
        frame rename `newframe' `newname'
        qui geoframe_link `newshpname'
    }
    _di_frame "(frame " `newname' " created)"
    _di_frame "(frame " `newshpname' " created)"
    _describe_and_make_current `newname' "`current'" nodescribe
end

program geoframe_collapse
    _collapse 0 `0'
end

program geoframe_contract
    _collapse 1 `0'
end

program _collapse
    gettoken contract 0 : 0
    local cframe = c(frame)
    gettoken frame 0 : 0, parse(" ,")
    tempname frame1
    frame `frame' {
        local opts COordinates(passthru) SELect(passthru) id(varname)/*
            */ GENerate GENerate2(str) noDOTs relax
        if `contract' {
            syntax [if] [in] [fw] [, `opts' * ]
        }
        else {
            syntax anything(equalok id="clist") [if] [in] [aw fw iw pw] [,/*
                */ `opts' cw ]
        }
        if `"`generate2'"'!="" local generate generate
        if "`id'"!="" { // ID already exists
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
        else { // create ID using spjoin
            // check coordinates
            if `"`coordinates'"'=="" {
                capt geoframe_get coordinates, strict
                if _rc==1 exit 1
                if _rc {
                    di "ccc"
                    di as err "no coordinates found in frame {bf:`frame'}"/*
                        */ "; specify option {bf:coordinates()}"
                    exit 111
                }
            }
            // generate option
            if `"`generate'"'!="" {
                _collapse_parse_gen `generate2' /*
                    returns generate, replace, set, sort*/
                local ID `generate'
                local novarnote
                if "`replace'"=="" {
                    capt confirm new variable `ID'
                    if _rc==1 exit 1
                    if _rc {
                        di as err "variable {bf:`ID'} already defined in"/*
                            */ " frame {bf:`frame'}"
                        exit 110
                    }
                }
            }
            else {
                tempname ID
                local replace
                local set noset
                local sort nosort
                local novarnote novarnote
            }
            // check for potential errors after spatial join
            frame put in 1, into(`frame1')
            frame `frame1' {
                qui gen byte `ID' = 1
                if `contract' contract `ID', `options'
                else          collapse `anything', by(`ID') fast
                    /* weights and option cw omitted intentionally to prevent
                       "no observations" error */
                if "`relax'"=="" {
                    qui ds *
                    local vlist `r(varlist)'
                    local vlist: list vlist - ID
                    frame `cframe' {
                        foreach var of local vlist {
                            capt confirm new variable `var'
                            if _rc==1 exit 1
                            if _rc {
                                di as err "variable {bf:`var'} already defined"/*
                                    */ " in frame {bf:`cframe'}"
                                exit 110
                            }
                        }
                    }
                }
            }
            frame drop `frame1'
            // now run spatial join
            geoframe_spjoin `cframe' `ID' `if' `in', `dots' `select'/*
                */ `coordinates' `replace' `set' `sort' `novarnote'
        }
    }
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
        _get id, local(id)
        if `"`id'"'!="`ID'" geoframe_set id `ID'
    }
    qui geoframe_copy `frame1' *, exclude(`ID') `relax'
    if r(k)==0 {
        _di_frame "(no variables added to frame " `cframe' ")"
        exit
    }
    if r(k)==1 di as txt "(variable " _c
    else       di as txt "(variables " _c
    di as res r(varlist) _c
    _di_frame " added to frame " `cframe' ")"
end

program _collapse_parse_gen
    syntax [name] [, replace noset nosort ]
    if "`namelist'"=="" local namelist _ID // default
    c_local generate `namelist'
    c_local replace `replace'
    c_local set `set'
    c_local sort `sort'
end

program geoframe_spjoin
    syntax [namelist(min=1 max=2)] [if] [in] [, SELect(str asis) /*
        */ COordinates(varlist min=2 max=2) replace nosort noset/*
        */ NOVARNOTE noDOTs ]
    marksample touse
    gettoken shpframe namelist : namelist
    gettoken id       namelist : namelist
    if "`id'"=="" local id _ID
    if "`replace'"=="" confirm new variable `id'
    if `"`coordinates'"'!="" local xy `coordinates'
    else {
        _get coordinates, local(xy) strict
        if `: list sizeof xy'==4 {
            di as err "four coordinate variables found in frame"/*
                */ " {bf:`c(frame)'}" _n "paired-coordinates data not"/*
                */ " supported by {bf:geoframe spjoin}"
            exit 499
        }
    }
    markout `touse' `xy'
    local frame = c(frame)
    local TOUSE
    frame `shpframe' {
        // check whether shpframe is an attribute frame linked to a shape frame
        // and process select()
        _get shpframe, local(shpframe2)
        if `"`shpframe2'"'!="" {
            tempvar TOUSE
            if `"`select'"'!="" {
                qui gen byte `TOUSE' = 0
                qui replace `TOUSE' = 1 if (`select')
            }
            else qui gen byte `TOUSE' = 1
            _markshapes `shpframe2' `TOUSE'
            local shpframe `shpframe2'
        }
        else if `"`select'"'!="" {
            tempvar TOUSE
            qui gen byte `TOUSE' = 0
            qui replace `TOUSE' = 1 if (`select')
        }
    }
    frame `shpframe' {
        _get ID, local(ID) strict
        local type: type `ID'
        _get coordinates, local(XY) strict
        if `: list sizeof XY'==4 {
            di as err "four coordinate variables found in frame"/*
                */ " {bf:`shpframe'}" _n "paired-coordinates data not"/*
                */ " supported by {bf:geoframe spjoin}"
            exit 499
        }
        _get pid, local(PID)
        if "`PID'"=="" {
            tempvar PID
            qui geoframe_generate pid `PID', noset
        }
        _get pl, local(PL)
        if "`PL'"=="" {
            di as txt "({helpb geoframe##gen_plevel:plevel} not set;"/*
                */ " assuming that there are no nested items)"
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
    if "`sort'"=="" {
        tempvar sortindex
        qui gen double `sortindex' = _n
        sort `id' `sortindex'
    }
    if "`set'"=="" {
        geoframe_set id `id'
    }
    if "`novarnote'"=="" {
        _di_frame "(variable {bf:`id'} added to frame " `frame' ")"
        if "`sort'"=="" {
            _di_frame "(data in frame " `frame' " sorted by {bf:`id'})"
        }
    }
end

program geoframe_copy, rclass
    // syntax
    gettoken frame2 0 : 0, parse(" ,")
    local frame = c(frame)
    if `"`frame2'"'==`"`frame'"' {
        di as err "{it:frame2} must be different from current frame"
        exit 498
    }
    confirm frame `frame2'
    frame `frame2' {
        syntax [varlist(default=none)] [if] [in] [, EXclude(varlist)/*
            */ TARget(namelist) id(namelist max=2) UNIQue relax ]
        local hasIF = `"`if'`in'"'!=""
        local varlist: list varlist - exclude
        _get id, local(id2)
        if !`hasIF' {
            _get shpframe, local(shpframe2)
            if `"`shpframe2'"'!=`"`frame'"' local shpframe2
        }
        gettoken ID  id : id
        gettoken ID2 id : id
        if "`ID2'"=="" local ID2 `ID'
        if "`ID2'"!="" confirm variable `ID2'
        else           local ID2 `id2'
        if "`ID2'"=="" {
            di as err "no ID variable found in frame {bf:`frame2'}"
            exit 111
        }
    }
    _get id, local(id)
    if !`hasIF' {
        _get shpframe, local(shpframe)
        if `"`shpframe'"'!=`"`frame2'"' local shpframe
        else if "`unique'"!=""          local shpframe
    }
    if "`ID'"!="" confirm variable `ID'
    else          local ID `id'
    if "`ID'"=="" {
        di as err "no ID variable found in the current frame"
        exit 111
    }
    // prepare varlist
    local tgtvlist
    foreach var of local varlist {
        gettoken TGT target : target
        if "`TGT'"=="" local TGT `var'
        capt confirm new variable `TGT'
        if _rc==1 exit 1
        else if _rc {
            local msg "variable {bf:`TGT'} already defined in frame {bf:`frame'}"
            if "`relax'"=="" {
                di as err "`msg'"
                exit 110
            }
            else {
                di as txt "(`msg'; {bf:`var'} will not be copied)"
                continue
            }
        }
        local tgtvlist `tgtvlist' `TGT'
        // use matched list so that variables starting with "_" are copied
        local vlist `vlist' `TGT' = `var'
    }
    if `: list sizeof tgtvlist'==0 {
        di as txt "(no variables to copy)"
        return scalar k = 0
        exit
    }
    // try existing link
    if "`ID'"=="`id'" & "`ID2'"=="`id2'" {
        if "`shpframe2'"!="" { // attribute -> shp
            frame `frame2': _get linkname, local(lnkvar2)
            if `"`lnkvar2'"'!="" {
                capt frget `vlist', from(`lnkvar2')
                if _rc==1 exit 1
                if _rc==0 {
                    local k = r(k)
                    if c(noisily) {
                        _copy_Nmis `ID' `lnkvar2'
                        _copy_di `frame2' `k' `Nmis' 0
                    }
                    return scalar k = `k'
                    exit
                }
            }
            local lnkvar2
        }
        if "`shpframe'"!="" { // shp -> attribute
            _get linkname, local(lnkvar)
            if `"`lnkvar'"'!="" {
                frame `frame2' {
                    tempvar merge
                    qui gen byte `merge' = 1
                }
                capt _copy_from_shpframe `merge' `varlist', id(`ID')/*
                    */ shpid(`ID2') shpframe(`frame2') lnkvar(`lnkvar')/*
                    */ vlist(`merge' = `merge' `vlist')
                if _rc==1 exit 1
                if _rc==0 {
                    local k = r(k) - 1
                    if c(noisily) {
                        qui count if `merge'>=.
                        _copy_di `frame2' `k' `r(N)' 1
                    }
                    return scalar k = `k'
                    exit
                }
            }
            local lnkvar2
        }
        if "`shpframe2'`shpframe'"!="" {
            di as txt "(existing link failed; trying to link on the fly)"
        }
    }
    // apply if/in
    if `hasIF' {
        tempname FRAME2
        frame `frame2': frame put `if' `in', into(`FRAME2')
    }
    else local FRAME2 `frame2'
    // cases
    capt isid `ID', missok
    if _rc==1 exit 1
    local uniq = _rc==0
    frame `FRAME2' {
        capt isid `ID2', missok
        if _rc==1 exit 1
        local uniq2 = _rc==0
        if !`uniq2' & "`unique'"!="" {
            di as err "units in frame {bf:`frame2'} not unique"
            exit 9
        }
    }
    // copy
    local firstobs 0
    tempvar lnkvar
    if `uniq' & `uniq2' { // attribute -> attribute
        qui frlink 1:1 `ID', frame(`FRAME2' `ID2') generate(`lnkvar')
        qui frget `vlist', from(`lnkvar')
        local k = r(k)
        if c(noisily) {
            qui count if `lnkvar'>=.
            local Nmis = r(N)
        }
    }
    else if `uniq2' { // attribute -> shp
        qui frlink m:1 `ID', frame(`FRAME2' `ID2') generate(`lnkvar')
        qui frget `vlist', from(`lnkvar')
        local k = r(k)
        if c(noisily) {
            qui count if (_n==1 | `ID'!=`ID'[_n-1]) & `lnkvar'>=.
            local Nmis = r(N)
        }
    }
    else if `uniq' { // shp -> attribute
        frame `FRAME2' {
            tempvar merge
            qui gen byte `merge' = 1
            qui frlink m:1 `ID2', frame(`frame' `ID') generate(`lnkvar')
        }
        _copy_from_shpframe `merge' `varlist', id(`ID')/*
            */ shpid(`ID2') shpframe(`FRAME2') lnkvar(`lnkvar')/*
            */ vlist(`merge' = `merge' `vlist')
        local k = r(k) - 1
        if c(noisily) {
            qui count if `merge'>=.
            local Nmis = r(N)
        }
        local firstobs 1
    }
    else { // shp -> shp
        // step 1: create tempframe with one obs per ID and link to frame2
        tempname tmpframe tag
        qui gen byte `tag' = _n==1 | `ID'!=`ID'[_n-1] // tag 1st obs per unit
        frame put `ID' `tag' if `tag', into(`tmpframe')
        frame `FRAME2' {
            tempvar merge
            qui gen byte `merge' = 1
            capt frlink m:1 `ID2', frame(`tmpframe' `ID') generate(`lnkvar')
            if _rc==1 exit 1
            if _rc { // current frame may not be ordered; select 1st obs per ID
                frame `tmpframe' {
                    mata: st_store(., "`tag'", mm_unique_tag(st_data(.,"`ID'"), 1))
                    qui keep if `tag'
                }
                qui frlink m:1 `ID2', frame(`tmpframe' `ID') generate(`lnkvar')
            }
        }
        // step 2: copy variables from frame2 into tempframe
        frame `tmpframe' {
            _copy_from_shpframe `merge' `varlist', id(`ID')/*
                */ shpid(`ID2') shpframe(`FRAME2') lnkvar(`lnkvar')
            if c(noisily) {
                qui count if `merge'>=.
                local Nmis = r(N)
            }
        }
        // step 3: link tmpframe to current frame and copy variables
        qui frlink m:1 `ID', frame(`tmpframe' `ID') generate(`lnkvar')
        qui frget `vlist', from(`lnkvar')
        local k = r(k)
        foreach var of local tgtvlist {
            qui replace `var' = `var'[_n-1] if `tag'==0
        }
        local firstobs 1
    }
    if c(noisily) {
        _copy_di `frame2' `k' `Nmis' `firstobs'
    }
    return scalar k = `k'
    return local varlist "`tgtvlist'"
end

program _copy_Nmis
    gettoken id 0 : 0
    gettoken lnkvar 0 : 0
    tempname touse nmis
    qui gen byte `touse' = `lnkvar'>=.
    mata: st_numscalar("`nmis'", mm_nunique(st_data(., "`id'" , "`touse'")))
    c_local Nmis = `nmis'
end

program _copy_di
    args frame k Nmis firstobs
    if "`Nmis'"=="0"/*
        */ _di_frame "(all units in frame " `c(frame)' " matched)"
    else if "`Nmis'"=="1"/*
        */ _di_frame "(1 unit in frame " `c(frame)' " unmatched)"
    else   _di_frame "(`Nmis' units in frame " `c(frame)' " unmatched)"
    if `k'==1 local msg variable
    else      local msg variables
    if `firstobs' local fmsg " using first observation per unit"
    else          local fmsg ""
    _di_frame "(`k' `msg' copied from frame " `frame' "`fmsg')"
end

program _copy_from_shpframe
    _parse comma vars 0 : 0
    syntax [, id(str) shpid(str) shpframe(str) lnkvar(str) vlist(str) ]
    if !`:list sizeof vars' exit
    tempname tmpframe
    frame `shpframe': frame put `shpid' `vars' if/*
        */ `lnkvar'<. & (_n==1 | `shpid'!=`shpid'[_n-1]), into(`tmpframe')
    capt frlink 1:1 `id', frame(`tmpframe' `shpid')
    if _rc==1 exit 1
    if _rc { // shpframe may not be ordered; select 1st obs per ID
        frame `tmpframe' {
            tempvar touse
            mata: st_store(., st_addvar("byte", "`touse'"),/*
                */ mm_unique_tag(st_data(.,"`shpid'"), 1))
            qui keep if `touse'
        }
        qui frlink 1:1 `id', frame(`tmpframe' `shpid')
    }
    if `"`vlist'"'=="" {
        foreach v of local vars {
            local vlist `vlist' `v' = `v'
        }
    }
    qui frget `vlist', from(`tmpframe')
end

program geoframe_append
    // syntax and sample
    gettoken frame 0 : 0, parse(" ,")
    confirm frame `frame'
    frame `frame' {
        syntax [varlist] [if] [in] [, EXclude(varlist)/*
            */ TARget(namelist) touse(varname numeric)/*
            */ force raw fast ]
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
        local fmts
        foreach v of local varlist {
            local types `types' `:type `v''
            local fmts `fmts' `:format `v''
        }
    }
    if `: list sizeof varlist'==0 {
        di as txt "(no variables to append)"
        exit
    }
    // prepare variables and data types
    local addvars
    local recast
    local ORG
    local TGT
    foreach v of local varlist {
        gettoken type types : types
        gettoken fmt fmts : fmts
        gettoken V target : target
        if `"`V'"'=="" local V `v'
        // append to new variable
        capt unab V : `V'
        if _rc==1 exit _rc
        if _rc { 
            confirm name `V'
            local addvars `addvars' `type' `fmt' `V'
            local ORG `ORG' `v'
            local TGT `TGT' `V'
            continue
        }
        // append to existing variable
        local TGT `TGT' `V'
        if c(stata_version)>=18 {
            if `: isalias `V'' {
                di as err "`V' is an alias; may not append to alias variables"
                exit 498
            }
        }
        local TYPE: type `V'
        capt n _append_check_types `type' `TYPE' // may update type
        if _rc==109 {
            if "`force'"=="" {
                di as err "type mismatch for {bf:`v'};"/*
                    */ " may not combine numeric and string"
                exit 109 // type mismatch
            }
            frame `frame' {
                tempvar tmp
                local ORG `ORG' `tmp'
                if substr("`type'",1,3)=="str" {
                    qui gen double `tmp' = .
                    qui replace `tmp' = real(`v')
                    qui recast `TYPE' `tmp' // changes type only if possible
                }
                else {
                    qui gen `TYPE' `tmp' = ""
                    qui replace `tmp' = strofreal(`v',"%12.0g")
                }
                local type: type `tmp'
                if "`type'"!="`TYPE'" {
                    local recast `recast' `type' `V'
                }
            }
        }
        else if _rc==499 {
            local ORG `ORG' `v'
            local recast `recast' `type' `V'
        }
        else if _rc exit _rc
        else {
            local ORG `ORG' `v'
        }
    }
    local dups: list dups TGT
    if `"`dups'"'!="" {
        di as err "duplicates not allowed in list of target variables"
        exit 198
    }
    // append data
    if "`fast'"=="" preserve
    local K = c(k)
    while (`"`addvars'"'!="") {
        gettoken type addvars : addvars
        gettoken fmt  addvars : addvars
        gettoken V    addvars : addvars
        mata: (void) st_addvar("`type'", "`V'")
        if "`raw'"=="" format `fmt' `V'
    }
    while (`"`recast'"'!="") {
        gettoken type recast : recast
        gettoken V    recast : recast
        recast `type' `V'
    }
    local N = _N
    local cframe = c(frame)
    capt n mata: _append("`frame'", "`touse'",/*
        */ tokens(st_local("ORG")), tokens(st_local("TGT")))
    if _rc {
        frame change `cframe'
        exit _rc
    }
    // copy labels
    if "`raw'"=="" {
        mata: _append_labels("`frame'", /*
            */ tokens(st_local("ORG")), tokens(st_local("TGT")))
    }
    // display
    local N = _N - `N'
    di as txt "(`N' observations appended)"
    local K = c(k) - `K'
    if `K' di as txt "(`K' new variables created)"
    if "`fast'"=="" restore, not
end

program _append_check_types
    args type TYPE
    if "`type'"=="`TYPE'" exit
    local str = substr("`type'",1,3)=="str"
    local STR = substr("`TYPE'",1,3)=="str"
    if `str'!=`STR' {
        exit 109 // type mismatch
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

program geoframe_stack
    // syntax
    syntax namelist(id="framelist" min=2) [,/*
        */ into(namelist max=2) NOSHP drop CURrent replace force ]
    if "`into'"=="" {
        if "`replace'"=="" {
            di as err "option {bf:replace} is required"/*
                */ " if {bf:into()} is omitted"
            exit 198
        }
    }
    // check frames and look for shape frames
    local hasSHP 0
    foreach frame of local namelist {
        confirm frame `frame'
        if "`noshp'"=="" {
            frame `frame': _get shpframe, local(shpframe)
            capt confirm frame `shpframe'
            if _rc==1 exit _rc
            if _rc==0 local hasSHP 1
        }
    }
    // determine target names
    if "`into'"!="" {
        gettoken tgtframe into : into
        if `hasSHP' {
            gettoken tgtshpframe into : into
            if "`tgtshpframe'"=="" local tgtshpframe `tgtframe'_shp
        }
    }
    else {
        gettoken tgtframe : namelist
        if `hasSHP' {
            frame `tgtframe': _get shpframe, local(tgtshpframe)
            if "`tgtshpframe'"=="" local tgtshpframe `tgtframe'_shp
        }
    }
    if "`replace'"=="" {
        foreach frame in `tgtframe' `tgtshpframe' {
            confirm new frame `frame'
        }
    }
    tempname tmpframe tmpshpframe
    // stack frames
    local n1 0
    local s1 0
    local idmax 0
    local first 1
    local firstshp 1
    local hasshp 0
    tempvar ID
    foreach frame of local namelist {
        local n0 = `n1' + 1
        local s0 = `s1' + 1
        frame `frame' {
            if `hasSHP' {
                _get shpframe, local(shpframe)
                local hasshp = `"`shpframe'"'!=""
            }
            else local hasshp 0
            if `hasshp' _get id, local(id) strict
            else        _get id, local(id)
        }
        if `first' {
            frame copy `frame' `tmpframe'
            frame `tmpframe' {
                local n1 = _N
                qui gen byte `ID' = .
                if `"`id'"'!="" {
                    qui replace `ID' = `id'
                    if `"`id'"'!="_ID" drop `id'
                }
                else qui replace `ID' = _n
                capt confirm new variable _FRAME
                if _rc==1 exit 1
                if _rc drop _FRAME
                qui gen str _FRAME = "`frame'"
            }
            local first 0
        }
        else {
            frame `tmpframe': qui geoframe append `frame', fast `force'
            frame `tmpframe' {
                local n1 = _N
                if `n1'>=`n0' {
                    if `"`id'"'!="" {
                        qui replace `ID' = `id' + `idmax' in `n0'/l
                        if `"`id'"'!="_ID" drop `id'
                    }
                    else qui replace `ID' = _n - (`n0' + `idmax') in `n0'/l
                    qui replace _FRAME = "`frame'" in `n0'/l
                }
            }
        }
        if `hasshp' {
            if `firstshp' {
                frame copy `shpframe' `tmpshpframe'
                frame `shpframe': _get id, local(id) strict
                frame `tmpshpframe' {
                    local s1 = _N
                    qui gen byte `ID' = .
                    qui replace `ID' = `id'
                    if `"`id'"'!="_ID" drop `id'
                    capt confirm new variable _FRAME
                    if _rc==1 exit 1
                    if _rc drop _FRAME
                    qui gen str _FRAME = "`shpframe'"
                    capt drop _GEOFRAME_lnkvar_*
                }
                local firstshp 0
            }
            else {
                frame `tmpshpframe': qui geoframe append `shpframe', fast/*
                    */ `force'
                frame `shpframe': _get id, local(id) strict
                frame `tmpshpframe' {
                    local s1 = _N
                    if `s1'>=`s0' {
                        qui replace `ID' = `id' + `idmax' in `s0'/l
                        if `"`id'"'!="_ID" drop `id'
                        qui replace _FRAME = "`shpframe'" in `s0'/l
                    }
                    capt drop _GEOFRAME_lnkvar_*
                }
            }
        }
        if `n1'>=`n0' {
            frame `tmpframe' {
                su `ID' in `n0'/l, meanonly
                local idmax = max(`idmax', r(max))
            }
        }
        if `hasshp' {
            if `s1'>=`s0' {
                frame `tmpshpframe' {
                    su `ID' in `s0'/l, meanonly
                    local idmax = max(`idmax', r(max))
                }
            }
        }
    }
    // rename ID
    frame `tmpframe' {
        capt confirm new variable _ID
        if _rc==1 exit 1
        if _rc drop _ID
        rename `ID' _ID
        order _FRAME _ID
        geoframe_set id _ID
        _set_shpframe
        _set_linkname
    }
    if `hasSHP' {
        frame `tmpshpframe' {
            capt confirm new variable _ID
            if _rc==1 exit 1
            if _rc drop _ID
            rename `ID' _ID
            order _FRAME _ID
            geoframe_set id _ID
        }
    }
    // rename frames and link
    nobreak {
        capt confirm new frame `tgtframe'
        if _rc frame drop `tgtframe'
        if `hasSHP' {
            capt confirm new frame `tgtshpframe'
            if _rc frame drop `tgtshpframe'
        }
        frame rename `tmpframe' `tgtframe'
        _di_frame "(frame " `tgtframe' " created)"
        if `hasSHP' {
            frame rename `tmpshpframe' `tgtshpframe'
            _di_frame "(frame " `tgtshpframe' " created)"
            frame `tgtframe': geoframe_link `tgtshpframe'
        }
    }
    _describe_and_make_current `tgtframe' "`current'" nodescribe
    // drop frames
    nobreak {
        if "`drop'"!="" {
            local cframe = c(frame)
            local DROP
            local namelist: list uniq namelist
            foreach frame of local namelist {
                if `hasSHP' {
                    frame `frame': _get shpframe, local(shpframe)
                    if `"`frame'"'=="`cframe'" {
                        di as txt "(frame {bf:`frame'} not dropped;"/*
                            */ " may not drop current frame)"
                    }
                    else if `"`frame'"'!="`tgtframe'" {
                        capt frame drop `frame'
                        if _rc==0 local DROP `DROP' `frame'
                    }
                    if `"`shpframe'"'!="" {
                        if `"`shpframe'"'=="`cframe'" {
                            di as txt "(frame {bf:`shpframe'} not dropped;"/*
                                */ " may not drop current frame)"
                        }
                        else if `"`shpframe'"'!="`tgtshpframe'" {
                            capt frame drop `shpframe'
                            if _rc==0 local DROP `DROP' `shpframe'
                        }
                    }
                    
                }
            }
            di as txt "(dropped frames: {bf:`DROP'})"
        }
    }
end

program geoframe_rename
    local cframe= c(frame)
    syntax namelist(id="newname" name=newnames max=2) [, noShp replace ]
    gettoken newname newnames : newnames
    gettoken newshpname : newnames
    if "`newshpname'"=="" local newshpname "`newname'_shp"
    if "`shp'"=="" _get shpframe, local(shpframe)
    if "`shpframe'"!="" {
        if "`newname'"=="`newshpname'" {
            di as txt "{it:newname} and {it:newshpname} may not be equal"
            exit 198
        }
        if "`newname'"=="`cframe'" & "`newshpname'"=="`shpframe'" {
            di as txt "({it:newname} and {it:newshpname} equal to current"/*
                */ " names; nothing to do)"
            exit
        }
        if "`replace'"=="" {
            confirm new frame `newname'
            confirm new frame `newshpname'
        }
        nobreak {
            qui geoframe unlink
            if "`newshpname'"=="`cframe'" {
                tempname tname
                frame rename `cframe' `tname'
            }
            else local tname `cframe'
            if "`newname'"=="`shpframe'" {
                tempname tshpname
                frame rename `shpframe' `tshpname'
            }
            else local tshpname `shpframe'
            if "`newname'"!="`tname'" {
                _geoframe_rename `cframe' `tname' `newname'
            }
            if "`newshpname'"!="`tshpname'" {
                _geoframe_rename `shpframe' `tshpname' `newshpname'
            }
            frame `newname': qui geoframe_link `newshpname'
        }
    }
    else {
        if "`newname'"=="`cframe'" {
            di as txt "({it:newname} equal to current name; nothing to do)"
            exit
        }
        if "`replace'"=="" confirm new frame `newname'
        nobreak _geoframe_rename `cframe' `cframe' `newname'
    }
end

program _geoframe_rename
    args lbl cname newname
    capt confirm new frame `newname'
    if _rc frame drop `newname'
    frame rename `cname' `newname'
    _di_frame "(frame {bf:`lbl'} renamed to " `newname' ")"
    frame `newname' {
        // update possible outgoing link
        _get shpframe, local(shpframe)
        if `"`shpframe'"'!="" {
            local rc = `"`shpframe'"'=="`newname'"
            if `rc'==0 {
                capt confirm frame `shpframe'
                if _rc==1 exit 1
                if _rc local rc 1
            }
            if `rc' {
                _set_shpframe
                _set_linkname
                di as txt "(frame {bf:`shpframe'} no longer exists;"/*
                    */ " link removed)"
            }
            else {
                qui geoframe_link `shpframe'
                _di_frame "(link to frame " `shpframe' " updated)"
            }
        }
        // update possible incoming links
        capt unab lnkvars: _GEOFRAME_lnkvar_*
        foreach lnkvar of local lnkvars {
            local aframe: char `lnkvar'[frlink_fname2]
            local rc = `"`aframe'"'=="`newname'"
            if `rc'==0 {
                capt confirm frame `aframe'
                if _rc==1 exit
                if _rc local rc 1
            }
            if `rc' {
                drop `lnkvar'
                di as txt "(frame {bf:`aframe'} no longer exists;"/*
                    */ " link removed)"
                continue
            }
            frame `aframe' {
                _get shpframe, local(shpframe)
                if `"`shpframe'"'!="`cname'" continue // keep linkvar
                _set_shpframe `newname'
                _set_linkname `lnkvar'
                _di_frame "(link from frame " `aframe' " updated)"
            }
        }
    }
end

program geoframe_duplicate
    syntax namelist(id="newname" name=newname) [,/*
        */ NOShp UNLink replace/*
        */ noDEScribe/* discontinued
        */ CURrent ]
    geoframe_select, into(`newname') `noshp' `unlink' `replace' `current'
end

program geoframe_link
    _get type, local(type)
    if inlist(`"`type'"',"shape","pc") {
        di as err `"current frame is of type {bf:`type'}"'/*
            */ "; {bf:geoframe link} not allowed"
        exit 498
    }
    local frame = c(frame)
    syntax name(id="shape frame" name=shpframe) [, quietly CLean CLean2(str) ]
    if `"`shpframe'"'==`"`frame'"' {
        di as err "{it:shpframe} must be different from current frame"
        exit 498
    }
    confirm frame `shpframe'
    _get shpframe, local(SHPFRAME)
    if `"`SHPFRAME'"'!="" geoframe_unlink // clear existing link
    frame `shpframe' {
        _get id, local(shpid)
        if `"`shpid'"'=="" {
            di as err "no ID variable defined in {it:shpframe}"
            exit 498
        }
        // create name for linkage variable
        local droplnkvar 0
        local i 1
        while (1) {
            local lnkvar _GEOFRAME_lnkvar_`i'
            capt confirm new variable `lnkvar'
            if _rc==1 exit _rc
            if _rc {
                // check whether existing linkage variable is valid
                local aframe: char `lnkvar'[frlink_fname2]
                capt confirm frame `aframe'
                if _rc==1 exit
                if _rc { // attribute frame does not exist; ok to replace lnkvar
                    local droplnkvar 1
                    continue, break
                }
                frame `aframe' {
                    _get shpframe, local(SHPFRAME)
                    if `"`SHPFRAME'"'!=`"`shpframe'"' { // attribute frame is
                        // linked to different shape frame; ok to replace lnkvar
                        local droplnkvar 1
                        continue, break
                    }
                }
                local ++i // name already exists; try next one
            }
            else continue, break // name ok
        }
    }
    _get id, local(id)
    if `"`id'"'=="" {
        di as err "no ID variable defined in current frame"
        exit 498
    }
    confirm variable `id' // error if ID variable does not exist
    _set_shpframe `shpframe'
    capt isid `id'
    if _rc==1 exit 1
    if _rc    local nolnkvar 1
    else {
        frame `shpframe' {
            if `droplnkvar' drop `lnkvar'
            qui frlink m:1 `shpid', frame(`frame' `id') generate(`lnkvar')
        }
        _set_linkname `lnkvar'
        local nolnkvar 0
    }
    `quietly' _di_frame "(link to frame " `shpframe' " added)"
    if `nolnkvar' {
        di as txt "(ID in attribute frame not unique; " /*
            */ "could not create permanent linkage variable)"
    }
    if `"`clean'`clean2'"'!="" {
        geoframe_clean, quietly `clean2'
    }
end

program geoframe_relink
    _get shpframe, local(shpframe)
    if `"`shpframe'"'=="" {
        di as txt "(no link to shape frame found)"
        exit
    }
    geoframe_link `shpframe'
end

program geoframe_unlink
    _get shpframe, local(shpframe)
    _get linkname, local(lnkvar)
    if `"`shpframe'"'=="" {
        di as txt "(no link to shape frame found)"
        exit
    }
    _di_frame "(removing link to frame " `shpframe' ")"
    if `"`lnkvar'"'!="" {
        capt confirm frame `shpframe'
        if _rc==1 exit 1
        if _rc==0 {
            frame `shpframe': capt drop `lnkvar'
        }
    }
    _set_shpframe
    _set_linkname
end

program geoframe_clean
    local frame = c(frame)
    syntax [, Shapes shp Units NOEmpty Emptyonly quietly ] // quietly undocu
    if "`shp'"!="" local shapes shapes
    if "`shapes'`units'"=="" {
        local shapes shapes
        local units units
    }
    _get shpframe, local(shpframe)
    if `"`shpframe'"'=="" {
        di as txt "(nothing to do; no link to shape frame found)"
        exit
    }
    _get id, local(id) strict
    local lnkupdate 0
    // identify linked shapes and empty shapes
    tempvar touse
    _markshapes `shpframe' `touse'
    frame `shpframe' {
        if "`noempty'"=="" {
            tempvar empty
            qui gen byte `empty' = 0
            _get coordinates, local(XY)
            if `"`XY'"'!="" {
                _get id, local(shpid) strict
                mata: _clean_empty("`empty'", "`shpid'", tokens("`XY'"),/*
                    */ "`touse'") // => Nempty
            }
            else local Nempty 0
         }
    }
    // copy linked shapes and empty shapes identifiers back to attribute file
    if "`units'"!="" | "`noempty'"=="" {
        qui geoframe_copy `shpframe' `touse' `empty'
    }
    nobreak {
        // drop obs from shape file
        if "`shapes'"!="" & "`emptyonly'"=="" {
            frame `shpframe' {
                qui keep if `touse'==1
                if r(N_drop) {
                    if r(N_drop)==1 local msg observation
                    else            local msg observations
                    local Ndrop `: di %9.0gc `r(N_drop)''
                    _di_frame "(dropped `Ndrop' unmatched `msg' in frame "/*
                        */ `shpframe' ")"
                }
                else {
                    `quietly' _di_frame /*
                        */ "(no unmatched observations in frame " `shpframe' ")"
                }
            }
        }
        // drop obs from attribute file
        if "`units'"!="" & "`emptyonly'"=="" {
            qui keep if `touse'==1
            if r(N_drop) {
                local lnkupdate 1
                if r(N_drop)==1 local msg observation
                else            local msg observations
                local Ndrop `: di %9.0gc `r(N_drop)''
                _di_frame "(dropped `Ndrop' unmatched `msg' in frame "/*
                    */ `frame' ")"
            }
            else {
                `quietly' _di_frame /*
                    */ "(no unmatched observations in frame " `frame' ")"
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
                        _di_frame "(dropped `Nempty' empty `msg' in frame "/*
                            */ `shpframe' ")"
                    }
                }
                if "`units'"!="" {
                    qui drop if `empty'==1
                    if r(N_drop)==1 local msg unit
                    else            local msg units
                    local Ndrop `: di %9.0gc `r(N_drop)''
                    _di_frame "(dropped `Ndrop' empty-shape `msg' in frame "/*
                        */ `frame' ")"
                }
                local lnkupdate 1
            }
            else {
                `quietly' di as txt "(no empty shapes)"
            }
        }
        if `lnkupdate' qui geoframe_link `shpframe'
    }
end

program geoframe_attach
    if c(stata_version)<18 {
        di as err "{bf:geoframe attach} requires Stata 18"
        exit 9
    }
    gettoken frame 0 : 0, parse(" ,")
    if `"`frame'"'==`"`c(frame)'"' {
        di as err "{it:attributeframe} must be different from current frame"
        exit 498
    }
    confirm frame `frame'
    frame `frame' {
        syntax [varlist] [, EXclude(varlist) ]
        _get id, local(id)
        if `"`id'"'=="" {
            di as err "no ID variable defined in {it:attributeframe}"
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
    _get id, local(id0)
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

program geoframe_detach
    if c(stata_version)<18 {
        di as err "{bf:geoframe detach} requires Stata 18"
        exit 9
    }
    syntax name(name=frame id="attributeframe")
    if "`frame'"==`"`c(frame)'"' {
        di as err "{it:attributeframe} must be different from current frame"
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

program geoframe_translate
    // parse subcommand
    gettoken subcmd : 0, parse(" ,")
    local subcmd = strlower(`"`subcmd'"')
    if      `"`subcmd'"'=="esri"    local subcmd esri
    else if `"`subcmd'"'=="shp"     local subcmd esri
    else if `"`subcmd'"'=="json"    local subcmd json
    else if `"`subcmd'"'=="geojson" local subcmd json
    else if `"`subcmd'"'=="wkt"     local subcmd wkt
    else                            local subcmd
    if "`subcmd'"!="" gettoken subcmd 0 : 0, parse(" ,")
    // wkt
    if "`subcmd'"=="wkt" {
        __translate_`subcmd' `macval(0)'
        exit
    }
    // other translators: parse [<destination>] [using] <using> [, <options>]
    _syntax_add_using `macval(0)'
    syntax [anything] using/ [, * ]
    _translate_toframe_check *, `options'
    // check for zipfile
    mata: _translate_zipname(st_local("using"))
        /*  if the path contains a zip file, the following will be returned:
                zipfile: path and name of zipfile
                shpfile: internal path/name, if specified
             e.g. "path/source.zip/location/name.shp" would be returned as
                zipfile: "path/source.zip"
                shpfile: "location/name.shp"
            if path contains no zip file, zipfile will be empty
        */
    // translate from zipfile
    if `"`zipfile'"'!="" {
        _translate_zip "`subcmd'" `"`anything'"' `"`zipfile'"' `"`shpfile'"'/*
            */, `options'
    }
    // translate regular file
    else {
        mata: _translate_findsource("using", "`subcmd'") /* updates subcmd and
            returns (path and) filename including suffix in using*/
        __translate_`subcmd' `"`anything'"' `"`using'"', `options'
    }
end

program geoframe_import
    _parse comma lhs 0 : 0
    syntax [, TOFrame * ]
    geoframe_translate `macval(lhs)', toframe `macval(options)'
end

program _translate_zip
    _parse comma lhs 0 : 0
    gettoken subcmd   lhs : lhs
    gettoken anything lhs : lhs
    gettoken zipfile  lhs : lhs
    gettoken shpfile      : lhs
    // make path of zipfile absolute
    local zipfile0 `"`zipfile'"'
    mata: !pathisabs(st_local("zipfile"))/*
        */ ? st_local("zipfile", pathjoin(pwd(), st_local("zipfile")))/*
        */ : J(0,0,.)
    // extract and convert
    local pwd `"`c(pwd)'"'
    nobreak {
        tempfile TMPDIR
        mkdir `"`TMPDIR'"'
        capture noisily break {
            qui cd `"`TMPDIR'"'
            qui unzipfile `"`zipfile'"', replace
            mata: _translate_findsource("shpfile", "`subcmd'",/*
                */ st_local("zipfile0")) /* updates subcmd and returns (path
                   and) filename including suffix in shpfile*/
            mata: st_local("shpfile", pathjoin("`TMPDIR'", st_local("shpfile")))
            qui cd `"`pwd'"'
            __translate_`subcmd' `"`anything'"' `"`shpfile'"' `0'
        }
        local rc = _rc
        qui cd `"`pwd'"'
        mata: _translate_zip_cleanup(st_local("TMPDIR"))
        exit `rc'
    }
end

program _translate_toframe_check
    _parse comma extra 0 : 0
    syntax [, TOFrame Feature(passthru) nodrop noCLean noDEScribe CURrent/*
        */ `extra' ]
    if "`toframe'"=="" {
        if `"`feature()'"'!="" {
            di as err "option {bf:feature()} only allowed together with"/*
                */ " {bf:toframe}"
            exit 198
        }
        foreach opt in drop clean describe current {
            if "``opt''"!="" {
                di as err "option {bf:``opt''} only allowed together with"/*
                    */ " {bf:toframe}"
                exit 198
            }
        }
    }
end

program _translate_toframe
    syntax anything [, replace Feature(str asis) nodrop noCLean noDEScribe/*
        */ CURrent ]
    gettoken target   anything : anything
    gettoken frame    anything : anything
    gettoken shpframe anything : anything
    // attribute frame
    frame `frame' {
        _di_frame "(creating frame " `target' ")"
        geoframe_set type attribute
        geoframe_set feature `feature'
        if "`replace'"!="" {
            capt confirm new frame `target'
            if _rc==1 exit _rc
            else if _rc frame drop `target'
        }
        frame rename `frame' `target'
        local frame `target'
    }
    // shape frame
    frame `shpframe' {
        _di_frame "(creating frame " `target'_shp ")"
        geoframe_set type shape
        if "`replace'"!="" {
            capt confirm new frame `target'_shp
            if _rc==1 exit _rc
            else if _rc frame drop `target'_shp
        }
        frame rename `shpframe' `target'_shp
        local shpframe `target'_shp
    }
    // link frames
    if "`drop'`clean"!="" local clean
    else                  local clean clean
    frame `frame': geoframe_link `shpframe', quietly `clean'
    // reporting and frame change
    _describe_and_make_current `frame' "`current'" "`describe'"
end

program _translate_save
    args target frame shpframe replace
    // save
    frame `frame':    qui save `"`target'.dta"', `replace'
    frame `shpframe': qui save `"`target'_shp.dta"', `replace'
    di as txt `"(files {bf:`target'.dta} and {bf:`target'_shp.dta} saved)"'
    // display geoframe create message (add quotes if needed)
    local 0 `"`"`0'"'"'
    local 0: list clean 0
    di as txt `"(type {bf:{stata geoframe create `target'}}"'/*
        */ " to load the data)"
end

program _translate_geom
    gettoken frame  0 : 0
    gettoken target 0 : 0, parse(" ,")
    frame `frame' {
        syntax varname(string) [if] [in] [,/*
            */ wkt GType(name) ADDMISsing noDOTs replace toframe * ]
        local geom `varlist'
        // prepare data
        if `"`if'`in'"'!="" qui keep `if' `in'
        if _N==0 {
            di as txt "(no observations; nothing to do)"
            exit
        }
        if inlist("`geom'","_ID","_CX","_CX", "`gtype'") {
            tempvar GEOM
            rename `geom' `GEOM'
            local geom `GEOM'
        }
        foreach v in _ID _CX _CY `gtype' {
            capt confirm new variable `v'
            if _rc==1 exit 1
            if _rc {
                di as txt "(replacing variable `v')"
                drop `v'
            }
        }
        qui gen byte _ID = .
        qui replace _ID = _n
        qui gen double _CX = .
        qui gen double _CY = .
        if "`gtype'"!="" qui gen str18 `gtype' = ""
        di as txt "(number of units = " _N ")"
        // process geometries
        tempname shpframe
        frame create `shpframe' `:type _ID' _ID double _X _Y
        mata: _translate_json_geom(st_data(., "_ID"), st_sdata(., "`geom'"),/*
            */ "`gtype'", "`addmissing'"!="", "`dots'"=="", "`wkt'"!="")
        drop `geom'
        if "`gtype'"!="" {
            lab def `gtype' /*
                */1 "Point" /*
                */2 "LineString" /*
                */3 "Polygon" /*
                */4 "MultiPoint" /*
                */5 "MultiLineString" /*
                */6 "MultiPolygon" /*
                */7 "GeometryCollection"
            tempvar tmp
            rename `gtype' `tmp'
            encode `tmp', generate(`gtype') label(`gtype')
            qui compress `gtype'
        }
        order _ID _CX _CY `gtype'
    }
    // create geoframes or save
    if `"`toframe'"'!="" {
        _translate_toframe `target' `frame' `shpframe', `replace' `options'
    }
    else {
        _translate_save `"`target'"' `frame' `shpframe' `replace'
    }
end

program __translate_esri
    // syntax
    _parse comma lhs 0 : 0
    syntax [, replace TOFrame * ]
    gettoken target lhs : lhs
    gettoken using  lhs : lhs
    mata: _translate_target(st_local("target"), st_local("using"), /*
        */ "`replace'"!="","`toframe'"!="")
    // import data
    tempname frame shpframe
    frame create `shpframe'
    frame `shpframe' {
        di as txt "(importing shp file) " _c
        import_shp using `"`using'"'
        capt drop rec_header
        sort _ID shape_order
        lab var _ID "Spatial-unit ID"
    }
    frame create `frame'
    frame `frame' {
        mata: st_local("dbf", pathrmsuffix(st_local("using"))+".dbf")
        di as txt "(importing dbf file) " _c
        import_dbase using `"`dbf'"'
        gen long _ID = _n
        qui compress _ID
        sort _ID
        qui gen double _CX = .
        qui gen double _CY = .
        mata: _translate_centroids("`frame'", "`shpframe'")
        lab var _ID "Spatial-unit ID"
        lab var _CX "X coordinate of centroid"
        lab var _CY "Y coordinate of centroid"
        order _ID _CX _CY
    }
    // create geoframes or save
    if `"`toframe'"'!="" {
        _translate_toframe `target' `frame' `shpframe', `replace' `options'
    }
    else {
        _translate_save `"`target'"' `frame' `shpframe' `replace'
    }
end

program __translate_json
    // syntax
    _parse comma lhs 0 : 0
    syntax [, ALLstring gtype(name) ADDMISsing noDOTs replace TOFrame * ]
    if "`gtype'"!="" {
        if inlist("`gtype'","_ID","_CX","_CY") {
            di as err "gtype() may not be _ID, _CX or _CY"
            exit 198
        }
        local GTYPE `gtype'
        local gtype gtype(`gtype')
    }
    gettoken target lhs : lhs
    gettoken using  lhs : lhs
    mata: _translate_target(st_local("target"), st_local("using"),/*
        */ "`replace'"!="","`toframe'"!="")
    // import json file
    tempname frame
    frame create `frame' byte _ID _CX _CY `GTYPE' // reserved names
    frame `frame' {
        mata: _translate_json_import(st_local("using"), "`allstring'"!="",/*
            */ "`dots'"=="")
        drop _ID _CX _CY `GTYPE' // drop reserved names
        qui compress
    }
    // translate geometries
    _translate_geom "`frame'" `"`target'"' _GEOMETRY,/*
        */ `gtype' `addmissing' `dots' `replace' `toframe' `options'
end

program __translate_wkt
    _parse comma lhs 0 : 0
    gettoken target lhs : lhs
    local 0 `"`lhs'`0'"'
    syntax varname(string) [if] [in] [, gtype(name) ADDMISsing noDOTs replace/*
        */ TOFrame * ]
    _translate_toframe_check, `toframe' `options'
    if "`gtype'"!="" {
        if inlist("`gtype'","_ID","_CX","_CY") {
            di as err "gtype() may not be _ID, _CX or _CY"
            exit 198
        }
        local gtype gtype(`gtype')
    }
    mata: _translate_target(st_local("target"), "", "`replace'"!="",/*
        */ "`toframe'"!="")
    tempname frame
    frame copy `c(frame)' `frame'
    _translate_geom "`frame'" `"`target'"' `varlist' `if' `in', wkt/*
        */ `gtype' `addmissing' `dots' `replace' `toframe' `options'
end

program _di_frame
    args lhs frame rhs
    di as txt `"`lhs'{stata geoframe describe `frame':{bf:`frame'}}`rhs'"'
end

program _syntax_add_using
    _parse comma 0 rhs : 0
    if `"`0'"'=="" exit // options only
    syntax [anything(equalok)] [using]
    if `"`using'"'!="" exit // using specified
    while (1) {
        gettoken t anything : anything, quotes bind
        if `"`macval(anything)'"'=="" {
            // add -using- before last token
            local lhs `"`macval(lhs)'`space'using `macval(t)'"'
            continue, break
        }
        local lhs `"`macval(lhs)'`space'`macval(t)'"'
        local space " "
    }
    c_local 0 `"`macval(lhs)'`macval(rhs)'"'
end

program _describe_and_make_current
    args frame current describe
    if "`describe'"=="" {
        geoframe_describe `frame'
    }
    if "`current'"!="" {
        if "`frame'"!=`"`c(frame)'"' {
            frame change `frame'
            if `"`c(prefix)'"'!="frame" {
                if "`describe'"=="" di "" 
                _di_frame "(frame " `frame' " made current)"
            }
        }
    }
end

program _markshapes
    // add variable TOUSE to shpframe that marks all observations in
    // shpframe that have a match in the current (attribute) frame (or
    // optionally among the observations in the current frame identified by
    // touse); uses the existing linkage variable if available; else uses a
    // temporary link (allowing the ID in the current frame to be non.unique)
    args shpframe TOUSE touse
    // case 1: linkage variable available
    _get linkname, local(lnkvar)
    if `"`lnkvar'"'!="" {
        frame `shpframe' {
            if "`touse'"=="" {
                qui gen byte `TOUSE' = `lnkvar'<.
            }
            else {
                qui frget `TOUSE' = `touse', from(`lnkvar')
                qui replace `TOUSE' = 0 if `TOUSE'>=.
            }
        }
        exit
    }
    // case 2: no linkage variable (e.g. because attribute ID not unique)
    _get id, local(id) strict
    frame `shpframe': _get id, local(ID) strict
    tempname tmpframe lnkvar
    frame put `id' `touse', into(`tmpframe')
    frame `tmpframe' {
        if "`touse'"!="" qui keep if `touse'
        qui bysort `id': keep if _n==1 // make id unique
    }
    frame `shpframe' {
        qui frlink m:1 `ID', frame(`tmpframe' `id') generate(`lnkvar')
        if "`touse'"=="" {
            qui gen byte `TOUSE' = `lnkvar'<.
        }
        else {
            qui frget `TOUSE' = `touse', from(`lnkvar')
            qui replace `TOUSE' = 0 if `TOUSE'>=.
        }
    }
end

version 16.1
mata:
mata set matastrict on

void _create_parse_shp_using(string scalar usng, string scalar usng0)
{
    string scalar path, fn
    
    if (pathsuffix(usng)!=".dta") usng = usng + ".dta"
    pathsplit(usng, path="", fn="")
    if (path=="") { // using has no path
        pathsplit(usng0, path, fn)
        if (path!="") usng = pathjoin(path, usng) // using0 has path
    }
    st_local("using", usng)
}

void _save_set_fname(string scalar PATH)
{
    string scalar fn, path, bn
    
    // empty
    if (strtrim(PATH)=="") {
        st_local("fname", st_global("c(frame)"))
        return
    }
    // path only
    if (pathbasename(PATH)=="") {
        fn = pathjoin(PATH, st_global("c(frame)"))
    }
    // filename specified, possibly including path
    else { 
        fn = PATH
        if (pathsuffix(fn)==".dta") fn = pathrmsuffix(fn) // remove .dta suffix
    }
    // check path
    path = bn = ""
    pathsplit(fn, path, bn)
    if (path!="") {
        if (!direxists(path)) {
            errprintf("directory %s does not exist\n", path)
            exit(601)
        }
    }
    // return
    st_local("fname", fn)
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

void _query_n(string scalar R, string scalar id, string scalar xy,
    string scalar touse)
{
    real colvector ID, PID, p, N
    real matrix    XY

    st_view(ID=., ., id, touse)
    N = J(5,1,0)
    if (rows(ID)) {
        st_view(XY=., ., xy, touse)
        if (cols(XY)==4) XY = J(rows(XY),2,0) // pc-data; each row is an item
        PID = geo_pid(ID, XY)
        p = selectindex(_mm_uniqrows_tag((ID, PID)))
        N[2] = rows(p)              // total number of items
        p = p[selectindex(_mm_unique_tag(ID[p],1))] // (last item per unit)
        N[1] = rows(p)              // number of units
        N[(3,5)] = minmax(PID[p])'  // min and max number of items per unit
        N[4] = N[2] / N[1]          // average number of items per unit
    }
    st_matrix(R, N)
}

void _query_dir(string scalar R, string rowvector id,
    string rowvector xy, string scalar touse)
{
    real scalar    i, a, b, dir, pos, neg, na
    real colvector ID, p
    real matrix    XY

    st_view(ID=., ., id, touse)
    st_view(XY=., ., xy, touse)
    if (cols(XY)==4) {
        // pc-data; each row is an item with undefined direction
        st_matrix(R, 0 \ 0 \ rows(XY))
        return
    }
    p = selectindex(_mm_uniqrows_tag((ID, geo_pid(ID,XY))))
    i = rows(p)
    a = rows(XY) + 1
    pos = neg = na = 0
    for (;i;i--) {
        b = a - 1; a = p[i]
        dir = geo_orientation(XY[|a,1 \ b,.|])
        if (dir==1)       pos++ 
        else if (dir==-1) neg++
        else              na++
    }
    st_matrix(R, neg \ pos \ na)
}

void _query_gtype(string scalar R, string scalar id, string scalar xy,
    string scalar touse)
{
    real colvector ID, PID
    real matrix    XY

    st_view(ID=., ., id, touse)
    st_view(XY=., ., xy, touse)
    if (cols(XY)==4) (void) _reshape_pc_to_line(ID, XY, PID=.)
    else             PID = geo_pid(ID, XY)
    st_matrix(R, geo_gtype(2, ID, PID, XY))
}

void _generate_centroids(string rowvector centr, string scalar id,
    string scalar xy)
{
    real colvector ID, p
    real matrix    XY
    
    st_view(ID=., ., id)
    st_view(XY=., ., xy)
    if (cols(XY)==4) {
        p = _reshape_pc_to_line(ID, XY)
        st_store(., centr, geo_centroid(1, ID, XY)[p,])
    }
    else st_store(., centr, geo_centroid(1, ID, XY))
}

void _generate_area(string scalar area, string scalar id, string scalar xy)
{
    real colvector ID
    real matrix    XY
    
    st_view(ID=., ., id)
    st_view(XY=., ., xy)
    st_store(., area, cols(XY)==4 ? J(rows(XY), 1, 0) : geo_area(1, ID, XY))
}

void _generate_pid(string scalar pid, string scalar id, string scalar xy)
{
    real colvector ID
    real matrix    XY
    
    st_view(ID=., ., id)
    st_view(XY=., ., xy)
    st_store(., pid, cols(XY)==4 ? _generate_pid_pc(ID) : geo_pid(ID, XY))
}

real colvector _generate_pid_pc(real colvector ID)
{
    real scalar    i, n, id, pid
    real colvector PID
    
    n = rows(ID)
    PID = J(n,1,.)
    id = pid = 0
    for (i=1;i<=n;i++) {
        if (ID[i]!=id) { // next unit
            pid = 0
            id  = ID[i]
        }
        PID[i] = ++pid
    }
    return(PID)
}

void _generate_dir(string scalar dir, string rowvector id,
    string rowvector xy)
{
    real scalar    i, a, b
    real colvector p, ID, DIR 
    real matrix    XY

    st_view(ID=., ., id)
    st_view(XY=., ., xy)
    if (cols(XY)==4) {
        st_store(., dir, J(rows(XY),1,0))
        return
    }
    st_view(DIR=., ., dir)
    p = selectindex(_mm_uniqrows_tag((ID, geo_pid(ID,XY))))
    i = rows(p)
    a = rows(XY) + 1
    for (;i;i--) {
        b = a - 1; a = p[i]
        DIR[|a \ b|] = J(b - a + 1, 1, geo_orientation(XY[|a,1 \ b,2|]))
    }
}

void _generate_gtype(string scalar gtype, string rowvector id,
    string rowvector xy)
{
    real colvector ID, PID, p
    real matrix    XY

    st_view(ID=., ., id)
    st_view(XY=., ., xy)
    if (cols(XY)==4) {
        p = _reshape_pc_to_line(ID, XY, PID=.)
        st_store(., gtype, geo_gtype(1, ID, PID, XY)[p,])
    }
    else {
        PID = geo_pid(ID, XY)
        st_store(., gtype, geo_gtype(1, ID, PID, XY))
    }
}

void _generate_plevel(real scalar nodots, string scalar pl, string scalar id,
    string scalar xy, string scalar touse, | string scalar BY, real scalar lvl)
{
    real colvector ID, PID, PL, p, pc, L
    real matrix    XY
    
    if (BY!="") {
        p   = selectindex(st_data(., BY, touse):==lvl)
        ID  = st_data(., id,  touse)[p]
        XY  = st_data(., xy,  touse)[p,]
    }
    else {
        st_view(ID=.,  ., id,  touse)
        st_view(XY=.,  ., xy,  touse)
    }
    if (cols(XY)==4) {
        pc = _reshape_pc_to_line(ID, XY, PID=.)
        L  = geo_plevel(1, ID, PID, XY, nodots)
    }
    else {
        pc = .
        PID = geo_pid(ID, XY)
        L = geo_plevel(1, ID, PID, XY, nodots)
    }
    if (BY!="") {
        st_view(PL=., ., pl,  touse)
        PL[p,] = L[pc]
    }
    else {
        st_store(., pl, touse, L[pc])
    }
    st_local("N", strofreal(sum(L:!=0 :& _mm_uniqrows_tag((ID,PID))), "%18.0g"))
}

void _project(string scalar touse, string rowvector xy,
    string scalar pname, string scalar pargs, string scalar radian)
{
    real matrix XY

    st_view(XY=., ., xy, touse)
    XY[.,.] = geo_project(XY, pname, radian!="", strtoreal(tokens(pargs)))
}

struct _clip_expand_info {
    pointer rowvector XY // copy of clipped polygon
    pointer rowvector ab // range of original polygon in data
}

void _clip(string scalar id, string scalar out, string rowvector xy,
    string scalar touse, string scalar mask, real scalar rclip,
    real scalar method, real scalar nodrop, real scalar dots)
{
    real scalar    i, a, b, dn, d0, pc
    real colvector ID, OUT, p, pp
    real matrix    MASK, XY, XYi, MAP
    struct _clip_expand_info scalar I
    
    if (rclip) MASK = _geo_rclip_limits(st_matrix(mask))
    else       MASK = _geo_clip_mask(st_matrix(mask), "matname")
    st_view(ID=., ., id, touse)
    st_view(XY=., ., xy, touse)
    pc = cols(XY)==4
    if (pc) {
        pp  = _reshape_pc_to_line(ID, XY)
        OUT = J(rows(XY),1,0)
    }
    else st_view(OUT=., ., out, touse)
    p = selectindex(_mm_unique_tag(ID))
    i = rows(p)
    if (dots) {
        displayas("txt")
        printf("(clipping shape items of %g unit%s)\n", i, i!=1 ? "s" : "")
        d0 = _geo_progress_init("(")
        dn = i
    }
    a = rows(ID) + 1
    for (;i;i--) {
        if (dots) _geo_progress(d0, 1-i/dn)
        b = a - 1; a = p[i]
        XYi = _geo_clip_or_select(XY[|a,1\b,.|], MASK, method, MAP=1)
        if (!rows(XYi)) { // empty
            OUT[|a\b|] = J(b-a+1, 1, 1)
            if (nodrop) { // keep unit
                XY[a,] = J(1, cols(XY), .)
                OUT[a] = 0
            }
            continue
        }
        _clip_fillin(OUT, XY, a-1, I, XYi, MAP)
    }
    st_local("Nadd", "0")
    if (pc) {
        if (dots) _geo_progress_end(d0, ")")
        if (length(I.ab)) {
            errprintf("unexpected data inconsistency;" +
                " cannot store clipped shapes")
            exit(499)
        }
        _clip_recast_line_to_pc(OUT, XY, pp)
        st_store(., out, touse, OUT)
        st_store(., xy, touse, XY)
    }
    else {
        _clip_expand(touse, I.XY, I.ab, xy) // store items with extra points
        if (dots) _geo_progress_end(d0, ")")
    }
}

void _clip_fillin(real colvector OUT, real matrix XY, real scalar a0,
    struct _clip_expand_info scalar I, real matrix XYi, real matrix M)
{
    real scalar i, a, b, n, a1, b1, n1
    
    i = rows(M)
    for (;i;i--) {
        a  = M[i,1] + a0; b  = M[i,2] + a0; n  = b - a + 1
        a1 = M[i,3];      b1 = M[i,4]     ; n1 = b1 - a1 + 1
        if (n1<1) { // drop item
            OUT[|a\b|] = J(n,1,1) // drop item
            continue
        }
        if (n1>n) { // too many observations; handle later
            I.XY = I.XY, &XYi[|a1,1\b1,.|]
            I.ab = I.ab, &(a\b)
            continue
        }
        OUT[|a\b|] = J(n, 1, 1)
        b = a + n1 - 1
        OUT[|a\b|] = J(n1, 1, 0)
        XY[|a,1\b,.|] = XYi[|a1,1\b1,.|]
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
        V[|*ab[i],(1\.)|] = *XY[i]
    }
}

void _clip_recast_line_to_pc(real colvector OUT0, real matrix XY0,
    real colvector pp)
{
    real scalar    i, a, b
    real colvector OUT
    real matrix    XY
    
    i = rows(pp)
    OUT = J(i,1,0)
    XY  = J(i,4,.)
    a = rows(OUT) + 1
    for (;i;i--) {
        b = a - 1
        a = pp[i]
        if (OUT0[a]==1) { // dropped item
            OUT[i] = 1
            continue
        }
        if (b==a)         continue // empty item in original data
        if (OUT0[a+1]==1) continue // keep empty item
        XY[i,] = XY0[a+1,], XY0[a+2,] // copy (clipped) coordinates
    }
    swap(OUT0,OUT)
    swap(XY0,XY)
}

void _simplify(string scalar id, string scalar out, string rowvector xy,
    string scalar touse, real scalar delta, real scalar jointly,
    real scalar nodrop, real scalar dots)
{
    real scalar    i, a, b, b0, j, c, d, dn, d0
    real colvector ID, OUT, PID, B, p, q
    real matrix    XY
    
    st_view(ID=., ., id, touse)
    st_view(OUT=., ., out, touse)
    st_view(XY=., ., xy, touse)
    PID = geo_pid(ID, XY)
    if (jointly) B = geo_bshare(ID, PID, XY, 3, !dots)
    p = selectindex(_mm_uniqrows_tag((ID, PID)))
    i = rows(p)
    if (dots) {
        displayas("txt")
        printf("(simplifying %g shape item%s)\n", i, i!=1 ? "s" : "")
        d0 = _geo_progress_init("(")
        dn = i
    }
    q = selectindex(_mm_unique_tag(ID[p]))
    c = rows(p) + 1
    a = rows(ID) + 1
    for (j=rows(q);j;j--) {
        // using a double loop so that progress dots are at level of items, but
        // nodrop operates at the level of units
        d = c - 1; c = q[j]; b0 = a - 1
        for (i=d; i>=c; i--) {
            b = a - 1; a = p[i]
            OUT[|a\b|] = jointly ?
                !geo_simplify(XY[|a,1\b,2|], delta, B[|a\b|]) :
                !geo_simplify(XY[|a,1\b,2|], delta)
            if (dots) _geo_progress(d0, 1-(i-1)/dn)
        }
        if (nodrop) {
            if (all(OUT[|a\b0|])) {
                OUT[a] = 0 // keep unit in data
                XY[a,] = (.,.)
            }
        }
    }
    if (dots) _geo_progress_end(d0, ")")
}

void _refine(string scalar id, string rowvector xy,
    string scalar touse, real scalar delta, real scalar dots)
{
    real scalar    i, a, b, dn, d0
    real colvector ID, p
    real matrix    XY, XYi
    struct _clip_expand_info scalar I

    st_view(ID=., ., id, touse)
    st_view(XY=., ., xy, touse)
    p = selectindex(_mm_uniqrows_tag((ID, geo_pid(ID,XY))))
    i = rows(p)
    if (dots) {
        displayas("txt")
        printf("(refining %g shape item%s)\n", i, i!=1 ? "s" : "")
        d0 = _geo_progress_init("(")
        dn = i
    }
    a = rows(ID) + 1
    for (;i;i--) {
        b = a - 1; a = p[i]
        XYi = geo_refine(XY[|a,1 \ b,2|], delta)
        if (rows(XYi)>(b-a+1)) {
            I.XY = I.XY, &XYi[.,.]
            I.ab = I.ab, &(a \ b)
        }
        if (dots) _geo_progress(d0, 1-(i-1)/dn)
    }
    if (dots) _geo_progress_end(d0, ")")
    st_local("Nadd", "0")
    _clip_expand(touse, I.XY, I.ab, xy) // store items with additional points
}

void _grid_pos(string scalar mat, real scalar n, real scalar tissot,
    real scalar tight, real scalar radian, real scalar min, real scalar max)
{
    real scalar    rnd, step, mid1, min1, max1
    real colvector x
    
    if (!tight) {
        if (max>min) { // apply some rounding
            if (radian) { // convert to degrees
                min = min / pi() * 180
                max = max / pi() * 180
            }
            rnd = (max-min) / max((n-1+2*tissot, 10))
            rnd = 10^(floor(log10(rnd)))
            if (rnd>0 & rnd<.) { // just to be sure
                mid1 = round((min+max)/2, rnd)
                if (n==1) min = max = mid1
                else {
                    step = (max-min) / (n-1+2*tissot)
                    step = ceil(2*step/rnd)*rnd/2 // only round up
                    while (1) {
                        min1 = mid1 - (n-1+2*tissot)/2 * step
                        max1 = min1 + (n-1+2*tissot)   * step
                        if (min1<=min & max1>=max) break
                        step = step + rnd/2 // increase step size
                    }
                    min = min1
                    max = max1
                }
            }
            if (radian) { // revert back to radians
                min = min / 180 * pi() 
                max = max / 180 * pi() 
            }
        }
    }
    if (n==1) x = (min+max)/2
    else      x = rangen(min, max, n+2*tissot)[|1+tissot \ n+tissot|]
    st_matrix(mat, x)
}

void _grid(string scalar shpframe, real scalar n, real colvector x, 
    real colvector y)
{
    real scalar nx, ny, xmin, xmax, ymin, ymax, j, b
    real matrix U, S
    
    nx = rows(x); xmin = x[1]; xmax = x[nx]
    ny = rows(y); ymin = y[1]; ymax = y[ny]
    U = J(nx + ny - 4, 8, .)
    S = J(rows(U)*(n+1), 3, .)
    j = b = 0
    __grid(U, S, n, j, b, x, ymin, ymax, 1)
    __grid(U, S, n, j, b, y, xmin, xmax, 2)
    st_addobs(rows(U))
    st_store(., tokens("_ID _CX _CY xmin ymin xmax ymax axis"), U)
    st_framecurrent(shpframe)
    st_addobs(rows(S))
    st_store(., tokens("_ID _X _Y"), S)
}

void __grid(real matrix U, real matrix S, real scalar n,
    real scalar j, real scalar b, real colvector x,
    real scalar min, real scalar max, real scalar axis)
{
    real scalar i, nx, a
    
    nx = rows(x) - 1
    for (i=2; i<=nx; i++) {
        j++
        if (axis==1) U[j,] = j, x[i], (min+max)/2, x[i], min, x[i], max, axis
        else         U[j,] = j, (min+max)/2, x[i], min, x[i], max, x[i], axis
        a = b + 1
        S[a++,1] = j // first row missing
        b = a + n - 1
        if (axis==1) S[|a,1\b,3|] = J(n,1,j), J(n,1,x[i]), rangen(min, max, n)
        else         S[|a,1\b,3|] = J(n,1,j), rangen(min, max, n), J(n,1,x[i]) 
    }
}

void _tissot(string scalar shpframe, real scalar n, real scalar rad,
    real scalar r, real colvector x, real colvector y)
{
    real scalar nx, ny, nxy, i, k
    real matrix U, S
    pointer (function) scalar f
    
    if (rad) f = &_geo_tissot()
    else     f = &geo_tissot()
    nx = rows(x); ny = rows(y); nxy = nx * ny
    U = J(nxy, 1, .), J(nx, 1, y)
    for (i=nx;i;i--) U[|(i-1)*ny+1,1 \ i*ny,1|] = J(ny, 1, x[i])
    k = n + 2 // (.,.) plus n+1 points returned by geo_tissot() 
    S = J(nxy * k, 3, .)
    for (i=nxy;i;i--) {
        S[|(i-1)*k+1,1 \ i*k,3|] = J(k, 1, i),
            ((.,.) \ (*f)(U[i,1], U[i,2], r, n))
    }
    st_addobs(rows(U))
    st_store(., "_ID", 1::nxy)
    st_store(., ("_CX", "_CY"), U)
    st_framecurrent(shpframe)
    st_addobs(rows(S))
    st_store(., ("_ID", "_X", "_Y"), S)
}

void _bbox(string scalar frame, string scalar xy, string scalar touse,
     real scalar bt, real scalar k, real scalar pad, real scalar adj,
     real scalar angl, string scalar by, string scalar id)
{
    string scalar  cframe
    real scalar    i, a, b, r
    real colvector p
    real matrix    XY, ID
    pointer (real matrix) colvector P

    // data
    st_view(XY=., ., xy, touse)
    if (by=="") {
        p  = 1
        ID = 1
        if (cols(XY)==4) XY = XY[,(1,2)] \ XY[,(3,4)] // stack data if pc
    }
    else {
        st_view(ID=., ., by, touse)
        if (by!=id) { // sort data if BY unequal to ID
            p = mm_order(ID,1,1)
            ID = ID[p]
            XY = XY[p,]
        }
        if (cols(XY)==4) (void) _reshape_pc_to_line(ID, XY) // stack data if pc
        p = selectindex(_mm_unique_tag(ID))
    }
    // generate shapes
    i = rows(p)
    if (i==1) {
        if      (bt==3) P = &__bbox_hull(XY, pad)
        else if (bt==2) P = &__bbox_mec(XY, k, pad, adj, angl)
        else            P = &__bbox(XY, bt, pad)
        r = rows(*P)
    }
    else {
        r = 0
        P = J(i,1,NULL)
        a = rows(XY) + 1
        for (;i;i--) {
            b = a - 1; a = p[i]
            if      (bt==3) P[i] = &__bbox_hull(XY[|a,1\b,.|], pad)
            else if (bt==2) P[i] = &__bbox_mec(XY[|a,1\b,.|], k, pad, adj, angl)
            else            P[i] = &__bbox(XY[|a,1\b,.|], bt, pad)
            r = r + rows(*P[i])
        }
    }
    // store shapes
    cframe = st_framecurrent()
    st_framecurrent(frame)
    st_addobs(r)
    b = 0
    for (i=rows(p);i;i--) {
        a = b + 1
        b = b + rows(*P[i])
        st_store((a,b), "_ID", J(b-a+1, 1, ID[p[i]]))
        st_store((a,b), ("_X","_Y"), *P[i])
    }
    st_framecurrent(cframe)
}

real matrix __bbox_hull(real matrix XY, real scalar pad)
{
    real rowvector c
    real matrix    xy
    
    xy = geo_hull(XY)
    if (pad) {
        c = __geo_centroid(1, xy)
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

void _bshare(string scalar frame, string scalar id, string scalar xy,
    string scalar touse, real scalar rtype, real scalar nodots)
{
    real colvector ID, XY
    real matrix    R
    
    st_view(ID=., ., id, touse)
    st_view(XY=., ., xy, touse)
    R = geo_bshare(ID, geo_pid(ID, XY), XY, rtype, nodots)
    if (rtype==3) R = mm_uniqrows(select((ID,XY),R))
    st_framecurrent(frame)
    st_addobs(rows(R))
    if (rtype==0) st_store(.,tokens("_ID _ID1 _ID2 _X _Y"), R)
    else          st_store(.,tokens("_ID _X _Y"), R)
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

void _append(string scalar frame, string scalar touse, string rowvector vars,
    string rowvector VARS)
{
    real scalar   i, n, n0
    string scalar cframe
    transmorphic  X

    cframe = st_framecurrent()
    // add obs
    st_framecurrent(frame)
    n = rows(st_data(., touse, touse))
    st_framecurrent(cframe)
    n0 = st_nobs()
    st_addobs(n)
    n = n0 + n; n0 = n0 + 1
    // copy variables, one by one
    i = length(vars)
    for (;i;i--) {
        st_framecurrent(frame)
        if (st_isstrvar(vars[i])) {
            X = st_sdata(., vars[i], touse)
            st_framecurrent(cframe)
            st_sstore((n0,n), VARS[i], X)
        }
        else {
            X = st_data(., vars[i], touse)
            st_framecurrent(cframe)
            st_store((n0,n), VARS[i], X)
        }
    }
}

void _append_labels(string scalar frame, string rowvector vars,
    string rowvector VARS)
{
    real scalar      i, v
    string scalar    cframe, vl, lnm, LNM
    real colvector   val, p
    string colvector lbl, LBL
    
    cframe = st_framecurrent()
    i = length(vars)
    for (;i;i--) {
        // collect labels from source
        st_framecurrent(frame)
        v = st_varindex(vars[i])
        vl = st_varlabel(v)
        lnm = st_varvaluelabel(v)
        if (lnm!="") {
            if (st_vlexists(lnm)) st_vlload(lnm, val=., lbl="")
            else lnm = ""
        }
        // add labels to target
        st_framecurrent(cframe)
        v = st_varindex(VARS[i])
        if (st_varlabel(v)=="") st_varlabel(v, vl)
        if (lnm!="") {
            LNM = st_varvaluelabel(v)
            if (LNM=="") {
                // variable does not yet have value labels; use the variable
                // name as label name if in the source the label name is
                // equal to the variable name; else use the label name from the
                // source variable
                if (lnm==vars[i]) LNM = VARS[i]
                else              LNM = lnm
            }
            LBL = st_vlmap(LNM, val)
            if (any(LBL:!=lbl :& LBL:!="")) {
                printf("{txt}(%s: conflicting labels between source " +
                    "and target; using target variant)\n", LNM)
            }
            p = selectindex(LBL:=="")
            if (length(p)) st_vlmodify(LNM, val[p], lbl[p])
            st_varvaluelabel(v, LNM)
        }
    }
}

void _clean_empty(string scalar empty, string scalar id, string rowvector xy,
    string scalar touse)
{
    real scalar    i, nempty, a, b 
    real colvector ID, E, p
    real matrix    XY

    st_view(E=., ., empty, touse)
    st_view(ID=., ., id, touse)
    st_view(XY=., ., xy, touse)
    p = selectindex(_mm_unique_tag(ID))
    i = rows(p)
    nempty = 0
    a = rows(ID) + 1
    for (;i;i--) {
        b = a - 1; a = p[i]
        if (all(rowmissing(XY[|a,1 \ b,.|]))) {
            E[|a \ b|] = J(b-a+1, 1, 1)
            nempty++
        }
    }
    st_local("Nempty", strofreal(nempty, "%18.0g"))
}

void _translate_zipname(string scalar fn)
{
    string scalar bn, shpfile, sep
    pragma unset shpfile
    
    if (!strpos(fn, ".zip")) {
        st_local("zip", "0")
        return
    }
    bn = pathbasename(fn)
    if (bn=="") { // fn ends with directory separator
        sep = substr(fn,-1,.)
        fn = substr(fn,1,strlen(fn)-1)
    }
    while (fn!="") {
        bn = pathbasename(fn)
        if (substr(bn,-4,.)==".zip") {
            if (!direxists(fn)) { // only if not a directory
                if (shpfile!="") shpfile = shpfile + sep
                st_local("zip", "1")
                st_local("zipfile", fn)
                st_local("shpfile", shpfile)
                return
            }
        }
        pathsplit(fn, fn, bn)
        shpfile = pathjoin(bn, shpfile)
    }
    // path contains no file with a .zip suffix
    st_local("zip", "0")
}

void _translate_zip_cleanup(string scalar PATH)
{   // use with extreme care; recursively deletes a directory
    real scalar   rc
    string scalar path, folder
    
    if      (!pathisabs(PATH)) rc =  1
    else if (!direxists(PATH)) rc = -1
    else {
        path = folder = ""
        pathsplit(PATH, path, folder)
        if (path!=st_global("c(tmpdir)")) rc = 1
        else rc = _stata("__rm_dir " + "`" + `"""' + PATH + `"""' + "'" +
            ", force",1) // undocumented command to delete folder recursively
            // returns -1 if the folder does not exist
    }
    if (rc>0) errprintf("could not remove temporary folder '%s'\n", PATH)
}

void _translate_findsource(string scalar nm, string scalar subcmd,
    | string scalar zip)
{
    real scalar      i, n, rc
    string scalar    fn, bn, sfx
    string colvector FN
    
    fn = st_local(nm)
    // dictionary
    if      (subcmd=="esri") sfx = (".shp") \ ("esri")
    else if (subcmd=="json") sfx = (".json", ".geojson") \ ("json", "json")
    else  sfx = (".json", ".geojson", ".shp") \ ("json", "json", "esri")
    // check whether path only
    if (!direxists(fn)) { // directory takes precedence
        bn = pathbasename(fn) 
    }
    // if filename specified (i.e. not only a path)
    if (bn!="") {
        // check file without adding suffix
        rc = fileexists(fn)
        if (rc) { // file found
            if (subcmd=="") { // find translator type
                for (i=cols(sfx);i;i--) {
                    if (sfx[1,i]==pathsuffix(fn)) {
                        subcmd = sfx[2,i]
                        break
                    }
                }
                if (subcmd=="") subcmd = "esri" // default
            }
        }
        // try with suffix
        else {
            for (i=cols(sfx);i;i--) {
                rc = fileexists(fn + sfx[1,i])
                if (rc) { // file found
                    fn = fn + sfx[1,i]
                    subcmd = sfx[2,i]
                    break
                }
            }
        }
        // no matching file found
        if (!rc) {
            if (zip=="") errprintf("source %s not found\n", fn)
            else errprintf("source %s not found in %s\n", fn, zip)
            exit(601)
        }
    }
    // if path only
    else {
        // check whether specified folder exists
        if (fn!="") {
            if (!direxists(fn)) {
                if (zip=="") errprintf("directory %s not found\n", fn)
                else errprintf("directory %s not found in %s\n", fn, zip)
                exit(601)
            }
        }
        // collect shape files in folder (and subfolders if zip!="")
        n = 0
        for (i=cols(sfx);i;i--) {
            FN = __translate_findsource(fn, sfx[1,i], zip!="")
            n = length(FN)
            if (n) {
                subcmd = sfx[2,i]
                break
            }
        }
        // no matching files found
        if (!n) {
            if (subcmd=="") subcmd = "esri"
            errprintf("no %s shapefile found in %s\n", subcmd, pathjoin(zip,fn))
            exit(601)
        }
        // multiple matching files found
        if (n>1) {
            printf("{txt}multiple shapefiles found in %s:\n", pathjoin(zip,fn))
            for (i=1;i<=n;i++) printf("  %s\n", FN[i])
        }
        fn = FN[1] // use first match
        printf("{txt}(translating %s)\n", pathjoin(zip,fn))
    }
    // return
    st_local(nm, fn)
    st_local("subcmd", subcmd)
}

string colvector __translate_findsource(string scalar path, string matrix sfx,
    real scalar recursive)
{
    real scalar      i, n
    string colvector fn, dir
    
    // collect files in current folder
    fn  = sort(dir(path, "files", "*"+sfx), 1)
    n = rows(fn)
    if (n) {
        fn = select(fn, substr(fn,1,1):!=".") // exclude hidden files
        n = rows(fn)
        if (!n) fn = J(0,1,"") // select may return J(0,0,"")
    }
    for (i=n;i;i--) fn[i] = pathjoin(path, fn[i]) // add path
    if (!recursive) return(fn)
    // collect directories
    dir = sort(dir(path, "dirs", "*"), 1)
    n = rows(dir)
    if (n) {
        dir = select(dir, substr(dir,1,1):!=".") // exclude hidden folders
        n = rows(dir)
        if (n) {
            dir = select(dir, dir:!="__MACOSX") // exclude __MACOSX folder
            n = rows(dir)
        }
        if (!n) dir = J(0,1,"") // select may return J(0,0,"")
    }
    // collect files from subfolders (recursively)
    for (i=1;i<=n;i++) {
        fn = fn \ __translate_findsource(pathjoin(path, dir[i]), sfx, 1)
    }
    return(fn)
}

void _translate_target(string scalar fn, string scalar source,
    real scalar replace, real scalar toframe)
{
    if (toframe) _translate_target_frame(fn, source, replace)
    else         _translate_target_file(fn, source, replace)
}

void _translate_target_frame(string scalar fn, string scalar source,
    real scalar replace) // fn assumed fleeting
{
    if (fn!="") {
        if (!st_isname(fn)) {
            errprintf("'%s' invalid destination name\n", fn)
            exit(7)
        }
    }
    else {
        fn = pathrmsuffix(pathbasename(source))
        if (fn!="") fn = ustrtoname(fn) // ustrtoname("") may crash Stata
    }
    if (fn=="") {
        errprintf("destination name required\n")
        exit(198)
    }
    if (!replace) {
        if (st_frameexists(fn)) {
            errprintf("frame {bf:%s} already defined\n", fn)
            exit(110)
        }
        if (st_frameexists(fn+"_shp")) {
            errprintf("frame {bf:%s} already defined\n", fn+"_shp")
            exit(110)
        }
    }
    st_local("target", fn)
}

void _translate_target_file(string scalar fn, string scalar source,
    real scalar replace) // fn assumed fleeting
{
    string scalar path, bn
    
    // if filename specified (i.e. not only a path)
    if (pathbasename(fn)!="") {
        if (pathsuffix(fn)==".dta") fn = pathrmsuffix(fn) // remove .dta suffix
    }
    // if path only
    else {
        bn = pathrmsuffix(pathbasename(source))
        fn = pathjoin(fn, bn)
    }
    // check directory and name
    pathsplit(fn, path="", bn="")
    if (path!="") {
        if (!direxists(path)) {
            errprintf("directory %s does not exist\n", path)
            exit(601)
        }
    }
    if (bn=="") {
        errprintf("destination name required\n")
        exit(198)
    }
    // check files
    if (!replace) {
        if (fileexists(fn+".dta")) {
            errprintf("file {bf:%s} already exists\n", fn+".dta")
            exit(602)
        }
        if (fileexists(fn+"_shp.dta")) {
            errprintf("file {bf:%s} already exists\n", fn+"_shp.dta")
            exit(602)
        }
    }
    // return
    st_local("target", fn) // path and name without .dta suffix
}

void _translate_centroids(string scalar frame, string scalar shpframe)
{
    real scalar r, n
    real matrix R
    
    st_framecurrent(shpframe)
    R = geo_centroid(0, st_data(.,"_ID") , st_data(., ("_X", "_Y")))[,(2,3)]
    st_framecurrent(frame)
    if (!(r = rows(R)))   return
    if (!(n = st_nobs())) return
    if (n==r) st_store(.,     ("_CX", "_CY"), R)
    if (n>r)  st_store((1,r), ("_CX", "_CY"), R)
    else      st_store(.,     ("_CX", "_CY"), R[|1,1\n,2|])
}

void _translate_json_import(string scalar fn, real scalar allstr,
    real scalar dots)
{
    real scalar      rc, j, J, n, idx, l
    string rowvector vars
    string matrix    S
    pragma unset rc
    
    if (dots) printf("{txt}(importing source ...")
    S = geo_json_import(fn, rc)
    n = rows(S) - 2
    if (n>=0) {
        vars = strtoname(S[1,]), st_varname(1..st_nvar())
        st_addobs(n)
        J = cols(S)
        for (j=1;j<=J;j++) {
            if (_st_varindex(vars[j])<.) _translate_json_import_vname(vars, j)
            if (allstr ? 0 : S[2,j]=="numeric") {
                idx = st_addvar("double", vars[j])
                st_store(., idx, strtoreal(S[|3,j \ .,j|]))
            }
            else {
                l = max(1 \ strlen(S[|3,j \ .,j|]))
                idx = st_addvar(l<=2045 ? "str"+strofreal(l) : "strL", vars[j])
                st_sstore(., idx, S[|3,j \ .,j|])
            }
        }
    }
    if (dots) printf("{txt} done)\n")
    if (rc) {
        printf("{txt}(invalid or incomplete GeoJSON source; " +
            "please check data)\n")
    }
}

void _translate_json_import_vname(string rowvector vars, real scalar j)
{
    real scalar   i, I
    string scalar v, n
    
    I = 1000
    v = vars[j]
    for (i=1;i<=I;i++) {
        n = strofreal(i)
        v = substr(vars[j], 1, 32-1-strlen(n)) + "_" + n
        if (anyof(vars, v)) continue
        break
    }
    if (i>I) {
        errprintf(
            "variable %s already exists; could not find alternative name\n",
            vars[j])
        exit(499)
    }
    vars[j] = v
}

void _translate_json_geom(real colvector id, string colvector S,
    string scalar gtype, real scalar mis, real scalar dots, real scalar wkt)
{
    real scalar      i, n, r, a, b, d, rc
    string scalar    cframe, gt
    string colvector GT
    real colvector   RC
    real matrix      XY, xy
    pragma unset gt
    
    cframe = st_framecurrent()
    st_framecurrent(st_local("shpframe"))
    n = rows(id)
    if (dots) {
        displayas("txt")
        printf("(processing %g geometry object%s)\n", n, n!=1 ? "s" : "")
        d = _geo_progress_init("(")
    }
    xy = J(n,2,.)
    GT = J(n,1,"")
    RC = J(n,1,0)
    b = rc = 0
    for (i=1;i<=n;i++) {
        if (wkt) XY =  geo_wkt2xy(S[i], mis!=0, gt, rc)
        else     XY = geo_json2xy(S[i], mis!=0, gt, rc)
        GT[i]  = gt
        RC[i]  = rc
        xy[i,] = _geo_centroid(XY)
        r = rows(XY)
        a = b + 1
        b = b + r
        st_addobs(r)
        st_store((a,b), 1, J(r,1,id[i])) // ! positional
        st_store((a,b), (2,3), XY)       // ! positional
        if (dots) _geo_progress(d, i/n)
    }
    st_framecurrent(cframe)
    st_store(., ("_CX","_CY"), xy)
    if (gtype!="") st_sstore(., gtype, GT)
    if (dots) _geo_progress_end(d, ")")
    if (any(RC)) {
        RC = selectindex(RC)
        printf("{txt}(empty, incomplete, or invalid geometry in unit%s %s; " +
            "please check data)\n",
            rows(RC)==1 ? "" : "s",
            invtokens(strofreal(RC)'))
    }
}

void _tag_unit_if_any(string scalar id, string scalar touse)
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
        if (anyof(TOUSE[|a \ b|],1)) TOUSE[|a \ b|] = J(b-a+1, 1, 1)
    }
}

real colvector _reshape_pc_to_line(real colvector ID0, real matrix XY0,
    | real matrix PID)
{   // modifies ID0 and XY0
    real scalar    i, n, a, b, id, pid
    real colvector ID, p
    real matrix    XY
    
    n = rows(XY0)
    p = J(n,1,.)
    ID = PID = J(n*3,1,.)
    XY = J(n*3,2,.)
    b = id = pid = 0
    for (i=1;i<=n;i++) {
        if (ID0[i]!=id) { // next unit
            pid = 0
            id  = ID0[i]
        }
        pid++
        if (missing(XY0[i,])==4) { // empty item
            b++
            p[i]   = b
            ID[b]  = id
            PID[b] = pid
            continue
        }
        a = b + 1
        b = b + 3
        p[i]       = a
        ID[|a\b|]  = J(3,1,id)
        PID[|a\b|] = J(3,1,pid)
        XY[|a,1\b,.|] = (.,.) \ XY0[i,(1,2)] \ XY0[i,(3,4)]
    }
    if (b<(n*3)) {
        ID0 = ID[|1\b|]
        XY0 = XY[|1,1\b,.|]
        PID = PID[|1\b|]
    }
    else {
        swap(ID0, ID)
        swap(XY0, XY)
    }
    return(p)
}

end

