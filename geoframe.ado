*! version 0.1.0  17may2023  Ben Jann

program geoframe
    version 18
    gettoken subcmd 0 : 0, parse(" ,")
    _parse_subcmd `subcmd'
    _geoframe_`subcmd' `0'
    if `"`local'"'!="" { // pass through returns from _geoframe_get
        c_local `local' `"`value'"'
    }
end

program _parse_subcmd
    local l = strlen(`"`0'"')
    if      `"`0'"'==substr("create", 1, max(2,`l'))    local 0 create
    else if `"`0'"'=="set"                              local 0 set
    else if `"`0'"'=="get"                              local 0 get
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
    syntax [anything(equalok)] [using] [, * ]
    if `:list sizeof using'==0 {
        gettoken frame anything : anything
        local 0 `macval(frame)' using `macval(anything)', `macval(options)'
    }
    syntax name(id="framename" name=frame) using/ [, replace set(str asis) ]
    nobreak {
        if "`replace'"!="" {
            qui _frame dir
            local framelist = r(contents)
            if `:list frame in framelist' {
                tempname tmpframe
                frame rename `frame' `tmpframe'
            }
        }
        frame create `frame'
        capture noisily break {
            frame `frame' {
                use `"`macval(using)'"'
                if `:list sizeof set' {
                    _geoframe_set `macval(set)'
                }
            }
        }
        if _rc {
            frame drop `frame'
            if "`tmpframe'"!="" {
                frame rename `tmpframe' `frame'
            }
            exit _rc
        }
    }
end

program _geoframe_set
    _parse comma lhs 0 : 0
    syntax [, relax ]
    local vlist ID PID EID Y X Y1 X1 Y2 X2
    while `: list sizeof lhs' {
        gettoken name lhs : lhs, parse(" =")
        gettoken eq   lhs : lhs, parse(" =")
        gettoken exp  lhs : lhs, parse(" =")
        if `"`eq'"'!="=" {
            di as err "geoframe set: invalid syntax"
            exit 198
        }
        local name = strupper(`"`name'"')
        capt mata: assert(st_isname("GEO_"+st_local("name")))
        if _rc==1 exit _rc
        if _rc {
            di as err `"geoframe set: `name' invalid name"'
            exit 198
        }
        if "`relax'"=="" {
            capt mata:/*
                */ assert(anyof(tokens(st_local("vlist")), st_local("name")))
            if _rc==1 exit _rc
            if _rc==0 {
                // check whether variable exists
                capt confirm variable `exp', exact
                if _rc==1 exit _rc
                if _rc {
                    di as err `"variable {bf:`exp'} not found"'
                    exit 111
                }
            }
        }
        char _dta[GEO_`name'] `exp'
    }
end

program _geoframe_get
    syntax anything(id="name" name=name), [ Local(str) relax noDEFault ] 
    local name = strupper(`"`name'"')
    capt mata: assert(st_isname("GEO_"+st_local("name")))
    if _rc==1 exit _rc
    if _rc {
        di as err `"geoframe get: `name' invalid name"'
        exit 198
    }
    if `"`local'"'!="" {
        capt mata: assert(st_islmname(st_local("local")))
        if _rc==1 exit _rc
        if _rc {
            di as err `"local(): `name' invalid name"'
            exit 198
        }
    }
    local exp: char _dta[GEO_`name']
    local vlist ID PID EID Y X Y1 X1 Y2 X2
    capt mata: assert(anyof(tokens(st_local("vlist")), st_local("name")))
    if _rc==1 exit _rc
    if _rc==0 {
        if `"`exp'"'=="" {
            if "`default'"=="" local exp _`name'
        }
        if `"`exp'"'!="" & "`relax'"=="" {
            capt confirm variable `exp', exact
            if _rc==1 exit _rc
            if _rc {
                di as err `"variable {bf:`exp'} not found in frame {bf:`c(frame)'}"'
                di as err "use {bf:geoframe set} to set appropriate name"
                exit 111
            }
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

program _geoframe_attach
    gettoken frame 0 : 0, parse(" ,")
    frame `frame' {
        syntax [varlist(default=none)] [, /*
            */ ID0(str) keep(varlist) drop(varlist) ]
        local id `varlist'
        if `"`id'"'=="" geoframe get ID, l(id)
        if "`keep'"=="" {
            qui ds
            local keep `"`r(varlist)'"'
        }
        local keep: list keep - drop
        local keep: list keep - id
    }
    local 0 `"`id0'"'
    syntax [varlist(default=none)]
    local id0 `varlist'
    if `"`id0'"'=="" geoframe get ID, l(id0)
    local keep: list keep - id0
    if `: list sizeof keep'==0 {
        di as txt "(no variables to attach)"
        exit
    }
    frlink m:1 `id0', frame(`frame' `id')
    fralias add `keep', from(`frame')
end

program _geoframe_detach
    syntax name(name=frame)
    qui frlink dir
    local frlink `"`r(vars)'"'
    local frlink: list frlink & frame
    if `: list sizeof frlink'==0 {
        di as err "{bf:frlink} variable {bf:`frame'} found"
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
    geoframe get ID, l(ID)
    geoframe get Y, l(Y)
    tempvar tmp
    if (`Y'[1]>=.) {
        // first coordinate missing: assuming polygons start with missing
        qui gen byte `tmp' = `Y'>=.
    }
    else {
        // first coordinate not valid: assuming polygons end with missing
        qui gen byte `tmp' = `Y'[_n-1]>=.
    }
    qui replace `tmp' = `tmp' + `tmp'[_n-1] if `ID'==`ID'[_n-1]
    capt confirm new variable `namelist'
    if _rc==1 exit _rc
    if _rc drop `namelist'
    rename `tmp' `namelist'
    geoframe set PID = `namelist'
end

program _geoframe_append
    gettoken frame 0 : 0, parse(" ,")
    frame `frame' {
        syntax [varlist] [if] [in] [, TARget(namelist) touse(varname numeric) ]
        // local dups: list dups varlist
        // if `"`dups'"'!="" {
        //     di as err "duplicates not allowed in {it:varlist}"
        //     exit 198
        // }
        if "`touse'"!="" {
            if `"`if'`in'"'!="" {
                di as err "touse() not allowed with if or in"
                exit 198
            }
        }
        else {
            marksample touse, novarlist
        }
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
        if `: isalias `V'' {
            di as err "`V' is an alias; may not append to alias variables"
            exit 498
        }
        local TYPE: type `V'
        capt n _check_types `type' `TYPE'
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
            di as err "type mismatch; may not combine long and float"
            exit 109
        }
        exit
    }
    if "`TYPE'"=="long" {
        if "`type'"=="double" exit 499 // => recast
        if "`type'"=="float" {
            di as err "type mismatch; may not combine long and float"
            exit 109
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

version 18
mata:
mata set matastrict on

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


