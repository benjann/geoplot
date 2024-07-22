*! version 1.1.5  22jul2024  Ben Jann

program _geoplot_label
    version 16.1
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0
    ***
    gettoken mlabl 0 : 0, parse(" ,[") match(par)
    if inlist(`"`mlabl'"', "", ",", "if", "in", "[") {
        di as err "must specify labels, either as {it:labelvar} or as "/*
            */ `"{bf:(}"{it:text}" ...{bf:)}"'
        exit 198
    }
    if "`par'"=="("           local mlabl mlabi(`mlabl')
    else if `"`mlabl'"'=="."  local mlabl mlabz
    else                      local mlabl mlabel(`mlabl')
    _parse comma lhs 0 : 0
    syntax [, POSition(str) VPOSition(str) gap(str) ANGle(str) TSTYle(str) /*
           */ SIze(str) COLor(str asis) Format(str) * ]
    if `"`position'"'=="" local position 0
    local opts
    foreach opt in position vposition gap angle size color format {
        if `"``opt''"'!="" local opts `opts' mlab`opt'(``opt'')
    }
    if `"`tstyle'"'!="" local opts `opts' mlabtextstyle(`tstyle')
    local options nolegend msymbol(i) `mlabl' `opts' `options'
    ***
    __geoplot_layer scatter `layer' `p' `frame' `lhs', `options'
    if `layer'<. char LAYER[layertype_`layer'] label
    c_local plot `plot'
    c_local p `p'
end
