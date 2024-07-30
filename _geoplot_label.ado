*! version 1.1.6  29jul2024  Ben Jann

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
    syntax [, Msymbol(passthru) MSIze(passthru)/*
           */ POSition(str) VPOSition(str) gap(str) ANGle(str) TSTYle(str)/*
           */ SIze(str) COLor(str asis) Format(str)/*
           */ Justification(str) ALign(str)/* undocumented
           */ MLABPosition(passthru) MLABVposition(passthru) MLABGap(passthru)/*
           */ MLABANGle(passthru) MLABTextstyle(passthru) MLABSize(passthru)/*
           */ MLABColor(passthru) MLABFormat(passthru)/*
           */ MLABJUSTification(passthru) mlabalign(passthru)/* undocumented
           */ * ]
    if `"`vposition'`mlabvposition'`position'"'=="" local position 0
    if `"`tstyle'"'!=""   local textstyle `"`tstyle'"'
    local opts
    foreach opt in position vposition gap angle size color format textstyle/*
        */ justification align {
        if `"`mlab`opt''"'!="" { // mlab...() takes precedence
            local opts `opts' `mlab`opt''
        }
        else if `"``opt''"'!="" {
            local opts `opts' mlab`opt'(``opt'')
        }
    }
    if `"`msymbol'"'=="" local msymbol msymbol(i)
    if `"`msize'"'==""   local msize msize(0)
    local options `msymbol' `msize' `mlabl' `opts' `options'
    ***
    __geoplot_layer scatter `layer' `p' `frame' `lhs', `options'
    if `layer'<. char LAYER[layertype_`layer'] label
    c_local plot `plot'
    c_local p `p'
end
