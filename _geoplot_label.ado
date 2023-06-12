*! version 0.2.2  12jun2023  Ben Jann

program _geoplot_label
    version 17
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0
    ***
    gettoken mlabl 0 : 0, parse(" ,[")
    if inlist(`"`mlabl'"', "", ",", "if", "in", "[") {
        di as err "must specify variable containing labels"
        exit 198
    }
    _parse comma lhs 0 : 0
    syntax [, POSition(str) VPOSition(str) gap(str) ANGle(str) TSTYle(str) /*
           */ SIze(str) COLor(str) Format(str) * ]
    if `"`position'"'=="" local position 0
    local opts
    foreach opt in position vposition gap angle size color format {
        if `"``opt''"'!="" local opts `opts' mlab`opt'(``opt'')
    }
    if `"`tstyle'"'!="" local opts `opts' mlabtextstyle(`tstyle')
    local options msymbol(i) mlabel(`mlabl') `opts' `options'
    ***
    __geoplot_layer 0 scatter `layer' `p' `frame' `lhs', `options'
    c_local plot `plot'
    c_local p `p'
end
