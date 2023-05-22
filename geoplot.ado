*! version 0.1.5  22may2023  Ben Jann

capt which colorpalette
if _rc==1 exit _rc
local rc_colorpalette = _rc

capt findfile lcolrspace.mlib
if _rc==1 exit _rc
local rc_colrspace = _rc

capt mata: assert(mm_version()>=200)
if _rc==1 exit _rc
local rc_moremata = _rc

if `rc_colorpalette' | `rc_colrspace' | `rc_moremata' {
    if `rc_colorpalette' {
        di as err "{bf:colorpalette} is required; " _c
        di as err "type {stata ssc install palettes, replace}"
    }
    if `rc_colrspace' {
        di as err "{bf:colrspace} is required; " _c
        di as err "type {stata ssc install colrspace, replace}"
    }
    if `rc_moremata' {
        di as err "{bf:moremata} version 2.0.0 or newer is required; " _c
        di as err "type {stata ssc install moremata, replace}"
    }
    exit 499
}

program geoplot, rclass
    version 17
    _parse comma lhs 0 : 0
    syntax [, /*
        */ NOLEGend LEGend LEGend2(str asis) CLEGend CLEGend2(str asis) /*
        */ wmax(numlist max=1 >0) /*
        */ rotate(real 0) /* rotate whole map around midpoint by angle
        */ Margin(numlist max=4 >=0) tight /* margin: l r b t (will be recycled)
        */ ASPECTratio(str) YSIZe(passthru) XSIZe(passthru) SCHeme(passthru) /*
        */ frame(str) * ]
    local legend = `"`legend'`legend2'"'!=""
    local clegend = `"`clegend'`clegend2'"'!=""
    if `clegend' _clegend_k, `clegend2'
    else if !`legend' local legend 2
        // legend = 0: legend not specified, but clegend specified
        // legend = 1: legend specified
        // legend = 2: neither legend nor clegend specified
    _parse_aspectratio `aspectratio' // returns aspectratio, aspectratio_opts
    if "`margin'"=="" local margin 0
    _parse_frame `frame' // returns frame, replace
    
    // parse layers
    _parse expand layer lg : lhs
    if `"`lg_if'"'!="" {
        di as err "global {bf:if} not allowed"
        exit 198
    }
    if `"`lg_in'"'!="" {
        di as err "global {bf:in} not allowed"
        exit 198
    }
    if `"`lg_op'"'!="" {
        error 198
    }
    
    // prepare frame
    local cframe = c(frame)
    tempname main
    frame create `main'
    frame `main' {
    
    // process layers
        gen byte LAYER = .
        char LAYER[Layers] `layer_n'
        gen byte ID = .
        qui set obs 2
        gen double W = _n - 1 // 0 and 1
        local p 0
        local plots
        nobreak {
            capture noisily break {
                mata: _GEOPLOT_ColrSpace_S = ColrSpace() // global object
                forv i = 1/`layer_n' {
                    local ii `i' // for error message
                    gettoken plottype layer : layer_`i', parse(" ,")
                    _parse_plottype `plottype'
                    gettoken lframe : layer, parse(" ,[")
                    if `"`lframe'"'=="" {
                        // frame may have been specified as ""
                        gettoken lframe layer : layer, parse(" ,[")
                        local lframe `"`cframe'"'
                    }
                    else if inlist(`"`lframe'"', ",", "if", "in", "[") {
                        // leave layer as is
                        local lframe `"`cframe'"'
                    }
                    else {
                        // remove frame from layer
                        gettoken lframe layer : layer, parse(" ,[")
                        if `"`lframe'"'=="." local lframe `"`cframe'"'
                    }
                    _geoplot_`plottype' `i' `p' `lframe' `layer' // => plot, p
                    local plots `plots' `plot'
                }
            }
            local rc = _rc
            capt mata mata drop _GEOPLOT_ColrSpace_S // remove global object
            if `rc' {
                if `rc'!=1 {
                    di as err "(error in layer `ii': `plottype' ...)"
                }
                exit `rc'
            }
        }
        if !`p' {
            di as txt "(nothing to plot)"
            exit
        }
        
    // normalize weights
        su W in 3/l, meanonly
        if r(N) {
            if "`wmax'"=="" qui replace W = W / r(max) in 3/l
            else {
                if `wmax'<r(max) {
                    local rmax `: di %18.0g r(max)'
                    di as txt "(maximum weight = {bf:`rmax'};" /*
                        */ " this is larger than {bf:wmax()})"
                }
                qui replace W = W / `wmax' in 3/l
            }
        }
        
    // rotate and process relative (non-rotating) coordinates
        _rotate `rotate'
        capt confirm variable dY, exact
        if _rc==0 {
            qui replace Y = Y + dY if dY<.
            qui replace X = X + dX if dX<.
            drop dY dX
        }
        
    // graph dimensions
        _grdim "`margin'" `aspectratio' // returns yrange, xrange, aratio
        local aspectratio aspectratio(`aratio'`aspectratio_opts')
        if "`tight'"!="" {
            // update ysize and ysize
            _grdim_tight, aratio(`aratio') `scheme' `ysize' `xsize' 
        }
        
    // compile legend
        if "`nolegend'"=="" {
            _legend `legend' `clegend_k', `legend2' // returns legend legend_k
        }
        else local legend legend(off)
        if `clegend' {
            _clegend `legend_k', `clegend2' // returns plot, clegend, clegend_k
            local plots `plots' `plot'
        }
        else local clegend
    
    // draw graph
         local graph /*
            */ graph twoway `plots',/*
            */ `legend' `clegend' /*
            */ graphregion(margin(small) style(none) istyle(none))/*
            */ plotregion(margin(zero) style(none) istyle(none))/*
            */ bgcolor(white) `scheme'/*
            */ `yrange' `xrange' `aspectratio' `ysize' `xsize' `options'
        `graph'
        return local graph `graph'
    }
    
    if "`frame'"!="" {
        local cframe `"`c(frame)'"'
        qui _frame dir
        local framelist = r(contents)
        if `:list frame in framelist' {
            if "`frame'"==`"`cframe'"' { // cannot drop current frame
                frame change `main'
            }
            frame drop `frame'
        }
        frame rename `main' `frame'
        di as txt "(graph data stored as frame {bf:`frame'})"
        if "`frame'"!=`"`cframe'"' {
            frame change `frame'
            di as txt "(current frame now {bf:`frame'})"
        }
    }
end

program _parse_aspectratio
    _parse comma lhs rhs : 0
    if `"`lhs'"'!="" {
        numlist "`lhs'", max(1)
        local lhs `r(numlist)'
    }
    else local lhs 1 // default
    c_local aspectratio `lhs'
    c_local aspectratio_opts `rhs'
end

program _parse_frame
    syntax [name(name=frame)] [, replace ]
    if "`frame'"=="" exit
    if "`replace'"=="" {
        qui _frame dir
        local framelist = r(contents)
        if `:list frame in framelist' {
            di as err "frame {bf:`frame'} already exists"
            exit 499
        }
    }
    c_local frame `frame'
    c_local replace `replace'
end

program _parse_plottype
    local l = strlen(`"`0'"')
    if      `"`0'"'==substr("scatter", 1, max(2,`l')) local 0 scatter
    else if `"`0'"'==substr("labels", 1, max(3,`l'))  local 0 labels
    capt mata: assert(st_islmname(st_local("0")))
    if _rc==1 exit _rc
    if _rc {
        di as err `"`0' invalid plottype"'
        exit 198
    }
    c_local plottype `"`0'"'
end

program _rotate
    if `0'==0 exit
    local r = `0' * _pi / 180
    tempname min max
    foreach v in Y X {
        su `v', mean
        scalar `min' = r(min)
        scalar `max' = r(max)
        capt confirm variable `v'2, exact
        if _rc==0 {
            su `v'2, mean
            scalar `min' = min(`min', r(min))
            scalar `max' = max(`max', r(max))
        }
        tempname `v'mid
        scalar ``v'mid' = (`max'-`min') / 2
    }
    tempvar y x
    qui gen double `y' = Y - `Ymid'
    qui gen double `x' = X - `Xmid'
    qui replace Y = (`x' * sin(`r') + `y' * cos(`r')) + `Ymid'
    qui replace X = (`x' * cos(`r') - `y' * sin(`r')) + `Xmid'
    capt confirm variable Y2, exact
    if _rc==0 {
        qui replace `y' = Y2 - `Ymid'
        qui replace `x' = X2 - `Xmid'
        qui replace Y2 = (`x' * sin(`r') + `y' * cos(`r')) + `Ymid'
        qui replace X2 = (`x' * cos(`r') - `y' * sin(`r')) + `Xmid'
    }
end

program _grdim
    args margin aratio
    tempname min max
    foreach v in X Y {
        tempname `v'min `v'max `v'range
        su `v', mean
        scalar ``v'min' = r(min)
        scalar ``v'max' = r(max)
        capt confirm variable `v'2, exact
        if _rc==0 {
            su `v'2, mean
            scalar ``v'min' = min(``v'min', r(min))
            scalar ``v'max' = max(``v'max', r(max))
        }
        scalar ``v'range' = ``v'max' - ``v'min'
        if "`MRG'"=="" local MRG `margin' // recycle
        gettoken m MRG : MRG
        scalar ``v'min' = ``v'min' - ``v'range' * (`m'/100)
        if "`MRG'"=="" local MRG `margin' // recycle
        gettoken m MRG : MRG
        scalar ``v'max' = ``v'max' + ``v'range' * (`m'/100)
    }
    c_local yrange yscale(off range(`=`Ymin'' `=`Ymax'')) ylabel(none)
    c_local xrange xscale(off range(`=`Xmin'' `=`Xmax'')) xlabel(none)
    c_local aratio = (`Ymax'-`Ymin') / (`Xmax'-`Xmin') * `aratio'
end

program _grdim_tight
     syntax, aratio(str) [ YSIZe(str) XSIZe(str) SCHeme(str) ]
     // ysize specified
     if `"`ysize'"'!="" {
         if `"`xsize'"'!="" exit // nothing to do
         local unit = substr(`"`ysize'"',-2,.)
         if inlist(`"`unit'"', "in", "pt", "cm") {
             local ysize = strtrim(substr(`"`ysize'"',1,strlen(`"`ysize'"')-2))
         }
         else local unit
         local xsize = `ysize' / `aratio'
         c_local xsize xsize(`xsize'`unit')
         exit
     }
     // xsize specified
     if `"`xsize'"'!="" {
         local unit = substr(`"`xsize'"',-2,.)
         if inlist(`"`unit'"', "in", "pt", "cm") {
             local xsize = strtrim(substr(`"`xsize'"',1,strlen(`"`xsize'"')-2))
         }
         else local unit
         local ysize = `xsize' * `aratio'
         c_local ysize ysize(`ysize'`unit')
         exit
     }
     // get ysize from scheme
     if `"`scheme'"'=="" local scheme `"`c(scheme)'"'
     if `"`.__SCHEME.scheme_name'"'!=`"`scheme'"' {
         qui .__SCHEME = .scheme.new, scheme(`scheme')
     }
     local ysize `.__SCHEME.graphsize.y'
     if `"`ysize'"'=="" local ysize 4.5
     local xsize = `ysize' / `aratio'
     c_local ysize ysize(`ysize')
     c_local xsize xsize(`xsize')
end

program _legend
    // syntax
    syntax [anything] [, Layer(numlist int max=1 >0) /*
        */ VERTical HORizontal ASCending DESCending/*
        */ Format(str) noLABel/*
        */ NOMISsing MISsing(str asis) * ]
    if `"`options'"'!="" local options legend(`options')
    _legend_parse_missing, `missing' // returns mis_lab, mis_gap, mis_first
    gettoken force anything : anything
    gettoken ck    anything : anything
    // get legend info
    local k `layer'
    local clayers: char LAYER[Layers_cvar]
    if "`k'"=="" & `force' {
        // get first unused cvar layer
        local k: list clayers - ck
        gettoken k : k
    }
    if !cond("`k'"=="", 0 ,`:list k in clayers') {
        if "`k'"!="" {
            di as txt "(legend omitted; layer `k' does not contain cvar)"
        }
        else if `force'==1 {
            if "`ck'"=="" di as txt "(legend omitted; no cvar layer found)"
            else di as txt "(legend omitted; no unused cvar layer found)"
        }
        c_local legend_k
        c_local legend legend(off)
        exit
    }
    local keys:     char LAYER[Keys_`k']
    local values:   char LAYER[Values_`k']
    local hasmis:   char LAYER[Hasmis_`k']
    local discrete: char LAYER[Discrete_`k']
    local labels `values'
    if `"`discrete'"'!="" {
        if "`label'"=="" {
            local labels: char LAYER[Labels_`k']
        }
    }
    if `"`format'"'!="" {
        capt confirm numeric format `format'
        if _rc local format
    }
    else local format: char LAYER[Format_`k']
    if `"`format'"'=="" local format %7.0g
    // orientation / layout
    local layout size(vsmall) symysize(3) symxsize(3) keygap(1)
    if "`horizontal'"!="" {
        local layout rows(1) colgap(0) stack `layout'
        local reverse = "`descending'"!=""
    }
    else {
        local layout cols(1) rowgap(0) `layout'
        local reverse = "`descending'"==""
    }
    // compile order()
    if `"`hasmis'"'!=""    gettoken mis_key keys : keys
    if `"`nomissing'"'!="" local mis_key
    if !`: list sizeof keys' & "`mis_key'"=="" {
        di as txt "(legend omitted; layer `k' contains no non-missing color keys)"
        c_local legend_k `k'
        c_local legend legend(off)
        exit
    }
    local order
    if `"`discrete'"'!="" {
        foreach key of local keys {
            gettoken lbl labels : labels
            capt confirm number `lbl'
            if _rc==0 {
                local lbl `: di `format' `lbl''
            }
            if `reverse' {
                local order `key' `"`lbl'"' `order'
            }
            else {
                local order `order' `key' `"`lbl'"'
            }
        }
    }
    else {
        gettoken val0 values : values
        local val0 `: di `format' `val0''
        local lp "["
        foreach key of local keys {
            gettoken val values : values
            local val `: di `format' `val''
            if `reverse' {
                local order `key' "`lp'`val0',`val']" `order'
            }
            else {
                local order `order' `key' "`lp'`val0',`val']"
            }
            local val0 `val'
            local lp "("
        }
    }
    // add missing
    if "`mis_key'"!="" {
        local mis_key `mis_key' `mis_lab'
        if `mis_first' {
            if `mis_gap' local order - " " `order'
            local order `mis_key' `order'
        }
        else {
            if `mis_gap' local order `order' - " "
            local order `order' `mis_key'
        }
    }
    // return legend option
    c_local legend_k `k'
    c_local legend legend(order(`order') on all `layout'/*
        */ position(0) bplace(nw) bmargin(zero)/*
        */ region(style(none) margin(zero))) `options'
end

program _legend_parse_missing
    syntax [, Label(str asis) first last nogap ]
    local label = strtrim(`"`label'"')
    if `"`label'"'=="" local label `""no data""'
    else {
        gettoken tmp : label, qed(hasquote)
        if !`hasquote' {
            local label `"`"`label'"'"'
        }
    }
    c_local mis_lab   `"`label'"'
    c_local mis_gap   = "`gap'"==""
    c_local mis_first = `"`first'"'!=""
end

program  _clegend_k
    if c(stata_version)<18 {
        di as err "{bf:clegend()} requires Stata 18"
        exit 9
    }
    syntax [, Layer(numlist int max=1 >0) * ]
    c_local clegend_k `layer'
end

program _clegend
    syntax [anything(name=lk)] [, Layer(numlist int max=1 >0) noLABel/*
        */ MISsing MISsing2(str asis) Format(str) * ]
    if `"`options'"'!=""  local options clegend(`options')
    if `"`missing2'"'!="" local missing missing
    mata: _set_default_and_add_quotes("missing2", "no data")
    local k `layer'
    local clayers: char LAYER[Layers_cvar]
    if "`k'"=="" {
        // ket first unused cvar layer
        local k: list clayers - lk
        gettoken k : k
    }
    if !cond("`k'"=="", 0 ,`:list k in clayers') {
        if "`k'"!="" {
            di as txt "(clegend omitted; layer `k' does not contain cvar)"
        }
        else {
            if "`lk'"=="" di as txt "(clegend omitted; no cvar layer found)"
            else di as txt "(clegend omitted; no unused cvar layer found)"
        }
        c_local clegend_k
        c_local clegend
        c_local plot
        exit
    }
    local values:   char LAYER[Values_`k']
    local colors:   char LAYER[Colors_`k']
    local hasmis:   char LAYER[Hasmis_`k']
    local discrete: char LAYER[Discrete_`k']
    local hasmis   = `"`hasmis'"'!=""
    local discrete = `"`discrete'"'!=""
    if `hasmis' {
        if `"`missing'"'=="" {
            gettoken cmis colors : colors, quotes
            if !`: list sizeof colors' {
                di as txt "(clegend omitted: "/*
                    */ "layer `k' contains no non-missing color keys)"
                c_local clegend_k `k'
                c_local clegend
                c_local plot
                exit
            }
            local hasmis 0
        }
    }
    if `discrete' {
        if "`label'"=="" local LABELS: char LAYER[Labels_`k']
        else             local LABELS `values'
        local i 0
        foreach lbl of local LABELS {
            local labels `labels' `i'.5 `"`lbl'"'
            local ++i
        }
    }
    if `"`format'"'!="" {
        capt confirm numeric format `format'
        if _rc local format
    }
    else local format: char LAYER[Format_`k']
    if `"`format'"'=="" local format format(%7.0g)
    else                local format format(`format')
    qui gen byte CLEG_Y = .
    qui gen byte CLEG_X = .
    qui gen double CLEG_Z = .
    local K: list sizeof values
    local N = `K' + `discrete' + `hasmis' 
    if `N'>_N {
        set obs `N'
    }
    if `discrete' {
        qui replace CLEG_Z = .0001
        local values 0
        forv i = 1/`K' {
            qui replace CLEG_Z = `i' in `=`i'+1'
            local values `values' `i'
        }
        local i = `K' + 1
        if `hasmis' {
            local ++i
            qui replace CLEG_Z = 0 in 1
            qui replace CLEG_Z = -.9999 in `i'
            local labels -.5 `missing2' `labels'
            local values -1 `values'
        }
        local height = min(100, (`N'-`hasmis')*3)
        local zlabels zlabel(`labels', `format' labsize(vsmall) notick labgap(1))/*
            */ zscale(noline)
    }
    else {
        local labels `values'
        if `hasmis' {
            local K: list sizeof values
            local v0: word 1 of `values'
            local v: word `K' of `values'
            local v0 = `v0'- (`v'-`v0')/`K'
            local values `v0' `values'
            local labels `v0' `missing2' `labels'
        }
        gettoken v0 VALUES : values
        gettoken v         : VALUES
        qui replace CLEG_Z = `v0' + (`v'-`v0')/10000 in 1 /* shift first point
            slightly up */
        local i 1
        foreach v of local VALUES {
            local ++i
            qui replace CLEG_Z = `v' in `i'
        }
        local height = min(50, (`N'-`hasmis')*3)
        local zlabels zlabel(`labels', `format' labsize(vsmall))
    }
    c_local plot (scatter CLEG_Y CLEG_X in 1/`i', colorvar(CLEG_Z) colorcuts(`values')/*
        */ colorlist(`colors') colorkeysrange)
    if "`lk'"!="" local bplace bplace(ne)
    else          local bplace bplace(nw)
    c_local clegend ztitle("") `zlabels' /*
        */ clegend(position(0) `bplace' width(3) height(`height')/*
        */ bmargin(l=0 r=0 b=1 t=1) region(margin(zero)))/*
        */ `options'
    c_local clegend_k `k'
end

program _process_coloropts // pass standard color options through ColrSpace
    local opts COLor FColor LColor MColor MFColor MLColor MLABColor
    foreach o of local opts {
        local OPTS `OPTS' `o'(str asis)
    }
    syntax [, `OPTS' * ]
    local opts = strlower("`opts'")
    local OPTS
    foreach o of local opts {
        if `"``o''"'!="" {
            mata: _get_color("`o'")
            local OPTS `OPTS' `o'(``o'')
        }
    }
    c_local options `OPTS' `options'
end

program _geoplot_area
    _layer area `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_line
    _layer line `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_point
    _layer scatter `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_scatter
    _layer scatter `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_labels
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0
    gettoken mlabl 0 : 0, parse(" ,[")
    if inlist(`"`mlabl'"', "", ",", "if", "in", "[") {
        di as err "label: variable name containing labels required"
        exit 198
    }
    _parse comma lhs 0 : 0
    syntax [, /*
        */ POSition(str) VPOSition(str) gap(str) ANGle(str) TSTYle(str) /*
        */ SIze(str) COLor(str) Format(str) * ]
    if `"`position'"'=="" local position 0
    foreach opt in position vposition gap angle size color format {
        if `"``opt''"'!="" local `opt' mlab`opt'(``opt'')
    }
    if `"`tstyle'"'!="" local tstyle mlabtextstyle(`tstyle')
    _layer scatter `layer' `p' `frame' `lhs', /*
        */ msymbol(i) mlabel(`mlabl') `position' `vposition' `gap' /*
        */ `angle' `tstyle' `size' `color' `format' /*
        */ `options'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcspike
    _layer pcspike `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pccapsym
    _layer pccapsym `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcarrow
    _layer pcarrow `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcbarrow
    _layer pcbarrow `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcpoint
    _layer pcscatter `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcscatter
    _layer pcscatter `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pointi
    _layeri scatteri `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_scatteri
    _layeri scatteri `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pci
    _layeri pci `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcarrowi
    _layeri pcarrowi `0'
    c_local plot `plot'
    c_local p `p'
end

program _layeri
    gettoken plottype 0 : 0
    gettoken layer 0 : 0
    gettoken p 0 : 0
    _parse comma lhs 0 : 0
    _process_coloropts `0' // returns options
    local ++p
    char LAYER[Keys_`layer'] `p'
    c_local p `p'
    c_local plot (`plottype' `lhs', `options')
end

program _layer
    // setup
    gettoken plottype 0 : 0
    local hasSHP 0
    local TYPE
    local hasPLV 0
    local PLVopts
    local WGT
    local MLABopts
    local CVARopts cvar(varname numeric) COLORVar(varname numeric)/*
        */ LEVels(str) cuts(numlist sort min=2) DISCRete /*
        */ COLor(str asis) LWidth(str) /*
        */ MISsing(str asis) 
    local YX Y X // coordinate variable names used in plot data
    if `"`plottype'"'=="area" {
        local TYPE   shape
        local hasSHP 1
        local hasPLV 1
        local PLVopts FColor(str asis) EColor(str asis)
        local varlist [varlist(default=none numeric max=1)]
        local colvlist 1
    } 
    else if `"`plottype'"'=="line" {
        local TYPE   shape
        local hasSHP 1
        local varlist [varlist(default=none numeric max=1)]
        local colvlist 1
    }
    else {
        if substr(`"`plottype'"',1,2)=="pc" { // paired coordinate plot
            local TYPE pc
            local varlist [varlist(default=none max=4 numeric)]
            local YX  Y  X Y2 X2 // variable names used in plot data
        }
        else {  // scatter assumed
            local TYPE unit
            local varlist [varlist(default=none max=2 numeric)]
            local WGT [aw iw fw pw/]
        }
        local colvlist 0
        local MLABopts MLabel(varname) MLABVposition(varname numeric) /*
            */ MLABFormat(str)
    }
    gettoken layer 0 : 0
    gettoken p 0 : 0
    gettoken frame 0 : 0
    frame `frame' {
        // syntax
        syntax `varlist' [if] [in] `WGT' [, `PLVopts' `MLABopts'/*
            */ `CVARopts' * ]
        local cuts: list uniq cuts
        capt n _parse_levels `levels' // => levels, method, l_wvar
        if _rc==1 exit _rc
        if _rc {
            di as err "(error in option {bf:levels()}"
            exit _rc
        }
        marksample touse, novarlist
        geoframe get feature, l(FEATURE)
        // check cvar
        if "`cvar'"=="" local cvar `colorvar'
        if `colvlist' {
            if "`cvar'"=="" local cvar `varlist'
            local varlist
        }
        local hasCVAR = `"`cvar'"'!=""
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
            if "`varlist'"!="" local yx `varlist'
            else geoframe get coordinates, flip local(yx) `typeSHP'
            if `:list sizeof yx'!=`: list sizeof YX' {
                di as err "wrong number of coordinate variables"
                exit 498
            }
            local ORG `yx'
            local TGT `YX'
            _get_PLV `hasPLV' `hasCVAR' `"`fcolor'"' `"`FEATURE'"' // => PLV
            if `hasPLV' {
                local ORG `ORG' `PLV'
                local TGT `TGT' PLV
            }
            geoframe get id, local(ID)
            if "`ID'"!="" {
                local ORG `ORG' `ID'
                local TGT `TGT' ID
            }
        }
        // handle weights
        local hasWGT = "`weight'"!=""
        if `hasWGT' {
            capt confirm variable `exp'
            if _rc {
                tempname wvar
                qui gen double `wvar' = `exp' if `touse'
            }
            else local wvar `exp'
            markout `touse' `wvar' // exclude obs with missing weight
            if `hasSHP' {
                tempname WVAR
                local org `org' `wvar'
                local tgt `tgt' `WVAR'
            }
            else local WVAR `wvar'
            local ORG `ORG' `WVAR'
            local TGT `TGT' W
            local wgt "[aw=W]"
        }
        // handle cvar
        if `hasCVAR' {
            local cvarfmt: format `cvar'
            if `hasSHP' {
                tempname CVAR
                local org `org' `cvar'
                local tgt `tgt' `CVAR'
            }
            else local CVAR `cvar'
            local ORG `ORG' `CVAR'
            local TGT `TGT' CVAR
            if "`l_wvar'"!="" {
                tempname L_WVAR
                if `hasSHP' {
                    local org `org' `l_wvar'
                    local tgt `tgt' `L_WVAR'
                    local ORG `ORG' `L_WVAR'
                }
                else local ORG `ORG' `l_wvar'
                local TGT `TGT' `L_WVAR'
           }
        }
        else {
            if `"`color'"'!=""  local options color(`color') `options'
            if `"`lwidth'"'!="" local options lwidth(`lwidth') `options'
        }
        // handle marker labels
        local hasMLAB = `"`mlabel'"'!=""
        if `hasMLAB' {
            if `"`mlabformat'"'!="" confirm format `mlabformat'
            if "`mlabvposition'"!="" {
                if `hasSHP' {
                    tempname MLABPOS
                    local org `org' `mlabvposition'
                    local tgt `tgt' `MLABPOS'
                }
                else local MLABPOS `mlabvposition'
                local ORG `ORG' `MLABPOS'
                local TGT `TGT' MLABPOS
            }
            tempname MLAB
            qui gen strL `MLAB' = ""
            mata: _generate_mlabels("`MLAB'", "`mlabel'", "`mlabformat'",/*
                */ "`touse'")
            if `hasSHP' {
                tempname CVAR
                local org `org' `MLAB'
                local tgt `tgt' `MLAB'
            }
            local ORG `ORG' `MLAB'
            // local TGT `TGT' MLAB (will be done later, because str)
        }
        // inject colors
        _process_coloropts, `options'
    }
    // copy data
    if `hasSHP' {
        // copy relevant variables from unit frame into shape frame
        qui frame `shpframe': geoframe copy `frame' `org', target(`tgt')
        qui frame `shpframe': replace `touse' = 0 if `touse'>=. 
    }
    local n0 = _N + 1
    geoframe varinit double `TGT'
    if `hasMLAB' {
        geoframe varinit strL MLAB
        local TGT `TGT' MLAB
    }
    qui geoframe append `shpframe' `ORG', target(`TGT') touse(`touse')
    local n1 = _N
    if "`TYPE'"=="shape" { // only if plottype area or line
        if !inlist(`"`typeSHP'"', "unit", "pc") { // only if data not unit or pc
            if "`ID'"!="" { // only if ID variable is available
                // remove units without shape data (i.e. units that only have
                // a single observation and for which the coordinate variables
                // are missing)
                mata: _drop_empty_shapes(`n0', `n1', "ID", tokens("`YX'"))
            }
        }
    }
    local n1 = _N
    if `n1'<`n0' {
        c_local plot
        c_local p `p'
        di as txt "(layer `layer' is empty)"
        exit
    }
    local in in `n0'/`n1'
    qui replace LAYER = `layer' `in'
    // handle cvar
    if `hasCVAR' {
        tempname CUTS
        mata: _cvar_cuts("`CUTS'", (`n0', `n1')) // => CUTS, cuts, levels
        _cvar_categorize `CUTS' `in', levels(`levels') `discrete'
            // => clevels hasmis
        if "`discrete'"!="" {
            frame `frame': _cvar_clabels `cvar' `cuts' // => clabels
        }
        if `levels' {
            _cvar_color `levels' `color' // => color
            _cvar_recycle lwidth `levels' "thin" `"`lwidth'"'
         }
        if "`hasmis'"!="" {
            if `"`missing'"'=="" local missing gs14 // default
            mata: _get_color("missing")
            local color `"`missing' `color'"'
        }
    }
    else local clevels 0
    // handle PLV
    if `hasPLV' {
        if `"`ecolor'"'=="" local ecolor white // default for enclaves
        mata: _get_color("ecolor")
        qui levelsof PLV `in'
        local plevels `r(levels)'
    }
    else local plevels .
    // compile options
    local opts
    if `"`plottype'"'=="area" {
        if `"`fcolor'"'!="" local fcolor fcolor(`fcolor')
        local opts `opts' cmissing(n) nodropbase lalign(center)/*
            */ lpattern(solid) 
        if `hasCVAR' {
            local opts `opts' lwidth(thin) lcolor(%0) finten(100)
        }
        else {
            local opts `opts' lwidth(thin) 
            if `"`FEATURE'"'=="water" {
                local opts `opts' color("135 206 235") finten(50) // "SkyBlue"
            }
            else {
                local opts `opts' lcolor(gray)
                if `"`fcolor'"'=="" local opts `opts' fcolor(none)
            }
        }
    }
    else if "`plottype'"'=="line" {
        local opts `opts' cmissing(n) lpattern(solid) 
        if `hasCVAR' {
            local opts `opts'
        }
        else {
            local opts `opts' lwidth(thin) 
            if `"`FEATURE'"'=="water" {
                local opts `opts' color("135 206 235") // "SkyBlue"
            }
            else {
                local opts `opts' lcolor(gray)
            }
        }
    }
    if `hasMLAB' {
        if "`mlabel'"!=""        local opts `opts' mlabel(MLAB)
        if "`mlabvposition'"!="" local opts `opts' mlabvposition(MLABPOS)
        if "`mlabformat'"!=""    local opts `opts' mlabformat(`mlabformat')
    }
    // compile plot
    if `hasWGT' local in inrange(_n,`n0',`n1')) | (_n<3)
    local plot
    local p0 = `p' + 1
    gettoken pl0 : plevels
    foreach pl of local plevels {
        local iff
        if `pl'<. {
            local iff PLV==`pl'
            local enclave = mod(`pl',2)
        }
        else local enclave 0
        if `hasCVAR' {
            local COLOR: copy local color
            local LWIDTH: copy local lwidth
        }
        foreach i of local clevels {
            if `enclave' {
                local IFF `iff'
                if `hasWGT' {
                    if `"`IFF'"'!=""  local IFF if (`IFF' & `in'
                    else              local IFF if (`in'
                }
                else if `"`IFF'"'!="" local IFF if `IFF' `in'
                else                  local IFF `in'
                local IFF `IFF' `wgt'
                local IFF `IFF', fcolor(`ecolor')
                local IFF `IFF' `options'
                local plot `plot' (`plottype' `YX' `IFF')
                local ++p
                continue, break
            }
            local OPTS `opts'
            local IFF `iff'
            if `hasCVAR' {
                foreach opt in color lwidth {
                    local OPT = strupper("`opt'")
                    gettoken tmp `OPT' : `OPT', quotes
                    local OPTS `OPTS' `opt'(`tmp')
                }
                if `"`IFF'"'!="" local IFF `IFF' & CVAR==`i'
                else             local IFF CVAR==`i'
                if `pl'!=`pl0' {
                    // skip empty plots in additional layers
                    qui count `IFF' `in'
                    if r(N)==0 continue
                }
            }
            if `hasWGT' {
                if `"`IFF'"'!=""  local IFF if (`IFF' & `in'
                else              local IFF if (`in'
            }
            else if `"`IFF'"'!="" local IFF if `IFF' `in'
            else                  local IFF `in'
            local IFF `IFF' `wgt'
            local OPTS `OPTS' `fcolor'
            local OPTS `OPTS' `options'
            if `"`OPTS'"'!="" {
                local IFF `IFF', `OPTS'
            }
            local plot `plot' (`plottype' `YX' `IFF')
            local ++p
        }
        if `pl'==`pl0' local p1 `p'
    }
    // return results
    _post_chars `layer' `p0' `p1' `hasCVAR' "`discrete'" "`hasmis'" "`cvarfmt'"/*
            */ `"`cuts'"' `"`clabels'"' `"`color'"'
    c_local plot `plot'
    c_local p `p'
end

program _get_PLV
    args hasPLV hasCVAR fcolor FEATURE
    if !`hasPLV' exit
    if `hasCVAR' {
        if strtrim(`"`fcolor'"')=="none" local hasPLV 0
    }
    else {
        if `"`fcolor'"'=="" & `"`FEATURE'"'!="water" local hasPLV 0
        else if strtrim(`"`fcolor'"')=="none"        local hasPLV 0
    }
    if `hasPLV' {
        geoframe get plevel, l(PLV)
        if "`PLV'"=="" local hasPLV 0
    }
    c_local hasPLV `hasPLV'
    c_local PLV `PLV'
end

program _parse_levels
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

program _cvar_categorize
    syntax anything(name=CUTS) in, levels(str) [ discrete ]
    tempname tmp
    qui gen byte `tmp' = . `in'
    if "`discrete'"!="" {
        forv i=1/`levels' {
            qui replace `tmp' = `i' if CVAR==`CUTS'[1,`i'] `in'
        }
    }
    else {
        forv i=1/`levels' {
            if `i'==1 local iff inrange(CVAR, `CUTS'[1,`i'], `CUTS'[1,`i'+1])
            else      local iff CVAR>`CUTS'[1,`i'] & CVAR<=`CUTS'[1,`i'+1]
            qui replace `tmp' = `i' if `iff' `in'
        }
    }
    qui count if CVAR>=. `in'
    if r(N) { // set missings to 0
        qui replace `tmp' = 0 if CVAR>=. `in'
        local hasmis hasmis
    }
    qui replace CVAR = `tmp' `in'
    if "`hasmis'"!="" local a 0
    else              local a 1
    numlist "`a'/`levels'"
    c_local clevels `r(numlist)'
    c_local hasmis `hasmis'
end

program _cvar_clabels
    gettoken var 0 : 0
    local labname: value label `var'
    if `"`labname'"'=="" {
        local clabels `0'
    }
    else {
        local clabels
        local space
        foreach val of local 0 {
            local lab: label `labname' `val'
            local clabels `"`clabels'`space'`"`lab'"'"'
            local space " "
        }
    }
    c_local clabels `"`clabels'"'
end

program _cvar_color
    gettoken levels 0 : 0
    _parse comma color 0 : 0
    if `"`color'"'=="" local color viridis
    syntax [, n(passthru) IPolate(passthru) * ]
    if `"`n'`ipolate'"'=="" local n n(`levels')
    colorpalette `color', nograph `n' `ipolate' `options'
    local color `"`r(p)'"'
    if `: list sizeof color'<`levels' {
        // recycle or interpolate colors if too few colors have been obtained
        local pclass `"`r(pclass)'"'
        colorpalette `color', nograph n(`levels') class(`pclass')
        local color `"`r(p)'"'
    }
    c_local color `"`color'"'
end

program _cvar_recycle
    args nm k def opt
    if `"`opt'"'=="" local opt `"`def'"'
    else {
        // try numlist
        capt numlist `"`opt'"'
        if _rc==1 _rc
        if _rc==0 {
            local opt `"`r(numlist)'"'
        }
    }
    // expand
    mata: st_local("opt", invtokens(_vecrecycle(`k', tokens(st_local("opt")))))
    c_local `nm' `"`opt'"'
end

program _post_chars
    args i p0 p1 hasCVAR discrete hasmis fmt values labels color
    numlist "`p0'/`p1'"
    char LAYER[Keys_`i'] `r(numlist)'
    if `hasCVAR'==0 exit
    char LAYER[Layers_cvar] `:char LAYER[Layers_cvar]' `i'
    char LAYER[Type_`i'] cvar
    char LAYER[Discrete_`i'] `discrete'
    char LAYER[Hasmis_`i'] `hasmis'
    char LAYER[Format_`i'] `fmt'
    char LAYER[Values_`i'] `values'
    char LAYER[Labels_`i'] `"`labels'"'
    char LAYER[Colors_`i'] `"`color'"'
end

version 17
mata:
mata set matastrict on

void _set_default_and_add_quotes(string scalar lname, string scalar def)
{
    string scalar s
    
    s = strtrim(st_local(lname))
    if (s=="") s = def
    if (substr(s,1,1)!=`"""') {
        if (substr(s,1,2)!=("`"+`"""')) {
            st_local(lname, "`" + `"""' + s + `"""' + "'")
            return
        }
    }
    if (length(tokens(s))>1) st_local(lname, "`" + `"""' + s + `"""' + "'")
    else                     st_local(lname, s)
}

void _drop_empty_shapes(real scalar n0, real scalar n1, string scalar ID,
    string rowvector YX)
{
    real colvector id, p
    real matrix    yx
    
    if (n1<n0) return // no data
    // look for units that only have a single obs
    id = st_data((n0,n1), ID) // assuming data is ordered by ID
    p = (_mm_unique_tag(id) + _mm_unique_tag(id, 1)):==2 // first = last
    p = selectindex(p)
    if (!length(p)) return // all units have more than one obs
    // check whether coordinate variables are missing
    st_view(yx=., (n0,n1), YX)
    p = select(p, rowmissing(yx[p,]):==cols(yx))
    // remove single-obs units for which all coordinate variables are missing
    if (length(p)) st_dropobsin((n0-1):+p) // remove empty obs
}

transmorphic vector _vecrecycle(real scalar k, transmorphic vector x)
{   // x must have at least one element
    real scalar r, c
    
    r = rows(x); c = cols(x)
    if (r>c) return(J(ceil(k/c), 1, x)[|1\k|]) // => rowvector if x is 1x1
    return(J(1, ceil(k/c), x)[|1\k|])
}

void _cvar_cuts(string scalar CUTS, real rowvector range)
{
    string scalar  cuts, method, wvar
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
    // tag first obs of each unit (if ID is available)
    if (st_local("ID")!="") {
        // assuming data is ordered by ID; assuming CVAR is constant
        // within ID
        p = selectindex(_mm_unique_tag(st_data(range, "ID")))
    }
    else p = .
    // CASE 2: discrete
    if (discrete) { // discrete specified
        C = mm_unique(st_data(range, "CVAR")[p])
        C = select(C, C:<.) // remove missing codes
        st_matrix(CUTS, C')
        st_local("cuts", invtokens(strofreal(C)'))
        st_local("levels", strofreal(length(C)))
        return
    }
    // get data
    X = st_data(range, "CVAR")[p]
    minmax = minmax(X)
    // SPECIAL CASE 1: no nonmissing data
    if (minmax==J(1,2,.)) {
        st_matrix(CUTS, J(1,0,.))
        st_local("cuts", "")
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
        st_local("cuts", invtokens(strofreal(minmax)))
        st_local("levels", "1")
        return
    }
    // CASE 3: equidistant intervals
    if (method=="") {
        C = rangen(lb, ub, k+1) 
        st_matrix(CUTS, C')
        st_local("cuts", invtokens(strofreal(C)'))
        st_local("levels", strofreal(length(C)-1))
        return
    }
    // get weights
    wvar  = st_local("L_WVAR")
    if (wvar!="") {
        w = st_data(range, wvar)[p]
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
        C = lb \ _cvar_cuts_kmeans(k, X)
    }
    else exit(error(3499))
    // return results from CASE 4 or 5
    C = _mm_unique(C)
    k = length(C)
    if (C[1]>lb) C[1] = lb // make sure that min is included
    if (C[k]<ub) C[k] = ub // make sure that max is included
    st_matrix(CUTS, C')
    st_local("cuts", invtokens(strofreal(C)'))
    st_local("levels", strofreal(k-1))
}

real colvector _cvar_cuts_kmeans(real scalar k, real colvector X)
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

void _get_color(string scalar lname, | string scalar lname2)
{   /* function assumes that global ColrSpace object "_GEOPLOT_ColrSpace_S"
       exists; maintaining a global is less work than initializing ColrSpace
       in each call */
    real scalar      i
    string scalar    c
    string rowvector C, kw1, kw2
    pointer (class ColrSpace scalar) scalar S
    
    if (args()<2) lname2 = lname
    kw1 = ("none", "bg", "fg", "background", "foreground")
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

end


