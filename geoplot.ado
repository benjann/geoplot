*! version 1.0.0  17jun2023  Ben Jann

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
        */ LEGend LEGend2(str asis) CLEGend CLEGend2(str asis) /*
        */ SBAR SBAR2(str asis) COMPass COMPass2(str asis) /*
        */ ANGle(real 0) tight Margin(str) REFdim(str) ASPECTratio(str) /* 
        */ YSIZe(passthru) XSIZe(passthru) SCHeme(passthru) /*
        */ frame(str) * ]
    local legend  = `"`legend'`legend2'"'!=""
    local clegend = `"`clegend'`clegend2'"'!=""
    if !`legend' & !`clegend' local legend 1
    _parse_aspectratio `aspectratio' // returns aspectratio, aspectratio_opts
    if "`margin'"=="" local margin 0 0 0 0
    else              _parse_margin `margin'
    _parse_refdim `refdim'
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
        /* default variables:
            LAYER:   layer ID
            Y:       Y coordinate
            X:       X coordinate
            W:       weight
          further variables possibly generated along the way:
            PLV      plot level for enclaves and exclaves
            Z:       categorized zvar()
            Y2:      secondary Y coordinate (pc)
            X2:      secondary X coordinate (pc)
            MLAB:    marker labels
            MLABPOS: marker label positions
          helper variables that will be dropped at the end
            cY cX:   centroids for non-rotating objects
        */
        gen byte LAYER = .
        char LAYER[Layers] `layer_n'
        gen double Y = .
        gen double X = .
        qui set obs 2
        gen double W = _n - 1 // 0 and 1
        
    // process layers
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
        
    // rotate map
        _rotate `angle'
        capt drop cY cX
        
    // graph dimensions
        _grdim "`margin'" "`refdim'" `aspectratio' /* returns refsize 
            Ymin Ymax Xmin Xmax yrange xrange aratio */
        local aspectratio aspectratio(`aratio'`aspectratio_opts')
        if "`tight'"!="" {
            // update ysize and ysize
            _grdim_tight, aratio(`aratio') `scheme' `ysize' `xsize' 
        }
        
    // scale bar
        if `"`sbar'`sbar2'"'!="" {
            _scalebar `refsize' `Ymin' `Ymax' `Xmin' `Xmax', `sbar2'
            local plots `plots' `plot'
        }
    
    // compass
        if `"`compass'`compass2'"'!="" {
            _compass `refsize' `Ymin' `Ymax' `Xmin' `Xmax', `compass2'
            local plots `plots' `plot'
        }
        
    // legend
        if `legend' {
            _legend, `legend2' // returns legend
        }
        else local legend legend(off)
    
    // clegend
        if `clegend' {
            _clegend, `clegend2' // returns plot, clegend
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
    }
    
    // returns
    return local graph `graph'
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

program _parse_plottype
    local l = strlen(`"`0'"')
    if      `"`0'"'==substr("scatter", 1, max(2,`l'))   local 0 scatter
    else if `"`0'"'==substr("labels", 1, max(3,`l'))    local 0 label
    else if `"`0'"'==substr("sym:bol", 1, max(2,`l'))   local 0 symbol
    capt mata: assert(st_islmname(st_local("0")))
    if _rc==1 exit _rc
    if _rc {
        di as err `"`0' invalid plottype"'
        exit 198
    }
    c_local plottype `"`0'"'
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

program _parse_margin
    capt numlist `"`0'"', max(4) range(>=0) // margin must be positive
    if _rc==1 exit 1
    if _rc==0 {
        local MRG `r(numlist)'
        forv i = 1/4 {
            if `"`mrg'"'=="" local mrg: copy local MRG // recycle
            gettoken m mrg : mrg
            local margin `margin' `m'
        }
        c_local margin `margin'
        exit
    }
    local l 0
    local r 0
    local b 0
    local t 0
    while (`"`0'"'!="") {
        gettoken m 0 : 0, parse("= ")
        if inlist(`"`m'"',"l","r","b","t") {
            gettoken eq : 0, parse("= ")
            if `"`eq'"'=="=" gettoken eq 0 : 0, parse("= ") // remove "="
            gettoken val 0 : 0
            capt numlist `"`val'"', max(1) range(>=0)
            if _rc==1 exit 1
            if _rc==0 {
                local `m' `val'
                continue
            }
        }
        di as err "invalid syntax in margin()"
        exit 198
    }
    c_local margin `l' `r' `b' `t'
end

program _parse_refdim
    if `"`0'"'=="" exit
    local l = strlen(`"`0'"')
    if      `"`0'"'=="y"                        local 0 y
    else if `"`0'"'=="x"                        local 0 x
    else if `"`0'"'==substr("vertical",1,`l')   local 0 y
    else if `"`0'"'==substr("horizontal",1,`l') local 0 x
    else {
        di as err "invalid specification in refdim()"
        exit 198
    }
    c_local refdim `0'
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
    capt confirm variable cY, exact
    if _rc==0 { // lock non-rotating shapes
        tempvar dY dX
        qui gen double `dY' = Y - cY if cY<.
        qui gen double `dX' = X - cX if cX<.
        qui replace Y = cY if cY<.
        qui replace X = cX if cX<.
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
    if "`dY'"!="" { // restore non-rotating shapes
        qui replace Y = Y + `dY' if cY<.
        qui replace X = X + `dX' if cX<.
    }
end

program _grdim
    args margin refdim aratio
    foreach v in X Y {
        su `v', mean
        local `v'min = r(min)
        local `v'max = r(max)
        capt confirm variable `v'2, exact
        if _rc==0 {
            su `v'2, mean
            local `v'min = min(``v'min', r(min))
            local `v'max = max(``v'max', r(max))
        }
        local `v'range = ``v'max' - ``v'min'
    }
    if      "`refdim'"=="y" local refsize `Yrange'
    else if "`refdim'"=="x" local refsize `Xrange'
    else                    local refsize = min(`Yrange', `Xrange')
    foreach v in X Y {
        gettoken m margin : margin
        local `v'min = ``v'min' - `refsize' * (`m'/100)
        gettoken m margin : margin
        local `v'max = ``v'max' + `refsize' * (`m'/100)
    }
    c_local refsize `refsize' // reference size (before adding margin)
    c_local Ymin `Ymin'       // after adding margin
    c_local Ymax `Ymax'       // after adding margin
    c_local Xmin `Xmin'       // after adding margin
    c_local Xmax `Xmax'       // after adding margin
    c_local yrange yscale(off range(`Ymin' `Ymax')) ylabel(none)
    c_local xrange xscale(off range(`Xmin' `Xmax')) xlabel(none)
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
    syntax [, off Layout(str asis) HORizontal OUTside POSition(str)/*
        */ SIze(passthru) SYMYsize(passthru) SYMXsize(passthru)/*
        */ KEYGap(passthru) COLGap(passthru) ROWGap(passthru)/*
        */ BMargin(passthru) REGion(passthru)/*
        */ order(passthru) Cols(passthru) Rows(passthru) NOCOLFirst COLFirst/*
        */ on all BPLACEment(passthru)/* will be ignored
        */ * ]
    if `"`off'"'!="" {
        c_local legend legend(off)
        exit
    }
    local on
    local all
    local bplacement
    // select layer if layer() is empty
    local zlayers: char LAYER[Layers_Z]
    if `"`layout'"'=="" {
        gettoken layout : zlayers // first layer containing Z
        if "`layout'"=="" {
            c_local legend legend(off)
            exit
        }
    }
    // compile legend
    if `"`order'"'=="" {
        local LAYOUT
        local ncols 0
        local kmax 0
        local nkeys 0
        // - first analyze layout
        while (1) {
            gettoken l layout : layout, parse(".|- ")
            if `"`l'"'=="" continue, break
            if `"`l'"'=="." {
                local ++nkeys
                local LAYOUT `LAYOUT' .
            }
            else if `"`l'"'=="|" {
                if !`nkeys' continue // ignore empty columns
                local ++ncols
                local kmax = max(`kmax', `nkeys')
                local nkeys 0
                local LAYOUT `LAYOUT' |
            }
            else if `"`l'"'=="-" {
                gettoken l : layout, quotes qed(hasquotes)
                local titl
                local space
                while (`hasquotes') {
                    gettoken l layout : layout, quotes
                    local titl `"`titl'`space'`l'"'
                    local space " "
                    gettoken l : layout, quotes qed(hasquotes)
                }
                local LAYOUT `LAYOUT' - `"`titl'"'
                local ++nkeys
            }
            else {
                capt n numlist `"`l'"', int range(>0)
                if _rc==1 exit 1
                if _rc {
                    di as err "(error in legend(layer()))"
                    exit _rc
                }
                local L `r(numlist)'
                foreach l of local L {
                    local lsize: char LAYER[Lsize_`l']
                    if `"`lsize'"'=="" continue
                    if !`lsize' continue
                    local nkeys = `nkeys' + `lsize'
                    local LAYOUT `LAYOUT' `l'
                }
            }
        }
        if `nkeys' { // close last column
            local ++ncols
            local kmax = max(`kmax', `nkeys')
            local LAYOUT `LAYOUT' |
        }
        if !`kmax' { // legend is empty
            c_local legend legend(off)
            exit
        }
        local nkeys 0
        while (1) {
            gettoken l LAYOUT : LAYOUT
            if `"`l'"'=="" continue, break
            if `"`l'"'=="." {
                local ++nkeys
                local order `order' - " "
            }
            else if `"`l'"'=="|" {
                while (`nkeys'<`kmax') {
                    local ++nkeys
                    local order `order' - " "
                }
                local nkeys 0
            }
            else if `"`l'"'=="-" {
                gettoken l LAYOUT : LAYOUT
                local ++nkeys
                local order `order' - `l'
            }
            else {
                local nkeys = `nkeys' + `: char LAYER[Lsize_`l']'
                local order `order' `: char LAYER[Legend_`l']'
            }
        }
        local order order(`order')
    }
    // orientation / layout
    if "`horizontal'"!="" {
        if `"`rows'`cols'"'=="" & "`ncols'"!="" local rows rows(`ncols')
        if "`colfirst'"==""                     local nocolfirst nocolfirst
    }
    else {
        if `"`rows'`cols'"'=="" & "`ncols'"!="" local cols cols(`ncols')
        if "`nocolfirst'"==""                   local colfirst colfirst
        if `"`rowgap'"'==""                     local rowgap rowgap(0)
    }
    local opts `rows'
    local opts `opts' `cols'
    local opts `opts' `nocolfirst' `colfirst'
    local opts `opts' `rowgap'
    if `"`size'"'==""     local size size(vsmall)
    if `"`symysize'"'=="" local symysize symysize(3)
    if `"`symxsize'"'=="" local symxsize symxsize(3)
    if `"`keygap'"'==""   local keygap keygap(1)
    if `"`colgap'"'==""   local colgap colgap(2)
    local opts `opts' `size' `symysize' `symxsize' `keygap' `colgap'
    if `"`position'"'=="" local position 2
    else                  _parse_position `position' // compass => clock
    if "`outside'"!=""    local position position(`position')
    else                  local position position(0) bplace(`position')
    if `"`bmargin'"'==""  local bmargin bmargin(zero)
    if `"`region'"'==""   local region region(style(none) margin(zero))
    local opts `opts' `position' `bmargin' `region' `options'
    // return legend option
    c_local legend legend(`order' on all `opts')
end

program _clegend
    if c(stata_version)<18 {
        di as err "{bf:clegend()} requires Stata 18"
        exit 9
    }
    // syntax
    syntax [, off Layer(numlist int max=1 >0) noLABel MISsing Format(str)/*
        */ OUTside POSition(str) width(passthru) height(passthru)/*
        */ BMargin(passthru) REGion(passthru)/*
        */ BPLACEment(passthru)/* will be ignored
        */ * ]
    if `"`off'"'!="" {
        c_local clegend
        c_local plot
        exit
    }
    local bplacement
    // select layer
    local k `layer'
    local clayers: char LAYER[Layers_C]
    if "`k'"=="" gettoken k : clayers // first layer that has colors
    else local k: list k & clayers
    if "`k'"=="" {
        if "`layer'"!="" di as txt /*
            */"(clegend omitted; layer `layer' does not contain color gradient)"
        else di as txt /*
            */ "(clegend omitted; no layer containing color gradient found)"
        c_local clegend
        c_local plot
        exit
    }
    // collect info on levels, colors, and labels
    local values:   char LAYER[Values_`k']
    local colors:   char LAYER[Colors_`k']
    local discrete: char LAYER[Discrete_`k']
    local hasmis:   char LAYER[Nmiss_`k']
    local hasmis = (`hasmis'!=0)
    if `hasmis' {
        if `"`missing'"'=="" {
            if !`: list sizeof colors' {
                di as txt "(clegend omitted: "/*
                    */ "layer `k' contains no non-missing color keys)"
                c_local clegend
                c_local plot
                exit
            }
            local hasmis 0
        }
    }
    if `hasmis' {
        local labmis:   char LAYER[Labmis_`k']
        if `: list sizeof labmis'>1 {
            local labmis `"`"`labmis'"'"'
        }
        local colors `"`:char LAYER[Colmis_`k']' `colors'"'
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
    // append data for clegend plot
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
            local labels -.5 `labmis' `labels'
            local values -1 `values'
        }
        local hght = min(100, (`N'-`hasmis')*3)
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
            local labels `v0' `labmis' `labels'
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
        local hght = min(40, (`N'-`hasmis')*3)
        local zlabels zlabel(`labels', `format' labsize(vsmall))
    }
    // layout of clegend
    if `"`position'"'==""  local position 4
    else                   _parse_position `position' // compass => clock
    if "`outside'"!=""     local position position(`position')
    else                   local position position(0) bplace(`position')
    if `"`width'"'==""     local width width(3)
    if `"`height'"'==""    local height height(`hght')
    if `"`bmargin'"'==""   local bmargin bmargin(l=0 r=0 b=1 t=1)
    if `"`region'"'==""    local region region(margin(zero))
    local options `position' `width' `height' `bmargin' `region' `options'
    // return clegend plot and clegend option
    c_local plot (scatter CLEG_Y CLEG_X in 1/`i', colorvar(CLEG_Z)/*
        */ colorcuts(`values') colorlist(`colors') colorkeysrange)
    c_local clegend ztitle("") `zlabels' clegend(`options')
end

program _scalebar
    // syntax
    syntax anything [,/*
        */ Length(numlist max=1 >0) Scale(str) n(numlist max=1 int >0) even/*
        */ Units(str) NOLABel LABel(str) TItle(str)/*
        */ Color(passthru) FIntensity(passthru) LWidth(passthru) /*
        */ Height(numlist max=1 >=0 <=100) POSition(str)/*
        */ XMargin(numlist max=1 >=0 <=100)/*
        */ YMargin(numlist max=1 >=0 <=100) * ]
    gettoken refsize anything : anything
    gettoken Ymin    anything : anything // (includes margin)
    gettoken Ymax    anything : anything // (includes margin)
    gettoken Xmin    anything : anything // (includes margin)
    gettoken Xmax    anything : anything // (includes margin)
    if `"`scale'"'=="" local scale 0.001 // 1/1000
    else               local scale = `scale'
    if `scale'>=. | `scale'<=0 {
        di as err "invalid syntax in scale()"
        exit 198
    }
    if "`n'"=="" local n 5
    _parse_scalerbar_label, `label'
    _parse_scalerbar_title `title'
    if `"`color'"'==""       local color color(black)
    if `"`fintensity'"'==""  local fintensity finten(100)
    if `"`lwidth'"'==""      local lwidth lwidth(vthin)
    local options `color' `fintensity' `lwidth' `options'
    if `"`position'"'=="" local position 7
    else                  _parse_position `position' // compass => clock
    if "`xmargin'"=="" local xmargin 1
    if "`ymargin'"=="" local ymargin 3
    if "`height'"==""  local height 1
    // determine length
    local lmax = `Xmax' - `Xmin'
    if !inlist(`position',0,6,12) local lmax = `lmax' * (1-`xmargin'/100)
    if "`length'"=="" {
        _natscale 0 `=`refsize'/3' `=`n'+1'
        local delta = r(delta)
        local LENGTH = `n' * `delta'
        if `LENGTH'>`lmax' {
            di as err "scalebar(): could not determine length; specify length()"
            exit 498
        }
        local length = `LENGTH' * `scale'
    }
    else {
        local LENGTH = `length' / `scale'
        if `LENGTH '>`lmax' {
            di as err "scalebar(): length too large; "/*
                */ "maximum available length is " `lmax'*`scale'
            exit 498
        }
        local delta = `LENGTH' / `n'
    }
    // coordinates of overall bar
    local xmargin = `refsize' * (`xmargin'/100)
    local ymargin = `refsize' * (`ymargin'/100)
    local height  = `refsize' * (`height'/100)
    if inrange(`position',7,11) {
        local X0 = `Xmin' + `xmargin'
        local X1 = `X0' + `LENGTH'
    }
    else if inrange(`position',1,5) {
        local X1 = `Xmax' - `xmargin'
        local X0 =  `X1' - `LENGTH'
    }
    else {
        local X0 = (`Xmin' + `Xmax' - `LENGTH')/2
        local X1 = (`Xmin' + `Xmax' + `LENGTH')/2
    }
    if inrange(`position',4,8) {
        local Y0 = `Ymin' + `ymargin'
        local Y1 = `Y0'   + `height'
    }
    else if inlist(`position',1,2,10,11,12) {
        local Y1 = `Ymax' - `ymargin'
        local Y0 = `Y1'   - `height'
    }
    else {
        local Y0 = (`Ymin' + `Ymax' - `height')/2
        local Y1 = (`Ymin' + `Ymax' + `height')/2
    }
    local b = _N + 4
    qui set obs `b'
    local a = `b' - 3
    mata: st_store((`a',`b'), "Y", (`Y0', `Y0', `Y1', `Y1')')
    mata: st_store((`a',`b'), "X", (`X0', `X1', `X1', `X0')')
    // labels
    if `lab_above' local Ylab `Y1'
    else           local Ylab `Y0'
    if "`nolabel'"=="" {
        local lblmax `: di `lab_format' `length''
        if `"`units'"'!="" {
            local ul = strlen(`"`units'"')
            if `ul' {
                local ul = `ul' + 2
                local lblmax `"{space `ul'}`lblmax' `units'"'
            }
        }
        if `lab_minmax' {
            local LABEL `Ylab' `X0' "0" `Ylab' `X1' `"`lblmax'"'
        }
        else {
            local LABEL `Ylab' `X0' "0"
            forv i=1/`=`n'-1' {
                local x1 = `X0' + `i'*`delta'
                local x1lab = `i'*`delta'*`scale'
                local x1lab `: di `lab_format' `x1lab''
                local LABEL `LABEL' `Ylab' `x1' "`x1lab'"
            }
            local LABEL `LABEL' `X0' "0" `Ylab' `X1' `"`lblmax'"'
        }
        if `lab_above' local lab_opts place(n) `lab_opts'
        else           local lab_opts place(s) `lab_opts'
        local LABEL text(`LABEL', `lab_opts')
    }
    else local LABEL
    // title
    if `"`title'"'!="" {
        if `ti_below' local Ylab `Y0'
        else          local Ylab `Y1'
        local x1 = (`X0' + `X1') / 2
        local TITLE `Ylab' `x1' `"`title'"'
        if `ti_below' local ti_opts place(s) `ti_opts'
        else          local ti_opts place(n) `ti_opts'
        local TITLE text(`TITLE', `ti_opts')
    }
    else local TITLE
    // plot for overall bar
    local plot (area Y X in `a'/`b', `LABEL' `TITLE' `options')
    // add stripes
    local b = _N
    local a = `b' + 1
    if "`even'"!="" local i 1
    else            local i 2
    forv i = `i'(2)`n' {
        local b = _N + 5
        qui set obs `b'
        local x0 = `X0' + (`i'-1)*`delta'
        local x1 = `X0' + `i'*`delta'
        mata: st_store((`b'-4,`b'-1), "Y", (`Y0', `Y0', `Y1', `Y1')')
        mata: st_store((`b'-4,`b'-1), "X", (`x0', `x1', `x1', `x0')')
    }
    if `a'<=`b' {
        local plot `plot' (area Y X in `a'/`b', `options'/*
            */ cmissing(n) fcolor(white))
    }
    // returns
    c_local plot `plot'
end

program _parse_scalerbar_label
    syntax [, minmax Above Format(str) SIze(passthru) Margin(passthru) * ]
    if `"`format'"'=="" local format %8.0g
    if `"`size'"'==""   local size   size(vsmall) 
    local options `size' `options'
    if `"`margin'"'=="" local margin margin(vsmall)
    local options `margin' `options'
    c_local lab_minmax = "`minmax'"!=""
    c_local lab_above = "`above'"!=""
    c_local lab_format `"`format'"'
    c_local lab_opts `options'
end

program _parse_scalerbar_title
    _parse comma title 0 : 0
    syntax [, Below SIze(passthru) Margin(passthru) * ]
    if `"`size'"'==""   local size size(vsmall) 
    local options `size' `options'
    if `"`margin'"'=="" local margin margin(vsmall)
    local options `margin' `options'
    c_local title `"`title'"'
    c_local ti_below = "`below'"!=""
    c_local ti_opts `options'
end

program _compass
    // syntax
    syntax anything [,/*
        */ Type(int 1) /*
        */ ANGle(real 0)/*
        */ SIze(numlist max=1 >=0)/*
        */ Color(passthru) FIntensity(passthru) LWidth(passthru) /*
        */ NOLABel LABel LABel2(str)/*
        */ noCIRcle noMSPikes /*
        */ POSition(str)/*
        */ XMargin(numlist max=1 >=0 <=100)/*
        */ YMargin(numlist max=1 >=0 <=100) * ]
    gettoken refsize anything : anything
    gettoken Ymin    anything : anything // (includes margin)
    gettoken Ymax    anything : anything // (includes margin)
    gettoken Xmin    anything : anything // (includes margin)
    gettoken Xmax    anything : anything // (includes margin)
    if `"`size'"'==""   local size 5
    if `"`color'"'==""  local color color(black)
    if `"`lwidth'"'=="" local lwidth lwidth(vthin)
    local options `color' `lwidth' `options'
    if `"`fintensity'"'=="" local fintensity finten(100)
    local aoptions lalign(center) `fintensity' `options' cmissing(n) nodropbase
    if `"`label2'"'!="" local label label
    _parse_compass_label, `label2'
    if `"`lab_color'"'=="" local lab_color `color'
    if "`nolabel'"!="" local label // nolabel takes precedence
    if `"`position'"'=="" local position 5
    else                  _parse_position `position' // compass => clock
    if "`xmargin'"=="" local xmargin 2
    if "`ymargin'"=="" local ymargin 2
    // compass elements
    local a0 = _N + 1
    local plot
    if `type'==1 {
        if "`circle'"=="" {
            mata: _compass_circle(100, .7, 0) // inner circle
            local plot (line Y X in `a'/`b', `options')
            mata: _compass_circle(100, .775, .05) // outer circle
            local plot `plot' (area Y X in `a'/`b', `aoptions')
        }
        if "`mspikes'"=="" {
            mata: _compass_spike1(.7, `angle'+45, 1) // inner empty spikes
            local plot `plot' (area Y X in `a'/`b', `aoptions' fcolor(white))
            mata: _compass_spike1(.7, `angle'+45, -1) // inner filled spikes
            local plot `plot' (area Y X in `a'/`b', `aoptions')
        }
        mata: _compass_spike1(1, `angle', 1) // outer empty spikes
        local plot `plot' (area Y X in `a'/`b', `aoptions' fcolor(white))
        mata: _compass_spike1(1, `angle', -1) // outer filled spikes
        local plot `plot' (area Y X in `a'/`b', \`LABEL' `aoptions' )
        // position of labels: local LABEL will be filled in later on
        if "`nolabel'"=="" {
            mata: _compass_label1(1 + `lab_gap'/100, `angle')
            local label label
        }
    }
    else if `type'==2 {
        mata: _compass_spike2(`angle')
        local plot `plot' (area Y X in `a'/`b', \`LABEL' `aoptions')
        // position of label: local LABEL will be filled in later on
        if "`nolabel'"=="" {
            mata: _compass_store(0, 0.5 + `lab_gap'/100, `angle')
            local label label
        }
    }
    else if `type'==3 {
        if "`circle'"=="" {
            mata: _compass_circle(100, .55,  0)
            local plot (line Y X in `a'/`b', `options')
        }
        mata: _compass_spike3(`angle', -1) // empty spike (south)
        local plot `plot' (area Y X in `a'/`b', `aoptions' fcolor(white))
        mata: _compass_spike3(`angle', 1) // filled spike (north)
        local plot `plot' (area Y X in `a'/`b', \`LABEL' `aoptions')
        if "`label'"!="" mata: _compass_store(0, 0.55+`lab_gap'/100, `angle')
    }
    else {
        di as err "invalid specification in type()"
        exit 198
    }
    // adjust size
    local SIZE = `refsize' * (`size'/100)
    qui replace Y = Y * `SIZE' in `a0'/`b'
    qui replace X = X * `SIZE' in `a0'/`b'
    // adjust position
    local xmargin = `refsize' * (`xmargin'/100)
    local ymargin = `refsize' * (`ymargin'/100)
    su Y in `a0'/`b', meanonly
    local ymin = r(min)
    local ymax = r(max)
    su X in `a0'/`b', meanonly
    local xmin = r(min)
    local xmax = r(max)
    local rc 0
    if inrange(`position',7,11) {
        local X0 = `Xmin' + `xmargin' - `xmin'
        if (`X0'+`xmax')>`Xmax' local rc 1
    }
    else if inrange(`position',1,5) {
        local X0 = `Xmax' - `xmargin' - `xmax'
        if (`X0'+`xmin')<`Xmin' local rc 1
    }
    else {
        local X0 = (`Xmin' + `Xmax')/2
        if (`X0'+`xmax')>`Xmax' local rc 1
        if (`X0'+`xmin')<`Xmin' local rc 1
    }
    if inrange(`position',4,8) {
        local Y0 = `Ymin' + `ymargin' - `ymin'
        if (`Y0'+`ymax')>`Ymax' local rc 1
    }
    else if inlist(`position',1,2,10,11,12) {
        local Y0 = `Ymax'-`ymargin' - `ymax'
        if (`Y0'+`ymin')<`Ymin' local rc 1
    } 
    else {
        local Y0 = (`Ymin' + `Ymax')/2
        if (`Y0'+`ymax')>`Ymax' local rc 1
        if (`Y0'+`ymin')<`Ymin' local rc 1
    }
    if `rc' {
         di as err "size of compass to large; reduce size()"
         exit 498
    }
    qui replace Y = Y + `Y0' in `a0'/`b'
    qui replace X = X + `X0' in `a0'/`b'
    // collect labels
    if "`label'"!="" {
        local LABEL
        local lbls "N S E W"
        forv i=`a'/`b' {
            gettoken lbl lbls : lbls
            local y `: di %12.0g Y[`i']'
            local x `: di %12.0g X[`i']'
            local LABEL `LABEL' `y' `x' `"`lbl'"'
        }
        local LABEL text(`LABEL', `lab_color' `lab_opts')
    }
    // returns
    c_local plot `plot'
end

program _parse_compass_label
    syntax [, Gap(numlist max=1 >=0) Color(passthru) SIze(passthru) * ]
    if "`gap'"==""    local gap 30
    if `"`size'"'=="" local size size(vsmall)
    c_local lab_gap   `gap'
    c_local lab_color `color'
    c_local lab_opts  `size' `options'
end

program _parse_position
    capt confirm number `0'
    if _rc==1 exit 1
    if _rc==0 {
        capt numlist `"`0'"', int max(1) range(>=0 <=12)
        if _rc==1 exit 1
        if _rc==0 exit // position is valid clockpos
    }
    local l = strlen(`"`0'"')
    if      `"`0'"'==substr("north", 1, `l')        local 0 12
    else if `"`0'"'==substr("top", 1, `l')          local 0 12
    else if `"`0'"'==substr("neast", 1, max(2,`l')) local 0 1 // or 2
    else if `"`0'"'==substr("east", 1, `l')         local 0 3
    else if `"`0'"'==substr("right", 1, `l')        local 0 3
    else if `"`0'"'==substr("seast", 1, max(2,`l')) local 0 5 // or 4
    else if `"`0'"'==substr("south", 1, `l')        local 0 6
    else if `"`0'"'==substr("bottom", 1, `l')       local 0 6
    else if `"`0'"'==substr("swest", 1, max(2,`l')) local 0 7 // or 8
    else if `"`0'"'==substr("west", 1, `l')         local 0 9
    else if `"`0'"'==substr("left", 1, `l')         local 0 9
    else if `"`0'"'==substr("nwest", 1, max(2,`l')) local 0 11 // 10
    else if `"`0'"'==substr("center", 1, `l')       local 0 0
    else {
        di as err "invalid syntax in position()"
        exit 198
    }
    c_local position `0'
end

program _geoplot_area
    __geoplot_layer 0 area `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_line
    __geoplot_layer 0 line `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_point
    __geoplot_layer 0 scatter `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_scatter
    __geoplot_layer 0 scatter `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcspike
    __geoplot_layer 0 pcspike `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pccapsym
    __geoplot_layer 0 pccapsym `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcarrow
    __geoplot_layer 0 pcarrow `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcbarrow
    __geoplot_layer 0 pcbarrow `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcpoint
    __geoplot_layer 0 pcscatter `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcscatter
    __geoplot_layer 0 pcscatter `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pointi
    __geoplot_layer 1 scatteri `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_scatteri
    __geoplot_layer 1 scatteri `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pci
    __geoplot_layer 1 pci `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_pcarrowi
    __geoplot_layer 1 pcarrowi `0'
    c_local plot `plot'
    c_local p `p'
end

program _geoplot_symboli
    gettoken layer 0 : 0
    gettoken p 0 : 0
    _parse comma args 0 : 0
    tempname frame
    frame create `frame' _Y _X
    while (`"`args'"'!="") {
        gettoken y args : args
        local y = real(`"`y'"')
        gettoken x args : args
        local x = real(`"`x'"')
        frame post `frame' (`y') (`x')
    }
    _geoplot_symbol `layer' `p' `frame' `0'
    c_local plot `plot'
    c_local p `p'
end

version 17
mata:
mata set matastrict on

void _compass_store(real colvector X, real colvector Y, real colvector A)
{
    real scalar    a, b
    real colvector R
    
    R = A * (pi() / 180)
    a = st_nobs() + 1
    b = rows(X)
    st_addobs(b)
    b = a + b - 1
    st_store((a,b), "X", X :* cos(R) - Y :* sin(R))
    st_store((a,b), "Y", X :* sin(R) + Y :* cos(R))
    st_local("a", strofreal(a))
    st_local("b", strofreal(b))
}

void _compass_circle(real scalar n, real scalar scale, real scalar wd)
{
    real colvector X, Y, A
    
    X = J(n+1, 1, scale)
    Y = J(n+1, 1, 0)
    A = (0::n) * (360/n)
    if (wd) {
        X = X \ J(n+1, 1, scale+wd)
        Y = Y \ J(n+1, 1, 0)
        A = A \ -A
    }
    _compass_store(X, Y, A)
}

void _compass_spike1(real scalar scale, real scalar angle, real scalar dir)
{
    real scalar    i
    real colvector A
    
    A = J(20, 1, .)
    for (i=1;i<=4;i++) A[|i*5-4 \ i*5|] = J(5, 1, angle + ((i-1) * 90))
    _compass_store(J(4, 1, scale * (0, 1,     .15, 0, .)'),
                   J(4, 1, scale * (0, 0, dir*.15, 0, .)'), A)
}

void _compass_label1(real scalar scale, real scalar angle)
{
    _compass_store(scale * (0,  0, 1, -1)',
                   scale * (1, -1, 0,  0)', angle)
}

void _compass_spike2(real scalar angle)
{
    _compass_store(( 0, -.3,    0,  .3,  0)',
                   (.5, -.5, -.25, -.5, .5)', angle)
}

void _compass_spike3(real scalar angle, real scalar dir)
{
    _compass_store(dir * ( 0, -.125, .125,  0)',
                   dir * (.5,     0,    0, .5)', angle)
}

end
