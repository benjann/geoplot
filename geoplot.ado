*! version 1.1.2  30sep2023  Ben Jann

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

program geoplot
    version 16.1
    nobreak {
        capture noisily break {
            mata: _GEOPLOT_ColrSpace_S = ColrSpace() // add global object
            _geoplot `0'
        }
        local rc = _rc
        capt mata mata drop _GEOPLOT_ColrSpace_S     // remove global object
        exit `rc'
    }
end

program _geoplot, rclass
    _parse comma lhs 0 : 0
    syntax [, /*
        */ NOLEGend LEGend LEGend2(str asis) CLEGend CLEGend2(str asis)/*
        */ SBAR SBAR2(str asis) COMPass COMPass2(str asis)/*
        */ ANGle(real 0) tight Margin(str) REFdim(str) ASPECTratio(str)/*
        */ YSIZe(passthru) XSIZe(passthru) SCHeme(passthru) /*
        */ frame(str) NOGRAPH * ]
    local legend  = `"`legend'`legend2'"'!=""
    local clegend = `"`clegend'`clegend2'"'!=""
    if !`legend' & !`clegend' & "`nolegend'"=="" local legend 1
    _parse_aspectratio `aspectratio' // returns aspectratio, aspectratio_opts
    if "`margin'"=="" local margin 0 0 0 0
    else              _parse_margin `margin'
    _parse_refdim `refdim'
    _parse_frame `frame' // returns frame, replace, nocurrent
    
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
        char LAYER[layers] `layer_n'
        gen double Y = .
        gen double X = .
        qui set obs 2
        gen double W = _n - 1 // 0 and 1
        
    // process layers
        local p 0
        local plots
        forv i = 1/`layer_n' {
            // provide tempnames for returns (matrices)
            if `"`: char LAYER[CUTS]'"'=="" {
                tempname TMP
                char LAYER[CUTS] `TMP'
            }
            if `"`: char LAYER[NOBS]'"'=="" {
                tempname TMP
                char LAYER[NOBS] `TMP'
            }
            // parse plottype and frame
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
            // generate plot
            capt n _geoplot_`plottype' `i' `p' `lframe' `layer' // => plot, p
            if _rc==1 exit 1
            if _rc {
                di as err "(error in layer `i': `plottype' ...)"
                exit _rc
            }
            local plots `plots' `plot'
        }
        char LAYER[CUTS]
        char LAYER[NOBS]
        if !`p' {
            return scalar layers = 0
            char LAYER[layers]
            di as txt "(nothing to plot)"
            exit
        }
        
    // rotate map
        _rotate `angle'
        capt drop cY cX
        
    // zoom
        _zoom `p' `layer_n' `options' // updates options
        local plots `plots' `plot'
        
    // graph dimensions
        _grdim, margin(`margin') refdim(`refdim') aratio(`aspectratio')/*
            */ `options' // returns refsize Ymin Ymax Xmin Xmax aratio options
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
            _legend, `legend2' // returns legend, legend_pos
        }
        else local legend legend(off)
        
    // clegend
        if `clegend' {
            _clegend `legend_pos', `clegend2' _gropts(`options') // plot clegend
            local plots `plots' `plot'
        }
        else local clegend
        
    // move char to r()
        forv i = `layer_n'(-1)1 {
            local lchars
            local schars
            local mchars
            if `"`: char LAYER[hasz_`i']'"'=="1" {
                local schars `schars' z_discrete
                local lchars `lchars' z_colors
                local schars `schars' z_hascol z_hasmis
                local mchars `mchars' z_nobs z_levels
            }
            local schars `schars' hasz
            local lchars `lchars' labels keys
            foreach char of local lchars {
                return local `char'_`i' `"`:char LAYER[`char'_`i']'"'
                char LAYER[`char'_`i']
            }
            foreach char of local schars {
                return scalar `char'_`i' = `:char LAYER[`char'_`i']'
                char LAYER[`char'_`i']
            }
            foreach char of local mchars {
                local TMP: char LAYER[`char'_`i']
                char LAYER[`char'_`i']
                return matrix `char'_`i' = `TMP'
            }
            char LAYER[z_reverse_`i']
            char LAYER[z_mleg_`i']
            char LAYER[z_format_`i']
        }
        return scalar layers = `layer_n'
        char LAYER[layers]
        
    // draw graph
        local graph /*
            */ graph twoway `plots', `legend' `clegend' /*
            */ graphregion(margin(small) style(none) istyle(none))/*
            */ plotregion(margin(zero) style(none) istyle(none))/*
            */ bgcolor(white) `scheme' `aspectratio' `ysize' `xsize' `options'
        if "`nograph'"=="" {
            `graph'
        }
    }
    
    // returns
    return local legend `legend'
    return local graph `graph'
    if "`frame'"!="" {
        local cframe `"`c(frame)'"'
        capt confirm new frame `frame'
        if _rc==1 exit 1
        if _rc {
            if "`frame'"==`"`cframe'"' { // cannot drop current frame
                frame change `main'
                local cframe `frame'
            }
            frame drop `frame'
        }
        frame rename `main' `frame'
        di as txt "(graph data stored as frame {bf:`frame'})"
        if "`nocurrent'"=="" {
            if "`frame'"!=`"`cframe'"' {
                frame change `frame'
                di as txt "(current frame now {bf:`frame'})"
            }
        }
    }
end

program _parse_plottype
    local l = strlen(`"`0'"')
    if      `"`0'"'==substr("scatter", 1, max(2,`l'))   local 0 scatter
    else if `"`0'"'==substr("labels", 1, max(3,`l'))    local 0 label
    else if `"`0'"'==substr("symbol", 1, max(3,`l'))    local 0 symbol
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
    syntax [name(name=frame)] [, replace NOCURrent ]
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
    c_local nocurrent `nocurrent'
end

program _rotate
    if `0'==0 exit
    local r = `0' * _pi / 180
    capt confirm variable Y2, exact
    if _rc==1 exit 1
    local hasXY2 = _rc==0
    tempname min max
    foreach v in Y X {
        su `v', mean
        scalar `min' = r(min)
        scalar `max' = r(max)
        if `hasXY2' {
            su `v'2, mean
            scalar `min' = min(`min', r(min))
            scalar `max' = max(`max', r(max))
        }
        tempname `v'mid
        scalar ``v'mid' = (`max'+`min') / 2
    }
    capt confirm variable cY, exact
    if _rc==1 exit 1
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
    if `hasXY2' {
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

program _zoom
    gettoken p 0 : 0
    gettoken n options : 0
    numlist "1/`n'"
    local layers `r(numlist)'
    while (1) {
        capt n __zoom `p' `layers', `options' // p plot done options layers
        if _rc==1 exit 1
        if _rc {
            di as err "error in zoom()"
            exit _rc
        }
        if `done' continue, break
        local plots `plots' `plot'
    }
    c_local options `options'
    c_local plot `plots'
    c_local p `p'
end

program __zoom
    // look for zoom() option
    gettoken p 0 : 0
    _parse comma LAYERS 0 : 0
    syntax [, zoom(passthru) * ]
    c_local options `options'
    if `"`zoom'"'=="" {
        c_local done 1
        exit                // no (more) zoom() option
    }
    c_local done 0
    syntax [, zoom(str) * ]
    if `"`zoom'"'=="" exit // zoom() specified, but empty
    // syntax: <layers>: scale [offset angle] [, options]
    // - parse <layers>
    _on_colon_parse `zoom'
    local args `"`s(after)'"'
    numlist `"`s(before)'"', int range(>0)
    local layers `r(numlist)'
    local layers: list layers & LAYERS
    if "`layers'"=="" exit // no (available) layers selected
    c_local LAYERS: list LAYERS - layers // update list of remaining layers
    // - parse rest
    _parse comma args 0 : args
    numlist `"`args'"', max(3)
    local args `r(numlist)'
    gettoken scale  args : args
    gettoken offset args : args
    gettoken angle  args : args
    if "`scale'"==""  local scale 1
    if "`offset'"=="" local offset 0
    if "`angle'"==""  local angle 0
    local r = mod(`angle', 360) * _pi / 180
    if `"`args'"'!="" exit 198
    syntax [, ABSolute box BOX2(str) CIRcle CIRcle2(str) PADding(real 0) /*
        */ NOCONnect CONnect CONnect2(str)/*
        */ LPattern(passthru) LWidth(passthru) LColor(passthru)/*
        */ LAlign(passthru) LSTYle(passthru) PSTYle(passthru) * ]
    if `"`connect2'"'!="" local connect connect
    if `"`circle2'"'!=""  local circle circle
    if `"`box2'"'!=""     local box box
    if "`box'"!="" & "`circle'"!="" {
        di as err "zoom(): only one of {bf:box} and {bf:circle} allowed"
        exit 198
    }
    _zoom_parse_box, `box2' `circle2'
    if "`box_none'"!="" & "`connect'"=="" local noconnect noconnect
    // mark sample
    tempvar touse
    gen byte `touse' = 0
    foreach l of local layers {
        qui replace `touse' = 1 if LAYER==`l'
    }
    qui replace `touse' = 0 if X>=. | Y>=.
    // obtain bounding box / minimum enclosing circle
    if "`circle'"!="" {
        mata: _st_welzl() // returns local Xmid Ymid R
    }
    else {
        su X if `touse', mean
        local Xmin = r(min)
        local Xmax = r(max)
        local Xmid = (`Xmin' + `Xmax') / 2
        su Y if `touse', mean
        local Ymin = r(min)
        local Ymax = r(max)
        local Ymid = (`Ymin' + `Ymax') / 2
        local R = sqrt((`Xmax'-`Xmid')^2 + (`Ymax'-`Ymid')^2) // half diagonal
    }
    // compute offset (if necessary) as a percentage of the diameter
    if "`absolute'"=="" local offset =/*
        */ `offset'/100 * `R' * `scale' * (1 + `padding'/100)
    // new midpoint
    local YMID = `Ymid' + `offset' * sin(`r')
    local XMID = `Xmid' + `offset' * cos(`r')
    // new position and size
    qui replace Y = (Y - `Ymid') * `scale' + `YMID' if `touse'
    qui replace X = (X - `Xmid') * `scale' + `XMID' if `touse'
    // also transform Y2, X2 if present
    capt confirm variable Y2, exact
    if _rc==1 exit 1
    if _rc==0 {
        qui replace Y2 = (Y2 - `Ymid') * `scale' + `YMID' if `touse'
        qui replace X2 = (X2 - `Xmid') * `scale' + `XMID' if `touse'
    }
    // plot box or MEC
    local plots
    if `"`lwidth'"'=="" local lwidth lwidth(.15)
    if `"`lcolor'"'=="" local lcolor lcolor(gray)
    foreach opt in lpattern lwidth lcolor lalign lstyle pstyle {
        local options ``opt'' `options'
        local opt_`opt' ``opt''
    }
    if "`circle'"!="" {
        local s = `R' * (1 + `padding'/100)
        local S = `s' * `scale'
        if "`box_none'"=="" {
            if "`box_destination'`box_origin'"=="" {
                local box_destination box_destination
                local box_origin box_origin
            }
            if "`box_origin'"!="" {
                _geoplot_symboli . `p' `Xmid' `Ymid' `s', `options'
                local plots `plots' `plot'
            }
            if "`box_destination'"!="" {
                _geoplot_symboli . `p' `XMID' `YMID' `S', `options'
                local plots `plots' `plot'
            }
        }
    }
    else {
        local s  = (`Xmax' - `Xmin')
        if `s' local hr = (`Ymax' - `Ymin') / (`Xmax' - `Xmin')
        else { // x-size 0
            local s  = (`Ymax' - `Ymin')
            if `s' {
                local hr = (`Xmax' - `Xmin') / (`Ymax' - `Ymin')
                local options angle(90) `options'
            }
            else local hr 1 // x-size and y-size zero 
        }
        local options shape(square) ratio(`hr') `options'
        local s = `s' / 2 * sqrt(2) * (1 + `padding'/100)
        local S = `s' * `scale'
        local Ymax = `Ymid' + `s' * sin( .25*_pi) * `hr'
        local Ymin = `Ymid' + `s' * sin(-.25*_pi) * `hr'
        local Xmax = `Xmid' + `s' * cos( .25*_pi)
        local Xmin = `Xmid' + `s' * cos( .75*_pi)
        local YMAX = `YMID' + `S' * sin( .25*_pi) * `hr'
        local YMIN = `YMID' + `S' * sin(-.25*_pi) * `hr'
        local XMAX = `XMID' + `S' * cos( .25*_pi)
        local XMIN = `XMID' + `S' * cos( .75*_pi)
        if "`box'"!="" {
            if "`box_none'"=="" {
                if "`box_destination'`box_origin'"=="" {
                    local box_destination box_destination
                    local box_origin box_origin
                }
                if "`box_origin'"!="" {
                    _geoplot_symboli . `p' `Xmid' `Ymid' `s', `options'
                    local plots `plots' `plot'
                }
                if "`box_destination'"!="" {
                    _geoplot_symboli . `p' `XMID' `YMID' `S', `options'
                    local plots `plots' `plot'
                }
            }
        }
    }
    // plot tangents
    if "`noconnect'"=="" & "`connect'`box'`circle'"!="" {
        local 0 `", `connect2'"'
        syntax [, LPattern(passthru) LWidth(passthru) LColor(passthru)/*
            */ LAlign(passthru) LSTYle(passthru) PSTYle(passthru) ]
        local options
        foreach opt in lpattern lwidth lcolor lalign lstyle pstyle {
            if `"``opt''"'=="" local `opt' `opt_`opt''
            local options `options' ``opt''
        }
        if "`circle'"!="" {
            mata: _st_circle_tangents(`Xmid', `Ymid', `s', `XMID',/*
                */ `YMID', `S')
        }
        else {
            mata: _zoom_boxconnect(`Xmax',`Xmin',`Ymax',`Ymin',/*
                */`XMAX',`XMIN',`YMAX',`YMIN')
        }
        if `"`YX'"'!="" {
            _geoplot_pci . `p' `YX', `options'
            local plots `plots' `plot'
        }
    }
    c_local plot `plots'
    c_local p `p'
end

program _zoom_parse_box
    syntax [, Destination Origin NONE ]
    c_local box_destination `destination'
    c_local box_origin `origin'
    c_local box_none `none'
end

program _grdim
    syntax [, margin(str) refdim(str) aratio(str)/*
        */ XSCale(str asis) YSCale(str asis) * ]
    // get dimensions of coordinates on map
    foreach v in X Y {
        su `v', mean
        local `v'MIN = r(min)
        local `v'MAX = r(max)
        capt confirm variable `v'2, exact
        if _rc==0 {
            su `v'2, mean
            local `v'MIN = min(``v'MIN', r(min))
            local `v'MAX = max(``v'MAX', r(max))
        }
        local `v'range = ``v'MAX' - ``v'MIN'
    }
    if      "`refdim'"=="y" local refsize `Yrange'
    else if "`refdim'"=="x" local refsize `Xrange'
    else                    local refsize = min(`Yrange', `Xrange')
    // add margin
    foreach v in X Y {
        gettoken m margin : margin
        local `v'MIN = ``v'MIN' - `refsize' * (`m'/100)
        local `v'min ``v'MIN'
        gettoken m margin : margin
        local `v'MAX = ``v'MAX' + `refsize' * (`m'/100)
        local `v'max ``v'MAX'
    }
    // update dimensions depending in x/yscale(), xylabel()
    foreach V in X Y {
        local v = strlower("`V'") 
        _grdim_parse_scale `V' ``V'min' ``V'max', ``v'scale'
        foreach O in LABel TIck MLABel MTIck {
            local xopts
            local O `V'`O'
            local o = strlower("`O'") 
            while (1) {
                capt n _grdim_parse_label `O' `o' `V' ``V'min' ``V'max',/*
                    */ `options'
                if _rc==1 exit 1
                if _rc {
                    di as err "error in `o'()"
                    exit _rc
                }
                if `done' continue, break
                local xopts `xopts' ``o''
            }
            local options `xopts' `options'
        }
    }
    // return
    c_local refsize `refsize' // reference size (before adding margin)
    c_local Xmin `Xmin'
    c_local Xmax `Xmax'
    c_local Ymin `Ymin'
    c_local Ymax `Ymax'
    c_local aratio = (`Ymax'-`Ymin') / (`Xmax'-`Xmin') * `aratio'
    c_local options xscale(range(`Xmin' `Xmax') `Xscale_opts')/*
        */ yscale(range(`Ymin' `Ymax') `Yscale_opts')/*
        */ xlabel(none, labsize(vsmall)) ylabel(none, labsize(vsmall))/*
        */ xtitle("")  ytitle("") `options'
end

program _grdim_parse_scale
    _parse comma lhs 0 : 0
    gettoken v    lhs : lhs
    gettoken vmin lhs : lhs
    gettoken vmax lhs : lhs
    syntax [, Range(numlist) off on * ]
    if "`on'"=="" local off off
    else          local off on
    if `"`range'"'!="" {
        mata: st_local("range", invtokens(strofreal(/*
            */ minmax(strtoreal(tokens(st_local("range"))))/*
            */ , "%18.0g")))
        gettoken min max : range
        gettoken max     : max
        if "`min'"!="" {
            local vmin = min(`vmin', `min')
        }
        if "`max'"!="" {
            local vmax = max(`vmax', `max')
        }
    }
    c_local `v'min `vmin'
    c_local `v'max `vmax'
    c_local `v'scale_opts `off' `options'
end

program _grdim_parse_label
    _parse comma  lhs 0 : 0
    gettoken OPT  lhs : lhs
    gettoken opt  lhs : lhs
    syntax [, `OPT'(str asis) * ]
    if `"``opt''"'=="" {
        c_local done 1
        exit
    }
    c_local done 0
    c_local options `options'
    gettoken v    lhs : lhs
    gettoken vmin lhs : lhs
    gettoken vmax lhs : lhs
    local vMIN `vmin'
    local vMAX `vmax'
    _parse comma 0 rhs : `opt'
    // process rule, if specified
    gettoken rule : 0
    local update 0
    if      `"`rule'"'=="."              gettoken rule 0 : 0
    else if `"`rule'"'=="none"           gettoken rule 0 : 0
    else if `"`rule'"'=="minmax"         gettoken rule 0 : 0
    else if substr(`"`rule'"',1,2)=="##" gettoken rule 0 : 0
    else if substr(`"`rule'"',1,1)=="#" {
        gettoken rule 0 : 0
        local rule = substr(`"`rule'"',2,.)
        _natscale `vMIN' `vMAX' `rule'
        local vmin = min(`vmin', r(min))
        local vmax = max(`vmax', r(max))
        local rule `r(min)'
        forv i = 2/`=r(n)' {
            local val = r(min) + (`i'-1) * r(delta)
            local rule `rule' `val'
        }
        local update 1
    }
    else local rule
    // process numlist [label] ...
    while (`"`0'"'!="") {
        local nlist
        while (`"`0'"'!="") {
            gettoken n 0: 0, quotes qed(q)
            if `q' continue, break
            local nlist `nlist' `n'
        }
        numlist `"`nlist'"'
        local nlist `r(numlist)'
        local rule `rule' `nlist' `n'
        mata: st_local("nlist", invtokens(strofreal(/*
            */ minmax(strtoreal(tokens(st_local("nlist"))))/*
            */ , "%18.0g")))
        gettoken min max : nlist
        gettoken max     : max
        if "`min'"!="" {
            local vmin = min(`vmin', `min')
        }
        if "`max'"!="" {
            local vmax = max(`vmax', `max')
        }
    }
    // returns
    c_local `v'min `vmin'
    c_local `v'max `vmax'
    if `update' {
        local `opt' `rule'`rhs'
    }
    c_local `opt' `opt'(``opt'')
end

program _grdim_tight
     syntax, aratio(str) [ YSIZe(str) XSIZe(str) SCHeme(str) ]
     // ysize specified
     if `"`ysize'"'!="" {
         if `"`xsize'"'!="" exit // nothing to do
         local unit = substr(`"`ysize'"',-2,.)
         if inlist(`"`unit'"', "in", "pt", "cm") {
             local ysize = strtrim(substr(`"`ysize'"',1,strlen(`"`ysize'"')-2))
             if      "`unit'"=="pt" local ysize = `ysize' / 72
             else if "`unit'"=="cm" local ysize = `ysize' / 2.54
         }
         local xsize = min(100,max(1,`ysize' / `aratio'))
         c_local xsize xsize(`xsize')
         exit
     }
     // xsize specified
     if `"`xsize'"'!="" {
         local unit = substr(`"`xsize'"',-2,.)
         if inlist(`"`unit'"', "in", "pt", "cm") {
             local xsize = strtrim(substr(`"`xsize'"',1,strlen(`"`xsize'"')-2))
             if      "`unit'"=="pt" local xsize = `xsize' / 72
             else if "`unit'"=="cm" local xsize = `xsize' / 2.54
         }
         local ysize = min(100,max(1,`xsize' * `aratio'))
         c_local ysize ysize(`ysize')
         exit
     }
     // get ysize from scheme
     if `"`scheme'"'=="" local scheme `"`c(scheme)'"'
     if `"`.__SCHEME.scheme_name'"'!=`"`scheme'"' {
         qui .__SCHEME = .scheme.new, scheme(`scheme')
     }
     local ysize `.__SCHEME.graphsize.y'
     if `"`ysize'"'=="" local ysize 4.5
     local xsize = min(100,max(1,`ysize' / `aratio'))
     c_local ysize ysize(`ysize')
     c_local xsize xsize(`xsize')
end

program _legend
    // syntax
    syntax [, off Layout(str asis) BOTtom HORizontal REVerse/*
        */ OUTside POSition(str)/*
        */ SIze(passthru) SYMYsize(passthru) SYMXsize(passthru)/*
        */ KEYGap(passthru) COLGap(passthru) ROWGap(passthru)/*
        */ BMargin(passthru) REGion(passthru)/*
        */ order(passthru) Cols(numlist max=1 int >0)/*
        */ Rows(numlist max=1 int >0) NOCOLFirst COLFirst/*
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
    if `"`layout'"'=="" {
        forv l=1/`: char LAYER[layers]' {
            if `"`: char LAYER[hasz_`l']'"'=="1" {
                if `"`: char LAYER[nolegend_`l']'"'=="" {
                    local layout `l'
                    continue, break
                }
            }
        }
        if "`layout'"=="" {
            c_local legend legend(off)
            exit
        }
    }
    // compile legend
    local LAYOUT
    local ncols 0
    local kmax 0
    local nkeys 0
    if "`horizontal'"=="" {
        if "`reverse'"=="" local reverse reverse
        else               local reverse
    }
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
            local nkeys_`ncols' `nkeys'
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
                local lsize_`l': char LAYER[keys_`l']
                local lsize_`l': list sizeof lsize_`l'
                if `"`: char LAYER[z_hasmis_`l']'"'=="1" {
                    local mleg_`l': char LAYER[z_mleg_`l']
                    if !inlist(`"`mleg_`l''"',"1","2","3","4") local mleg_`l'
                    if "`mleg_`l''"=="" {
                        local --lsize_`l'
                    }
                    else if inlist("`mleg_`l''", "2", "4") {
                        local ++lsize_`l'
                    }
                }
                else local mleg_`l'
                if `lsize_`l''<=0 continue
                local nkeys = `nkeys' + `lsize_`l''
                local LAYOUT `LAYOUT' `l'
            }
        }
    }
    if `nkeys' { // close last column
        local ++ncols
        local nkeys_`ncols' `nkeys'
        local kmax = max(`kmax', `nkeys')
        local LAYOUT `LAYOUT' |
    }
    if !`kmax' { // legend is empty
        c_local legend legend(off)
        exit
    }
    // - now compile legend
    local ORDER
    local LABELS
    local nkeys 0
    local lcol 1
    local newcol 1
    while (1) {
        gettoken l LAYOUT : LAYOUT
        if `"`l'"'=="" continue, break
        if `newcol' {
            if "`bottom'"!="" {
                while (`nkeys'<(`kmax'-`nkeys_`lcol'')) {
                    local ++nkeys
                    local ORDER `ORDER' - " "
                }
            }
            local newcol 0
        }
        if `"`l'"'=="." {
            local ++nkeys
            local ORDER `ORDER' - " "
        }
        else if `"`l'"'=="|" {
            while (`nkeys'<`kmax') {
                local ++nkeys
                local ORDER `ORDER' - " "
            }
            local nkeys 0
            local ++lcol
            local newcol 1
        }
        else if `"`l'"'=="-" {
            gettoken l LAYOUT : LAYOUT
            local ++nkeys
            local ORDER `ORDER' - `l'
        }
        else {
            local nkeys = `nkeys' + `lsize_`l''
            local keys:   char LAYER[keys_`l']
            local lbls:   char LAYER[labels_`l']
            if `"`: char LAYER[z_hasmis_`l']'"'=="1" {
                gettoken mkey keys : keys
                gettoken mlbl lbls : lbls
            }
            local revrs = `"`:char LAYER[z_reverse_`l']'"'=="1"
            if "`reverse'"!="" {
                if `revrs' local revrs 0
                else       local revrs 1
            }
            local KEYS
            foreach key of local keys {
                gettoken lbl lbls : lbls
                gettoken tmp : lbl, qed(hasquote)
                if !`hasquote' local lbl `"`"`lbl'"'"'
                local labels `labels' label(`key' `lbl')
                if `revrs' local KEYS `key' `KEYS'
                else       local KEYS `KEYS' `key'
            }
            if "`mleg_`l''"!="" {
                gettoken tmp : mlbl, qed(hasquote)
                if !`hasquote' local mlbl `"`"`mlbl'"'"'
                local labels `labels' label(`mkey' `mlbl')
                if      "`mleg_`l''"=="1" local KEYS `KEYS' `mkey'
                else if "`mleg_`l''"=="2" local KEYS `KEYS' - " " `mkey'
                else if "`mleg_`l''"=="3" local KEYS `mkey' `KEYS'
                else if "`mleg_`l''"=="4" local KEYS `mkey' - " " `KEYS'
            }
            local ORDER `ORDER' `KEYS'
        }
    }
    if `"`order'"'=="" local order order(`ORDER')
    else               local ncols
    // orientation / layout
    if "`horizontal'"!="" {
        if `"`rows'`cols'"'=="" & "`ncols'"!="" local rows `ncols'
        if "`colfirst'"=="" & "`cols'"!="1"     local nocolfirst nocolfirst
    }
    else {
        if `"`rows'`cols'"'=="" & "`ncols'"!="" local cols `ncols'
        if "`nocolfirst'"=="" & "`rows'"!="1"   local colfirst colfirst
        if `"`rowgap'"'==""                     local rowgap rowgap(0)
    }
    local opts
    if "`rows'"!="" local opts `opts' rows(`rows')
    if "`cols'"!="" local opts `opts' cols(`cols')
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
    c_local legend_pos `position'
    if "`outside'"!=""    local position position(`position')
    else                  local position position(0) bplace(`position')
    if `"`bmargin'"'==""  local bmargin bmargin(zero)
    if `"`region'"'==""   local region region(style(none) margin(zero))
    local opts `opts' `position' `bmargin' `region' `options'
    // return legend option
    c_local legend legend(`order' `labels' on all `opts')
end

program _clegend
    if c(stata_version)<18 {
        di as err "{bf:clegend()} requires Stata 18"
        exit 9
    }
    // syntax
    _parse comma legend_pos 0 : 0
    syntax [, off Layer(numlist int max=1 >0) noLABel MISsing Format(str)/*
        */ OUTside POSition(str) width(passthru) height(passthru)/*
        */ BMargin(passthru) REGion(passthru) cuts(str asis)/*
        */ BPLACEment(passthru)/* will be ignored
        */ _gropts(str asis) * ]
    if `"`off'"'!="" {
        c_local clegend
        c_local plot
        exit
    }
    local bplacement
    _clegend_parse_cuts, `cuts'
    // select layer
    local k `layer'
    if "`k'"!="" {
        if `"`: char LAYER[z_hascol_`k']'"'!="1" local k
    }
    else {
        forv l=1/`: char LAYER[layers]' {
            if `"`: char LAYER[z_hascol_`l']'"'=="1" {
                local k `l'
                continue, break
            }
        }
    }
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
    local CUTS:   char LAYER[z_levels_`k']
    local colors: char LAYER[z_colors_`k']
    local FMT:    char LAYER[z_format_`k']
    local LABELS: char LAYER[labels_`k']
    local discrete = `"`: char LAYER[z_discrete_`k']'"'=="1"
    local hasmis   = `"`: char LAYER[z_hasmis_`k']'"'=="1"
    if `hasmis' {
        gettoken labmis LABELS : LABELS
        if `"`missing'"'=="" {
            gettoken colmis colors : colors
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
    local K: colsof `CUTS'
    if `discrete' {
        forv i = 1/`K' {
            if "`label'"=="" gettoken lbl LABELS : LABELS
            else  local lbl `"`=`CUTS'[1,`i']'"'
            local labels `labels' `=`i'-.5' `"`lbl'"'
        }
    }
    if `"`format'"'!="" {
        capt confirm numeric format `format'
        if _rc local format
    }
    else local format `"`FMT'"'
    if `"`format'"'=="" local format format(%7.0g)
    else                local format format(`format')
    // append data for clegend plot
    qui gen byte CLEG_Y = .
    qui gen byte CLEG_X = .
    qui gen double CLEG_Z = .
    local N = `K' + `discrete' + `hasmis' 
    if `N'>_N {
        set obs `N'
    }
    if `discrete' {
        qui replace CLEG_Z = .0001 in 1
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
            local labels -.5 `"`labmis'"' `labels'
            local values -1 `values'
        }
        local hght = min(100, (`N'-1)*3)
        local zlabel zlabel(`labels', `format' labsize(vsmall) notick labgap(1))
        local zscale zscale(noline)
        if `hasmis' local Zmin = CLEG_Z[`K' + 2]
        else        local Zmin = CLEG_Z[1]
        local Zmax = CLEG_Z[`K' + 1]
        local K = `K' + 1 + `hasmis'
    }
    else {
        forv i = 1/`K' {
            local val = `CUTS'[1,`i']
            local values `values' `val'
        }
        local labels `values'
        if `hasmis' {
            local v0 = `CUTS'[1,1] - (`CUTS'[1,`K']-`CUTS'[1,1])/`K'
            matrix `CUTS' = (`v0', `CUTS')
            local values `v0' `values'
            local labels `v0' `"`labmis'"' `labels'
            local ++K
        }
        qui replace CLEG_Z = `CUTS'[1,1] + (`CUTS'[1,2]-`CUTS'[1,1])/10000 in 1
            /* shift first point slightly up */
        forv i = 2/`K' {
            qui replace CLEG_Z = `CUTS'[1,`i'] in `i'
        }
        local hght = min(40, (`N'-1)*3)
        local Zmin = CLEG_Z[1]
        local Zmax = CLEG_Z[`K']
        local zscale
        if "`cuts'"!="label" local zlabel zlabel(none, `format' labsize(vsmall))
        else local zlabel zlabel(`labels', `format' labsize(vsmall))
        if "`cuts'"=="mlabel" /*
            */ local zlabel `zlabel' zmlabel(`labels', `format' labsize(tiny))
        else if "`cuts'"=="tick"  local zlabel `zlabel' ztick(`values')
        else if "`cuts'"=="mtick" local zlabel `zlabel' zmtick(`values')
    }
    // adjust max
    local ZMAX `Zmax'
    _clegend_adjustmax `Zmin' `Zmax', `_gropts'
    if `Zmax'!=`ZMAX' {
        mata: st_local("values", ///
            invtokens((tokens(st_local("values"))[|1 \ `N'-1|], "`Zmax'")))
    }
    // layout of clegend
    if `"`position'"'=="" {
        if inlist("`legend_pos'","1","2") local position 4
        else                              local position 2
    }
    else                   _parse_position `position' // compass => clock
    if "`outside'"!=""     local position position(`position')
    else                   local position position(0) bplace(`position')
    if `"`width'"'==""     local width width(3)
    if `"`height'"'==""    local height height(`hght')
    if `"`bmargin'"'==""   local bmargin bmargin(l=0 r=0 b=1 t=1)
    if `"`region'"'==""    local region region(margin(zero))
    local options `position' `width' `height' `bmargin' `region' `options'
    // return clegend plot and clegend option
    c_local plot (scatter CLEG_Y CLEG_X in 1/`K', colorvar(CLEG_Z)/*
        */ colorcuts(`values') colorlist(`colors') colorkeysrange)
    c_local clegend clegend(`options') ztitle("") `zlabel' `zscale'
end

program _clegend_parse_cuts
    syntax [, NONE LABel TIck MLABel MTIck ]
    local cuts `label' `tick' `mlabel' `mtick'
    if "`none'"!="" {
        if "`cuts'"!="" {
            di as err "clegend(cuts()): too many keywords specified"
            exit 198
        }
        c_local cuts
        exit
    }
    if "`cuts'"=="" local cuts label
    else if `: list sizeof cuts'>1 {
        di as err "clegend(cuts()): too many keywords specified"
        exit 198
    }
    c_local cuts `cuts'
end

program _clegend_adjustmax
    _parse comma lhs 0 : 0
    gettoken Zmin lhs : lhs
    gettoken Zmax lhs : lhs
    syntax [, ZSCale(str asis) * ]
    _grdim_parse_scale Z `Zmin' `Zmax', `zscale'
    foreach O in LABel TIck MLABel MTIck {
        local O Z`O'
        local o = strlower("`O'")
        while (1) {
            capt n _grdim_parse_label `O' `o' Z `Zmin' `Zmax', `options'
            if _rc==1 exit 1
            if _rc {
                di as err "error in `o'()"
                exit _rc
            }
            if `done' continue, break
        }
    }
    c_local Zmax `Zmax'
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
    if `"`lwidth'"'==""      local lwidth lwidth(.15)
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
    if `"`lwidth'"'=="" local lwidth lwidth(.15)
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
    frame create `frame' double(_X _Y SIZE)
    while (`"`args'"'!="") {
        gettoken x args : args
        local x = real(`"`x'"')
        gettoken y args : args
        local y = real(`"`y'"')
        gettoken size args : args
        local size = real(`"`size'"')
        frame post `frame' (`x') (`y') (`size')
    }
    syntax [, size(passthru) * ]
    _geoplot_symbol `layer' `p' `frame', size(SIZE) `options'
    c_local plot `plot'
    c_local p `p'
end

version 16.1
mata:
mata set matastrict on

void _st_welzl()
{
    real rowvector C
    
    C = geo_welzl(st_data(., "X Y", st_local("touse")))
    st_local("Xmid", strofreal(C[1], "%18.0g"))
    st_local("Ymid", strofreal(C[2], "%18.0g"))
    st_local("R",    strofreal(C[3], "%18.0g"))
}

void _st_circle_tangents(
    real scalar x1, real scalar y1, real scalar r1,
    real scalar x2, real scalar y2, real scalar r2)
{
    real matrix R
    
    R = geo_circle_tangents((x1,y1,r1), (x2,y2,r2))
    if (length(R)) st_local("YX", invtokens(strofreal(vec(R[,(2,1)]')',
                   "%18.0g")))
    else           st_local("YX", "")
}

void _zoom_boxconnect(real scalar Xmax, real scalar Xmin, real scalar Ymax,
    real scalar Ymin, real scalar XMAX, real scalar XMIN, real scalar YMAX,
    real scalar YMIN)
{
    real scalar    i
    real matrix    yx, YX, p, P
    
    yx = (Ymin,Xmax) \ (Ymax,Xmax) \ (Ymax,Xmin) \ (Ymin,Xmin)
    YX = (YMIN,XMAX) \ (YMAX,XMAX) \ (YMAX,XMIN) \ (YMIN,XMIN)
    yx = yx :- (YMIN, YMAX) // for sake of precision
    YX = YX :- (YMIN, YMAX)
    P = J(0,2,.)
    for (i=1;i<=4;i++) {
        // handle edge connecting lower right corner
        p = __zoom_boxconnect(i, yx:-YX[i,], YX:-YX[i,]) :+ YX[i,]
        if (length(p)) {
            _geo_rotate(p, -(i-1)*90) // undo rotation
            P = P \ p
        }
        // tilt by -90 degree so that next edge is in lower right corner 
        yx = geo_rotate(yx, 90) 
        YX = geo_rotate(YX, 90)
    }
    P = P :+ (YMIN, YMAX)
    st_local("YX", invtokens(strofreal(vec(P')', "%18.0g")))
}

real matrix __zoom_boxconnect(real scalar i, real matrix yx, real matrix YX)
{
    real scalar y, x, Y, X, Ymax, Xmin
    
    y = yx[i,1]
    x = yx[i,2]
    if (y<0 | x>0) return((y,x) \ (0,0)) // no crossing
    Ymax = YX[mod(i,4)+1,1]
    Xmin = YX[mod(i+1,4)+1,2]
    if (y<=Ymax & x>=Xmin) return(J(0,2,.)) // inside box
    if (((x*Ymax - y*Xmin) / sqrt(Xmin^2 + Ymax^2))>0) { // above diagonal
        Y = Ymax
        X = x * Ymax / y
    }
    else { // below diagonal
        Y = y * Xmin / x
        X = Xmin
    }
    return((y,x) \ (Y,X))
}

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
    st_local("a", strofreal(a, "%18.0g"))
    st_local("b", strofreal(b, "%18.0g"))
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

