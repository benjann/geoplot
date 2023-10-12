# geoplot
Stata module to draw maps

`geoplot` draws maps from shape files and other datasets. The procedure is to
first create one or several frames containing the source data using command
`geoframe` and then apply `geoplot` to plot the data from these frames. Multiple
layers of elements such as regions, borders, lakes, roads, labels, and symbols
can be freely combined. The look of the elements (e.g. their color) can be
varied depending on the values of variables.

To install `geoplot` from the SSC Archive, type

    . ssc install geoplot, replace

in Stata. Stata version 16.1 or newer is required. Furthermore,
the following packages need to be installed on the system:

    . ssc install palettes, replace
    . ssc install colrspace, replace
    . ssc install moremata, replace

Installation of `geoplot` from GitHub:

    . net install geoplot, replace from(https://raw.githubusercontent.com/benjann/geoplot/main/)

---

Examples:

Load data using `geoframe`.

    local url http://fmwww.bc.edu/repec/bocode/i/
    geoframe create regions  `url'Italy-RegionsData.dta, id(id) coord(xcoord ycoord) ///
        shpfile(Italy-RegionsCoordinates.dta)
    geoframe create country  `url'Italy-OutlineCoordinates.dta
    geoframe create capitals `url'Italy-Capitals.dta, coord(xcoord ycoord)
    geoframe create lakes    `url'Italy-Lakes.dta, feature(water)
    geoframe create rivers   `url'Italy-Rivers.dta, feature(water)

Basic map of Italian regions.

    geoplot (area regions) (line country, lwidth(medthick))

![example 1](/images/1.png)

Basic map with lakes and rivers.

    geoplot (area regions) (area lakes) (line rivers)

![example 2](/images/2.png)

Regions colored by number of fortune tellers (per million population).

    geoplot (area regions fortell) (line regions)

![example 3](/images/3.png)

Different formatting of legend labels.

    geoplot (area regions fortell, label("@lb-@ub (N=@n)")) (line regions)

![example 4](/images/4.png)

Similar graph with more colors and alternative type of legend (requires Stata 18)

    geoplot (area regions fortell, levels(20) lcolor(gray)) ///
        , clegend(height(30)) zlabel(4(3)28)

![example 5](/images/5.png)

Map with provincial capitals.

    geoplot ///
        (area regions) ///
        (point capitals i.size [w=pop98], color(Set1, opacity(50)) mlcolor(%0)) ///
        (label capitals city if pop98>250000, color(black)) ///
        , legend compass sbar(length(300) units(km))

![example 6](/images/6.png)

Map with composite legend.

    geoplot ///
        (area regions fortell) ///
        (point capitals i.size [w=pop98], color(Set1, reverse opacity(50)) ///
            mlcolor(white)) ///
        , legend(layout(- "FORTELL" 1 | - "CITY SIZE" 2) position(sw))

![example 7](/images/7.png)

Map with pie charts.

    geoplot (area regions) (pie regions relig?, label(, reverse))

![example 8](/images/8.png)

Map with polar area diagrams.

    geoplot (area regions) ///
        (pie regions relig?, polar outline(lc(gs6)) label(, reverse))

![example 9](/images/9.png)

Map with exploded pie charts, scaled by population size.

    geoplot (area regions) ///
        (pie regions relig? [w=pop98], size(*2) explode(3 = 40) angle(0) ///
            label(, reverse))

![example 10](/images/10.png)

Map with bar charts.

    geoplot (area regions) (bar regions relig1, asis outline)

![example 11](/images/11.png)

Zoom.

    geoplot (area regions)                                           /// 1
        (area regions         if id==1, fc(Coral*.5) lc(gray))       /// 2
        (label regions region if id==1, color(black))                /// 3
        (area regions         if id==1, fc(Coral) lc(gray) /*
                                     */ box(circle pad(5) fc(gs14))) /// 4
        (pie regions relig1 relig2 relig3 if id==1, lab(, reverse))  /// 5
        , legend(pos(se) rowgap(1)) zoom(4/5: 6 90 210)

![example 12](/images/12.png)

---

Main changes:

    12oct2023
    - -geoframe translate- added (wrapper for spshape2dta)
    - -geoframe query orientation- added
    - -geoframe query gtype- added
    - -geoframe generate shpmatch- added
    - -geoframe copy- has been revised; can now specify custom IDs; can now copy
      data between any type of frames
    - option scale() in -geoframe generate area- and in -geoplot, sbar()- now has
      a different interpretation; it now specified the number of units of the
      underlying coordinates that form the base unit for the areas or the scale bar
    - -geoframe- no longer leaves undocumented resulte behind in r() 
    - -geoframe [r]clip- now has option -nodrop- to keep empty shapes in the data;
      various smaller improvements to code implementing clipping
    - -geoframe create- now also drops unmatched units, not only empty shapes (unless
      nodrop is specified)
    - -geoframe clean- classified shapes as empty if they consisted a single
      missing opservations; this is changed; all shapes that only contain missing
      are now considered empty
    - categorization of zvar in geoplot is now more efficient
    - a note is now displayed if zvar contains values not covered by cuts()

    09oct2023
    - -geoframe clip- added for clipping by convex shape; the clipping algorithm
      used by -geoframe [r]clip- has been rewritten and improved; it now returns
      divided polygons if necessary
    - -sid- and -pid- are now updated by -geoframe [r]clip- (if sid and
       pid have been set in the original data)
    - -geoframe query bbox- now calls -geoframe bbox- and supports the corresponding
      options
    - option noshp, angle(), and noadjust added to -geoframe bbox-
    - -geoframe bbox, circle- now asjust the radius so that the n-gram encloses the
      circle; type noadjust to omit the adjustment
    - now making use of -units- suboption in aspectratio() to control the aspect
      ratio of the plot in Stata 18, born 04oct2023, or newer

    05oct2023
    - added -geoframe rclip- for rectangular clipping
    - added -geoframe query- to obtain information on the shapes in a geoframe
    - -geoframe generate plevel- and -geoframe spjoin- now display progress dots;
       specify option -nodots- to suppress the progress dots
    - -geoframe genenerate pid- no longer refuses to add a PID to a frame that has
      not been declared as a shape frame
    - -geoframe genenerate pid- now treats points in a unit as separate shape items
      if there is no indication that the points form a polygon or line (i.e. if
      neither the first nor the last coordinate is missing)

    30sep2023
    - layer type -label- now supports printing labels depending on zvar

    28sep2023
    geoplot.ado
    - there were some numerical precision issues with zoom(); this is fixed
    - by default, legend(horizontal) now arranges keys from lowest to highest, not
      from highest to lowest
    - legend(reverse) did not work with multiline labels; this is fixed
    - some improvement of behavior of legend() if order(), rows(), or cols() is
      specified; note added to help that discourages use of order(), rows(), or
      cols()
    - internally, legend labels are now set by label() options rather then by
      including the labels in order()
    - internal communication between geoplot.ado and __geoplot_layer.ado has been
      revised
    - some information on layers is now returned in r()
    - plottype symbol supressd the outline if option color() was specified; this
      is fixed
  
    12sep2023
    __geoplot_layer.ado (version 1.1.1)
    - a typo introduced in one of the previous versions caused the computations 
      by levels(, quantile) and levels(, kmeans) to be based on the a dataset
      containing one observation for each coordinate of the shape polygons (rather
      than one observation per unit); this is fixed
    - some improvements have been made on how default rendering options are
      determined for area
    - label(): "@lab" now maps to "@lb - @ub" (rather than "@lb-@ub")

    11sep2023 (version 1.1.0)
    - Stata 16 now supported
    geoplot 
    - box() added to layer options to draw bounding box
    - select() added to layer options to (sub)select objects to be printed
    - zoom() failed to print origin and destination boxes; this is fixed
    - symbol: pentagram and hexagram now use crossing lines; star and star6 added
    - clegend(): suboption cuts() added
    geoframe
    - subcommands collapse and contract added
    - spjoin: option select() added
    - get coordinates: now also tries _X _Y if type is "unit"
    - get coordinates: now also tries _CX _CY if type is ""

    06jul2023
    geoplot (version 1.0.7)
    - compiled legend option now returned in r(legend)
    - geoplot now parses graph options such as xscale(range()) or ylabel() and
      adjust computation of aspect ratio accordingly
    - clegend() sometimes displayed funny colors at the top if zscale(range())
      or zlabel() was specified; this should now be fixed

    06jul2023
    geoplot (version 1.0.6)
    - i.zvar now allowed, implying -discrete-; also in colorvar()
    - label() now uses values rather than indices in case of i.zvar or discrete 
    - i.zvar and -discrete- now recycle colors (rather than interpolating) unless
      class of the palette is non-categorical
    - can now specify -nolegend- as an alternative to -legend(off)-
    - legend() now has suboption -reverse- to revers order of legend keys within
      layers
    - clegend() is now also placed north-east by default, unless there is already
      a standard legend in this place
    - immediate arguments for -symboli- are now -x y size-, no longer -y x-; option
      size() will be ignored 
    - fixed bug related to offset() in symbol/pie/bar
    geoframe (version 1.0.6)
    - -geoframe create- no longer looks at chars written by -spshape2dta- to find
      out whether there is a shape file to be loaded automatically; a shapefile is
      now autoloaded if a file called <basename>_shp.dta is available in the same
      folder as the main file, where <basename> is the base name of the main file
    - -geoframe create- now adds the path of the main file to the file specified
      in shpframe() if the file is specified without path
    - -geoplot bbox- is now faster if by() is equal to the unit ID
    - -geoframe symbol- and -geoframe symboli- added
    lgeoplot.mlib
    - geo_symbol(): inner circle of pin2 now clockwise

    02jul2023
    geoplot (version 1.0.5)
    - default linewidth now everywhere .15
    - geoplot failed if zvar was all missing; this is fixed
    geoframe (version 1.0.5)
    - option -rotate- added to -geoframe bbox- (minimum-area bounding box)
    lgeoplot.mlib
    - geo_bbox() added to lgeoplot.mlib

    30jun2023
    geoframe (version 1.0.4)
    - option -hull- added to -geoframe bbox-
    lgeoplot.mlib
    - geo_hull() added to lgeoplot.mlib
    - ID can now be scalar in geo_area() and geo_centroid()

    27jun2023 (version 1.0.3):
    general:
    - geoplot now has a Mata library for common functions (lgeoplot.mlib;
      source code in lgeoplot_source.sthlp)
    geoplot:
    - option zoom() added
    - option feature() added
    - option frame() now has suboption -nocurrent-
    - option -nograph- added
    - layertypes symbol/pie/bar now allow expressions involving variables in size()
    - layertype symbol:
      o numlist and matrix in shape() are now interpreted as (X,Y), not (Y,X)
      o orientation was affected by global angle(); this is fixed
    - global option angle() had an error that did not affect the look, but
      shifted the underlying numeric values of the coordinates; this is fixed
    - categorization of zvar returned error if there were no observations in the
      layer; this is fixed
    - ysize()/xsize() computed by -tight- are now restricted to [1,100] inch
    geoframe:
    - -geoframe bbox- added
    - -geoframe generate plevel- now has option by(); unselected obs are no longer
       set to 0
    - -geoframe spjopin- now supports -if- and -in- and can now be used with an
       attribute frame that is linked to a shape frame
    - -geoframe generate area- and -geoframe generate centroids- falsely took into
      account the plot level (if a plot level variable was available); this is fixed
    - _geoframe_generate_plevel.ado and geoframe_spjoin.ado now integrated into
      geoframe.ado

    21jun2023
    geoframe (version 1.0.2):
    - -geoframe select- had several issues that are now fixed
    - option -keepshapes- in -geoframe select- now called -noshp-
    - option -unlink- added in -geoframe select-
    - -geoframe duplicate- added
    - -geoframe rename- added
    - -geoframe describe- now reports number of observations
    - -geograme clean- broke link in some situations; this is fixed

    19jun2023 (version 1.0.1)
    geoplot
    - legend() now has option -bottom- to align legend keys in multicolumn legends
      at bottom
    - composite palette specifications now possible in color() if zvar is specified
    - categorization of zvar is now faster
    - algorithm to remove empty shapes deleted from __geoplot_layer.ado
    geoframe
    - -geoframe clean- added
    - -geoframe select- added
    - -geoframe create- now drops unmatched and empty shapes in shpframe unless
      option -nodrop- is specified
    - -geoframe link- now has option clean() to remove unlinked/empty shapes or units
    - -geoframe link- now creates a permanent linkage variable in shpframe; this
      makes execution of -geoframe copy- faster
    - -geoframe relink- added
    - -geoframe set shpframe- no longer allowed (only -geoframe get shpframe-)
    - -geoframe get linkname- added
    - -geoframe create- now has -nocurrent- option
    - automatic loading of shape file is now only applied by -geoframe create- if
      (inferred) type is "unit"
    - -geoframe append- displayed some irrelevant output; this is fixed
    - -geoframe generate plevel- now allows if and in; if applied to an attribute
      frame, only the shapes related to units in the attribute file will be
      considered; if applied repeatedly, only plevel will be updated for the
      selected shapes, leaving the other values unchanged; option -force- discarded

    17jun2023 (version 1.0.0)
    - geoplot published on SSC
    - fixed minor bug related to label()
    - default lwidth for "water" now vthin

    16jun2023 (version 0.2.5)
    geoplot
    - geoplot symbol/pie/bar no longer restrict sample to 1st obs per ID
    - option id() added to area and line
    - option plevel() added to area 
    - geoplot symbol now looks for Mata functions called _geoplot_symbol_<name>(),
      not __geoplot_symbol_<name>()
    geoframe
    - geoframe spjoin added
    - geoframe generate pid and geoframe generate plevel now operate on the linked
      shape frame if applied to an attribute frame

    14jun2023 (version 0.2.4)
    geoplot:
    - the palette class was not always taken into account when
      interpolation/recycling colors; this is fixed
    geoframe:
    - geoframe generate plevel added
    - geoframe generate centroids did not allow abbreviation; this is fixed
    - geoframe set stored varnames into char even if they were equal to the default
      names; this is fixed

    13jun2023 (version 0.2.3)
    geoplot
    - pie and bar now documented
    - some bug fixes and refinements to pie and bar; option type() in bar replaced
      by option -polar-; circle() and box() now called outline()
    - suboption nolabel in label() did not work; this is fixed

    12jun2023 (version 0.2.2)
    geoplot
    - layertypes -symbol- and -symboli- added
    - layertypes -pie- and -bar- added
    - syntax in now more consistent across layertypes; point, label, and pc*
      now allow argument zvar, and custom coordinates can be provided as
      coordinates(); area/line now also support coordinates(); options centroids() 
      and area() can now be used to provide custom centroids and area sizes in
      area/line; option zvar() is discontinued
    - zvar now implies color()
    - default color scheme for discrete zvar now Set1
    - color(none), color(fg), color(bg), color(%#), color(*#) now supported if zvar
      is specified
    - label():
      o @n now insterts the number of units
      o @lb, @ub, @mid now also supported if -discrete-
      o @lab now inserts standard label (value label if -discrete-, @lb-@ub else)
      o options nomissing, mfirst, and gap removed
    - missing():
      o @n can now be used in label() to insert number of missing units
      o options nolabel, first, and gap added
    - can now specify wmax without argument to use obs max even if smaller than 1
    - layertype -label- is now an external program
    - __geoplot_layer now supports argument <layer> equal to . ("hidden" layer)
    geoframe:
    - geoframe flip added (undocumented)
    - -geoframe get coordinates, flip- did not return correct order in case of pc;
      this is fixed

    05jun2023 (version 0.2.1)
    geoplot:
    - global option sbar() added
    - global option compass() added
    - global option margin() now supports syntax {l|r|b|t}=#
    - global option margin() now uses the minimum of the vertical and horizontal
      size as reference by default; global option refdim() added to select the
      reference (the reference size will also be used by sbar() and compass())
    - global option rotate() renamed to angle()
    - __geoplot_layer is now a separate ado; this allows users to program additional
      layer types that make use of __geoplot_layer

    02jun2023 (version 0.2.0)
    - option -lock- added to area/line to lock the orientation of shapes in case of
      rotate(); this is undocumented
    - wmax() was not allowed in area; this is fixed

    02jun2023 (version 0.1.9)
    - global dmax() and wmax() discontinued; weights and size() will now be
      normalized within layer; size() has now suboptions scale() and dmax(); wmax()
      can now be specified within layer
    - revised positioning options for legend() and clegend(); positioning is now
      done with two suboptions, -position()- to select the position and -outside-
      place the legend outside of the plot region rather than inside
    - conflicting legend_options are now ignored by legend()

    01jun2023 (version 0.1.8)
    - suboption layout() in legend() can now be used to compile a legend from
      multiple layers; suboption layer() is discontinued; several suboptions that
      were available in legend() are now controlled via label()
    - option label() can now be used in layers to determine labels of legend
      keys and other legend-related settings
    - suboption label() in missing() now sets the label for missing in legend and
      clegend; suboption missing() in clegend() discontinued
    - legend(horizontal) no longer stacks keys and labels
    - in area/line without zvar() the line color was set to gray and, for area, the
      fill was omitted, even if color() was specified; this is changed
    - parsing of levels() was broken; this is fixed

    30may2023 (version 0.1.7)
    geoframe
    - -geoframe generate centroids- added
    - -geoframe set- no longer adds default variable names to char; defaults are
       now implicit
    geoplot
    - centroids and areas are now computed on the fly if needed; a corresponding
      message is displayed
  
    29may2023 (version 0.1.6)
    geoplot
    - option size() added to plotypes area and line
    - plotypes area and line now support weights to rescale the sizes of the shapes
    - cvar() renamed to zvar()
    - zvar() no also supports fintensity(), lpattern(), msymbol(), msize(), 
      msangle(), mlabcolor(), mlwidth(), mlabsize(), mlabangle(); color() takes
      precedence over mlabcolor() if both are specified
    geoframe
    - geoframe attach and geoframe copy now also alias/copy variables that start
      with "_"
    - geframe create has new options centroids() and area(); geoframe set/get now
      support centroids and area
    - -geoframe generate area- added

    22may2023 (version 0.1.5)
    geoplot
    - cvar() now also applies to lwidth(), not only color()

    22may2023 (version 0.1.4)
    geoplot
    - now displaying a note if a layer is empty
    - option frame() now makes the created frame the current frame
    - observations with missing weight are now excluded in plottype point/scatter if
      weights are specified 
    - units with empty shape data (i.e. units for which there is only a single
      observation in the shape frame and for which the coordinate variables in that
      observation are all missing) will now be excluded from plotting in plottypes
      area and line
    - units that do not exist in an attribute frame but for which shape data is
      available in the linked shape frame are now excluded in plottypes area and
      line if the attribute frame is specified as the source frame
    - option colvar() is now called cvar(); the idea is that the option will
      eventually also be used for other aspects such as line widths or marker
      symbols; processing of cvar is now done in a different place or the process
    - color options were passed through ColrSpace only in case of immediate plots; 
      this is fixed
    - weights in point/scatter are now normalized across all layers and subplots
      so that weighted markers sizes are comparable within the graph; global option
      wmax() can be used to make marker sizes comparable across graphs

    21may2023 (version 0.1.3)
    geoframe:
    - large parts rewritten
    - -geoframe create- can now read attribute files and shape files together; a
      link is established between the files so that one can work with the frame
      containing the attribute file in geoplot without having to bother about the
      frame containing the shapes; geoplot will grab the shapes automatically
    - -geoframe create- now makes the created frame the current (working) frame
    - -goeframe create- can now be applied to data that is already in memory
    - -geoframe set- and -geoframe get- now operate on given set of keywords;
      defaults have been revised to be in line with how spshape2dta defines files
    - -geoframe create- now has options corresponding to the keyword of
      -geoframe set-
    - new command -geoframe describe- to describe a frame created by -geoframe-;
      -geoframe describe- is called by can -geoframe create- after the frame has
      been created
    - new commands -geoframe link- and -geoframe unlink-; these commands support
      the linking of frames; -geoframe link- is called by -geoframe create- when
      reading an attribute file together with a shape file
    - the syntax of -geoframe attach- and -geoframe copy- has been simplified; it
      is no longer possible to specify custom ID variables; the ID variables are
      assumed to have been set by -geoframe create- or -geoframe set-
    geoplot
    - support for linked frames from geoframe added
    - level() can now generate color intervals based on quantiles (suboption
      -quantile-) or on kmeans clustering (suboption -kmeans-)
    - clegend() no longer include missing by default; specify suboption -missing-
      to include missing
    - clegend() now has a format() and a nolabel suboption

    17may2023 (version 0.1.1)
    - geoplot and geoframe can now also be used in Stata 17, not only in Stata 18;
      restrictions in Stata 17 are as follows: option clegend() in geoplot is not
      supported; -geoframe attach- is not supported (use -geoframe copy- insted)  
    - geoframe copy added (Stata 17 substitute to geoframe attach)
    - geoframe attach no longer tries to add variables that already exist
    - geograme attach/detach now return error if frame2 is same as current frame
    - geoframe append no longer returns type mismatch error if a variable of type
      long (float) is appended to a variable of type float (long); the variable
      will now be propmoted to double
    - geoplot now returns an informative error message if colorpalette, colrspace,
      or moremata is not installed
    - documentation for geoframe completed

    17may2023 (version 0.1.0)
    - released on GitHub


