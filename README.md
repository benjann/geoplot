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

See [doi.org/10.48350/188246](https://doi.org/10.48350/188246) for a
presentation on `geoplot`. See [doi.org/10.48350/188248](https://doi.org/10.48350/188248)
for examples from a workshop on `geoplot`. See 
[Maps in Stata III: geoplot](https://medium.com/the-stata-guide/maps-in-stata-iii-geoplot-a764cf42688a)
for a post on `geoplot` by Asjad Naqvi.

Examples:

Load data using `geoframe`.

    local url http://fmwww.bc.edu/repec/bocode/i/
    geoframe create regions  `url'Italy-RegionsData.dta, id(id) coord(xcoord ycoord) ///
        shp(Italy-RegionsCoordinates.dta)
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
        (point capitals i.size [w=pop98], color(Set1%50) mlcolor(%0)) ///
        (label capitals city if pop98>250000, color(black)) ///
        , legend compass sbar(length(300) units(km))

![example 6](/images/6.png)

Map with composite legend.

    geoplot ///
        (area regions fortell) ///
        (point capitals i.size [w=pop98], color(Set1%50, reverse) mlcolor(white)) ///
        (symbol capitals if city=="Roma", shape(pin2) color(red) label(Rome)) ///
        , glegend(layout(- "FORTELL" 1 | - "CITY SIZE" 2 3) position(sw))

![example 7](/images/7.png)

Map with size legend.

    geoplot ///
        (area regions fortell) ///
        (symbol capitals [w=pop98], color(stc2%50) lcolor(white) size(*6)) ///
        (label capitals city if pop98>250000, color(gs14) size(vsmall)) ///
        , glegend(layout(- "FORTELL" 1) position(sw)) ///
          slegend(100000 "100 K" 5e5 "500 K" 1e6 "1 M" 2e6 "2 M", position(ne) ///
            overlay lcolor(stc2) heading("City size") hsize(small))

![example 7b](/images/7b.png)

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

Map with bar charts (unstacked).

    geoplot (area regions) (bar regions relig?, nostack size(*2)), ///
        legend(position(s) horizontal outside)

![example 12](/images/12.png)

Zoom.

    geoplot (area regions)                                           /// 1
        (area regions         if id==1, fc(Coral*.5) lc(gray))       /// 2
        (label regions region if id==1, color(black))                /// 3
        (area regions         if id==1, fc(Coral) lc(gray) /*
                                     */ box(circle pad(5) fc(gs14))) /// 4
        (pie regions relig1 relig2 relig3 if id==1, lab(, reverse))  /// 5
        , legend(pos(se) rowgap(1)) zoom(4/5: 6 90 210)

![example 13](/images/13.png)

Inset.

    geoplot ///
        (area regions         if id==1, fc(Coral) lc(gray))           ///
        (label regions region if id==1, color(black) position(6))     ///
        (bar regions relig1 relig2 relig3 if id==1, nostack size(*5)) ///
        , glegend(pos(se) rowgap(1) reverse textwidth(30))            ///
          inset((area regions)                                        ///
                (area regions if id==1, fc(Coral*.5) lc(gray))        ///
                , position(nw) title(Italy))

![example 14](/images/14.png)

---

Main changes:

    17sep2024
    geoplot:
    - new suboptions keep() and drop() to select legend keys in option label()

    08sep2024
    geoplot:
    - values provided in cuts() will now be sorted if zvar is continuous
    - cuts() now supports (extended) missing values
    - cuts(matname) now supported
    - levels() now has suboptions min() and max()
    geoframe:
    - geoframe spsmooth: option target() renamed to at(); suboption -fill- added in
      at(); new kernel(Gaussian) (with capital "G") to use an unclipped gaussian
      kernel 
    mata library:
    - geo_ksmooth(): support for kernel "Gaussian" added; argument -fill- added

    05sep2024
    geoplot:
    - zvar option color(): automatic application of palette option n() was
      suppressed if the user specified ipolate(); it is now suppressed if the
      user specifies ipolate(), select(), or drop()
    geoframe:
    - command -geoframe spsmooth- added
    mata library:
    - function -geo_ksmooth()- added

    30aug2024
    geoframe:
    - -geoframe bshare, outline- now creates variable _PLEVEL only if relevant
    - -geoframe raster- now creates variable _PLEVEL only if relevant
    mata library:
    - geo_raster(): some technical changes to clipping algorithm; original
      coordinates are now returned for cells that lie completely within a shape

    29aug2024
    geoplot
    - option inset() did not display anything if the included coordinates had a
      range of zero; this is fixed
    geoframe
    - command -geoframe raster- added
    - geoframe bshare now has option -outline-
    - geoframe stack revised
      o new IDs are now only generated if necessary (i.e., if there are duplicate
        IDs between frames)
      o new IDs are now generated in a different way; the old approach did not
        always generate unique IDs
      o the name of the ID variable in the first frame is now used as name for the
        ID variable in the stacked frame 
    - geoframe append now always matches variables based on exact name
    mata library:
    - new geo_raster() function
    - geo_plevel() now updates the progress bar more often
    - geo_rotate() no longer checks the number of columns in the input and now
      returns the input as is if angle==0
    - geo_spjoin() now returns flag for whether point has been matched in 2nd column
    - geo_area() and geo_centroid() now using quad precision

    31jul2024
    geoplot
    - layertype symbol:
      o option slant() added
      o new predefined symbol shapes: bar, cross, asterisk, fasterisk, barrow,
        fbarrow, trapezoid

    30jul2024
    geoplot:
    - glegend(): custom key labels can now be specified directly within layout()

    30jul2024
    geoplot:
    - glegend() now has option symsize() as shorthand for symxsize() and symysize();
      minimum abbreviaton of option symscale() now symsc(), not syms()

    30jul2024
    geoplot:
    - layertype symbol:
      o shape("text") now allowed
      o default symbil size (i.e. radius) is now 1.5% of reference size;
        computation of reference size revised
      o option select() was not fully functional; this is fixed
    - global option -noisily- added
    - layertype label: specified marker_label_options now take precedence over
      size(), color(), angle() etc.; undocumented options justification() and
      align() added
    - if zvar is specified and color()/mlabcolor() contains a single opacity and/or
      intensity operator, colors will now be selected from the default palette (st
      or viridis, depending on context); in the previous version, the operator was
      passed through without selecting explicit colors
    - layertype labels: msize(0) is now applied; this affects the positioning of the
      labels if position() is unequal 0
    - glegend(): improved handling of options related to marker labels; support for
      symbol with shape("text") added

    22jul2024
    geoplot
    - layertype symbol now allows syntax -symbol frame (shape) ...- as an
      alternative to -symbol frame ..., shape()-; likewise, symboli now allows
      -symboli (shape) ...- as an alternative to -symboli ..., shape(shape)-
    - glegend() returned error if applied to symbol layers containing
      -shape(numlist)- or -shape(matname)-; this is fixed

    22jul2024
    geoplot:
    - layertype label now has better support in glegend(); if syntax
      -label frame ("text" ...)- is used, the specified texts are use as symbols
      in the legend keys
    - arrow and farrow shapes in layertype symbol now have arguments; by default,
      the shaft of farrow now has nonzero width

    19jul2024
    geoplot:
    - suboption common added to symscale() in glegend() to preserve symbol sizes
      across keys

    18jul2024
    geoplot:
    - glegend():
       o now using min(symysize(), symxsize()) to determine the size of keys from
         symbol layers, no just symysize()
       o when creating composite keys from multiple symbol layers, the relative
         positioning of symbols depending on align() and offset() is now preserved
         (in addition to preserving relative size)
    - slegend():
       o rescaling imposed by project() was not taken account; this is fixed
       o option reverse did not work as expected in case of overlay; this is fixed
       o positioning of labels somewhat improved in case of overlay
    geoframe
    - geoframe symbol did not allow option align(); this is fixed

    17jul2024
    - some further refinements related to pc-data; geoframe grid/tissot now have 
      undocumented coordinates() option; layertype symbol now always generates
      a unique internal ID

    16jul2024
    general
    - handling of paired-coordinates data has been made more consistent; all
      functions in the mata library now require/assume n x 2 input; if needed,
      geoplot and geoframe reshape the data on the fly (or display error if pc data
      is not supported); several geoplot options and geoframe subcommands
      did not handle pc data correctly (typically ignoring the secondary
      coordinates); this should now be fixed; pc items are now classified as
      LineString, no longer as Point
    geoplot
    - new predefined symbol shapes: line, pipe, plus, x, diamond, v, arrow, farrow
    - option grid() can now be repeated
    - if -glegend- and -glegend(...)- are both specified, two glegends are now
      created
    geoframe:
    - the by() option in -geoframe generate plevel- did not work; this is fixed
    - geoframe contract/collapse failed unless option id() was specified; this is
      fixed

    15jul2024
    geoplot
    - zvar options gloptions() and missing(gloptions()) added to specify override
      options that will be applied to the symbols created by glegend()
    - glegend() ignored unsupported options; an error is now displayed if unsupported
      options are specified

    14jul2024
    geoplot
    - zoom() now has option otype() to set the scaling if the offset; option absolute
      now undocumented
    - option box in zoom() returned error in some situations; this is fixed

    13jul2024
    geoplot:
    - option title() in inset(), zoom(), glegend(), slegend(), and sbar() can
      now be repeated to create multiple titles; furthermore, title() now
      supports multiline specifications, has a position() suboption for flexible
      positioning, and supports textbox rendering options
    geoframe:
    - now using command __rm_dir rather than mata function path_remove() to delete
      tempdir when translating/importing shapes from zipfile
    - geoframe translate/convert without option -toframe- displayed an invalid
      geoframe create message; this is fixed

    11jul2024
    - suboption title() added to zoom()
    - glegend() can now create legend keys with composite symbols from several
      layers
    - slegend(, overlay): a minimum vertical skip equal to lineskip() is now enforced
      from one label to the next to prevent label overlap
    - enclaves were not displayed in white if the main color option contained an
      opacity operator; this is fixed

    06jul2024
    geoplot
    - the placement of symbols and labels in slegend() could be off if option
      angle() was applied to the symbol layer; this is fixed
    - option tfloat in slegend() now moves the labels closer to the symbols if
      feasible
    mata library:
    - geo_bshare() is now faster (typically by a factor of about 3-4)

    04jul2024
    geoplot
    - global option slegend() added
    - global option glegend() now has suboptions tsize(), tcolor(), and tangle()
      (as well as undocumented tgap()) to affect the rendering of the labels
    - global option glegend() now has suboption textfirst; suboption tposition()
      is discontinued
    - option textwidth() in glegend() did not allow multiple values; this is
      fixed
    geoframe
    - -geoframe set type- now checks whether the current coordinates setting is
      compatible with the specified type and clears the setting if not

    02jul2024
    geoplot:
    - options colgap(), tposition(), talign(), halign() and nospan added to
      glegend(); new default for textwidth() is 12

    02jul2024:
    geoplot:
    - global option glegend() added (repeatable)
    - layertype -areai- and -linei- added
    - inset():
        o suboption margin() added; by default, a 1% inner margin is added
        o padding() in suboption box() discontinued
        o suboption refdim() discontinued; now using global refdim() setting
        o some changes in defaults
    - suboption padding() in background() now supports marginexp
    - suboption padding() in grid() and tissot() now supports marginexp
    - layertype symbol now sets the default legend label to the name of the frame;
      option align(center) can now be used to center symbols
    geoframe:
    - option padding() in geoframe grid and geoframe tissot now supports marginexp

    28jun2024
    geoplot:
    - global option inset() added (repeatable)
    - suboption -refine- added in layer option box()
    - options -reverse- and -nostack- added in layertype bar
    - option -align()- added in layertype symbol

    25jun2024
    geoplot:
    - syntax -geoplot <layer> || [<layer> || ...] [, options]- is now allowed
      (global options must be separated by || in this case)
    - global options -project()-, -background()-, -grid()-, and -tissot()- added
    - global option -rotate()- added as an alias for -angle()-
    - global option -axes- added to turn Stata's coordinate system (plotregion and
      axes) on
    - default categorical color scale now -st-, no longer -Set1-
    - some changes related to latest update of -colorpalette- and -colrspace-;
      option -color()- may now behave somewhat differently in some situations
    - i.zvar now allowed if zvar is string
    - -ifshp()- in layer types -point- and -label- now takes account of linked
      shape frames (if there is any) even if option -shp- is not specified
    geoframe:
    - -geoframe create- has been rewritten and expanded:
      o it can now import data directly from ESRI or GeoJSON
      o options -noshapeframe- and -shapeframe()- renamed to -noshp- and -shp()-
      o it now also looks for a shapeframe when applied to the current frame
      o -type(unit)- has been renamed to -type(attribute)-; -type(unit)- is no
        longer supported
      o a consistent strategy is now used to determine the data type and decide
        on whether to load shape data if -type()- and -shp()- are not specified
      o existing geoframe characteristics are no longer overwritten unless
        a corresponding option is explicitly specified
      o when opening a file, old linkage settings/variables will now be cleared
        before proceeding
      o -geoframe load- (alias for -geoframe create-) is now undocumented
    - new -geoframe save- and -geoframe use- commands
    - -geoframe rename- now also renames the linked shape frame; specify option
      -noshp- for old behavior
    - most commands are now implemented in such a way that the original data
      is restored if the user hits Break
    - types of geometry items are now handled more consistently across subcommands
    - -geoframe query n- now only counts units that have a match in the shape frame
      and ignores duplicate units
    - -geoframe query items- added as a synonym for -geoframe query gtype-
    - several projections added to -geoframe project-; user command -geo2xy- is no
      longer used
    - new -geoframe rescale- command
    - new -geoframe tissot- command
    - -geoframe grid- now extends the lines to the range of the data (including
      padding) by default; specify option -noextend- for previous behavior; option
      -mesh- now only removes outer lines if there are 3 or more lines; furthermore,
      range and spacing of the grid is now determined in a such way that each grid
      line is placed at a nice (rounded) value (unless custom values are provided);
      specify option -tight- for old behavior
    - syntax and functionality of -geoframe bshare- changed
    - -geoframe simplify- now automatically applies -geoframe refine- after
      simplifying the shapes; type -norefine- for old behavior
    - -geoframe spjoin-: points that are exactly on the edge of a polygon are now
      always treated as inside the polygon; if a point is on the edge of
      several polygons, it is assigned to the first polygon against which it
    - -geoframe translate- now has option -toframe- to write data to frames rather
      than files; new command -geoframe import- added (wrapper for
      -geoframe translate- with option -toframe-) 
    - -geoframe translate esri- no longer uses command -spshape2dta-; it now
      directly employs -import_shp- and -import_dbase-; option -user- is
      discontinued
    - option -gtype()- in -geoframe translate json/wkt- now generates a labeled
      numeric variable, no longer a string variable
    - -geoframe copy- now has option -unique-: abort with error in case 3 or case 4
    - -geoframe copy- now supports -if- and -in-
    - -geoframe copy- now aborts with error if a variable to be copied already
      exists; specify option -relax- to skip such variables instead of aborting
      (option -relax- also added -geoframe collapse- and -geoframe contract-, but
      not documented)
    - -geoframe copy- now returns the (target) names of the copied variables in
      r(varlist)
    - -geoframe copy- failed in the both-frames-not-unique case if -target()- was
      specified; this is fixed
    - -geoframe copy- could fail if data was not sorted by matching ID; this is
      fixed
    - -geoframe collapse/contract- now check for syntax errors and varname
      conflicts before running the spatial join
    - -geoframe link- no longer aborts with error, if creating a physical linkage
      variable fails; geoframe commands now generally also work if the attribute
      frame contains duplicates (i.e. units with the same ID)
    - -geoframe link-: when searching for a name for the linkage variable,
      an existing linkage variable is now only protected if it still a valid
      geoframe linkage
    - -geoframe link- is no longer allowed if frame type is "shape" or "pc";
      -geoframe get shpframe- and -geoframe get linkname- now return nothing if
      frame type is "shape" or "pc"; conceptually, frame type "pc" is now treated
      as a shape frame
    - when reading from a zipfile, -geoframe translate- now extracts into a folder
      obtained by -tempfile-
    Mata library:
    - in general, consistent classification of geometry types is now used across
      functions
    - function geo_project() added
    - function geo_tissot() added
    - geo_bshare() rewritten and improved
    - geo_spjoin() did not handle points in "true" enclaves correctly (i.e. in
      enclaves without matching exclave in another unit); this is fixed
    - geo_spjoin() now evaluates points in random order so that progress is more
      even
    - geo_pointinpolygon() is now more informative (0 outside, 1 inside, 2 on edge,
      3 on vertex) and also (somewhat) faster in most situations
    - geo_simplify() revised; geo_pid() is now used to identify shape items;
      "triangle area*2 < delta" was erroneously used as evaluation criterion
      instead "triangle area < delta", this is fixed; lines with only 2 remaining
      points are no longer dropped if they have a length of at least
      2*sqrt(2*delta)

    24dec2023
    **note that -geoframe create- no longer makes the new frame the current
      frame by default; specify option -current- for old behavior**
    **note that syntax for immediate layer types (e.g. pointi, pci, symboli)
      has been revised**
    geoplot:
    - zvar can now be string
    - the specified order in option cuts() now affects the order of categories
      in the legend in case of categorical zvar
    - option -shp- added to layer types -point- and -label-, to use data from linked
      shape frame; support for options ifshp(), lock, id(), centroids() added
    - layertypes symbol and symboli now support marker labels
    - syntax for layertype symboli changed; argument size is now optional, but must
      be enclosed in []; can also specify *size for relative size; can specifiy
      option size() to set a default
    - layertypes pccapsymi, pcbarrowi, pcpointi added
    - consistent syntax is now used for all immediate layer types (i.e. coordinates
      are now specified as x y for all types, no longer as x y for some and as y x
      for others)
    - option label() in compass() now has suboption text() to override the default
      labels
    - in some situations, rendering options such as lcolor() specified in missing()
      did not have an effect; this is fixed
    - fill color was turned off if zvar contained missing only; this is fixed
    - specifying immediate plots without arguments is no longer considered an error
    - when copying from a shape frame, -geoframe copy- failed if the data in the
      shape frame was not ordered by the ID; this is fixed
    geoframe:
    - new command geoframe stack
    - geoframe append now also copies formats and labels, unless option -raw- is
      specified; also added options -force- and -fast-
    - syntax for -geoframe symboli- changed; see layertype symboli above
    - support for GeoJSON and WKT (Well-known text geometry) added
      (see -geoframe translate json- and -geoframe translate wkt-)
    - geoframe create no longer makes the created frame the current frame by
      default; specify option -current- to make the created frame the current frame
    - option -noclean- is now allowed as a synonym for -nodrop- in geoframe create
    - geoframe load can now be used as a synonym for geoframe create
    - geoframe query bbox did no longer work due to a change in geoframe bbox; this
      is fixed
    - geoframe clean failed to remove empty shapes; this is fixed

    02nov2023
    geoplot:
    - option ifshp() added for layer types area and line
    geoframe spjoin:
    - by default, data is now sorted by the generated ID variable; specify option
      nosort to omit sorting
    geoframe collapse/contract:
    - option nodots added
    - suboption nosort added in option generate()
    geoframe [r]clip:
    - the default in case of -noclip- (or when clipping point data) is now to
      treat each shape item individually; specify option -nosplit- to include or
      exclude items that belong to the same unit together
    geoframe refine:
    - argument -delta- is now interpreted as a divisor, not a multiplicator
    - now using a different algorithm to determine the points that leads to more
      similar distances
    geoframe project:
    - option ifshp() did not work correctly unless option into() was also specified;
      this is fixed
    geoframe bbox/symbol/symboli:
    - an attribute frame and a linked shape frame is now created, not just a shape
      frame (similar to geoframe grid)

    30oct2023
    geoplot:
    - change in default behavior of zoom(): only outer connecting lines between
      boxes are now printed by default (i.e. lines that neither cross the origin
      box nor the destination box); type -connect(all)- to print all four lines
    geoframe:
    - -geoframe refine- added
    - there was an error in the computation of the default value for delta in
      -geoframe simplify- such it was only about 1/4 of the value claimed in
      the documentation; the default value is now computed as half a pixel in a
      in a 2000x2000 bitmap and documentation has been updated

    30oct2023
    geoframe:
    - geoframe translate can now directly read from zipfiles
    geoplot:
    - zoom() has now option position() for explicit positioning
    - layer types area, line, point, label, pie, bar can now also be specified as
      areas, lines, points, labels, pies, bars
    - parsing of axis label options now takes into account that twoway allows an "s"
      at the end, e.g. xlabels() rather than xlabel(), even tough the options are
      documented without "s" in help axis_label_options

    23oct2023
    - geoframe project added
    - geoframe grid added
    - geoframe select: option shpif() renamed to ifshp()
    - geoframe select and geoframe duplicate no longer run geoframe describe; option
      nodescribe discontinued
    - geoframe select, simplify, bshare, and duplicate no longer change the current
      frame automatically
    - option current added to geoframe bbox/symbol/symboli

    18oct2023
    - -geoframe select- now has option shpif() to select observations based on 
      variables in the linked shape frame
    - -geoframe generate direction- added (generate variable containing direction
      of stapes)
    - -geoframe generate gtype- added (generate variable containing geometry
      type of shapes)
    - -geoframe translate- now has option -user- to use SSC command shp2dta
      instead of official spshape2dta
    - -geoframe translate- now has a workaround for the problem that spshape2dta
      chokes on paths that contain ".shp"
    - -geoframe convert- can now be used as a synonym for -geoframe translate-

    16oct2023
    - geoframe [r]clip:
      o observations that do not satisfy the if and in qualifiers
       are no longer removed from the data unless option into() is specified
      o progress dots are now displayed; type -nodots- to suppress
    - geograme simplify added
    - geoframe bshare added

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


