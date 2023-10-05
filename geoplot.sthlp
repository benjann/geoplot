{smcl}
{* 30sep2023}{...}
{hi:help geoplot}{...}
{right:{browse "https://github.com/benjann/geoplot/"}}
{hline}

{title:Title}

{pstd}{hi:geoplot} {hline 2} Command to draw maps from shape files


{title:Syntax}

{p 8 15 2}
    {cmd:geoplot} {cmd:(}{it:layer}{cmd:)} [{cmd:(}{it:layer}{cmd:)} ...]
    [{cmd:,}
    {help geoplot##opt:{it:global_options}}
    ]

{pstd}
    where {it:layer} is

{p 8 15 2}
    {help geoplot##layertype:{it:layertype}} [{it:frame}] [{it:...}] [{cmd:,}
    {help geoplot##zopts:{it:zvar_options}}
    {it:other_options} ]

{pstd}
    and {it:frame} is the name of a frame containing data prepared by
    {helpb geoframe}. {it:frame} may be omitted if
    there are no subsequent arguments (apart from {it:{help if}},
    {it:{help in}}, {it:{help weight}}, or options); in this case, the
    current (working) frame will be used. To use the current (working) frame
    you can also type {cmd:.} (missing).


{synoptset 20 tabbed}{...}
{marker layertype}{synopthdr:layertype}
{synoptline}
{synopt :{helpb geoplot##area:area}}shapes, potentially filled
    {p_end}
{synopt :{helpb geoplot##line:line}}shapes, line only
    {p_end}
{synopt :{helpb geoplot##point:point}}single-coordinate markers
    {p_end}
{synopt :{helpb geoplot##labels:{ul:lab}el}}single-coordinate labels
    {p_end}
{synopt :{helpb geoplot##symbol:{ul:sym}bol}}single-coordinate symbols (circles,
    hexagons, stars, etc.)
    {p_end}

{p2coldent:* {helpb geoplot##pie:pie}}pie charts
    {p_end}
{p2coldent:* {helpb geoplot##bar:bar}}stacked bar charts
    {p_end}

{synopt :{helpb geoplot##pcspike:pcspike}}paired-coordinate spikes
    {p_end}
{synopt :{helpb geoplot##pcspike:pccapsym}}paired-coordinate spikes capped with
    symbols
    {p_end}
{synopt :{helpb geoplot##pcspike:pcarrow}}paired-coordinate arrows
    {p_end}
{synopt :{helpb geoplot##pcspike:pcbarrow}}paired-coordinate arrows with two
    heads
    {p_end}
{synopt :{helpb geoplot##pcspike:pcpoint}}paired-coordinate markers
    {p_end}

{p2coldent:* {helpb geoplot##pointi:pointi}}{cmd:point} with immediate arguments
    {p_end}
{p2coldent:* {helpb geoplot##pointi:pci}}{cmd:pcspike} with immediate arguments
    {p_end}
{p2coldent:* {helpb geoplot##pointi:pcarrowi}}{cmd:pcarrow} with immediate
    arguments
    {p_end}
{p2coldent:* {helpb geoplot##symboli:symboli}}{cmd:symbol} with immediate arguments
    {p_end}
{synoptline}
{p 4 6 2}
    * {help geoplot##zopts:{it:zvar_options}} without asterisk not supported.


{synoptset 20 tabbed}{...}
{marker zopts}{synopthdr:zvar_options}
{synoptline}
{syntab :Main}
{synopt :{helpb geoplot##discrete:{ul:discr}ete}}treat
    {help geoplot##zvar:{it:zvar}} as discrete instead of continuous
    {p_end}
{synopt :{helpb geoplot##levels:{ul:lev}els({it:spec})}}number of levels and
    method to determine cuts
    {p_end}
{synopt :{helpb geoplot##cuts:cuts({it:numlist})}}use levels defined by
    specified cuts
    {p_end}
{synopt :{helpb geoplot##colorvar:{ul:colorv}ar({sf:[}i.{sf:]}{it:zvar})}}alternative to
    specifying {help geoplot##zvar:{it:zvar}} as argument
    {p_end}

{syntab :Styling}
{p2coldent:* {helpb geoplot##color:{ul:col}or({it:palette})}}colors
    {p_end}
{p2coldent:* {helpb geoplot##lwidth:{ul:lw}idth({it:list})}}line widths
    {p_end}
{p2coldent:* {helpb geoplot##lwidth:{ul:lp}attern({it:list})}}line patterns
    {p_end}
{p2coldent:* {helpb geoplot##lwidth:{ul:fi}ntensity({it:list})}}fill intensities
    {p_end}
{p2coldent:* {helpb geoplot##lwidth :{ul:m}symbol({it:list})}}marker symbols
    {p_end}
{p2coldent:* {helpb geoplot##lwidth:{ul:msiz}e({it:list})}}marker sizes
    {p_end}
{p2coldent:* {helpb geoplot##lwidth:{ul:msa}ngle({it:list})}}marker angles
    {p_end}
{p2coldent:* {helpb geoplot##lwidth:{ul:mlw}idth({it:list})}}marker outline widths
    {p_end}
{p2coldent:* {helpb geoplot##lwidth:{ul:mlabs}ize({it:list})}}marker label sizes
    {p_end}
{p2coldent:* {helpb geoplot##lwidth:{ul:mlabang}le({it:list})}}marker label angles
    {p_end}
{p2coldent:* {helpb geoplot##mlabcolor:{ul:mlabc}olor({it:palette})}}marker label colors
    {p_end}

{syntab :Legend keys}
{p2coldent:* {helpb geoplot##label:{ul:lab}el({it:spec})}}set labels of legend
    keys and related settings
    {p_end}
{synopt :{helpb geoplot##nolegend:nolegend}}do not consider the layer for the default
    legend
    {p_end}

{syntab :Missing}
{synopt :{helpb geoplot##missing:{ul:mis}sing({it:options})}}styling of elements
    for which {help geoplot##zvar:{it:zvar}} is missing
    {p_end}
{synoptline}
{p 4 6 2}
    * These options are also effective if {help geoplot##zvar:{it:zvar}} is not
    specified, albeit with different interpretation and syntax. See the
    descriptions of the options below.


{synoptset 20 tabbed}{...}
{marker opt}{synopthdr:global_options}
{synoptline}
{syntab :Main}
{synopt :{helpb geoplot##angle:{ul:ang}le({it:angle})}}rotate map by {it:angle}
    {p_end}
{synopt :{helpb geoplot##tight:tight}}adjust graph size to dimension of map
    {p_end}
{synopt :{helpb geoplot##margin:{ul:m}argin({it:spec})}}specify (minimum) margin
    around map
    {p_end}
{synopt :{helpb geoplot##refdim:{ul:ref}dim({it:spec})}}select reference
    dimension
    {p_end}
{synopt :{helpb geoplot##aspect:{ul:aspect}ratio({it:spec})}}adjust aspect
    ratio of map
    {p_end}
{synopt :{help geoplot##twopts:{it:twoway_options}}}twoway options, other than {cmd:by()}

{syntab :Legends}
{synopt :{helpb geoplot##legend:{ul:leg}end{sf:[}({it:options}){sf:]}}}add
    standard legend
    {p_end}
{synopt :{helpb geoplot##clegend:{ul:cleg}end{sf:[}({it:options}){sf:]}}}add
    {helpb contour} plot legend
    {p_end}
{synopt :{helpb geoplot##sbar:sbar{sf:[}({it:options}){sf:]}}}add scale bar
    {p_end}
{synopt :{helpb geoplot##compass:{ul:comp}ass{sf:[}({it:options}){sf:]}}}add
    compass
    {p_end}

{syntab :Zoom}
{synopt :{helpb geoplot##zoom:zoom({it:spec})}}zoom in on specific layers;
    can be repeated
    {p_end}

{syntab :Data}
{synopt :{helpb geoplot##frame:frame({it:spec})}}store ploted data in new frame
    {p_end}
{synopt :{helpb nograph}}do not generate a graph
    {p_end}
{synoptline}


{title:Description}

{pstd}
    {cmd:geoplot} draws maps from shape files and other datasets prepared by
    {helpb geoframe}. The procedure is to first create one or several frames
    containing the source data using {helpb geoframe} and then apply
    {cmd:geoplot} to plot the data from these frames.

{pstd}
    Multiple layers of elements such as regions, borders, lakes, roads, labels,
    etc., can be freely combined. The look of the elements can be varied
    depending on the values of variables.

{pstd}
    Some of the functions used by {cmd:geoplot} and {helpb geoframe} are provided
    in Mata library lgeoplot.mlib. See {helpb lgeoplot_source} for source and
    minimal documentation of these functions.

{pstd}
    {cmd:geoplot} requires {helpb colorpalette}, {helpb colrspace}, and
    {helpb moremata}. To install these packages type

        {com}. {stata ssc install palettes}{txt}
        {com}. {stata ssc install colrspace}{txt}
        {com}. {stata ssc install moremata}{txt}


{title:Layer types}

{marker area}{...}
{dlgtab:shapes, potentially filled}

{p 8 15 2}
    {cmd:area} {it:frame} [{help geoplot##zvar:{it:zvar}}] {ifin} {weight}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the shapes to be
    plotted (see {helpb geoframe}), {help geoplot##zvar:{it:zvar}} is an optional
    variable to determine styling, and {it:weight}, specified as
    {cmd:[}{cmdab:w:eight}{cmd:=}{it:exp}{cmd:]} or
    {cmd:[}{cmdab:iw:eight}{cmd:=}{it:exp}{cmd:]}, rescales the coordinates of
    the shapes by the absolute (and normalized) values of {it:exp}. Type
    {cmd:i.}{it:zvar} to treat {help geoplot##zvar:{it:zvar}} as a categorical
    variable. {it:options} are as follows.

{phang}
    {it:{help geoplot##zopts:zvar_options}} are options determining the look of
    the shapes (e.g. color) depending on the values of
    {help geoplot##zvar:{it:zvar}} as described
    {help geoplot##zvar_options:below}.

{phang}
    {cmd:wmax}[{cmd:(}{it:#}{cmd:)}] specifies a custom upper bound for
    normalization of weights. This is only relevant if {it:{help weight}} has
    been specified. The default is to normalize by max(1,{it:wmax}), where
    {it:wmax} is the observed maximum of the (absolute) weights (within
    layer). Specify {opt wmax(#)} to normalize by {it:#}. Specify {cmd:wmax}
    without argument to normalize by {it:wmax} even if {it:wmax}<1.

{marker size}{...}
{phang}
    {cmdab:si:ze(}{it:{help exp}}[{cmd:,} {opt s:cale(#)} {opt d:max(#)}]{cmd:)}
    resizes the shapes such that their sizes are proportional to {it:exp}
    (typically, {it:exp} is a {varname}; the size of a shape is equal to the
    area covered by the shape). Default normalization is such that the shape
    with the highest density (within layer), defined as abs({it:exp}) divided
    by the area of the shape, will keep its original size. Negative values in
    {it:exp} will be treated as positive; shapes for which {it:exp} is missing
    will keep their original size.

{pmore}
    Suboption {cmd:scale()} multiplies all sizes by {it:#} (after
    normalization). Suboption {cmd:dmax()} specifies a custom maximum density
    for normalization; use this option to make sizes comparable across layers.

{phang}
    {opth sel:ect(exp)} selects the shapes to be included in the
    plot. {cmd:select()} is applied after determining the cuts for {it:zvar}
    and after processing weights and {cmd:size()}. Specify {it:{help if}} or
    {it:{help in}} if you want to select observations upfront.

{phang}
    {cmdab:ec:olor(}{help colorpalette##colorlist:{it:colorspec}}{cmd:)}
    sets the fill color used for enclaves. {it:colorspec} is a (single) color
    specification as described in
    {helpb colorpalette##colorlist:colorpalette}. Default is
    {cmd:ecolor(white)}. The color of an enclave will only be visible if not
    covered by a corresponding exclave.

{phang}
    {it:{help area_options}} are options to affect the look of areas as
    described in {helpb twoway area}. For example, use option {cmd:lcolor()} to
    set the outline color. Color options support {it:colorspec} as described in
    {helpb colorpalette##colorlist:colorpalette}.

{phang}
    {opt lock} causes the orientation of the shapes to be unaffected by global
    option {helpb geoplot##angle:angle()}. That is, if {cmd:lock} is specified,
    {helpb geoplot##angle:angle()} will rotate positions only.

{phang}
    {opt box}[{cmd:(}{it:suboptions}{cmd:)}] draws a bounding box around the
    plotted shapes. {it:suboptions} are as follows.

{phang2}
    {opt rot:ate}, {opt cir:cle}, {opt hull}, {opt pad:ding(#)}, and {opt n(n)} as
    described in help {helpb geoframe##bbox:geoframe bbox}.

{phang2}
    {opt line} plots the bounding box using plot type {helpb geoplot##line:line}. The
    default is to plot the bounding box using plot type {helpb geoplot##area:area}. Note
    that, by default, the bounding box does not have a fill color even if option
    {cmd:line} is omitted. Specify {cmd:color(}{help colorpalette##colorlist:{it:colorspec}}{cmd:)} or
    {cmd:fcolor(}{help colorpalette##colorlist:{it:colorspec}}{cmd:)} to set a
    fill color.

{phang2}
    {it:{help area_options}} or {it:{help line_options}}, depending on whether
    option {cmd:line} has been specified, are regular graph options to affect
    the look of the bounding box. Color options support {it:colorspec} as described in
    {helpb colorpalette##colorlist:colorpalette}.

{phang}
    {opt f:eature(string)} specifies the type of feature represented in the
    layer. The default is to use the setting returned by
    {helpb geoframe##get:geoframe get feature}.

{phang}
    {opt coor:dinates(X Y)} specifies custom coordinate variables. The default
    is to use the variables returned by
    {helpb geoframe##get:geoframe get coordinates}.

{phang}
    {opt pl:evel(PLEVEL)} specifies a custom plot level variable. The default is
    to use the variable returned by {helpb geoframe##get:geoframe get plevel}.

{phang}
    {opt id(ID)} specifies a custom ID variable. The default is
    to use the variable returned by {helpb geoframe##get:geoframe get id}. An ID
    variable is not strictly needed, but if available it will be taken into
    account when categorizing {help geoplot##zvar:{it:zvar}}.

{phang}
    {opt centr:oids(X Y)} specifies custom centroid variables. The default is
    to use the variables returned by
    {helpb geoframe##get:geoframe get centroids}. If centroids are needed and
    no centroid variables are found, the centroids are computed on the fly using
    {helpb geoframe##generate:geoframe generate centroids}. The centroids are
    needed if weights, option {cmd:size()}, or option {cmd:lock} is specified.

{phang}
    {opt area(AREA)} specifies a custom shape size variable. The default is to
    use the variable returned by
    {helpb geoframe##get:geoframe get area}. If shape sizes are needed and no
    shape size variable is found, the sizes are computed on the fly using
    {helpb geoframe##generate:geoframe generate area}. Shape sizes
    are only needed if option {cmd:size()} is specified.

{pstd}
    By default, the shapes do not have a fill color. Specify
    {help geoplot##zvar:{it:zvar}} to color the shapes depending on the values
    of {help geoplot##zvar:{it:zvar}}. Specify
    {cmd:fcolor(}{help colorpalette##colorlist:{it:colorspec}}{cmd:)} to set a
    single fill color for all shapes.

{marker line}{...}
{dlgtab:shapes, line only}

{p 8 15 2}
    {cmd:line} {it:frame} [{help geoplot##zvar:{it:zvar}}] {ifin} {weight}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the shapes to be
    plotted (see {helpb geoframe}), {help geoplot##zvar:{it:zvar}} is an optional
    variable to determine styling, and {it:weight}, specified as
    {cmd:[}{cmdab:w:eight}{cmd:=}{it:exp}{cmd:]} or
    {cmd:[}{cmdab:iw:eight}{cmd:=}{it:exp}{cmd:]}, rescales the coordinates of
    the shapes by the absolute (and normalized) values of {it:exp}. Type
    {cmd:i.}{it:zvar} to treat {help geoplot##zvar:{it:zvar}} as a categorical
    variable. {it:options} are as follows.

{phang}
    {it:{help geoplot##zopts:zvar_options}}, {cmd:wmax()}, {cmd:size()}, {cmd:select()},
    {cmd:lock}, {opt box()}, {opt feature()}, {opt coordinates()}, {opt id()},
    {opt centroids()}, and {cmd:area()} are
    options as described for layer type {helpb geoplot##area:area}.

{phang}
    {it:{help line_options}} are options to affect the look of lines as
    described in {helpb twoway line}. For example, use option {cmd:lwidth()} to
    set the width of lines. Color options support {it:colorspec} as described in
    {helpb colorpalette##colorlist:colorpalette}.

{marker point}{...}
{dlgtab:single-coordinate markers}

{p 8 15 2}
    {cmd:point} {it:frame} [{help geoplot##zvar:{it:zvar}}] {ifin} {weight}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the points to be
    plotted (see {helpb geoframe}), {help geoplot##zvar:{it:zvar}} is an optional
    variable to determine styling, and {it:weight}, specified as
    {cmd:[}{cmdab:w:eight}{cmd:=}{it:exp}{cmd:]} or
    {cmd:[}{cmdab:iw:eight}{cmd:=}{it:exp}{cmd:]}, scales the size of the
    markers by the absolute (and normalized) values of {it:exp} (also see
    {help scatter##remarks14:Weighted markers} in {helpb twoway scatter}). Type
    {cmd:i.}{it:zvar} to treat {help geoplot##zvar:{it:zvar}} as a categorical
    variable. {cmdab:sc:atter} may be used as a synonym for {cmd:point}. {it:options}
    are as follows.

{phang}
    {it:{help geoplot##zopts:zvar_options}}, {cmd:wmax()}, {cmd:select()},
    {opt box()}, and {opt coordinates()} are
    options as described for layer type {helpb geoplot##area:area}.

{phang}
    {it:{help marker_options}}, {it:{help marker_label_options}},
    {it:{help connect_options}}, and {it:jitter_options} are options to affect
    the look of markers as described in {helpb twoway scatter}. For example,
    use option {cmd:msymbol()} to set the marker symbol. Color options
    support {it:colorspec} as described in {helpb colorpalette##colorlist:colorpalette}.

{marker labels}{...}
{dlgtab:single-coordinate labels}

{p 8 15 2}
    {cmd:label} {it:frame} {it:labelspec} [{help geoplot##zvar:{it:zvar}}] {ifin}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the coordinates of the labels to be
    plotted (see {helpb geoframe}), {it:labelspec} provides the labels,
    and {help geoplot##zvar:{it:zvar}} is an optional
    variable to determine styling. Type
    {cmd:i.}{it:zvar} to treat {help geoplot##zvar:{it:zvar}} as a categorical
    variable.

{pstd}
    The syntax of {it:labelspec} is as follows. To print labels from a variable, type

{p 8 15 2}
    {cmd:label} {it:frame} {help varname:{it:labelvar}} [{help geoplot##zvar:{it:zvar}}]
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    where {it:labelvar} may be a numeric variable or a string variable. To print
    custom labels, type

{p 8 15 2}
    {cmd:label} {it:frame} {cmd:("}{it:text}{cmd:"} [{cmd:"}{it:text}{cmd:"} {it:...}]{cmd:)}
    [{help geoplot##zvar:{it:zvar}}] {ifin} [{cmd:,} {it:options} ]

{pstd}
    If {help geoplot##zvar:{it:zvar}} is omitted, the first {cmd:"}{it:text}{cmd:"} element 
    will be assigned to all selected observations. Othewise
    the elements will be assigned one-by-one to the levels determined by
    {help geoplot##zvar:{it:zvar}} (using the first element for missing, if there
    are missing values; elements will be recycled if there are fewer elements than
    levels). SMCL markup directives can be used in {it:text}; see
    {help graph_text:{bf:[G-4]} {it:text}}. Finally, to print labels
    as defined by {helpb geoplot##label:label()} and
    {helpb geoplot##missing:missing(label())}, type

{p 8 15 2}
    {cmd:label} {it:frame} {cmd:.} [{help geoplot##zvar:{it:zvar}}] {ifin} [{cmd:,} {it:options} ]

{pstd}
    {it:options} are as follows.

{phang}
    {it:{help geoplot##zopts:zvar_options}}, {opt box()}, {cmd:select()}, and
    {opt coordinates()} are
    options as described for layer type {helpb geoplot##area:area}.

{phang}
    {opth si:ze(textsizestyle)},
    {cmdab:col:or(}{help colorpalette##colorlist:{it:colorspec}}{cmd:)},
    {opth ang:le(anglestyle)},
    {opth tsty:le(textstyle)},
    {opth f:ormat(%fmt)},
    {opth gap(size)},
    {opth pos:ition(clockposstyle)}, and
    {opth vpos:ition(varname)}
    are marker label options equivalent to the corresponding options
    described in help {it:{help marker_label_options}}. Color options
    support {it:colorspec} as described in {helpb colorpalette##colorlist:colorpalette}.

{pmore}
    If {help geoplot##zvar:{it:zvar}} is specified, options {cmd:size()},
    {cmd:color()}, and {cmd:angle()} are interpreted in the way as described
    {help geoplot##zvar_options:below} for {cmd:mlabsize()}, {cmd:mlabcolor()},
    and {cmd:mlabangle()}.

{pstd}
    Layer type {cmd:label} is implemented as a wrapper for {helpb geoplot##point:point}.

{marker symbol}{...}
{dlgtab:single-coordinate symbols}

{p 8 15 2}
    {cmdab:sym:bol} {it:frame} [{help geoplot##zvar:{it:zvar}}] {ifin} {weight}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the positions of
    the symbols (see {helpb geoframe}), {help geoplot##zvar:{it:zvar}} is an optional
    variable to determine styling, and {it:weight}, specified as
    {cmd:[}{cmdab:w:eight}{cmd:=}{it:exp}{cmd:]} or
    {cmd:[}{cmdab:iw:eight}{cmd:=}{it:exp}{cmd:]}, scales the symbols by the
    absolute (and normalized) values of {it:exp}. Type
    {cmd:i.}{it:zvar} to treat {help geoplot##zvar:{it:zvar}} as a categorical
    variable. {it:options} are as follows.

{phang}
    {opt sh:ape(spec)} selects or defines the shape of the symbols. {it:shape} may be

{p2colset 9 25 27 2}{...}
{p2col:{cmdab:c:ircle}}circle (or oval)
    {p_end}
{p2col:{cmdab:t:riangle}}triangle
    {p_end}
{p2col:{cmdab:s:quare}}square (or rectangle)
    {p_end}
{p2col:{cmdab:p:entagon}}pentagon
    {p_end}
{p2col:{cmdab:hex:agon}}hexagon
    {p_end}
{p2col:{cmdab:hep:tagon}}heptagon
    {p_end}
{p2col:{cmdab:o:ctagon}}octagon
    {p_end}
{p2col:{cmdab:a:rc} [{it:angle}]}section of circle; {it:angle} in [-360,360]
    {p_end}
{p2col:{cmdab:sl:ice} [{it:angle}]}slice of circle; {it:angle} in [-360,360]
    {p_end}
{p2col:{cmdab:star}}5-pointed star
    {p_end}
{p2col:{cmdab:star6}}6-pointed star
    {p_end}
{p2col:{cmdab:pentagr:am}}pentagram (5-pointed star with crossing lines)
    {p_end}
{p2col:{cmdab:hexagr:am}}hexagram (6-pointed star with crossing lines)
    {p_end}
{p2col:{cmdab:pin} [{it:headsize}]}pin (needle); {it:headsize} in [0,1]
    {p_end}
{p2col:{cmdab:pin2} [{it:headsize}]}alternative pin; {it:headsize} in [0,1]
    {p_end}
{p2col:{it:{help numlist}}}manual shape coordinates
    {p_end}
{p2col:{it:matname}}matrix containing shape coordinates
    {p_end}
{p2col:{it:name} [{it:arg}]}mata function {cmd:_geo_symbol_}{it:name}{cmd:()} returning shape coordinates
    {p_end}

{pmore}
    The default is {cmd:circle}. For {cmd:arc} and
    {cmd:slice}, argument {it:angle} in [-360,360] specifies the size of the
    segment in degrees; default is {cmd:180} (half circle). In case of
    {cmd:arc} you may want to specify option {cmd:line} (see below) to prevent
    connecting the first and the last point. For {cmd:pin} and {cmd:pin2}, argument
    {it:headsize} in [0,1] specifies the size of the head; default is one third for
    {cmd:pin} and 0.6 for {cmd:pin2}.

{pmore}
    Use {opt shape(numlist)} or {opt shape(matname)} to
    create a custom symbol. {it:{help numlist}} is a list of shape coordinates specified
    as

            {it:x1} {it:y1} [{it:x2} {it:y2} ...]

{pmore}
    {it:matname} is the name of a matrix containing the shape coordinates. The matrix may
    contain a single row or column of consecutive points or it may contain two
    rows or two columns, one for X and one for Y. For example, to create a
    kite symbol, you could type

            {cmd:shape(0 -1 .5 0 0 .5 -.5 0 0 -1)}

{pmore}
    or you could define a matrix such as

            {cmd:matrix KITE = (0, -1, .5, 0, 0, .5, -.5, 0, 0, -1)}

{pmore}
    or

            {cmd:matrix KITE = (0, .5, 0, -.5, 0)', (-1, 0, .5, 0, -1)'}

{pmore}
    and then type {cmd:shape(KITE)}. Furthermore, you may create a custom symbol
    by defining a Mata function
    {cmd:_geo_symbol_}{it:name}{cmd:()} and then call the function
    as {cmd:shape(}{it:name} [{it:arg}]{cmd:)}. Two arguments will be submitted to
    the function, real scalar {it:n} and string scalar {it:arg}. The function must
    return a numeric {it:r} x 2 matrix containing the (X,Y)
    coordinates of the symbol (or a {it:r} x 3 matrix containing the
    coordinates in the first two columns and a plot level indicator in the
    third column; use the plot level indicator in multi-part symbols
    to create white space by setting the plot level to 0 for
    regular parts and to 1 for white-space parts; more precisely, the parts
    of the symbol will be printed in ascending order of the plot level with
    even-level parts treated as regular parts and odd-level parts treated as
    white space). For example, to create a kite symbol, define function

            {com}real matrix _geo_symbol_Kite(real scalar n, string scalar arg)
            {
                pragma unused n
                pragma unused arg

                return(((0, .5, 0, -.5, 0)',(-1, 0, .5, 0, -1)'))
            }{txt}

{pmore}
    and then type {cmd:shape(Kite)}. Note that repeating the first point at the
    end is not strictly necessary (at
    least if option {cmd:line} is not specified). Multipart symbols can be
    created by introducing a missing point between parts.

{phang}
    {opt n(n)} sets the number of points used to draw a circle. Technically,
    a circle is drawn as a polygon with {it:n} edges; if {it:n} is large enough,
    the polygon appears as a smooth circle. This also means that you can use {cmd:shape(circle)}
    with low {it:n} to create regular polygons. In fact, symbols
    {cmd:triangle}, {cmdab:square}, ..., {cmd:octagon} are implemented as
    {cmd:shape(circle)} with {it:n} set to 3, 4, ..., 8, respectively. For {cmd:shape(circle)},
    the default is {it:n} = 100. For {cmd:shape(arc} {it:angle}{cmd:)} and
    {cmd:shape(slice} {it:angle}{cmd:)} the default is {it:n} =
    max(2, ceil(abs({it:angle})/360 * 100)). For {cmd:shape(pin)} and {cmd:shape(pin2)}, the default
    is {it:n} = max(4, ceil({it:headsize} * 100)).

{phang}
    {opt ratio(#)} adjusts the ratio between the height and the width of the symbols. The default
    is {cmd:ratio(1)}. For example, type {cmd:ratio(2)} to double the height.

{phang}
    {opt ang:le(angle)} rotates the symbols by {it:angle} degrees (counterclockwise). Global
    option {helpb geoplot##angle:angle()} has no effect on the orientation of the symbols.

{phang}
    {cmdab:si:ze(}[{cmd:*}]{it:exp}{cmd:)} sets or adjusts the size of the symbols. For
    the predefined symbols, {cmd:size()} sets the length of the
    radius of the circle enclosing the symbol (i.e. the distance between the center
    and the outermost point). For custom symbols, {cmd:size()} sets
    the unit length of the specified coordinates. Type {opt size(exp)} to specify
    an absolute size (i.e. in units of the underlying
    map). Alternatively, specify {cmd:size(*}{it:exp}{cmd:)} to multiply the default
    size by {it:exp}. The default size is set to 3% of the minimum of the horizontal
    and vertical size of the underlying map (as it exists at the point when the
    symbols are added, including the positions of the symbols; the smallest possible
    default size is 1).

{phang}
    {cmdab:off:set(}{it:offset} [{it:angle}]{cmd:)} offsets the positions of the
    symbols by {it:offset} percent of {cmd:size()} in the direction of {it:angle}. The
    default {it:angle} is 0 (east). For example, set {it:angle} to 90 for north, 180
    for west, or -45 for south-east.

{phang}
    {opt line} plots the symbols using plot type {helpb geoplot##line:line}. The
    default is to plot the symbols using plot type {helpb geoplot##area:area}. Note
    that, by default, the symbols do not have a fill color even if option
    {cmd:line} is omitted. Specify {help geoplot##zvar:{it:zvar}} to color the
    symbols depending on the values of {help geoplot##zvar:{it:zvar}}. Specify
    {cmd:color(}{help colorpalette##colorlist:{it:colorspec}}{cmd:)} or
    {cmd:fcolor(}{help colorpalette##colorlist:{it:colorspec}}{cmd:)} to set a
    single fill color for all symbols.

{phang}
    {it:{help area_options}} or {it:{help line_options}}, depending on whether
    option {cmd:line} has been specified, are regular graph options to affect
    the look of the symbols. Color options support {it:colorspec} as described in
    {helpb colorpalette##colorlist:colorpalette}.

{phang}
    {it:{help geoplot##zopts:zvar_options}}, {cmd:wmax()}, {cmd:select()}, {cmd:ecolor()}, {cmd:box()},
    {opt feature()}, and {opt coordinates()} are options as described for layer type
    {helpb geoplot##area:area}.

{marker pie}{...}
{dlgtab:pie charts}

{p 8 15 2}
    {cmd:pie} {it:frame} {varlist} {ifin} {weight}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the positions and data of
    the pie charts (see {helpb geoframe}), {it:varlist} specifies the variables
    determining the slice sizes (a separate pie chart will be created for each
    unit/row in the data), and
    {it:weight}, specified as {cmd:[}{cmdab:w:eight}{cmd:=}{it:exp}{cmd:]} or
    {cmd:[}{cmdab:iw:eight}{cmd:=}{it:exp}{cmd:]}, scales the pie charts by the
    absolute (and normalized) values of {it:exp}. {it:options}
    are as follows.

{phang}
    {opt asis} omits normalization of the values provided by {it:varlist}. By
    default, the values will be normalized such they sum to 100 across {it:varlist}
    in each row of the data. Option {cmd:asis} may be useful if the provided
    values are percentages and you want to leave part of the pie blank
    if the percentages do not sum up to 100.

{phang}
    {opt expl:ode(spec)} displays exploded pie charts. An exploded pie
    chart is a pie chart in which one or more slices are moved out from the
    center. {it:spec} may be {it:#} to move all slices out by {it:#} percent of
    the radius of the pie. Alternatively, {it:spec} may be

            {it:index} = {it:#} [{it:index} = {it:#} ...]

{pmore}
    where {it:index} is the index of the slice to be moved out ({cmd:1} for the
    slice corresponding to the first variable, {cmd:2} for the second, etc.)
    and {it:#} is the value of the shift in percent of the radius of the pie.

{phang}
    {opt polar} causes polar area diagrams to be displayed rather than regular
    pie charts. A polar area diagram is a pie chart in which each slice has the
    same angle and the values from {it:varlist} determine how far the
    slices extend from the center of the pie. In a regular pie chart the values of
    {it:varlist} determine the angles of the slices and all slices extend from the
    center by the same amount.

{phang}
    {opt rev:erse} travels in counterclockwise direction around the circle. The
    default is to arrange the slices in clockwise order.

{phang}
    {opt mid} shifts the start of the first slice by half its angle. By default,
    the edge of the first slice starts at 90 degrees (north). Specify {cmd:mid}
    to center the first slice at 90 degrees.

{phang}
    {opt ang:le(angle)} rotates the pies by {it:angle} degrees
    (counterclockwise). The default is {cmd:angle(90)} such that the first
    slice of a pie starts at an angle of 90 degrees (north). For example, type
    {cmd:angle(0)} place the first slice at 0 degrees (east). Global
    option {helpb geoplot##angle:angle()} has no effect on the orientation of the pies.

{phang}
    {cmdab:si:ze(}[{cmd:*}]{it:exp}{cmd:)} sets or adjusts the size of the
    pies. Type {opt size(exp)} to specify an absolute size (i.e. in units of the underlying
    map) for the radius of the pie. Alternatively, specify {cmd:size(*}{it:exp}{cmd:)}
    to multiply the default radius by {it:exp}. The default radius is set to 3% of the minimum
    of the horizontal and vertical size of the underlying map (as it exists at the point when the
    symbols are added, including the positions of the pies; the smallest possible
    default radius is 1).

{phang}
    {cmdab:off:set(}{it:offset} [{it:angle}]{cmd:)} offsets the positions of the
    pies by {it:offset} percent of their radius in the direction of {it:angle}. The
    default {it:angle} is 0 (east). For example, set {it:angle} to 90 for north, 180
    for west, or -45 for south-east.

{phang}
    {opt out:line}[{cmd:(}{it:options}]{cmd:)} adds an outline circle to each pie
    by calling layertype {helpb geoplot##symbol:symbol}. This may
    be useful, for example, to complete the circle for the part of a pie that may be
    left empty if {cmd:asis} is specified, to draw a reference outline in
    case of {cmd:polar}, or to create doughnut charts. {it:options} are as follows.

{phang2}
    {opt below} plots the outlines below the pie charts. The the default is
    to plot the outlines on top.

{phang2}
    {opt now:eight} requests that the specified weights be ignored when
    determining the sizes of the outlines.

{phang2}
    {cmd:size()}, {cmd:ratio()}, {cmd:n()}, {cmd:offset()}, {cmd:line},
    and {it:{help area_options}} or {it:{help line_options}} are
    options as described for layertype {helpb geoplot##symbol:symbol}. Several
    of these options will be set automatically, which can be overridden by specifying
    the options explicitly. {cmd:size(*}{it:#}{cmd:)} will be interpreted
    as {it:#} times the size used for the pies.

{phang}
    {opt n(n)} sets the number of points used to draw a circle. Default is
    {cmd:n(100)}. Each slice will use a fraction of points corresponding to the
    angle of the slice (with an imposed minimum of 2 points).

{phang}
    {opt nolab:el} uses variable names rather variable labels in the legend.

{phang}
    {it:{help geoplot##zopts:zvar_options}}, {cmd:wmax()}, {cmd:select()},
    {it:{help area_options}}, {cmd:box()}, and {opt coordinates()} are options as described
    for layer type {helpb geoplot##area:area}. {it:zvar_options} without asterisk
    will be ignored.

{marker bar}{...}
{dlgtab:stacked bar charts}

{p 8 15 2}
    {cmd:bar} {it:frame} {varlist} {ifin} {weight}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the positions and data of
    the bar charts (see {helpb geoframe}), {it:varlist} specifies the variables determining
    the bar segments (a separate bar chart will be created for each unit/row in the data), and
    {it:weight}, specified as {cmd:[}{cmdab:w:eight}{cmd:=}{it:exp}{cmd:]} or
    {cmd:[}{cmdab:iw:eight}{cmd:=}{it:exp}{cmd:]}, scales the bar charts by the
    absolute (and normalized) values of {it:exp}. {it:options}
    are as follows.

{phang}
    {opt asis} omits normalization of the values provided by {it:varlist}. By
    default, the values will be normalized such they sum to 100 across {it:varlist}
    in each row of the data. Option {cmd:asis} may be useful if the provided
    values are percentages and you want to leave part of the bar chart blank
    if the percentages do not sum up to 100.

{phang}
    {opt ang:le(angle)} rotates the bar charts by {it:angle} degrees
    (counterclockwise). The default is to display upright bar charts. For
    example, Type {cmd:angle(-90)} for horizontal bar charts (west to east). Global
    option {helpb geoplot##angle:angle()} has no effect on the orientation of the symbols.

{phang}
    {cmdab:si:ze(}[{cmd:*}]{it:exp}{cmd:)} sets or adjusts the size of the
    bar charts. Type {opt size(exp)} to specify an absolute size (i.e. in units of
    the underlying map) for the width of the bars. Alternatively, specify {cmd:size(*}{it:exp}{cmd:)} to
    multiply the default width by {it:exp}. The default width is set to 3% of the minimum
    of the horizontal and vertical size of the underlying map (as it exists at the
    point when the symbols are added, including the positions of the bar charts;
    the smallest possible default width is 1).

{phang}
    {opt ratio(#)} adjusts the ratio between the height and the width of the
    bars charts. The default is {cmd:ratio(2)}.

{phang}
    {cmdab:off:set(}{it:offset} [{it:angle}]{cmd:)} offsets the positions of the
    bar charts by {it:offset} percent of their width in the direction of {it:angle}. The
    default {it:angle} is 0 (east). For example, set {it:angle} to 90 for north, 180
    for west, or -45 for south-east.

{phang}
    {opt out:line}[{cmd:(}{it:options}]{cmd:)} adds an outline frame to each bar
    chart by calling layertype {helpb geoplot##symbol:symbol}. This may
    be useful, for example, to illustrate the part of the bar chart
    that may be left empty if {cmd:asis} is specified. {it:options} are as follows.

{phang2}
    {opt below} plots the outlines below the bar charts. The the default is
    to plot the outlines on top.

{phang2}
    {opt now:eight} requests that the specified weights be ignored when
    determining the sizes of the outlines.

{phang2}
    {cmd:size()}, {cmd:ratio()}, {cmd:angle()}, {cmd:offset()}, {cmd:line},
    and {it:{help area_options}} or {it:{help line_options}} are
    options as described for layertype {helpb geoplot##symbol:symbol}. Several
    of these options will be set automatically, which can be overridden by specifying
    the options explicitly. {cmd:size(*}{it:#}{cmd:)} will be interpreted
    as {it:#} times the size used for the bar charts.

{phang}
    {opt nolab:el} uses variable names rather variable labels in the legend.

{phang}
    {it:{help geoplot##zopts:zvar_options}}, {cmd:wmax()}, {cmd:select()},
    {it:{help area_options}}, {cmd:box()}, and {opt coordinates()} are options as described
    for layer type {helpb geoplot##area:area}. {it:zvar_options} without asterisk
    will be ignored.

{marker pcspike}{...}
{dlgtab:paired-coordinate spikes, arrows, or markers}

{p 8 15 2}
    {cmd:pcspike} {it:frame} [{help geoplot##zvar:{it:zvar}}] {ifin}
    [{cmd:,} {it:options} ]

{p 8 15 2}
    {cmd:pccapsym} {it:frame} [{help geoplot##zvar:{it:zvar}}] {ifin}
    [{cmd:,} {it:options} ]

{p 8 15 2}
    {cmd:pcarrow} {it:frame} [{help geoplot##zvar:{it:zvar}}] {ifin}
    [{cmd:,}  {it:options} ]

{p 8 15 2}
    {cmd:pbcarrow} {it:frame} [{help geoplot##zvar:{it:zvar}}] {ifin}
    [{cmd:,} {it:options} ]

{p 8 15 2}
    {cmd:pcpoint} {it:frame} [{help geoplot##zvar:{it:zvar}}] {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    where {it:frame} is the frame containing the paired coordinates (see
    {helpb geoframe}) and {help geoplot##zvar:{it:zvar}} is an optional
    variable to determine styling. Type
    {cmd:i.}{it:zvar} to treat {help geoplot##zvar:{it:zvar}} as a categorical
    variable. {cmd:pcscatter} may be used as a synonym
    for {cmd:pcpoint}. {it:options} are as follows.

{phang}
    {it:{help geoplot##zopts:zvar_options}}, {cmd:select()}, and {opt box()} are
    options as described for layer type {helpb geoplot##area:area}.

{phang}
    Options to affect the rendering of the spike
    lines, arrows, and markers as described in {helpb twoway pcspike},
    {helpb twoway pccapsym}, {helpb twoway pcarrow}, and
    {helpb twoway pcscatter}, respectively. Color options support
    {it:colorspec} as described in {helpb colorpalette##colorlist:colorpalette}.

{phang}
    {opt coor:dinates(X1 Y1 X2 Y2)} specifies custom coordinate variables. The
    default is to use the variables returned by
    {helpb geoframe##get:geoframe get coordinates}.

{marker pointi}{...}
{dlgtab:point, spikes, or arrows with immediate arguments}

{p 8 15 2}
    {cmd:pointi} {it:immediate_values} [{cmd:,} {opt lab:el(label)} {it:options} ]

{p 8 15 2}
    {cmd:pci} {it:immediate_values} [{cmd:,} {opt lab:el(label)} {it:options} ]

{p 8 15 2}
    {cmd:pcarrowi} {it:immediate_values} [{cmd:,} {opt lab:el(label)} {it:options} ]

{pstd}
    where {it:immediate_values} and {it:options} as described in
    {helpb twoway scatteri}, {helpb twoway pci}, and {helpb twoway pcarrowi},
    respectively. Color options support {it:colorspec} as described in
    {helpb colorpalette##colorlist:colorpalette}. {cmd:scatteri} may be used as
    a synonym for {cmd:pointi}. Option {opt label(label)}, where {it:label} is
    {cmd:"}{it:text}{cmd:"} [{cmd:"}{it:text}{cmd:"} {it:...}], sets a key label
    for use by {helpb geoplot##legend:legend()}. Multiple lines are created if
    multiple {cmd:"}{it:text}{cmd:"} elements are specified.

{pstd}
    Note that {cmd:geoplot} passes {it:immediate_values} through to the underlying
    graph command as is. This means that coordinates must be specified in
    revere order (i.e. Y followed by X, not X followed by Y).

{marker symboli}{...}
{dlgtab:symbol with immediate arguments}

{p 8 15 2}
    {cmd:symboli} {it:x1} {it:y1} {it:size1} [{it:x2} {it:y2} {it:size2} ...]
        [{cmd:,} {opt lab:el(label)} {it:options} ]

{pstd}
    where each set of coordinates and size creates a symbol of the specified
    size at the specified position. Option
    {opt label(label)} specifies a key label for the legend. {it:options} are
    as described for layertype {helpb geoplot##symbol:symbol}; option
    {cmd:size()} will be ignored.


{title:Options}

{marker zvar_options}{...}
{dlgtab:zvar options}

{marker zvar}{...}
{pstd}
    Many layer types support (numeric) variable {help varname:{it:zvar}} (or
    {cmd:i.}{help varname:{it:zvar}}) as an argument. Specify {it:zvar}
    to determine the colors and other aspects of the
    units displayed in the layer by the values of {it:zvar}. The
    range of values of {it:zvar} will be divided into levels and each
    level will be represented by a different style. Use the options below to
    determine how {it:zvar} is divided into levels, how the levels will be
    styled, and how the levels will be represented in the legend. Type
    {cmd:i.}{it:zvar} to treat {help varname:{it:zvar}} as a categorical variable.

{marker discrete}{...}
{phang}
    {opt discr:ete} treats {help geoplot##zvar:{it:zvar}} as a discrete
    variable. Each unique value of {help geoplot##zvar:{it:zvar}} will form a
    separate level. Typing {cmd:i.}{it:zvar} implies {cmd:discrete}. Note that
    {cmd:i.}{it:zvar} only allows positive and integer values, while
    {cmd:discrete} does not impose such a restriction.

{marker levels}{...}
{phang}
    {cmd:levels(}[{it:#}][{cmd:,} {it:method} {opth w:eight(varname)}]{cmd:)} specifies the number of
    levels to be formed by {help geoplot##zvar:{it:zvar}}. By default, a regular grid
    (equidistant cuts) of 5 intervals from the observed minimum to the observed
    maximum of {help geoplot##zvar:{it:zvar}} will be used (first interval closed, remaining intervals
    left-open). Specify {opt levels(#)} to create {it:#} intervals. Specify {it:method}
    to use a non-regular grid, where {it:method} can be one of the following.

{p2colset 13 23 25 2}{...}
{p2col: {cmdab:q:uantile}}use quantiles as cuts
    {p_end}
{p2col: {cmdab:k:means}}use cuts determined by {helpb cluster kmeans}
    {p_end}

{pmore}
    The number of resulting levels may be less than {it:#} with these methods, depending on the
    distribution of {help geoplot##zvar:{it:zvar}}. In case of {cmdab:quantile}, weights to be taken
    into account when computing the quantiles can be specified in suboption {cmd:weight()}. Option
    {cmd:levels()} has no effect if {cmd:i.}{it:zvar} or
    {helpb geoplot##discrete:discrete} is specified.

{marker cuts}{...}
{phang}
    {opth cuts(numlist)} is an alternative to {cmd:levels()} and
    provides custom breaks for the levels to be formed by
    {help geoplot##zvar:{it:zvar}}. For continuous {help geoplot##zvar:{it:zvar}},
    specify {it:#}+1 breaks to form {it:#} intervals (first interval closed, remaining
    intervals left-open). Observations with values below the first break or above
    the last break will be ignored. If {cmd:i.}{it:zvar} or {helpb geoplot##discrete:discrete}
    is specified, {it:numlist} is interpreted as the distinct values of
    {help geoplot##zvar:{it:zvar}} to be selected.

{marker colorvar}{...}
{phang}
    {opt colorvar(zvar)} is an alternative to specifying
    {help geoplot##zvar:{it:zvar}} as an argument. If both are specified,
    {opt colorvar()} will take precedence. Type {cmd:colorvar(i.}{it:zvar}{cmd:)}
    to treat {it:zvar} as categorical.

{marker color}{...}
{phang}
    {cmd:color(}[{it:{help colorpalette##palette:palette}}] [{cmd:,}
    {it:{help colorpalette##opts:palette_options}}]{cmd:)}
    selects the colors to be used for the levels. {it:palette} is any
    palette allowed by {helpb colorpalette} (which can also be a simple list of colors, see
    help {it:{help colorpalette##colorlist:colorlist}}) and {it:palette_options} are
    corresponding options. For example, type {cmd:colors(hcl bluered, reverse)} for a
    red to blue HCL color scheme. The default is {cmd:color(viridis)} or,
    if {cmd:i.}{it:zvar} or {helpb geoplot##discrete:discrete} is specified,
    {cmd:color(Set1)}. An appropriate number of colors will automatically
    be retrieved from {it:palette} (applying interpolation or recycling, if necessary,
    depending on type of palette). Specify a single color if you do not want
    the color to depend on the values of {help geoplot##zvar:{it:zvar}}.

{pmore}
    If {help geoplot##zvar:{it:zvar}} is omitted, {cmd:color()}
    is interpreted as a standard graph option; in this case, the syntax is
    {cmdab:color(}{help colorpalette##colorlist:{it:colorspec}}{cmd:)}.

{marker lwidth}{...}
{phang}
    {opt lwidth(list)},
    {opt lpattern(list)},
    {opt fintensity(list)},
    {opt msymbol(list)},
    {opt msize(list)},
    {opt msangle(list)},
    {opt mlwidth(list)},
    {opt mlabsize(list)}, and
    {opt mlabangle(list)} specify lists of line widths, line pattern, fill
    intensities, markers symbols, marker sizes, marker angles, marker outline
    widths, marker label sizes, and marker label angles to be used for the
    levels. The availability of these options depends on layer type; for example,
    marker options will be available with {helpb geoplot##point:point}, but not
    with {helpb geoplot##area:area} or {helpb geoplot##line:line}.

{pmore}
    {it:list} can be a {it:{help numlist}} or an appropriate
    {it:stylelist} ({it:{help linewidthstyle}list},
    {it:{help linepatternstyle}list},
    {it:{help intensitystyle}list},
    {it:{help symbolstyle}list},
    {it:{help markersizestyle}list},
    {it:{help anglestyle}list}, or
    {it:{help textsizestyle}list}). Specify {it:{help numlist}} with exactly two
    elements, e.g. {cmd:lwidth(0.5 1)}, to use a regular grid of values
    between the two numbers. In all other cases, the specified
    elements will be recycled if the number of levels is larger than the
    number of elements in {it:list}. Omit an option or specify a single
    element, e.g. {cmd:lwidth(thin)}, if you want to keep a property
    constant across levels.

{pmore}
    If {help geoplot##zvar:{it:zvar}} is omitted, the options are
    interpreted as a standard graph options with their regular syntax.

{marker mlabcolor}{...}
{phang}
    {cmd:mlabcolor(}[{it:{help colorpalette##palette:palette}}] [{cmd:,}
    {it:{help colorpalette##opts:palette_options}}]{cmd:)} is like
    {helpb geoplot##color:color()} but affects marker labels
    only.

{marker label}{...}
{phang}
    {cmd:label(}[{it:labelinfo}] [{cmd:,} {it:options}]{cmd:)} determines how
    the levels formed by {help geoplot##zvar:{it:zvar}} will be labeled in
    {helpb geoplot##legend:legend()}. {it:labelinfo} is

            [{it:value} [{cmd:=}]] {it:label} [ {it:value} [{cmd:=}] {it:label} {it:...} ]

{pmore}
    where {it:value} is the index of the level to be affected
    in case of continuous {it:zvar}, or the relevant value of {it:zvar} in case
    of {cmd:i.}{it:zvar} or {helpb geoplot##discrete:discrete}, and {it:label} is

            {cmd:"}{it:text}{cmd:"} [{cmd:"}{it:text}{cmd:"} {it:...}]

{pmore}
    (a multiline label will be created if {it:label} contains multiple
    {cmd:"}{it:text}{cmd:"} elements).

{pmore}
    {it:value} may be {it:#} to select a specific level
    (e.g., in case of continuous {it:zvar}, {cmd:1} for the 1st level,
    {cmd:2} for 2nd level, etc.) or it may be a specification including
    wildcard characters {cmd:*} and {cmd:?} to select multiple levels
    (e.g. {cmd:?} for levels 1-9, {cmd:1?} for levels 10-19, {cmd:*} for all
    levels). In any case, the label of the first matching
    specification will be assigned. You may also type {cmd:label(}{it:label}{cmd:)}
    rather than {cmd:label(}{cmd:*} = {it:label}{cmd:)} to assign the
    same (type of) label to all levels.

{pmore}
    {it:text} may include the following placeholders:

{p2colset 13 20 22 2}{...}
{p2col: {cmd:@lb}}lower bound of the interval
    {p_end}
{p2col: {cmd:@mid}}middle of the interval
    {p_end}
{p2col: {cmd:@ub}}upper bound of the interval
    {p_end}
{p2col: {cmd:@lab}}value label in case of {cmd:i.}{it:zvar} or
    {helpb geoplot##discrete:discrete}, else equivalent to {cmd:@lb - @ub}
    {p_end}
{p2col: {cmd:@n}}number of units
    {p_end}

{pmore}
    In case of {cmd:i.}{it:zvar} or {helpb geoplot##discrete:discrete}, the default is
    {cmd:label("@lab")}. For example, type {cmd:label("@lab (@n)")} to
    include the number of units in the label. For continuous {help geoplot##zvar:{it:zvar}},
    the default is equivalent to {cmd:label(1 = "[@lb,@ub]" * = "(@lb,@ub]")}. For example,
    type {cmd:label("@lb - @ub")} or {cmd:label("@lab")} to create labels
    formatted as "{it:lb} - {it:ub}", where {it:lb} and {it:ub} are the lower
    and upper bounds of the intervals.

{pmore}
    {it:options} are as follows.

{phang2}
    {opt nol:abel} omits the use of value labels in case of {cmd:i.}{it:zvar} or
    {helpb geoplot##discrete:discrete}.

{phang2}
    {opth f:ormat(%fmt)} selects the display format to be applied to numeric
    values. The default is to use the display format of {help geoplot##zvar:{it:zvar}}.

{phang2}
    {opt r:everse} reverses the order of the legend keys.

{pmore}
    If {help geoplot##zvar:{it:zvar}} is omitted in a layer (such that, technically,
    the layer only contains a single level), type {cmd:label(}{it:label}{cmd:)} to
    set the label of the layer's legend key (the default is to use the name of
    the plotted frame as label). The above {it:options} and placeholders are ineffective in this case.

{marker nolegend}{...}
{phang}
    {opt nolegend} requests that the current layer not be considered for the
    legend that is displayed by default if there are layers containing a
    {help geoplot##zvar:{it:zvar}}. {opt nolegend} only affects the default
    behavior; you can still include the layer manually in the legend using the
    {cmd:layer()} suboption of {helpb geoplot##legend:legend()}.

{marker missing}{...}
{phang}
    {opt missing(options)} specifies the styling of elements for which {help geoplot##zvar:{it:zvar}}
    is missing, where {it:options} are as follows.

{phang2}
    {cmdab:lab:el("}{it:text}{cmd:"} [{cmd:"}{it:text}{cmd:"} {it:...}]{cmd:)}
    sets a label for missing in the legend. The default is
    {cmd:"no data"}. Multiple lines are created if multiple
    {cmd:"}{it:text}{cmd:"} elements are specified. {cmd:@n} can be used in
    {it:text} as a placeholder for the number of missing units.

{phang2}
    {opt nolab:el} omits missing from the legend.

{phang2}
    {opt first} places missing at the top (or leftmost) rather than at
    the bottom (or rightmost) of the legend.

{phang2}
    {opt nogap} omits the gap between missing and the other keys in the legend.

{phang2}
    {cmd:color()}, {cmd:lwidth()}, {cmd:lpattern()}, etc. are standard graph
    options depending on layer type. In case of {helpb geoplot##area:area},
    the default is to use color {cmd:gs14} for areas for which {help geoplot##zvar:{it:zvar}} is missing.

{dlgtab:Global options}

{marker angle}{...}
{phang}
    {opt angle(angle)} rotates the map by {it:angle} degrees (counterclockwise).

{marker tight}{...}
{phang}
    {opt tight} adjusts the size of the overall graph such that it tightly fits
    the map. If {cmd:tight} is not specified, the size of the graph is as set by
    the chosen {help scheme:graph scheme}. Specify {cmd:tight} to remove
    possible white space. By default, {cmd:tight} preserves the vertical size
    of the graph and adjust the horizontal size such that the graph has the
    same aspect ratio as the map. However, if {helpb region_options:xsize()}
    is specified, the vertical size is adjusted. If {helpb region_options:xsize()}
    and {helpb region_options:ysize()} are both specified, {cmd:tight} has no effect.

{pmore}
    Note that {cmd:tight} may not remove all white space if elements outside the
    plot region, e.g. a {helpb title_options:title()}, are added to the
    graph (in this case the available space for the map may have a different
    aspect ratio than the overall graph, which means that the map will not use up
    all available space). Also note that, by default, {cmd:geoplot} includes a small margin around
    the plotregion such that the map will not touch the edges of the
    graph. Specify {helpb region_options:graphregion(margin(zero))} to remove this margin.

{marker margin}{...}
{phang}
    {cmd:margin(}{it:marginexp}{cmd:)} sets the margin by which
    the plotregion extends the size of the map, where {it:marginexp} is
    {it:#} [{it:#} {it:#} {it:#}] or one or more elements of the form

            {{cmd:l}|{cmd:r}|{cmd:b}|{cmd:t}} [{cmd:=}] {it:#}

{pmore}
    such as {cmd:l=5} or {cmd:t=10}. Default is {cmd:margin(0)},
    which means that the plotregion will tightly fit the map. Specify, for example,
    {cmd:margin(1)} to increase the plotregion by 1 percent on each side of the
    map. The default reference for computing the margins is the minimum of the horizontal
    and vertical size of the map; also see option {helpb geoplot##refdim:refdim()}.

{pmore}
    Specify {cmd:margin(}{it:#} {it:#} {it:#} {it:#}{cmd:)} to use different
    margins on the left, right, bottom, and top (numbers will be recycled if
    less than four numbers are specified). For example, {cmd:margin(10 5 0 7)}
    will increase the plotregion by 10 percent on the left, 5 percent on the
    right, 0 percent at the bottom, and 7 percent at the top.

{pmore}
    Alternatively, use the {{cmd:l}|{cmd:r}|{cmd:b}|{cmd:t}}{cmd:=}{it:#}
    syntax. For example, {cmd:margin(r=5 t=10)} will increase the plotregion
    by 5 percent on the right and by 10 at the top (and by 0 percent on the other
    sides).

{pmore}
    {cmd:margin()} may be useful, for example, if you need to make space for a
    legend, such that it does not cover parts of the map.

{marker refdim}{...}
{phang}
    {cmd:refdim(}{it:spec}{cmd:)} selects the reference dimension for size
    calculations (e.g., when determining margins). {it:spec} may be {cmd:y} or
    {cmdab:v:ertical} for the vertical dimension, or {cmd:x} or {cmdab:h:orizontal}
    for the horizontal dimension. If {cmd:refdim()} is omitted, the
    minimum of the horizontal and vertical size of the map is used a the
    reference size.

{marker aspect}{...}
{phang}
    {cmd:aspectratio(}[{it:#}] [{cmd:,} {it:pos_option}]{cmd:)} may be used to
    adjust the aspect ratio of the map and to determine the placement of the
    plotregion in the graph. By default, {cmd:geoplot} sets the aspect ratio
    in a way such that horizontal and vertical distances are proportional to
    the numeric values of the coordinates. Specify {opt aspectratio(#)} to multiply
    the aspect ratio determined by {cmd:geoplot} by {it:#} (values between 0 and 1
    will compress the map vertically, values larger than 1 will compress the map
    horizontally). For {it:pos_option} see help {it:{help aspect_option}}.

{marker twopts}{...}
{phang}
    {it:twoway_options} are general twoway options, other than {cmd:by()}, as
    documented in help {it:{help twoway_options}}. {cmd:geoplot} imposes own
    default settings for options such as {helpb region_options:graphregion()},
    {helpb region_options:plotregion()}, {helpb axis_scale_options:xscale()},
    {helpb axis_scale_options:yscale()}, or {cmd:bgcolor()} (an apparently
    undocumented option to set the color of the canvas, which only becomes
    visible if the graphregion has no color). Specify such options manually to
    override {cmd:geoplot}'s defaults.

{marker legend}{...}
{phang}
    {cmd:legend}[{cmd:(}{it:options}{cmd:)}] prints a composite legend of the objects from one
    or several layers. {it:suboptions} are as follows:

{phang2}
    {opt l:ayout(layout)} selects and arranges the layers to be included in the
    legend. {it:layout} is

                {it:el} [ {it:el} ... ]

{pmore2}
    where {it:el} is one of

{p2colset 17 26 28 2}{...}
{p2col: {it:#}}include the legend keys of layer {it:#}
    {p_end}
{p2col: {cmd:.}}add a gap between layers
    {p_end}
{p2col: {cmd:|}}start a new column (or a new row)
    {p_end}
{p2col: {cmd:-} {it:title}}add a subtitle, where {it:title} is
    {cmd:"}{it:text}{cmd:"} [{cmd:"}{it:text}{cmd:"} {it:...}]
    {p_end}

{phang2}
    {opt bot:tom} aligns the legend keys at the bottom (rightmost) if there are multiple
    columns (rows) in the legend. The default is to align the keys at the top (leftmost).

{phang2}
    {opt hor:izontal} arranges the legend horizontally (i.e., in rows). The default is to
    arrange the legend vertically (in columns).

{phang2}
    {opt rev:erse} reverses the order of the legend keys within layers.

{phang2}
    {opt position(spec)} overrides the default location of the legend,
    which is in the upper right corner. {it:spec} may be {it:{help compassdirstyle}}
    or {it:{help clockposstyle}}.

{phang2}
    {opt out:side} places the legend outside of the plot region. The default is
    to place the legend inside the plot region.

{phang2}
    {it:contents} and {it:location} options are further options to affect the
    rendering of the legend as documented in {it:{help legend_option}}. Be
    careful with options {cmd:order()}, {cmd:rows()}, {cmd:cols()}, and
    {cmd:colfirst}, as these options are set automatically by {cmd:geoplot} and
    you may not want to override these settings. Option
    {cmd:bplacement()} will be ignored.

{pmore}
    Type {cmd:legend(off)} or {cmd:nolegend} to suppress the legend that is printed by default if
    {help geoplot##zvar:{it:zvar}} has been specified in at least one layer.

{marker clegend}{...}
{phang}
    {cmd:clegend}[{cmd:(}{it:options}{cmd:)}] prints a {help clegend_option:contour} plot legend
    of the colors used in one of the layers that include
    {help geoplot##zvar:{it:zvar}} with a {help geoplot##color:color gradient}. {it:options}
    are as follows.

{phang2}
    {opt l:ayer(#)} selects the layer for which the legend be created. The default
    is to use the first layer containing a color gradient. For example, if layers
    3 and 4 in a graph contain {help geoplot##zvar:{it:zvar}} with a
    {help geoplot##color:color gradient}, type {cmd:layer(4)} to create a legend
    for layer 4.

{phang2}
    {opth f:ormat(%fmt)} select the display format to be applied to the values
    in the legend labels. The default is to use the display format of
    {help geoplot##zvar:{it:zvar}}.

{phang2}
    {opt cuts(keyword)} determines how ticks and labels are used to indicated
    the cuts between the color levels. {it:keyword} may be {cmd:none} (do not
    indicate the cuts), {cmdab:lab:el} (major ticks plus labels), {cmdab:ti:ck}
    (major ticks only), {cmdab:mlab:el} (minor ticks plus labels), and {cmdab:mti:ck}
    (minor ticks only). Default is {cmd:cuts(label)}. You may use options
    {helpb axis_label_options:zlabel()}, {helpb axis_label_options:ztick()},
    {helpb axis_label_options:zmlabel()}, and {helpb axis_label_options:zmtick()}
    outside of {cmd:clegend()} to add further ticks and labels or to override the
    ticks and labels added by {cmd:cuts()}. {cmd:cuts()} has no effect if
    {help geoplot##zvar:{it:zvar}} is {helpb geoplot##discrete:discrete}

{phang2}
    {opt nolab:el} omits the use of value labels. This is only relevant
    if {help geoplot##zvar:{it:zvar}} is {helpb geoplot##discrete:discrete}.

{phang2}
    {opt mis:sing} requests that missing value is
    included in the legend. Use option {helpb geoplot##missing:missing()} to
    set the label for missing.

{phang2}
    {opt position(spec)} overrides the default location of the legend,
    which is in the lower right corner. {it:spec} may be {it:{help compassdirstyle}}
    or {it:{help clockposstyle}}.

{phang2}
    {opt out:side} places the legend outside of the plot region. The default is
    to place the legend inside the plot region.

{phang2}
    {it:clegend_suboption} are further options to affect the rendering of the
    legend as documented in {it:{help clegend_option}}. For example, use options
    {opth width(size)} and {opth height(size)} to set the size of the legend. Option
    {cmd:bplacement()} will be ignored.

{pmore}
    The rendering of the axis included in the contour legend is controlled by
    options specified outside of {cmd:clegend()}. In particular, use
    option {helpb axis_title_options:ztitle()} to set the axis title, options
    {helpb axis_label_options:zlabel()} and {helpb axis_label_options:ztick()}
    to affect the axis labels and ticks, and option
    {helpb axis_scale_options:zscale()} to control further aspects of the axis.

{marker sbar}{...}
{phang}
    {cmd:sbar}[{cmd:(}{it:options}{cmd:)}] adds a scale bar to the map. {it:options}
    are as follows.

{phang2}
    {opt s:cale(exp)} determines how coordinates will be translated into the units
    of the scale bar. Default is {cmd:scale(1/1000)} (that is, by default, if
    coordinates are in meters, the scale bar will be in kilometers).

{phang2}
    {opt l:ength(#)}, {it:#}>0, set the length of the scale bar in units
    depending on {cmd:scale()}. {cmd:geoplot} will abort with error
    if the resulting scale bar is too large (i.e., has
    coordinates outside of the plotregion). An appropriate
    length is determined automatically if {cmd:length()} is omitted.

{phang2}
    {opt n(#)} sets the number of segments in the scale bar. Default is
    {cmd:n(5)}.

{phang2}
    {opt even} causes even segments to be filled. The default is to fill
    odd segments.

{phang2}
    {opt h:eight(#)}, {it:#}>=0, sets the height of the bar in percent of
    the map's reference size; see {helpb geoplot##refdim:refdim()}. Default is
    {cmd:height(1)}. {cmd:geoplot}
    will abort with error if the resulting scale bar is too large (i.e., has
    coordinates outside of the plotregion).

{phang2}
    {opt lab:el(options)} affects the rendering of the numeric labels, where
    {it:options} are {it:{help textbox_options}} such as {opt c:olor()} or {opt si:ze()}
    (defaults are {cmd:color(black)} and {cmd:size(vsmall)}), {cmd:minmax} to label
    only the minimum and maximum, {opt a:bove} to place the labels above the
    scale bar (rather than below), and {opth f:ormat(fmt)} to set the display
    format; default is {cmd:format(%8.0g)}. Type {opt nolab:el} to omit the numeric
    labels.

{phang2}
    {opt u:nits(text)} specifies text to be appended to the rightmost label. Use
    this to denote units, e.g., {cmd:units(km)}.

{phang2}
    {cmdab:ti:tle(}{it:text}[{cmd:,} {it:options}]{cmd:)} adds a title above the
    scale bar. {it:options} are {it:{help textbox_options}} such as {opt c:olor()}
    or {opt si:ze()} (defaults are {cmd:color(black)} and {cmd:size(vsmall)}), and
    {opt b:elow} to place the title below the scale bar (rather than above).

{phang2}
    {opth pos:ition(compassdirstyle)} overrides the default location
    of the scale bar, which is in the lower left corner.

{phang2}
    {opt xm:argin(#)} and {opt ym:argin(#)} specify how much the
    scale bar will be moved away from the edge of the plotregion, in percent of the map's
    reference size; see {helpb geoplot##refdim:refdim()}. Defaults are {cmd:xmargin(1)}
    and {cmd:ymargin(3)}.

{phang2}
    {it:{help area_options}} are general options to affect the rendering of the
    scale bar, such as {cmdab:c:olor()} or {cmdab:lw:idth()}. Defaults are
    {cmd:color(black)}, {cmd:fintensity(100)}, and {cmd:lwidth(vthin)}.

{marker compass}{...}
{phang}
    {cmd:compass}[{cmd:(}{it:options}{cmd:)}] adds a compass to the map. {it:options}
    are as follows.

{phang2}
    {opt t:ype(type)} selects the type of compass. Argument {it:type} may be
    {cmd:1} (the default), {cmd:2}, or {cmd:3}.

{phang2}
    {opt ang:le(angle)} rotates the compass by {it:angle} degrees
    (counterclockwise).

{phang2}
    {opt si:ze(#)}, {it:#}>=0, sets the size (or half-size) of the compass
    in percent of the map's reference size; see {helpb geoplot##refdim:refdim()}. The default
    is {cmd:size(5)}. For compass type 1, {cmd:size()} sets the half-size; for compass types
    2 and 3, {cmd:size()} sets the size. {cmd:geoplot} will abort with error if the resulting
    compass is too large (i.e., has coordinates outside of the plotregion).

{phang2}
    {opt lab:el}[{cmd:(}{it:options}{cmd:)}] affects the rendering of the compass
    labels, where {it:options} are {it:{help textbox_options}} such as {opt c:olor()} or
    {opt si:ze()} (defaults are {cmd:color(black)} and {cmd:size(vsmall)}) as
    well as {opt g:ap(#)} to determine how far the labels will be moved out from
    the edge of the compass. Argument {it:#}>0 is in percent of the size
    (or half-size) of the compass; default is {cmd:gap(30)}. Labels are printed
    for compass types 1 ("N", "S", "E", and "W") and 2 ("N" only) by default; specify
    {cmd:label} to print label "N" in compass type 3. Specify {opt nolab:el}
    to omit the labels in compass types 1 and 2.

{phang2}
    {opt nocir:cle} omits the circle in compass types 1 and 3.

{phang2}
    {opt nomsp:ikes} omits the minor spikes in compass type 1.

{phang2}
    {opth pos:ition(compassdirstyle)} overrides the default location
    of the compass, which is in the lower right corner.

{phang2}
    {opt xm:argin(#)} and {opt ym:argin(#)} specify how much the
    compass will be moved away from the edge of the plotregion, in percent of the map's
    reference size; see {helpb geoplot##refdim:refdim()}. Defaults are {cmd:xmargin(2)}
    and {cmd:ymargin(2)}.

{phang2}
    {it:{help area_options}} are general options to affect the rendering of the
    compass, such as {cmdab:c:olor()} or {cmdab:lw:idth()}. Defaults are
    {cmd:color(black)}, {cmd:fintensity(100)}, and {cmd:lwidth(vthin)}.

{marker zoom}{...}
{phang}
    {opt zoom(spec)} magnifies the objects from selected layers. Option {cmd:zoom()}
    can be repeated to magnify multiple sets of objects. The syntax of
    {it:spec} is

            {it:layers}{cmd::} {it:scale} [{it:offset} [{it:angle}]] [{cmd:,} {it:options}]

{pmore}
    where {help numlist:{it:layers}} is a list of layer numbers and {it:scale} is a scaling
    factor. For example, type {cmd:zoom(2/4: 2.5)} to magnify the objects in layers 2 through 4
    by factor 2.5. Argument {it:offset} specifies the distance by which the objects be moved
    and {it:angle} specifies the direction of the move in degrees. The default {it:offset}
    is 0 (no move) and the default {it:angle} is 0 (east). For example, type
    {cmd:zoom(2/4: 2.5 100 90)} to move the objects by 100 percent (1 radius)
    in northward direction. {it:options} are as follows.

{phang2}
    {opt abs:olute} specifies that {it:offset} is in absolute units of the map. The default
    is to interpret {it:offset} as a percentage of the radius (half-diagonal) of the bounding box
    (or enclosing circle) of the objects (including padding).

{phang2}
    {cmd:box}[{cmd:(}{it:which}{cmd:)}] displays the origin bounding box and
    the destination bounding box of the objects. Argument {it:what} can be
    {cmdab:d:estination} (print destination  box only) or
    {cmdab:o:rigin} (print origin box only). Only one of {cmd:box} and
    {cmd:circle} is allowed.

{phang2}
    {cmdab:cir:cle}[{cmd:(}{it:which}{cmd:)}] displays the origin MEC (minimum
    enclosing circle) and the destination MEC of the objects. Argument
    {it:what} can be {cmdab:d:estination} (print destination circle only) or
    {cmdab:o:rigin} (print origin circle only). Only
    one of {cmd:circle} and {cmd:box} is allowed.

{pmore2}
    By default, the positioning of the magnified objects is
    based on the midpoint and radius of the (rectangular)
    bounding box. However, if {cmd:circle()} is specified, the positioning is based on the
    midpoint and radius of the MEC. To use the MEC as basis without displaying
    the circles, you can type {cmd:circle(none)}.

{phang2}
    {opt pad:ding(#)} adds padding to the bounding box or MEC so that it does
    not touch the objects. Argument {it:#}
    is in percent of the radius of the (unpadded) bounding box or MEC. For example,
    type {cmd:padding(5)} to add 5% padding. Default is {cmd:padding(0)}.

{phang2}
    {opt nocon:nect} suppresses the connecting lines between origin and destination
    bounding boxes or MECs that are typically printed if {cmd:box}
    or {cmd:circle} is specified.

{phang2}
    {cmdab:con:nect}[{cmd:(}{it:{help line_options}}{cmd:)}] enforces printing the
    connecting lines even if no bounding boxes or MECs are printed. Specify
    {it:{help line_options}} to affect the rendering of the connecting lines. For
    example, type {cmd:connect(lpattern(dash))} to use dashed lines.

{phang2}
    {it:{help area_options}} are options that affect the look of the bounding boxes
    or MECs. For example, type {cmd:lcolor(cranberry)} to use red lines. The specified
    options also affect the connecting lines unless the options are overridden by options
    specified in {cmd:connect()}.

{pmore2}
    Note that bounding boxes and MECs generated by {cmd:zoom()}
    are printed on top of the map (i.e. in front of the existing
    objects). Adding fill to a bounding box or MEC will cover the
    objects lying behind it. To include a filled box or MEC that does not
    cover the objects, you may use {helpb geoframe##bbox:geoframe bbox} to
    generate a frame containing the coordinates of the box or MEC and then
    add tit to the graph as regular layer using {it:layertype}
    {helpb geoplot##area:area}.

{marker frame}{...}
{phang}
    {cmd:frame(}{it:name}[{cmd:,} {cmd:replace} {cmdab:nocur:rent}]{cmd:)}
    stores the compiled data of the plot in a {helpb frame} called
    {it:name}. Applying the command in {cmd:r(graph)} to the frame
    will reproduce the map. Option {cmd:replace} allows overwriting
    an existing frame. Option {cmd:nocurrent} does not make the created frame
    the current frame.

{marker nograph}{...}
{phang}
    {opt nograph} suppresses the graph. The graph command will still be
    returned in {cmd:r(graph)}, but the command will not be executed. Use option
    {cmd:nograph} if you are only interested in obtaining the compiled data
    using option {helpb geoplot##frame:frame()} and do not want to waste
    computer time on rendering the graph.


{title:Examples}

{pstd}
    Load data

{p 8 12 2}
    {stata "local url http://fmwww.bc.edu/repec/bocode/i/"}
    {p_end}
{p 8 12 2}
    {stata geoframe create regions `url'Italy-RegionsData.dta, id(id) coord(xcoord ycoord) shpfile(Italy-RegionsCoordinates.dta)}
    {p_end}
{p 8 12 2}
    {stata geoframe create country `url'Italy-OutlineCoordinates.dta}
    {p_end}
{p 8 12 2}
    {stata geoframe create capitals `url'Italy-Capitals.dta, coord(xcoord ycoord)}
    {p_end}
{p 8 12 2}
    {stata geoframe create lakes `url'Italy-Lakes.dta, feature(water)}
    {p_end}
{p 8 12 2}
    {stata geoframe create rivers `url'Italy-Rivers.dta, feature(water)}
    {p_end}

{pstd}
    Basic map of Italian regions

{p 8 12 2}
    {stata geoplot (area regions) (line country, lwidth(medthick)), tight}
    {p_end}

{pstd}
    Basic map with lakes and rivers

{p 8 12 2}
    {stata geoplot (area regions) (area lakes) (line rivers), tight}
    {p_end}

{pstd}
    Regions colored by number of fortune tellers (per million population)

{p 8 12 2}
    {stata geoplot (area regions fortell) (area regions), tight}
    {p_end}

{pstd}
    Different formatting of legend labels

{p 8 12 2}
    {stata geoplot (area regions fortell, label("@lb-@ub (N=@n)")) (area regions), tight}
    {p_end}

{pstd}
    Similar graph with more colors and alternative type of legend (requires Stata 18)

{p 8 12 2}
    {stata geoplot (area regions fortell, levels(20) lcolor(gray)), tight clegend(position(ne)) zlabel(4(3)28)}
    {p_end}

{pstd}
    Map with provincial capitals

{p 8 12 2}
    {stata local layer1 (area regions)}
    {p_end}
{p 8 12 2}
    {stata local layer2 (point capitals i.size [w=pop98], color(Set1, opacity(50)) mlcolor(%0))}
    {p_end}
{p 8 12 2}
    {stata local layer3 (label capitals city if pop98>250000, color(black))}
    {p_end}
{p 8 12 2}
    {stata geoplot `layer1' `layer2' `layer3', tight legend(position(sw))}
    {p_end}

{pstd}
    Map with composite legend

{p 8 12 2}
    {stata local layer1 (area regions fortell)}
    {p_end}
{p 8 12 2}
    {stata local layer2 (point capitals i.size [w=pop98], color(Set1, reverse opacity(50)) mlcolor(white))}
    {p_end}
{p 8 12 2}
    {stata geoplot `layer1' `layer2', tight legend(layout(- "FORTELL" 1 | - "CITY SIZE" 2) position(sw))}
    {p_end}

{pstd}
    Map with pie charts

{p 8 12 2}
    {stata geoplot (area regions) (pie regions relig?, label(, reverse)), tight}
    {p_end}

{pstd}
    Map with bar charts

{p 8 12 2}
    {stata geoplot (area regions) (bar regions relig1, asis outline), tight}
    {p_end}


{title:Returned results}

{pstd} Scalars:

{p2colset 5 22 22 2}{...}
{p2col : {cmd:r(layers)}}number of layers in the graph
    {p_end}
{p2col : {cmd:r(hasz_}{it:#}{cmd:)}}1 if layer {it:#} has {help geoplot##zvar:{it:zvar}}; 0 else
    {p_end}
{p2col : {cmd:r(z_hascol_}{it:#}{cmd:)}}1 if a color gradient has been used for
    {help geoplot##zvar:{it:zvar}} in layer {it:#}; 0 else (if relevant)
    {p_end}
{p2col : {cmd:r(z_discrete_}{it:#}{cmd:)}}1 if {help geoplot##zvar:{it:zvar}} in layer {it:#}
    is discrete or categorical; 0 else (if relevant)
    {p_end}
{p2col : {cmd:r(z_hasmis_}{it:#}{cmd:)}}1 if levels of {help geoplot##zvar:{it:zvar}} in layer {it:#}
    include a missing category; 0 else (if relevant)
    {p_end}

{pstd} Macros:

{p2col : {cmd:r(graph)}}copy of the called graph command
    {p_end}
{p2col : {cmd:r(legend)}}copy of the legend option in {cmd:r(graph)} (so
    that the legend can be restored when modifying the graph using command
    {helpb addplot}; see {stata ssc describe addplot}).
    {p_end}
{p2col : {cmd:r(keys_}{it:#}{cmd:)}}legend keys of layer {it:#}
    {p_end}
{p2col : {cmd:r(labels_}{it:#}{cmd:)}}labels of legend keys of layer {it:#}
    {p_end}
{p2col : {cmd:r(z_colors_}{it:#}{cmd:)}}colors used for the levels of
    {help geoplot##zvar:{it:zvar}} in layer {it:#} (if relevant)
    {p_end}

{pstd} Matrices:

{p2col : {cmd:r(z_levels_}{it:#}{cmd:)}}cut points or levels of {help geoplot##zvar:{it:zvar}}
    in layer {it:#} (if relevant)
    {p_end}
{p2col : {cmd:r(z_nobs_#)}}number of units per level of {help geoplot##zvar:{it:zvar}}
    in layer {it:#} (if relevant)
    {p_end}

{pstd}
    If the levels of {help geoplot##zvar:{it:zvar}} in layer {it:#} include
    a missing category, the first element in
    {cmd:r(keys_}{it:#}{cmd:)},
    {cmd:r(labels_}{it:#}{cmd:)},
    {cmd:r(z_colors_}{it:#}{cmd:)}, or
    {cmd:r(z_nobs_}{it:#}{cmd:)} corresponds to the missing category.


{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2023). geoplot: Stata module to draw maps. Available from
    {browse "https://ideas.repec.org/c/boc/bocode/s459211.html"}.

{pstd}
    Various features of {cmd:geoplot} have been inspired by corresponding
    features in {helpb spmap} by Maurizio Pisati.

{pstd}
    I am grateful to Asjad Naqvi for extensive testing and many valuable
    suggestions.


{title:Also see}

{psee}
    Online:  help for
    {helpb geoframe}, {helpb graph}, {helpb frames}, {helpb spshape2dta}
