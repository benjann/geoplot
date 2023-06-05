{smcl}
{* 05jun2023}{...}
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
    {help geoplot##zopts:{it:z_options}}
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

{synopt :{helpb geoplot##pcspike:pcspike}}paired-coordinate spikes
    {p_end}
{synopt :{helpb geoplot##pccapsym:pccapsym}}paired-coordinate spikes capped with symbols
    {p_end}
{synopt :{helpb geoplot##pcarrow:pcarrow}}paired-coordinate arrows
    {p_end}
{synopt :{helpb geoplot##pcbarrow:pcbarrow}}paired-coordinate arrows with two heads
    {p_end}
{synopt :{helpb geoplot##pcpoint:pcpoint}}paired-coordinate markers
    {p_end}

{p2coldent:* {helpb geoplot##pointi:pointi}}{cmd:point} with immediate arguments
    {p_end}
{p2coldent:* {helpb geoplot##pci:pci}}{cmd:pcspike} with immediate arguments
    {p_end}
{p2coldent:* {helpb geoplot##pcarrowi:pcarrowi}}{cmd:pcarrow} with immediate arguments
    {p_end}
{synoptline}
{p 4 6 2}
    * {help geoplot##zopts:{it:z_options}} without asterisk not supported.


{synoptset 20 tabbed}{...}
{marker zopts}{synopthdr:z_options}
{synoptline}
{syntab :Main}
{synopt :{helpb geoplot##zvar:{ul:z}var({it:Z})}}variable to control the look of the plotted elements
    {p_end}
{synopt :{helpb geoplot##discrete:{ul:discr}ete}}treat {it:Z} as discrete instead of continuous
    {p_end}
{synopt :{helpb geoplot##levels:{ul:lev}els({it:spec})}}number of levels and method to determine cuts
    {p_end}
{synopt :{helpb geoplot##cuts:cuts({it:numlist})}}use levels defined by specified cuts
    {p_end}

{syntab :Styling}
{p2coldent:* {helpb geoplot##color:{ul:col}or{sf:[}({it:palette}){sf:]}}}color palette
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
{p2coldent:* {helpb geoplot##mlabcolor:{ul:mlabc}olor({it:list})}}marker label colors
    {p_end}

{syntab :Legend keys}
{p2coldent:* {helpb geoplot##label:{ul:lab}el({it:spec})}}set labels of legend keys and related
    settings
    {p_end}

{syntab :Missing}
{synopt :{helpb geoplot##missing:{ul:mis}sing({it:options})}}styling of elements
    for which {it:Z} is missing
    {p_end}
{synoptline}
{p 4 6 2}
    * These options are also effective if {cmd:zvar()} is not specified, albeit
    with different interpretation and syntax. See the descriptions of the options
    below.


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
{synopt :{helpb geoplot##refdim:{ul:ref}dim({it:spec})}}select reference dimension
    {p_end}
{synopt :{helpb geoplot##aspect:{ul:aspect}ratio({it:spec})}}adjust aspect
    ratio of map
    {p_end}
{synopt :{it:{help twoway_options}}}twoway options, other than {cmd:by()}

{syntab :Legends}
{synopt :{helpb geoplot##legend:{ul:leg}end{sf:[}({it:options}){sf:]}}}add standard legend
    {p_end}
{synopt :{helpb geoplot##clegend:{ul:cleg}end{sf:[}({it:options}){sf:]}}}add {helpb contour} plot legend
    {p_end}
{synopt :{helpb geoplot##sbar:sbar{sf:[}({it:options}){sf:]}}}add scale bar
    {p_end}
{synopt :{helpb geoplot##compass:{ul:comp}ass{sf:[}({it:options}){sf:]}}}add compass
    {p_end}

{syntab :Data}
{synopt :{helpb geoplot##frame:frame({it:spec})}}store ploted data in new frame
    {p_end}
{synoptline}


{title:Description}

{pstd}
    {cmd:geoplot} draws maps from shape files and other datasets prepared by
    {helpb geoframe}. The procedure is to first create one or several frames
    containing the source data using {helpb geoframe} and then apply
    {cmd:geoplot} to plot the data from these frames.

{pstd}
    Multiple layers of elements such as regions, borders, lakes, roads, labels, etc.,
    can be freely combined. The look of the elements can be varied
    depending on the values of variables.

{pstd}
    {cmd:geoplot} requires {helpb colorpalette}, {helpb colrspace}, and {helpb moremata}. To
    install these packages type

        {com}. {stata ssc install palettes}{txt}
        {com}. {stata ssc install colrspace}{txt}
        {com}. {stata ssc install moremata}{txt}


{title:Layer types}

{marker area}{...}
{dlgtab:shapes, potentially filled}

{p 8 15 2}
    {cmd:area} [{it:frame} [{it:Z}]] {ifin} {weight}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the coordinates of the shapes to be
    plotted (see {helpb geoframe}), {it:Z} is an optional variable to determine
    styling, and {it:weight}, specified as {cmd:[}{cmdab:w:eight}{cmd:=}{it:exp}{cmd:]}
    or {cmd:[}{cmdab:iw:eight}{cmd:=}{it:exp}{cmd:]}, rescales the coordinates
    of the shapes by the absolute (and normalized) values of
    {it:exp}. {it:options} are as follows.

{phang2}
    {it:{help geoplot##zopts:z_options}} are options determining the look
    of the shapes (e.g. color) depending on the values of {it:Z} as described
    {help geoplot##z_options:below}.

{phang2}
    {opt wmax(#)} specifies a custom upper bound for normalization of
    weights. This is only relevant if {it:{help weight}} has been specified. The
    default is to normalize by max(1,{it:wmax}), where {it:wmax}
    is the observed maximum of the (absolute) weights (within layer).

{marker size}{...}
{phang2}
    {cmd:size(}{it:{help exp}}[{cmd:,} {opt s:cale(#)} {opt d:max(#)}]{cmd:)}
    resizes the shapes such that their sizes are proportional
    to {it:exp} (typically, {it:exp} is a {varname}; the size of a shape is equal
    to the area covered by the shape). Default normalization is such that the
    shape with the highest density (within layer), defined as abs({it:exp}) divided by the
    area of the shape, will keep its original size. Negative values in {it:exp} will be treated
    as positive; shapes for which {it:exp} is missing will keep their original
    size.

{pmore2}
    Suboption {cmd:scale()} multiplies all sizes by {it:#} (after
    normalization). Suboption {cmd:dmax()} specifies a custom maximum density
    for normalization; use this option to make sizes comparable across layers.

{phang2}
    {cmdab:ec:olor(}{help colorpalette##colorlist:{it:colorspec}}{cmd:)}
    sets the fill color used for enclaves. {it:colorspec} is a (single) color
    specification as described in {helpb colorpalette##colorlist:colorpalette}. Default
    is {cmd:ecolor(white)}. The color of an enclave will only be visible if not
    covered by a corresponding exclave.

{phang2}
    {it:{help area_options}} are options to affect the look of areas as described in
    {helpb twoway area}. For example, use option {cmd:lcolor()} to set
    the outline color. Color options support {it:colorspec} as described in
    {helpb colorpalette##colorlist:colorpalette}.

{pstd}
    By default, the shapes do not have a fill color. Specify {it:Z} and
    {helpb geoplot##color:color()} to color the shapes depending on the values of
    {it:Z}. Specify {cmd:fcolor(}{help colorpalette##colorlist:{it:colorspec}}{cmd:)}
    to set a single fill color for all shapes.

{marker line}{...}
{dlgtab:shapes, line only}

{p 8 15 2}
    {cmd:line} [{it:frame} [{it:Z}]] {ifin} {weight}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the coordinates of the shapes to be
    plotted (see {helpb geoframe}), {it:Z} is an optional variable to determine
    styling, and {it:weight}, specified as {cmd:[}{cmdab:w:eight}{cmd:=}{it:exp}{cmd:]}
    or {cmd:[}{cmdab:iw:eight}{cmd:=}{it:exp}{cmd:]}, rescales the coordinates
    of the shapes by the absolute (and normalized) values of
    {it:exp}. {it:options} are as follows.

{phang2}
    {it:{help geoplot##zopts:z_options}} are options determining the look
    of the lines (e.g. color or linewidth) depending on values of {it:Z} as described
    {help geoplot##z_options:below}.

{phang2}
    {opt wmax(#)} specifies a custom upper bound for normalization of
    weights. This is only relevant if {it:{help weight}} has been specified. The
    default is to normalize by max(1,{it:wmax}), where {it:wmax}
    is the observed maximum of the (absolute) weights (within layer).

{phang2}
    {cmd:size(}{it:{help geoplot##size:spec}}{cmd:)} sets the shape sizes as
    described for layer type {helpb geoplot##area:area}.

{phang2}
    {it:{help line_options}} are options to affect the look of lines as described in
    {helpb twoway line}. For example, use option {cmd:lwidth()} to set
    the width of lines. Color options support {it:colorspec} as
    described in {helpb colorpalette##colorlist:colorpalette}.

{marker point}{...}
{dlgtab:single-coordinate markers}

{p 8 15 2}
    {cmd:point} [{it:frame} [{it:Y} {it:X}]] {ifin} {weight}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the coordinates of the points to be
    plotted (see {helpb geoframe}), {it:Y} and {it:X} are custom variables names
    for the coordinates, and {it:weight}, specified as
    {cmd:[}{cmdab:w:eight}{cmd:=}{it:exp}{cmd:]} or
    {cmd:[}{cmdab:iw:eight}{cmd:=}{it:exp}{cmd:]}, scales the size of the
    markers by the absolute (and normalized) values of {it:exp} (also see
    {help scatter##remarks14:Weighted markers} in {helpb twoway scatter}). {it:options}
    are as follows.

{phang2}
    {it:{help geoplot##zopts:z_options}} are options determining the look
    of the markers (e.g. color or size) depending on values of a variable as described
    {help geoplot##z_options:below}.

{phang2}
    {opt wmax(#)} specifies a custom upper bound for normalization of
    weights. This is only relevant if {it:{help weight}} has been specified. The
    default is to normalize by max(1,{it:wmax}), where {it:wmax}
    is the observed maximum of the (absolute) weights (within layer). Use option
    {cmd:wmax()} to make marker sizes comparable across layers.

{phang2}
    {it:{help marker_options}}, {it:{help marker_label_options}},
    {it:{help connect_options}}, and {it:jitter_options} are options to affect
    the look of markers as described in {helpb twoway scatter}. For example,
    use option {cmd:msymbol()} to set the marker symbol. Color options
    support {it:colorspec} as described in {helpb colorpalette##colorlist:colorpalette}.

{pstd}
    {cmdab:sc:atter} may be used as a synonym for {cmd:point}.

{marker labels}{...}
{dlgtab:single-coordinate labels}

{p 8 15 2}
    {cmd:label} [{it:frame} {it:labelvar} [{it:Y} {it:X}]] {ifin}
    [{cmd:,}
    {it:options} ]

{pstd}
    where {it:frame} is the frame containing the coordinates of the points to be
    plotted (see {helpb geoframe}), {it:labelvar} is a (numeric or string) variable
    providing the labels, and {it:Y} and {it:X} are custom variables names
    for the coordinates. {it:options} are as follows.

{phang2}
    {it:{help geoplot##zopts:z_options}} are options determining the look
    of the labels (e.g. color or size) depending on values of a variable as described
    {help geoplot##z_options:below}.

{phang2}
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

{pmore2}
    If {helpb geoplot##zvar:zvar()} is specified, options {cmd:size()},
    {cmd:color()}, and {cmd:angle()} are interpreted in the same way as described
    {help geoplot##z_options:below} for {cmd:mlabsize()}, {cmd:mlabcolor()},
    and {cmd:mlabangle()}.

{phang}
    Layer type {cmd:label} is implemented as a wrapper for layer type {cmd:point}.

{marker pcspike}{...}
{dlgtab:paired-coordinate spikes}

{p 8 15 2}
    {cmd:pcspike} [{it:frame} [{it:Y1} {it:X1} {it:Y2} {it:X2}]] {ifin}
    [{cmd:,} {it:{help geoplot##zopts:z_options}} {it:options} ]

{pstd}
    where {it:frame} is the frame containing the coordinates of the spikes to be
    plotted (see {helpb geoframe}), {it:Y1}, {it:X1}, {it:Y2}, and {it:X2} are custom
    variables names for the coordinates, {it:z_options} are as described
    {help geoplot##z_options:below} and {it:options} are further options to affect
    the rendering of the spike lines as described in {helpb twoway pcspike}. Color
    options support {it:colorspec} as described in
    {helpb colorpalette##colorlist:colorpalette}.

{marker pccapsym}{...}
{dlgtab:paired-coordinate spikes capped with symbols}

{p 8 15 2}
    {cmd:pccapsym} [{it:frame} [{it:Y1} {it:X1} {it:Y2} {it:X2}]] {ifin}
    [{cmd:,} {it:{help geoplot##zopts:z_options}} {it:options} ]

{pstd}
    where {it:frame} is the frame containing the coordinates of the spikes to be
    plotted (see {helpb geoframe}), {it:Y1}, {it:X1}, {it:Y2}, and {it:X2} are custom
    variables names for the coordinates, {it:z_options} are as described
    {help geoplot##z_options:below} and {it:options} are further options
    to affect the rendering of the spike lines and markers
    as described in {helpb twoway pccapsym}. Color options support
    {it:colorspec} as described in {helpb colorpalette##colorlist:colorpalette}.

{marker pcarrow}{...}
{dlgtab:paired-coordinate arrows}

{p 8 15 2}
    {cmd:pcarrow} [{it:frame} [{it:Y1} {it:X1} {it:Y2} {it:X2}]] {ifin}
    [{cmd:,} {it:{help geoplot##zopts:z_options}} {it:options} ]

{pstd}
    where {it:frame} is the frame containing the coordinates of the arrows to be
    plotted (see {helpb geoframe}), {it:Y1}, {it:X1}, {it:Y2}, and {it:X2} are custom
    variables names for the coordinates, {it:z_options} are as described
    {help geoplot##z_options:below} and {it:options} are further options
    to affect the renderig of the arrows  as described in
    {helpb twoway pcarrow}. Color options support {it:colorspec} as described
    in {helpb colorpalette##colorlist:colorpalette}.

{marker pcbarrow}{...}
{dlgtab:paired-coordinate arrows with two heads}

{p 8 15 2}
    {cmd:pbcarrow} [{it:frame} [{it:Y1} {it:X1} {it:Y2} {it:X2}]] {ifin}
    [{cmd:,} {it:{help geoplot##zopts:z_options}} {it:options} ]

{pstd}
    where {it:frame} is the frame containing the coordinates of the arrows to be
    plotted (see {helpb geoframe}), {it:Y1}, {it:X1}, {it:Y2}, and {it:X2} are custom
    variables names for the coordinates, {it:z_options} are as described
    {help geoplot##z_options:below} and {it:options} are further options
    to affect the rendering of the arrows as described in
    {helpb twoway pcarrow}. Color options support {it:colorspec} as described
    in {helpb colorpalette##colorlist:colorpalette}.

{marker pcpoint}{...}
{dlgtab:plot paired-coordinate markers}

{p 8 15 2}
    {cmd:pcpoint} [{it:frame} [{it:Y1} {it:X1} {it:Y2} {it:X2}]] {ifin}
    [{cmd:,} {it:{help geoplot##zopts:z_options}} {it:options} ]

{pstd}
    where {it:frame} is the frame containing the coordinates of the paired markers to be
    plotted (see {helpb geoframe}), {it:Y1}, {it:X1}, {it:Y2}, and {it:X2} are custom
    variables names for the coordinates, {it:z_options} are as described
    {help geoplot##z_options:below} and {it:options} are further options to
    affect the rendering of the markers as described in
    {helpb twoway pcscatter}. Color options support {it:colorspec} as described in
    {helpb colorpalette##colorlist:colorpalette}.

{pstd}
    {cmd:pcscatter} may be used as a synonym for {cmd:pcpoint}.

{marker pointi}{...}
{dlgtab:point with immediate arguments}

{p 8 15 2}
    {cmd:pointi} {it:immediate_values} [{cmd:,} {opt lab:el(label)} {it:options} ]

{pstd}
    where {it:immediate_values} and {it:options} as described in
    {helpb twoway scatteri} and {cmd:label()} sets a label for the
    legend. Color options support {it:colorspec} as described in
    {helpb colorpalette##colorlist:colorpalette}. {cmd:scatteri} may be used as a
    synonym for {cmd:pointi}.

{marker pci}{...}
{dlgtab:pcspike with immediate arguments}

{p 8 15 2}
    {cmd:pci} {it:immediate_values} [{cmd:,} {opt lab:el(label)} {it:options} ]

{pstd}
    where {it:immediate_values} and {it:options} as described in
    {helpb twoway pci} and {cmd:label()} sets a label for the
    legend. Color options support {it:colorspec} as
    described in {helpb colorpalette##colorlist:colorpalette}.

{marker pcarrowi}{...}
{dlgtab:pcarrow with immediate arguments}

{p 8 15 2}
    {cmd:pcarrowi} {it:immediate_values} [{cmd:,} {opt lab:el(label)} {it:options} ]

{pstd}
    where {it:immediate_values} and {it:options} as described in
    {helpb twoway pcarrowi} and {cmd:label()} sets a label for the
    legend. Color options support {it:colorspec} as described
    in {helpb colorpalette##colorlist:colorpalette}.


{title:Options}

{marker z_options}{...}
{dlgtab:z options}

{marker zvar}{...}
{phang}
    {opt zvar(Z)} specifies that the colors or other aspects of the areas,
    lines, markers, or labels in a layer be determined by the values of
    (numeric) variable {help varname:{it:Z}}. The
    range of values of {it:Z} will be divided into levels and each
    level will be represented by a different style. {opt colorvar()} is treated
    as a synonym for {opt zvar()}.

{pmore}
    Some {help geoplot##layertype:{it:layertypes}} allow {it:Z} as an
    argument. In these cases option {cmd:zvar()} is not needed; if specified nonetheless,
    it takes precedence over {it:Z} specified as an argument.

{marker discrete}{...}
{phang}
    {opt discr:ete} treats {help geoplot##zvar:{it:Z}} as a discrete variable. Each unique value
    of {help geoplot##zvar:{it:Z}} will form a separate level.

{marker levels}{...}
{phang}
    {cmd:levels(}[{it:#}][{cmd:,} {it:method} {opth w:eight(varname)}]{cmd:)} specifies the number of
    levels to be formed by {help geoplot##zvar:{it:Z}}. By default, a regular grid
    (equidistant cuts) of 5 intervals from the observed minimum to the observed
    maximum of {help geoplot##zvar:{it:Z}} will be used (first interval closed, remaining intervals
    left-open). Specify {opt levels(#)} to create {it:#} intervals. Specify {it:method}
    to use a non-regular grid, where {it:method} can be one of the following.

{p2colset 13 23 25 2}{...}
{p2col: {cmdab:q:uantile}}use quantiles as cuts
    {p_end}
{p2col: {cmdab:k:means}}use cuts determined by {helpb cluster kmeans}
    {p_end}

{pmore}
    The number of resulting levels may be less than {it:#} with these methods, depending on the
    distribution of {help geoplot##zvar:{it:Z}}. In case of {cmdab:quantile}, weights to be taken
    into account when computing the quantiles can be specified in suboption {cmd:weight()}. Option
    {cmd:levels()} has no effect if {helpb geoplot##discrete:discrete} is specified.

{marker cuts}{...}
{phang}
    {opth cuts(numlist)} is an alternative to {cmd:levels()} and
    provides custom breaks for the levels to be formed by
    {help geoplot##zvar:{it:Z}}. For continuous {help geoplot##zvar:{it:Z}},
    specify {it:#}+1 breaks to form {it:#} intervals (first interval closed, remaining
    intervals left-open). Observations with values below the first break or above
    the last break will be ignored. If {helpb geoplot##discrete:discrete}
    is specified, {it:numlist} is interpreted as the distinct values of
    {help geoplot##zvar:{it:Z}} to be selected.

{marker color}{...}
{phang}
    {cmd:color}[{cmd:(}[{it:{help colorpalette##palette:palette}}] [{cmd:,}
    {it:{help colorpalette##opts:palette_options}}]{cmd:)}]
    selects the colors to be used for the levels. {it:palette} is any
    palette allowed by {helpb colorpalette} (which can also be a simple list of colors, see
    help {it:{help colorpalette##colorlist:colorlist}}) and {it:palette_options} are
    corresponding options. For example, type {cmd:colors(hcl bluered, reverse)} for a
    red to blue HCL color scheme; typing {cmd:color} without argument is equivalent to
    {cmd:color(viridis)}. An appropriate number of colors will automatically
    be retrieved from {it:palette} (applying interpolation or recycling, if necessary,
    depending on type of palette). Specify a single color if you do not want
    the color to depend on the values of {help geoplot##zvar:{it:Z}}.

{pmore}
    If {help geoplot##zvar:{it:Z}} is omitted, {cmd:color()}
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
    If {help geoplot##zvar:{it:Z}} is omitted, the options are
    interpreted as a standard graph options with their regular syntax.

{marker mlabcolor}{...}
{phang}
    {cmd:mlabcolor(}[{it:{help colorpalette##palette:palette}}] [{cmd:,}
    {it:{help colorpalette##opts:palette_options}}]{cmd:)} is like
    {helpb geoplot##color:color()}, but affects marker labels only. If
    {cmd:mlabcolor()} and {cmd:color()} are both specified, {cmd:color()}
    takes precedence over {cmd:mlabcolor()}, which affects markers as well as
    marker labels. An exception is if {cmd:mlabcolor()} is specified with a
    single color; in this case {cmd:color()} will not override
    {cmd:mlabcolor()}. Likewise, {cmd:color()} specified with a single
    color will not override {cmd:mlabcolor()} (and will also not set the
    color for marker labels).

{marker label}{...}
{phang}
    {cmd:label(}[{it:labelinfo}] [{cmd:,} {it:options}]{cmd:)} determines how
    the levels formed by {help geoplot##zvar:{it:Z}} will be labeled in the
    legend. {cmd:label()} only sets labels for {helpb geoplot##legend:legend()}, but not
    for {helpb geoplot##clegend:clegend()}. {it:labelinfo} is

            [ {it:id} [{cmd:=}] ] {it:label} [ {it:id} [{cmd:=}] {it:label} {it:...} ]

{pmore}
    where {it:label} is

            {cmd:"}{it:text}{cmd:"} [{cmd:"}{it:text}{cmd:"} {it:...}]

{pmore}
    (a multiline label will be created if {it:label} contains multiple
    {cmd:"}{it:text}{cmd:"} elements) and {it:id} may be {it:#} to select a single level
    ({cmd:1} for the 1st level, {cmd:2} for 2nd level, etc.) or a specification including
    wildcard characters {cmd:*} and {cmd:?} to select multiple levels. For example, type
    {cmd:?} to select levels 1-9, type {cmd:1?} to select
    levels 10-19, type {cmd:*} to select all levels. In any
    case, the label of the first matching specification will be used.

{pmore}
    For continuous {help geoplot##zvar:{it:Z}}, that is, if
    {helpb geoplot##discrete:discrete} has not been specified, {it:text} may include
    {cmd:@lb}, {cmd:@mid}, and {cmd:@ub}, which will be replaced by the lower bound,
    the middle, and the upper bound of the level's interval. For example, the default
    setting is equivalent to typing
    {cmd:label(1 = "[@lb,@ub]" * = "(@lb,@ub]")}. Furthermore, to use the same
    (type of) label for all levels, you may type {cmd:label(}{it:label}{cmd:)}
    rather than {cmd:label(}{cmd:*} = {it:label}{cmd:)}. For example,
    type {cmd:label("@lb-@ub")} to create labels formatted as "{it:lb}-{it:ub}",
    where {it:lb} ({it:ub}) is the value of the lower (upper) bound of each
    interval.

{pmore}
    If {helpb geoplot##discrete:discrete} had been specified, the default is to
    use the value label (or the value) of a level as label in the legend. Use
    {cmd:label()} to assign custom labels to selected levels in this case.

{pmore}
    {it:options} are as follows.

{phang2}
    {opt nol:abel} omits the use of value labels in case of
    {helpb geoplot##discrete:discrete} {help geoplot##zvar:{it:Z}}.

{phang2}
    {opth f:ormat(%fmt)} selects the display format to be applied to {cmd:@lb},
    {cmd:@mid}, and {cmd:@ub} in case of continuous
    {help geoplot##zvar:{it:Z}}. The default is to use the display format of
    {help geoplot##zvar:{it:Z}}.

{phang2}
    {opt r:everse} reverses the order of the legend keys.

{phang2}
    {opt nom:issing} omits the legend key for missing values. See option
    {helpb geoplot##missing:missing()} below on how to customize the label for missing.

{phang2}
    {opt mf:irst} places the missing key at the top (or leftmost) rather than at
    the bottom (or rightmost).

{phang2}
    {opt nog:ap} omits the gap between the missing key and the other keys.

{pmore}
    If {help geoplot##zvar:{it:Z}} is omitted in a layer (such that, technically,
    the layer only contains a single level), type {cmd:label(}{it:label}{cmd:)} to
    set the label of the layer's legend key (the default is to use the name of
    the plotted frame as label). The above {it:options} are ineffective in this case.

{marker missing}{...}
{phang}
    {opt missing(options)} specifies the styling of elements for which {help geoplot##zvar:{it:Z}}
    is missing, where {it:options} are as follows.

{phang2}
    {cmdab:lab:el("}{it:text}{cmd:"} [{cmd:"}{it:text}{cmd:"} {it:...}]{cmd:)}
    sets a label for missing in the legend. The default is
    {cmd:"no data"}. Multiple lines are created if multiple
    {cmd:"}{it:text}{cmd:"} elements are specified.

{phang2}
    {cmd:color()}, {cmd:lwidth()}, {cmd:lpattern()}, etc. are standard graph
    options depending on layer type. In case of {helpb geoplot##area:area},
    the default is to use color {cmd:gs14} for areas for which {help geoplot##zvar:{it:Z}} is missing.

{dlgtab:Global options}

{marker angle}{...}
{phang}
    {opt angle(angle)} rotates the map by {it:angle} degrees (counter clock-wise).

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
    {opt hor:izontal} arranges the legend horizontally (i.e., in rows). The default is to
    arrange the legend vertically (in columns).

{phang2}
    {opt position(spec)} overrides the default location of the legend,
    which is in the upper right corner. {it:spec} may be {it:{help compassdirstyle}}
    or {it:{help clockposstyle}}.

{phang2}
    {opt out:side} places the legend outside of the plot region. The default is
    to place the legend inside the plot region.

{phang2}
    {it:contents} and {it:location} options are further options to affect the
    rendering of the legend as documented in {it:{help legend_option}}. Options
    {cmd:order()}, {cmd:holes()}, {cmd:cols()}, {cmd:rows()},
    [{cmd:no}]{cmd:colfirst}, and {cmd:bplacement()} will be ignored.

{pmore}
    Type {cmd:legend(off)} to suppress the legend that is printed by default if
    {help geoplot##zvar:{it:Z}} has been specified in at least one layer.

{marker clegend}{...}
{phang}
    {cmd:clegend}[{cmd:(}{it:options}{cmd:)}] prints a {help clegend_option:contour} plot legend
    of the colors used in one of the layers that include
    {help geoplot##zvar:{it:Z}} with a {help geoplot##color:color gradient}. {it:options}
    are as follows.

{phang2}
    {opt l:ayer(#)} selects the layer for which the legend be created. The default
    is to use the first layer containing a color gradient. For example, if layers
    3 and 4 in a graph contain {help geoplot##zvar:{it:Z}} with a
    {help geoplot##color:color gradient}, type {cmd:layer(4)} to create a legend
    for layer 4.

{phang2}
    {opth f:ormat(%fmt)} select the display format to be applied to the values
    in the legend labels. The default is to use the display format of
    {help geoplot##zvar:{it:Z}}.

{phang2}
    {opt nolab:el} omits the use of value labels if {helpb geoplot##discrete:discrete}
    has been specified.

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
    (counter clock-wise).

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

{marker frame}{...}
{phang}
    {cmd:frame(}{it:name}[{cmd:, replace}]{cmd:)} stores the compiled data of
    the plot in a {helpb frame} called {it:name}. Option {cmd:replace} allows overwriting
    an existing frame. Applying the command in {cmd:r(graph)} to the frame
    will reproduce the map.


{title:Examples}

{pstd}
    Load data

{p 8 12 2}
    {stata "local url http://fmwww.bc.edu/repec/bocode/i/"}
    {p_end}
{p 8 12 2}
    {stata geoframe create regions `url'Italy-RegionsData.dta, id(id) coord(xcoord ycoord) shpfile(`url'Italy-RegionsCoordinates.dta)}
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
    {stata geoplot (area regions fortell, color) (area regions), tight}
    {p_end}

{pstd}
    Different formatting of legend labels

{p 8 12 2}
    {stata geoplot (area regions fortell, color label("@lb-@ub")) (area regions), tight}
    {p_end}

{pstd}
    Similar graph with more colors and alternative type of legend (requires Stata 18)

{p 8 12 2}
    {stata geoplot (area regions fortell, color levels(20) lcolor(gray)), tight clegend(position(ne) height(30)) zlabel(4(3)28)}
    {p_end}

{pstd}
    Map with provincial capitals

{p 8 12 2}
    {stata local layer1 (area regions)}
    {p_end}
{p 8 12 2}
    {stata local layer2 (point capitals [w=pop98], z(size) discrete color(Set1, opacity(50)) mlcolor(%0))}
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
    {stata local layer1 (area regions fortell, color)}
    {p_end}
{p 8 12 2}
    {stata local layer2 (point capitals [w=pop98], z(size) discrete color(Set1, reverse opacity(50)) mlcolor(white))}
    {p_end}
{p 8 12 2}
    {stata geoplot `layer1' `layer2', tight legend(layout(- "FORTELL" 1 | - "CITY SIZE" 2) position(sw))}
    {p_end}


{title:Returned results}

{pstd}
    {cmd:geoplot} returns a copy of the called graph command in macro
    {cmd:r(graph)}.


{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2023). geoplot: Stata module to draw maps. Available from
    {browse "https://github.com/benjann/geoplot/"}.

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
