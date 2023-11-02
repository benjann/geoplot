{smcl}
{* 02nov2023}{...}
{hi:help geoframe}{...}
{right:{browse "https://github.com/benjann/geoplot/"}}
{hline}

{title:Title}

{pstd}{hi:geoframe} {hline 2} Command to prepare data for {helpb geoplot}


{title:Syntax}

{p 8 15 2}
     [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {it:{help geoframe##subcmd:subcommand}} [{it:...}]


{synoptset 15 tabbed}{...}
{marker subcmd}{synopthdr:subcommand}
{synoptline}
{syntab :Main}
{p2col :{helpb geoframe##translate:{ul:tr}anslate}}translate shapefile source to Stata format (without loading)
    {p_end}
{p2col :{helpb geoframe##translate:convert}}synonym for {cmd:geoframe translate}
    {p_end}
{synopt :{helpb geoframe##create:{ul:cr}eate}}load data into geoframe or declare
    current frame as geoframe
    {p_end}
{p2col :{helpb geoframe##link:{ul:l}ink}}link shape frame to current frame
    {p_end}
{p2col :{helpb geoframe##clean:{ul:cl}ean}}delete unmatched/empty shapes and units
    {p_end}
{p2col :{helpb geoframe##query:{ul:q}uery}}obtain information on shapes in geoframe
    {p_end}
{synopt :{helpb geoframe##describe:{ul:d}escribe}}describe geoframe
    {p_end}

{syntab :Manipulation}
{p2col :{helpb geoframe##project:project}}apply projection
    {p_end}
{p2col :{helpb geoframe##select:{ul:sel}ect}}select units and shapes
    {p_end}
{p2col :{helpb geoframe##clip:clip}}clip shapes using convex window
    {p_end}
{p2col :{helpb geoframe##rclip:rclip}}clip shapes using rectangular window
    {p_end}
{p2col :{helpb geoframe##simplify:simplify}}simplify (generalize) shapes
    {p_end}
{p2col :{helpb geoframe##refine:refine}}add extra points to straight lines
    {p_end}
{p2col :{helpb geoframe##bshare:bshare}}select shared borders
    {p_end}
{p2col :{helpb geoframe##generate:{ul:g}enerate}}generate special-purpose variables
    {p_end}
{p2col :{helpb geoframe##copy:copy}}copy variables between frames
    {p_end}
{p2col :{helpb geoframe##append:{ul:ap}pend}}append observations between frames
    {p_end}

{syntab :Spatial join}
{p2col :{helpb geoframe##collapse:collapse}}collapse points from other frame into current frame
    {p_end}
{p2col :{helpb geoframe##contract:contract}}contract points from other frame into current frame
    {p_end}
{p2col :{helpb geoframe##spjoin:spjoin}}match points in current frame to shapes from other frame
    {p_end}

{syntab :Generate shapes}
{p2col :{helpb geoframe##grid:grid}}store grid lines in new frame
    {p_end}
{p2col :{helpb geoframe##bbox:{ul:bb}ox}}store bounding box, enclosing circle, or convex hull in new frame
    {p_end}
{p2col :{helpb geoframe##symbol:{ul:sym}bol}}generate symbol shapes and store in new frame
    {p_end}
{p2col :{helpb geoframe##symboli:symboli}}{cmd:geoframe symbol} with immediate arguments
    {p_end}

{syntab :Settings}
{synopt :{helpb geoframe##set:set}}update geoframe settings of current frame
    {p_end}
{synopt :{helpb geoframe##get:get}}retrieve geoframe settings from current frame
    {p_end}

{syntab :Utilities}
{p2col :{helpb geoframe##rename:{ul:ren}ame}}rename a geoframe
    {p_end}
{p2col :{helpb geoframe##duplicate:{ul:dup}licate}}duplicate a geoframe
    {p_end}
{p2col :{helpb geoframe##relink:{ul:rel}ink}}fix linkage variable after modifying data
    {p_end}
{p2col :{helpb geoframe##unlink:{ul:unl}ink}}unlink shape frame from current frame
    {p_end}
{p2col :{helpb geoframe##attach:{ul:at}tach}}attach attribute frame to current frame using aliases (Stata 18 required)
    {p_end}
{p2col :{helpb geoframe##detach:{ul:det}ach}}detach attribute frame from current frame (Stata 18 required)
    {p_end}
{synoptline}


{title:Description}

{pstd}
    {cmd:geoframe} prepares data for {helpb geoplot}.


{title:Subcommands}

{marker translate}{...}
{dlgtab:geoframe translate}

{p 8 15 2}
    {cmd:geoframe} {cmdab:tr:anslate} [{it:translator}] [{it:destination}] [{cmd:using}]
    {it:source} [{cmd:,} {cmd:replace} {cmd:user} ]

{pstd}
    translates shapefile source data to Stata format, where {it:translator} specifies
    the type of translation. Currently, the only available translator is {cmd:esri}, which
    translates ESRI shapefile data to Stata format using official Stata's
    {helpb spshape2dta} command or, if option {cmd:user} is specified, the
    user command {helpb shp2dta} (see {bf:{stata ssc describe shp2dta}}). Omitting
    {it:translator} is equivalent to specifying {cmd:esri}.

{pstd}
    Optional argument {it:destination} specifies a destination for the translated
    data. The syntax for {it:destination} is

        [{it:path}][{it:basename}]

{pstd}
    where {it:path} is an absolute or relative path to an (existing) directory
    (e.g. {cmd:mydata/shapfiles/} on Mac OS or {cmd:mydata\shapfiles\}
    on Windows) and {it:basename} provides a custom base name for the created files. If
    {it:path} is omitted, the files are stored in the working directory. If
    {it:basename} is omitted, the name of the source will be used. Two files
    will be created, {it:basename}{cmd:.dta} and
    {it:basename}{cmd:_shp.dta}. Option {cmd:replace} allows overwriting
    existing files.

{pstd}
    Argument {it:source} identifies the source data to be translated. The syntax
    for {it:source} is

        {it:sourcepath}[{it:sourcename}]

{pstd}
    where {it:sourcepath} is an absolute or relative path to the directory containing
    the source (e.g. {cmd:mydata/shapfiles/source/world/} on Mac OS or
    {cmd:mydata\shapfiles\source\world\} on Windows) and {it:sourcename} specifies the
    base name of the source to be translated (in case of an ESRI shape file,
    the source consists of {it:sourcename}{cmd:.shp}, {it:sourcename}{cmd:.dbf},
    and possibly some additional files with the same base name). If {it:sourcepath}
    contains multiple sources and {it:sourcename} is omitted, the first source
    will be translated. Alternatively, {it:source} can be

        {it:zipfile}[{it:separator}[{it:location}][{it:sourcename}]]

{pstd}
    where {it:zipfile} is the (path and) name of a zip file containing the source
    (e.g. {cmd:mydata/shapfiles/source/world.zip} on Mac OS or {cmd:mydata\shapfiles\source\world.zip}
    on Windows), {it:separator} is the operating system's directory separator (i.e. {cmd:/} on Mac OS, {cmd:\} on Windows),
    {it:location} specifies the directory of the source within the zip file, and
    {it:sourcename} specifies the name of the source to be translated. For example, you could
    type {cmd:world.zip/50m/} to translate the first source found in folder {cmd:50m} within
    zip file {cmd:world.zip}, or you could type {cmd:world.zip/50m/country_borders} to translate
    the source called {cmd:country_borders} in folder {cmd:50m} within zip file {cmd:world.zip}.

{pstd}
    {cmd:geoframe convert} is a synonym for {cmd:geoframe translate}.

{marker create}{...}
{dlgtab:geoframe create}

{p 8 15 2}
    {cmd:geoframe} {cmdab:cr:eate} [{it:frame}] [{cmd:using}] {it:{help filename}}
    [{cmd:,} {it:options} ]

{pstd}
    loads the data from {it:filename} into a new frame called
    {it:frame}, where {it:filename} is a valid Stata dataset, and declares the
    created frame as a geoframe. If {it:frame} is omitted, the base name of {it:filename}
    is used as the name of the created frame. Typically, {it:filename} is
    an attribute file created by {helpb geoframe##translate:geoframe translate}
    (or {helpb spshape2dta}). If a shape file belonging
    to {it:filename} is available in the same folder (that is, if there is
    a file called {it:basename}{cmd:_shp.dta} in the folder, where {it:basename} is the base
    name of {it:filename}), the shape file will automatically
    be loaded into a second frame called {it:frame}{cmd:_shp} and a link between
    the two frames will be established. Alternatively,
    {it:filename} may also be a shape file or any other valid Stata dataset.

{pstd}
    You may also type

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:cr:eate}
    [{cmd:,} {it:options} ]

{pstd}
    to declare the current frame as a geoframe.

{pstd}
    Options are as follows.

{phang}
    {opt replace} allows replacing existing frames.

{phang}
    {opt nodes:cribe} suppresses the description of the frame that is displayed
    by default.

{phang}
    {opt nocur:rent} does not make the created frame the current frame. The default
    is to make the created frame the current frame.

{phang}
    {opt t:ype(type)} declares the type of data included in the
    frame. {it:type} may be {opt unit} (attribute data), {opt pc}
    (paired-coordinate data), or {opt shape} (shape data). If {cmd:type()} is omitted,
    {cmd:geoframe create} infers the type from context.

{phang}
    {opt f:eature(string)} declares the type of feature represented by the
    units in the frame. For example, type {cmd:feature(water)} if the frame
    contains data on lakes or rivers. {it:string} can be any text.

{phang}
    {opt id(varname)} specifies the name of the unit ID. Default is {cmd:_ID}.

{phang}
    {opt co:ordinates(X Y [X2 Y2])} specifies the names of the variables containing
    the coordinates. The default depends on data type. It is {cmd:_X _Y} for data type {cmd:shape},
    {cmd:_CX _CY} for data type {cmd:unit}, and {cmd:_X1 _Y1 _X2 _Y2} for data type {cmd:pc}.

{phang}
    {opt cen:troids(CX CY)} specifies the names of the variables containing
    the centroids of the units. The default is {cmd:_CX _CY}
    for data types {cmd:shape} and {cmd:pc}; for data type {cmd:unit}, {cmd:centroids()}
    is a synonym for {cmd:coordinates()}.

{phang}
    {opt area(AREA)} specifies the name of the variable containing the sizes
    of the units (i.e. the areas of the shapes). The default is {cmd:_AREA}.

{phang}
    {opt sid(varname)} specifies the name of the within-unit sort ID. Default is
    {cmd:shape_order}. This is only relevant for data of type {cmd:shape}

{phang}
    {opt pid(varname)} specifies the name of the within-unit polygon ID. Default is
    {cmd:_PID}. This is only relevant for data of type {cmd:shape}

{phang}
    {opt pl:evel(varname)} specifies the name of the plot level ID (enclaves and exclaves). Default is
    {cmd:_PLEVEL}. This is only relevant for data of type {cmd:shape}

{phang}
    {opt nos:hpfile} deactivates automatic loading of the shape file. By default,
    as described above, {cmd:geoframe create} loads the shape file belonging to
    {it:filename} if such a file is found on disk. Specify
    {cmd:noshpfile} to suppress this behavior. Data types {cmd:shape} and {cmd:pc}
    imply {cmd:noshpfile}.

{phang}
    {opt s:hpfile(spec)} specifies a custom shape file to be loaded along with the main
    file. The syntax of {it:spec} is

{p 12 15 2}
    [[{it:shpframe}] [{cmd:using}] {it:shpfilename} ] [{cmd:,} {it:suboptions} ]

{pmore}
    where {it:shpfilename} is the name of the shape file on disk and {it:shpframe}
    provides a name for the shape frame. If {it:shpfilename} is specified without
    path, {cmd:geoframe} looks for the file in the same folder as the main file. If
    {it:shpframe} is omitted, {it:frame}{cmd:_shp}
    is used as the name for the shape frame. {it:suboptions} are {opt feat:ure()},
    {cmd:id()}, {opt co:ordinates()}, {cmd:sid()}, {cmd:pid()}, and {opt pl:evel()}
    as described above.

{phang}
    {opt nodrop} prevents dropping unmatched units and empty shapes
    when linking the attribute file and the shape file (i.e., do not apply
    {helpb geoframe##clean:geoframe clean}).

{marker link}{...}
{dlgtab:geoframe link}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:l:ink} {it:shpframe}
    [{cmd:,} {cmdab:cl:ean}[{cmd:(}{it:options}{cmd:)}] ]

{pstd}
    establishes a link between the current frame and {it:shpframe}. {cmd:geoframe link}
    is an alternative to linking frames automatically when loading them
    using {cmd:geoframe create}.

{pstd}
    The current frame must be an attribute frame (one row per unit) and
    {it:shpframe} is typically a frame containing shape information on the units
    represented in the current frame. For each frame only one link to a
    shape frame can be registered; calling {cmd:geoframe link} will
    remove any existing link to another shape frame. (But note that a single
    shape frame may contain links from multiple attribute frames.)

{pstd}
    Option {cmd:clean()} calls {helpb geoframe##clean:geoframe clean} after
    linking the frames. See below for a description of {it:options}.

{marker clean}{...}
{dlgtab:geoframe clean}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:clean}
        [{cmd:,} {it:options} ]

{pstd}
    removes unmatched shapes and units if applied to an attribute frame that
    has a link to a shape frame. {it:options} are as
    follows.

{phang}
    {opt s:hapes} deletes unmatched and empty shapes in the linked shape frame.

{phang}
    {opt u:nits} deletes unmatched units and units linked to empty
    shapes in the current frame.

{pmore}
    If neither {cmd:shapes} nor {cmd:units} is specified, the default is to deletes both,
    unmatched or empty shapes in the linked shape frame as well as unmatched units or
    units linked to empty shapes in the current frame.

{phang}
    {opt noe:mpty} does not delete empty shapes or units linked to empty shapes.

{phang}
    {opt e:mptyonly} deletes empty shapes and units linked to empty shapes
    only. Unmatched shapes and units will be retained.

{pstd}
    A shape is considered empty if it contains only missing coordinates.

{marker query}{...}
{dlgtab:geoframe query}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:q:uery} [{it:function}] [...]

{pstd}
    provides functions to obtain information about the units and shapes in the
    current frame. Available functions are:

{p2colset 9 22 24 2}{...}
{p2col : {helpb geoframe##q_n:n}}number of units and shapes; the default
    {p_end}
{p2col : {helpb geoframe##q_dir:{ul:dir}ection}}orientation of shape items
    {p_end}
{p2col : {helpb geoframe##q_dir:{ul:or}ientation}}synonym for {cmd:direction}
    {p_end}
{p2col : {helpb geoframe##q_gtype:{ul:gt}ype}}geometry type of shape items
    {p_end}
{p2col : {helpb geoframe##q_bbox:{ul:bb}ox}}bounding box of shapes
    {p_end}

{marker q_n}{...}
{pstd}{ul:Number of units and shapes}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {opt q:uery} [{opt n}] {ifin}

{pstd}
    obtains information about the number of selected units and corresponding
    shape items (depending on context, a shape item can be a polygon, a line,
    or a point). The following scalars are returned in {cmd:r()}.

{p2colset 9 22 22 2}{...}
{p2col : {cmd:r(units)}}number of units
    {p_end}
{p2col : {cmd:r(items)}}number of shape items (polygons, lines, or points)
    {p_end}
{p2col : {cmd:r(min)}}minimum number of shape items per unit
    {p_end}
{p2col : {cmd:r(avg)}}average number of shape items per unit
    {p_end}
{p2col : {cmd:r(max)}}maximum number of shape items per unit
    {p_end}

{marker q_dir}{...}
{pstd}{ul:Orientation of shape items}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {opt q:uery} {opt dir:ection} {ifin}

{pstd}
    obtains information about the orientation of the shape items of the
    selected units. The following scalars are returned in {cmd:r()}.

{p2colset 9 22 22 2}{...}
{p2col : {cmd:r(items)}}total number of shape items
    {p_end}
{p2col : {cmd:r(neg)}}number of items with negative orientation (clockwise)
    {p_end}
{p2col : {cmd:r(pos)}}number of items with positive orientation (counterclockwise)
    {p_end}
{p2col : {cmd:r(null)}}number of items with undefined orientation (items
    with less than 3 unique points)
    {p_end}

{pstd}
    {cmd:geoframe query orientation} is a synonym for {cmd:geoframe query direction}.

{marker q_gtype}{...}
{pstd}{ul:Geometry type of shape items}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {opt q:uery} {opt gt:ype} {ifin}

{pstd}
    obtains information about the type of geometry of the shape items of the
    selected units. The following scalars are returned in {cmd:r()}.

{p2colset 9 22 22 2}{...}
{p2col : {cmd:r(items)}}total number of shape items
    {p_end}
{p2col : {cmd:r(polygons)}}number of polygon items (three or more points; first
    point equal to last point)
    {p_end}
{p2col : {cmd:r(lines)}}number of line items (two or more points; first point
    unequal last point, unless there are only two points)
    {p_end}
{p2col : {cmd:r(points)}}number of point items (one point)
    {p_end}
{p2col : {cmd:r(empty)}}number of empty items (missing)
    {p_end}

{marker q_bbox}{...}
{pstd}{ul:Bounding box of shapes}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {opt q:uery} {opt bb:ox} {ifin} [{cmd:,} {it:options} ]

{pstd}
    obtains the bounding box of the selected shapes and returns
    it in {cmd:r()}. {it:options} are {opt rot:ate}, {opt cir:cle}, {opt hull},
    {opt pad:ing(#)}, {opt n(#)}, {opt ang:le(#)}, {opt noadj:ust}, and
    {opt nos:hp} as described in {helpb geoframe##bbox:geoframe bbox}. The following
    results are returned in {cmd:r()}.

{pmore} Scalars:

{p2colset 9 22 22 2}{...}
{p2col : {cmd:r(xmin)}}lower X limit of the bounding box
    {p_end}
{p2col : {cmd:r(xmax)}}upper X limit of the bounding box
    {p_end}
{p2col : {cmd:r(xmid)}}midpoint between {cmd:r(xmin)} and {cmd:r(xmax)}
    {p_end}
{p2col : {cmd:r(ymin)}}lower Y limit of the bounding box
    {p_end}
{p2col : {cmd:r(ymax)}}upper Y limit of the bounding box
    {p_end}
{p2col : {cmd:r(ymid)}}midpoint between {cmd:r(ymin)} and {cmd:r(ymax)}
    {p_end}

{pmore} Matrix:

{p2col : {cmd:r(bbox)}}(X,Y) coordinates of the bounding box
    {p_end}
{p2col : {cmd:r(limits)}}row vector containing limits of the bounding box (xmin, xmax, ymin, ymax)
    {p_end}

{marker describe}{...}
{dlgtab:geoframe describe}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:d:escribe}

{pstd}
    displays the geoframe settings of the current frame. You may also type

{p 8 15 2}
    {cmd:geoframe} {cmdab:d:escribe} {it:frame}

{pstd}
    to display the settings of frame {it:frame}.

{marker project}{...}
{dlgtab:geoframe project}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:project} [{it:projection} [{it:args}]] {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    applies the specified projection to the coordinates of the selected
    shapes. X coordinates are assumed to be within -180 and 180, Y coordinates
    within -90 and 90; values outside these ranges will be
    clipped before applying the projection. If the current frame is linked to a
    shape frame, the coordinates in the shape frame will also be translated. The
    following projections are currently available:

{p2colset 9 40 42 2}{...}
{p2col :{opt rob:inson} [{it:#}]}Robinson projection; argument {it:#} sets the scale (earth radius;
    default is {cmd:1}; for example, set {it:#} to 6371 if you want the
    resulting coordinates to be in km)
        {p_end}
{p2col : {helpb geo2xy_web_mercator:{ul:web}_mercator} [{it:args}]}Web Mercator projection (spherical model); the default
    {p_end}
{p2col : {helpb geo2xy_mercator_sphere:{ul:mercator_s}phere} [{it:args}]}Mercator projection (spherical model)
    {p_end}
{p2col : {helpb geo2xy_mercator:{ul:merc}ator} [{it:args}]}Mercator projection (ellipsoid model)
    {p_end}
{p2col : {helpb geo2xy_equidistant_cylindrical:{ul:eq}uidistant_cylindrical} [{it:args}]}Equidistant cylindrical projection (spherical model)
    {p_end}
{p2col : {helpb geo2xy_albers_sphere:{ul:albers_s}phere} [{it:args}]}Albers equal-area conic projection (spherical model)
    {p_end}
{p2col : {helpb geo2xy_albers:{ul:alb}ers} [{it:args}]}Albers equal-area conic projection (ellipsoid model)
    {p_end}
{p2col : {helpb geo2xy_lambert_sphere:{ul:lamb}ert_sphere} [{it:args}]}Lambert conformal conic projection (spherical model)
    {p_end}

{pstd}
Except for {cmd:robinson}, the projections are computed by user command
{helpb geo2xy}, which must be installed on the system (see
{bf:{stata ssc describe geo2xy}}); {it:args} are documented as
{it:proj_opts} in {helpb geo2xy}. {it:options} are as follows

{phang}
    {opth xy(varlist)} specifies additional variables in the current
    frame to be translated. By default, only the variables returned by
    {helpb geoframe##:geoframe get coordinates} will be affected. {it:varlist}
    must provide an even number of variables (i.e., pairs of X and Y
    coordinates).

{phang}
    {opth if:shp(exp)} specifies an {it:if} condition to be applied to
    the linked shape frame. Observations in the
    linked shape frame that do not satisfy the specified condition (i.e.,
    observations for which {it:exp} evaluates to zero), as well as units in the
    current frame for which {cmd:ifshp()} leads to an empty selection in the
    shape frame, will not be transformed. {cmd:ifshp()} is only allowed
    if the current frame is linked to a shape frame. {cmd:ifshp()} and
    {cmd:noshp} are not both allowed.

{phang}
    {opt nos:hp} applies the projection to the current frame only, even if there
    is a linked shape frame.

{phang}
    {opt into(newname [newshpname])} copies the transformed data into a new frame
    called {it:newname} and leaves the current frame unchanged. If the current
    frame has a linked shape frame, {cmd:into()} also creates a new shape
    frame and leaves the original shape frame unchanged. The
    default name for the new shape frame is {it:newname}{cmd:_shp}; specify
    {it:newshpname} to provide an alternative name. {cmd:into()} and {cmd:noshp}
    are not both allowed.

{phang}
    {opt replace} allows {cmd:into()} to overwrite existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame. ({cmd:current} has
    no effect if the {cmd:frame} prefix is applied, as Stata will immediately switch back
    to the prior frame.)

{marker select}{...}
{dlgtab:geoframe select}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:sel:ect} {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    selects the observations satisfying the {it:if} and {it:in} qualifiers in
    the current frame. By default, if the current frame has been linked to a
    shape frame by {helpb geoframe##create:geoframe create} or
    {helpb geoframe##link:geoframe link}, the selection will also be applied to
    the shape frame and the linkage will be rebuilt. {it:options} are as
    follows.

{phang}
    {opth if:shp(exp)} specifies an {it:if} condition to be applied to
    the linked shape frame when selecting observations. Observations in the
    linked shape frame that do not satisfy the specified condition (i.e.,
    observations for which {it:exp} evaluates to zero) will be removed from
    the (copy of the) linked shape frame, and units for which no observations
    remain in the linked shape frame after applying {cmd:ifshp()} will be
    dropped from the (copy of the) current frame. {cmd:ifshp()} is only allowed
    if the current frame is linked to a shape frame. {cmd:ifshp()} is not
    allowed together with {cmd:noshp} or {cmd:unlink}.

{phang}
    {opt nodrop} retains units in the (copy of the) current frame for which
    {cmd:ifshp()} removed all shape-frame observations. The default
    is to keep only those units for which at least one observation remains
    after applying {cmd:ifshp()}. {cmd:nodrop} is only relevant if
    {cmd:ifshp()} has been specified.

{phang}
    {opt nos:hp} modifies the (copy of the) current frame
    only, even if the current frame contains a link to a shape frame. That is,
    the linked shape frame will remain unchanged (apart from updating the
    linkage). {opt noshp} also implies that {cmd:into()} will make no copy of
    the shape frame.

{phang}
    {opt unl:ink} does not establish a link to a shape frame in the modified
    frame, even if the current frame contains a link to a shape
    frame. {cmd:unlink} implies {cmd:noshp}.

{phang}
    {opt into(newname [newshpname])} copies the selection in a new frame
    called {it:newname} and leaves the current frame unchanged. If the current
    frame has a linked shape frame, {cmd:into()} also creates a new shape
    frame and leaves the original shape frame unchanged. The
    default name for the new shape frame is {it:newname}{cmd:_shp}; specify
    {it:newshpname} to provide an alternative name.

{phang}
    {opt replace} allows {cmd:into()} to overwrite existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame. ({cmd:current} has
    no effect if the {cmd:frame} prefix is applied, as Stata will immediately switch back
    to the prior frame.)

{marker clip}{...}
{dlgtab:geoframe clip}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:clip} {it:matname} {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    clips the selected shapes by a convex mask, where {it:matname}
    is a matrix containing the (X,Y) coordinates of the clipping mask. For example,
    you could type {bind:{cmd:geoframe rclip r(bbox)}} to apply clipping by the bounding box
    returned by {helpb geoframe##q_bbox:geoframe query bbox}. {it:options} are as
    follows.

{phang}
    {opt l:ine} enforces line clipping for polygons. By default,
    a polygon is clipped in such a way that the result is a
    (closed) polygon (or a set of closed polygons) again. Specify {cmd:line}, to clip a polygon
    to a set of one or more (unclosed) lines. Polygons that lie completely within
    the clipping window remain intact in any case. {cmd:line} has no effect
    if {cmd:noclip} is specified.

{phang}
    {opt nocl:ip} applies selection rather than clipping. Shape items that
    are at least partially inside the clipping window will be selected. {cmd:noclip}
    is implied when processing point data or paired-coordinate data. For
    paired-coordinate data, an item is considered inside if the origin coordinate
    is within the clipping window.

{phang}
    {opt st:rict} changes the behavior of {cmd:noclip}. By default, {cmd:noclip}
    selects shape items that are at least partially inside the clipping
    window. Specify {cmd:strict} to restrict the selection
    to items that are completely within the window.

{phang}
    {opt nosp:lit} changes the behavior of {cmd:noclip}. By default, {cmd:noclip}
    selects or excludes each shape item (i.e. each polygon, line, or point)
    individually. Specify {cmd:nosplit} to treat shape
    items that belong to the same unit (e.g. main territory and exclaves) as a
    group of items to be included or excluded together.

{phang}
    {opt nodrop} retains empty-shape units in the data. The default is to keep only
    those units that remain non-empty after the modification.

{phang}
    {opt nodot:s} suppresses the progress dots that are displayed by default.

{phang}
    {opt into(newname [newshpname])} copies the processed shapes
    into a frame called {it:newname} (and possibly a linked shape
    frame called {it:newshpname}) and leaves the current frame (and its linked
    shape frame) unchanged. The default for {it:newshpname} is
    {it:newname}{cmd:_shp}. Only observations that satisfy the {it:if} and {it:in}
    qualifiers will be copied.

{phang}
    {opt replace} allows {cmd:into()} to overwrite existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame. ({cmd:current} has
    no effect if the {cmd:frame} prefix is applied, as Stata will immediately switch back
    to the prior frame.)

{marker rclip}{...}
{dlgtab:geoframe rclip}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:rclip} {it:limits} {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    clips the selected shapes by a rectangular window, where {it:limits}
    specifies the boundaries of the window. {it:limits} can be specified as a
    {it:{help numlist}} providing the minimum and maximum
    of X and Y in the following order:

            {it:xmin} {it:xmax} {it:ymin} {it:ymax}

{pstd}
    Specify {cmd:.} (missing) to set a boundary to (minus)
    infinity (or omit the boundary). For example, you could type
    {bind:{cmd:geoframe rclip . .} {it:ymin}} to clip from below. Alternatively,
    specify {it:limits} as {it:matname} where
    {it:matname} is the name of a {helpb matrix} (row or column vector)
    containing the limits (in the same order as above). For example, you
    could type {bind:{cmd:geoframe rclip r(limits)}} to apply clipping by the
    (limits of the) bounding box returned by
    {helpb geoframe##q_bbox:geoframe query bbox}. {it:options} are as
    described for {helpb geoframe##clip:geoframe clip}.

{pstd}
    You can also use {helpb geoframe##clip:geoframe clip} for rectangular clipping, but
    {cmd:geoframe rclip} may be more convenient (and, in some situations, faster).

{marker simplify}{...}
{dlgtab:geoframe simplify}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:simplify} [{it:delta}] {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    simplifies (generalizes) the selected shapes (using a Visvalingam–Whyatt
    algorithm), where {it:delta} specifies the degree of simplification. The
    larger {it:delta}, the stronger the simplification. The default value for
    {it:delta} is one. {it:options} are as follows.

{phang}
    {opt joint:ly} causes the selected shapes to be simplified
    jointly, taking into account shared borders. By default, each
    shape item is simplified individually, which may lead to inconsistencies
    in shared borders (i.e., different points may be selected for each
    item). Such inconsistencies are visually insignificant with moderate
    degrees of simplification, and this is why {cmd:jointly} is turned off
    by default. Specify {cmd:jointly} if you want to enforce shared borders
    to be simplified in the same way for all involved shape items. This may be
    time consuming in large datasets.

{phang}
    {opt abs:olute} causes {it:delta} to be interpreted as an absolute
    threshold (minimum triangle area by which points are dropped). {cmd:absolute}
    only has an effect if {it:delta} is specified.

{pmore}
    By default, {it:delta} is interpreted as a multiplier for the base threshold, which
    is determined from the coordinates of the selected shapes as
    0.5 * (Xmax-Xmin)/2000 * (Ymax-Ymin)/2000 (corresponding to half a pixel
    in a four megapixel image). This default is chosen so
    that the simplification will be barely visible when plotting the shapes, but
    thousands of data points may still be removed depending on the complexity of
    the original data. Shape files often come with a great amount of
    detail that will only be visible when zooming in on selected areas of
    the map. Use {cmd:geoframe simplify} to reduce the amount of data in these
    cases, yielding faster screen rendering and smaller graph files, without
    loosing much accuracy of the display. Specify, for example,
    {cmd:geoframe simplify 100} if you want to simplify the shapes with a clear
    visual effect.

{pmore}
    Specify {it:delta} with option {cmd:absolute} if you want to provide a
    custom threshold rather than a multiplier for the default threshold.

{phang}
    {opt nodrop} retains empty-shape units in the data. The default is to keep only
    those units that remain non-empty after the modification.

{phang}
    {opt nodot:s} suppresses the progress dots that are displayed by default.

{phang}
    {opt into(newname [newshpname])} copies the processed shapes
    into a frame called {it:newname} (and possibly a linked shape
    frame called {it:newshpname}) and leaves the current frame (and its linked
    shape frame) unchanged. The default for {it:newshpname} is
    {it:newname}{cmd:_shp}. Only observations that satisfy the {it:if} and {it:in}
    qualifiers will be copied.

{phang}
    {opt replace} allows {cmd:into()} to overwrite existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame. ({cmd:current} has
    no effect if the {cmd:frame} prefix is applied, as Stata will immediately switch back
    to the prior frame.)

{marker refine}{...}
{dlgtab:geoframe refine}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:refine} [{it:delta}] {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    refines the selected shapes by adding extra points within segments that are
    longer than a given threshold. That is, if the distance between two
    neighboring points in a polygon or line is larger than the threshold,
    additional points will be added such that the maximum distance between
    points is smaller than or equal to the threshold. Argument {it:delta}
    affects the degree of refinement; see option {cmd:absolute}
    below. {cmd:geoframe refine} may be useful if you want to apply
    projections. {it:options} are as follows.

{phang}
    {opt abs:olute} causes {it:delta} to be interpreted as an absolute value
    for the threshold (maximum distance between neighboring points). By
    default, {it:delta} is interpreted as a factor by which the base threshold
    will be divided. The base threshold is determined such that a diagonal
    across the map would have at least 200 points. Specify, for example,
    {cmd:geoframe refine 2} to use a threshold that is half the size of the
    default threshold (at least 400 points on the diagonal). Alternatively, specify
    {it:delta} with option {cmd:absolute} if you want to provide a custom
    threshold rather than a divisor for the default threshold. {cmd:absolute}
    only has an effect if {it:delta} is specified.

{phang}
    {opt nodot:s}, {opt into()}, {opt replace}, and {opt cur:rent} as described for
    {helpb geoframe##simplify:geoframe simplify}.

{marker bshare}{...}
{dlgtab:geoframe bshare}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:bshare} {ifin}
    [{cmd:,} {cmd:not} {it:options} ]

{pstd}
    modifies the selected shapes so that only shared borders remain. Specify option
    {cmd:not} to remove shared borders and retain the other parts of the shape
    items. The default is to retain shared borders and remove the other
    parts. Further {it:options} are {opt nodrop}, {opt nodot:s}, {opt into()},
    {opt replace}, and {opt cur:rent} as described for
    {helpb geoframe##simplify:geoframe simplify}.

{marker generate}{...}
{dlgtab:geoframe generate}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:g:enerate} {it:function} {it:...}

{pstd}
    provides functions to generate specific variables in the current frame or in a
    linked shape frame. Available functions are:

{p2colset 9 22 24 2}{...}
{p2col : {helpb geoframe##g_centroids:{ul:cen}troids}}generate centroid variables in current frame
    {p_end}
{p2col : {helpb geoframe##g_area:area}}generate shape size variable in current frame
    {p_end}
{p2col : {helpb geoframe##g_pid:pid}}generate within-unit polygon ID (shape item ID) in shape frame
    {p_end}
{p2col : {helpb geoframe##g_dir:{ul:dir}ection}}generate direction indicator for shape items in shape frame
    {p_end}
{p2col : {helpb geoframe##g_dir:{ul:or}ientation}}synonym for {cmd:geoframe generate direction}
    {p_end}
{p2col : {helpb geoframe##g_gtype:{ul:gt}ype}}generate geometry-type indicator for shape items in shape frame
    {p_end}
{p2col : {helpb geoframe##g_plevel:{ul:pl}evel}}generate enclave/exclaves identifier in shape frame
    {p_end}
{p2col : {helpb geoframe##g_shpmatch:{ul:shp}match}}generate indicator in current frame
    whether unit has match in the linked shape frame
    {p_end}

{marker g_centroids}{...}
{pstd}{ul:Centroids}

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:cen:troids} [{it:CX CY}] [{cmd:,} {opt replace} {opt noset} ]

{pstd}
    generates variables containing the coordinates of the centroid of each shape
    (across all shape items belonging to the same unit). The command can be
    applied both to a shape frame (in which case the centroids
    will be stored in the shape frame) or to an attribute frame that has a linked shape
    frame (in which case the centroids will be stored in the attribute frame). {it:CX}
    and {it:CY} specify the names of the generated
    variables; {cmd:_CX} and {cmd:_CY} are used as default variable names. Option
    {cmd:replace} allows overwriting existing variables. The created variables
    will be registered using {helpb geoframe##set:geoframe set centroids}
    unless option {cmd:noset} is specified.

{marker g_area}{...}
{pstd}{ul:Shape sizes}

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmd:area} [{it:AREA}] [{cmd:,} {opt s:cale(exp)} {opt replace} {opt noset} ]

{pstd}
    generates a variable containing the size of the area enclosed in each
    shape (including all shape items belonging to the same unit). The command
    can be applied both to a shape frame (in which case the variable
    will be stored in the shape frame) or to an attribute frame that has a linked shape
    frame (in which case the variable will be stored in the attribute frame). {it:AREA}
    specifies a name for the generated variable; {cmd:_AREA}
    is used as default variable name. Option {opt scale(exp)} determines
    the scale of the resulting areas; for example, if the coordinates are in meters,
    type {cmd:scale(100)} to obtain areas in hectares, or type {cmd:scale(1000)}
    to obtain areas in square kilometers. Option {cmd:replace} allows overwriting
    an existing variable. The created variable will be registered using
    {helpb geoframe##set:geoframe set area} unless option {cmd:noset} is
    specified.

{marker g_pid}{...}
{pstd}{ul:Shape item IDs}

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmd:pid} [{it:PID}] [{cmd:,} {opt replace} {opt noset} ]

{pstd}
    generates a variable identifying the different polygons or other shape items
    (lines, points) within each unit
    represented in the frame. If {cmd:geoframe generate pid} is applied to an
    attribute frame that has a linked shape frame, the variable will be added to
    the shape frame (considering all observations of the shape frame). {it:PID}
    specifies a name for the generated variable; {cmd:_PID} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. The created variable will be registered in the shape frame using
    {helpb geoframe##set:geoframe set pid} unless option {cmd:noset} is
    specified.

{pstd}
    Shape items are assumed to be separated by rows of missing
    coordinates. If a unit contains no missing rows, each observation in the
    unit is assumed to be a separate point item.

{marker g_dir}{...}
{pstd}{ul:Orientation of shape items}

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:dir:ection} [{it:DIR}] [{cmd:,} {opt replace} {opt nolab:el} ]

{pstd}
    generates a variable identifying the orientation of each shape item in the
    frame: 1 = positive (counterclockwise), -1 = negative (clockwise), 0 = undetermined
    (item has less than 3 unique points). If {cmd:geoframe generate direction} is applied to an
    attribute frame that has a linked shape frame, the variable will be added to
    the shape frame (considering all observations of the shape frame). {it:DIR}
    specifies a name for the generated variable; {cmd:_DIR} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. Option {cmd:nolabel} omits creating and assigning value labels.

{pstd}
    {cmd:geoframe generate orientation} is a synonym for {cmd:geoframe generate direction}.

{marker g_gtype}{...}
{pstd}{ul:Geometry types of shape items}

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:gt:ype} [{it:GTYPE}] [{cmd:,} {opt replace} {opt nolab:el} ]

{pstd}
    generates a variable identifying the geometry type of each shape item in the
    frame: 1 = polygon, 2 = (poly)line, 3 = point, 0 = empty (item contains missing
    only). If {cmd:geoframe generate gtype} is applied to an
    attribute frame that has a linked shape frame, the variable will be added to
    the shape frame (considering all observations of the shape frame). {it:GTYPE}
    specifies a name for the generated variable; {cmd:_GTYPE} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. Option {cmd:nolabel} omits creating and assigning value labels.

{marker g_plevel}{...}
{pstd}{ul:Enclaves and exclaves}

{marker gen_plevel}{...}
{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:pl:evel} [{it:PLEVEL}] {ifin}
    [{cmd:,} {cmd:by(}{help varname:{it:byvar}}{cmd:)} {opt replace} {opt noset}
    {opt nodot:s} ]

{pstd}
    generates (or updates) a variable identifying the plot order of the different polygons
    represented in the frame. If {cmd:geoframe generate plevel} is applied to an
    attribute frame that has a linked shape frame, the variable will be added to
    or updated in the shape frame.

{pstd}
    {it:PLEVEL} specifies a name for the generated variable; {cmd:_PLEVEL} is used as default
    variable name. If variable {it:PLEVEL} already exists and is numeric, the
    values of the selected observations will be updated; the existing values of
    the other observations will not be changed. If variable {it:PLEVEL} does not
    exist, a new variable will be created, which will be set to missing for all
    other observations. If variable {it:PLEVEL} already exists and is not
    numeric, an error will be displayed. Specify option {cmd:replace} to allow
    overwriting the variable in this case. You can also use {cmd:replace}
    to replace an existing numeric variable rather then updating it. Variable
    {it:PLEVEL} will be registered in the shape frame using
    {helpb geoframe##set:geoframe set plevel} unless option {cmd:noset} is
    specified. Option {cmd:nodots} suppresses the progress dots that are displayed
    by default.

{pstd}
    By default, {cmd:geoframe generate plevel} will do a search for nested polygons
    across all units in the data. Use option {cmd:by()} to stratify the search, for example,
    if the data contain separate regions without overlap or if the data contain units
    from different aggregation levels that should not be compared. If {cmd:by()}
    is specified, a separate search will be done within each group of units defined by the
    levels of {it:byvar}.

{pstd}
    {cmd:geoframe generate plevel} is useful to identify enclaves and exclaves
    so that they do not get covered when plotting filled areas. The generated
    variable will be set to 0 for polygons that are neither enclaves nor exclaves,
    1 for enclaves, 2 for exclaves, 3 for enclaves within exclaves, 4 for exclaves
    within enclaves, etc. {helpb geoplot} will then print the polygons in this
    order. The algorithm used by {cmd:geoframe generate plevel} assumes
    that polygons do not overlap (i.e. that a polygon is always either completely
    inside or completely outside of another polygon; technically, a polygon is classified
    as outside as soon as at least one coordinate lies outside). It also assumes that
    enclaves are explicitly included in the data (that is, if unit A has
    an exclave in unit B, then unit B must contain a corresponding enclave
    polygon). Results will be invalid if these assumptions are not met.

{pstd}
    {cmd:geoframe generate plevel} can be slow in larger datasets. Make sure to
    apply {cmd:geoframe generate plevel} only to shapes at the same
    level of aggregation (e.g. regions or countries, but not regions and countries
    simultaneously); if relevant, use option {cmd:by()} to stratify by level. Furthermore,
    it is generally a good idea to restrict the
    data to the geographic region you want to include in your
    graph (e.g. using {helpb geoframe##select:geoframe select}) before applying
    {cmd:geoframe generate plevel}.

{marker g_shpmatch}{...}
{pstd}{ul:Match in shape frame}

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:shp:match} [{it:SHPMATCH}] [{cmd:,} {opt replace} ]

{pstd}
    generates a variable indicating whether a unit has a match in the linked
    shape frame (i.e., whether shape information is available for a unit). The command
    can only be applied to a frame that has a link to a shape frame. {it:SHPMATCH}
    specifies a name for the generated variable; {cmd:_SHPMATCH} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing variable.

{marker copy}{...}
{dlgtab:geoframe copy}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:copy} {it:frame2}
    {varlist} [{cmd:,} {opt tar:get(namelist)} {opt ex:clude(varlist)}
    {cmd:id(}{it:id} [{it:id2}]{cmd:)} ]

{pstd}
    copies variables from {it:frame2} (source frame) into the current
    frame (target frame), where {varlist} specifies the variables to be copied. Both
    frames can be unique (one row per unit; e.g.,
    an attribute frame) or non-unique (multiple rows per unit; e.g., a shape
    frame) and the operation performed by {cmd:geoframe copy} depends on the specific
    combination.

{phang}
{space 1}o{space 2}Both frames unique: The variables will be
    copied from {it:frame2} into the current frame using a one-to-one merge.

{phang}
{space 1}o{space 2}Current frame non-unique, {it:frame2} unique: The variables will be
    copied using a many-to-one merge.

{phang}
{space 1}o{space 2}Current frame unique, {it:frame2} non-unique: The variables will be
    copied by selecting the first observation from each unit in
    {it:frame2} (assuming the variables are constant within units).

{phang}
{space 1}o{space 2}Both frames non-unique: The variables will be
    copied by selecting the first observation from each unit in
    {it:frame2} and duplicating values within units in the
    current frame.

{pstd}
    If a link from {helpb geoframe##create:geoframe create} or
    {helpb geoframe##link:geoframe link} exists between the two frames, the
    existing link will be employed. Otherwise, an appropriate
    link will be established on the fly. Options are as follows.

{phang}
    {opt target(namelist)} specifies alternative variable names to be used
    in the current frame. {it:varlist} and {cmd:target()}, after applying {cmd:exclude()},
    will be matched one by one. If {cmd:target()} contains fewer
    elements than {it:varlist}, the remaining names will be taken
    from {it:varlist}.

{phang}
    {opt exclude(varlist)} excludes the specified variables from the main
    {varlist}. These variables will not be copied.

{phang}
    {cmd:id(}{it:id} [{it:id2}]{cmd:)} specifies custom ID variables for the
    merging. The default is to use the ID variables returned by
    {helpb geoframe##set:geoframe get id} (and to abort with error if
    {helpb geoframe##set:geoframe get id} fails in any of the two frames). Use
    option {cmd:id()} to override this default behavior. Argument {it:id} provides
    the name of the ID in the current frame, optional {it:id2} provides
    the name of the ID in {it:frame2}; the same name will be used in
    both frames if {it:id2} is omitted.

{pstd}
    Results stored by the internal call to {helpb frget} are passed through in
    {cmd:r()}.

{marker append}{...}
{dlgtab:geoframe append}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:ap:pend} {it:frame2}
    [{varlist}] {ifin}
    [{cmd:,} {opth tar:get(varlist)} {opth touse(varname)} ]

{pstd}
    appends observations from {it:frame2} to the current frame. By default, all variables
    from {it:frame2} will be included, creating new variables in the current
    frame if necessary. Specify {varlist} to include a selection of variables only.

{pstd}
    Option {cmd:target()} specifies the names of the target variables
    to which the observations be appended. Use this option to append a variable
    from {it:frame2} to a variable that has a different name in the current
    frame; {it:varlist} and {cmd:target()} are matched one by one; if {cmd:target()}
    contains fewer elements than {it:varlist}, the remaining names will be taken
    from {it:varlist}. The resulting list of target variable must be unique.

{pstd}
    Option {cmd:touse()} specifies a variable identifying the observations to be
    appended (i.e. observations for which the specified variable is unequal
    zero). Use this option as an alternative to {it:{help if}} and {it:{help in}}.

{marker collapse}{...}
{dlgtab:geoframe collapse}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:collapse} {it:frame2}
    {help collapse:{it:clist}} {ifin} {weight} [{cmd:,} {it:options} ]

{pstd}
    finds the positions of the points provided by {it:frame2} in the shapes
    defined by the current frame using {helpb geoframe##spjoin:geoframe spjoin},
    computes summary statistics such as means,
    sums, or counts by shapes using {helpb collapse}, and then adds the results
    as additional variables to the current frame (which can be
    a shape frame or an attribute frame that is linked to a shape frame). Argument
    {it:clist} specifies the statistics to be computed; see {helpb collapse}. Only
    points from {it:frame2} that satisfy the {it:if} and {it:in}
    qualifiers will be considered. {cmd:aweight}s, {cmd:iweight}s, {cmd:fweight}s,
    and {cmd:pweight}s are allowed; see {help weight}, and see {help collapse##weights:Weights}
    in {helpb collapse}. {it:options} are as follows.

{phang}
    {cmd:cw} specifies casewise deletion. If {cmd:cw} is not specified, all
    possible observations are used for each calculated statistic.

{phang}
    {opth sel:ect(exp)} restricts the shapes from the current frame to be
    considered. The default is to use all shapes. Specify
    {opt select(exp)} to consider only shapes for which {it:exp} is unequal 0.

{phang}
    {opt co:ordinates(X Y)} specifies custom coordinate variables
    in {it:frame2}. The default is to use the variables returned by
    {helpb geoframe##set:geoframe get coordinates}.

{phang}
    {opt gen:erate}[{cmd:(}{it:spec}{cmd:)}] causes the IDs of the matched
    shapes to be left behind as a new variable in {it:frame2}. {it:spec} is

            [{help newvar:{it:ID}}] [{cmd:,} {cmd:nosort} {cmd:noset} {cmd:replace} ]

{pmore}
    where {it:ID} is the variable name to be used, {cmd:nosort} omits sorting the data in
    {it:frame2} by the generated ID variable, {cmd:noset} omits registering the
    generated ID variable in {it:frame2} using {helpb geoframe##set:geoframe set id}, and
    {cmd:replace} allows overwriting an existing variable. The default for {it:ID} is {cmd:_ID}.

{phang}
    {opt nodot:s} suppresses the progress dots of the spatial join that are displayed by default.

{phang}
    {cmd:id(}{help varname:{it:ID}}{cmd:)} indicates that an ID variable matching the points in
    {it:frame2} to the shapes in the current frame already exists, e.g. from an
    earlier call to {cmd:geoframe collapse} with option {cmd:generate()} or from
    a call to {helpb geoframe##spjoin:geoframe spjoin}. In this case,
    {cmd:geoframe collapse} will base its computations on existing variable
    {it:ID} rather than computing a new ID variable. Options {cmd:select()},
    {cmd:coordinates()}, and {cmd:generate()} are not allowed if {cmd:id()} is specified.

{marker contract}{...}
{dlgtab:geoframe contract}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:contract} {it:frame2}
    {ifin} {weight} [{cmd:,} {it:options} ]

{pstd}
    is analogous to {helpb geoframe##collapse:geoframe collapse} but uses
    {helpb contract} to summarize the data. {cmd:fweight}s are allowed; see
    {help weight}. {it:options} are options described in {helpb contract}
    as well as {cmd:select()}, {cmd:coordinates()}, {cmd:generate()}, and
    {cmd:id()} as described in {helpb geoframe##collapse:geoframe collapse}.

{marker spjoin}{...}
{dlgtab:geoframe spjoin}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:spjoin} {it:frame2} [{it:ID}]
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    finds the positions of the points provided by the current frame in the shapes
    defined by {it:frame2} (note the reversed logic of the syntax compared to
    {helpb geoframe##collapse:geoframe collapse}) and stores the IDs of the matched shapes
    in variable {it:ID} in the current frame; {cmd:_ID} is used as default
    variable name. Only points satisfying the {it:if} and {it:in}
    qualifiers will be considered; for all other points {it:ID} will be set to
    missing. {it:frame2} can be a shape frame or an attribute frame
    that is linked to a shape frame. The spacial join algorithm assumes that
    shapes do not overlap (no crossings). It also assumes that nested shapes in
    {it:frame2} (or its linked shape frame) have been tagged using
    {helpb geoframe##gen_plevel:geoframe generate plevel}
    (if there are nested shapes). {it:options} are as follows.

{phang}
    {opth sel:ect(exp)} restricts the shapes from {it:frame2} that will be
    considered in the spatial join. The default is to use all shapes. Specify
    {opt select(exp)} to consider only shapes for which {it:exp} is unequal 0.

{phang}
    {opt co:ordinates(X Y)} specifies custom coordinate variables
    in the current frame. The default is to use the variables returned by
    {helpb geoframe##set:geoframe get coordinates}.

{phang}
    {cmd:nosort} does not sort the data. The default is to sort the data in the
    current frame by the created ID variable (preserving the original sort order
    within ID).

{phang}
    {cmd:noset} does not register the created ID variable. By default,
    the created variable will be registered in the current frame using
    {helpb geoframe##set:geoframe set id}.

{phang}
    {cmd:replace} allows overwriting an existing variable.

{phang}
    {opt nodot:s} suppresses the progress dots that are displayed by default.

{marker grid}{...}
{dlgtab:geoframe grid}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:grid} {it:newname} [{it:newshpname}]
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    generates grid lines covering the range of the selected shapes and stores
    them in a new frame called {it:newname} and an associated shape frame
    called {it:newshpname}; {it:newname}{cmd:_shp} is
    used as name for the shape frame if {it:newshpname} is omitted. The current
    frame may be a shape frame or an attribute frame that has been linked to a
    shape frame. {it:options} are as follows.

{phang}
    {cmd:x(}{cmd:#}{it:#}{cmd:)} or {opth x(numlist)} determines the
    positions of the vertical grid lines. Use {cmd:x(}{cmd:#}{it:#}{cmd:)},
    where {it:#} is the desired number of lines, to
    determine the positions automatically based on the range of the coordinates of
    the selected shapes. Use {opth x(numlist)} to specify custom positions. Default
    is {cmd:x(#10)}.

{phang}
    {cmd:y(}{cmd:#}{it:#}{cmd:)} or {opth y(numlist)} determines the
    positions of the horizontal grid lines. Use {cmd:y(}{cmd:#}{it:#}{cmd:)},
    where {it:#} is the desired number of lines, to
    determine the positions automatically based on the range of the coordinates of
    the selected shapes. Use {opth y(numlist)} to specify custom positions. Default
    is {cmd:y(#10)}.

{phang}
    {opt pad:ding(#)} adds padding to the range of the grid (only relevant for
    positions that are determined automatically, not for custom
    positions). Argument {it:#} is in percent of the size in each dimension. For
    example, type {cmd:padding(5)} to add 5% padding. Default is {cmd:padding(0)}.

{phang}
    {opt mesh} omits the first and last line on each axis
    so that the grid has no frame.

{phang}
    {opt n(n)} sets the number of points used to construct a line. The
    default is {cmd:n(100)}.

{phang}
    {opt nos:hp} uses information on coordinates from the current frame even
    if the current frame is linked to a shape frame.

{phang}
    {opt replace} allows overwriting an existing frame.

{phang}
    {opt cur:rent} makes the created frame the current frame. ({cmd:current} has
    no effect if the {cmd:frame} prefix is applied, as Stata will immediately switch back
    to the prior frame.)

{marker bbox}{...}
{dlgtab:geoframe bbox}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:bb:ox} {it:newname} [{it:newshpname}]
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    computes the bounding box of the selected shapes in the current
    frame and stores it in a new frame called {it:newname} and an associated shape frame
    called {it:newshpname}; {it:newname}{cmd:_shp} is used as name for the shape
    frame if {it:newshpname} is omitted. The current
    frame may be a shape frame or an attribute frame that has been linked to a
    shape frame. {it:options} are as follows.

{phang}
    {opt nos:hp} obtains the bounding box from the coordinates found in the
    current frame even if the current frame is linked to a shape frame. The default
    is to base the bounding box on the coordinates in the shape frame.

{phang}
    {cmd:by(}{help varname:{it:byvar}}{cmd:)} computes a separate bounding box
    for each group of units defined by the levels of {it:byvar}. In the new
    frame, the levels of {it:byvar} will be used as values of the ID variable.

{phang}
    {opt rot:ate} allows rotation of the bounding box such that the minimum-area
    bounding box will be found. {cmd:rotate} has no effect if {cmd:circle} or
    {cmd:hull} is specified.

{phang}
    {opt cir:cle} computes the minimum enclosing circle (MEC) rather than a
    rectangular bounding box. Only one of {cmd:circle} and {cmd:hull} is allowed.

{phang}
    {opt hull} computes the convex hull rather than a rectangular bounding
    box. Only one of {cmd:hull} and {cmd:circle} is allowed.

{phang}
    {opt pad:ding(#)} adds padding to the bounding box. Argument {it:#}
    is in percent of the size in each dimension. For example,
    type {cmd:padding(5)} to add 5% padding. Default is {cmd:padding(0)}.

{phang}
    {opt n(n)} sets the number of points used to construct a circle. This is only
    relevant if {cmd:circle} has been specified. The default is {cmd:n(100)}.

{phang}
    {opt ang:le(angle)} rotates the coordinates of the minimum enclosing circle
    by {it:angle} degrees (counterclockwise). This may be useful when using
    {cmd:circle} with a small value for {cmd:n()}.

{phang}
    {opt noadj:ust} omits radius adjustment for the minimum enclosing circle. By
    default, the radius is adjusted so that the true circle is enclosed in
    the n-gram used to form the circle. Specify {cmd:noadjust} if, conversely,
    you want the n-gram to be enclosed within the true circle. {cmd:noadjust} is only
    relevant if {cmd:circle} has been specified.

{phang}
    {opt replace} allows overwriting an existing frame.

{phang}
    {opt cur:rent} makes the created frame the current frame. ({cmd:current} has
    no effect if the {cmd:frame} prefix is applied, as Stata will immediately switch back
    to the prior frame.)

{marker symbol}{...}
{dlgtab:geoframe symbol}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:sym:bol} {it:newname} [{it:newshpname}]
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    creates symbol shapes at the positions of the points in the current frame
    and stores them in a new frame called {it:newname} and an associated shape frame
    called {it:newshpname}; {it:newname}{cmd:_shp} is
    used as name for the shape frame if {it:newshpname} is omitted. {it:options}
    are as follows.

{phang}
    {cmd:shape()}, {cmd:n()}, {cmd:ratio()}, {cmd:angle()}, {cmd:size()}, and
    {cmd:offset()} are options as described for
    layertype {helpb geoplot##symbol:symbol} in {helpb geoplot}.

{phang}
    {opt replace} allows overwriting an existing frame.

{phang}
    {opt cur:rent} makes the created frame the current frame. ({cmd:current} has
    no effect if the {cmd:frame} prefix is applied, as Stata will immediately switch back
    to the prior frame.)

{marker symboli}{...}
{dlgtab:geoframe symboli}

{p 8 15 2}
    {cmd:geoframe} {cmd:symboli} {it:newname} [{it:newshpname}]
    {it:x1} {it:y1} {it:size1} [{it:x2} {it:y2} {it:size2} ...] [{cmd:,} {it:options} ]

{pstd}
    creates symbol shapes of the specified sizes at the specified
    positions and stores stores them in a new frame called {it:newname} and an
    associated shape frame called {it:newshpname}; {it:newname}{cmd:_shp} is
    used as name for the shape frame if {it:newshpname} is omitted. {it:options}
    are as describes for {helpb geoframe##symbol:geoframe symbol}; option {cmd:size()}
    will be ignored.

{marker set}{...}
{dlgtab:geoframe set}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:set} {it:key} [{it:value}]

{pstd}
    defines or adjusts the settings of the current frame. The settings, for example, contain
    information on the names of the coordinate variables. Such information will be
    retrieved by {cmd:geoframe} or {helpb geoplot} when processing a frame. {it:key} can
    be as follows.

{p2colset 9 22 24 2}{...}
{p2col :{it:key}}Description
    {p_end}
{p2col :{cmdab:t:ype}}the type of data in the frame; {it:value} may be
    {cmd:unit} (attribute file), {cmd:pc} (paired-coordinates file),
    or {cmd:shape} (shape file)
    {p_end}
{p2col :{cmdab:f:eature}}the type of features represented by the units in the frame;
    {it:value} can be any text
    {p_end}
{p2col :{cmd:id}}the name of the variable containing the unit ID; {it:value} is a single
    variable name; default is {cmd:_ID}
    {p_end}
{p2col :{cmdab:co:ordinates}}the names of the variables containing the coordinates; {it:value} is a list of
    two (for data types {cmd:unit} and {cmd:shape}) or four (for data type {cmd:pc}) variable names; default is
    {cmd:_X _Y} for data type {cmd:shape}, {cmd:_CX _CY} for data type {cmd:unit},
    and {cmd:_X1 _Y1 _X2 _Y2} for data type {cmd:pc}
    {p_end}
{p2col :{cmdab:cen:troids}}the names of the variables containing the centroids; {it:value} is a list of
    two variable names; default is {cmd:_CX _CY} for data types {cmd:shape} and {cmd:pc}; for data type
    {cmd:unit}, {cmd:centroids} is a synonym for {cmd:coordinates}
    {p_end}
{p2col :{cmd:area}}the name of the variable containing the shape sizes;
    {it:value} is a single variable name; default is {cmd:_AREA}
    {p_end}
{p2col :{cmd:sid}}the name of the variable containing the within-unit sort ID;
    {it:value} is a single variable name; default is {cmd:shape_order}; the setting
    is only relevant for data of type {cmd:shape}
    {p_end}
{p2col :{cmd:pid}}the name of the variable containing the within-unit polygon ID;
    {it:value} is a single variable name; default is {cmd:_PID}; the setting
    is only relevant for data of type {cmd:shape}
    {p_end}
{p2col :{cmdab:pl:evel}}the name of the variable containing the plot level ID (enclaves and exclaves);
    {it:value} is a single variable name; default is {cmd:_PLEVEL}; the setting
    is only relevant for data of type {cmd:shape}
    {p_end}
{p2col :{cmdab:s:hpframe}}the name of the linked shape frame; only allowed with
    {helpb geoframe##get:geoframe get}
    {p_end}
{p2col :{cmd:linkname}}the name of the linking variable in the linked shape frame; only
    allowed with {helpb geoframe##get:geoframe get}
    {p_end}

{pstd}
    {it:key} is case insensitive; you may type {it:key} in lowercase
    (as above) or in uppercase.

{marker get}{...}
{dlgtab:geoframe get}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:get} {it:key} [{cmd:,} {opt l:ocal(lname)} ]

{pstd}
    retrieves the value of a geoframe setting, where {it:key} is the name of the
    setting to be retrieved. Available keys are as listed
    {help geoframe##set:above}.

{pstd}
    Option {opt local()} stores the value of the setting in a local called
    {it:lname} instead of displaying it.

{marker rename}{...}
{dlgtab:geoframe rename}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:ren:ame} {it:newname}
    [{cmd:,} {opt replace} ]

{pstd}
    renames the current frame to {it:newname}. If the current frame has been linked
    to a shape frame by {helpb geoframe##create:geoframe create} or
    {helpb geoframe##link:geoframe link}, the link will
    be updated. If the current frame is a shape
    frame containing links from attribute frames created by
    {helpb geoframe##create:geoframe create} or {helpb geoframe##link:geoframe link},
    these links will be updated. Option
    {cmd:relplace} allows overwriting an existing frame.

{pstd}
    Renaming geoframes using official Stata's {helpb frame rename} is discouraged
    as this will break links created by
    {helpb geoframe##create:geoframe create} or {helpb geoframe##link:geoframe link}.

{marker duplicate}{...}
{dlgtab:geoframe duplicate}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:dup:licate} {it:newname}
    [{it:newshpname}] [{cmd:,} {it:options}]

{pstd}
    copies the current frame to a new frame called {it:newname}. By default,
    if the current frame has been linked to a shape frame by
    {helpb geoframe##create:geoframe create} or
    {helpb geoframe##link:geoframe link}, the shape frame will also be copied
    and a link between the new frames will be established. The
    default name for the new shape frame is {it:newname}{cmd:_shp}; specify
    {it:newshpname} to provide an alternative name. {it:options} are as follows.

{phang}
    {opt nos:hp} makes a copy of the current frame only, even if the
    current frame contains a link to a shape frame. Instead of copying the
    shape frame, a link to the original shape frame will be established in the
    new frame.

{phang}
    {opt unl:ink} does not establish a link to a shape frame in the new frame
    even if the current frame contains a link to a shape frame. {cmd:unlink}
    implies {cmd:noshp}.

{phang}
    {opt replace} allows overwriting existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame. ({cmd:current} has
    no effect if the {cmd:frame} prefix is applied, as Stata will immediately switch back
    to the prior frame.)

{pstd}
    Copying geoframes using official Stata's {helpb frame copy} is discouraged
    as this will break links created by
    {helpb geoframe##create:geoframe create} or {helpb geoframe##link:geoframe link}.

{marker relink}{...}
{dlgtab:geoframe relink}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:rel:ink}

{pstd}
    rebuilds an existing link to a shape frame that has been created in the
    current frame by {helpb geoframe##create:geoframe create} or
    {helpb geoframe##link:geoframe link}. For example, use {cmd:geoframe relink}
    to fix a broken linkage after dropping or adding observations. If the current frame
    does not contain a link to a shape frame, {cmd:geoframe relink} has no effect.

{pstd}
    Alternatively, type {cmd:geoframe link} {it:shpframe} to fix a broken link.

{marker unlink}{...}
{dlgtab:geoframe unlink}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:unl:ink}

{pstd}
    removes the link to a shape frame that has been created in the
    current frame by {helpb geoframe##create:geoframe create} or
    {helpb geoframe##link:geoframe link}. If the current frame does not contain
    a link to a shape frame, {cmd:geoframe unlink} has no effect.

{marker attach}{...}
{dlgtab:geoframe attach}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:at:tach} {it:frame2}
    [{varlist}] [{cmd:,} {opth ex:clude(varlist)} ]

{pstd}
    attaches {it:frame2} to the current frame using an {cmd:m:1} merge. That is, a
    link to {it:frame2} is established and all variables from {it:frame2}
    that do not already exist in the current frame (except for the ID) are added
    as aliases to the current frame. {it:frame2} must be an attribute frame
    (one row per unit) and the current frame is typically a frame containing
    shape information on the units represented in {it:frame2}. After attaching
    {it:frame2} to the current frame, the variables
    from {it:frame2} will be available in the current frame like any other variables
    in the current frame.

{pstd}
    By default, all relevant variables from {it:frame2} will be added as aliases
    to the current frame. Specify {varlist} to select the variables
    to be added; use option {cmd:exlcude()} to exclude variables from being added.

{pstd}
    {cmd:geoframe attach} requires Stata 18.

{marker detach}{...}
{dlgtab:geoframe detach}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:det:ach} {it:frame2}

{pstd}
    detaches {it:frame2} from the current frame. All alias variables related to
    {it:frame2} will be removed and the link to {it:frame2}
    will be deleted from the current frame.

{pstd}
    {cmd:geoframe detach} requires Stata 18.


{title:Examples}

{pstd}
    Load attribute data and associated shape file in one call:

{p 8 12 2}
    {stata "local url http://fmwww.bc.edu/repec/bocode/i/"}
    {p_end}
{p 8 12 2}
    {stata geoframe create regions `url'Italy-RegionsData.dta, id(id) coord(xcoord ycoord) shpfile(Italy-RegionsCoordinates.dta)}
    {p_end}
{p 8 12 2}
    {stata geoplot (area regions fortell), tight}
    {p_end}

{pstd}
    Options {cmd:id()} and {cmd:coord()} have been specified because
    {cmd:Italy-RegionsData.dta} uses custom names for the unit ID and the
    coordinate variables (the default variable names for an attribute file are {cmd:_ID},
    {cmd:_CX} and {cmd:_CY}). No such options were specified for the shape file
    because the variables in {cmd:Italy-RegionsCoordinates.dta} comply with the
    default naming conventions for shape files ({cmd:_ID},
    {cmd:_X} and {cmd:_Y}).

{pstd}
    Load attribute data and associated shape file in two steps and then link them
    manually using {helpb geoframe##link:geoframe link}:

{p 8 12 2}
    {stata "local url http://fmwww.bc.edu/repec/bocode/i/"}
    {p_end}
{p 8 12 2}
    {stata geoframe create regions `url'Italy-RegionsData.dta, id(id) coord(xcoord ycoord) replace}
    {p_end}
{p 8 12 2}
    {stata geoframe create regions_shp `url'Italy-RegionsCoordinates.dta, replace}
    {p_end}
{p 8 12 2}
    {stata "frame regions: geoframe link regions_shp"}
    {p_end}
{p 8 12 2}
    {stata "frame regions: geoframe describe"}
    {p_end}
{p 8 12 2}
    {stata geoplot (area regions fortell), tight}
    {p_end}

{pstd}
    Option {cmd:replace} has been specified so that the existing frames from the
    first examples can be overwritten.

{pstd}
    Load shape file and attribute data in two steps and then link them using
    {helpb geoframe##attach:geoframe attach}: An alternative to the above linking
    approach is to load the shape file and the
    attribute file separately and then use {helpb geoframe##attach:geoframe attach}
    to make the attribute data accessible directly from within the
    shape frame (this requires Stata 18). Conceptually, the shape file is then
    the main working frame and the attribute data is kept in an
    auxiliary frame.

{p 8 12 2}
    {stata "local url http://fmwww.bc.edu/repec/bocode/i/"}
    {p_end}
{p 8 12 2}
    {stata geoframe create regions `url'Italy-RegionsCoordinates.dta, replace}
    {p_end}
{p 8 12 2}
    {stata geoframe create regions_data `url'Italy-RegionsData.dta, id(id) coord(xcoord ycoord)}
    {p_end}
{p 8 12 2}
    {stata "frame regions: geoframe attach regions_data"}
    {p_end}
{p 8 12 2}
    {stata "frame regions: describe"}
    {p_end}
{p 8 12 2}
    {stata geoplot (area regions fortell), tight}
    {p_end}

{pstd}
    An advantage of such a reversed approach is that multiple attribute frames
    can be attached to the same shape frame.

{pstd}
    Yet another approach is to load the shape file and the attribute data
    separately and then use {helpb geoframe##copy:geoframe copy} to copy selected
    variables from the attribute data into the shape frame (not shown).

{pstd}
    Use of option {cmd:feature()} for lakes and rivers:

{p 8 12 2}
    {stata "local url http://fmwww.bc.edu/repec/bocode/i/"}
    {p_end}
{p 8 12 2}
    {stata geoframe create regions `url'Italy-RegionsCoordinates.dta, replace}
    {p_end}
{p 8 12 2}
    {stata geoframe create lakes `url'Italy-Lakes.dta, feature(water)}
    {p_end}
{p 8 12 2}
    {stata geoframe create rivers `url'Italy-Rivers.dta, feature(water)}
    {p_end}
{p 8 12 2}
    {stata geoplot (area regions) (area lakes) (line rivers), tight}
    {p_end}


{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2023). geoplot: Stata module to draw maps. Available from
    {browse "https://ideas.repec.org/c/boc/bocode/s459211.html"}.


{title:Also see}

{psee}
    Online:  help for
    {helpb geoplot}, {helpb frames}, {helpb spshape2dta}
