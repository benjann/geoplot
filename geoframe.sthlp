{smcl}
{* 11sep2023}{...}
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
{synopt :{helpb geoframe##create:{ul:cr}eate}}load data into geoframe or declare
    current frame as geoframe
    {p_end}
{p2col :{helpb geoframe##link:{ul:l}ink}}link shape frame to current frame
    {p_end}
{p2col :{helpb geoframe##clean:{ul:cl}ean}}delete unmatched/empty shapes and units
    {p_end}
{p2col :{helpb geoframe##select:{ul:sel}ect}}select units and shapes
    {p_end}
{synopt :{helpb geoframe##describe:{ul:d}escribe}}describe geoframe
    {p_end}

{syntab :Manipulation}
{p2col :{helpb geoframe##generate:{ul:g}enerate}}generate special-purpose variable in current frame
    {p_end}
{p2col :{helpb geoframe##bbox:{ul:bb}ox}}store bounding box, enclosing circle, or convex hull in new frame
    {p_end}
{p2col :{helpb geoframe##symbol:{ul:sym}bol}}generate symbol shapes and store in new frame
    {p_end}
{p2col :{helpb geoframe##symboli:symboli}}{cmd:symbol} with immediate arguments
    {p_end}

{syntab :Spatial join}
{p2col :{helpb geoframe##collapse:collapse}}collapse points from other frame into current frame
    {p_end}
{p2col :{helpb geoframe##contract:contract}}contract points from other frame into current frame
    {p_end}
{p2col :{helpb geoframe##spjoin:spjoin}}match points in current frame to shapes from other frame
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
{p2col :{helpb geoframe##copy:copy}}copy variables from attribute frame to current frame
    {p_end}
{p2col :{helpb geoframe##append:{ul:ap}pend}}append observations from other frame to current frame
    {p_end}
{synoptline}


{title:Description}

{pstd}
    {cmd:geoframe} prepares data for {helpb geoplot}.


{title:Subcommands}

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
    an attribute file created by {helpb spshape2dta}. If a shape file belonging
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
    {opt nodrop} prevents dropping unmatched or empty shapes from {it:shpframe}. By
    default, {cmd:geoframe create} will only keep shapes in {it:shpframe} that
    are not empty and that have a matching unit in the main frame. A shape is
    considered empty if it contains only a single observation and if the
    coordinates in this observation are missing.

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
    A shape is
    considered empty if it contains only a single observation and if the
    coordinates in this observation are missing.

{marker select}{...}
{dlgtab:geoframe select}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:select} {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    selects the observations satisfying the {it:if} and {it:in} qualifiers in
    the current frame. By default, if the current frame has been linked to a
    shape frame by {helpb geoframe##create:geoframe create} or
    {helpb geoframe##link:geoframe link}, the selection will also be applied to
    the shape frame and the linkage will be rebuilt. {it:options} are as
    follows.

{phang}
    {opt into(newname [newshpname])} copies the selection in a new frame
    called {it:newname} and leaves the current frame unchanged. If the current
    frame has been linked to a shape frame by
    {helpb geoframe##create:geoframe create} or
    {helpb geoframe##link:geoframe link}, {cmd:into()} also creates a new shape
    frame and leaves the original shape frame unchanged. The
    default name for the new shape frame is {it:newname}{cmd:_shp}; specify
    {it:newshpname} to provide an alternative name.

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
    {opt replace} allows {cmd:into()} to overwrite existing frames.

{phang}
    {opt nodes:cribe} suppresses the description of the created frame.

{phang}
    {opt nocur:rent} does not make the created frame the current frame.

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

{marker generate}{...}
{dlgtab:geoframe generate}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:g:enerate} {it:fnc} {it:...}

{pstd}
    provides functions to generate specific variables in the current frame. Available
    functions are as follows.

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:cen:troids} [{it:CX CY}] [{cmd:,} {opt replace} {opt noset} ]

{pstd}
    generates variables containing the coordinates of the centroids
    of the shapes. The command can be applied both to a shape frame (in which case the centroids
    will be stored in the shape frame) or to an attribute frame that has a linked shape
    frame (in which case the centroids will be stored in the attribute frame). {it:CX}
    and {it:CY} specify the names of the generated
    variables; {cmd:_CX} and {cmd:_CY} are used as default variable names. Option
    {cmd:replace} allows overwriting existing variables. The created variables
    will be registered using {helpb geoframe##set:geoframe set centroids}
    unless option {cmd:noset} is specified.

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmd:area} [{it:AREA}] [{cmd:,} {opt s:cale(exp)} {opt replace} {opt noset} ]

{pstd}
    generates a variable containing the size of the area enclosed in each
    shape. The command can be applied both to a shape frame (in which case the variable
    will be stored in the shape frame) or to an attribute frame that has a linked shape
    frame (in which case the variable will be stored in the attribute frame). {it:AREA}
    specifies a name for the generated variable; {cmd:_AREA}
    is used as default variable name. Option {cmd:scale()} multiplies the resulting
    areas by {it:exp}; for example, type {cmd:scale(1/10000)} if the coordinates
    are in meters and you want the areas to be in hectares. Option
    {cmd:replace} allows overwriting
    an existing variable. The created variable will be registered using
    {helpb geoframe##set:geoframe set area} unless option {cmd:noset} is
    specified.

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmd:pid} [{it:PID}] [{cmd:,} {opt replace} {opt noset} ]

{pstd}
    generates a variable identifying the different polygons within each unit
    represented in the frame. If {cmd:geoframe generate pid} is applied to an
    attribute frame that has a linked shape frame, the variable will be added to
    the shape frame (considering all observations of the shape frame). {it:PID}
    specifies a name for the generated variable; {cmd:_PID} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. The created variable will be registered in the shape frame using
    {helpb geoframe##set:geoframe set pid} unless option {cmd:noset} is
    specified.

{marker gen_plevel}{...}
{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:pl:evel} [{it:PLEVEL}] {ifin}
    [{cmd:,} {cmd:by(}{help varname:{it:byvar}}{cmd:)} {opt replace} {opt noset} ]

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
    specified.

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

{marker bbox}{...}
{dlgtab:geoframe bbox}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:bb:ox} {it:newname}
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    computes the bounding box of the selected shapes in the current
    frame and stores its coordinates in a new frame called {it:newname}. The current
    frame may be a shape frame or an attribute frame that has been linked to a
    shape frame. {it:options} are as follows.

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
    {opt replace} allows overwriting an existing frame.

{marker symbol}{...}
{dlgtab:geoframe symbol}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:sym:bol} {it:newname}
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    creates symbol shapes at the positions of the points in the current frame
    and stores their coordinates in a new frame called {it:newname}. {it:options}
    are as follows.

{phang}
    {cmd:shape()}, {cmd:n()}, {cmd:ratio()}, {cmd:angle()}, {cmd:size()}, and
    {cmd:offset()} are options as described for
    layertype {helpb geoplot##symbol:symbol} in {helpb geoplot}.

{phang}
    {opt replace} allows overwriting an existing frame.

{marker symboli}{...}
{dlgtab:geoframe symboli}

{p 8 15 2}
    {cmd:geoframe} {cmd:symboli} {it:newname}
    {it:x1} {it:y1} {it:size1} [{it:x2} {it:y2} {it:size2} ...] [{cmd:,} {it:options} ]

{pstd}
    creates symbol shapes of the specified sizes at the specified
    positions and stores their coordinates in a new frame called {it:newname}. {it:options}
    are as describes for {helpb geoframe##symbol:geoframe symbol}; option {cmd:size()}
    will be ignored.

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
    {opt sel:ect(exp)} restricts the shapes from the current frame to be
    considered. The default is to use all shapes. Specify
    {opt select(exp)} to consider only shapes for which {it:exp} is unequal 0.

{phang}
    {opt co:ordinates(X Y)} specifies custom coordinate variables
    in {it:frame2}. The default is to use the variables returned by
    {helpb geoframe##set:geoframe get coordinates}.

{phang}
    {opt gen:erate}[{cmd:(}{it:spec}{cmd:)}] causes the IDs of the matched
    shapes to be left behind as a new variable in {it:frame2}. {it:spec} is

            [{help newvar:{it:ID}}] [{cmd:,} {cmd:replace} {cmd:noset} ]

{pmore}
    where {it:ID} is the variable name to be used, {cmd:replace} allows
    overwriting an existing variable, and {cmd:noset} omits registering the
    variable in {it:frame2} using {helpb geoframe##set:geoframe set id}. The
    default for {it:ID} is {cmd:_ID}.

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
    {ifin} [{cmd:,} {opt co:ordinates(X Y)} {opt replace} {opt noset} {opt sel:ect(exp)} ]

{pstd}
    finds the positions of the points provided by the current frame in the shapes
    defined by {it:frame2} (note the reversed logic of the syntax compared to
    {helpb geoframe##collapse:geoframe collapse}). Only points satisfying the {it:if} and {it:in}
    qualifiers will be considered. {it:frame2} can be a shape frame or an attribute frame
    that is linked to a shape frame.

{pstd}
    The IDs of the matched shapes will be stored in
    variable {it:ID} in the current frame; {cmd:_ID} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. The created variable will be registered in the current frame using
    {helpb geoframe##set:geoframe set id} unless option {cmd:noset} is
    specified. Option {opt coordinates()} specifies custom coordinate variables
    in the current frame; the default is to use the variables returned by
    {helpb geoframe##set:geoframe get coordinates}.

{pstd}
    Option {opt select()} restricts the shapes from {it:frame2} that will be
    considered in the spatial join. The default is to use all shapes. Specify
    {opt select(exp)} to consider only shapes for which {it:exp} is unequal 0.

{pstd}
    The spacial join algorithm assumes that shapes do not overlap (no
    crossings). It also assumes that nested shapes in
    {it:frame2} (or its linked shape frame) have been tagged using
    {helpb geoframe##gen_plevel:geoframe generate plevel}
    (if there are nested shapes).

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
    {opt nodes:cribe} suppresses the description of the created frame.

{phang}
    {opt nocur:rent} does not make the created frame the current frame.

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
    [{cmd:frame} {it:shpframe}{cmd::}] {cmd:geoframe} {cmdab:at:tach} {it:frame}
    [{varlist}] [{cmd:,} {opth ex:clude(varlist)} ]

{pstd}
    attaches {it:frame} to the current frame using an {cmd:m:1} merge. That is, a
    link to {it:frame} is established and all variables from {it:frame}
    that do not already exist in the current frame (except for the ID) are added
    as aliases to the current frame. {it:frame} must be an attribute frame
    (one row per unit) and the current frame is typically a frame containing
    shape information on the units represented in {it:frame}. After attaching
    {it:frame} to the current frame, the variables
    from {it:frame} will be available in the current frame like any other variables
    in the current frame.

{pstd}
    By default, all relevant variables from {it:frame} will be added as aliases
    to the current frame. Specify {varlist} to select the variables
    to be added; use option {cmd:exlcude()} to exclude variables from being added.

{pstd}
    {cmd:geoframe attach} requires Stata 18.

{marker detach}{...}
{dlgtab:geoframe detach}

{p 8 15 2}
    [{cmd:frame} {it:shpframe}{cmd::}] {cmd:geoframe} {cmdab:det:ach} {it:frame}

{pstd}
    detaches {it:frame} from the current frame. All alias variables related to
    {it:frame} will be removed and the link to {it:frame}
    will be deleted from the current frame.

{pstd}
    {cmd:geoframe detach} requires Stata 18.

{marker copy}{...}
{dlgtab:geoframe copy}

{p 8 15 2}
    [{cmd:frame} {it:shpframe}{cmd::}] {cmd:geoframe} {cmdab:copy} {it:frame}
    {varlist} [{cmd:,} {opth ex:clude(varlist)} {opt tar:get(namelist)} ]

{pstd}
    copies the specified variables from {it:frame} into the current
    frame. {it:frame} must be an attribute frame (one row per unit) and
    the current frame is typically a frame containing shape information on the
    units represented in {it:frame}. Use option {cmd:exlcude()} to exclude
    selected variables that have been specified in {varlist} from being added.

{pstd}
    Use option {cmd:target()} to specify alternative variable names to be used
    in the current frame. {it:varlist} and {cmd:target()}, after applying {cmd:exclude()},
    will be matched one by one; if {cmd:target()} contains fewer
    elements than {it:varlist}, the remaining names will be taken
    from {it:varlist}.

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
