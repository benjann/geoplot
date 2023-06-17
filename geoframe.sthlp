{smcl}
{* 17jun2023}{...}
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
{synopt :{helpb geoframe##describe:{ul:d}escribe}}describe geoframe
    {p_end}

{syntab :Manipulation}
{p2col :{helpb geoframe##generate:{ul:g}enerate}}generate special-purpose variable in current frame
    {p_end}
{p2col :{helpb geoframe##spjoin:spjoin}}spatial join of points in current frame to shape frame
    {p_end}

{syntab :Settings}
{synopt :{helpb geoframe##set:set}}update geoframe settings of current frame
    {p_end}
{synopt :{helpb geoframe##get:get}}retrieve geoframe settings from current frame
    {p_end}
{p2col :{helpb geoframe##link:{ul:l}ink}}link shape frame to current frame
    {p_end}
{p2col :{helpb geoframe##unlink:{ul:unl}ink}}unlink shape frame from current frame
    {p_end}

{syntab :Utilities}
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
    an attribute file created by {helpb spshape2dta}. If a linked shape
    file created by {helpb spshape2dta} is available in the same folder as
    {it:filename}, the shape file will automatically be loaded into a second frame called
    {it:frame}{cmd:_shp}, and the link will be registered in {it:frame}. Alternatively,
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

{phang2}
    {opt replace} allows replacing existing frames.

{phang2}
    {opt nodes:cribe} suppresses the description of the frame that is displayed
    by default.

{phang2}
    {opt t:ype(type)} declares the type of data included in the
    frame. {it:type} may be {opt unit} (attribute data), {opt pc}
    (paired-coordinate data), or {opt shape} (shape data). If {cmd:type()} is omitted,
    {cmd:geoframe create} infers the type from context.

{phang2}
    {opt feat:ure(string)} declares the type of feature represented by the
    units in the frame. For example, type {cmd:feature(water)} if the frame
    contains data on lakes or rivers. {it:string} can be any text.

{phang2}
    {opt id(varname)} specifies the name of the unit ID. Default is {cmd:_ID}.

{phang2}
    {opt co:ordinates(X Y [X2 Y2])} specifies the names of the variables containing
    the coordinates. The default depends on data type. It is {cmd:_X _Y} for data type {cmd:shape},
    {cmd:_CX _CY} for data type {cmd:unit}, and {cmd:_X1 _Y1 _X2 _Y2} for data type {cmd:pc}.

{phang2}
    {opt cen:troids(CX CY)} specifies the names of the variables containing
    the centroids of the units. The default is {cmd:_CX _CY}
    for data types {cmd:shape} and {cmd:pc}; for data type {cmd:unit}, {cmd:centroids()}
    is a synonym for {cmd:coordinates()}.

{phang2}
    {opt area(AREA)} specifies the name of the variable containing the sizes
    of the units (i.e. the areas of the shapes). The default is {cmd:_AREA}.

{phang2}
    {opt sid(varname)} specifies the name of the within-unit sort ID. Default is
    {cmd:shape_order}. This is only relevant for data of type {cmd:shape}

{phang2}
    {opt pid(varname)} specifies the name of the within-unit polygon ID. Default is
    {cmd:_PID}. This is only relevant for data of type {cmd:shape}

{phang2}
    {opt pl:evel(varname)} specifies the name of the plot level ID (enclaves and exclaves). Default is
    {cmd:_PLEVEL}. This is only relevant for data of type {cmd:shape}

{phang2}
    {opt noshp:file} deactivates automatic loading of the shape file. By default,
    as described above, {cmd:geoframe create} loads the shape file linked to
    {it:filename} if {it:filename} is an attribute file generated by {helpb spshape2dta}
    and the shape file is available on disk. Specify
    {cmd:noshpfile} to suppress this behavior. Data type {cmd:shape} implies {cmd:noshpfile}.

{phang2}
    {opt shp:file(spec)} specifies a custom shape file to be loaded along with the main
    file. The syntax of {it:spec} is

{p 16 19 2}
    [[{it:shpframe}] [{cmd:using}] {it:shpfilename} ] [{cmd:,} {it:suboptions} ]

{pmore2}
    where {it:shpfilename} is the name (and path) of the shape file on disk and {it:shpframe}
    provides a name for the shape frame. If {it:shpframe} is omitted, {it:frame}{cmd:_shp}
    is used as the name for the shape frame. {it:suboptions} are {opt feat:ure()},
    {cmd:id()}, {opt co:ordinates()}, {cmd:sid()}, {cmd:pid()}, and {opt pl:evel()}
    as described above.

{marker describe}{...}
{dlgtab:geoframe describe}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:d:escribe}

{pstd}
    displays the geoframe settings of the current frame. You may also type
    {cmd:geoframe} {cmd:describe} {it:frame}.

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
    of the shapes. {it:CX} and {it:CY} specify the names of the generated
    variables; {cmd:_CX} and {cmd:_CY} are used as default variable names. Option
    {cmd:replace} allows overwriting existing variables. The created variables
    will be registered using {helpb geoframe##set:geoframe set centroids}
    unless option {cmd:noset} is specified.

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmd:area} [{it:AREA}] [{cmd:,} {opt replace} {opt noset} ]

{pstd}
    generates a variable containing the size of the area enclosed in each
    shape. {it:AREA} specifies a name for the generated variable; {cmd:_AREA}
    is used as default variable name. Option {cmd:replace} allows overwriting
    an existing variable. The created variable will be registered using
    {helpb geoframe##set:geoframe set area} unless option {cmd:noset} is specified.

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmd:pid} [{it:PID}] [{cmd:,} {opt replace} {opt noset} ]

{pstd}
    generates a variable identifying the different polygons within each unit
    represented in the frame. If {cmd:geoframe generate pid} is applied to an attribute
    frame, the operations will be performed on the linked shape frame (if available). {it:PID}
    specifies a name for the generated variable; {cmd:_PID} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. The created variable will be registered in the shape frame using
    {helpb geoframe##set:geoframe set pid} unless option {cmd:noset} is
    specified.

{marker gen_plevel}{...}
{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:pl:evel} [{it:PLEVEL}] [{cmd:,} {opt replace} {opt noset} {opt force} ]

{pstd}
    generates a variable identifying the plot order of the different polygons
    represented in the frame. If {cmd:geoframe generate pid} is applied to an attribute
    frame, the operations will be performed on the linked shape frame (if available). {it:PLEVEL}
    specifies a name for the generated variable; {cmd:_PLEVEL} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. The created variable will be registered in the shape frame using
    {helpb geoframe##set:geoframe set plevel} unless option {cmd:noset} is
    specified. Option {cmd:force} adds the variable even if no nested polygons
    are found (the variable will be 0 for all observations in this case).

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

{marker spjoin}{...}
{dlgtab:geoframe spjoin}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:spjoin} {it:shpframe} [{it:ID}]
    [{cmd:,} {opt co:ordinates(X Y)} {opt replace} {opt noset} ]

{pstd}
    finds the positions of the points provided by the current frame in the shapes
    defined by {it:shpframe}. The IDs of the matched shapes will be stored in
    variable {it:ID} in the current frame; {cmd:_ID} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. The created variable will be registered in the current frame using
    {helpb geoframe##set:geoframe set id} unless option {cmd:noset} is
    specified. Option {opt coordinates()} specifies custom coordinate variables
    in the current frame; the default is to use the variables returned by
    {helpb geoframe##set:geoframe get coordinates}. The spacial join algorithm assumes
    that shapes do not
    overlap (no crossings). It also assumes that nested shapes in {it:shpframe}
    have been tagged using {helpb geoframe##gen_plevel:geoframe generate plevel}
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
{p2col :{cmdab:feat:ure}}the type of features represented by the units in the frame;
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
{p2col :{cmdab:shp:frame}}the name of the linked shape frame; this is set
    automatically by the {cmd:shapeframe()} option of {helpb geoframe##create:geoframe create} or by
    {helpb geoframe##link:geoframe link}
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

{marker link}{...}
{dlgtab:geoframe link}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:link} {it:shpframe}

{pstd}
    registers a link between the current frame and {it:shpframe}. The current
    frame must be an attribute frame (one row per unit) and
    {it:shpframe} is typically a frame containing shape information on the units
    represented in the current frame. For each frame only one link to a
    {it:shpframe} can be registered; calling {cmd:geoframe link} will
    unregisters any existing link to another shape frame. {cmd:geoframe link}
    is an alternative to linking frames automatically when loading them
    using {cmd:geoframe create}.

{marker unlink}{...}
{dlgtab:geoframe unlink}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:unlink}

{pstd}
    unregisters the link to a shape frame in the current frame.

{marker attach}{...}
{dlgtab:geoframe attach}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:at:tach} {it:unitframe}
    [{varlist}] [{cmd:,} {opth ex:clude(varlist)} ]

{pstd}
    attaches {it:unitframe} to the current frame using an {cmd:m:1} merge. That is, a
    link to {it:unitframe} is established and all variables from {it:unitframe}
    that do not already exist in the current frame (except for the ID) are added
    as aliases to the current frame. {it:unitframe} must be an attribute frame
    (one row per unit) and the current frame is typically a frame containing
    shape information on the units represented in {it:unitframe}. After attaching
    {it:unitframe} to the current frame, the variables
    from {it:unitframe} will be available in the current frame like any other variables
    in the current frame.

{pstd}
    By default, all relevant variables from {it:unitframe} will be added as aliases
    to the current frame. Specify {varlist} to select the variables
    to be added; use option {cmd:exlcude()} to exclude variables from being added.

{pstd}
    {cmd:geoframe attach} requires Stata 18.

{marker detach}{...}
{dlgtab:geoframe detach}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:det:ach} {it:unitframe}

{pstd}
    detaches {it:unitframe} from the current frame. All alias variables related to
    {it:unitframe} will be removed and the link to {it:unitframe}
    will be deleted from the current frame.

{pstd}
    {cmd:geoframe detach} requires Stata 18.

{marker copy}{...}
{dlgtab:geoframe copy}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:copy} {it:unitframe}
    {varlist} [{cmd:,} {opth ex:clude(varlist)} {opt tar:get(namelist)} ]

{pstd}
    copies the specified variables from {it:unitframe} into the current
    frame. {it:unitframe} must be an attribute frame (one row per unit) and
    the current frame is typically a frame containing shape information on the
    units represented in {it:unitframe}. Use option {cmd:exlcude()} to exclude
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
    {stata geoframe create regions `url'Italy-RegionsData.dta, id(id) coord(xcoord ycoord) shpfile(`url'Italy-RegionsCoordinates.dta)}
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
    {browse "https://github.com/benjann/geoplot/"}.


{title:Also see}

{psee}
    Online:  help for
    {helpb geoplot}, {helpb frames}, {helpb spshape2dta}
