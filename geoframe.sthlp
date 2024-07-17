{smcl}
{* 17jul2024}{...}
{vieweralsosee "geoplot" "help geoplot"}{...}
{vieweralsosee "[D] frames" "help frames"}{...}
{vieweralsosee "[SP] spshape2dta" "help spshape2dta"}{...}
{viewerjumpto "Syntax" "geoframe##syntax"}{...}
{viewerjumpto "Description" "geoframe##description"}{...}
{viewerjumpto "Subcommands" "geoframe##subcommands"}{...}
{viewerjumpto "Examples" "geoframe##examples"}{...}
{viewerjumpto "Author" "geoframe##author"}{...}
{hi:help geoframe}{...}
{right:{browse "https://github.com/benjann/geoplot/"}}
{hline}

{title:Title}

{pstd}{hi:geoframe} {hline 2} Command to prepare data for {helpb geoplot}


{marker syntax}{...}
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
{synopt :{helpb geoframe##use:use}}load geoframe from disk
    {p_end}
{synopt :{helpb geoframe##save:save}}save geoframe to disk
    {p_end}
{synopt :{helpb geoframe##describe:{ul:d}escribe}}describe geoframe
    {p_end}
{synopt :{helpb geoframe##set:set}}update geoframe settings of current frame
    {p_end}
{synopt :{helpb geoframe##get:get}}retrieve geoframe settings from current frame
    {p_end}

{syntab :Manage shapes}
{p2col :{helpb geoframe##query:{ul:q}uery}}obtain information on units and shapes in geoframe
    {p_end}
{p2col :{helpb geoframe##generate:{ul:g}enerate}}generate helper variables related to shapes
    {p_end}
{p2col :{helpb geoframe##select:{ul:sel}ect}}select units and shapes
    {p_end}

{syntab :Manipulate shapes}
{p2col :{helpb geoframe##project:project}}apply projection
    {p_end}
{p2col :{helpb geoframe##rescale:rescale}}rescale coordinates
    {p_end}
{p2col :{helpb geoframe##clip:clip}}clip shapes using convex window
    {p_end}
{p2col :{helpb geoframe##rclip:rclip}}clip shapes using rectangular window
    {p_end}
{p2col :{helpb geoframe##simplify:simplify}}simplify (generalize) shapes
    {p_end}
{p2col :{helpb geoframe##refine:refine}}add extra points to straight lines
    {p_end}

{syntab :Generate shapes}
{p2col :{helpb geoframe##grid:grid}}store grid lines in new frame
    {p_end}
{p2col :{helpb geoframe##tissot:tissot}}store Tissot's indicatrices in new frame
    {p_end}
{p2col :{helpb geoframe##bbox:{ul:bb}ox}}store bounding box, enclosing circle, or convex hull in new frame
    {p_end}
{p2col :{helpb geoframe##bshare:bshare}}store shared borders in new frame
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

{syntab :Merge and append}
{p2col :{helpb geoframe##copy:copy}}copy and merge variables from other frame
    {p_end}
{p2col :{helpb geoframe##append:{ul:ap}pend}}append observations from other frame
    {p_end}
{p2col :{helpb geoframe##stack:stack}}combine multiple geoframes into one
    {p_end}

{syntab :Further utilities}
{p2col :{helpb geoframe##rename:{ul:ren}ame}}rename geoframe
    {p_end}
{p2col :{helpb geoframe##duplicate:{ul:dup}licate}}duplicate geoframe
    {p_end}
{p2col :{helpb geoframe##link:{ul:l}ink}}link shape frame to current frame
    {p_end}
{p2col :{helpb geoframe##relink:{ul:rel}ink}}fix linkage variable after modifying data
    {p_end}
{p2col :{helpb geoframe##unlink:{ul:unl}ink}}unlink shape frame from current frame
    {p_end}
{p2col :{helpb geoframe##clean:{ul:cl}ean}}delete unmatched/empty shapes and units
    {p_end}
{p2col :{helpb geoframe##attach:{ul:at}tach}}attach attribute frame to current frame using aliases (Stata 18 required)
    {p_end}
{p2col :{helpb geoframe##detach:{ul:det}ach}}detach attribute frame from current frame (Stata 18 required)
    {p_end}

{syntab :Convert source}
{p2col :{helpb geoframe##translate:{ul:tr}anslate}}translate shapefile source to Stata format
    {p_end}
{p2col :{helpb geoframe##translate:convert}}synonym for {cmd:translate}
    {p_end}
{p2col :{helpb geoframe##import:import}}translate and import shapefile source
    {p_end}
{synoptline}


{marker description}{...}
{title:Description}

{pstd}
    {cmd:geoframe} prepares data for use by {helpb geoplot}.

{pstd}
    Some of the functions used by {cmd:geoplot} and {helpb geoframe} are provided
    in Mata library lgeoplot.mlib. See {helpb lgeoplot_source} for source and
    minimal documentation of these functions.


{marker subcommands}{...}
{title:Subcommands}

{marker create}{...}
{dlgtab:geoframe create}

{pmore}
    {help geoframe##create1:Syntax 1: Create geoframe from Stata source}
    {p_end}
{pmore}
    {help geoframe##create2:Syntax 2: Create geoframe from ESRI or GeoJSON source}
    {p_end}
{pmore}
    {help geoframe##create3:Syntax 3: Declare current frame as geoframe}
    {p_end}

{marker create1}{...}
{pstd}
    {ul:Syntax 1: Create geoframe from Stata source}

{p 8 15 2}
    {cmd:geoframe} {cmdab:cr:eate} [{it:name}] [{cmd:using}] {it:{help filename}}
    [{cmd:,} {cmd:dta} {it:options} ]

{pstd}
    loads the data from {it:filename} into a new frame called
    {it:name}, where {it:filename} is a Stata dataset, and declares the
    created frame as a geoframe. If {it:name} is omitted, {it:basename}
    is used as the name of the created frame, where {it:basename} is the base
    name of {it:filename}. {it:filename} can be any valid Stata
    dataset, but typically it is an attribute file created by
    {helpb geoframe##translate:geoframe translate}
    (or, e.g., by command {helpb spshape2dta}). Options are as follows.

{pstd}
    {cmd:dta} enforces Syntax 1. Suffix {cmd:.dta} in {it:filename} implies
    {cmd:dta}.

{phang}
    {opt replace} allows replacing existing frames.

{phang}
    {opt nos:hp} deactivates automatic loading of a shape file. By default,
    if a file called {it:basename}{cmd:_shp.dta} is available in the
    same folder as {it:filename}, it will be loaded into a secondary frame
    called {it:name}{cmd:_shp} and the two frames will be linked. Type
    {cmd:noshp} to suppress this behavior. Data types
    {cmd:shape} and {cmd:pc} imply {cmd:noshp}; see option
    {helpb geoframe##create_type:type()}.

{phang}
    {opt s:hp(spec)} specifies a custom shape file to be loaded along with the main
    file. The syntax of {it:spec} is

{p 12 15 2}
    [[{it:shpname}] [{cmd:using}] {it:shpfilename} ] [{cmd:,} {it:suboptions} ]

{pmore}
    where {it:shpfilename} specifies the file and {it:shpname} provides a name for the
    shape frame. If {it:shpfilename} is specified without path, {cmd:geoframe}
    looks for the file in the same folder as the main file. If {it:shpname} is
    omitted, {it:name}{cmd:_shp} is used as the name for the shape
    frame. {it:suboptions} are {cmd:type()}, {cmd:id()}, {cmd:coordinates()},
    {cmd:centroids()}, {cmd:area()}, {cmd:sid()}, {cmd:pid()}, and
    {cmd:plevel()} as described below.

{phang}
    {opt nodrop} prevents dropping unmatched units and empty shapes
    when linking the attribute frame and the shape frame (i.e., do not apply
    {helpb geoframe##clean:geoframe clean}).

{phang}
    {opt nocl:ean} is a synonym for {cmd:nodrop}.

{phang}
    {opt nodes:cribe} suppresses the description of the frame that is displayed
    by default.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{marker create_type}{...}
{phang}
    {opt t:ype(type)} declares the type of data included in the
    frame. {it:type} may be {opt a:ttribute} (attribute data), {opt pc}
    (paired-coordinate data), or {opt s:hape} (shape data). If {cmd:type()} is omitted,
    {cmd:geoframe create} infers the type from context. To be precise, the rule is as
    follows.{p_end}
{phang2}(1) If option {cmd:type()} is provided, use the specified type;{p_end}
{phang2}(2) else if option {cmd:shp()} is provided, impose type {cmd:attribute};{p_end}
{phang2}(3) else if the type is already set in {it:filename}, use the set type;{p_end}
{phang2}(4) else if four names are provided in option {cmd:coordinates()}, impose type {cmd:pc};{p_end}
{phang2}(5) else if a file called {it:basename}{cmd:_shp.dta} is available in the same folder as {it:filemame}, impose type {cmd:attribute};{p_end}
{phang2}(6) else if a unit ID is available and the ID is nonunique, impose type {cmd:shape};{p_end}
{phang2}(7) else impose type {cmd:attribute}.

{phang}
    {opt f:eature(string)} declares the type of feature represented by the
    units in the frame. For example, type {cmd:feature(water)} if the frame
    contains data on lakes or rivers. {it:string} can be any text.

{phang}
    {opt id(varname)} specifies the name of the unit ID. Default is {cmd:_ID}.

{phang}
    {opt co:ordinates(X Y [X2 Y2])} specifies the names of the variables containing
    the coordinates. The default depends on data type. It is {cmd:_X _Y} for data type {cmd:shape},
    {cmd:_CX _CY} for data type {cmd:attribute}, and {cmd:_X1 _Y1 _X2 _Y2} for data type {cmd:pc}.

{phang}
    {opt cen:troids(CX CY)} specifies the names of the variables containing
    the centroids of the units. The default is {cmd:_CX _CY}
    for data types {cmd:shape} and {cmd:pc}; for data type {cmd:attribute}, {cmd:centroids()}
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

{marker create2}{...}
{pstd}
    {ul:Syntax 2: Create geoframe from ESRI or GeoJSON source}

{p 8 15 2}
    {cmd:geoframe} {cmdab:cr:eate} [{it:name}] [{cmd:using}] {it:source}
    [{cmd:,} {opt d:type(dtype)} {it:options} ]

{pstd}
    imports an ESRI or GeoJSON shapefile source into an attribute
    frame called {it:name} and a linked shape frame called
    {it:name}{cmd:_shp}. {it:source} identifies the source to be imported. It
    can be a filename with or without path (e.g. {cmd:shapefiles/source/world.shp}
    or {cmd:world.geojson}), or it
    can be a path without filename (e.g. {cmd:shapefiles/source/}), in which
    case the topmost source in the specified folder will be imported. {it:source}
    can also be a zip file, possibly including an internal path. For example, you can
    specify {cmd:shapefiles/source/world.zip} to translate the source contained
    in {cmd:world.zip}, {cmd:shapefiles/source/world.zip/50m/} to translate the
    source in folder {cmd:50m} within {cmd:world.zip}, or
    {cmd:shapefiles/source/world.zip/50m/lakes.shp} to translate {cmd:lakes.shp}
    in folder {cmd:50m} within {cmd:world.zip}. Options are as follows.

{phang}
    {opt dtype(dtype)} identifies the format of the source. {it:dtype} may be
    {cmd:esri} (ESRI shapefile format), {cmd:shp} (synonym for {cmd:esri}),
    {cmd:json} (GeoJSON format), or {cmdab:geoj:son} (synonym for
    {cmd:json}). Suffix {cmd:.geojson} or {cmd:.json} in {it:source} implies
    {cmd:dtype(geojson)}; else {cmd:dtype(esri)} is assumed.

{phang}
    {cmd:allstring}, {cmd:gtype()}, {cmd:addmissing}, and {cmd:nodots} as
    as described for {helpb geoframe##tr_json:geoframe translate json}, if the source
    is in GeoJSON format.

{phang}
    {cmd:replace}, {cmd:nodrop}, {cmd:noclean}, {cmd:nodescribe}, {cmd:current}, and
    {cmd:feature()} as described for {help geoframe##create1:Syntax 1}.

{marker create3}{...}
{pstd}
    {ul:Syntax 3: Declare current frame as geoframe}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:cr:eate}
    [{cmd:,} {it:options} ]

{pstd}
    declares the current frame as a geoframe. {it:options} are as follows.

{phang}
    {opt nos:hp} deactivates automatic linking of a shape frame. By default,
    if a frame called {it:frame}{cmd:_shp} is in memory, it will be treated as
    a shape frame to be linked to the current frame. Type
    {cmd:noshp} to suppress this behavior. Data types
    {cmd:shape} and {cmd:pc} imply {cmd:noshp}; see option
    {helpb geoframe##create_type:type()}.

{phang}
    {opt s:hp(spec)} specifies a custom shape frame to be linked to the current
    frame. The syntax of {it:spec} is

{p 12 15 2}
    [{it:shpname}] [{cmd:,} {it:suboptions} ]

{pmore}
    where {it:shpname} is the name of the shape frame. The default for {it:shpname}
    is {it:frame}{cmd:_shp}. {it:suboptions} are {cmd:type()},
    {cmd:id()}, {cmd:coordinates()}, {cmd:centroids()},
    {cmd:area()}, {cmd:sid()}, {cmd:pid()}, and {cmd:plevel()}
    as described for {help geoframe##create1:Syntax 1}.

{phang}
    {cmd:nodrop}, {cmd:noclean}, {cmd:nodescribe}, {cmd:type()},
    {cmd:feature()}, {cmd:id()}, {cmd:coordinates()}, {cmd:centroids()},
    {cmd:area()}, {cmd:sid()}, {cmd:pid()}, and {cmd:plevel()}
    as described for {help geoframe##create1:Syntax 1}.

{marker use}{...}
{dlgtab:geoframe use}

{p 8 15 2}
    {cmd:geoframe} {cmd:use} [{it:name}] [{cmd:using}] {it:{help filename}}
    [{cmd:,} {it:options} ]

{pstd}
    loads the data from {it:filename} into a new frame called
    {it:name}, where {it:filename} is a Stata dataset that has been saved by
    {helpb geoframe##save:geoframe save}. If {it:name} is omitted, {it:basename}
    is used as the name of the created frame, where {it:basename} is the base
    name of {it:filename}. {it:options} are as follows.

{phang}
    {opt replace} allows replacing existing frames.

{phang}
    {opt nos:hp} deactivates automatic loading of the shape file. By default,
    if {it:filename} is an attribute file and a file
    called {it:basename}{cmd:_shp.dta} is available in the same folder,
    it will be loaded into a secondary frame called {it:name}{cmd:_shp} and the
    two frames will be linked. Type {cmd:noshp} to load the attribute file only.

{phang}
    {opt nodes:cribe} suppresses the description of the frame that is displayed
    by default.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{pstd}
    {cmd:geoframe use} is implemented as a wrapper for
    {helpb geoframe##create:geoframe create}. Typing
    {cmd:geoframe use} {it:filename} is equivalent to
    {cmd:geoframe create} {it:filename}{cmd:, dta noclean}.

{marker save}{...}
{dlgtab:geoframe save}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}]
    {cmd:geoframe} {cmd:save} [{it:{help filename}}] {ifin} [{cmd:,} {it:options} ]

{pstd}
    saves the current frame on disk under the name {it:{help filename}} (with
    extension {cmd:.dta}). If {it:filename} is omitted, {it:frame}{cmd:.dta} is
    used, where {it:frame} is the name of the current frame. Use the {it:if} and
    {it:in} qualifiers to
    restrict the observations of the current frame to be saved (and include only
    the shapes of the selected units in the shape file). {it:options} are as follows.

{phang}
    {opt replace} allows replacing existing files.

{phang}
    {opt nos:hp} deactivates automatic saving of the shape frame. By default,
    if the current frame is linked to a shape frame, the shape frame is stored
    on disk under the same name with extension {cmd:_shp.dta}. Type {cmd:noshp} to store
    the attribute frame only.

{phang}
    {opth if:shp(exp)} specifies an {it:if} condition to restrict
    the observations of the shape frame to be saved (and, in the main file, include
    only units with a nonzero selection in the shape frame). {cmd:ifshp()} is
    not allowed together with {cmd:noshp}.

{marker describe}{...}
{dlgtab:geoframe describe}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:d:escribe}

{pstd}
    displays the geoframe settings of the current frame. Alternatively, type

{p 8 15 2}
    {cmd:geoframe} {cmdab:d:escribe} {it:frame}

{pstd}
    to display the settings of frame {it:frame}.

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
    {cmd:attribute} (attribute data), {cmd:pc} (paired-coordinates data),
    or {cmd:shape} (shape data)
    {p_end}
{p2col :{cmdab:f:eature}}the type of features represented by the units in the frame;
    {it:value} can be any text
    {p_end}
{p2col :{cmd:id}}the name of the variable containing the unit ID; {it:value} is a single
    variable name; default is {cmd:_ID}
    {p_end}
{p2col :{cmdab:co:ordinates}}the names of the variables containing the coordinates; {it:value} is a list of
    two (for data types {cmd:attribute} and {cmd:shape}) or four (for data type {cmd:pc}) variable names; default is
    {cmd:_X _Y} for data type {cmd:shape}, {cmd:_CX _CY} for data type {cmd:attribute},
    and {cmd:_X1 _Y1 _X2 _Y2} for data type {cmd:pc}
    {p_end}
{p2col :{cmdab:cen:troids}}the names of the variables containing the centroids; {it:value} is a list of
    two variable names; default is {cmd:_CX _CY} for data types {cmd:shape} and {cmd:pc}; for data type
    {cmd:attribute}, {cmd:centroids} is a synonym for {cmd:coordinates}
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
    Option {opt local()} stores the value of the setting in a {help macro:local macro} called
    {it:lname} instead of displaying it.

{marker query}{...}
{dlgtab:geoframe query}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:q:uery} [{it:function}] [...]

{pstd}
    provides functions to obtain information about the units and shapes in the
    current frame. Available functions are:

{p2colset 9 22 24 2}{...}
{p2col : {helpb geoframe##q_n:n}}number of units and shape items; the default
    {p_end}
{p2col : {helpb geoframe##q_gtype:{ul:item}s}}number of shape items by geometry type
    {p_end}
{p2col : {helpb geoframe##q_gtype:{ul:gt}ype}}synonmy for {cmd:items}
    {p_end}
{p2col : {helpb geoframe##q_dir:{ul:dir}ection}}orientation of shape items
    {p_end}
{p2col : {helpb geoframe##q_dir:{ul:or}ientation}}synonym for {cmd:direction}
    {p_end}
{p2col : {helpb geoframe##q_bbox:{ul:bb}ox}}bounding box of shapes
    {p_end}

{marker q_n}{...}
{pstd}{ul:Number of units and shapes}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {opt q:uery} [{opt n}] {ifin}

{pstd}
    obtains information about the number of selected units and corresponding
    shape items. The following scalars are returned in {cmd:r()}.

{p2colset 9 22 22 2}{...}
{p2col : {cmd:r(units)}}number of units
    {p_end}
{p2col : {cmd:r(items)}}number of shape items
    {p_end}
{p2col : {cmd:r(min)}}minimum number of shape items per unit
    {p_end}
{p2col : {cmd:r(avg)}}average number of shape items per unit
    {p_end}
{p2col : {cmd:r(max)}}maximum number of shape items per unit
    {p_end}

{marker q_gtype}{...}
{pstd}{ul:Number of shape items by geometry type}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {opt q:uery} {opt item:s} {ifin}

{pstd}
    obtains information about the number of shape items by type of geometry
    for the selected units. The following scalars are returned in {cmd:r()}.

{p2colset 9 24 26 2}{...}
{p2col : {cmd:r(items)}}total number of shape items
    {p_end}
{p2col : {cmd:r(Polygon)}}number of polygon items (four or more points and first
    point equal to last point)
    {p_end}
{p2col : {cmd:r(LineString)}}number of line items (four or more points and
    first point unequal last point, two points, or three points)
    {p_end}
{p2col : {cmd:r(Point)}}number of point items (one point)
    {p_end}
{p2col : {cmd:r(Empty)}}number of empty items (missing)
    {p_end}

{pstd}
    {cmd:geoframe query gtype} is a synonym for {cmd:geoframe query items}.

{marker q_dir}{...}
{pstd}{ul:Orientation of shape items}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {opt q:uery} {opt dir:ection} {ifin}

{pstd}
    obtains information about the orientation of the shape items of the
    selected units. The following scalars are returned in {cmd:r()}.

{p2colset 9 22 24 2}{...}
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
    Lines are trated like polygons. {cmd:geoframe query orientation} is a
    synonym for {cmd:geoframe query direction}.

{marker q_bbox}{...}
{pstd}{ul:Bounding box of shapes}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {opt q:uery} {opt bb:ox} {ifin} [{cmd:,} {it:options} ]

{pstd}
    obtains the bounding box of the selected shapes and returns
    it in {cmd:r()}. {it:options} are {cmd:pading()}, {cmd:rotate}, {cmd:hull}, {cmd:circle},
    {cmd:n()}, {cmd:angle()}, {cmd:noadjust}, and
    {cmd:noshp} as described in {helpb geoframe##bbox:geoframe bbox}. The following
    results are returned.

{pmore} Scalars:

{p2colset 9 22 24 2}{...}
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

{marker generate}{...}
{dlgtab:geoframe generate}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:g:enerate} {it:function} {it:...}

{pstd}
    provides functions to generate specific variables in the current frame or in a
    linked shape frame. Available functions are as follows.

{p2colset 9 22 24 2}{...}
{p2col : {helpb geoframe##g_centroids:{ul:cen}troids}}generate centroid variables in current frame
    {p_end}
{p2col : {helpb geoframe##g_area:area}}generate shape size variable in current frame
    {p_end}
{p2col : {helpb geoframe##g_pid:pid}}generate within-unit polygon ID (shape item ID) in shape frame
    {p_end}
{p2col : {helpb geoframe##g_gtype:{ul:gt}ype}}generate geometry-type indicator for shape items in shape frame
    {p_end}
{p2col : {helpb geoframe##g_dir:{ul:dir}ection}}generate direction indicator for shape items in shape frame
    {p_end}
{p2col : {helpb geoframe##g_dir:{ul:or}ientation}}synonym for {cmd:geoframe generate direction}
    {p_end}
{p2col : {helpb geoframe##g_plevel:{ul:pl}evel}}generate enclave/exclaves identifier in shape frame
    {p_end}
{p2col : {helpb geoframe##g_shpmatch:{ul:shp}match}}generate indicator in current frame
    for match of unit in the linked shape frame
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
    type {cmd:scale(100)} to obtain the area in hectares, or type {cmd:scale(1000)}
    to obtain the area in square kilometers. Option {cmd:replace} allows overwriting
    an existing variable. The created variable will be registered using
    {helpb geoframe##set:geoframe set area} unless option {cmd:noset} is
    specified.

{marker g_pid}{...}
{pstd}{ul:Shape item ID}

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmd:pid} [{it:PID}] [{cmd:,} {opt replace} {opt noset} ]

{pstd}
    generates a variable identifying the different polygons or other shape items
    within each unit. If the current frame is linked to a shape frame,
    {cmd:geoframe generate pid} will be applied to the linked shape frame. {it:PID}
    specifies a name for the generated variable; {cmd:_PID} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. The created variable will be registered in the shape frame using
    {helpb geoframe##set:geoframe set pid} unless option {cmd:noset} is
    specified.

{marker g_gtype}{...}
{pstd}{ul:Geometry types of shape items}

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:gt:ype} [{it:GTYPE}] [{cmd:,} {opt replace} {opt nolab:el} ]

{pstd}
    generates a variable identifying the geometry type of each shape
    item: 1 = Point, 2 = LineString, 3 = Polygon, 0 = Empty (item contains missing
    only). If the current frame is linked to a shape frame,
    {cmd:geoframe generate gtype} will be applied to the linked shape
    frame. {it:GTYPE} specifies a name for the generated variable; {cmd:_GTYPE} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. Option {cmd:nolabel} omits creating and assigning value labels.

{marker g_dir}{...}
{pstd}{ul:Orientation of shape items}

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:dir:ection} [{it:DIR}] [{cmd:,} {opt replace} {opt nolab:el} ]

{pstd}
    generates a variable identifying the orientation of each shape
    item: 1 = positive (counterclockwise), -1 = negative (clockwise), 0 = undetermined
    (item has less than 3 unique points). If the current frame is linked to a shape frame,
    {cmd:geoframe generate direction} will be applied to the linked shape
    frame. {it:DIR} specifies a name for the generated variable; {cmd:_DIR} is used as default
    variable name. Option {cmd:replace} allows overwriting an existing
    variable. Option {cmd:nolabel} omits creating and assigning value labels.

{pstd}
    {cmd:geoframe generate orientation} is a synonym for {cmd:geoframe generate direction}.

{marker g_plevel}{...}
{pstd}{ul:Enclaves and exclaves}

{marker gen_plevel}{...}
{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmdab:pl:evel} [{it:PLEVEL}] {ifin}
    [{cmd:,} {cmd:by(}{help varname:{it:byvar}}{cmd:)} {opt replace} {opt noset}
    {opt nodot:s} ]

{pstd}
    generates (or updates) a variable identifying the plot order of the shape
    items. If the current frame is linked to a shape frame,
    {cmd:geoframe generate plevel} will be applied to the linked shape
    frame.

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
    By default, {cmd:geoframe generate plevel} will do a search for nested shape items
    across all units in the data. Use option {cmd:by()} to stratify the search, for example,
    if the data contains separate regions without overlap or if the data contains units
    from different aggregation levels that should not be compared. If {cmd:by()}
    is specified, a separate search will be done within each group of units defined by the
    levels of {it:byvar}.

{pstd}
    {cmd:geoframe generate plevel} is useful to identify enclaves and exclaves
    so that they do not get covered when plotting filled areas. The generated
    variable will be set to 0 for items that are neither enclaves nor exclaves,
    1 for enclaves, 2 for exclaves, 3 for enclaves within exclaves, 4 for exclaves
    within enclaves, etc. {helpb geoplot} will then print the items in this
    order. The algorithm used by {cmd:geoframe generate plevel} assumes
    that items do not overlap (i.e. no crossings). It also assumes that
    enclaves are explicitly included in the data (that is, if unit A has
    an exclave in unit B, then unit B must contain a corresponding enclave
    item). Results will be invalid if these assumptions are not met.

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

{marker select}{...}
{dlgtab:geoframe select}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:sel:ect} {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    selects the observations satisfying the {it:if} and {it:in} qualifiers in
    the current frame. By default, if the current frame is linked to a
    shape frame, a corresponding selection will also be applied to
    the shape frame and the linkage between the frames will be
    updated. {it:options} are as follows.

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
    only even if the current frame is linked to a shape frame. That is,
    the linked shape frame will remain unchanged (apart from updating the
    linkage). {opt noshp} also implies that {cmd:into()} will make no copy of
    the shape frame.

{phang}
    {opt unl:ink} does not establish a link to a shape frame in the modified
    frame even if the current frame is linked to a shape
    frame. {cmd:unlink} implies {cmd:noshp}.

{phang}
    {opt into(newname [newshpname])} copies the selection in a new frame
    called {it:newname} and leaves the current frame unchanged. If the current
    frame is linked to a shape frame, {cmd:into()} also creates a new shape
    frame and leaves the original shape frame unchanged. The
    default name for the new shape frame is {it:newname}{cmd:_shp}; specify
    {it:newshpname} to provide an alternative name.

{phang}
    {opt replace} allows {cmd:into()} to overwrite existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{marker project}{...}
{dlgtab:geoframe project}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:project} [{it:projection} [{it:args}]] {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    transforms the coordinates of the selected shapes by applying the specified
    projection. If the current frame is linked to a shape frame, the coordinates
    in both the current frame and the shape frame will be translated. For a general
    introduction to map projections
    see {browse "https://en.wikipedia.org/wiki/Map_projection":en.wikipedia.org/wiki/Map_projection};
    for an extensive list of projections see
    {browse "https://en.wikipedia.org/wiki/List_of_map_projections":en.wikipedia.org/wiki/List_of_map_projections}).

{pstd}
    The following projections are currently supported by {cmd:geoframe project}. Input
    coordinates are assumed to represent latitude (Y) and
    longitude (X) in degrees (typically WGS84); Y values outside +/- 90 will
    clipped before applying the projection (unless noted otherwise). The names
    of the projections can be abbreviated and typed in lower- or uppercase
    letters (if abbreviation is ambiguous, the first match in the list will be
    used).

{p2colset 11 41 43 2}{...}
{pmore}Cylindrical{p_end}
{p2col : {opt webmercator} [{it:zoom} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Web_Mercator_projection":Web Mercator}
    projection (the default), where
    {it:zoom} sets the zoom level (default is {cmd:0}, resulting in an equator
    length of 256 pixels; set the zoom level to {cmd:1} for 512 pixels, {cmd:2}
    for 1024 pixels, etc.);
    Y values outside +/- 85.051129 will be clipped before applying the
    projection
    {p_end}
{p2col : {opt mercator} [{it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Mercator_projection":Mercator}
    projection (spherical earth model)
    {p_end}
{p2col : {opt emercator} [{it:r} {it:x0} {it:m}]}{browse "https://en.wikipedia.org/wiki/Mercator_projection":Mercator}
    projection (ellipsoid earth model), where
    {it:m} sets the inverse flattening parameter (default is {cmd:298.257223563},
    corresponding to {browse "https://en.wikipedia.org/wiki/World_Geodetic_System":WGS 84})
    {p_end}
{p2col :{opt miller} [{it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Miller_cylindrical_projection":Miller cylindrical}
    projection
    {p_end}
{p2col :{opt gallstereo} [{it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Gall_stereographic_projection":Gall stereographic}
    projection
    {p_end}
{p2col :{opt lambert} [{it:r} {it:x0} {it:S}]}{browse "https://en.wikipedia.org/wiki/Lambert_cylindrical_equal-area_projection":Lambert cylindrical equal-area}
    projection, where {it:S} sets the stretch factor
    (default is {cmd:1}; see
    {browse "https://en.wikipedia.org/wiki/Cylindrical_equal-area_projection":en.wikipedia.org/wiki/Cylindrical_equal-area_projection})
    {p_end}
{p2col :{opt behrmann} [{it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Behrmann_projection":Behrmann}
    projection; equal to {cmd:lambert} with {it:S} = 0.75
    {p_end}
{p2col :{opt hobodyer} [{it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Hobo%E2%80%93Dyer_projection":Hobo–Dyer}
    projection; equal to {cmd:lambert} with {it:S} = cos(37.5 * _pi / 180)^2
    {p_end}
{p2col :{opt gallpeters} [{it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Gall%E2%80%93Peters_projection":Gall–Peters}
    projection; equal to {cmd:lambert} with {it:S} = 0.5
    {p_end}
{p2col :{opt peters} [{it:r} {it:x0}]}synonym for {cmd:gallpeters}
    {p_end}
{p2col :{opt equirectangular} [{it:y1} {it:r} {it:x0} {it:y0}]}{browse "https://en.wikipedia.org/wiki/Equirectangular_projection":Equirectangular}
    (equidistant cylindrical) projection; no clipping will be applied
    {p_end}

{pmore}Pseudocylindrical{p_end}
{p2col :{opt robinson} [{it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Robinson_projection":Robinson}
    projection
    {p_end}
{p2col :{opt equalearth} [{it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Equal_Earth_projection":Equal Earth}
    projection
    {p_end}
{p2col :{opt naturalearth} [{it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Natural_Earth_projection":Natural Earth}
    projection
    {p_end}

{pmore}Pseudoazimuthal{p_end}
{p2col :{opt winkeltripel} [{it:y1} {it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Winkel_tripel_projection":Winkel tripel}
    projection; default for {it:y1} is {cmd:180/_pi*acos(2/_pi)}
    {p_end}
{p2col :{opt hammer} [{it:r} {it:x0}]}{browse "https://en.wikipedia.org/wiki/Hammer_projection":Hammer}
    projection
    {p_end}

{pmore}Conic{p_end}
{p2col :{opt conic} [{it:y1} {it:y2} {it:r} {it:x0} {it:y0}]}{browse "https://en.wikipedia.org/wiki/Equidistant_conic_projection":Equidistant conic}
    (simple conic) projection
    {p_end}
{p2col :{opt albers} [{it:y1} {it:y2} {it:r} {it:x0} {it:y0}]}{browse "https://en.wikipedia.org/wiki/Albers_projection":Albers equal-area conic}
    projection
    {p_end}
{p2col :{opt lambertconic} [{it:y1} {it:y2} {it:r} {it:x0} {it:y0}]}{browse "https://en.wikipedia.org/wiki/Lambert_conformal_conic_projection":Lambert conformal conic}
    projection
    {p_end}

{pmore}Azimuthal{p_end}
{p2col :{opt orthographic} [{it:r} {it:x0} {it:y0}]}{browse "https://en.wikipedia.org/wiki/Orthographic_map_projection":Orthographic}
    projection; Y values outside {it:y0} +/- 90 and X values outside {it:x0} +/- 90 will
    be clipped before applying the projection (note that such clipping of individual
    coordinates can lead to artifacts at the edges of the displayed hemisphere; preferably,
    use {helpb geoframe##rclip:geoframe rclip} to clip the shapes to the relevant
    range before applying {cmd:geoframe project orthographic})
    {p_end}

{pmore}Legend:{p_end}
{p2colset 11 16 18 2}{...}
{p2col :{it:y1}}the (first) standard parallel (in degrees); default is {cmd:0} (unless noted otherwise){p_end}
{p2col :{it:y2}}the second standard parallel (in degrees); default is {cmd:60}{p_end}
{p2col :{it:r}}the scale of the projection (earth radius); default is {cmd:1}{p_end}
{p2col :{it:x0}}the central meridian (in degrees); default is {cmd:0}{p_end}
{p2col :{it:y0}}the central parallel (in degrees); default is {cmd:0}{p_end}

{pstd}
    {it:options} are as follows.

{phang}
    {opt rad:ian} indicates that the input coordinates (and arguments {it:x0},
    {it:y0}, {it:y1}, {it:y1}) are in radians, not in degrees.

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
    {opt nos:hp} applies the projection to the current frame only even if the
    current frame is linked to a shape frame.

{phang}
    {opt into(newname [newshpname])} copies the transformed data into a new frame
    called {it:newname} and leaves the current frame unchanged. If the current
    frame has a linked shape frame, {cmd:into()} also creates a new shape
    frame and leaves the original shape frame unchanged. The
    default name for the new shape frame is {it:newname}{cmd:_shp}; specify
    {it:newshpname} to provide an alternative name.

{phang}
    {opt replace} allows {cmd:into()} to overwrite existing frames.

{phang}
    {opt fast} does not restore the original data should the user press
    {cmd:Break}. {cmd:fast} is intended for use by programmers. {cmd:fast} has
    no effect if {cmd:into()} is specified.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{marker rescale}{...}
{dlgtab:geoframe rescale}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:rescale}
    [{help exp:{it:sx}} [{help exp:{it:sy}}]] {ifin}
    [{cmd:,} {cmd:x0(}{help exp:{it:x0}}{cmd:)}
             {cmd:y0(}{help exp:{it:y0}}{cmd:)} {it:options} ]

{pstd}
    transforms the coordinates of the selected shapes as follows:

        {it:X'} = ({it:X} - {it:x0}) * {it:sx}
        {it:Y'} = ({it:Y} - {it:y0}) * {it:sy}

{pstd}
    The default for {it:sx} is {cmd:1}; {it:sy} is set to {it:sx} if omitted;
    the default for {it:x0} and {it:y0} is {cmd:0}. If the current frame is
    linked to a shape frame, the coordinates in both the current frame and
    the shape frame will be transformed. {it:options} are {cmd:xy()},
    {cmd:ifshp()}, {cmd:noshp}, {cmd:into()}, {cmd:replace}, {cmd:fast},
    and {cmd:current} as described for {helpb geoframe##project:geoframe project}.

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
    polygon (or a set of polygons). Specify {cmd:line}, to clip a polygon in
    such a way that the result is a line (or a set of lines), unless the
    polygon lies entirely within the clipping window. {cmd:line} has no effect
    if {cmd:noclip} is specified.

{phang}
    {opt nocl:ip} applies selection rather than clipping. Shape items that
    are at least partially inside the clipping window will be selected.

{phang}
    {opt st:rict} changes the behavior of {cmd:noclip}. By default, {cmd:noclip}
    selects shape items that are at least partially inside the clipping
    window. Specify {cmd:strict} to restrict the selection
    to items that are entirely within the window.

{phang}
    {opt nosp:lit} changes the behavior of {cmd:noclip}. By default, {cmd:noclip}
    selects or excludes each shape item individually. Specify {cmd:nosplit} to treat shape
    items that belong to the same unit as a
    group of items to be included or excluded together.

{phang}
    {opt nodrop} retains empty-shape units in the data. The default is to keep only
    those units that remain non-empty after the modification.

{phang}
    {opt nodot:s} suppresses the progress dots that are displayed by default.

{phang}
    {opth if:shp(exp)} specifies an {it:if} condition to be applied to
    the linked shape frame. {cmd:ifshp()} is only allowed
    if the current frame is linked to a shape frame.

{phang}
    {opt into(newname [newshpname])} copies the processed shapes
    into a frame called {it:newname} (and possibly a linked shape
    frame called {it:newshpname}) and leaves the current frame (and its linked
    shape frame) unchanged. The default for {it:newshpname} is
    {it:newname}{cmd:_shp}. Only the selected shapes will be copied.

{phang}
    {opt replace} allows {cmd:into()} to overwrite existing frames.

{phang}
    {opt fast} does not restore the original data should the user press
    {cmd:Break}. {cmd:fast} is intended for use by programmers. {cmd:fast} has
    no effect if {cmd:into()} is specified.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{marker rclip}{...}
{dlgtab:geoframe rclip}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:rclip} {it:limits} {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    clips the selected shapes by a rectangular window, where {it:limits}
    specifies the boundaries of the window. {it:limits} is a {it:{help numlist}}
    providing the minimum and maximum of X and Y in the following order:

            {it:xmin} {it:xmax} {it:ymin} {it:ymax}

{pstd}
    Element in {it:limits} that are omitted or specified as {cmd:.} (missing) are
    treated as (minus) infinity. For example, you could type
    {bind:{cmd:geoframe rclip . .} {it:ymin}} to clip from below.

{pstd}
    Alternatively, specify {it:limits} as {it:matname} where
    {it:matname} is the name of a {helpb matrix} (row or column vector)
    containing the limits (in the same order as above). For example, you
    could type {bind:{cmd:geoframe rclip r(limits)}} to apply clipping by the
    (limits of the) bounding box returned by
    {helpb geoframe##q_bbox:geoframe query bbox}.

{pstd} {it:options} are as described for {helpb geoframe##clip:geoframe clip}. Rectangular
    clipping is a special case of convex clipping.

{marker simplify}{...}
{dlgtab:geoframe simplify}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:simplify} [{it:delta}] {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    simplifies (generalizes) the selected shapes (using a Visvalingam–Whyatt
    algorithm), where {it:delta} specifies the degree of simplification; see option
    {cmd:absolute} below. The larger {it:delta}, the stronger the
    simplification. The default value for {it:delta} is one. {it:options} are as follows.

{phang}
    {opt joint:ly} causes the selected shapes to be simplified
    jointly, taking into account shared borders. By default, each
    shape item is simplified individually, which may lead to inconsistencies
    in shared borders. Such inconsistencies are visually insignificant with moderate
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
    [{ul:{cmd:no}}]{cmdab:ref:ine}[{cmd:(}[{it:delta}] [{cmd:,} {opt abs:olute}]{cmd:)}]
    omits refinement or sets the degree of refinement. The simplification algorithm removes
    points on long straight lines, which may lead to undesired artifacts
    when applying projections. Therefore, by default, {cmd:geoframe simplify} calls
    {cmd:geoframe refine} after simplifying the shapes so that extra points
    are added back in on long edges. Use option {cmd:refine()} to determine the degree of
    refinement, with argument {it:delta} and suboption {cmd:absolute}
    as described for {helpb geoframe##refine:geoframe refine}. Type {cmd:norefine}
    to omit refinement after simplification.

{phang}
    {cmd:nodrop}, {cmd:nodots}, {cmd:ifshp()}, {cmd:into()}, {cmd:replace},
    {cmd:fast}, and {cmd:current} as described for {helpb geoframe##clip:geoframe clip}.

{pstd}
    Note that {cmd:geoframe simplify} drops all point items
    (unless the threshold is zero). Line items that are simplified to two points will
    be dropped unless their length is at least 2*sqrt(2*{it:d}), where {it:d} is the
    threshold. Polygon items that are simplified to two points will be dropped.

{pstd}
    {cmd:geoframe simplify} cannot be used with paired-coordinates data.

{marker refine}{...}
{dlgtab:geoframe refine}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:refine} [{it:delta}] {ifin}
    [{cmd:,} {it:options} ]

{pstd}
    refines the selected shapes by adding extra points within edges that are
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
    will be divided. The base threshold is determined in such a way that a diagonal
    across the map has at least 200 points. Specify, for example,
    {cmd:geoframe refine 2} to use a threshold that is half the size of the
    default threshold (at least 400 points on the diagonal). Alternatively, specify
    {it:delta} with option {cmd:absolute} if you want to provide a custom
    threshold rather than a divisor for the default threshold. {cmd:absolute}
    only has an effect if {it:delta} is specified.

{phang}
    {cmd:nodots}, {cmd:ifshp()}, {cmd:into()}, {cmd:replace},
    {cmd:fast}, and {cmd:current} as described for {helpb geoframe##clip:geoframe clip}.

{pstd}
    {cmd:geoframe refine} cannot be used with paired-coordinates data.

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
    frame may be a shape frame or an attribute frame that is linked to a
    shape frame. {it:options} are as follows.

{phang}
    {cmd:x(}{cmd:#}{it:#}{cmd:)} or {opth x(numlist)} determines the
    positions of the vertical grid lines. Use {cmd:x(}{cmd:#}{it:#}{cmd:)},
    where {it:#} is the desired number of lines, to
    determine the positions automatically based on the range of the coordinates of
    the selected shapes. Use {opth x(numlist)} to specify custom positions. The default
    depends on option {cmd:y()}; if {cmd:x()} is omitted, the number of
    vertical lines is determined in a way such that the spacing between vertical lines
    is similar to the spacing between horizontal lines. Specify {cmd:x(#0)}
    to omit vertical lines.

{phang}
    {cmd:y(}{cmd:#}{it:#}{cmd:)} or {opth y(numlist)} determines the
    positions of the horizontal grid lines. Use {cmd:y(}{cmd:#}{it:#}{cmd:)},
    where {it:#} is the desired number of lines, to
    determine the positions automatically based on the range of the coordinates of
    the selected shapes. Use {opth y(numlist)} to specify custom positions. Default
    is {cmd:y(#7)} (unless {cmd:x()} is specified, in which case the default number of
    horizontal lines is determined in a way such that the spacing between horizontal lines
    is similar to the spacing between vertical lines). Specify {cmd:y(#0)}
    to omit horizontal lines.

{phang}
    {opt tight} omits rounding of the position of the grid lines. This is only
    relevant for positions that are determined automatically, not for custom
    positions. By default, {cmd:geoframe grid} determines the range and spacing
    of the grid lines in a way such that each grid line is placed at a nice (rounded)
    value. This implies that the grid will typically be somewhat larger than the
    range of the data. Type {cmd:tight} to omit the rounding and generate a grid
    that tightly fits the data.

{phang}
    {opt pad:ding(marginexp)} adds padding to the range of the grid. This is
    only relevant for positions that are determined automatically, not for
    custom positions. The values in {it:marginexp} are in percent of the size
    of the map in each direction. For example, type {cmd:padding(5)} to
    increase the range of the grid by 5% on each side; type {cmd:padding(-5)}
    decrease by 5%. Provide multiple values in {it:marginexp} to use different
    padding on the left, right, bottom, and top (in this order; values will be
    recycled). Alternatively, specify one or more elements of the form
    {{cmd:l}|{cmd:r}|{cmd:b}|{cmd:t}} [{cmd:=}] {it:#}, such as
    {cmd:padding(l=5)} (add 5% padding on the left; no padding on other sides)
    or {cmd:padding(r=5 t=10)} (add 5% padding on the right and 10% padding at
    the top; no padding on other sides). {cmd:padding()} implies {cmd:tight}.

{phang}
    {opt rad:ian} indicates that coordinates are in radians. This is only
    relevant for positions that are determined automatically, not for custom
    positions. If {cmd:radian} is specified, rounding is performed on a
    transformed scale such that the grid lines are placed at nice values in
    degrees. {cmd:radian} has no effect if {cmd:tight} is specified.

{phang}
    {cmdab:noex:tend} omits extending the length of lines to cover the data
    range (including padding; negative padding will reduce the length of the
    lines). If {cmd:noextend} is specified, the lines will
    only be drawn within the limits of the requested grid.

{phang}
    {opt mesh} omits the first and last line on each axis (if there are 3 or more
    lines).

{phang}
    {opt n(n)} sets the number of points used to construct a line. The
    default is {cmd:n(100)}.

{phang}
    {opt nos:hp} uses information on coordinates from the current frame even
    if the current frame is linked to a shape frame.

{phang}
    {opt replace} allows overwriting existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{marker tissot}{...}
{dlgtab:geoframe tissot}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:tissot} {it:newname} [{it:newshpname}]
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    generates Tissot's indicatrices covering the range of the selected shapes
    (whose coordinates are assumed to be unprojected and in
    degrees) and stores
    them in a new frame called {it:newname} and an associated shape frame
    called {it:newshpname}; {it:newname}{cmd:_shp} is
    used as name for the shape frame if {it:newshpname} is omitted. The current
    frame may be a shape frame or an attribute frame that is linked to a
    shape frame. {it:options} are as follows.

{phang}
    {opt r(#)}, with #>0, to set a custom radius for the indicatrices. The default
    radius depends on the spacing of the indicatrices.

{phang}
    {cmd:x()}, {cmd:y()}, {cmd:tight}, {cmd:padding()}, {cmd:n()},
    {cmd:noshp}, {cmd:replace}, and {cmd:current} as described for
    {helpb geoframe##grid:geoframe grid}, with the following differences. Default
    for {cmd:y()} is {cmd:#5} rather than {cmd:#7}; {cmd:y(#0)} and
    {cmd:y(#0)} are not allowed; the values in {cmd:y(}{it:numlist}{cmd:)} must
    be in [-90,90] or, if {cmd:radian} has been specified, in [-pi/2,pi/2];
    option {cmd:radian}, apart of affecting automatic positioning, makes sure
    that the indicatrices are generated in a way that is consistent with data
    in radians.

{pstd}
    Tissot's indicatrices are used to illustrate characteristics of
    projections; see
    {browse "https://en.wikipedia.org/wiki/Tissot%27s_indicatrix":en.wikipedia.org/wiki/Tissot's_indicatrix}.

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
    frame may be a shape frame or an attribute frame that is linked to a
    shape frame. {it:options} are as follows.

{phang}
    {cmd:by(}{help varname:{it:byvar}}{cmd:)} computes a separate bounding box
    for each group of units defined by the levels of {it:byvar}. In the new
    frame, the levels of {it:byvar} will be used as values of the ID variable.

{phang}
    {opt pad:ding(#)} adds padding to the bounding box. Argument {it:#}
    is in percent of the halfwidth in each direction. For example,
    type {cmd:padding(5)} to add 5% padding. Default is {cmd:padding(0)}.

{phang}
    {opt rot:ate} allows rotation of the bounding box such that the minimum-area
    bounding box will be found. {cmd:rotate} has no effect if {cmd:circle} or
    {cmd:hull} is specified.

{phang}
    {opt hull} computes the convex hull rather than a rectangular bounding
    box. Only one of {cmd:hull} and {cmd:circle} is allowed.

{phang}
    {opt cir:cle} computes the minimum enclosing circle (MEC) rather than a
    rectangular bounding box. Only one of {cmd:circle} and {cmd:hull} is allowed.

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
    {opt nos:hp} uses information on coordinates from the current frame even if
    the current frame is linked to a shape frame.

{phang}
    {opt replace} allows overwriting existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{marker bshare}{...}
{dlgtab:geoframe bshare}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:bshare} {it:newname} [{it:newshpname}]
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    extracts shared borders from the selected shapes and stores them in a new
    frame called {it:newname} and an associated shape frame called
    {it:newshpname}; {it:newname}{cmd:_shp} is used as name for the shape
    frame if {it:newshpname} is omitted. A shared border is defined as a
    sequence of vertices that exists in two shape items (irrespective of
    direction). The current frame may be a shape frame or an attribute frame
    that is linked to a shape frame. {it:options} are as follows.

{phang}
    {opt uniq:ue} omits duplicates. By default, each shared border is included
    twice (possibly with different orientation), once for each of the two shape
    items that share the border. Specify {cmd:unique} to keep only one of the two
    copies. Only one of {cmd:unique}, {cmd:endpoints}, and
    {cmd:not} is allowed.

{phang}
    {opt end:points} extracts start and end points of shared
    borders only. Only one of {cmd:unique}, {cmd:endpoints}, and
    {cmd:not} is allowed.

{phang}
    {cmd:not} extracts non-shared borders rather than shared borders. That is,
    from each shape item all sequences of vertices are extracted that do not
    exist in any other item. Only one of {cmd:unique}, {cmd:endpoints}, and
    {cmd:not} is allowed.

{phang}
    {opt nodot:s} suppresses the progress dots that are displayed by default.

{phang}
    {opt nos:hp} uses information on coordinates from the current frame even if
    the current frame is linked to a shape frame.

{phang}
    {opt replace} allows overwriting existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{pstd}
    {cmd:geoframe bshare} cannot be used with paired-coordinates data.

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
    {opt replace} allows overwriting existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{marker symboli}{...}
{dlgtab:geoframe symboli}

{p 8 15 2}
    {cmd:geoframe} {cmd:symboli} {it:newname} [{it:newshpname}]
    {it:immediate_values} [{cmd:,} {it:options} ]

{pstd}
    creates symbol shapes at specified positions
    and stores them in a new frame called {it:newname} and an
    associated shape frame called {it:newshpname}; {it:newname}{cmd:_shp} is
    used as name for the shape frame if {it:newshpname} is omitted.

{pstd}
    {it:immediate_values} is one or more of

        {it:#_x} {it:#_y} [{cmd:[}[{cmd:*}]{it:size}{cmd:]}]

{pstd}
    where {it:#_x} and {it:#_y} specify the position of the symbol and optional argument
    {it:size} specifies a custom size for the symbol ({it:size} must be enclosed in
    square brackets). Specify {cmd:*}{it:size} to multiply the default size by
    {it:size}; specify {it:size} without {cmd:*} for an absolute size. {it:options}
    are as follows.

{phang}
    {cmdab:si:ze(}[{cmd:*}]{it:size}{cmd:)} sets the size argument for those
    symbols for which no custom size has been specified.

{phang}
    {cmd:shape()}, {cmd:n()}, {cmd:ratio()}, {cmd:angle()}, and
    {cmd:offset()} are options as described for
    layertype {helpb geoplot##symbol:symbol} in {helpb geoplot}.

{phang}
    {opt replace} allows overwriting existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{marker collapse}{...}
{dlgtab:geoframe collapse}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:collapse} {it:frame2}
    {help collapse:{it:clist}} {ifin} {weight} [{cmd:,} {it:options} ]

{pstd}
    finds the positions of the points provided by {it:frame2} in the shapes
    defined by the current frame using {helpb geoframe##spjoin:geoframe spjoin},
    computes summary statistics such as means,
    sums, or counts by units using {helpb collapse}, and then adds the results
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
    {opth sel:ect(exp)} restricts the data from the current frame to be
    considered. The default is to use all data. Specify
    {opt select(exp)} to consider only data for which {it:exp} is unequal 0.

{phang}
    {opt co:ordinates(X Y)} specifies custom coordinate variables
    in {it:frame2}. The default is to use the variables returned by
    {helpb geoframe##set:geoframe get coordinates}.

{phang}
    {opt gen:erate}[{cmd:(}{it:spec}{cmd:)}] causes the unit IDs of the matched
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
    {help weight}. {it:options} are as described in {helpb contract}
    as well as {cmd:select()}, {cmd:coordinates()}, {cmd:generate()}, and
    {cmd:id()} as described in {helpb geoframe##collapse:geoframe collapse}.

{marker spjoin}{...}
{dlgtab:geoframe spjoin}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:spjoin} {it:frame2} [{it:ID}]
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    finds the positions of the points provided by the current frame in the shapes
    defined by {it:frame2} (note the reverse logic of the syntax compared to
    {helpb geoframe##collapse:geoframe collapse}) and stores the unit IDs of the matched shapes
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
    {opth sel:ect(exp)} restricts the data from {it:frame2} that will be
    considered in the spatial join. The default is to use all data. Specify
    {opt select(exp)} to consider only data for which {it:exp} is unequal 0.

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

{pstd}
    {cmd:geoframe spjoin} cannot be used with paired-coordinates data.

{marker copy}{...}
{dlgtab:geoframe copy}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:copy} {it:frame2}
    {varlist} {ifin} [{cmd:,} {it:options} ]

{pstd}
    copies variables from {it:frame2} (source frame) into the current
    frame (target frame), where {varlist} specifies the variables to be
    copied and the {it:if} and {it:in} qualifiers select the observations
    to be considered from {it:frame2} (all obervations by default).

{pstd}
    Both frames can be unique (one row per unit; e.g.,
    an attribute frame) or non-unique (multiple rows per unit; e.g., a shape
    frame). The operation performed by {cmd:geoframe copy} depends on the specific
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
    If a suitable {cmd:geoframe} link exists between the two frames (and neither
    {it:if} nor {it:in} is specified), the existing link will be employed. Otherwise,
    an appropriate link will be established on the fly. {it:options} are as follows.

{phang}
    {opt tar:get(namelist)} specifies alternative variable names to be used
    in the current frame. {it:varlist} and {cmd:target()}, after applying {cmd:exclude()},
    will be matched one by one. If {cmd:target()} contains fewer
    elements than {it:varlist}, the remaining names will be taken
    from {it:varlist}.

{phang}
    {opt ex:clude(varlist)} excludes the specified variables from the main
    {varlist}. These variables will not be copied.

{phang}
    {opt relax} causes variables that already exist in the current frame to be
    skipped. The default is to abort with error if a variable already exists.

{phang}
    {cmd:id(}{it:id} [{it:id2}]{cmd:)} specifies custom ID variables for the
    merging. The default is to use the ID variables returned by
    {helpb geoframe##set:geoframe get id} (and to abort with error if
    {helpb geoframe##set:geoframe get id} fails in any of the two frames). Use
    option {cmd:id()} to override this default behavior. Argument {it:id} provides
    the name of the ID in the current frame, optional {it:id2} provides
    the name of the ID in {it:frame2}; the same name will be used in
    both frames if {it:id2} is omitted.

{phang}
    {opt uniq:ue} aborts with error if {it:frame2} is not unique.

{pstd}
    The number of copied variables is returned in scalar {cmd:r(k)}. The (target)
    names of the copied variables are returned in {cmd:r(varlist)}.

{marker append}{...}
{dlgtab:geoframe append}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:ap:pend} {it:frame2}
    [{varlist}] {ifin} [{cmd:,} {it:options} ]

{pstd}
    appends the selected observations from {it:frame2} to the current frame. By
    default, all variables from {it:frame2} will be included, creating new
    variables in the current frame if necessary. Specify {varlist} to include
    a selection of variables only. {it:options} are as follows.

{phang}
    {opt raw} appends raw data only, without copying labels or display
    formats. By default, {cmd:geoframe append} complements variable labels
    for variables that do not yet have a label, merges value label definitions
    for variables that have value labels, and copies display formats for new
    variables. Specify {cmd:raw} to copy only the data, ignoring labels and
    display formats.

{phang}
    {opt force} allows string variables to be appended to numeric variables and
    vice versa, applying functions {helpb real()} or {helpb strofreal()}
    to transform data, respectively.

{phang}
    {opt tar:get(namelist)} specifies alternative variable names to be used
    in the current frame. Use this option to append a variable
    from {it:frame2} to a variable that has a different name in the current
    frame; {it:varlist} and {cmd:target()} are matched one by one; if {cmd:target()}
    contains fewer elements than {it:varlist}, the remaining names will be taken
    from {it:varlist}. The resulting list of target variable must be unique.

{phang}
    {opth touse(varname)} specifies a variable identifying the observations to be
    appended (i.e. observations for which the specified variable is unequal
    zero). Use this option as an alternative to {it:{help if}} and
    {it:{help in}}. {cmd:touse()} is intended for use by programmers.

{phang}
    {opt fast} does not restore the original data should the user press
    {cmd:Break}. {cmd:fast} is intended for use by programmers.

{marker stack}{...}
{dlgtab:geoframe stack}

{p 8 15 2}
    {cmd:geoframe} {cmd:stack} {it:framelist} [{cmd:,} {it:options} ]

{pstd}
    stacks the data of the specified frames and also creates
    a stacked shape frame if any of the specified frames is linked to a
    shape frame. {it:framelist} must contain at least two names. A new
    {cmd:_ID} variable will be generated to identify the units in the stacked frame
    (replacing the existing IDs; other geoframe settings will be taken from
    the first frame). Furthermore, variable {cmd:_FRAME} will be
    added, containing the name of the source frame for each
    observation. {it:options} are as follows.

{phang}
    {opt force} allows string variables to be appended to numeric variables and
    vice versa, applying functions {helpb real()} or {helpb strofreal()}
    to transform data, respectively.

{phang}
    {opt noshp} causes linked shape frames to be ignored. That is, if {cmd:noshp}
    is specified, no stacked shape frame will be created even if the frames in
    {it:framelist} contain links to shape frames.

{phang}
    {opt into(newname [newshpname])} specifies a custom name for the frame
    containing the stacked data (and a custom name for the stacked shape data;
    {it:newname}{cmd:_shp} is used if {it:newshpname} is omitted). By default,
    {it:newname} is set to the first name in {it:framelist}.

{phang}
    {opt replace} allows replacing existing frames. {cmd:replace} is required
    if {cmd:into()} is omitted.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{phang}
    {opt drop} drops the source frames from memory after appending them.

{marker rename}{...}
{dlgtab:geoframe rename}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:ren:ame} {it:newname} [{it:newshpname}]
    [{cmd:,} {it:options} ]

{pstd}
    renames the current frame to {it:newname} and, if the current frame is linked
    to a shape frame, renames the shape frame to {it:newshpname}; {it:newname}{cmd:_shp}
    is used as name for the shape frame if {it:newshpname} is omitted.

{phang}
    {opt nos:hp} renames the current frame only even if the current frame is
    linked to a shape frame.

{phang}
    {opt replace} allows overwriting existing frames.

{pstd}
    Renaming geoframes using official Stata's {cmd:frame rename} is discouraged
    as this will break links created by {cmd:geoframe}. Use
    {cmd:geoframe rename} to make sure that the links will be preserved.

{marker duplicate}{...}
{dlgtab:geoframe duplicate}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:dup:licate} {it:newname}
    [{it:newshpname}] [{cmd:,} {it:options}]

{pstd}
    copies the current frame to a new frame called {it:newname} and, if the
    current frame is linked to a shape frame, copies the shape frame to a new
    frame called {it:newshpname}; {it:newname}{cmd:_shp} is used as name for
    the shape frame if {it:newshpname} is omitted. {it:options} are as follows.

{phang}
    {opt nos:hp} copies the current frame only even if the current frame is
    linked to a shape frame. A link to the original shape frame will be
    established in the copied frame.

{phang}
    {opt unl:ink} does not establish a link to a shape frame in the copied
    frame. {cmd:unlink} implies {cmd:noshp}.

{phang}
    {opt replace} allows overwriting existing frames.

{phang}
    {opt cur:rent} makes the created frame the current frame.

{pstd}
    Copying geoframes using official Stata's {cmd:frame copy} is discouraged
    as this will break links created by created by {cmd:geoframe}. Use
    {cmd:geoframe duplicate} to make sure that the links will be preserved.

{marker link}{...}
{dlgtab:geoframe link}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:l:ink} {it:shpframe}
    [{cmd:,} {cmdab:cl:ean}[{cmd:(}{it:options}{cmd:)}] ]

{pstd}
    establishes a link between the current frame and {it:shpframe}. {cmd:geoframe link}
    is an alternative to linking frames automatically when loading them
    using {helpb geoframe##create:geoframe create}.

{pstd}
    The current frame must be an attribute frame (one row per unit) and
    {it:shpframe} is typically a frame containing shape information on the units
    represented in the current frame. For each frame only one link to a
    shape frame can be registered; calling {cmd:geoframe link} will
    remove any existing link to another shape frame. (But a single
    shape frame may contain incoming links from multiple attribute frames.)

{pstd}
    Option {cmd:clean()} calls {helpb geoframe##clean:geoframe clean} after
    linking the frames. See below for a description of {it:options}.

{marker relink}{...}
{dlgtab:geoframe relink}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:rel:ink}

{pstd}
    rebuilds an existing link to a shape frame that has been created in the
    current frame by {cmd:geoframe}. For example, use {cmd:geoframe relink}
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
    current frame by {cmd:geoframe}. If the current frame does not contain
    a link to a shape frame, {cmd:geoframe unlink} has no effect.

{marker clean}{...}
{dlgtab:geoframe clean}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:clean}
        [{cmd:,} {it:options} ]

{pstd}
    removes unmatched shapes and units if applied to an attribute frame that
    is linked to a shape frame. {it:options} are as follows.

{phang}
    {opt s:hapes} deletes unmatched and empty shapes in the linked shape frame.

{phang}
    {opt u:nits} deletes unmatched units and units linked to empty
    shapes in the current frame.

{pmore}
    If neither {cmd:shapes} nor {cmd:units} is specified, the default is to delete both,
    unmatched or empty shapes in the linked shape frame as well as unmatched units or
    units linked to empty shapes in the current frame.

{phang}
    {opt noe:mpty} does not delete empty shapes or units linked to empty shapes.

{phang}
    {opt e:mptyonly} deletes empty shapes and units linked to empty shapes
    only. Unmatched shapes and units will be retained.

{pstd}
    A shape is considered empty if it contains only missing coordinates.

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

{marker translate}{...}
{dlgtab:geoframe translate}

{p 8 15 2}
    {cmd:geoframe} {cmdab:tr:anslate} [{it:translator}] {it:...}

{pstd}
    translates shapefile source data to Stata format, where {it:translator} specifies
    the type of translation. Available translators are:

{p2colset 9 18 20 2}{...}
{p2col : {helpb geoframe##tr_esri:esri}}translate ESRI shapefile (the default)
    {p_end}
{p2col : {helpb geoframe##tr_esri:shp}}synonym for {cmd:geoframe translate esri}
    {p_end}
{p2col : {helpb geoframe##tr_json:json}}translate GeoJSON shapefile
    {p_end}
{p2col : {helpb geoframe##tr_json:geojson}}synonym for {cmd:geoframe translate json}
    {p_end}
{p2col : {helpb geoframe##tr_wkt:wkt}}translate WKT (well-known text) geometries
    {p_end}

{pstd}
    {cmd:geoframe convert} is a synonym for {cmd:geoframe translate}.

{marker tr_esri}{...}
{pstd}{ul:ESRI shapefile}

{p 8 15 2}
    {cmd:geoframe} {cmdab:tr:anslate} [{cmd:esri}] [{it:path}][{it:name}] [{cmd:using}]
    {it:source} [{cmd:,} {it:options} ]

{pstd}
    translates an ESRI format shapefile to Stata format (using commands
    {helpb import_shp} and {helpb import_dbase}). Translator {cmd:esri}
    is implied unless {it:source} contains a {cmd:.geojson} or {cmd:.json}
    suffix. Arguments and options are as follows.

{phang}
    {it:path} specifies an optional destination path for the translated data
    (e.g. {cmd:shapefiles/dta/}). The working directory is used if {it:path} is
    omitted. Argument {it:path} is not allowed if option
    {cmd:toframe} is specified.

{phang}
    {it:name} provides a custom name for the translated
    data. The base name of the source file will be used if {it:name} is
    omitted. If option {cmd:toframe} is specified, frames
    {it:name} and {it:name}{cmd:_shp} will be created. If option {cmd:toframe}
    is omitted files {it:name}{cmd:.dta} and
    {it:name}{cmd:_shp.dta} will be created in the directory identified by
    {it:path}.

{phang}
    {it:source} identifies the source to be translated. It can be
    a filename with or without path (e.g. {cmd:countries.shp}
    or {cmd:shapefiles/source/countries}; the file suffix is optional),
    or it can be a path without filename (e.g. {cmd:shapefiles/source/}). If the
    specified directory contains multiple source files,
    the topmost source will be translated.

{pmore}
    {it:source} can also be a zip file. For example, you can
    specify {cmd:shapefiles/source/countries.zip} to translate the source
    contained in {cmd:countries.zip}. If the zip file contains multiple source
    files, the topmost source will be translated. Alternatively, you can specify an internal path
    (e.g. {cmd:world.zip/50m/}) and/or name pointing to the source to be translated
    (e.g. {cmd:world.zip/countries.shp} or {cmd:world.zip/50m/countries}; again, the
    file suffix is optional).

{phang}
    {cmd:replace} allows overwriting existing files or frames.

{phang}
    {opt tof:rame} writes the translated data into frames instead of saving them in
    files on disk. This is equivalent to translating the data and then
    loading them using {helpb geoframe##create:geoframe create}, but without
    generating any files.

{phang}
    {cmd:nodescribe}, {cmd:current}, {cmd:feature()}, {cmd:nodrop}, and
    {cmd:noclean} as described for {helpb geoframe##create:geoframe create}. These
    options are only allowed if option {cmd:toframe} is specified.

{marker tr_json}{...}
{pstd}{ul:GeoJSON shapefile}

{p 8 15 2}
    {cmd:geoframe} {cmdab:tr:anslate} [{cmd:json}] [{it:path}][{it:name}] [{cmd:using}]
    {it:source} [{cmd:,} {it:options} ]

{pstd}
    translates a GeoJSON format shapefile to Stata format (see
    {browse "https://en.wikipedia.org/wiki/GeoJSON"}). Arguments
    {it:path}, {it:name}, and {it:source} are as described for
    {helpb geoframe##tr_esri:geoframe translate esri} (apart from file
    suffix). Translator {cmd:json}
    is implied if {it:source} contains file suffix {cmd:.geojson} or
    {cmd:.json}. {it:options} are as follows.

{phang}
    {opt all:string} imports all attributes as string variables.

{phang}
    {opth gt:ype(newvar)} adds a variable to the attribute file that contains
    the geometry type of each unit (1 = Point, 2 = LineString, 3 = Polygon, 4 =
    MultiPoint, 5 = MultiLineString, 6 = MultiPolygon, 7 = GeometryCollection).

{phang}
    {opt addmis:sing} adds missing rows between point geometries. In the
    translated data, each line or polygon will start with a row of missing values
    so that the different shape items can be distinguished by {helpb geoplot}. By
    default, such missing rows are omitted for items obtained from a Point or
    MultiPoint object (unless the objects are part of a GeometryCollection). Specify
    {opt addmissing} to treat points in the same way as lines and polygons.

{phang}
    {opt nodot:s} suppresses the progress dots that are displayed by default.

{phang}
    {cmd:replace}, {opt toframe}, {cmd:nodescribe}, {cmd:current},
    {cmd:feature()}, {cmd:nodrop}, and {cmd:noclean} as for
    {helpb geoframe##tr_esri:geoframe translate esri}.

{marker tr_wkt}{...}
{pstd}{ul:WKT geometries}

{p 8 15 2}
    {cmd:geoframe} {cmdab:tr:anslate} {cmd:wkt} [{it:path}]{it:name} {it:geomvar}
    {ifin} [{cmd:,} {it:options} ]

{pstd}
    where arguments {it:path} and {it:name} are as described for
    {helpb geoframe##tr_esri:geoframe translate esri} (except that {it:name}
    is not optional) and {it:geomvar} is the name of a string variable
    containing {browse "https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry":WKT geometry}
    definitions. That is, the source data is assumed to be in memory (e.g.
    imported from a CSV or Excel file), with one of the variables containing
    the WKT geometry definitions. An attribute file or frame and a shape file
    or frame is then created from this data. Units that do not satisfy the
    {it:if} and {it:in} qualifiers will not be considered. Options
    are as described for {helpb geoframe##tr_json:geoframe translate json} (except
    {cmd:addmissing}, which is not allowed).

{marker import}{...}
{dlgtab:geoframe import}

{p 8 15 2}
    {cmd:geoframe} {cmdab:import} [{it:translator}] {it:...}

{pstd}
    translates shapefile source data to Stata format and loads it into
    frames. This is equivalent to translating the data using
    {helpb geoframe##translate:geoframe translate} and then
    loading the translated data using
    {helpb geoframe##create:geoframe create}, but without generating any files.

{pstd}
    {cmd:geoframe import} is implemented by calling
    {helpb geoframe##translate:geoframe translate} with option
    {cmd:toframe}. Syntax and options are identical to
    {helpb geoframe##translate:geoframe translate} (with option
    {cmd:toframe} assumed)


{marker examples}{...}
{title:Examples}

{pstd}
    Load attribute data and associated shape file:

{p 8 12 2}
    {stata "local url http://fmwww.bc.edu/repec/bocode/i/"}
    {p_end}
{p 8 12 2}
    {stata geoframe create regions `url'Italy-RegionsData.dta, id(id) coord(xcoord ycoord) shp(Italy-RegionsCoordinates.dta)}
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


{marker author}{...}
{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2023). geoplot: Stata module to draw maps. Available from
    {browse "https://ideas.repec.org/c/boc/bocode/s459211.html"}.

