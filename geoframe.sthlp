{smcl}
{* 17may2023}{...}
{hi:help geoframe}{...}
{right:{browse "http://github.com/benjann/geoplot/"}}
{hline}

{title:Title}

{pstd}{hi:geoframe} {hline 2} Command to prepare data for {helpb geoplot}


{title:Syntax}

{pstd}
    Load data into frame

{p 8 15 2}
    {cmd:geoframe} {cmdab:cr:eate} {it:frame} [{cmd:using}] {it:{help filename}}
    [{cmd:,} {it:{help geoframe##create:options}} ]


{pstd}
    Manipulate existing frame

{p 8 15 2}
     [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {it:{help geoframe##subcmb:subcommand}} {it:...}

{p2colset 9 22 24 2}{...}
{marker subcmb}{...}
{p2col :{it:subcommand}}Description
    {p_end}
{p2line}
{p2col :{helpb geoframe##set:set}}adjust settings of current frame
    {p_end}
{p2col :{helpb geoframe##get:get}}retrieve setting from current frame
    {p_end}
{p2col :{helpb geoframe##attach:{ul:at}tach}}attach other frame to current frame using aliases (Stata 18 required)
    {p_end}
{p2col :{helpb geoframe##detach:{ul:det}ach}}detach other frame from current frame (Stata 18 required)
    {p_end}
{p2col :{helpb geoframe##copy:copy}}copy variables from other frame to current frame
    {p_end}
{p2col :{helpb geoframe##append:{ul:ap}pend}}append observations from other frame to current frame
    {p_end}
{p2col :{helpb geoframe##generate:{ul:g}enerate}}generate special-purpose variables in current frame
    {p_end}
{p2col :{helpb geoframe##generate:varinit}}initialize variables in current frame
    {p_end}
{p2line}


{title:Description}

{pstd}
    {cmd:geoframe} prepares data for {helpb geoplot}.


{title:Subcommands}

{marker create}{...}
{dlgtab:geoframe create}

{p 8 15 2}
    {cmd:geoframe} {cmdab:cr:eate} {it:frame} [{cmd:using}] {it:{help filename}}
    [{cmd:,}
    {opt replace} {opt set(settings)}
    ]

{pstd}
    loads the data from {it:{help filename}} into a new frame called
    {it:name}, where {it:filename} is a valid Stata dataset. For example,
    {it:filename} may contain coordinate data from shape files obtained by
    {helpb spshape2dta}. Alternatively, 
    {it:filename} may contain attribute data of the units (countries, regions, etc.)
    represented in a shape file, that can then be linked to the frame containing
    the coordinate data, or it could contain coordinates and related
    attributes of other elements (e.g. coordinates and
    population sizes of cities). Options are as follows.

{phang2}
    {opt replace} allows replacing an existing frame.

{phang2}
    {opt set(settings)} applies the specified settings to the created
    frame. The syntax of {it:settings} is as explained in 
    {helpb geoframe##set:geoframe set}.

{marker set}{...}
{dlgtab:geoframe set}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:set} {it:settings} [{cmd:,} {opt relax} ]

{pstd}
    defines or adjust the settings of the current frame. The settings, for example, contain
    information on the names of the coordinate variables. Such information will be
    retrieved by {cmd:geoframe} or {helpb geoplot} when processing a frame. The syntax for
    {it:settings} is 

            {it:name} {cmd:=} {it:value} [ {it:name} {cmd:=} {it:value} ...]

{pstd}
    where {it:name} is the name of the setting and {it:value} is its definition. Enclose {it:value}
    in quotes if it contains spaces. You are free to define any settings you wish, but note that
    {helpb geoframe} has a number of standard settings with associated default values. These
    settings are as follows:

{p2colset 9 16 28 2}{...}
{p2col :{it:name}}Default{space 3}Description
    {p_end}
{p2col :{cmd:ID}}{cmd:_ID}{space 7}{it:varname} identifying the units represented in the frame
    {p_end}
{p2col :{cmd:PID}}{cmd:_PID}{space 6}{it:varname} identifying the different polygons within a unit
    {p_end}
{p2col :{cmd:EID}}{cmd:_EID}{space 6}{it:varname} identifying polygons that are enclaves or exclaves
    {p_end}
{p2col :{cmd:Y}}{cmd:_Y}{space 8}{it:varname} of vertical coordinates (latitude)
    {p_end}
{p2col :{cmd:X}}{cmd:_X}{space 8}{it:varname} of horizontal coordinates (longitude)
    {p_end}
{p2col :{cmd:Y1}}{cmd:_Y1}{space 7}{it:varname} of origin coordinates
    (paired-coordinate data)
    {p_end}
{p2col :{cmd:X1}}{cmd:_X1}{space 7}{it:varname} of horizontal origin coordinates
    (paired-coordinate data)
    {p_end}
{p2col :{cmd:Y2}}{cmd:_Y2}{space 7}{it:varname} of vertical destination coordinates
    (paired-coordinate data)
    {p_end}
{p2col :{cmd:X2}}{cmd:_Y2}{space 7}{it:varname} of horizontal destination coordinates
    (paired-coordinate data)
    {p_end}
{p2col :{cmd:TYPE}}(empty){space 3}the type of elements represented in the frame;
    for example, set {cmd:TYPE = "water"} if the frame contains coordinates
    of lakes or rivers
    {p_end}

{pstd}
    Note that {it:name} is case insensitive; you may type {it:name} in uppercase
    (as above) or in lowercase.

{pstd}
    For the above settings that provide information on a variables name, {cmd:geoframe set}
    will return error if you modify such a setting and the specified variable
    does not exist in the data. Type option {cmd:relax} to prevent this behavior (for example,
    if you want to define a setting before the relevant variable has been created).

{pstd}
    {cmd:geoframe} settings are stored as characteristics; see {helpb char}.

{marker get}{...}
{dlgtab:geoframe get}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:get} {it:name} [{cmd:,} {opt l:ocal(lname)} {opt relax} {opt nodef:ault} ]

{pstd}
    displays the value of the setting stored under {it:name}. {it:name} is case
    insensitive; you may type {it:name} in uppercase or in lowercase. Options
    are as follows:

{phang2}
    {opt local(lname)} stores the value of the setting in a local called {it:lname}
    instead of displaying it.

{phang2}
    {cmd:relax} prevents {cmd:geoframe get} from aborting with error if a standard setting
    referring to a variable is retrieved and the variable does not exist in the data.

{phang2}
    {cmd:nodefault} returns nothing if a standard setting is retrieved for
    which no value has been defined by the user, even if the setting has a default
    value.

{marker attach}{...}
{dlgtab:geoframe attach}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:at:tach} {it:frame2}
    [{it:idvar2}]
    [{cmd:,} {opt id(idvar)} {opth keep(varlist)} {opth drop(varlist)} ]

{pstd}
    attaches {it:frame2} to the current frame using an {cmd:m:1} merge. That is, a
    link to {it:frame2} is established and all variables from {it:frame2}
    that do not already exist in the current frame (except for the ID) are added
    as aliases to the current frame. Typically the current frame will contain the shape
    coordinates of countries, regions, or other units
    and {it:frame2} will contain attribute data for each unit such as, e.g.,
    population size. After attaching {it:frame2} to the current frame, the variables
    from {it:frame2} will be available in the current frame like any other variables
    in the current frame.

{pstd}
    The IDs to merge the frames are obtained from {helpb geoframe##get:geoframe get}; alternatively,
    specify a custom ID variable for {it:frame2} as argument {it:idvar2} or for the current frame using
    option {cmd:id()}.

{pstd}
    By default, all relevant variables from {it:frame2} will be added as aliases
    to the current frame. Use option {cmd:keep()} to select the variables
    to be added; use option {cmd:drop()} to exclude variables from being added.

{pstd}
    {cmd:geoframe attach} requires Stata 18.

{marker detach}{...}
{dlgtab:geoframe detach}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:det:ach} {it:frame2}

{pstd}
    detaches {it:frame2} from the current frame. That is, all alias variables will be
    removed and the link variable will be deleted.

{pstd}
    {cmd:geoframe detach} requires Stata 18.

{marker copy}{...}
{dlgtab:geoframe copy}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:copy} {it:frame2}
    [{it:idvars2}]{cmd:,} {opth keep(varlist)} [ {opt id(idvars)} {opth drop(varlist)} ]

{pstd}
    copies variables from {it:frame2} into the current frame using an {cmd:m:1}
    merge. Syntax is as for {helpb geoframe##attach:geoframe attach}, except that
    {cmd:keep()} is required.

{pstd}
    {cmd:geoframe copy} is a Stata 17 substitute to {helpb geoframe##attach:geoframe attach}; users of
    Stata 18 are advised to use {helpb geoframe##attach:geoframe attach}, which is more powerful and
    more efficient.

{marker append}{...}
{dlgtab:geoframe append}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:ap:pend} {it:frame2}
    [{varlist}] {ifin}
    [{cmd:,} {opth tar:get(varlist)} {opth touse(varname)} ]

{pstd}
    appends observations from {it:frame2} to the current frame. By default, all variables
    from {it:frame2} will be included; creating new variables in the current
    frame if necessary. Specify {varlist} to include a selection of variables only.

{pstd}
    Option {cmd:target()} specifies the names of the target variables
    to which the observations be appended. Use this option to append a variable
    from {it:frame2} to a variable that has a different name in the current
    frame; {it:varlist} and {cmd:target()} are matched one by one; if {cmd:target()}
    contains fewer elements than {it:varlist}, the remaining elements will be taken
    from {it:varlist}. The resulting list of target variable must be unique.

{pstd}
    Option {cmd:touse()} specifies a variable identifying the observations to be
    appended (i.e. observations for which the specified variable is unequal
    zero). Use this option an alternative to {it:{help if}} and {it:{help in}}.

{marker generate}{...}
{dlgtab:geoframe generate}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:g:enerate} {it:fnc} {it:...}

{pstd}
    provides functions to generate specific variables in the current frame.

{pstd}
    Currently the only such function is

{p 8 15 2}
    {cmd:geoframe} {cmdab:g:enerate} {cmd:pid} [{it:newname}] [{cmd:,} {opt replace} ]

{pstd}
    to generate a variable identifying the different polygons with each unit
    represented in the frame. {cmd:_PID} is used as variable name if
    {it:newname} is omitted. Option {cmd:replace} allows overwriting an existing
    variable. The crated variable will be registered under {it:name} {cmd:PID} using
    {helpb geoframe##set:geoframe set}.

{marker varinit}{...}
{dlgtab:geoframe varinit}

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:varinit} {it:{help datatypes:type}} {it:namelist}

{pstd}
    initialize new variables of a given {help datatypes:storage type} in the current frame (unless the variables already
    exist).


{title:Examples}

{pstd}
    See the examples in help {helpb geoframe}.


{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2023). geoplot: Stata module to draw maps.. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s?.html"}.


{title:Also see}

{psee}
    Online:  help for
    {helpb geoplot}, {helpb frames}, {helpb spshape2dta}
