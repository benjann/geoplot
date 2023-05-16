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
    [{cmd:,}
    {opt replace} {opt set(arguments)}
    ]

{pstd}
    Set characteristics for current frame

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:set} {it:arguments} [{cmd:,} {opt relax} ]

{pmore}
    where {it:arguments} is

            {it:charname} {cmd:=} {it:value} [ {it:charname} {cmd:=} {it:value} ...]

{pstd}
    Retrieve value of characteristic from current frame

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:get} {it:charname} [{cmd:,} {opt l:ocal(lname)} {opt relax} {opt nodef:ault} ]

{pstd}
    Attach other frame to current frame ({cmd:n:1} merge; importing variables as aliases)

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:at:tach} {it:frame2}
    {it:idvars2}
    [{cmd:,} {opt id(idvars)} {opth keep(varlist)} {opth drop(varlist)} ]

{pstd}
    Detach other frame from current frame

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:det:ach} {it:frame2}

{pstd}
    Append observations from other frame to current frame

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:ap:pend} {it:frame2}
    [{varlist}] {ifin}
    [{cmd:,} {opth tar:get(varlist)} {opt touse(varname)} ]

{pstd}
    Generate special-purpose variables in current frame

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmdab:g:enerate} {it:fnc} {it:...}

{pstd}
    Initialize variables in current frame

{p 8 15 2}
    [{cmd:frame} {it:frame}{cmd::}] {cmd:geoframe} {cmd:varinit} {it:{help datatypes:type}} {it:namelist}


{title:Description}

{pstd}
    {cmd:geoframe} prepares data for {helpb geoplot}.


{title:Options}


{title:Examples}


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
    {helpb geoplot}, {helpb frames}
