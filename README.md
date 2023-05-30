# geoplot
Stata module to draw maps

**This is a beta version that will likely have various issues.**

To install `geoplot` from GitHub, type

    . net install geoplot, replace from(https://raw.githubusercontent.com/benjann/geoplot/main/)

Stata 17 is required.

The following packages are required:

    . ssc install palettes, replace
    . ssc install colrspace, replace
    . ssc install moremata, replace

---

Main changes:

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


