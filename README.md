# geoplot
Stata module to draw maps

**This is a beta version that will likely have various issues. Also documentation is still incomplete.**

To install `geoplot` from GitHub, type

    . net install geoplot, replace from(https://raw.githubusercontent.com/benjann/geoplot/main/)

Stata 17 is required.

The following packages are required:

    . ssc install palettes, replace
    . ssc install colrspace, replace
    . ssc install moremata, replace

---

Main changes:

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


