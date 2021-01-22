## Version 0.0.4

### BUGFIXES

* Modified configure script:  
    *  configuration was not working when `--with-mzn` argument was provided during the installation              because file variable path was not initialized for that section. It has now been fixed.

## Version 0.0.3

### BUGFIXES

* Modified configure script:  
    *  checking for built libmzn.a, then looking for headers and then defining MZN_PATH so that mzn_parse           works correctly for custom paths. (It was only working for default paths earlier).
* Corrected config.h.in to make sure the correct definitions of `mzn_parse()` and `mzn_eval()` are used.

## Version 0.0.2

### CHANGES

* Provided a more elaborate description of the package.
* Corrected package version number in the configure script.

### BUGFIXES

* Modified configure script to make the package compatible with other Unix-like systems like Solaris and
  resolved a possible error that would occur in case the user tries to install rminizinc with
    a) just solver binaries and not libminizinc.
    b) solver binaries and unbuilt libminizinc (i.e. without libmzn.a)
* Modified config.h.in to prevent possible issues on Solaris.
* Added a check for the minimum required pandoc version in the vignette and also update the SystemRequirements field of Description accordingly.
* Changed Rscript to Rscript.exe in Makevars.win.in to avoid issues on Windows.
