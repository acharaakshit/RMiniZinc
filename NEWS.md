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