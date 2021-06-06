## Version 0.0.7

### CHANGES
* Allow all command line options to be provided with `mzn_eval()` for solving a MiniZinc model like `other_cl_options`. 

## Version 0.0.6

### CHANGES

* Added functions for directly solving magic square, magic series, assignment and production planning problem.
* `IntDecl()`, `FloatDecl()` and  `BoolDecl()` can directly accept R integer, double and boolean values respectively.
* `Let` class had incorrect serialization logic and its member functions were not working which has been resolved.
* `Array` class had incorrect serialization which has been resolved.
* The vignette has been re-written to make it easy to understand and more detailed.
* Installation was not working in some OS due to the strip binary in `Makevars.in` which is now resolved.
* Corrected the error messages in `mzn_parse()` and `mzn_eval()`.

## Version 0.0.5

### CHANGES

* Added functions for directly solving knapsack and assignment problem.
* Improved `IntDecl()` and `FloatDecl()`. Now, integers and floats with domains can be declared.
* Updated the MiniZinc files according to the latest MiniZinc release.

### BUGFIXES

* `AssignItem` constructor had incorrect function calls due to which the mzn_parse() function was throwing errors for models containing assignments. This has now been fixed.
* The assertion in the `ArrayAccess` class was incorrected which has now been resolved. 


## Version 0.0.4

### BUGFIXES

* Modified configure script:  
  *  Configuration was not working when `--with-mzn` argument was provided during the installation because file variable path was not initialized for that section. It has now been fixed.

## Version 0.0.3

### BUGFIXES

* Modified configure script:  
  *  Checking for built libmzn.a, then looking for headers and then defining MZN_PATH so that mzn_parse works correctly for custom paths. (It was only working for default paths earlier).
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
