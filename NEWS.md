## Version 1.0.1

### CHANGES

* Provided a more elaborate description of the package.

### BUGFIXES

* Modified configure script to make the package compatible with other Unix-like systems like Solaris and
  resolved a possible error that would occur in case the user tries to install rminizinc with 
    a) just solver binaries and not libminizinc.
    b) solver binaries and unbuilt libminizinc (i.e. without libmzn.a)
* Modified config.h.in to prevent possible issues on Solaris.