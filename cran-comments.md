## Notes

### Fifth Submission

Please find the bug fixes in NEWS.md

### Fourth Submission

Please find the bug fixes in NEWS.md.

### Third Submission

> configure: error: "OS  is not supported"

I was only checking for Windows, OSX and Linux OS in my configure
script but Solaris is a different OS.
The package should be configured differently for Windows and
Non-Windows OS. Now, I am just checking if an OS is Windows or
not. This will resolve issues for all types of systems.

I have also updated the config.h.in file to make use
of #if and #else correctly such that it is compatible
with all OS.

I added a check for the minimum required pandoc version in the
vignette and also updated the SystemRequirements field of 
Description accordingly.

### Second Submission

> Possibly mis-spelled words in DESCRIPTION:  
    MiniZinc (3:27, 7:97, 8:31)

I have now added single quotes around the software name.

> Flavor: r-devel-windows-ix86+x86_64  
Check: compiled code, Result: NOTE  
  Note: information on .o files for i386 is not available  
  Note: information on .o files for x64 is not available  
  File 'd:/RCompile/CRANincoming/R-devel/lib/rminizinc/libs/i386/rminizinc.dll':  
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)  
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)  
    Found 'printf', possibly from 'printf' (C)  
  File 'd:/RCompile/CRANincoming/R-devel/lib/rminizinc/libs/x64/rminizinc.dll':  
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)  
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)  
    Found 'printf', possibly from 'printf' (C)   
 Compiled code should not call entry points which might terminate R nor
  write to stdout/stderr instead of to the console, nor use Fortran I/O
  nor system RNGs. The detected symbols are linked into the code but
  might come from libraries and not actually be called.  
  See 'Writing portable packages' in the 'Writing R Extensions' manual.  
Flavor: r-devel-linux-x86_64-debian-gcc  
Check: compiled code, Result: NOTE  
Note: information on .o files is not available

I have added cleanup scripts for cleaning up the files instead to using `clean` in Makevars due to which the object files were removed and the NOTE was showing up. It has been resolved. Thanks.

## Test environments
* Linux Mint 19 Tara, R version 4.0.3
* win-builder(except for r-oldrelease)
* R-hub check_for_cran

## R CMD check results

0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE  
  installed size is  5.6Mb  
  sub-directories of 1Mb or more:  
    minizinc   3.2Mb
