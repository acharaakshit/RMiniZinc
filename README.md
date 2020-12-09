# MINIZINC INTERFACE FOR R

[![Build Status](https://travis-ci.org/acharaakshit/RMiniZinc.svg?branch=master)](https://travis-ci.org/acharaakshit/RMiniZinc)

### TABLE OF CONTENTS

* [INTRODUCTION](#INTRODUCTION)
* [INSTALLATION](#INSTALLATION)
* [FEATURES](#FEATURES)
* [SCOPE OF FEATURES](#SCOPE-OF-FEATURES)
* [PROJECT STATUS](#PROJECT-STATUS)
* [ACKNOWLEDGMENT](#ACKNOWLEDGMENT)

### INTRODUCTION

**rminizinc** is aimed towards providing the R users an interface to [MiniZinc](https://www.minizinc.org/). Various functionalities are providing for users that are comfortable with MiniZinc and use some results in their analysis in R and for users that are less confident in MiniZinc and want to create MiniZinc models and obtain results using R. The package [vignette](https://github.com/acharaakshit/RMiniZinc/blob/master/vignettes/R_MiniZinc.Rmd) contains a basic demonstration of all the features of the package and explains how to interpret the results.

### INSTALLATION

First, You need to download and build libminizinc library for MiniZinc to work properly. Please follow these steps:

Linux:

 * `sudo git clone https://github.com/MiniZinc/libminizinc.git`
    * `cd libminizinc/`  
    *  `sudo sed -i '3 i set(CMAKE_POSITION_INDEPENDENT_CODE ON)' CMakeLists.txt`
    * `sudo cmake CMakeLists.txt`
    * `sudo make`
    * `sudo make install`
    
Similarly, build libminizinc on Windows and OSX.

Now download the solver binaries from the binary bundles at (https://www.minizinc.org/) to be able to solve the models and achieve full functionality of the package.

Once these steps are over, you just need to re-install rminizinc by using

* `remotes::install_github("acharaakshit/rminizinc", configure.args="--with-mzn=/path/to/libminizinc")`  

* Please note that if path arguments are not passed along with the installation (as `--with-mzn`), the default path `/usr/local/lib` for Linux and OSX, and `C:/Program Files/` for Windows  will be chosen but only if libminizinc in present in these default paths.

If you have the source tar, then you can do `R CMD INSTALL rminizinc_0.0.0.99.tar.gz --configure-args="--with-bin=/home/akshit/Documents/GSOC/bin --with-mzn=/path/to/libminizinc/"`

NOTE: Please don't use `\` at the end of the path given to `--with-bin` as it will cause some solver configuration issues.

### FEATURES

  * Parse a MiniZinc model and get the details as a named list in R.
  * Find the model parameters which have not been assigned a value yet.
  * Set the values of unassigned parameters. (Scope needs to be extended)
  * Solve a model and get parsed solutions as a named list in R.
  * Create your own MiniZinc model in R using the [R6](https://adv-r.hadley.nz/r6.html) classes from MiniZinc API mirror (Note that the API mirror only includes the relevant functionalities to serialize various objects to MiniZinc along with some checks).
  * Use the setter and getter functions of R6 classes to manipulate a model.
  * Use the named list returned after parsing to initialize all the relevant objects in R, manipulate the model and serialize back to MiniZinc.

### SCOPE OF FEATURES

The features have been tested over a wide variety of problems but doesn't cover MiniZinc in its entirety. Please feel free to [open an issue](https://docs.github.com/en/enterprise/2.15/user/articles/creating-an-issue) or submit a PR if you find any problems in the package.

### PROJECT STATUS

**Under Development**: Please note that the project is in it's early development stage and all the features haven't been tested for all of MiniZinc.

### ACKNOWLEDGMENT
  * Thanks to my mentor [Lars Kotthoff](https://github.com/larskotthoff) and co-mentors [Hans W Borchers](https://github.com/hwborchers) and [Guido Tack](https://github.com/guidotack) who helped me make key decisions for the project. Special thanks to [Jip Dekker](https://github.com/Dekker1) for solving my queries regarding libminizinc.
  * I would like to thank all the developers of libminizinc.
