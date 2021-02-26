# R INTERFACE TO MINIZINC


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

1) Build libminizinc  

First, You need to download the latest libminizinc release and build libminizinc library for MiniZinc to work properly.  

Please follow these steps for Linux:  

*  Download the latest libminizinc release from https://github.com/MiniZinc/libminizinc/releases.
*  Extract the downloaded tar.gz or zip (for Windows) and name the extracted folder `libminizinc`.
* `cd libminizinc/`  
* `sudo sed -i '3 i set(CMAKE_POSITION_INDEPENDENT_CODE ON)' CMakeLists.txt`  
* `sudo cmake CMakeLists.txt`  
* `sudo make`  
* `sudo make install`  

Similarly, build libminizinc on Windows (can use cygwin) and OSX.

If `sed` command doesn't work for you, just add `set(CMAKE_POSITION_INDEPENDENT_CODE ON)` in the 3rd line (or any empty line in the starting) of CMakeLists.txt and follow the next steps.

2) Get Solver Binaries

Solvers are required to solve the MiniZinc models. The solvers currently  supported by rminizinc are Chuffed, FindMUS and Gecode. Any solver can be selected based on the type of problem that is required to be solved.

Now download the solver binaries to be able to solve the models and achieve full functionality of the package.

To get the solver binaries, the Users can download the MiniZinc binary bundles for Windows, MAC OS or Linux from https://www.minizinc.org/software.html and the provide the path to the bin folder of the MiniZinc bundle folder as the `--with-bin` argument. All the required solver binaries are present in that folder. The solver binary corresponding to Gecode will be `fzn-gecode`, FindMUS will be `findMUS`, Chuffed will be `fzn-chuffed` (.exe extentions will be there on Windows for eg. `fzn-gecode.exe`). ALternatively, if you don't want to keep the MiniZinc bundle, you can copy the solver binaries to another folder and just provide the path to that folder with `--with-bin`.

Once these steps are over, you just need to re-install rminizinc by using

`install.packages("rminizinc", configure.args="--with-mzn=/path/to/libminizinc --with-bin=/path/to/bin")`  

NOTE: Please don't use `\` at the end of the path given to `--with-bin` as it will cause some solver configuration issues.

Please note that if path arguments are not passed along with the installation (as `--with-mzn`), the default path `/usr/local/lib` for Linux and OSX, and `C:/Program Files/` for Windows  will be chosen but only if libminizinc in present in these default paths.

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

## ACKNOWLEDGMENT

* Thanks to my mentor [Lars Kotthoff](https://github.com/larskotthoff) and co-mentors, [Hans W Borchers](https://github.com/hwborchers) and [Guido Tack](https://github.com/guidotack) who helped me make key decisions for the project, and [Jip Dekker](https://github.com/Dekker1) for solving my queries regarding libminizinc.

* I would like to thank all the developers of [libminizinc](https://github.com/MiniZinc/libminizinc)  for allowing me to use the library in my package and for providing help in understanding the usage of the library and MiniZinc.

* Special thanks to   
  Uwe Ligges <ligges@statistik.tu-dortmund.de>  
  Dirk Eddelbuettel <edd@debian.org>   
  Nik Pocuca <nikpocuca@gmail.com>  
  [Allan Cameron](https://github.com/AllanCameron)  
  Duncan Murdoch <murdoch.duncan@gmail.com>  
  and many other people who helped me on mailing lists as well as other platforms like StackOverflow.
