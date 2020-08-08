# RMiniZinc

## Under Development

This package is aimed towards providing the r users to use MiniZinc from within R. Currently basic integer programming problems can be solved. The package vignette contains a basic demonstration of how to create and/or solve a MiniZinc model for a wide variety of constraint programming problems, obtain the string solution or parsed solution for an existing model. Please refer to the vignette for more information. The vignette will be updated from time to time and more features will be added to the package.

## Getting Started

One of the prerequisites to use this package is that [MiniZinc](https://www.minizinc.org/) is installed on your system and has been added to the path. The installation instructions can be found [here](https://www.minizinc.org/doc-2.4.3/en/installation.html). This package is currently available only for linux users, the installation steps for osx and windows users will be released later. 
To install minizinc : `snap install minizinc --classic` should be used. You can verify if MiniZinc is accessible from the command line by typing `minizinc`. You can use the package once you have set up MiniZinc.
Once you have installed the snap binary, you need to build the [libminizinc](https://github.com/MiniZinc/libminizinc) library by following the steps given below:

Clone the repository:

`sudo git clone https://github.com/MiniZinc/libminizinc.git`

Now go to the libminizinc folder:

`cd libminizinc/`  

Set the flag to prevent the relocation errors while integrating the library with Rcpp:

`sudo sed -i '3 i set(CMAKE_POSITION_INDEPENDENT_CODE ON)' CMakeLists.txt`

Now, build the library (make sure you have `cmake` installed on your system):

`sudo cmake CMakeLists.txt`

`sudo make`

`sudo make install`

Now you need to put the solver configurations from the MiniZinc binary to the libminizinc solvers:

`sudo cp -r /snap/minizinc/current/share/minizinc/solvers  /path/to/libminizinc/share/minizinc`

`cd share/minizinc/solvers`

Change the paths accordingly in the library (Currently the package is only using Gecode solver to solve CP problems):

`sudo sed -i 's+../../../bin+/snap/minizinc/current/bin+g' gecode.msc`

Now, the library is built and you need to add it to the path:

`export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/libminizinc`

`sudo echo 'export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/libminizinc' >> ~/.bashrc`

`sudo ldconfig`

Other than solving CP problems using the libminizinc library, the package also uses the command line, so, you need to add the solvers from your binary to the path where minizinc can find them:

`sudo cp -r /snap/minizinc/current/share/minizinc/. /usr/local/share/minizinc/`

`cd /usr/local/share/minizinc/solvers`

Change the path in the configuration files accordingly:

`sudo sed -i 's+../../../bin+/snap/minizinc/current/bin+g' gecode.msc`

The package can be installed using  
`R CMD INSTALL rminizinc_0.0.0.99.tar.gz --configure-args='--with-mzn=/path/to/libminizinc'` if already built or using `remotes::install_github("acharaakshit/rminizinc", configure.args="--with-mzn=/path/to/libminizinc")`

Please note that if path arguments are not passed along with the installation, the default path `usr/local/lib` will be chosen so you will require to install libminizinc in `usr/local/lib`.

Now, you can use the package!  
