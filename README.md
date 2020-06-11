# RMiniZinc

## Under Development

This package is aimed towards providing the r users to use MiniZinc from within R. Currently basic integer programming problems can be solved. The package vignette contains a basic demonstration of how to create a MiniZinc model for basic constraint programming problems and obtain a string output. The vignette will be updated from time to time and more features will be added to the package.

## Getting Started

One of the prerequisites to use this package is that [MiniZinc](https://www.minizinc.org/) is installed on your system and has been added to the path. The installation instructions can be found [here](https://www.minizinc.org/doc-2.4.3/en/installation.html). This package is currently available only for linux users, the installation steps for osx and windows users will be released later. 
To install minizinc : `snap install minizinc --classic` should be used. You can verify if MiniZinc is accessible from the command line by typing `minizinc`. You can use the package once you have set up MiniZinc.
Once you have installed the snap binary, you need to build the [libminizinc](https://github.com/MiniZinc/libminizinc) library by following the steps given below:

Go to the lib folder and clone the repository:
`cd /usr/local/lib`
`sudo git clone https://github.com/MiniZinc/libminizinc.git`

Now go to the libminizinc folder:
`cd libminizinc/`  

Set the CFLAGS, CPPFLAGS AND CXXFLAGS as compilation with -fpic is required to prevent the relocation errors while integrating the library with Rcpp:
`sudo sed -i '3 i set(CMAKE_CXX_FLAGS "-fpic" CACHE INTERNAL "")' CMakeLists.txt`
`sudo sed -i '4 i set(CMAKE_CPP_FLAGS "-fpic" CACHE INTERNAL "")' CMakeLists.txt`
`sudo sed -i '5 i set(CMAKE_C_FLAGS "-fpic" CACHE INTERNAL "")' CMakeLists.txt`

Now, build the library (make sure you have `cmake` installed on your system):
`sudo cmake CMakeLists.txt`
`sudo make`
`sudo make install`

Now you need to put the solver configurations from the MiniZinc binary to the libminizinc solvers:
`sudo cp -r /snap/minizinc/current/share/minizinc/solvers  /usr/local/lib/libminizinc/share/minizinc`
`cd share/minizinc/solvers`

Change the paths accordingly in the library (Currently the package is only using Gecode solver to solve CP problems):
`sudo sed -i 's+../../../bin+/snap/minizinc/current/bin+g' gecode.msc`

Now, the library is built and you need to add it to the path:
`export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/libminizinc`
`sudo echo 'export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/libminizinc' >> ~/.bashrc`
`sudo ldconfig`

Other than solving CP problems using the libminizinc library, the package also uses the command line, so, you need to add the solvers from your binary to the path where minizinc can find them:
`sudo cp -r /snap/minizinc/current/share/minizinc/. /usr/local/share/minizinc/`
`cd /usr/local/share/minizinc/solvers`

Change the path in the configuration files accordingly:
`sudo sed -i 's+../../../bin+/snap/minizinc/current/bin+g' gecode.msc`

Now, you can use the package!

Please note that these are workaround instructions to run this package currently and improved installation instructions will be released soon. 


