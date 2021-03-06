% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DeclFunctions.R
\name{BoolArrDecl}
\alias{BoolArrDecl}
\title{n-D bool array declaration}
\usage{
BoolArrDecl(name, kind, ind, value = NULL, ndim)
}
\arguments{
\item{name}{variable/parameter name}

\item{kind}{"var" or "par"}

\item{ind}{index of the array}

\item{value}{value (NULL by default)}

\item{ndim}{number of dimensions of the array}
}
\description{
Declare a new n-dimensional array
of bools
}
