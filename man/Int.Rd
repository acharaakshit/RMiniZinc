% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Expression.R
\name{Int}
\alias{Int}
\title{Int}
\description{
Create an integer in MiniZinc
}
\examples{
newInt = Int$new(10)
newInt$c_str()
newInt$setIntVal(newInt$getIntVal() + 20)
newInt$c_str()
}
\section{Super class}{
\code{\link[rminizinc:Expression]{rminizinc::Expression}} -> \code{Int}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{.value}}{object of class expression}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{.value}}{object of class expression}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Int$new()}}
\item \href{#method-getIntVal}{\code{Int$getIntVal()}}
\item \href{#method-setIntVal}{\code{Int$setIntVal()}}
\item \href{#method-c_str}{\code{Int$c_str()}}
\item \href{#method-clone}{\code{Int$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
constructor
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Int$new(val)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{val}}{the value of the integer}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getIntVal"></a>}}
\if{latex}{\out{\hypertarget{method-getIntVal}{}}}
\subsection{Method \code{getIntVal()}}{
get the IntVal value
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Int$getIntVal()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-setIntVal"></a>}}
\if{latex}{\out{\hypertarget{method-setIntVal}{}}}
\subsection{Method \code{setIntVal()}}{
set the IntVal value
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Int$setIntVal(val)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{val}}{value to be set}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-c_str"></a>}}
\if{latex}{\out{\hypertarget{method-c_str}{}}}
\subsection{Method \code{c_str()}}{
get the MiniZinc representation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Int$c_str()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Int$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
