\name{print}
\docType{methods}
% \alias{print-methods}
\alias{print}
\alias{print,PSTf-method}
\alias{print,PSTr-method}
\title{ Print method for objects of class \code{PSTf} and \code{PSTr} }

\description{
Display a probabilistic suffix tree
}

\usage{
\S4method{print}{PSTr}(x, max.level = NULL, digits = 1, give.attr = FALSE, 
    nest.lev = 0, indent.str = "", stem = "--")
}

\arguments{
  \item{x}{
	A PST, that is an object of class \code{PSTf} as returned by the \code{\link{pstree}} or \code{\link{prune}} method.
}
  \item{max.level}{ integer. The maximal depth for the display of the tree.
}

  \item{digits}{ integer specifying the precision for printing.
}

  \item{give.attr}{ logical. If \code{TRUE} the attributes of each node (an object of class \code{PSTr}) are displayed.
}
  \item{nest.lev}{ integer. Parameter used internally by the function. 
}

  \item{indent.str}{ character. String used to indent each line when displaying the tree. Default to ''.
}

  \item{stem}{ character. String used to display the stems. Default to '--'.}
}

\section{Methods}{
\describe{

\item{\code{signature(x = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "PSTf")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "PSTr")}}{
%%  ~~describe this method here~~
}
}}
\keyword{methods}
\keyword{print}


