\name{power-class}
\Rdversion{1.1}
\docType{class}
% powPar
\alias{powPar-class}
\alias{dim,powPar-method}
\alias{n,powPar-method}
\alias{theta,powPar-method}
\alias{xi,powPar-method}
\alias{pp,powPar-method}
\alias{powCalc,powPar-method}
% powFun
\alias{powFun-class}
% powCalc
\alias{powCalc-class}
\alias{powEx,powCalc-method}
\alias{dim,powCalc-method}
\alias{update,powCalc-method}
% powEx
\alias{powEx-class}
% power
\alias{power-class}
\alias{refine,power-method}
\alias{inspect,power-method}
\alias{show,power-method}
\alias{plot,ANY,ANY-method}
\alias{plot,power,missing-method}
\alias{tex,power,character-method}

\title{Class "power" and its subclasses "powPar", "powFun", "powCalc", "powEx".}

\description{
  A class used internally for storing information about the power
  calculation. It inherits from different classes, see section "Extends" for
  details.
}

\usage{
\S4method{show}{power}(object)
\S4method{dim}{powPar}(x)
\S4method{dim}{powCalc}(x)
}

\arguments{
  \item{object,x}{object of class \linkS4class{power}, \linkS4class{powPar} or \linkS4class{powCalc}}
}

\section{Extends}{
  The power-class has no privat slots but inherits everything from its
  ancestors.
  
  Class \linkS4class{powPar}, by class \linkS4class{powCalc}, distance 2.
  Class \linkS4class{powFun}, by class \linkS4class{powCalc}, distance 2.
  Class \linkS4class{powCalc}, directly.
  Class \linkS4class{powEx}, directly.
}

\section{powPar-class}{
  A class used internally. Contains all the parameters needed to feed
  the 'power function'.  For users there is a function
  \code{\link{powPar}} that constructs objects of this class and there
  are several functions for extracting information \code{\link{n}},
  \code{\link{theta}}, \code{\link{xi}}, \code{\link{pp}}. These
  functions should be used when writing the 'power function'.
  Contains the Slots:
  \describe{
    \item{\code{list}:}{Object of class \code{"list"} that contains all
      elements that were provided to \code{\link{powPar}} with the
      \code{...} argument.}
    \item{\code{theta}:}{An \code{"numeric"} vector with values that are
      used to evaluate the power.}
    \item{\code{theta.name}:}{Object of class \code{"character"} only
      used if argument 'theta.name' of \code{\link{powPar}} was used.}
    \item{\code{theta.act}:}{Object of class \code{"numeric"}. This slot
    is only used during evaluation where it contains one element of
    'theta' after the other. The function \code{\link{theta}} extracts
    this value. If not under evaluation this slot contains the first
    element of 'theta'.}
    \item{\code{xi}:}{Object of class \code{"numeric"} with values that
      are used to evaluate the power.}
    \item{\code{xi.name}:}{Object of class \code{"character"} only
      used if argument 'theta.name' of \code{\link{powPar}} was used.}
    \item{\code{xi.act}:}{Object of class \code{"numeric"}. This slot
    is only used during evaluation where it contains one element of
    'xi' after the other. The function \code{\link{xi}} extracts
    this value. If not under evaluation this slot contains the first
    element of xi'.}
    \item{\code{n}:}{Object of class \code{"integer"} with values that
      are used to evaluate the power.}
    \item{\code{n.act}:}{Object of class \code{"integer"}.  This slot
    is only used during evaluation where it contains one element of
    'n' after the other. The function \code{\link{n}} extracts
    this value. If not under evaluation this slot contains the first
    element of 'n'.}
  }
}

\section{powFun-class}{
  A class used internally. It contains the user defined 'power function'
  and some information about what this function returns.
  
  \code{showMethods(class = "powFun")} shows what methods are available.
  
  Contains the Slots:
  \describe{
    \item{\code{statistic}:}{Object of class \code{"function"}}
    \item{\code{return.power}:}{Object of class \code{"logical"},
    indicating if the 'power function' returns a \code{"logical"}.}
    \item{\code{return.list}:}{Object of class \code{"logical"},
    indicating if the 'power function' returns a \code{"list"}.}
    \item{\code{return.n}:}{Object of class \code{"integer"}, indicating
    the number of elements returned by the 'power function'.}
    \item{\code{return.names}:}{Object of class \code{"character"},
    indicating the name of the elements returned by the 'power
    function'.}
  }
}

\section{powCalc-class}{
  A class used internally. Objects can be created by calls to the
  function \code{\link{powCalc}}.

  \code{showMethods(class = "powCalc")} shows what methods are available.
  
  Contains the Slots:
  \describe{
    \item{\code{core}:}{Object of class \code{"array"} with 4
      dimentions, n, theta, xi and endpoint.}
    \item{\code{core}:}{Object of class \code{"array"} with 4
      dimentions, n, theta, xi and endpoint.}
    \item{\code{statistic}:}{Object of class \code{"function"}, this
      function was used for calculating the power.}
    \item{\code{endpoint.name}:}{Object of class \code{"character"}
      names of the endpoints}
    \item{\code{iter}:}{Object of class \code{"integer"} how many
      iterations were used to estimate the power}
  }
}

\section{powEx-class}{
  A class used internally. Objects can be created by calls to the
  function \code{\link{powEx}} but are never visible because they are
  directly included in \linkS4class{power}.

  \code{showMethods(class = "powEx")} shows what methods are available.
  
  Contains the Slots:
  \describe{
    \item{\code{theta.example}:}{Object of class \code{"numeric"}, the
      value of the parameter 'theta' used for illustration and for
      estimating sample size.}
    \item{\code{xi.example}:}{Object of class \code{"numeric"} Like
      'theta.example' but for 'xi'.}
    \item{\code{endpoint.example}:}{Object of class \code{"character"},
      the endpoint to be used, only if several endpoints were evaluated.}
    \item{\code{power.example}:}{Object of class \code{"numeric"},
      indicating for which power the sample size should be evaluated and
      which power (contourline) should be used for illustration
      \code{\link{plot}}.}
    \item{\code{method}:}{Object of class \code{"character"}, indicating
      which 'method' was used for sample size estimation, see
      \code{\link{powEx}}.}
    \item{\code{lm.range}:}{Object of class \code{"numeric"}, indicating
      which 'lm.range' was used for sample size estimation, see
      \code{\link{powEx}}.}
    \item{\code{drop}:}{Object of class \code{"numeric"}, indicating
      which drop out rate was used for sample size estimation, see
      \code{\link{powEx}}.}
    \item{\code{forceDivisor}:}{Object of class \code{"logical"}, indicating
      whether a divisability was forced for sample size estimation, see
      \code{\link{powEx}}.}
    \item{\code{divisor}:}{Object of class \code{"logical"}, indicating
      which 'divisor' was enforced for sample size estimation, see
      \code{\link{powEx}}.}
  }
}

\section{power-class}{
  A class used internally. Objects can be created by calls to the
  function \code{\link{powEx}}.
  
  \code{showMethods(class = "power")} shows what methods are available.

  For the users important are \code{\link{inspect}}, \code{\link{plot}},
  \code{\link{tex}}.
}

\author{\email{thomas.fabbro@unibas.ch}}

\examples{
showClass("power")
}

\keyword{internal}
\keyword{classes}
\keyword{methods}
