\name{cndkernel-class}
\docType{class}
\alias{cndkernel-class}

\alias{qpar,cndkernel-method}
\alias{show,cndkernel-method}


\alias{nonlkernel-class}
\alias{polykernel-class}
\alias{rbfkernel-class}
\alias{laplkernel-class}
\alias{anokernel-class}
\alias{ratikernel-class}
\alias{multkernel-class}
\alias{invkernel-class}
\alias{wavkernel-class}
\alias{powkernel-class}
\alias{logkernel-class}
\alias{caukernel-class}
\alias{chikernel-class}
\alias{studkernel-class}
\alias{norkernel-class}





\title{Class "cndkernel" "nonlkernel" "polykernel" "rbfkernel" "laplkernel"}
\description{  The built-in kernel classes in \pkg{qkerntool}}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("nonlkernel")},
\code{new{"polykernel"}}, \code{new{"rbfkernel"}},
\code{new{"laplkernel"}}, \code{new{"anokernel"}},
\code{new{"ratikernel"}}, \code{new{"multkernel"}},
\code{new{"invkernel"}}, \code{new{"wavkernel"}},
\code{new{"powkernel"}}, \code{new{"logkernel"}},
\code{new{"caukernel"}}, \code{new{"chikernel"}},
\code{new{"studkernel"}},\code{new{"norkernel"}}


or by calling the \code{nonlcnd},\code{polycnd}, \code{rbfcnd},
\code{laplcnd}, \code{anocnd}, \code{raticnd}, \code{multcnd},
\code{invcnd}, \code{wavcnd}, \code{powcnd}, \code{logcnd},
\code{caucnd}, \code{chicnd}, \code{studcnd}, \code{norcnd}  functions etc..
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"function"} containing
      the kernel function }
    \item{\code{qpar}:}{Object of class \code{"list"} containing the
      kernel parameters }
  }
}

\section{Methods}{
  \describe{
    \item{cndkernmatrix}{\code{signature(kernel = "rbfkernel", x ="matrix")}: computes the kernel matrix}
  }
}

\author{Yusen Zhang\cr \email{yusenzhang@126.com} }



\seealso{\code{\link{qkernmatrix}},\code{\link{cndkernmatrix}}}
\examples{
cndkfunc <- rbfcnd(gamma = 1)
cndkfunc

qpar(cndkfunc)

## create two vectors
x <- rnorm(10)
y <- rnorm(10)


cndkfunc(x,y)
}

\keyword{classes}
