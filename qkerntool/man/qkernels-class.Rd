
\name{qkernel-class}
\docType{class}

\alias{qkernel-class}
\alias{qpar,qkernel-method}
\alias{show,qkernel-method}

\alias{rbfqkernel-class}
\alias{nonlqkernel-class}
\alias{laplqkernel-class}
\alias{ratiqkernel-class}
\alias{multqkernel-class}
\alias{invqkernel-class}
\alias{wavqkernel-class}
\alias{powqkernel-class}
\alias{logqkernel-class}
\alias{cauqkernel-class}
\alias{chiqkernel-class}
\alias{studqkernel-class}
\alias{kfunction-class}
\alias{input-class}

\title{Class "qkernel" "rbfqkernel" "nonlqkernel" "laplqkernel" "ratiqkernel"}
\description{The built-in kernel classes in \pkg{qkerntool}}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rbfqkernel")},
\code{new{"nonlqkernel"}}, \code{new{"laplqkernel"}},
\code{new{"ratiqkernel"}}, \code{new{"multqkernel"}},
\code{new{"invqkernel"}}, \code{new{"wavqkernel"}},
\code{new{"powqkernel"}}, \code{new{"logqkernel"}},
\code{new{"cauqkernel"}}, \code{new{"chiqkernel"}},
\code{new{"studqkernel"}}

or by calling the \code{rbfbase}, \code{nonlbase}, \code{laplbase}, \code{ratibase},
\code{multbase}, \code{invbase}, \code{wavbase}, \code{powbase},
\code{logbase}, \code{caubase}, \code{chibase}, \code{studbase} functions etc..
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"function"} containing
      the kernel function}
    \item{\code{qpar}:}{Object of class \code{"list"} containing the
      kernel parameters }
  }
}

\section{Methods}{
  \describe{
    \item{qkernmatrix}{\code{signature(kernel = "rbfqkernel", x =
	"matrix")}: computes the qkernel matrix}

  }
}

\author{Yusen Zhang\cr \email{yusenzhang@126.com} }



\seealso{\code{\link{qkernmatrix}},\code{\link{cndkernmatrix}}}
\examples{
qkfunc <- rbfbase(sigma=1,q=0.8)
qkfunc

qpar(qkfunc)

## create two vectors
x <- rnorm(10)
y <- rnorm(10)

## calculate dot product
qkfunc(x,y)

}
\keyword{classes}
