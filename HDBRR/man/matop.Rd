\name{matop}
\docType{package}
\alias{matop}
\title{
matop
}
\description{
Compute the SVD or QR decomposition of the matrix X.
}
\usage{
matop(y = NULL, X, method = c("svd", "qr"), bigmat = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
The data vector (numeric, n) NAs allowed. The default value is NULL, It is possible to compute the SVD or QR decomposition without y.
}
  \item{X}{
Design Matrix of dimension n x p.
}
  \item{method}{
Options for the posterior computation. Two methods, \code{"qr"} and \code{"svd"} decomposition. The default value for the method is SVD descomposition.
}
\item{bigmat}{
Use of the bigstatsr package. The default value for bigmat is \code{TRUE}.
}
}
\details{
Use the bigstartsr package when \code{p >> n}. Auxiliary in the HDBRR function.
}
\value{
If the method used is svd then the list containing the following components:
  \item{y}{
The data vector (numeric, \code{n}) NAs allowed.
  }
  \item{X}{
Design Matrix of dimension \code{n x p}.
  }
  \item{D}{
A vector containing the singular values of \code{X}, of lenght \code{min(n,p)}.
  }
  \item{L}{
A matrix whose columns contain the left singular vectors of \code{X},
  }
  \item{R}{
A matrix whose columns contain the right singular vectors of \code{X}.
  }
  \item{ev}{
A vector containing the square of \code{D}.
  }
  \item{Ly}{
The cross-product between the matrix \code{L} and vector \code{y}.
  }
  \item{n}{
Number of rows of \code{X}.
  }
  \item{p}{
Number of columns of \code{X}.
  }
If the method used is qr then the list containing the following components:
\item{y}{
The data vector (numeric, \code{n}) NAs allowed.
  }
  \item{X}{
Design Matrix of dimension \code{n x p}.
  }
  \item{R}{
An upper triangular matrix of dimension \code{n x p}.
  }
  \item{n}{
Number of rows of \code{X}.
  }
  \item{p}{
Number of columns of \code{X}.
  }

}

\author{
Sergio Perez-Elizalde, Blanca E. Monroy-Castillo, Paulino Perez-Rodriguez.
}

\examples{
n <- 30
p <- 100
X <- matrix(rnorm(n*(p-1),1,1/p),nrow = n,ncol = p-1)
Beta <- sample(1:p,p-1,rep = FALSE)
Beta <- c(1,Beta)
y <- cbind(rep(1,n),X) \%*\% Beta+rnorm(n,0,1)
matop(y, X, bigmat = TRUE)
}
\seealso{
\code{\link{qr}}, \code{\link{svd}}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
