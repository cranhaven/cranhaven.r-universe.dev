\name{thetaSmoothingNks}
\alias{thetaSmoothingNks}

\title{
  Smooth estimates degree frequencies.
}
\description{
  Smoothes estimated \eqn{N_k} by assuming that \eqn{\beta_k=\beta_0 * k^\theta}.
}
\usage{
  thetaSmoothingNks(rds.object, bin=1)
}

\arguments{
  \item{rds.object}{A \code{rds-object} class object as returned by \code{\link{Estimate.b.k}}}
  \item{bin}{Number of degrees to bin together for estimation.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A numeric vector of smoothed \eqn{N_k} values.}

\author{
Jonathan D. Rosenblatt \email{john.ros.work@gmail.com}
}

\seealso{
  \code{\link{Estimate.b.k}},
  \code{\link{getTheta}}
}

\examples{
# See estimate.b.k()
}
