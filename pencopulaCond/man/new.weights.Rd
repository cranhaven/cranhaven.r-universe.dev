\name{new.weights}
\alias{new.weights}
\title{Calculating new weights b.}

\description{
Calculating new weights b using quadratic programing.
}

\usage{
new.weights(penden.env,start=FALSE)
}

\arguments{
  \item{penden.env}{Containing all information, environment of pencopula()}
  \item{start}{Default=FALSE, for the first calculation some specifications are introduced.}
}

\details{
  If the quadratic program does not find a new feasible solution, the whole
  program terminates. For solving the quadratic program, we use the
  function 'solve.QP' from the  R-package 'quadprog'.
}

\value{
  \item{ck.val.temp}{Calculated new values for the weights 'b'. The add
    on 'temp' means, that there is a check in the next step if the
    weights 'b' have been converted or not. If converted, the new values
    'ck.val.temp' are unnoted. If not converted, 'ck.val.temp' become
    the ordinary 'ck.val' for the next iteration. This check is done in my.loop.}
  'ck.val.temp' is saved in the environment.
}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Flexible Copula Density Estimation with Penalized
  Hierarchical B-Splines, Kauermann G., Schellhase C. and Ruppert, D. (2013), Scandinavian Journal of Statistics 40(4), 685-705.

Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}
