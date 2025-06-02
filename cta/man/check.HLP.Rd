\name{check.HLP}
\alias{check.HLP}
\title{
HLP Link Status Check
}
\description{
Checks whether the link function \eqn{L(\cdot)} is a candidate HLP link
function.

Specifically, this program checks whether \eqn{L(\cdot)} satisfies
certain necessary conditions that follow from a sufficient condition for HLP
link status.

If the necessary conditions are satisfied then there is corroborating evidence
that \eqn{L(\cdot)} has HLP link status. If the necessary conditions are not satisfied,
then the sufficient condition for HLP link status is not satisfied, so \eqn{L(\cdot)}
may or may not have HLP link status.
}
\usage{
check.HLP(L.fct, Z, tol = NULL)
}
\arguments{
  \item{L.fct}{An \R function object, indicating the link \eqn{L(\cdot)} for HLP
               link status check.}
  \item{Z}{Population (aka strata) matrix \eqn{Z}.}
  \item{tol}{The pre-set tolerance with which \code{norm(diff)} is to be compared
             with.}
}
\details{
The main idea:

The model \eqn{L(m) = X\beta} is an HLP model if \eqn{L(\cdot)} is a smooth link function that satisfies the HLP conditions with respect to \eqn{Z} (i.e. strata \eqn{s}) and \eqn{X}. That is,
\itemize{
\item (1) \eqn{L(\cdot)} has HLP link status with respect to \eqn{Z},   and
\item (2) The implied constraint function \eqn{h(m) = U'L(m)} is \eqn{Z} homogeneous.
      Here, \eqn{null(U') = span(X)}.
}

Here, (1) \eqn{L(\cdot)} has HLP link status with respect to \eqn{Z} if, for \eqn{m = Diag(Z\gamma)p}, equivalently, for \eqn{\gamma = Z'm} and \eqn{p = Diag^{-1}(ZZ'm)m},
\itemize{
\item (1)(a) \eqn{L(m) = a(\gamma) + L(p)}, where \eqn{a(\gamma_{1}/\gamma_{2}) - a(1) = a(\gamma_{1}) - a(\gamma_{2})},
      i.e. \eqn{a(\gamma)} has the form \eqn{C \log\gamma + \texttt{constant}};
                  or
\item (1)(b) \eqn{L(m) = G(\gamma) L(p)}, where \eqn{G(\gamma)} is a diagonal matrix with
         diagonal elements that are powers of the \eqn{\gamma} elements, i.e. \eqn{L(\cdot)}
         is \eqn{Z} homogeneous (see Lang (2004));
                  or
\item (1)(c) The components of \eqn{L(\cdot)} are a mixture of types (a) and (b):
             \eqn{L_{j}(m) = a_{j}(\gamma) + L_{j}(p)} or  \eqn{L_{j}(m) = G_{j}(\gamma) L_{j}(p)},
             \eqn{j = 1, \ldots, l}.
}

N.B. Lang (2005) defined HLP models as those satisfying (1)(a) and (2). \code{mph.fit} uses a broader definition of HLP model. Specifically, models
satisfying (1)(b) and (2) or (1)(c) and (2) are also considered HLP models.

Conditions (1)(b) and (2) can be checked using the \code{check.homog} function.
Condition (1)(c) is not checked.

This function, \code{check.HLP}, is concerned with sufficient condition (1)(a) only. If \eqn{L(\cdot)} satisfies (1)(a)  then
\itemize{
\item (i) \eqn{\texttt{diff1} = [L(Diag(Z\gamma_{1})p_{1}) - L(Diag(Z\gamma_{2})p_{1})] - [L(Diag(Z \gamma_{1}/\gamma_{2})p_{1}) - L(p_{1})] = 0}, and
\item (ii) \eqn{\texttt{diff2} = [L(Diag(Z\gamma_{1})p_{1}) - L(Diag(Z\gamma_{1})p_{2})] - [L(p_{1}) - L(p_{2})] = 0}.
}
Here \eqn{p_{i} = Diag^{-1}(ZZ'm_{i})m_{i}}, where \eqn{m_{i} = Diag(Z\gamma_{i})p_{i}}, \eqn{i = 1, 2}.

This program randomly generates \code{g1} (\eqn{\gamma_{1}}), \code{g2} (\eqn{\gamma_{2}}), \code{p1}, \code{p2}, and
computes \code{norm(diff) = sqrt(norm(diff1)^2 + norm(diff2)^2)}. It returns a warning if \code{norm(diff)} is too far from \eqn{0}.
}
\value{
\code{check.HLP} returns a character string \code{chk}. If \code{chk = ""}, then
there is corroborating evidence that \eqn{L(\cdot)} has HLP link status. If
\code{chk = paste("L(m) may not be an HLP link [based on tol=",tol,"]!")}, then the sufficient condition for HLP link status is not satisfied, so \eqn{L(\cdot)} may or
may not have HLP link status.
}
\references{
Lang, J. B. (2004) Multinomial-Poisson homogeneous models for contingency tables, \emph{Annals of Statistics}, \bold{32}, 340--383.

Lang, J. B. (2005) Homogeneous linear predictor models for contingency tables, \emph{Journal of the American Statistical Association}, \bold{100}, 121--134.
}
\author{
Joseph B. Lang
}
\seealso{
\code{\link{mph.fit}}, \code{\link{check.homog}}, \code{\link{check.zero.order.homog}}
}
\examples{
# 3-by-3-by-3 Table.
# For a description of the model, see Michael Haber's Example 2,
# p. 433, in  Biometrics (in Shorter Communications), Vol. 42,
# No. 2. (Jun., 1986), pp. 429-435.
A <- gl(3, 9, 27)
B <- gl(3, 3, 27)
C <- gl(3, 1, 27)
MAB <- kronecker(diag(9), matrix(1, 1, 3))
MAC <- kronecker(diag(3), kronecker(matrix(1, 1, 3), diag(3)))
MBC <- kronecker(matrix(1, 1, 3), diag(9))
M <- rbind(MAB, MAC, MBC)
Mr <- M[-c(3, 6, 7, 8, 9, 12, 15, 16, 17, 18, 21, 24,
           25, 26, 27), ]
C <- c(1, -1, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 1, -1, -1, 1, 0, 0, 0, 0,
       0, 0, 0, 0, 0, 0, 0, 0, 1, -1, -1, 1)
C <- matrix(C, 3, 12, byrow = TRUE)
L.fct <- function(m) {
  p <- m / sum(m)
  C \%*\% log(Mr \%*\% p)
}
Z <- matrix(rep(1, 27), ncol = 1)
check.HLP(L.fct, Z)
}
