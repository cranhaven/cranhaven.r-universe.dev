\name{M.fct}
\alias{M.fct}
\title{
Marginalizing Matrix Based on Strata Information
}
\description{
Creates the marginalizing matrix \eqn{M} using strata information found in input variable \code{strata}. That is, \eqn{Mp} gives the marginal probabilities corresponding to the levels of factor \code{strata}.
}
\usage{
M.fct(strata, ncells = length(strata))
}
\arguments{
  \item{strata}{The vector that gives the stratum membership identifier.}
  \item{ncells}{Number of contingency table cells. Default:
                \code{ncells = length(strata)}.}
}
\value{
\code{M.fct} returns the marginalizing matrix \code{M}.
}
\note{
Marginals are ordered according to the levels of factor
\code{strata}.

Examples:

\tabular{crr}{
  \code{V1}  \tab  \code{V2}  \tab    \code{y}\cr
  \code{b} \tab   \code{yes}  \tab   \code{15}\cr
 \code{a}  \tab   \code{no} \tab    \code{12}\cr
  \code{a} \tab   \code{yes}  \tab   \code{13}\cr
 \code{b} \tab   \code{yes}   \tab     \code{5}\cr
  \code{b} \tab    \code{no}  \tab   \code{1}\cr
}

\code{M1 <- M.fct(V1)}\cr
\code{M1 \%*\% y}\cr
\tabular{rr}{
\tab \code{[,1]}\cr
\code{a} \tab  \code{25}\cr
\code{b} \tab  \code{21}\cr
}

\code{M2 <- M.fct(V2)}\cr
\code{M2 \%*\% y}\cr
\tabular{rr}{
\tab \code{[,1]}\cr
\code{no} \tab  \code{13}\cr
\code{yes} \tab  \code{33}\cr
}

\code{M12 <- M.fct(paste(V1, V2))}\cr
\code{M12 \%*\% y}
\tabular{rrr}{
\tab \tab \code{[,1]}\cr
\code{a} \tab \code{no} \tab  \code{12}\cr
\code{a} \tab \code{yes}  \tab \code{13}\cr
\code{b} \tab \code{no}   \tab \code{1}\cr
\code{b} \tab \code{yes}  \tab  \code{20}\cr
}
}
\references{
Lang, J. B. (2004) Multinomial-Poisson homogeneous models for contingency tables, \emph{Annals of Statistics}, \bold{32}, 340--383.
}
\author{
Joseph B. Lang
}
\examples{
M.fct(rep(1, 9))
M.fct(seq(1, 9))
M.fct(c(1, 1, 2, 3, 3))
}
