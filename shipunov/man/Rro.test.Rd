\name{Rro.test}
\alias{Rro.test}
\title{Robust rank order test}

\description{Robust rank order test}

\usage{Rro.test(x1, y1)}

\arguments{
\item{x1}{Fist numerical variable}
\item{y1}{Second numerical variable}
}

\details{

Robust rank order test (modification of Wilcoxon test for samples with
contrasting variation), a variant of Fligner-Policello test.

Alternatives: robustrank::mod.wmw.test() (probably more sophisticated);
npsm::fp.test(); NSM3::pFligPoli() (very advanced, with possibilities of
exact and Monte Carlo testing); RVAideMemoire::fp.test() (developed in
the way similar to most base R tests, probably the best alternative).

}

\value{Returns z statistic and p-value.}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
## data from help(wilcox.test)
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
Rro.test(x, y)
}

\keyword{htest}
