\name{pwr.plot}
\alias{pwr.plot}
\title{
Power curves for different parameter settings (sample size and effect size) in balanced one-way ANOVA models
}
\description{
Draw power curves for different parameter settings in balanced one-way ANOVA models.
}
\usage{
pwr.plot(n=n, k=k, f=f, alpha=alpha)
}

\arguments{
  \item{n}{
Sample size per group
}
  \item{k}{
Number of groups
}
  \item{f}{
Effect size
}
  \item{alpha}{
Significant level (Type I error probability)
}
}

\details{
This function demonstrates drawing power curves for different sample size and effect size settings. N and f can be either a single value or a sequence of values, but they cannot be single values simultaneously. The combination of them could be (a sequence of n, a sequence of f), (a sequence of n, a single f), or (a single n, a sequence of f).
}

\author{
Pengcheng Lu, Junhao Liu, and Devin Koestler.  
}

\references{
Angela Dean & Daniel Voss (1999). Design and Analysis of Experiments. Springer.
}

\examples{
## Example 1
n <- seq(2, 30, by=4)
f <- 0.5
pwr.plot(n=n, k=5, f=f, alpha=0.05)

## Example 2
n <- 20
f <- seq(0.1, 1.0, length.out=10)
pwr.plot(n=n, k=5, f=f, alpha=0.05)

## Example 3
n <- seq(2, 30, by=4)
f <- seq(0.1, 1.0, length.out=10)
pwr.plot(n=n, k=5, f=f, alpha=0.05)
}

\keyword{Graph}
\keyword{Power Curve}
