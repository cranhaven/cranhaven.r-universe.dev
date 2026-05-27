\name{ss.1way}
\alias{ss.1way}
\title{
Sample size calculation for balanced one-way ANOVA models
}
\description{
Calculate sample size for one-way ANOVA models.
}
\usage{
ss.1way(k=k, alpha=alpha, beta=beta, f=NULL, delta=delta, sigma=sigma, B=B)
}

\arguments{
  \item{k}{
Number of groups
}
  \item{alpha}{
Significant level (Type I error probability)
}
  \item{beta}{
Type II error probability (Power=1-beta)
}
  \item{f}{
Effect size
}
  \item{delta}{
The smallest difference among k group
}
  \item{sigma}{
Standard deviation, i.e. square root of variance
}
  \item{B}{
Iteration times, default number is 100
}
}

\details{
Beta is the type II error probability which equals 1-power. For example, if the target power is 85\% (=0.85), the corresponding beta equals 0.15. If effect size f is known, plug it in to the function; If delta and sigma are known instead of effect size, put NULL to f, or just miss f argument.
}

\value{
Object of class "power.htest", a list of the arguments (including the computed one) augmented with "method" and "note" elements.
}

\author{
Pengcheng Lu, Junhao Liu, and Devin Koestler.  
}

\references{
Angela Dean & Daniel Voss (1999). Design and Analysis of Experiments. Springer.
}

\examples{
## Example 1
ss.1way(k=5, alpha=0.05, beta=0.1, f=1.5, B=100)

## Example 2
ss.1way(k=5, alpha=0.05, beta=0.1, delta=1.5, sigma=1, B=100)
ss.1way(k=5, alpha=0.05, beta=0.1, f=NULL, delta=1.5, sigma=1, B=100)
}

\keyword{One-Way ANOVA}
\keyword{Sample Size}

