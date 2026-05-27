\name{pwr.1way}
\alias{pwr.1way}
\title{
Power calculation for balanced one-way ANOVA models
}
\description{
Calculate power for one-way ANOVA models.
}
\usage{
pwr.1way(k=k, n=n, alpha=alpha, f=NULL, delta=delta, sigma=sigma)
}

\arguments{
  \item{k}{
Number of groups
}
  \item{n}{
Sample size per group
}
  \item{f}{
Effect size
}
  \item{alpha}{
Significant level (Type I error probability)
}
  \item{delta}{
The smallest difference among k groups
}
  \item{sigma}{
Standard deviation, i.e. square root of variance
}
}

\details{
If effect size f is known, plug it in to the function; If delta and sigma are known instead of effect size, put NULL to f. 
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
pwr.1way(k=5, n=15, alpha=0.05, delta=1.5, sigma=1)
pwr.1way(k=5, n=15, f=NULL, alpha=0.05, delta=1.5, sigma=1)

## Example 2
pwr.1way(k=5, n=15, f=0.4, alpha=0.05)
}

\keyword{One-Way ANOVA}
\keyword{Power}
