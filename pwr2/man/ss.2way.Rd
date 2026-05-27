\name{ss.2way}
\alias{ss.2way}
\title{
Sample size calculation for balanced two-way ANOVA models
}
\description{
Calculate sample size for two-way ANOVA models.
}
\usage{
ss.2way(a=a, b=b, alpha=alpha, beta=beta, f.A=NULL, f.B=NULL, 
delta.A=NULL, delta.B=NULL, sigma.A=NULL, sigma.B=NULL, B=B)
}

\arguments{
  \item{a}{
Number of groups in Factor A
}
  \item{b}{
Number of groups in Factor B
}
  \item{alpha}{
Significant level (Type I error probability)
}
  \item{beta}{
Type II error probability (Power=1-beta)
}
  \item{f.A}{
Effect size of Factor A
}
  \item{f.B}{
Effect size of Factor B
}
  \item{delta.A}{
The smallest difference among a groups in Factor A
}
  \item{delta.B}{
The smallest difference among b groups in Factor B
}
  \item{sigma.A}{
Standard deviation, i.e. square root of variance in Factor A
}
  \item{sigma.B}{
Standard deviation, i.e. square root of variance in Factor B
}
  \item{B}{
Iteration times, default number is 100
}
}

\details{
Beta is the type II error probability which equals 1-power. For example, if the target power is 85\% (=0.85), the corresponding beta equals 0.15. If effect size f is known, plug it in to the function; If delta and sigma are known instead of effect size, put NULL to f. 
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
ss.2way(a=3, b=3, alpha=0.05, beta=0.1, f.A=0.4, f.B=0.2, B=100)
ss.2way(a=3, b=3, alpha=0.05, beta=0.1, f.A=0.4, f.B=0.2, 
delta.A=NULL, delta.B=NULL, sigma.A=NULL, sigma.B=NULL, B=100)

## Example 2
ss.2way(a=3, b=3, alpha=0.05, beta=0.1, delta.A=1, delta.B=2, sigma.A=2, sigma.B=2, B=100)
}

\keyword{Two-Way ANOVA}
\keyword{Sample Size}
