\name{pwr.2way}
\alias{pwr.2way}
\title{
Power calculation for balanced two-way ANOVA models
}
\description{
Calculate power for two-way ANOVA models.
}
\usage{
pwr.2way(a=a, b=b, alpha=alpha, size.A=size.A, size.B=size.B, f.A=NULL, f.B=NULL, 
delta.A=NULL, delta.B=NULL, sigma.A=NULL, sigma.B=NULL)
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
  \item{size.A}{
Sample size per group in Factor A
}
  \item{size.B}{
Sample size per group in Factor B
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
}

\details{
If effect sizes f.A and f.B are known, plug them in to the function; If delta.A and sigma.A are known instead of f.A, put NULL to f.A. Similarly as delta.B and sigma.B.
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
pwr.2way(a=3, b=3, alpha=0.05, size.A=4, size.B=5, f.A=0.8, f.B=0.4)

## Example 2
pwr.2way(a=3, b=3, alpha=0.05, size.A=4, size.B=5, delta.A=4, delta.B=2, sigma.A=2, sigma.B=2)
pwr.2way(a=3, b=3, alpha=0.05, size.A=4, size.B=5, f.A=NULL, f.B=NULL, 
delta.A=4, delta.B=2, sigma.A=2, sigma.B=2)
}

\keyword{Two-Way ANOVA}
\keyword{Power}
