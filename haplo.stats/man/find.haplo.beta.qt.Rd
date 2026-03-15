\name{find.haplo.beta.qt}
\alias{find.haplo.beta.qt}
\alias{find.beta.qt.phase.known}
\alias{find.intercept.qt.phase.known}
\title{
  Find beta coefficients for risk haplotypes, for specified r2
}
\description{
  Find betas for risk haplotypes and intercept (beta for base.index
  haplotype) with a given r2
}
\usage{
find.haplo.beta.qt(haplo, haplo.freq, base.index, haplo.risk, r2, y.mu=0, y.var=1)
find.beta.qt.phase.known(beta.size, haplo.risk, base.index, haplo,
haplo.freq, r2, y.mu, y.var)
find.intercept.qt.phase.known(beta.no.intercept, base.index, haplo, haplo.freq, y.mu)
}
\arguments{

\item{haplo}{
 matrix of haplotypes, with rows the different haplotypes and
columns the alleles of the haplotypes. For H haplotypes of L loci,
haplo has dimension H x L.
}
\item{haplo.freq }{
vector of length H for the population haplotype frequencies
(corresponding to the rows of haplo)
}
\item{base.index }{
integer index of the haplotype considered to be the base-line for
logistic regression (index between 1 and H); often, the most common
haplotype is chosen for the base-line.
}
\item{haplo.risk }{
vector of relative risks for haplotypes
}
\item{r2}{
correlation coefficient
}
\item{y.mu }{
mean of y, a quantitative trait
}
\item{y.var }{
variance of y, a quantitative trait
}
\item{beta.size}{
beta values for risk haplotypes in find.beta.qt.phase.known
}
\item{beta.no.intercept}{
  beta vector for haplotypes for quantitative trait,
  excluding the beta for intercept 
}
}
\value{
  beta estimates for haplotypes or intercept
}
\keyword{power}
% docclass is function
% Converted by Sd2Rd version 43268.
