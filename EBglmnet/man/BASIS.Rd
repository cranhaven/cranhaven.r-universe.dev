\name{BASIS}
\alias{BASIS}
\docType{data}
\title{An Example Data}
\description{This is a 1000x481 sample feature matrix}
\usage{data(BASIS)}
\format{
  The format is:
 int [1:1000, 1:481] 0 -1 0 0 1 0 1 0 1 0 ...
}
\details{The data was simulated on a 2400 centimorgan (cM) chromosome of an F2 population from 
a cross of two inbred lines. The three genotype of AA, Aa and aa were coded as
1, 0, -1, respectively. Each column corresponds to an even spaced d= 5cM genetic marker, 
and each row represents a sample. The Haldane map function was assumed in the simulation,
such that correlation between markers having distance d is \eqn{R = exp(-2d)}. Example of using this dataset for multiple QTL mapping is available in the EBglmnet Vignette.}
\source{ 	Huang, A., Xu, S., and Cai, X. (2014). Empirical Bayesian elastic net for multiple quantitative trait locus mapping. Heredity 10.1038/hdy.2014.79 \cr
}

\examples{
data(BASIS)
}
\keyword{datasets}
