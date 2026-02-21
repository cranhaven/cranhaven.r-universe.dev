\name{demo}
\alias{demo}
\docType{data}
\title{
GWAS p_values demo data
}
\description{
GWAS p-values (tab delimited file). 
First Column must contain the SNP ids and the column name = "name"
}
\usage{data(demo)}
\format{
  A data frame with SNPs identifiers and gwas p-values of association
  \describe{
    \item{\code{name}}{a character vector}
    \item{\code{Trait1}}{a numeric vector}
    \item{\code{Trait2}}{a numeric vector}
    \item{\code{Trait3}}{a numeric vector}
    \item{\code{Trait4}}{a numeric vector}
    \item{\code{Trait5}}{a numeric vector}
    \item{\code{Trait6}}{a numeric vector}
    \item{\code{Trait7}}{a numeric vector}
    \item{\code{Trait8}}{a numeric vector}
    \item{\code{Trait9}}{a numeric vector}
  }
  \preformatted{
	name       Trait1      Trait2    Trait3   Trait4 	Trait5    Trait6
	rs10000010 0.9122360 0.30088096 0.2332038 0.5193068 0.1255104 0.07253145 
	rs10000023 0.8642906 0.52064064 0.9243443 0.7177759 0.9512171 0.81716250 
	rs10000030 0.2832705 0.99021664 0.8359339 0.9662707 0.8491221 0.50208681 

  }
}
\examples{
#Read input demo file for "read_pvals" function
data(demo)
}
\keyword{datasets}