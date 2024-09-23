\name{checkdata}
\alias{checkdata}
\title{
A function used to check the core inputs of function glide
}
\description{
It is used to check the core inputs of function glide
}
\usage{
checkdata(formula,exposure_coeff,genotype_columns,data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A an object of class "formula": outcome ~ adjusting covariates.}
  \item{exposure_coeff}{
A named mumeric vector or a one-column dataframe for the external coefficients 
for genotype-exposure associations.}
  \item{genotype_columns}{An integer vector, column indices of data in which genotype data are stored.}
  \item{data}{A data frame composed of formula-related components and snp genotype columns.}
}
\value{No return value, would generate error messages if input data are not right.}
\author{
James Y. Dai and X. Wang
}
\examples{
#load an example data; it includes a dataframe "simdat" and a one-column dataframe  "coeff"
data(simdata)
simdat=simdata$simdat
coeff=simdata$coeff
formula=as.formula("outcome~age+sex+pc1+pc2+pc3")
genotype_columns=which(grepl("^SNP",colnames(simdat)))
checkdata(formula=formula,exposure_coeff=coeff,genotype_columns,data=simdat)
}
