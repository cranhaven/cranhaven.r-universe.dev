\name{HiCseg-package}
\alias{HiCseg-package}
\alias{HiCseg}
\docType{package}
\title{
Analysis of HiC data
}
\description{
Two-dimensional segmentation for analyzing HiC data
}
\details{
\tabular{ll}{
Package: \tab HiCsegv6\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-25\cr
License: \tab GPL-2\cr
}
}
\author{
Celine Levy-Leduc

Maintainer: <celine.levy-leduc@agroparistech.fr>
C. Levy-Leduc
}
\examples{
library(HiCseg)
data(matrix)
n=200
Kmax=10
res=HiCseg_linkC_R(n,Kmax,"G",matrix,"D")
print(res)
}
