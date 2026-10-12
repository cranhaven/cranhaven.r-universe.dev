\name{PAS-package}
\alias{PAS}
\docType{package}
\title{
Polygenic Analysis System (PAS)
}
\description{
The PAS package was developed to implement the method and algorithm developed by Zhiqiu Hu, Shizhong Xu, Zhiquan Wang, and Rongcai Yang for genomic value prediction. Although the current version of the package only provided functions for the bin model analysis (Hu et al., 2012), the package will be developed continuously to incorporate new methods of genomic value prediction that will be introduced by the authors in the near future.\cr\cr
\bold{updates:}\cr
1. A new option \emph{foldid} was added into the binmod function to allow users assigning foldid for cross-validations;\cr
2. A new output item \emph{obj$optimal$map.binsnp} was added bridging the bin map and the snp map in a binmod object.\cr
}
\details{
\tabular{ll}{
Package: \tab PAS\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-04-12\cr
License: GPL (>2.0)\cr
}

}
\author{
Zhiqiu Hu, Shizhong Xu, Zhiquan Wang, Rong-cai Yang

Maintainer: Zhiqiu Hu <Zhiqiu.hu@gmail.com>
}
\references{
  
    {\code{bin model}}{\cr Zhiqiu Hu, Zhiquan Wang, and Shizhong Xu (2012) An infinitesimal model for quantitative trait genomic value prediction. PloS ONE 7: e41336.}
  
}
\keyword{ package }
