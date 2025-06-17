\name{breakpoint-package}
\alias{breakpoint-package}
\alias{breakpoint}
\docType{package}
\title{Multiple Break-Point Detection via the Cross-Entropy Method}
\description{
The breakpoint package implements variants of the Cross-Entropy (CE) method proposed in Priyadarshana and Sofronov (2015, 2012a and 2012b) to estimate both the number and the corresponding locations of break-points in biological sequences of continuous and discrete measurements. The proposed method primarily built to detect multiple break-points in genomic sequences. However, it can be easily extended and applied to other problems.
}
\details{
\tabular{ll}{
Package: \tab breakpoint\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2016-01-11\cr
License: \tab GPL 2.0\cr
}
"breakpoint"" package provides estimates on both the number as well as the corresponding locations of break-points. The algorithms utilize the Cross-Entropy (CE) method, which is a model-based stochastic optimization procedure to obtain the estimates on locations. Model selection procedures are used to obtain the number of break-points. Current implementation of the methodology works as an exact search method in estimating the number of break-points. However, it supports calculations if the initial locations are provided. A parallel implementation of the procedures can be carried-out in Unix/Linux/MAC OSX and WINDOWS OS with the use of "parallel" and "doParallel" packages. 
}

\author{
Priyadarshana, W.J.R.M. and Sofronov, G.

Maintainer: Priyadarshana, W.J.R.M. <mjayawardana@swin.edu.au>
}
\references{
 Priyadarshana, W. J. R. M., Sofronov G. (2015). Multiple Break-Points Detection in Array CGH Data via the Cross-Entropy Method, IEEE/ACM Transactions on Computational Biology and Bioinformatics, 12 (2), pp.487-498.

  Priyadarshana, W. J. R. M. and Sofronov, G. (2012a). A Modified Cross- Entropy Method for Detecting Multiple Change-Points in DNA Count Data. In Proc. of the IEEE Conference on Evolutionary Computation (CEC), 1020-1027, DOI: 10.1109/CEC.2012.6256470.
  
  Priyadarshana, W. J. R. M. and Sofronov, G. (2012b). The Cross-Entropy Method and Multiple Change-Points Detection in Zero-Inflated DNA read count data. In: Y. T. Gu, S. C. Saha (Eds.) The 4th International Conference on Computational Methods (ICCM2012), 1-8, ISBN 978-1-921897-54-2.
  
  Rubinstein, R., and Kroese, D. (2004) The Cross-Entropy Method: A Unified Approach to Combinatorial Optimization, Monte-Carlo Simulation and Machine Learning. Springer-Verlag, New York.
  
  Zhang, N.R., and Siegmund, D.O. (2007) A modified Bayes information criterion with applications to the analysis of comparative genomic hybridization data. Biometrics, 63, 22-32.
}
