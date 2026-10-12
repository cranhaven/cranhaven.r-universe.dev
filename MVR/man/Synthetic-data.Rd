\name{Synthetic}

\docType{data}

\alias{Synthetic}

\title{Multi-Groups Synthetic Dataset}

\description{
    Generation of a synthetic dataset with \emph{n}=10 observations (samples) and \eqn{p=100} variables, 
    where \eqn{nvar=20} of them are significantly different between the two sample groups.
    
    This is a balanced design with two sample groups (\eqn{G=2}), under unequal sample group variance.
}

\usage{Synthetic}

\format{
    A numeric matrix containing \eqn{n=10} observations (samples) by rows 
    and \eqn{p=100} variables by columns, named \eqn{v_{1},...,v_{p}}.
    Samples are balanced (\eqn{n_{1}=5},\eqn{n_{2}=5}) between the two groups (\eqn{G_{1}, G_{2}}).
    Compressed Rda data file.
}

\source{See model #2 in Dazard et al., 2011, 2012.}

\author{
    \itemize{
        \item "Jean-Eudes Dazard, Ph.D." \email{jean-eudes.dazard@case.edu}
        \item "Hua Xu, Ph.D." \email{huaxu77@gmail.com}
        \item "Alberto Santana, MBA." \email{ahs4@case.edu}
    }
    Maintainer: "Jean-Eudes Dazard, Ph.D." \email{jean-eudes.dazard@case.edu}    
}

\references{
    \itemize{
        \item Dazard J-E. and J. S. Rao (2010). "\emph{Regularized Variance Estimation and Variance Stabilization of High-Dimensional Data.}"
              In JSM Proceedings, Section for High-Dimensional Data Analysis and Variable Selection. 
              Vancouver, BC, Canada: American Statistical Association IMS - JSM, 5295-5309.
        \item Dazard J-E., Hua Xu and J. S. Rao (2011). "\emph{R package MVR for Joint Adaptive Mean-Variance Regularization and Variance Stabilization.}"
              In JSM Proceedings, Section for Statistical Programmers and Analysts. 
              Miami Beach, FL, USA: American Statistical Association IMS - JSM, 3849-3863.
        \item Dazard J-E. and J. S. Rao (2012). "\emph{Joint Adaptive Mean-Variance Regularization and Variance Stabilization of High Dimensional Data.}"
              Comput. Statist. Data Anal. 56(7):2317-2333.
    }
}

\section{Acknowledgments}{
    This work made use of the High Performance Computing Resource in the Core Facility for Advanced Research Computing at Case Western Reserve University. 
    This project was partially funded by the National Institutes of Health (P30-CA043703).
}

\keyword{datasets}

