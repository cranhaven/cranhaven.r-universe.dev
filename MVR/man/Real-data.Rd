\name{Real}

\docType{data}

\alias{Real}

\title{Real Proteomics Dataset}

\description{
    The dataset comes from a quantitative Liquid Chromatography/Mass-Spectrometry (LC/MS) 
    shotgun (bottom-up) proteomics experiment. It consists of \eqn{n=6} independent cell cultures 
    of human of Myeloid Dendritic Cells (MDCs) from normal subjects. Samples were split 
    into a control ("\emph{M}") and a treated group ("\emph{S}"), stimulated with either media alone 
    or a Toll-Like receptor-3 Ligand respectively. The goal was to identify differentially expressed 
    peptides (or proteins) between the two groups involved in the immune response
    of human MDCs upon TLR-3 Ligand binding.
    
    The dataset is assumed to have been pre-processed for non-ignorable missing values, 
    leaving a complete dataset with \eqn{p=9052} unique peptides or predictor variables.
    
    This is a balanced design with two sample groups (\eqn{G=2}), under unequal sample group variance.
}

\usage{Real}

\format{
    A numeric matrix containing \eqn{n=6} observations (samples) by rows and 
    \eqn{p=9052} variables by columns, named after peptide names (\eqn{diffset_{1}, ..., diffset_{p}}).
    Samples are balanced (\eqn{n_{1}=3},\eqn{n_{2}=3}) between the two groups ("\emph{M}", "\emph{S}").
    Compressed Rda data file.
}

\source{See real proteomics data application in Dazard et al., 2011, 2012.}

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

