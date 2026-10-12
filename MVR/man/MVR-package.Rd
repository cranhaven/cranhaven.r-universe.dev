\name{MVR-package}
\alias{MVR}
\docType{package}
\title{Mean-Variance Regularization Package}
\description{
    MVR is a non-parametric method for joint adaptive mean-variance regularization
    and variance stabilization of high-dimensional data.

    It is suited for handling difficult problems posed by high-dimensional multivariate datasets 
    (\eqn{p \gg n} paradigm), such as in omics-type data, among which are that the variance is 
    often a function of the mean, variable-specific estimators of variances are not reliable, 
    and tests statistics have low powers due to a lack of degrees of freedom.
    
    Key features include:
    \enumerate{
        \item{Normalization and/or variance stabilization of the data}
        \item{Computation of mean-variance-regularized \emph{t}-statistics (\emph{F}-statistics} to come)
        \item{Generation of diverse diagnostic plots}
        \item{Computationally efficient implementation using C/C++ interfacing 
              and an option for parallel computing to enjoy a fast and easy experience 
              in the R environment \cr}
    }
}

\details{
The following describes all the end-user functions, and internal R subroutines 
needed for running a complete MVR procedure. Other internal subroutines are not 
to be called by the end-user at any time. For computational efficiency, 
end-user regularization functions offer the option to configure a cluster.
This is indicated by an asterisk (* = optionally involving cluster usage).
The R functions are categorized as follows:

\enumerate{

\item{END-USER REGULARIZATION & VARIANCE STABILIZATION FUNCTION} \cr
\code{\link[MVR]{mvr}} (*) 
\bold{Function for Mean-Variance Regularization and Variance Stabilization.} \cr
End-user function for Mean-Variance Regularization (MVR) and Variance Stabilization 
by similarity statistic under sample group homoscedasticity or heteroscedasticity assumption.
The function takes advantage of the R package \pkg{parallel}, which allows users 
to create a cluster of workstations on a local and/or remote machine(s), 
enabling parallel execution of this function and scaling up with the number of CPU 
cores available.\cr
              
\item{END-USER REGULARIZED TESTS-STATISTICS FUNCTIONS} \cr
\code{\link[MVR]{mvrt.test}} (*)
\bold{Function for Computing Mean-Variance Regularized T-test Statistic and Its Significance.}\cr
End-user function for computing MVR t-test statistic and its significance (p-value) 
under sample group homoscedasticity or heteroscedasticity assumption.
The function takes advantage of the R package \pkg{parallel}, which allows users 
to create a cluster of workstations on a local and/or remote machine(s), 
enabling parallel execution of this function and scaling up with the number of CPU 
cores available.\cr

\item{END-USER DIAGNOSTIC PLOTS FOR QUALITY CONTROL} \cr
\code{\link[MVR]{cluster.diagnostic}}
\bold{Function for Plotting Summary Cluster Diagnostic Plots.}\cr
Plot similarity statistic profiles and the optimal joint clustering configuration for the means and the variances by group.
Plot quantile profiles of means and standard deviations by group and for each clustering configuration, to check that the distributions 
of first and second moments of the MVR-transformed data approach their respective null distributions under the optimal configuration found, 
assuming independence and normality of all the variables.\cr

\code{\link[MVR]{target.diagnostic}}
\bold{Function for Plotting Summary Target Moments Diagnostic Plots.}\cr
Plot comparative distribution densities of means and standard deviations of the data before and after Mean-Variance Regularization
to check for location shifts between observed first and second moments and their expected target values under a target centered homoscedastic model. 
Plot comparative QQ scatterplots to look at departures between observed distributions of first and second moments of the MVR-transformed data
and their theoretical distributions assuming independence and normality of all the variables.\cr

\code{\link[MVR]{stabilization.diagnostic}}
\bold{Function for Plotting Summary Variance Stabilization Diagnostic Plots.}\cr
Plot comparative variance-mean plots to check the variance stabilization across variables before and after Mean-Variance Regularization.\cr

\code{\link[MVR]{normalization.diagnostic}}
\bold{Function for Plotting Summary Normalization Diagnostic Plots.}\cr
Plot comparative Box-Whisker and Heatmap plots of variables across samples check the effectiveness of normalization before and after 
Mean-Variance Regularization.\cr

\item{OTHER END-USER FUNCTIONS} \cr
\code{\link[MVR]{MVR.news}}
\bold{Display the \pkg{MVR} Package News}\cr
Function to display the log file \code{NEWS} of updates of the \pkg{MVR} package.\cr

\item{END-USER DATASETS} \cr
A \code{Real} dataset coming from a quantitative proteomics experiment, consisting of \eqn{n=6} 
samples split into a control ("\emph{M}") and a treated group ("\emph{S}") with \eqn{p=9052} 
unique peptides or predictor variables. This is a balanced design with two sample groups (\eqn{G=2}), 
under unequal sample group variance.\cr

A \code{Synthetic} dataset with \eqn{n=10} observations (samples) and \eqn{p=100} variables, 
where \eqn{nvar=20} of them are significantly different between the two sample groups.
This is a balanced design with two sample groups (\eqn{G=2}), under unequal sample group variance.\cr
}

Known Bugs/Problems : None at this time.
}

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

\seealso{
    \itemize{
        \item \code{makeCluster} (R package \pkg{parallel})
        \item \code{justvsn} (R package \pkg{vsn}) Variance stabilization and calibration for microarray data \cite{Huber, 2002}
        \item \code{eBayes} (R package \pkg{limma}) Bayesian Regularized t-test statistic \cite{Smyth, 2004}
        \item \code{samr} (R package \pkg{samr}) SAM Regularized t-test statistic \cite{Tusher et al., 2001, Storey, 2003}
        \item \code{matest} (R package \pkg{maanova}) James-Stein shrinkage estimator-based Regularized t-test statistic \cite{Cui et al., 2005}
        \item \code{ebam} (R package \pkg{siggenes}) Empirical Bayes Regularized z-test statistic \cite{Efron, 2001}
        \item \code{bayesT} Hierarchical Bayesian Regularized t-test statistic \cite{Baldi et al., 2001}
    }
}

\keyword{High-Dimensional Data}
\keyword{Mean-Variance Estimators}
\keyword{Regularization}
\keyword{Variance Stabilization}
\keyword{Normalization}
\keyword{Regularized Test Statistics}
\keyword{Parallel Programming}
\keyword{High Performance Computing}
\keyword{Documentation}
