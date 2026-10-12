\name{target.diagnostic}

\alias{target.diagnostic}

\title{Function for Plotting Summary Target Moments Diagnostic Plots}

\description{
    Plot comparative distribution densities of means and standard deviations of the data before and after Mean-Variance Regularization
    to check for location shifts between observed first and second moments and their expected target values under a target centered homoscedastic model. 

    Plot comparative QQ scatterplots to look at departures between observed distributions of first and second moments of the MVR-transformed data
    and their theoretical distributions assuming independence and normality of all the variables.
}

\usage{
    target.diagnostic(obj, 
                      title = "Target Moments Diagnostic Plots",
                      device = NULL, 
                      file = "Target Moments Diagnostic Plots",
                      path = getwd(),
                      horizontal = FALSE, 
                      width = 8.5, 
                      height = 6.5, ...)
}

\arguments{
  \item{obj}{Object of class "\code{mvr}" returned by \code{\link[MVR]{mvr}}.}
  \item{title}{Title of the plot. Defaults to "Target Moments Diagnostic Plots".}
  \item{device}{Graphic display device in \{NULL, "PS", "PDF"\}. Defaults to NULL (standard output screen).
                Currently implemented graphic display devices are "PS" (Postscript) or "PDF" (Portable Document Format).}
  \item{file}{File name for output graphic. Defaults to "Target Moments Diagnostic Plots".}
  \item{path}{Absolute path (without final (back)slash separator). Defaults to working directory path.}
  \item{horizontal}{\code{Logical} scalar. Orientation of the printed image. Defaults to \code{FALSE}, that is potrait orientation.}
  \item{width}{\code{Numeric} scalar. Width of the graphics region in inches. Defaults to 8.5.}
  \item{height}{\code{Numeric} scalar. Height of the graphics region in inches. Defaults to 6.5.}
  \item{\dots}{Generic arguments passed to other plotting functions.}
}

\details{
    The plots of the density distribution of means and standard deviations checks that the distributions of means and standard deviations 
    of the MVR-transformed data have correct target first moments, i.e. with mean ~ 0 and mean ~ 1. The expected target mean and standard 
    deviation are shown in red (before and) after MVR-transformation. Caption shows the p-values from the parametric two-sample two-sided t-tests 
    for the equality of parameters to their expectations (assuming normality since usually sample sizes are large : \eqn{p \gg 1}, 
    or a relative robustness to moderate violations of the normality assumption).
    
    In the general case, the variables are not normally distributed and not even independent and identically distributed before 
    and after MVR-transformation. Therefore, the distributions of untransformed first and second moments usually differ from their respective 
    theoretical null distributions, i.e., from \eqn{N(0, \frac{1}{n})} for the means and from \eqn{\sqrt{\frac{\chi_{n - G}^{2}}{n - G}}} for the standard 
    deviations, where \eqn{G} denotes the number of sample groups (see \cite{Dazard, J-E. and J. S. Rao (2012)} for more details). 
    Also, the observed distributions of transformed first and second moments are unknown. This is reflected in the QQ plots, 
    where theoretical and empirical quantiles do not necessarily align with each other. Caption shows the p-values from the nonparametric two-sample 
    two-sided Kolmogorov-Smirnov tests of the null hypothesis that a parameter distribution differs from its theoretical distribution. 
    Each black dot represents a variable. The red solid line depicts the interquartile line, which passes through the first and third quartiles. 

    Option \code{file} is used only if device is specified (i.e. non \code{NULL}).
}

\value{
    None. Displays the plots on the chosen \code{device}.
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

\note{
    End-user function.
}

\seealso{
    \code{justvsn} (R package \pkg{vsn}) Variance stabilization and calibration for microarray data.
    \code{loess} (R package \pkg{stats}) Fit a polynomial surface determined by one or more numerical predictors, using local fitting.
}

\examples{
\dontrun{
    #===================================================
    # Loading the library and its dependencies
    #===================================================
    library("MVR")
    library("RColorBrewer")

    #===================================================
    # MVR package news
    #===================================================
    MVR.news()

    #================================================
    # MVR package citation
    #================================================
    citation("MVR")

    #===================================================
    # Loading of the Synthetic and Real datasets
    # (see description of datasets)
    #===================================================
    data("Synthetic", "Real", package="MVR")
    ?Synthetic
    ?Real

    #===================================================
    # Mean-Variance Regularization (Real dataset)
    # Multi-Group Assumption
    # Assuming unequal variance between groups
    # Without cluster usage
    #===================================================
    nc.min <- 1
    nc.max <- 30
    probs <- seq(0, 1, 0.01)
    n <- 6
    GF <- factor(gl(n = 2, k = n/2, length = n), 
                 ordered = FALSE, 
                 labels = c("M", "S"))
    mvr.obj <- mvr(data = Real, 
                   block = GF, 
                   log = FALSE, 
                   nc.min = nc.min, 
                   nc.max = nc.max, 
                   probs = probs,
                   B = 100, 
                   parallel = FALSE, 
                   conf = NULL,
                   verbose = TRUE,
                   seed = 1234)

    #===================================================
    # Summary Target Moments Diagnostic Plots (Real dataset)
    # Multi-Group Assumption
    # Assuming unequal variance between groups
    #===================================================
    target.diagnostic(obj = mvr.obj, 
                      title = "Target Moments Diagnostic Plots 
                      (Real - Multi-Group Assumption)",
                      device = NULL,
                      horizontal = FALSE, 
                      width = 8.5, 
                      height = 6.5)
    }
}
\keyword{Mean-Variance Estimators}
\keyword{Regularization}
\keyword{Variance Stabilization}
\keyword{Normalization}

