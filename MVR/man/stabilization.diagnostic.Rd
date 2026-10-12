\name{stabilization.diagnostic}

\alias{stabilization.diagnostic}

\title{Function for Plotting Summary Variance Stabilization Diagnostic Plots}

\description{
    Plot comparative variance-mean plots to check the variance stabilization across variables before and after Mean-Variance Regularization.
}

\usage{
    stabilization.diagnostic(obj, 
                             span = 0.5, 
                             degree = 2, 
                             family = "gaussian", 
                             title = "Stabilization Diagnostic Plots", 
                             device = NULL, 
                             file = "Stabilization Diagnostic Plots",
                             path = getwd(),
                             horizontal = FALSE, 
                             width = 7, 
                             height = 5, ...)
}

\arguments{
  \item{obj}{Object of class "\code{mvr}" returned by \code{\link[MVR]{mvr}}.}
  \item{title}{Title of the plot. Defaults to "Stabilization Diagnostic Plots".}
  \item{span}{Span parameter of the \code{loess()} function (R package \pkg{stats}), which controls the degree of smoothing. 
              Defaults to 0.75.}
  \item{degree}{Degree parameter of the \code{loess()} function (R package \pkg{stats}), which controls the degree of the polynomials to be used. 
                Defaults to 2. (Normally 1 or 2. Degree 0 is also allowed, but see the "Note" in loess {stats} package.)}
  \item{family}{Family distribution in {"gaussian", "symmetric"} of the \code{loess()} function (R package \pkg{stats}), used for local fitting .
                If "gaussian" fitting is by least-squares, and if "symmetric" a re-descending M estimator is used with Tukey's biweight function.}
  \item{device}{Graphic display device in \{NULL, "PS", "PDF"\}. Defaults to NULL (standard output screen).
                Currently implemented graphic display devices are "PS" (Postscript) or "PDF" (Portable Document Format).}
  \item{file}{File name for output graphic. Defaults to "Stabilization Diagnostic Plots".}
  \item{path}{Absolute path (without final (back)slash separator). Defaults to working directory path.}
  \item{horizontal}{\code{Logical} scalar. Orientation of the printed image. Defaults to \code{FALSE}, that is potrait orientation.}
  \item{width}{\code{Numeric} scalar. Width of the graphics region in inches. Defaults to 7.}
  \item{height}{\code{Numeric} scalar. Height of the graphics region in inches. Defaults to 5.}
  \item{\dots}{Generic arguments passed to other plotting functions.}
}

\details{
    In the plots of standard deviations vs. means, standard deviations and means are calculated in a feature-wise manner from the expression matrix.
    The scatterplot allows to visually verify whether there is a dependence of the standard deviation (or variance) on the mean. 
    The black dotted line depicts the LOESS scatterplot smoother estimator. If there is no variance-mean dependence, 
    then this line should be approximately horizontal.
    
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
    # Summary Stabilization Diagnostic Plots (Real dataset)
    # Multi-Group Assumption
    # Assuming unequal variance between groups
    #===================================================
    stabilization.diagnostic(obj = mvr.obj, 
                             title = "Stabilization Diagnostic Plots 
                             (Real - Multi-Group Assumption)",
                             span = 0.75, 
                             degree = 2, 
                             family = "gaussian",
                             device = NULL,
                             horizontal = FALSE, 
                             width = 7, 
                             height = 5)
    }
}

\keyword{Mean-Variance Estimators}
\keyword{Regularization}
\keyword{Variance Stabilization}
\keyword{Normalization}


