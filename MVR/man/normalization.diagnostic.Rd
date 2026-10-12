\name{normalization.diagnostic}

\alias{normalization.diagnostic}

\title{Function for Plotting Summary Normalization Diagnostic Plots}

\description{    
    Plot comparative Box-Whisker and Heatmap plots of variables across samples check the effectiveness 
    of normalization before and after Mean-Variance Regularization.
}

\usage{
    normalization.diagnostic(obj, 
                             pal,
                             title = "Normalization Diagnostic Plots",
                             device = NULL, 
                             file = "Normalization Diagnostic Plots",
                             path = getwd(),
                             horizontal = FALSE, 
                             width = 7, 
                             height = 8, ...)
}

\arguments{
  \item{obj}{Object of class "\code{mvr}" returned by \code{\link[MVR]{mvr}}.}
  \item{title}{Title of the plot. Defaults to "Normalization Diagnostic Plots".}
  \item{pal}{Color palette.}
  \item{device}{Graphic display device in \{NULL, "PS", "PDF"\}. Defaults to NULL (standard output screen).
                Currently implemented graphic display devices are "PS" (Postscript) or "PDF" (Portable Document Format).}
  \item{file}{File name for output graphic. Defaults to "Normalization Diagnostic Plots".}
  \item{path}{Absolute path (without final (back)slash separator). Defaults to working directory path.}
  \item{horizontal}{\code{Logical} scalar. Orientation of the printed image. Defaults to \code{FALSE}, that is potrait orientation.}
  \item{width}{\code{Numeric} scalar. Width of the graphics region in inches. Defaults to 7.}
  \item{height}{\code{Numeric} scalar. Height of the graphics region in inches. Defaults to 8.}
  \item{\dots}{Generic arguments passed to other plotting functions.}
}

\details{
    Option \code{file} is used only if device is specified (i.e. non \code{NULL}).
    The argument \code{pal} can be any color palette, e.g. as provided by R package \pkg{RColorBrewer}.
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
    # Summary Normalization Diagnostic Plots (Real dataset)
    # Multi-Group Assumption
    # Assuming unequal variance between groups
    #===================================================
    normalization.diagnostic(obj = mvr.obj, 
                             title = "Normalization Diagnostic Plots 
                             (Real - Multi-Group Assumption)",
                             pal = brewer.pal(n=11, name="RdYlGn"),
                             device = NULL,
                             horizontal = FALSE, 
                             width = 7, 
                             height = 8)

    }
}

\keyword{Mean-Variance Estimators}
\keyword{Regularization}
\keyword{Variance Stabilization}
\keyword{Normalization}


