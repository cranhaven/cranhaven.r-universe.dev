\name{cluster.diagnostic}

\alias{cluster.diagnostic}

\title{Function for Plotting Summary Cluster Diagnostic Plots}

\description{
    Plot similarity statistic profiles and the optimal joint clustering configuration for the means and the variances by group.
    
    Plot quantile profiles of means and standard deviations by group and for each clustering configuration, to check that the distributions 
    of first and second moments of the MVR-transformed data approach their respective null distributions under the optimal configuration found, 
    assuming independence and normality of all the variables.
}

\usage{
    cluster.diagnostic(obj, 
                       span = 0.75, 
                       degree = 2, 
                       family = "gaussian", 
                       title = "Cluster Diagnostic Plots", 
                       device = NULL, 
                       file = "Cluster Diagnostic Plots",
                       path = getwd(),
                       horizontal = FALSE, 
                       width = 8.5, 
                       height = 11, ...)
}

\arguments{
  \item{obj}{Object of class "\code{mvr}" returned by \code{\link[MVR]{mvr}}.}
  \item{title}{Title of the plot. Defaults to "Cluster Diagnostic Plots".}
  \item{span}{Span parameter of the \code{loess()} function (R package \pkg{stats}), which controls the degree of smoothing. 
              Defaults to 0.75.}
  \item{degree}{Degree parameter of the \code{loess()} function (R package \pkg{stats}), which controls the degree of the polynomials to be used. 
                Defaults to 2. (Normally 1 or 2. Degree 0 is also allowed, but see the "Note" in loess {stats} package.)}
  \item{family}{Family distribution in {"gaussian", "symmetric"} of the \code{loess()} function (R package \pkg{stats}), used for local fitting .
                If "gaussian" fitting is by least-squares, and if "symmetric" a re-descending M estimator is used with Tukey's biweight function.}
  \item{device}{Graphic display device in \{NULL, "PS", "PDF"\}. Defaults to NULL (standard output screen).
                Currently implemented graphic display devices are "PS" (Postscript) or "PDF" (Portable Document Format).}
  \item{file}{File name for output graphic. Defaults to "Cluster Diagnostic Plots".}
  \item{path}{Absolute path (without final (back)slash separator). Defaults to working directory path.}
  \item{horizontal}{\code{Logical} scalar. Orientation of the printed image. Defaults to \code{FALSE}, that is potrait orientation.}
  \item{width}{\code{Numeric} scalar. Width of the graphics region in inches. Defaults to 8.5.}
  \item{height}{\code{Numeric} scalar. Height of the graphics region in inches. Defaults to 11.}
  \item{\dots}{Generic arguments passed to other plotting functions.}
}

\details{
    In a plot of a similarity statistic profile, one checks the goodness of fit of the transformed data relative to the hypothesized underlying reference 
    distribution with mean-0 and standard deviation-1 (e.g. \eqn{N(0, 1)}). The red dashed line depicts the LOESS scatterplot smoother estimator.
    The subroutine internally generates reference null distributions for computing the similarity statistic under each cluster configuration. 
    The optimal cluster configuration (indicated by the vertical red arrow) is found where the similarity statistic reaches its minimum plus/minus 
    one standard deviation (applying the conventional one-standard deviation rule). A smaller cluster number configuration indicates under-regularization, 
    while over-regularization starts to occur at larger numbers. This over/under-regularization must be viewed as a form of over/under-fitting 
    (see \cite{Dazard, J-E. and J. S. Rao (2012)} for more details).
    The quantile diagnostic plots uses empirical quantiles of the transformed means and standard deviations to check how 
    closely they are approximated by theoretical quantiles derived from a standard normal \emph{equal-mean}/\emph{homoscedastic} 
    model (solid green lines) under a given cluster configuration. To assess this goodness of fit of the transformed data, theoretical null distributions 
    of the mean and variance are derived from a standard normal \emph{equal-mean}/\emph{homoscedastic} model with independence of the first two moments, 
    i.e. assuming i.i.d. normality of the raw data. However, we do not require i.i.d. normality of the data in general: these theoretical null distributions are 
    just used here as convenient ones to draw from. Note that under the assumptions that the raw data is i.i.d. standard normal ($N(0, 1)$) 
    with independence of first two moments, the theoretical null distributions of means and standard deviations for each variable 
    are respectively: \eqn{N(0, \frac{1}{n})} and \eqn{\sqrt{\frac{\chi_{n - G}^{2}}{n - G}}}, where \eqn{G} denotes the number of sample groups. 
    The optimal cluster configuration found is indicated by the most horizontal red curve. The single cluster configuration, corresponding to no transformation, 
    is the most vertical curve, while the largest cluster number configuration reaches horizontality. Notice how empirical quantiles of transformed
    pooled means and standard deviations converge (from red to black) to the theoretical null distributions (solid green lines) for the optimal 
    configuration. One should see a convergence towards the target null, after which overfitting starts to occur (see \cite{Dazard, J-E. and J. S. Rao (2012)} 
    for more details).
    Both cluster diagnostic plots help determine (i) whether the minimum of the \emph{Similarity Statistic} is observed within the range of clusters 
    (i.e. a large enough number of clusters has been accommodated), and (ii) whether the corresponding cluster configuration is a good fit. 
    If necessary, run the procedure again with larger value of the \code{nc.max} parameter in the \code{\link[MVR]{mvr}} as well as 
    in \code{\link[MVR]{mvrt.test}} functions until the minimum of the similarity statistic profile is reached.       
    
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
    ===================================================
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
    # Summary Cluster Diagnostic Plots (Real dataset)
    # Multi-Group Assumption
    # Assuming unequal variance between groups
    #===================================================
    cluster.diagnostic(obj = mvr.obj, 
                       title = "Cluster Diagnostic Plots 
                       (Real - Multi-Group Assumption)",
                       span = 0.75, 
                       degree = 2, 
                       family = "gaussian",
                       device = NULL,
                       horizontal = FALSE, 
                       width = 8.5, 
                       height = 11)

    }
}

\keyword{Mean-Variance Estimators}
\keyword{Regularization}
\keyword{Variance Stabilization}
\keyword{Normalization}


