#' Species–area (collector's curve) function
#'
#' Computes species accumulation (collector's) curves based on sample units (SUs).
#' The function performs random resampling of the input matrix or data frame to estimate
#' the expected species richness per number of SUs, with confidence intervals derived
#' from multiple permutations.
#'
#' @param formula An optional formula specifying the relationship between taxa and sample units
#'   (e.g., \code{Taxon ~ Sample}). If provided, the function extracts variables from \code{data}.
#'   A third variable may be included to remove dead individuals (e.g., \code{Taxon ~ Sample - Dead}).
#' @param data A data frame containing the variables specified in \code{formula} ('long format').
#'   It must contain one column representing the sample unit labels (e.g., quadrats or points)
#'   and one column representing the taxon names of the individual plants. This argument accepts
#'   the data frame used in the argument \code{x} in \code{\link{phytoparam}}.
#' @param x Species-by-sample matrix, with rows representing SUs and columns representing taxa
#'   ('wide format'). Can be either an abundance or presence–absence matrix. Ignored if
#'   \code{formula} and \code{data} are used.
#' @param times Integer. Number of random permutations used in calculations. Default is 1000.
#'   Larger values (> 1000) yield more stable estimates.
#' @param replace Logical. Indicates whether resampling is performed with replacement (\code{TRUE}, bootstrap)
#'   or without replacement (\code{FALSE}, default).
#' @param prob Numeric. Probability level used for computing confidence intervals around
#'   species accumulation (default = 0.95).
#' @param spar Numeric. Controls the smoothing parameter for plotted confidence intervals
#'   via spline interpolation. Default = 0 (no smoothing).
#' @param xlab Character. Label for the x-axis in the plot. Default = \code{"Number of samples"}.
#' @param ylab Character. Label for the y-axis in the plot. Default = \code{"Number of species"}.
#' @param plot Logical. If \code{TRUE} (default), the species accumulation curve is plotted.
#' @param long Logical. If \code{TRUE}, returns detailed results, including the full set of
#'   resampling matrices. Default = \code{FALSE}.
#' @param theme Character string specifying the ggplot2 theme to apply
#'   (e.g., \code{"theme_classic"}, \code{"theme_bw"}, \code{"theme_minimal"}).
#'   Defaults to \code{"theme_classic"}.
#'
#' @details
#' Species accumulation curves are computed by sequentially adding sample units and recording
#' species richness across permutations. Confidence intervals are estimated from the empirical
#' distribution of resampled richness values. The plotted confidence intervals are smoothed
#' using spline interpolation if \code{spar} > 0.
#'
#' It is recommended to assign the output to an object, as the complete output
#' (particularly with \code{long = TRUE}) can be large.
#'
#' @return If \code{long = FALSE} (default), returns a data frame with columns:
#' \itemize{
#'   \item \code{n}: number of sample units.
#'   \item \code{s}: mean number of species.
#'   \item \code{lower}: lower confidence interval bound.
#'   \item \code{upper}: upper confidence interval bound.
#' }
#' If \code{long = TRUE}, returns a list with:
#' \itemize{
#'   \item \code{matrix.s}: full matrix of species richness per permutation.
#'   \item \code{s}: the summarized data frame described above.
#' }
#'
#' @note With \code{long = TRUE}, the function provides access to the complete set of resampling
#' results, useful for additional data analyses.
#'
#' @author Rodrigo Augusto Santinelo Pereira \email{raspereira@usp.br}
#' @author Adriano Sanches Melo
#'
#' @references
#' Magurran, A. E. (1988). \emph{Ecological Diversity and Its Measurement}. Croom Helm.
#'
#' Magurran, A. E. (2004). \emph{Measuring Biological Diversity}. Blackwell Publishing.
#'
#' @examples
#' ## Using 'formula' (long format)
#' ## Without smoothing confidence intervals
#' collector.curve(
#'   formula = Species ~ Plot - Morta,
#'   data = quadrat.df,
#'   times = 1000, long = FALSE, plot = TRUE
#' )
#'
#' ## Smoothing confidence intervals
#' collector.curve(
#'   formula = Species ~ Plot - Morta,
#'   data = quadrat.df,
#'   spar = 0.6, times = 1000, long = FALSE, plot = TRUE
#' )
#' \donttest{
#' ## Using different plot themes
#' collector.curve(
#'   formula = Species ~ Plot - Morta,
#'   data = quadrat.df,
#'   times = 1000, long = FALSE, plot = TRUE, theme = "theme_light"
#' )
#' collector.curve(
#'   formula = Species ~ Plot - Morta,
#'   data = quadrat.df,
#'   times = 1000, long = FALSE, plot = TRUE, theme = "theme_bw"
#' )
#' collector.curve(
#'   formula = Species ~ Plot - Morta,
#'   data = quadrat.df,
#'   times = 1000, long = FALSE, plot = TRUE, theme = "theme_minimal"
#' )
#'
#' ## Using a matrix (wide format)
#' data.matrix <- with(
#'   quadrat.df,
#'   table(Plot, Species, exclude = "Morta")
#' )
#' collector.curve(x = data.matrix, times = 1000)
#'
#' ## Alternatively...
#' data.matrix <- as.matrix(
#'   xtabs(~ Plot + Species, data = quadrat.df, exclude = "Morta")
#' )
#' collector.curve(x = data.matrix, times = 1000)
#' }
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon xlab ylab aes theme_classic
#' @importFrom stats as.formula quantile smooth.spline predict
#' @importFrom utils getFromNamespace
#' @export



collector.curve <- function(formula, data, x, times = 1000, replace = FALSE, prob = 0.95, spar=0, xlab, ylab, plot=TRUE, long=FALSE, theme = "theme_classic")
{
  if(!missing(formula)) {
    fm <- as.formula(formula) # Transforms the "formula" argument into a formula object
    vars <- all.vars(fm) # Creates a vector with the variable names
    taxon<-vars[1]
    su<-vars[2]

    y<-is.na(data) | data == "" # Logical test to find all empty cells or NAs
    data <- data[!apply(y == TRUE, 1, all), !apply(y == TRUE, 2, all)]  # Removes all empty/NA rows and columns

    if (length(vars)==3) # if the option is to remove dead plants from the data frame
    {
      dead<-vars[3]
      filter = data[[taxon]] == dead # tests which individuals are "dead"
      data <- data[!filter, ] # removes rows containing dead plants from the data frame
      message(sum(filter), " dead individuals removed from the dataset.") # displays a message with the number of "dead" individuals removed from the data frame
    }
    x<-table(data[[su]], data[[taxon]])
  }
  x <- x > 0
  lines.x <- dim(x)[1]
  # creates presence-absence matrix
  size <- seq(1:lines.x)
  if(missing(xlab)) xlab <- "Number of samples"
  if(missing(ylab)) ylab <- "Number of species"
  #
  # creates a matrix of ones and zeros
  line.zero.one <- lower.tri(matrix(, nrow=lines.x, ncol=lines.x), diag=TRUE)
  #
  # beginning of sampling. 1st is done with the original order
  # creates matrix of accumulated richness per plot
  matrix.sum <- line.zero.one %*% x
  # Calculates the number of spp per plot
  non.zeros <- rowSums(matrix.sum > 0)
  # Calculates the number of spp with one occurrence (uniques)
  matrix.s <- non.zeros # Stores the vector with the number of spp per plot
  #
  # loop for the remaining re-samplings
  for(j in 2:times) {
    sampled.lines <- sample(lines.x, lines.x) # creates a vector of indices
    x.perm <- x[sampled.lines,  ] # permutes the matrix according to the index vector
    #
    # repeats previous calculations
    matrix.sum <- line.zero.one %*% x.perm
    non.zeros <- rowSums(matrix.sum > 0)
    matrix.s <- rbind(matrix.s, non.zeros) # Matrix with the number of spp per plot, each row is a re-sample
  }
  #
  matrix.s<-as.data.frame(matrix.s)
  m.s <- as.vector(colMeans(matrix.s)) # Calculates the mean number of spp per plot
  #
  inf<-(1-prob)/2
  sup<-prob+inf
  ci<-t(apply(matrix.s, MARGIN=2, FUN=quantile, probs=c(0.025, 0.975))) # Calculates the confidence interval of s
  colnames(ci) <- c("lower", "upper")
  #
  dimnames(matrix.s)<-list(1:times, size)
  m.s <- as.data.frame(cbind(n=size, s=m.s, ci)) # vector with the mean number of spp per plot
  rownames(m.s)<-NULL
  # Creates list with the results
  # Long results, including matrices with the re-samples
  if(long==TRUE){
    result.names <- c("matrix.s", "s")
    result <- list(matrix.s, m.s)
    names(result) <- result.names
  }
  else {
    result <- m.s
  }
  #
  # Displays results
  if (plot==TRUE){
#    if(all((.packages())!= "ggplot2")) library(ggplot2)

    ## 1) Smooth CI limits
    ss_lower <- stats::smooth.spline(x = m.s$n, y = m.s$lower, spar = spar)
    ss_upper <- stats::smooth.spline(x = m.s$n, y = m.s$upper, spar = spar)

    m.s$lower_smooth <- stats::predict(ss_lower, x = m.s$n)$y
    m.s$upper_smooth <- stats::predict(ss_upper, x = m.s$n)$y

    ## Keep lower <= upper after smoothing
    lo <- pmin(m.s$lower_smooth, m.s$upper_smooth)
    up <- pmax(m.s$lower_smooth, m.s$upper_smooth)
    m.s$lower_smooth <- lo
    m.s$upper_smooth <- up

    ## map theme from string
    resolve_theme <- function(th) {
      if (inherits(th, "theme")) return(th)
      if (is.character(th) && nzchar(th)) {
        if (exists(th, where=asNamespace("ggplot2"), inherits=FALSE)) {
          fun <- getFromNamespace(th, "ggplot2")
          if (is.function(fun)) {
            obj <- try(fun(), silent=TRUE)
            if (inherits(obj, "theme")) return(obj)
          }
        }
      }
      theme_classic()  # fallback
    }
    theme_obj <- resolve_theme(theme)

    p<-ggplot(data=m.s, aes(x=n, y=s)) +
      geom_line() +
      geom_ribbon(aes(ymin = lower_smooth, ymax = upper_smooth), alpha = 0.1) +
      xlab(xlab) +
      ylab(ylab) +
      theme_obj

    print(p)
  }
  result
}
