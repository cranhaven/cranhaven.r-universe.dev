#' @title Discovery Curve
#' @description Calculate the components of a species discovery curve.
#' 
#' @param f a vector of species frequencies where \code{f[i]} is the number of 
#'   species represented by only \code{i} samples.
#' @param max.x the maximum number of samples to calculate the curve for. 
#'   Defaults to the sample size of \code{f}.
#' @param n.pts number of points between 0 and \code{max.x} to estimate.
#' @param ci size of the confidence interval (0.5:1).
#' @param f0.func function to use to calculate \code{\link{f0}}.
#' @param plot plot the curve?
#' @param ... other arguments to \code{f0.func}.
#' 
#' @return a list with:
#' \item{f.stats}{a named vector from \code{f0.func}.}
#' \item{curve}{a \code{data.frame} defining the rarefaction and 
#' extrapolation curves (specified in the \code{section} column), and columns 
#' providing the lower (\code{lci}) and upper (\code{uci}).}
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov} 
#' 
#' @references Colwell, R.K., A. Chao, N.J. Gotelli, S.-Y. Lin, C.X. Mao, 
#'   R.L. Chazdon, and J.T. Longino. 2012. Models and estimators linking 
#'   individual-based and sample-based rarefaction, extrapolation and 
#'   comparison of assemblages. Journal of Plant Ecology 5(1):3-21.
#' 
#' @examples
#' data(osa.old.growth)
#' f <- expand.freqs(osa.old.growth)
#' d <- discovery.curve(f, f0.func = Chao1, max.x = 1200)
#' 
#' print(str(d))
#' 
#' @export
#' 
discovery.curve <- function(f, f0.func, max.x = sum(f * 1:length(f)), 
                            n.pts = 100, ci = 0.95, plot = TRUE, ...) {
  n.total <- sum(f * 1:length(f))
  n.seq <- ceiling(seq(0, ceiling(max.x), length.out = n.pts))
  n.seq <- sort(unique(c(n.total, n.seq)))
  s.ind <- t(sapply(n.seq, expected.num.species, f = f, f0.func = f0.func, ...))
  f.stats <- s.ind[1, 4:7]
  n <- s.ind[, 'n']
  s.ind <- s.ind[, -(4:7)]
  s.ind <- s.ind[, c(3, 1, 2)]
  
  s.ind.ci <- s.ind[, "s.ind"] + stats::qnorm((1 - ci) / 2) * 
    cbind(s.ind[, "sd.s.ind"], -s.ind[, "sd.s.ind"])
  
  i <- max(which(s.ind[, "m"] <= n))
  df <- cbind(
    data.frame(x = n.seq),
    y = s.ind[, "s.ind"],
    lci = s.ind.ci[, 1],
    uci = s.ind.ci[, 2],
    type = c(
      rep("rarefaction", i), 
      rep("extrapolation", length(n.seq) - i)
    )
  )
  
  if(plot) {
    g <- ggplot2::ggplot(df, ggplot2::aes_string(x = "x")) +
      ggplot2::geom_ribbon(
        ggplot2::aes_string(ymin = "lci", ymax = "uci"), 
        fill = "salmon", 
        alpha = 0.7
      ) +
      ggplot2::geom_line(ggplot2::aes_string(y = "y", linetype = "type")) +
      ggplot2::annotate(
        geom = 'point',
        x = n.total, 
        y = sum(f)
      ) +
      ggplot2::scale_linetype_manual(
        values = c(rarefaction = "solid", extrapolation = "dashed")
      ) +
      ggplot2::theme(legend.position = "none")
    
    print(g)
  }
  
  list(f.stats = f.stats, curve = df)
}
