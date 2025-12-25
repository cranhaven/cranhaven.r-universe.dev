#' Automatically cluster 1-dimensional continuous data.
#'
#' @param x Vector to be clustered. Must contain at least 1 non-missing value.
#' @param bw kernel bandwidth. Default "SJ" should suffice more application, however you can supply a custom numeric value. See ?stats::density for more information.
#' @param fixed logical; if \code{TRUE}, performs simple 1-dimensional clustering without kernel density estimation. default FALSE.
#'
#' @return
#' An integer vector identifying the cluster corresponding to each element in \code{x}.
#' @export
#'
#' @examples
#' # Below vector clearly has 2 groups.
#' # kluster will identify these groups using kernel density estimation.
#' kluster(c(0.1, 0.2, 1))
#'
#' # kluster shines in cases where manually assigning groups via "eyeballing" is impractical.
#' # Suppose we obtained vector 'x' without knowing how it was generated.
#' set.seed(1)
#' nodes <- runif(10, min = 0, max = 100)
#' x <- lapply(nodes, function(x) rnorm(10, mean = x, sd = 0.1))
#' x <- unlist(x)
#'
#' kluster(x) # kluster reveals the natural grouping
#'
#' kluster(x, bw = 10) # adjust bandwidth depending on application
#'
#' # Example with faithful dataset
#' faithful$k <- kluster(faithful$eruptions)
#' library(ggplot2)
#' ggplot(faithful, aes(eruptions)) +
#'   geom_density() +
#'   geom_rug(aes(color = factor(k))) +
#'   theme_minimal() +
#'   scale_color_discrete(name = "k")
kluster <- function(x, bw = "SJ", fixed = FALSE) {

  # Coerce x to numeric
  tryCatch(
    warning = function(cnd) {
      stop("input x must be coercible to <numeric>")
    },
    x <- as.numeric(x)
  )

  if (length(x) < 1) {
    stop("input x must contain at least 1 non-missing value")
  }

  if (fixed) {
    tryCatch(
      warning = function(cnd) {
        stop("input bw must be coercible to <numeric> if fixed = TRUE")
      },
      bw <- as.numeric(bw[[1]])
    )
  }

  # Handle missing data while preserving indices
  xdf <- data.frame(i = seq_along(x),
                    x = x,
                    out = NA_integer_)

  xnm <- xdf[!is.na(xdf$x), ]
  xnm <- xnm[order(xnm$x), ]
  nnm <- nrow(xnm)

  if (nnm < 1) {
    stop("input x must contain at least 1 non-missing value")
  }

  xnm$x <- xnm$x - min(xnm$x)


  if (nnm == 1) {
    xdf$out[xnm$i] <- 1L
    return(xdf$out)
  }

  # kernel density estimation
  if (!fixed) {
    d <- stats::density(xnm$x, bw = bw)
    d$y <- d$y / max(d$y)
    d$y[d$y < 0.00001] <- 0

    tp <- pastecs::turnpoints(d$y)
    boundaries <- d$x[tp$pos[tp$pits]]

    xnm$out <- findInterval(xnm$x, boundaries) + 1
  } else {
    xnm$out <- xnm$x - dplyr::lag(xnm$x, default = xnm$x[1])
    xnm$out <- cumsum(xnm$out > bw) + 1
  }

  result <- rbind(xdf[is.na(xdf$x), ], xnm)

  return(result[order(result$i), ]$out)
}
