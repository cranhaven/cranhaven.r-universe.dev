#' Beta response models
#'
#' @rdname betaresp
#' @aliases betaresp print.betaresp plot.betaresp betaresponse
#'
#' @description
#'   Generating beta response models. Implemented to compare the hierarchical logistic regression models
#'   with beta response models.
#'
#' @param x Gradient values
#' @param p1,p2 response endpoints
#' @param alpha,gamma shape parameters of the response
#' @param hi maximum height of response
#'
#' @author Jari Oksanen
#'
#' @examples
#'  x <- seq(0,3, len=201)
#'  m <- betaresp(x, 2, 0.8, 0.1, 0.5, 0.8)
#'  plot(m)
#'
#' @export
`betaresp` <- function(
		x, ### x: gradient
		p1, ### p1, p2: response endpoints
		p2,
		alpha, ### alpha, gamma: shape parameters of the response
		gamma,
		hi  ### hi: maximum height of the response
    ) {
    ## take care that p1 < p2
    if (p1 > p2) {
        tmp <- p1
        p1 <- p2
        p2 <- tmp
    }
    ## solve multiplier 'k' from desired max height 'hi', requires
    ## max(hi) <= 1
    hi <- min(hi, 1)
    ran <- p2 - p1
    t2 <- ran/(alpha + gamma)
    k <- hi/((alpha * t2)^alpha)/((gamma * t2)^gamma)
    ## Fitted values
    mu <- ifelse(x <= p1 | x >= p2, 0, k * (x - p1)^alpha * (p2 - x)^gamma)
    ## noisy response
    y <- rbinom(length(mu), 1, mu)
    ## integral
    t3 <- 1/(alpha+gamma)
    const <- hi/(alpha*t3)^alpha/(gamma*t3)^gamma
    area <-  const * ran * beta(alpha + 1, gamma + 1)
    out <- list(x = x, mu = mu, y = y, integral = area,
                par = list(k = k, p1 = p1, p2 = p2,
                alpha = alpha, gamma = gamma))
    class(out) <- "betaresp"
    out
}

### plot response curve and simulated points
#' @export
`plot.betaresp` <-
    function(x, xlab = "Gradient", ylab = "Response", cex = 0.5,
             ylim = range(x$y), rug = TRUE, ...)  {
      k <- order(x$x)
      plot(x$x[k], x$mu[k], type = "l", ylim = ylim, xlab = xlab, ylab = ylab,
           ...)
      if(rug)  points(x$x, x$y, cex = cex, ...)
      invisible()
  }

### add response curve (mu) to an existing plot
#' @export
`lines.betaresp`<-
    function(x, ...) {
      k <- order(x$x)
      lines(x$x[k], x$mu[k], ...)
  }
