
# Helper functions for shiny app "visBeta"
# =======================================

# The plotting function
# ---------------------

visQuad <- function (beta0, beta1, beta2, npoints=100)
{
  xx <- seq(-1, 1, length=npoints)
  y2 <- beta0 + beta1*xx + beta2*xx^2
  y1 <- beta0 + beta1*xx
  plot(xx, y2, type='l', las=1, ylim=c(-2, 2),
      xlab = "x", ylab = "y")
  lines(xx,  y1, col='red')
  abline(h = beta0, v=0, lty=3)
  if(beta2 != 0) {
    xmin <- -beta1 / (2*beta2)
    ymin <- beta0 + beta1*xmin + beta2*xmin^2
    points(xmin, ymin, cex=2)
  }
  msg <- sprintf("%1.1f %+1.1fx %+1.1f", beta0, beta1, beta2)
  title(main = bquote(y == .(msg)*x^2))
}

# 