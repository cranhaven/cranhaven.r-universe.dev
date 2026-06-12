#' Interaction Plot
#'
#' @param x numeric matrix, \eqn{a \times b} data matrix where the number of row and column is corresponding to the number of factor levels.
#' @param ... plot parameters
#' @return  Plots an interaction plot for input.
#' @author Shenavari, Z.; Haghbin, H.; Kharrati-Kopaei, M.; Najibi, S.M.
#' @examples \dontrun{this is an example}
#' data(CNV)
#' interaction_plot(CNV)
#' @importFrom graphics axis legend matplot par matpoints
#' @importFrom grDevices topo.colors
#' @export
interaction_plot <- function(x, ...) {
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    t <- ncol(x)
    b <- nrow(x)
    oldpar <- par(mfcol = c(1, 1))
    on.exit(par(oldpar))
    par(mfcol = c(1, 2), mai = c(0.45, 0.38, 0.10, 0.54), tck = 0.01, mgp = c(1, 0, 0), xpd = TRUE)
    matplot(t(x), type = "b", xaxt = "n", ylab = "Observed values", xlab = "Factor2(column)", col = topo.colors(b), lwd = 2, lty = 1:b, ...)
    matpoints(t(x), type = "p", pch = as.character(1:b), col = "black")
    axis(1, at = 1:t, labels = 1:t, cex.axis = 1)
    legend(t + 0.02, max(x), rep(paste0("row", 1:b)), lty = 1:b, bty = "n", cex = 0.60, col = topo.colors(b), lwd = 2)
    par(mai = c(0.45, 0.25, 0.10, 0.54), tck = 0.01, mgp = c(1, 0, 0), xpd = TRUE)
    matplot(x, type = "b", xaxt = "n", ylab = "", xlab = "Factor1(row)", col = topo.colors(t), lwd = 2, lty = 1:t, ...)
    matpoints(x, type = "p", pch = as.character(1:t), col = "black")
    legend(b + 0.05, max(x), rep(paste0("col", 1:t)), lty = 1:t, bty = "n", cex = 0.60, col = topo.colors(t), lwd = 2)
    axis(1, at = 1:b, labels = 1:b, cex.axis = 1)
  }
}


