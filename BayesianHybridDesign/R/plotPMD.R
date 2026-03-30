#' Plot Posterior Mean Difference (PMD) Distributions
#'
#' Plots the density of the posterior mean difference between the hybrid and
#' concurrent control arms, based on an object from `power.DPP()`. Can
#' compare one or two such objects on the same plot.
#'
#' @param o An object produced by the `power.DPP()` function.
#' @param o2 (Optional) A second object from `power.DPP()` to compare on the
#'   same plot. Default is `NULL`.
#'
#' @return Invisibly returns a list containing summary statistics. The
#'   structure of the list depends on whether `o2` is provided.
#'   \itemize{
#'     \item{If `o2` is `NULL`}: A list with a scalar `mean_PMD`, a scalar
#'       `sd_PMD`, and a numeric vector `CI95_PMD`.
#'     \item{If `o2` is provided}: A list with numeric vectors for `mean_PMD`
#'       and `sd_PMD`, and a matrix for `CI95_PMD`, with each row
#'       corresponding to an input object.
#'   }
#'
#' @examples
#' # nsim reduced for faster example
#' o1a <- power.DPP(pt=0.65, nt=60, pc=0.45, nc=30, pc.calib=0.45,
#'                  pch=0.45, nche=60, nch=200, alpha=0.1, nsim=1000)
#' o1b <- power.DPP(pt=0.65, nt=60, pc=0.45, nc=30, pc.calib=0.45,
#'                  pch=0.45, nche=30, nch=200, alpha=0.1, nsim=1000)
#'
#' # Plot a single object
#' stats1 <- plotPMD(o=o1a)
#'
#' # Plot two objects for comparison
#' stats2 <- plotPMD(o=o1a, o2=o1b)
#'
#' @importFrom stats density quantile
#' @importFrom graphics plot lines polygon legend
#' @export
#'
plotPMD <- function(o, o2 = NULL) {
  if (is.null(o2)) {
    # Compute densities
    d <- density(o$mean_hca - o$mean_c, adjust = 25)
    # Plot the first density curve
    plot(d, col = "blue", lwd = 2, ylim = c(0, max(d$y)),
         main = "Posterior Mean ORR Difference",
         xlab = "ORR Difference (Hybrid control - concurrent control)",
         ylab = "Density")

    # Fill under first curve
    polygon(c(d$x, rev(d$x)), c(d$y, rep(0, length(d$y))),
            col = grDevices::rgb(0, 0, 1, 0.3), border = NA)

    # 95%CI
    q <- round(quantile(o$mean_hca - o$mean_c, probs = c(0.025, 0.975)), 3)

    # Add legend
    legend("topleft",
           legend = c(paste("95%CI: ", q[1], ", ", q[2], sep = "")),
           fill = c(grDevices::rgb(0, 0, 1, 0.3)),
           bty = "n", cex = 1)

    ans <- list(mean_PMD = mean(o$mean_hca - o$mean_c),
                sd_PMD = sd(o$mean_hca - o$mean_c), CI95_PMD = q)
    return(ans)
  } else {
    # Compute densities
    d <- density(o$mean_hca - o$mean_c, adjust = 25)
    d2 <- density(o2$mean_hca - o2$mean_c, adjust = 25)

    # Plot the first density curve
    plot(d, col = "blue", lwd = 2, ylim = c(0, max(d$y, d2$y)),
         main = "Posterior Mean ORR Difference",
         xlab = "ORR Difference (Hybrid control - concurrent control)",
         ylab = "Density")

    # Add the second density curve
    lines(d2, col = "red", lwd = 2)

    # Fill under first curve
    polygon(c(d$x, rev(d$x)), c(d$y, rep(0, length(d$y))),
            col = grDevices::rgb(0, 0, 1, 0.3), border = NA)

    # Fill under second curve
    polygon(c(d2$x, rev(d2$x)), c(d2$y, rep(0, length(d2$y))),
            col = grDevices::rgb(1, 0, 0, 0.3), border = NA)

    # Add legend
    legend("topright", legend = c("Design 1", "Design 2"),
           fill = c(grDevices::rgb(0, 0, 1, 0.3), grDevices::rgb(1, 0, 0, 0.3)),
           bty = "n", cex = 1)
    # 95%CI
    q <- round(quantile(o$mean_hca - o$mean_c, probs = c(0.025, 0.975)), 3)
    q2 <- round(quantile(o2$mean_hca - o2$mean_c, probs = c(0.025, 0.975)), 3)

    # Add legend
    legend("topleft",
           legend = c(paste("95%CI: ", q[1], ", ", q[2], sep = ""),
                      paste("95%CI: ", q2[1], ", ", q2[2], sep = "")),
           fill = c(grDevices::rgb(0, 0, 1, 0.3), grDevices::rgb(1, 0, 0, 0.3)),
           bty = "n", cex = 1)

    ans <- list(mean_PMD = c(mean(o$mean_hca - o$mean_c), mean(o2$mean_hca - o2$mean_c)),
                sd_PMD = c(sd(o$mean_hca - o$mean_c), sd(o2$mean_hca - o2$mean_c)),
                CI95_PMD = rbind(q, q2))
    return(ans)
  }
}
