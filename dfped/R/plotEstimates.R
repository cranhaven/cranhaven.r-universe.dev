#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
plotEstimates <-
function(doseMin, doseMax, slope, intercept,
                          estimMetaAnalysis, estimatesNP, estimatesP, doseChildren, Clch){
    
    dbis <- seq(doseMin, doseMax, by = 1)
    proba_metaAnalysisSarah <-  1/(1 + exp(-intercept - slope*(dbis/Clch)))
    
    par(mar = c(8, 4, 2, 1) + 0.1)
    plot(dbis, proba_metaAnalysisSarah, type = "l", lwd = 1, cex = 0.2, col = "black", xlab = "", ylab = "",
         xlim = c(15, 115), ylim = c(0,1), main = NULL, cex.axis = 0.9, cex.main = 1, tck = -0.035, xaxt = "n",
         yaxt = "n", bty = 'n')
    lines(doseChildren[1:4], estimMetaAnalysis, type = "b", col = "blue")
    lines(doseChildren[1:4], estimatesNP, type = "b", col = "red")
    lines(doseChildren[1:4], estimatesP, type = "b", col = "forestgreen")
    
    axis(1, at = seq(0, 110, by = 10), labels = seq(0, 110, by = 10), tick = TRUE, line = NA,
         pos = 0, outer = FALSE, font = NA, lty = "solid", hadj = NA, padj = NA, tck = - 0.025,
         cex.axis = 0.9,  mgp = c(3,0.6,1))
    axis(2, at = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1), tick = TRUE, line = NA,
         pos = 11, outer = FALSE, font = NA, lty = "solid", hadj = NA, padj = NA, tck = - 0.025,
         cex.axis = 0.9,  mgp = c(3,0.6,1))
    arrows(x0 = 0, y0 =0, x1 = 118, y1 = 0, angle = 30)
    axis(1, at = c(25, 35, 45, 55, 70), labels = c(25, 35, 45, 55, 70), tick = TRUE, line = NA,
         pos = -0.15, outer = FALSE, font = NA, lty = "solid", col = NULL, col.ticks = NULL,
         hadj = NA, padj = NA, tck = - 0.015, mgp = c(3,0.4,1), cex.axis = 0.9)
    axis(1, at = c(35, 50, 65, 80, 100), labels = c(35, 50, 65, 80, 100), tick = TRUE, line = NA,
         pos = -0.25, outer = FALSE, font = NA, lty = "solid", col = NULL, col.ticks = NULL,
         hadj = NA, padj = NA, cex.axis = 0.9, tck = - 0.015, mgp = c(3,0.4,1))
    axis(1, at = c(30, 45, 55, 70, 85), labels = c(30, 45, 55, 70, 85), tick = TRUE, line = NA,
         pos = -0.35, outer = FALSE, font = NA, lty = "solid", col = NULL, col.ticks = NULL,
         hadj = NA, padj = NA, tck = - 0.015, mgp = c(3,0.4,1), cex.axis = 0.9)
    
    mtext('LA', side = 2, at = c(-10, -0.17), las = 1, cex = 0.9)
    mtext('AA', side = 2, at = c(-10, -0.27), las = 1, cex = 0.9)
    mtext('MA', side = 2, at = c(-10, -0.37), las = 1, cex = 0.9)
    mtext('Dose (mg/kg)', side=1, line=1, adj=1, cex=0.8)
    mtext('Probability', side=2, line=2, adj=1, cex=0.8)
    
    legend(15, 0.98, legend = c(expression(eta(d)), expression(gamma[l]^(T)), expression(gamma[l]^(2)), expression(gamma[l]^(1))),
           col = c("black", "blue", "red", "forestgreen"), pch = c(NA,1,1,1), lty = c(1,1,1,1), bty = "n", text.col = "black",
           inset = c(0.1, 0.1), horiz = FALSE, box.lwd = 1, box.col = "white", bg = "white", text.font= 1, cex = 1)
}
