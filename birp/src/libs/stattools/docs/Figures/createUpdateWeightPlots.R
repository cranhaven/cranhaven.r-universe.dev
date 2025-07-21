
#-------------------------------
# Equations
#-------------------------------

geometricUniform <- function(r, t, x){
  p <- 1. - exp(log(1-x) / (t * length(r)))
  
  uniform <- 1/length(r)
  
  w <- 0.3*uniform + 0.7 * p * (1-p)^r

  return(w)
}

getLog10StatePos <- function(x){
  x[x < 0.01] <- 0.01
  x[x > 0.99] <- 0.99
  w <- -log10(1 - x)
  w <- w / sum(w)
  return(w)
}


getPowerState <- function(x, intercept, exponent){
  w <- intercept + (1.0 - intercept) * x^exponent; 
  w <- w / sum(w)
  return(w)
}

#-------------------------------
# Plotting functions
#-------------------------------

plotGeoUni <- function(colors){
  x <- 1:1000
  plot(x, geometricUniform(x, 0.2, 0.9),
       col = colors[1],
       xlab = "r",
       ylab = "Q",
       lwd = 2,
       type = "l")
  lines(x, geometricUniform(x, 0.3, 0.9),
        col = colors[2],
        lwd = 2)
  lines(x, geometricUniform(x, 0.3, 0.5),
        col = colors[3],
        lwd = 2)
  legend("topright", c("t=0.2, r=0.9", "t=0.3, r=0.9", "t=0.2, r=0.3"), 
         col = colors, 
         lty = rep(1, 3),
         bty = "n")
  text(x = par("usr")[1] - 230, y = par("usr")[4], labels = "(A)", font = 2, xpd = T, cex = 1.5)
}

plotLog10 <- function(colors){
  x <- seq(0, 1, length.out = 1000)
  plot(x, getLog10StatePos(x), 
       col = colors[1],
       lwd = 2,
       xlab = "p",
       ylab = "Q",
       type = "l")
  text(x = par("usr")[1] - 0.25, y = par("usr")[4], labels = "(B)", font = 2, xpd = T, cex = 1.5)
}

plotPowerState <- function(colors){
  x <- seq(0, 1, length.out = 1000)
  plot(x, getPowerState(x, 0.1, 0.5),
       col = colors[1],
       xlab = "p",
       ylab = "Q",
       lwd = 2,
       ylim = c(0.0001, 0.0025),
       type = "l")
  lines(x, getPowerState(x, 0.1, 1),
        col = colors[2],
        lwd = 2)
  lines(x, getPowerState(x, 0.1, 2),
        col = colors[3],
        lwd = 2)
 
  legend("topleft", c(expression("m=0.1, "*beta~"= 0.5"), 
                      expression("m=0.1, "*beta~"= 1"),
                      expression("m=0.1, "*beta~"= 2")), 
         col = colors, 
         lty = rep(1, 3),
         bty = "n")
  text(x = par("usr")[1] - 0.25, y = par("usr")[4], labels = "(C)", font = 2, xpd = T, cex = 1.5)
}



colors <- c("orange2", "dodgerblue", "darkgreen")

pdf("/data/git/stattools/docs/Figures/updateWeights.pdf", width = 8, height = 8/3)
par(mfrow = c(1, 3), mar = c(4, 4, 1, 0), pty = "s")
plotGeoUni(colors)
plotLog10(colors)
plotPowerState(colors)
dev.off()



