plot.PolyTrend <-
function(x, fig.dates = c(1:length(x$Y)),  xlab = "Year", ylab = "NDVI", fig.text="", ...) {
  
  plot(c(1:length(x$Y)), x$Y, type="l", col="gray32", lwd=2, xlab = "", ylab = "", xaxt = "n")
  axis(1, at=1:length(x$Y), labels=fig.dates) 
  
  mtext(side=3, line=0.5, fig.text, cex=1.1, adj = 0)
  mtext(side=1, xlab, line=2.5, cex=0.8)
  mtext(side=2, ylab, line=2.5, cex=0.8)
  
  strTrendType <-c("Concealed Trend", "No Trend", "Linear Trend", "Quadratic Trend", "Cubic Trend")

  strSignificance <- "sig."
  if(x$Significance < 0) strSignificance <- "in-sig."
  strSlope <- paste("Slope=",round(x$Slope, 4), sep = "")
  
  strTrendType.Text <- strTrendType[x$TrendType+2]
  if(x$TrendType==-1 && x$PolynomialDegree > 1) {
          strForm = c("(quadratic form)","(cubic form)")
          strTrendType.Text <- paste(strTrendType.Text, strForm[x$PolynomialDegree-1], sep = " ")
  }
  
  strLabel <- paste(strTrendType.Text,strSlope,strSignificance,sep = ", ")
  legend("topleft", strLabel, bty = "n", cex=0.9, text.font = 1, y.intersp=1)
  
  
  if(x$PolynomialDegree == 1) {
    
    fit1 <- lm(x$Y ~ poly( c(1:length(x$Y)), raw=TRUE))
    abline(fit1, col = "gray8", lwd=4)
    points(c(1:length(x$Y)), x$Y, type="p", pch = 20, col="gray32") 
  }
  
  if(x$PolynomialDegree == 2) {
    
    fit2 <- lm(x$Y ~ poly( c(1:length(x$Y)), 2, raw=TRUE))
    pol2 <- function(x) coef(fit2)[3]*x^2 + coef(fit2)[2]*x + coef(fit2)[1]
    
    curve(pol2,  col = "gray8", lwd=4, add=T)
    points(c(1:length(x$Y)), x$Y, type="p", pch = 20, col="gray32")
    
    if(x$Significance == -1) {
      p1 <- lm(x$Y ~ c(1:length(x$Y)))
      abline(coef = coef(p1), col = "gray42", lty = 5, lwd = 3)
    }
    
  }
  
  if(x$PolynomialDegree == 3) {
    
    fit3 <- lm(x$Y ~ poly( c(1:length(x$Y)), 3, raw=TRUE))
    pol3 <- function(x) coef(fit3)[4]*x^3 + coef(fit3)[3]*x^2 + coef(fit3)[2]*x + coef(fit3)[1]
    
    curve(pol3,  col = "gray8", lwd=4, add=T)
    points(c(1:length(x$Y)), x$Y, type="p", pch = 20, col="gray32")
    
    if(x$Significance == -1) {
      p1 <- lm(x$Y ~ c(1:length(x$Y)))
      abline(coef = coef(p1), col = "gray42", lty = 5, lwd = 3)
    }
    
  }
  
}
