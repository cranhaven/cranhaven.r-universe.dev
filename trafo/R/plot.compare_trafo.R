#' Plots for linear models with transformed dependent variable
#'
#' For the two transformed models a range of plots is returned in order to check
#' model assumptions graphically.  
#' 
#' @param x an object of type \code{trafo_compare}
#' @param ... additional arguments that are not used in this method
#' @importFrom pryr %<a-%
#' @importFrom grDevices dev.flush dev.hold
#' @importFrom graphics abline par plot
#' @export

plot.trafo_compare <- function(x, ...) {
  
  residFit_One <- NULL
  residFit_Two <- NULL
  QQ_One <- NULL
  QQ_Two <- NULL
  hist_One <- NULL
  hist_Two <- NULL
  yFit_One <- NULL
  yFit_Two <- NULL
  scatter_One <- NULL
  scatter_Two <- NULL
  scaleLoc_One <- NULL
  scaleLoc_Two <- NULL
  residLev_One <- NULL
  residLev_Two <- NULL
  cooks_One <- NULL
  cooks_Two <- NULL
  
  ranef <- NULL

  
  if (inherits(x$trafoOne, "lm")) {
    
    n <- length(x$trafoOne$residuals)
    
    residFit_One %<a-%  plot(x$trafoOne, which = c(1L), main = x$trafos[[1]],
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    residFit_Two %<a-%  plot(x$trafoTwo, which = c(1L), main = x$trafos[[2]],
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    QQ_One %<a-%  plot(x$trafoOne, which = c(2L), main = x$trafos[[1]],
                        labels.id = 1:n, cex.oma.main = 1.15, 
                        sub.caption = "")
    QQ_Two %<a-%  plot(x$trafoTwo, which = c(2L), main = x$trafos[[2]],
                         labels.id = 1:n, cex.oma.main = 1.15, 
                         sub.caption = "")
    
    # Histogram
    resid_One <- residuals(x$trafoOne, level = 0, type = "pearson")
    resid_Two <- residuals(x$trafoTwo, level = 0, type = "pearson")
    
    
    hist_One %<a-% hist(resid_One, nclass = 20, xlab = "Pearson residuals", 
                         main = x$trafos[[1]], prob = TRUE)
    hist_Two %<a-% hist(resid_Two, nclass = 20, xlab = "Pearson residuals", 
                          main = x$trafos[[2]], prob = TRUE)
    
    # Fitted vs. observed
    fitted_One <- predict(x$trafoOne)
    fitted_Two <- predict(x$trafoTwo)
    
    y_One <- model.response(x$trafoOne$model)
    y_Two <- model.response(x$trafoTwo$model)
    
    yFit_One %<a-% plot(fitted_One, y_One,
                         ylab = "Transformed y", xlab = "Fitted values",
                         main = x$trafos[[1]])
    yFit_Two %<a-% plot(fitted_Two, y_Two,
                          ylab = "Transformed y", xlab = "Fitted values",
                          main = x$trafos[[2]])
    
    # Scatterplots
    scatter_One %<a-% pairs(formula(x$trafoOne$terms), data = x$trafoOne$model,
                             main = x$trafos[[1]])
    scatter_Two %<a-% pairs(formula(x$trafoTwo$terms), data = x$trafoTwo$model,
                              main = x$trafos[[2]])
    
    
    scaleLoc_One %<a-%  plot(x$trafoOne, which = c(3L), main = x$trafos[[1]],
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    scaleLoc_Two %<a-%  plot(x$trafoTwo, which = c(3L), main = x$trafos[[2]],
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    cooks_One %<a-%  plot(x$trafoOne, which = c(4L), main = x$trafos[[1]],
                           labels.id = 1:n, cex.oma.main = 1.15, 
                           sub.caption = "")
    cooks_Two %<a-%  plot(x$trafoTwo, which = c(4L), main = x$trafos[[2]],
                            labels.id = 1:n, cex.oma.main = 1.15, 
                            sub.caption = "")
    
    residLev_One %<a-%  plot(x$trafoOne, which = c(5L), main = x$trafos[[1]],
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    residLev_Two %<a-%  plot(x$trafoTwo, which = c(5L), main = x$trafos[[2]],
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    dev.hold()
    old.par <- par(mfrow = c(1, 1))
    par(mfrow = c(1, 2))  
    # Normality
    QQ_One
    QQ_Two
    cat("Press [enter] to continue")
    line <- readline()
    hist_One
    mtext("Histogram", 3, 0.25, cex = 1)
    hist_Two
    mtext("Histogram", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    # Homoscedasticity
    residFit_One
    residFit_Two
    cat("Press [enter] to continue")
    line <- readline()
    scaleLoc_One
    scaleLoc_Two
    cat("Press [enter] to continue")
    line <- readline()
    # Linearity
    yFit_One
    abline(lm(as.numeric(y_One) ~ as.numeric(fitted_One)),col = "red",lwd = 1.5)
    mtext("Transformed observed vs Fitted", 3, 0.25, cex = 1)
    yFit_Two
    abline(lm(as.numeric(y_Two) ~ as.numeric(fitted_Two)),col = "red",lwd = 1.5)
    mtext("Transformed observed vs Fitted", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    par(old.par)
    scatter_One
    mtext("Scatter plot", 3, 0.25, outer = TRUE, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    scatter_Two   
    mtext("Scatter plot", 3, 0.25, outer = TRUE, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    old.par <- par(mfrow = c(1, 1))
    par(mfrow = c(1, 2))
    cooks_One
    cooks_Two
    cat("Press [enter] to continue")
    line <- readline()
    residLev_One
    residLev_Two
    par(old.par)
    dev.flush()
    
  } else if (inherits(x$trafoOne, "lme")) {
    residOne <- residuals(x$trafoOne, level = 0, type = "pearson")
    residTwo <- residuals(x$trafoTwo, level = 0, type = "pearson")
    
    QQ_residOne <- NULL
    QQ_residOne %<a-% qqnorm(residOne,
                               ylab = "Sample-quantiles Pearson residuals",
                               main = x$trafos[[1]])
    QQ_residTwo <- NULL
    QQ_residTwo %<a-% qqnorm(residTwo,
                                ylab = "Sample-quantiles Pearson residuals",
                                main = x$trafos[[2]])
    hist_residOne <- NULL
    hist_residOne %<a-% hist(residOne, nclass = 20, xlab = "Pearson residuals", 
                               main = x$trafos[[1]], prob = TRUE)
    hist_residTwo <- NULL
    hist_residTwo %<a-% hist(residTwo, nclass = 20, xlab = "Pearson residuals", 
                                main = x$trafos[[2]], prob = TRUE)

    
    ranefOne <- ranef(x$trafoOne)$'(Intercept)'
    sranefOne <- (ranefOne - mean(ranefOne)) / sd(ranefOne)
    
    ranefTwo <- ranef(x$trafoTwo)$'(Intercept)'
    sranefTwo <- (ranefTwo - mean(ranefTwo)) / sd(ranefTwo)
    
    QQ_sranefOne <- NULL
    QQ_sranefOne %<a-% qqnorm(sranefOne,
                                 ylab = "Sample-quantiles Std. random effects",
                                 main = x$trafos[[1]])
    QQ_sranefTwo <- NULL
    QQ_sranefTwo %<a-% qqnorm(sranefTwo,
                                ylab = "Sample-quantiles Std. random effects",
                                main = x$trafos[[2]])
    
    hist_sranefOne <- NULL
    hist_sranefOne %<a-% hist(sranefOne, nclass = 20, xlab = "Std. random effects", 
                                main = x$trafos[[1]], prob = TRUE)
    hist_sranefTwo <- NULL
    hist_sranefTwo %<a-% hist(sranefTwo, nclass = 20, xlab = "Std. random effects", 
                                 main = x$trafos[[2]], prob = TRUE)
    
    
    
    dev.hold()
    old.par <- par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
    par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))  
    QQ_residOne
    qqline(residOne)
    mtext("Normal Q-Q Plots", 3, 0.25, cex = 1)
    QQ_residTwo
    qqline(residTwo)
    mtext("Normal Q-Q Plots", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    hist_residOne
    mtext("Histogram", 3, 0.25, cex = 1)
    hist_residTwo
    mtext("Histogram", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    QQ_sranefOne
    qqline(sranefOne)
    mtext("Normal Q-Q Plots", 3, 0.25, cex = 1)
    QQ_sranefTwo
    qqline(sranefTwo)
    mtext("Normal Q-Q Plots", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    hist_sranefOne
    mtext("Histogram", 3, 0.25, cex = 1)
    hist_sranefTwo
    mtext("Histogram", 3, 0.25, cex = 1)
    par(old.par)
    dev.flush()
  }
  invisible()
}