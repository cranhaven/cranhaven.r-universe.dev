#' Plot for regression models with untransformed and transformed dependent 
#' variable
#'
#' For the untransformed and transformed model a range of plots is returned in 
#' order to check model assumptions graphically. 
#' 
#' @param x an object of type \code{trafo_lm}
#' @param ... additional arguments that are not used in this method
#' @importFrom pryr %<a-%
#' @importFrom grDevices dev.flush dev.hold
#' @importFrom graphics abline par plot hist mtext pairs panel.smooth
#' @importFrom graphics strwidth text
#' @importFrom stats cooks.distance formula predict qqline qqnorm cor
#' @export



plot.trafo_lm <- function(x, ...) {
  
  
  QQ_orig <- NULL
  QQ_trafo <- NULL
  hist_orig <- NULL
  hist_trafo <- NULL
  residFit_orig <- NULL
  residFit_trafo <- NULL
  yFit_orig <- NULL
  yFit_trafo <- NULL
  scatter_orig <- NULL
  scatter_trafo <- NULL
  cooks_orig <- NULL
  cooks_trafo <- NULL
  scaleLoc_orig <- NULL
  scaleLoc_trafo <- NULL
  residLev_orig <- NULL
  residLev_trafo <- NULL
  QQ_resid_orig <- NULL
  QQ_resid_trafo <- NULL
  hist_resid_orig <- NULL
  hist_resid_trafo <- NULL
  QQ_sranef_orig <- NULL
  QQ_sranef_trafo <- NULL
  hist_sranef_orig <- NULL
  hist_sranef_trafo <- NULL
  
  
  if (inherits(x$orig_mod, "lm")) {
    
    n <- length(x$orig_mod$residuals)
    
    
    # QQ plots
    QQ_orig %<a-%  plot(x$orig_mod, which = c(2L), main = "Untransformed model",
                        labels.id = 1:n, cex.oma.main = 1.15, 
                        sub.caption = "")
    QQ_trafo %<a-%  plot(x$trafo_mod, which = c(2L), main = "Transformed model",
                         labels.id = 1:n, cex.oma.main = 1.15, 
                         sub.caption = "")
    
    # Histogram
    resid_orig <- residuals(x$orig_mod, level = 0, type = "pearson")
    resid_trafo <- residuals(x$trafo_mod, level = 0, type = "pearson")
    
    
    hist_orig %<a-% hist(resid_orig, nclass = 20, xlab = "Pearson residuals", 
                           main = "Untransformed model", prob = TRUE)
    hist_trafo %<a-% hist(resid_trafo, nclass = 20, xlab = "Pearson residuals", 
                         main = "Transformed model", prob = TRUE)
    

    
    # Residuals vs Fitted
    residFit_orig %<a-%  plot(x$orig_mod, which = c(1L), main = "Untransformed model",
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    residFit_trafo %<a-%  plot(x$trafo_mod, which = c(1L), main = "Transformed model",
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    # Fitted vs. observed
    fitted_orig <- predict(x$orig_mod)
    fitted_trafo <- predict(x$trafo_mod)
    
    y_orig <- model.response(x$orig_mod$model)
    y_trafo <- model.response(x$trafo_mod$model)
    
    yFit_orig %<a-% plot(fitted_orig, y_orig,
                         ylab = "y", xlab = "Fitted values",
                         main = "Untransformed model")
    yFit_trafo %<a-% plot(fitted_trafo, y_trafo,
                          ylab = "Transformed y", xlab = "Fitted values",
                          main = "Transformed model")
    
    # Scatterplots
    scatter_orig %<a-% pairs(formula(x$orig_mod$terms), data = x$orig_mod$model,
                         main = "Untransformed model", lower.panel = panel.smooth, 
                         upper.panel = panel.cor)
    scatter_trafo %<a-% pairs(formula(x$trafo_mod$terms), data = x$trafo_mod$model,
                             main = "Transformed model", lower.panel = panel.smooth, 
                             upper.panel = panel.cor)
    
    cooks_orig %<a-%  plot(x$orig_mod, which = c(4L), 
                           main = "Untransformed model",
                           labels.id = 1:n, cex.oma.main = 1.15, 
                           sub.caption = "")
    cooks_trafo %<a-%  plot(x$trafo_mod, which = c(4L), 
                            main = "Transformed model",
                            labels.id = 1:n, cex.oma.main = 1.15, 
                            sub.caption = "")
    
    scaleLoc_orig %<a-%  plot(x$orig_mod, which = c(3L), 
                              main = "Untransformed model",
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    scaleLoc_trafo %<a-%  plot(x$trafo_mod, which = c(3L), 
                               main = "Transformed model",
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    residLev_orig %<a-%  plot(x$orig_mod, which = c(5L), 
                              main = "Untransformed model",
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    residLev_trafo %<a-%  plot(x$trafo_mod, which = c(5L), 
                               main = "Transformed model",
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    dev.hold()
    old.par <- par(mfrow = c(1, 1))
    par(mfrow = c(1, 2))  
    # Normality
    QQ_orig
    QQ_trafo
    cat("Press [enter] to continue")
    line <- readline()
    hist_orig
    mtext("Histogram", 3, 0.25, cex = 1)
    hist_trafo
    mtext("Histogram", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    # Homoscedasticity
    residFit_orig
    residFit_trafo
    cat("Press [enter] to continue")
    line <- readline()
    scaleLoc_orig
    scaleLoc_trafo
    cat("Press [enter] to continue")
    line <- readline()
    # Linearity
    yFit_orig
    abline(lm(as.numeric(y_orig) ~ as.numeric(fitted_orig)),col = "red",lwd = 1.5)
    mtext("Observed vs Fitted", 3, 0.25, cex = 1)
    yFit_trafo
    abline(lm(as.numeric(y_trafo) ~ as.numeric(fitted_trafo)),col = "red",lwd = 1.5)
    mtext("Transformed observed vs Fitted", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    par(old.par)
    scatter_orig
    #mtext("Scatter plot", 3, 0.25, outer = FALSE, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    scatter_trafo    
    #mtext("Scatter plot", 3, 0.25, outer = FALSE, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    old.par <- par(mfrow = c(1, 1))
    par(mfrow = c(1, 2))
    cooks_orig
    cooks_trafo
    cat("Press [enter] to continue")
    line <- readline()
    residLev_orig
    residLev_trafo
    par(old.par)
    dev.flush()
    
  } else if (inherits(x$orig_mod, "lme")) {
    resid <- residuals(x$orig_mod, level = 0, type = "pearson")
    residt <- residuals(x$trafo_mod, level = 0, type = "pearson")
    
    
    QQ_resid_orig %<a-% qqnorm(resid,
                          ylab = "Sample-quantiles Pearson residuals",
                          main = "Untransformed model")
    QQ_resid_trafo %<a-% qqnorm(residt,
                           ylab = "Sample-quantiles Pearson residuals",
                           main = "Transformed model")
    
    hist_resid_orig %<a-% hist(resid, nclass = 20, xlab = "Pearson residuals", 
                                main = "Untransformed model", prob = TRUE)
    hist_resid_trafo %<a-% hist(residt, nclass = 20, xlab = "Pearson residuals", 
                                main = "Transformed model", prob = TRUE)
    
    
    #n <- length(residt)
    
    # residFit_orig %<a-% plot_lm_adj(x$orig_mod, which = c(1L), sub.caption = "",
    #                                labels.id = 1:n, 
    #                                main = "Untransformed model")
    # residFit_trafo %<a-% plot_lm_adj(x$trafo_mod, which = c(1L), sub.caption = "",
    #                                 labels.id = 1:n, 
    #                                 main = "Transformed model") 
    
  #  scaleLoc_orig %<a-%  plot_lm_adj(x$orig_mod, which = c(3L), 
  #                                   main = "Untransformed model",
  #                            labels.id = 1:n, cex.oma.main = 1.15, 
  #                            sub.caption = "")
  #  scaleLoc_trafo %<a-%  plot_lm_adj(x$trafo_mod, which = c(3L), 
  #                                    main = "Transformed model",
  #                             labels.id = 1:n, cex.oma.main = 1.15, 
  #                             sub.caption = "")
    
 #   residLev_orig %<a-%  plot(x$orig_mod, which = c(5L), 
#                              main = "Untransformed model",
#                              labels.id = 1:n, cex.oma.main = 1.15, 
#                              sub.caption = "")
 #   residLev_trafo %<a-%  plot(x$trafo_mod, which = c(5L), 
#                               main = "Transformed model",
 #                              labels.id = 1:n, cex.oma.main = 1.15, 
 #                              sub.caption = "")
    
    raneft <- ranef(x$trafo_mod)$'(Intercept)'
    sraneft <- (raneft - mean(raneft)) / sd(raneft)
    
    ranef <- ranef(x$orig_mod)$'(Intercept)'
    sranef <- (ranef - mean(ranef)) / sd(ranef)
    
    QQ_sranef_trafo %<a-% qqnorm(sraneft,
                            ylab = "Sample-quantiles Std. random effects",
                            main = "Transformed model")
    QQ_sranef_orig %<a-% qqnorm(sranef,
                            ylab = "Sample-quantiles Std. random effects",
                            main = "Untransformed model")
    
    hist_sranef_orig %<a-% hist(sranef, nclass = 20, xlab = "Std. random effects", 
                               main = "Untransformed model", prob = TRUE)
    hist_sranef_trafo %<a-% hist(sraneft, nclass = 20, xlab = "Std. random effects", 
                                main = "Transformed model", prob = TRUE)
    
    
    
    dev.hold()
    old.par <- par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
    par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))  
    #old.par <- par(mfrow = c(1, 1))
    #par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
    # residFit_orig
    # residFit_trafo
    # cat("Press [enter] to continue")
    # line <- readline()
    #old.par <- par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
    QQ_resid_orig
    qqline(resid)
    mtext("Normal Q-Q Plots", 3, 0.25, cex = 1)
    QQ_resid_trafo
    qqline(residt)
    mtext("Normal Q-Q Plots", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    hist_resid_orig
    mtext("Histogram", 3, 0.25, cex = 1)
    hist_resid_trafo
    mtext("Histogram", 3, 0.25, cex = 1)
    #mtext("Normal Q-Q Plots - Pearson residuals", outer = TRUE, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    QQ_sranef_orig
    qqline(sranef)
    mtext("Normal Q-Q Plots", 3, 0.25, cex = 1)
    QQ_sranef_trafo
    qqline(sraneft)
    mtext("Normal Q-Q Plots", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    hist_sranef_orig
    mtext("Histogram", 3, 0.25, cex = 1)
    hist_sranef_trafo
    mtext("Histogram", 3, 0.25, cex = 1)
  #  cat("Press [enter] to continue")
  #  line <- readline()
  #  old.par <- par(mfrow = c(1, 2))
  #  scaleLoc_orig
  #  scaleLoc_trafo
  #  par(old.par)
   # cat("Press [enter] to continue")
  #  line <- readline()
  #  old.par <- par(mfrow = c(1, 2))
  #  residLev_orig
   # residLev_trafo
    par(old.par)
    dev.flush()
    
  }
  invisible()
}


panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}