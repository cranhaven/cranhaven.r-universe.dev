QE <-
function(HFC=NULL, HC, FC, equation="raw", quadratic=FALSE, data=NULL,  return_PI = FALSE) {
  if(is.null(HFC)) HFC <- HC+FC
  if(equation=="raw") {
    ols.fit <- mod_OLS(verbose = TRUE, plot = FALSE)
    ols.fit.pred<-predict(ols.fit$model, newdata = data.frame(xvar = log10(HFC)), interval = "prediction")
    log.QE <- ols.fit.pred[,1]
    QE<-round(10^log.QE, 1)
    ppe.err<-QE*0.2563
    upper.QE.PPE<-round(QE + ppe.err, 1)
    lower.QE.PPE<-round(QE - ppe.err, 1)
    upper.QE.95PI <- round(10^(ols.fit.pred[,3]), 1)
    lower.QE.95PI <- round(10^(ols.fit.pred[,2]), 1)
    if(quadratic) {
      quad.fit <- mod_QUAD(verbose = TRUE, plot = FALSE)
      quad.fit.pred <- predict(quad.fit$model, newdata = data.frame(xvar = log10(HFC)), interval = "prediction")
      log.qQE <- quad.fit.pred[,1]
      qQE <- round(10^log.qQE,  1)
      ppe.err <- qQE * 0.2537316
      upper.qQE.PPE <- round(qQE + ppe.err,  1)
      lower.qQE.PPE <- round(qQE - ppe.err,  1)
      upper.qQE.95PI <- round(10^(quad.fit.pred[,3]), 1)
      lower.qQE.95PI <- round(10^(quad.fit.pred[,2]), 1)
    }
  }
  if(equation=="phylocor") {
    if(return_PI) stop("Prediction intervals are currently only available for the non-phylogenetically corrected equation.")
    log.QE<-2.754*log10(HFC)-1.097
    QE<-round(10^log.QE, 1)
    ppe.err<-QE*0.2503
    upper.QE.PPE<-round(QE+ppe.err, 1)
    lower.QE.PPE<-round(QE-ppe.err, 1)
    if(quadratic) {
      log.hfc <- log10(HFC)
      log.qQE <- -0.0585856 * log.hfc^2 + 2.9629676 * log.hfc - 1.2646675
      qQE <- round(10^log.qQE,  1)
      ppe.err <- qQE * 0.2469842
      upper.qQE.PPE <- round(qQE + ppe.err,  1)
      lower.qQE.PPE <- round(qQE - ppe.err,  1)
    }
  }
  if(quadratic) {
    res <- cbind(data, log.QE, QE, lower.QE.PPE, upper.QE.PPE, log.qQE, qQE, lower.qQE.PPE, upper.qQE.PPE)
    if(return_PI) res <- cbind(data, log.QE, QE, lower.QE.PPE, upper.QE.PPE, lower.QE.95PI, upper.QE.95PI, log.qQE, qQE, lower.qQE.PPE, upper.qQE.PPE, lower.qQE.95PI, upper.qQE.95PI)
  }
  else {
    res <- cbind(data, log.QE, QE, lower.QE.PPE, upper.QE.PPE)
    if(return_PI) res <- cbind(data, log.QE, QE, lower.QE.PPE, upper.QE.PPE, lower.QE.95PI, upper.QE.95PI)
  }
  return(res)
}
