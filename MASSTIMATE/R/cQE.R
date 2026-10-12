cQE <-
function(FC,equation="raw",cor=2,quadratic=FALSE,data=NULL, return_PI = FALSE) {
  if(equation=="raw") {
    ols.fit <- mod_OLS(verbose = TRUE, plot = FALSE)
    ols.fit.pred <- predict(ols.fit$model, newdata = data.frame(xvar = log10(FC*sqrt(cor))), interval = "prediction")
    log.cQE <- ols.fit.pred[,1]
    cQE<-round(10^log.cQE,1)
    ppe.err<-cQE*0.2563
    upper.cQE.PPE<-round(cQE+ppe.err,1)
    lower.cQE.PPE<-round(cQE-ppe.err,1)
    upper.cQE.95PI <- round(10^(ols.fit.pred[,3]), 1)
    lower.cQE.95PI <- round(10^(ols.fit.pred[,2]), 1)
    if(quadratic) {
      quad.fit <- mod_QUAD(verbose = TRUE, plot = FALSE)
      quad.fit.pred <- predict(quad.fit$model, newdata = data.frame(xvar = log10(FC*sqrt(cor))), interval = "prediction")
      log.qcQE <- quad.fit.pred[,1]
      qcQE <- round(10^log.qcQE, 1)
      ppe.err <- qcQE * 0.2537316
      upper.qcQE.PPE <- round(qcQE + ppe.err, 1)
      lower.qcQE.PPE <- round(qcQE - ppe.err, 1)
      upper.qcQE.95PI <- round(10^(quad.fit.pred[,3]), 1)
      lower.qcQE.95PI <- round(10^(quad.fit.pred[,2]), 1)
    }
  }
  if(equation=="phylocor") {
    if(return_PI) stop("Prediction intervals are currently only available for the non-phylogenetically corrected equation.")
    log.cQE<-2.754*log10(FC*sqrt(cor))-1.097
    cQE<-round(10^log.cQE,1)
    ppe.err<-cQE*0.2503
    upper.cQE.PPE<-round(cQE+ppe.err,1)
    lower.cQE.PPE<-round(cQE-ppe.err,1)
    if(quadratic) {
      cor.FC <- log10(FC*sqrt(cor))
      log.qcQE <- -0.0585856 * cor.FC^2 + 2.9629676 * cor.FC - 1.2646675
      qcQE <- round(10^log.qcQE, 1)
      ppe.err <- qcQE * 0.2469842
      upper.qcQE.PPE <- round(qcQE + ppe.err, 1)
      lower.qcQE.PPE <- round(qcQE - ppe.err, 1)
    }
  }
  if(quadratic) {
    res <- cbind(data, log.cQE, cQE, lower.cQE.PPE, upper.cQE.PPE, log.qcQE, qcQE, lower.qcQE.PPE, upper.qcQE.PPE)
    if(return_PI) res <- cbind(data, log.cQE, cQE, lower.cQE.PPE, upper.cQE.PPE, lower.cQE.95PI, upper.cQE.95PI, log.qcQE, qcQE, lower.qcQE.PPE, upper.qcQE.PPE, lower.qcQE.95PI, upper.qcQE.95PI)
  }
  else {
    res <- cbind(data, log.cQE, cQE, lower.cQE.PPE, upper.cQE.PPE)
    if(return_PI) res <- cbind(data, log.cQE, cQE, lower.cQE.PPE, upper.cQE.PPE, lower.cQE.95PI, upper.cQE.95PI)
  }
  return(res)
}
