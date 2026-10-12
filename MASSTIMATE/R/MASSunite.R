MASSunite <- function(HFC = NULL, HC, FC, BM, ES.type = "QE", gait, VD.par = NULL, line = FALSE, ...) {
  par(mar = c(5, 4, 1, 1), mfrow = c(1, 2))
  
  #ES standard plot  
  if(ES.type == "QE") std <- mod_OLS(verbose = TRUE, ...)
  if(ES.type == "qQE") std <- mod_QUAD(verbose = TRUE, ...)
  
  #ES body mass estimation
  if(gait == "biped") {
    HFC <- FC * sqrt(2) #apply corrections factor
    est.BM <- cQE(FC)
    if(ES.type == "qQE")  est.BM <- cQE(FC, quadratic = TRUE)
  }
  if (gait == "quadruped") {
    if(is.null(HFC)) HFC <- HC + FC
    est.BM <- QE(HFC)
    if(ES.type == "qQE")  est.BM <- QE(HFC, quadratic = TRUE)
  }

  points(log10(HFC), log10(BM), pch = VD.par$pch, col = VD.par$col)
  if(line) {
    mod.line <- lm(log10(BM)~log10(HFC))
    abline(mod.line, col = VD.par$col)
  }
  if(!is.null(VD.par$names)) text(log10(HFC), log10(BM), labels = VD.par$names, pos = 4)
  #deviation metrics
  res.BM <- log10(BM)-est.BM[,1]
  ppe.BM <- ppe(BM, est.BM[,2])
  
  #residual plot
  par(mar = c(5, 4, 1, 1))
  if(length(res.BM > 1)) boxplot(res.BM, ylim = range(res.BM, na.rm = TRUE), pch = VD.par$pch, col = VD.par$col, ylab = "Residual Deviations")
  else plot(1, res.BM, ylim = range(res.BM, na.rm = TRUE), pch = VD.par$pch, col = VD.par$col, ylab = "Residual Deviations")
  upper.error <- std$prediction[,3] - std$prediction[,1]
  lower.error <- std$prediction[,2] - std$prediction[,1]
  abline(h = range(upper.error), lty = 2)
  abline(h = range(lower.error), lty = 2)
  
  #outliers
  outs <- which(res.BM > max(upper.error) | res.BM < min(lower.error))
  if(!is.null(VD.par$names) & length(outs) > 0) text(rep(1, length(outs)), res.BM[outs], labels = VD.par$names[outs], pos = 4)
  if(length(outs) >= 1 & line == TRUE) {
    res <- list(Residual = res.BM, mean.Residual = mean(res.BM, na.rm = TRUE), PPE = ppe.BM, Inferred.Line = mod.line, Outliers = outs, Caution = warning("One or more of the models are inconsistent with ES expectations"))
  }
  if(length(outs) >= 1 & line != TRUE) {
    res <- list(Residual = res.BM, mean.Residual = mean(res.BM, na.rm = TRUE), PPE = ppe.BM, Outliers = outs, Caution = warning("One or more of the models are inconsistent with ES expectations"))
  }
  if(length(outs) < 1 & line == TRUE) {
    res <- list(Residual = res.BM, mean.Residual = mean(res.BM, na.rm = TRUE), PPE = ppe.BM, Inferred.Line = mod.line)
  }
  if(length(outs) < 1 & line != TRUE) {
    res <- list(Residual = res.BM, mean.Residual = mean(res.BM, na.rm = TRUE), PPE = ppe.BM)
  }
  return(res)
}