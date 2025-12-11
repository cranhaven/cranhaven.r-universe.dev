#' @export

EasyTreeVarImp <- function(ct, nsim = 1) {
  
  XY <- ct[[1]]$data
  XY <- XY[,names(XY)!="(weights)"]
  Y <- ct[[1]]$fitted[,"(response)"]
  whereY <- which(sapply(XY, function(x) identical(x,Y)))
  X <- XY[,-whereY]
  
  if(is.numeric(Y)) {
    r2 <- vip::vi(ct, 
                  method = "permute",
                  nsim = nsim,
                  sort = FALSE,
                  train = X[complete.cases(XY),], 
                  target = Y[complete.cases(XY)],
                  metric = "rsq",
                  pred_wrapper = function(object,newdata) {predict(object,newdata)})
    tau <- vip::vi(ct, 
                   method = "permute",
                   nsim = nsim,
                   sort = FALSE,
                   train = X[complete.cases(XY),], 
                   target = Y[complete.cases(XY)],
                   metric = function(truth, estimate) {cor(truth, estimate, method="kendall")},
                   smaller_is_better = FALSE,
                   pred_wrapper = function(object,newdata) {predict(object,newdata)})
    imp_tot <- data.frame(Variable = r2$Variable, r2 = round(r2$Importance,3), tau = round(tau$Importance,3))
    colnames(imp_tot)[2:3] <- c("r2","tau")
  }
  
  if(is.factor(Y)) {
    acc <- vip::vi(ct, 
                   method = "permute",
                   nsim = nsim,
                   sort = FALSE,
                   train = X[complete.cases(XY),], 
                   target = Y[complete.cases(XY)],
                   metric = "accuracy",
                   smaller_is_better = FALSE,
                   pred_wrapper = function(object,newdata) {predict(object,newdata)})
    bac <- vip::vi(ct, 
                   method = "permute",
                   nsim = nsim,
                   sort = FALSE,
                   train = X[complete.cases(XY),], 
                   target = Y[complete.cases(XY)],
                   # metric = function(truth, estimate) {
                   #   mat <- table(truth, estimate)
                   #   return(mean(diag(mat)/rowSums(mat)))
                   # },
                   # metric = yardstick::bal_accuracy_vec,
                   metric = "bal_accuracy",
                   smaller_is_better = FALSE,
                   pred_wrapper = function(object,newdata) {predict(object,newdata)})
    tr <- list()
    for(i in 1:nlevels(Y)){
      tr[[i]] <- vip::vi(ct, 
                         method = "permute",
                         nsim = nsim,
                         sort = FALSE,
                         train = X[complete.cases(XY),], 
                         target = Y[complete.cases(XY)],
                         metric = function(truth, estimate) {
                           mat <- table(truth, estimate)
                           return(diag(mat)[i]/rowSums(mat)[i])
                         },
                         smaller_is_better = FALSE,
                         pred_wrapper = function(object,newdata) {predict(object,newdata)})[,2]
      tr[[i]] <- round(tr[[i]],3)
    }
    tr <- do.call("cbind.data.frame", tr)
    names(tr) <- paste(names(XY)[whereY],levels(Y))
    if(nlevels(Y)==2) {
      auc <- vip::vi(ct, 
                     method = "permute",
                     nsim = nsim,
                     sort = FALSE,
                     train = X[complete.cases(XY),], 
                     target = Y[complete.cases(XY)],
                     metric = "roc_auc",
                     event_level = "second",
                     smaller_is_better = FALSE,
                     pred_wrapper = function(object,newdata) {predict(object,newdata,type="prob")[,levels(Y)[2]]})
    }
    if(nlevels(Y)>2) {
      auc <- vip::vi(ct, 
                     method = "permute",
                     nsim = nsim,
                     sort = FALSE,
                     train = X[complete.cases(XY),], 
                     target = Y[complete.cases(XY)],
                     # metric = "mauc",
                     metric = function(truth, estimate) {
                       as.numeric(pROC::multiclass.roc(truth, estimate)$auc)
                     },
                     smaller_is_better = FALSE,
                     pred_wrapper = function(object,newdata) {predict(object,newdata,type="prob")})
    }
    imp_tot <- data.frame(Variable = acc$Variable, auc = round(auc$Importance,3), acc = round(acc$Importance,3), bac = round(bac$Importance,3), tr)
    colnames(imp_tot)[2:4] <- c("AUC","accuracy","balanced accuracy")
  }
  rownames(imp_tot) <- NULL
  return(imp_tot)
}
