#Funciones tomadas del paquete ADABAG 

#Evolución del error 
error <- function (object, newdata) 
{
  if (!("boosting" %in% class(object))) {
    stop("object must of class 'boosting' ")
  }
  vardep <- newdata[, as.character(object$formula[[2]])]
  mfinal <- length(object$trees)
  n <- length(newdata[, 1])
  nclases <- nlevels(vardep)
  
  ponderacion <- object$weights
  
  erroracum <- rep(0, mfinal)
  pred <- as.data.frame(sapply(object$trees, predict, newdata = newdata, 
                               type = "class"))
  mvotos <- list()
  classfinal <- array(0, c(n, nlevels(vardep)))
  for (i in 1:nlevels(vardep)) {
    mvotos[[i]] <- matrix(as.numeric(pred == levels(vardep)[i]), 
                          nrow = n) %*% diag(ponderacion)
  }
  for (j in 1:mfinal) {
    if (j == 1) {
      for (i in 1:nlevels(vardep)) {
        classfinal[, i] <- mvotos[[i]][, 1]
      }
    }
    else {
      for (i in 1:nlevels(vardep)) {
        classfinal[, i] <- apply(cbind(classfinal[, i], 
                                       mvotos[[i]][, j]), 1, sum)
      }
    }
    predclass <- rep("O", n)
    predclass[] <- apply(classfinal, 1, FUN = selectada, vardep.summary = summary(vardep))
    error <- 1 - sum(predclass == vardep)/n
    erroracum[j] <- error
  }
  output <- list(error = erroracum)
  class(output) <- "errorevol"
  output
}

#Select
selectada <- function (fila, vardep.summary, ...) 
{
  if (length(which(fila == max(fila))) > 1) {
    predclass <- names(vardep.summary[which(fila == max(fila))])[order(vardep.summary[which(fila == 
                                                                                              max(fila))], decreasing = TRUE)[1]]
  }
  else {
    predclass <- as.character(names(vardep.summary)[(order(fila, 
                                                           decreasing = TRUE)[1])])
  }
  predclass
}

#Reglas
rules <- function (model, train, var.pred, compact = FALSE, ...){
  if (!inherits(model, "rpart"))
    stop(Rtxt("Not a legitimate rpart tree"))
  rtree <- length(attr(model, "ylevels")) == 0
  target <- as.character(attr(model$terms, "variables")[2])
  frm <- model$frame
  names <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <- model$frame[1, ]$n
  if (rtree){
    ordered <- rev(sort(frm$n, index = TRUE)$ix)
  } else {
    ordered <- rev(sort(frm$yval2[, 5], index = TRUE)$ix)
  }
  for (i in ordered) {
    if (frm[i, 1] == "<leaf>") {
      if (rtree)
        yval <- frm[i, ]$yval
      else {
        yval <- ylevels[frm[i, ]$yval]
        yval <- ifelse(yval == -1, 1, 2)
        yval <- levels(train[,var.pred])[yval]
      }
      cover <- frm[i, ]$n
      pcover <- round(100 * cover/ds.size)
      if (!rtree)
        prob <- frm[i, ]$yval2[, 5]
      cat("\n")
      pth <- path.rpart(model, nodes = as.numeric(names[i]),
                               print.it = FALSE)
      pth <- unlist(pth)[-1]
      if (!length(pth))
        pth <- "True"
      if (compact) {
        cat(sprintf("R%03s ", names[i]))
        if (rtree)
          cat(sprintf("[%2.0f%%,%0.2f]", pcover, prob))
        else cat(sprintf("[%2.0f%%,%0.2f]", pcover, prob))
        cat(sprintf(" %s", pth), sep = "")
      }
      else {
        cat(sprintf(Rtxt("Rule number: %s "), names[i]))
        if (rtree){
          cat(sprintf("[%s=%s cover=%d (%.0f%%)]\n", target, yval, cover, pcover))
        }else{
          cat(sprintf("[%s=%s cover=%d (%.0f%%) prob=%0.2f]\n", target, yval, cover, pcover, prob))
        }
        cat(sprintf("  %s\n", pth), sep = "")
      }
    }
  }
  cat("\n")
  invisible(ordered)
}

#Funciones tomadas del paquete RATTLE
Rtxt <- function (...) 
{
  gettext(paste(...), domain = "R-rattle")
}