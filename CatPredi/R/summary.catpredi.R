summary.catpredi <-
function(object, digits = 4, ...) {
	object$digits <- digits
	var.names <- all.vars(object$formula)
  formula <- object$formula
  data <- object$data
  X <- data[,object$cat.var]
  Y <- data[,var.names[1]]
  cutoffs <- sort(unique(c(max(X, na.rm=TRUE), min(X, na.rm=TRUE), object$results$cutpoints)))
  x.cut <- cut(X, cutoffs, include.lowest=TRUE,right=TRUE)
  name_var_cat <- paste(object$cat.var,"_cat",sep="")
  data[,name_var_cat] <- x.cut
  new.formula <- paste("~ . + ", name_var_cat, sep = "")
  formula.n <- update(formula, as.formula(new.formula))
  fit <- gam(formula.n, family = binomial, data = data)
  object$fit.gam <- fit
	class(object) <- "summary.catpredi"
  return(object)
}
