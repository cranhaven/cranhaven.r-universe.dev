compare.AUC.ht <- 
function(obj1, obj2, level = 0.95, nb = 100, parallel = TRUE, plot = TRUE){
  if ((length(obj2$results$cutpoints) - length(obj1$results$cutpoints)) != 1) {
    stop(paste("Models (model 1 and model 2) do not have respectively", length(obj1$results$cutpoints), "and", length(obj1$results$cutpoints) + 1 ,"cut-off points."))
  }
  if (identical(obj1$formula, obj2$formula)){
    formula.ht <- obj1$formula
  } else {
    stop("The formulas of the models are not identical. Please verify that both formulas are the same.")
  }
  if (identical(obj1$cat.var, obj2$cat.var)) {
    cat.var.ht <- obj1$cat.var
  } else {
    stop("The variable to categorise is not the same in both models. Please ensure you are using the same variables.")
  }
  if (identical(obj1$data[, -ncol(obj1$data)],obj2$data[, -ncol(obj2$data)])) {
    data.ht <- obj1$data
  } else {
    stop("The datasets are not the same in both models. Please make sure you are using the same data.")
  }
  if (identical(obj1$method, obj2$method)) {
    method.ht <- obj1$method
  } else {
    stop("The methods are not the same in both models. Please ensure you are using the same method.")
  }
  if (identical(obj1$range, obj2$range)) {
    range.ht <- obj1$range
  } else {
    stop("Range argument is not the same in both models. Please verify they are specified in the same way.")
  }
  if (identical(obj1$correct.AUC, obj2$correct.AUC)) {
    correct.AUC.ht <- obj1$correct.AUC
  } else {
    stop("The correct.AUC argument is not the same in both models. Please verify they are defined in the same way.")
  }
  
  Tstatistic <- obj2$results$AUC - obj1$results$AUC
  
  nc <- length(obj1$results$cutpoints)
  variables <- all.vars(formula.ht)
  # res <- catpredi(formula = formula, cat.var = cat.var, cat.points = nc, data, method = method, range = range, control = controlcatpredi())
  formula.ht.n <- update(formula.ht, as.formula(paste0("~ . +", cat.var.ht, "_CatPredi")))
  model <- mgcv::gam(formula.ht.n, data = data.ht, family = "binomial")
  p.boot <- predict(model, data.ht, type = "response")
  
  if (parallel) {
    n.cores <- max(1, parallel::detectCores() - 1)
    
    tryCatch({
      doParallel::registerDoParallel(n.cores)
    }, error = function(e) {
      message("Parallel setup failed: ", conditionMessage(e))
      message("Setting number of cores to 2.")
      n.cores <- 2
      doParallel::registerDoParallel(n.cores)
    })
    
  } else {
    n.cores <- 1
    doParallel::registerDoParallel(n.cores)
  }
  
  # Bootstrap
  df1.all <- foreach::foreach(i = 1:nb, .verbose = FALSE, .errorhandling = "stop", .inorder=FALSE, .combine = c) %dopar% {
    y <- rbinom(n = nrow(data.ht), size = 1, prob = p.boot)
    data.ht$y <- y
    formula.n <- update(formula.ht, as.formula("y ~ ."))
    df1 <- catpredi(formula = formula.n, cat.var=cat.var.ht, cat.points = nc+1, data=data.ht, method = method.ht, range = range.ht, control = controlcatpredi())$results$AUC[length(catpredi(formula=formula.n, cat.var=cat.var.ht, cat.points = nc+1, data=data.ht, method = method.ht, range = range.ht, control = controlcatpredi())$results$AUC)]-
      catpredi(formula = formula.n, cat.var=cat.var.ht, cat.points = nc, data=data.ht, method = method.ht, range = range.ht, control = controlcatpredi())$results$AUC[length(catpredi(formula=formula.n, cat.var=cat.var.ht, cat.points = nc, data=data.ht, method = method.ht, range = range.ht, control = controlcatpredi())$results$AUC)]
    return(df1)
    gc()
  }
  # Stop the cluster
  doParallel::stopImplicitCluster()
  
  if(plot){
    plot(density(df1.all), xlab = "t.boot", ylab = "", xlim = range(Tstatistic, density(df1.all)$x),
         main = paste0("Formula = ", as.expression(formula.ht.n), "\n", nc, " vs ", nc+1, " cut-off points"))
    t <- quantile(df1.all, level)
    abline(v = t, lty = 1)
    abline(v = Tstatistic, lty = 2)
    legend("topleft", legend = c(expression(t[level]),expression(t[null])), lty = 1:2, bty = "n", cex=1.5)
  }

  cat("\n\n*************************************************\n")
  cat(paste0("Bootstrap-based hypothesis test for ", nc, " vs ", nc+1, " cut points"))
  cat("\n*************************************************\n\n")
  
  if (Tstatistic <= t) {
    cat(paste("Don't reject the null hypothesis (H0). Thus, it is enough to take k =", nc, sep = " "))
  } else {
    cat(paste("Reject the null hypothesis (H0). Thus, it is advisable to take k =", nc + 1, sep = " "))
  }
  
  res<- list(t.null = Tstatistic, t.boost = df1.all, t.null = Tstatistic, t.alpha = t)
  class(res) <- "compare.AUC.ht"
  res  
}