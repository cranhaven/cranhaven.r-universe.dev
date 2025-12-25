.onLoad<-function(libname, pkgname){
  
if (!identical(objective@system, R.version$system)) 
  {
  objective <- cxxfunctionplus2(methods::signature(), body=objective.body,
                                includes=objective.include, plugin="RcppArmadillo",save.dso=TRUE)
  utils::assignInNamespace("objective", objective, ns = "slasso")

  gradient <- cxxfunctionplus2(methods::signature(), body=gradient.body,
                               includes=gradient.include, plugin="RcppArmadillo",save.dso=TRUE)
  utils::assignInNamespace("gradient", gradient, ns = "slasso")
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Smooth LASSO Estimator for the Function-on-Function Linear Regression Model")
}
