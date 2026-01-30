#' Plot for optimal transformation parameter - linear models
#' 
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter.
#' @param lambdaoptim optimal or given transformation parameter.
#' @param measoptim measure at the optimal transformation parameter.
#' @param y dependent variable.
#' @param x matrix of regressors
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Maximum likelihood approach ("ml"), (ii) Skewness minimization ("skew"),
#' (iii) Kurtosis optimization ("kurt"), (iv) Divergence minimization by 
#' Kolmogorov-Smirnov ("div.ks"), by Cramer-von-Mises ("div.cvm") or by 
#' Kullback-Leibler ("div.kl"). Defaults to "ml". In case of no and
#' log transformation "NA" can be selected since no optimization is necessary
#' for these two transformation types.
#' @param trafo a character string that selects the transformation.
#' @keywords internal


plot_trafolm <- function(lambdarange, lambdaoptim, measoptim, 
                         y, x, trafo, method, custom_func, 
                         custom_func_std) {
  
  
  if (diff(range(lambdarange)) <= 1000) {
    tol <- 0.025
  } else if (diff(range(lambdarange)) > 1000 && diff(range(lambdarange)) <= 2000) {
    tol <- 1
  } else if (diff(range(lambdarange)) > 2000 && diff(range(lambdarange)) <= 5000) {
    tol <- 1.5
  } else if (diff(range(lambdarange)) > 5000 && diff(range(lambdarange)) <= 10000) {
    tol <- 2
  } else if (diff(range(lambdarange)) > 10000 && diff(range(lambdarange)) <= 50000) {
    tol <- 5
  } else if (diff(range(lambdarange)) > 50000 && diff(range(lambdarange)) <= 100000) {
    tol <- 10
  } else if (diff(range(lambdarange)) > 100000) {
    tol <- 100
  }
  
  lambdavector <- seq(lambdarange[1], lambdarange[2], tol)
  l <- length(lambdavector)
  lambdavector[l + 1]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  measvector <- sapply(lambdavector, estim_lm, y = y, x = x, trafo = trafo,
                       method = method, custom_func = custom_func, 
                       custom_func_std = custom_func_std)
  vline <- lambdaoptim
  
  if (method == "ml" | method == "reml") {
    measvector <- -measvector
    data1 <- data.frame(measvector = measvector,  lambdavector = lambdavector)  
    measoptim <- -measoptim
    y_lab <- "Profile log-likelihood"
    
    
  } else if (method == "skew" | method == "pskew") {
    data1 <- data.frame(measvector = measvector,  lambdavector = lambdavector)
    y_lab <- "Skewness"
  } else if (method == "kurt") {
    data1 <- data.frame(measvector = measvector,  lambdavector = lambdavector)
    y_lab <- "Kurtosis"
  }
  else if (method == "div.ks" | method == "div.cvm" | method == "div.kl") {
    data1 <- data.frame(measvector = measvector,  lambdavector = lambdavector)
    y_lab <- "Divergence"
  }
  
  #plot <- ggplot(data1, aes(x = lambdavector,
  #                          y = measvector)) + geom_line() + 
  #  geom_vline(xintercept = vline, linetype = "dashed") + 
  #  geom_hline(yintercept = measoptim, color = "red", linetype = "dashed") + 
  #  xlab(expression(lambda)) + ylab(y_lab)
  
  dev.hold()
  plot(data1$lambdavector, data1$measvector, type = "l", lwd = 1.5,
      xlab = expression(lambda), ylab = y_lab) 
  abline(h = measoptim, lty = 2, col = "red")
  abline(v = lambdaoptim, lty = 2)
  dev.flush()


  out <- list(lambdavector = lambdavector, 
              measvector = measvector)
  return(out)
}



