geovol <- function (e, initial.values = list()) 
{
    dates <- index(e)
    e <- as.matrix(e)
    m <- ncol(e)
    n <- nrow(e)
    if (is.null(colnames(e))) {
      colnames(e) <- paste("e", paste(1:m, sep = ""), sep = "")
    }
    names <- colnames(e)
    e2 <- e^2
    if (is.null(initial.values$s)) {
      for (t in 1:nrow(e)) {
        e2[t,which(is.na(e2[t, ]))] <- mean(e2[t,!is.na(e2[t, ])])
      }
      pca0 <- princomp(e2, cor = TRUE, fix_sign = TRUE)
      s0 <- pca0$loadings[1,]
      s0 <- s0/sqrt(sum(s0^2))
      names(s0) <- names
    }
    else s0 <- initial.values$s
    if (is.null(initial.values$x)) x0 <- abs(pca0$scores[,1])
    else x0 <- initial.values$x
    xhat <- x0
    shat <- s0
    iter <- 1
    repeat{
      '
        Estimate x at each point in time t
      '
      xhat.it <- numeric(n)
      for (t in 1:n) {
        if (iter == 1) {
          x_t <- x0[t]
          s_t <- s0[!is.na(e[t,])]
        }
        else {
          x_t <- xhat[t]
          s_t <- shat[!is.na(e[t,])]
        }
        fit <- optim(par = x_t, fn = geovolObj,
                     par.fixed = s_t,
                     y = c(e[t,!is.na(e[t,])]),
                     x = TRUE, flag = 1, lower = 5e-5,
                     method = "L-BFGS-B")
        xhat.it[t] <- fit$par
      }
      xhat.it <- xhat.it/mean(xhat.it) 
      conv.x <- max(abs(xhat-xhat.it)) 
      xhat <- xhat.it
      '
        Estimate s for each asset i
      '
      shat.it <- numeric(m)
      for(i in 1:m){
        if (iter == 1) s_i <- s0[i] 
        else s_i <- shat[i]
        fit <- optim(par = s0[i], fn = geovolObj, 
                     par.fixed = xhat[!is.na(e[,i])], 
                     y = na.omit(e[,i]), x = FALSE,
                     flag = 1, lower = 0, upper = 1,
                     method = "L-BFGS-B")
        shat.it[i] <- fit$par
      }
      shat.it <- shat.it/sqrt(sum(shat.it^2)) 
      conv.s <- max(abs(shat-shat.it)) 
      shat <- shat.it
      if( max(c(conv.x,conv.s)) < 0.001 || iter == 100 ) break 
      else iter = iter + 1
    }    
    geovol2 <- matrix(NA, n, m)
    for (i in 1:m) {
      geovol2[!is.na(e[,i]),i] <- geovolObj(par = shat[i], par.fixed = xhat[!is.na(e[,i])], 
                               y = na.omit(e[,i]), x = FALSE, flag = 2)
    }
    colnames(geovol2) <- paste(names)
    rownames(geovol2) <- paste(dates)
    residuals <- geovol2
    residuals <- e/sqrt(geovol2)
    shat <- as.matrix(shat)
    rownames(shat) <- paste(names)
    colnames(shat) <- "loading"
    shat <- shat[order(shat, decreasing = TRUE),1]
    xhat <- as.matrix(xhat)
    rownames(xhat) <- paste(dates)
    colnames(xhat) <- "factor"
    results <- list(geovol2 = geovol2, x = xhat, s = shat, e = e, 
                    residuals = residuals, names = names, 
                    dates = dates, n = n , m = m, iter = iter, 
                    date = date())
    class(results) <- "geovol"
    return(results)
}

