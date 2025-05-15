
robustsae <- function(formula, S2, ni, nsim = 1000, burnin = 500, data, truemean){
  result <- list(mean = NA, variance = NA, criteria=list())

  # c(theta, beta, nu, lambda, sigma, V), est=c(theta=NA, mean=NA)
  # Specify the model
  if (missing(data))
  {
    newdata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  } else
  {
    newdata <- model.frame(formula, na.action = na.omit, data)
    X <- model.matrix(formula, data)
  }
  y <- newdata[, 1]


  # Specify the attributes of the model
  if (attr(attributes(newdata)$terms, "response") == 1){
    attrformula <- paste(formula[2], formula[1], formula[3])
  } else {
    attrformula <- paste(formula[1], formula[2])
  }

  # Remove NA values
  if (length(na.action(newdata)) > 0)
    stop("Argument formula=", attrformula, " contains NA values.")

  m <- length(y) # No. of observation or areas
  p <- ncol(X) # No. of auxiliary covariates w/ intercept

  # Stack the sample values
  accept.sim <- numeric(nsim)  #to see acceptace rate
  nu.sim <- rep(0, nsim)
  lambda.sim <- array(0, dim = c(m, m, nsim))
  sigma2.sim <- array(0, nsim)
  beta.n.sim <- array(0, dim = c(nsim ,p))
  theta.sim.t <- array(0, dim = c(nsim ,m))
  meantheta.sim <- array(0, dim = c(nsim ,m))
  vartheta.sim <- array(0, dim = c(nsim ,m))
  V.sim <- array(0, dim = c(m, m, nsim))

  # Initial values
  nu.sim [1] <- 3
  lambda.sim[,, 1] <- diag(rgamma(m, nu.sim[1] / 2, nu.sim[1] / 2))
  sigma2.sim[1] <- 1
  beta.n.sim[1, ] <- runif(p, -1, 1)
  theta.sim.t[1,] <- rmvnorm(1, X %*% beta.n.sim[1, ], sigma = sigma2.sim[1] * solve(lambda.sim[,, 1]))
  V.sim[,, 1] <- runif(m, 0, 10)
  a <- 1  # for the modified Jeffreys' prior on sigma^2

  for(k in 2:nsim){

    #sample of nu
    prop.sim <- rgamma(1, (m + 1) / 2,
                       (sum(diag(lambda.sim[,, k-1])) - sum(log(diag(lambda.sim[,, k-1]))) - m) / 2)
    psiprop.sim <- {(prop.sim + 1)^{p / 2 - 1}}/{{(prop.sim + 3)^{p / 2 + 1 / 2}}}
    psicurr.sim <- {(nu.sim[k-1] + 1)^{p / 2 - 1}}/{{(nu.sim[k-1] + 3)^{p / 2 + 1 / 2}}}

    alpha.sim <- min(1, psiprop.sim / psicurr.sim)

    if (accept.sim[k-1] <- (runif(1) < alpha.sim)){
      xaccept.sim <- prop.sim
    } else{
      xaccept.sim <- nu.sim[k-1]
    }

    nu.sim[k] <- xaccept.sim

    #samples of etas/lambda
    for(i in 1:m){
      xib <- (theta.sim.t[k-1, i] - X[i, ] %*% beta.n.sim[k-1, ]) 
      lambda.sim[i, i, k] <- rgamma(1, (nu.sim[k] + 1 ) / 2, nu.sim[k] / 2 + xib * xib / (2 * sigma2.sim[k-1]))
    }

    #samples of V
    for(i in 1:m)
    {
      V.sim[i, i, k] <- rigamma(1, ni[i] / 2,
                                ((y[i] - theta.sim.t[k-1, i]) ^ 2) / 2 + (ni[i] - 1) * S2[i] / 2)
    }

    #sample of sigma_delta
    newval <- theta.sim.t[k-1, ] - X %*% beta.n.sim[k-1, ]
    newlambda.sim <- t(newval) %*% lambda.sim[,, k] %*% newval
    sigma2.sim[k] <-rigamma(1, (p + m) / 2, (a + newlambda.sim) / 2)

    #samples of beta
    Omega.sim <- solve(t(X) %*% lambda.sim[,, k] %*% X)
    Omega.sim <- (Omega.sim + t(Omega.sim)) / 2
    betahat.sim <- Omega.sim %*% t(X) %*% lambda.sim[,, k] %*% theta.sim.t[k-1, ]
    beta.n.sim[k,] <- rmvnorm(1, betahat.sim, sigma2.sim[k] * Omega.sim)

    #samples of Theta
    for(i in 1:m){
      meantheta.sim[k, i] <- (sigma2.sim[k] * y[i] + V.sim[i, i, k] * lambda.sim[i, i, k] * X[i, ] %*%
                                beta.n.sim[k, ]) / (sigma2.sim[k] + V.sim[i, i, k] * lambda.sim[i, i, k])
      vartheta.sim[k, i] <- (sigma2.sim[k] * V.sim[i, i, k] ) / (sigma2.sim[k] + V.sim[i, i, k] * lambda.sim[i, i, k])
      theta.sim.t[k, i] <- rnorm(1, mean = meantheta.sim[k, i], sd = sqrt(vartheta.sim[k, i]))
    }

  }

  # Posterior mean
  theta.mean.t <- meantheta.sim[(burnin+1):nsim, ]
  bmean <- apply(theta.mean.t, 2, sum) / (nsim - burnin)
  bmean2 <- apply((theta.mean.t)^2, 2, sum) / (nsim - burnin)

  # Posterior variance
  bvar <- vartheta.sim[(burnin+1):nsim, ]
  bvar2 <- apply(bvar, 2, sum) / (nsim-burnin)

  bvariance <- bvar2 + bmean2 - (bmean) * (bmean)
  
  if(!missing(truemean)){
  truemean <- as.matrix(truemean)
  #cat("\n ASD OUR \n")
  OUR <- mean((bmean - truemean) * (bmean - truemean))

  #cat("\n AAB OUR \n")
  OUR2 <- mean(abs(bmean- truemean)) #AAB

  #cat("\n ASRB OUR \n")
  OUR3 <- mean((bmean - truemean) * (bmean - truemean) / (truemean * truemean)) #ASRB

  #cat("\n ARB OUR \n")
  OUR4 <- mean(abs((bmean - truemean) / truemean)) #ARB
  } else {
    OUR <- OUR2 <-  OUR3 <- OUR4 <- NA
  }

  result$mean <- bmean
  result$variance <- bvariance
  result$Criteria$ASD <- OUR
  result$Criteria$AAB <- OUR2
  result$Criteria$ASRB <- OUR3
  result$Criteria$ARB <- OUR4
  result
 }



