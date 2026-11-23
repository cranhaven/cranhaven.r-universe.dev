#' @title Small Area Estimation using Hierarchical Bayesian under Zero Inflated Binomial Distribution
#'
#' @description This function is implemented to variable of interest \eqn{(y)} that assumed to be a Zero Inflated Binomial Distribution. The range of data is \eqn{(0 < y < \infty)}. This model can be used to handle overdispersion caused by excess zero in data.
#'
#' @param formula Formula that describe the fitted model
#' @param n.samp Number of sample in each area
#' @param iter.update Number of updates with default \code{3}
#' @param iter.mcmc Number of total iterations per chain with default \code{2000}
#' @param coef.nonzero Optional argument for mean on coefficient's prior distribution or \eqn{\beta}'s prior distribution which value is non-zero
#' @param var.coef.nonzero Optional argument for the variances of the prior distribution of the model coefficients (\eqn{\beta})
#' @param coef.zero Optional argument for mean on coefficient's prior distribution or \eqn{\alpha}'s prior distribution which value is non-zero
#' @param var.coef.zero Optional argument for the variances of the prior distribution of the model coefficients (\eqn{\alpha})
#' @param thin Thinning rate, must be a positive integer with default \code{1}
#' @param burn.in Number of iterations to discard at the beginning with default \code{1000}
#' @param tau.u.nZ Variance of random effect area for non-zero of variable interest \eqn{(y)} with default \code{1}
#' @param data The data frame
#'
#' @returns This function returns a list of the following objects:
#'  \item{Est}{A vector with the values of Small Area mean Estimates using Hierarchical bayesian method}
#'  \item{refVar}{Estimated random effect variances}
#'  \item{coefficient}{A dataframe with the estimated model coefficient}
#'  \item{plot_alpha}{Trace, Density, Autocorrelation Function Plot of MCMC samples}
#'  \item{plot_beta}{Trace, Density, Autocorrelation Function Plot of MCMC samples}
#'
#' @examples
#' #Compute Fitted Model
#'  y ~ X1 +X2
#'
#' # For data without any nonsampled area
#' # Load Dataset
#'   data(dataZIB)
#'   saeHB.ZIB <- ziBinomial(formula = y~X1+X2, "s", iter.update=3, iter.mcmc = 1000,
#'                 burn.in = 200,data = dataZIB)
#' #the setting of iter.update, iter.mcmc, and burn.in in this example
#' #is considered to make the example execution time be faster.

#' #Result
#' saeHB.ZIB$Est                                    #Small Area mean Estimates
#' saeHB.ZIB$Est$SD                                 #Standard deviation of Small Area Mean Estimates
#' saeHB.ZIB$refVar                                 #refVar
#' saeHB.ZIB$coefficient                            #coefficient
#' #Load Library 'coda' to execute the plot
#' #autocorr.plot(saeHB.ZIB$plot_alpha[[3]]) is used to   #ACF Plot for alpha
#' #autocorr.plot(saeHB.ZIB$plot_beta[[3]]) is used to    #ACF Plot for beta
#' #plot(saeHB.ZIB$plot_alpha[[3]]) is used to            #Dencity and trace plot for alpha
#' #plot(saeHB.ZIB$plot_beta[[3]]) is used to             #Dencity and trace plot for beta


## For data without any nonsampled area use dataBetaNs

#' @export

ziBinomial <- function(formula, n.samp, iter.update=3, iter.mcmc=10000,
                         coef.nonzero, var.coef.nonzero, coef.zero, var.coef.zero,
                         thin = 2, burn.in =2000, tau.u.nZ = 1, data) {

  result <- list(Est=NA, refVar=NA, coefficient=NA, plot=NA)

  formuladata <- stats::model.frame(formula, data,  na.action=NULL)

  if(any(is.na(formuladata[,-1])))
    stop("Auxilary Variables contains NA Values.")

  auxVar <- as.matrix(formuladata[,-1])
  nvar <- ncol(auxVar)+1
  formuladata <- data.frame(formuladata, n.samp=data[,n.samp])

  if (!missing(var.coef.nonzero)){

    if( length(var.coef.nonzero) != nvar ){
      stop("length of vector var.coef nonzero does not match the number of regression coefficients, the length must be ",nvar)
    }

    tau.b.value = 1/var.coef.nonzero
  } else {
    tau.b.value = 1/rep(1,nvar)
  }

  if (!missing(coef.nonzero)){
    if( length(coef.nonzero) != nvar ){
      stop("length of vector coef nonzero does not match the number of regression coefficients, the length must be ",nvar)
    }
    mu.b.value = coef.nonzero
  } else {
    mu.b.value = rep(0,nvar)
  }

  if (!missing(var.coef.zero)){

    if( length(var.coef.zero) != nvar ){
      stop("length of vector var.coef zero does not match the number of regression coefficients, the length must be ",nvar)
    }

    tau.a.value = 1/var.coef.zero
  } else {
    tau.a.value = 1/rep(1,nvar)
  }

  if (!missing(coef.zero)){
    if( length(coef.zero) != nvar ){
      stop("length of vector coef zero does not match the number of regression coefficients, the length must be ",nvar)
    }
    mu.a.value = coef.zero
  } else {
    mu.a.value = rep(0,nvar)
  }

  if (iter.update < 3){
    stop("the number of iteration updates at least 3 times")
  }


  #Fungsi Tersampel

  if (!any(is.na(formuladata[,1]))){
    formuladata <- as.matrix(stats::na.omit(formuladata))

    x <- stats::model.matrix(formula,data = as.data.frame(formuladata))
    n <- nrow(formuladata)


    mu.a=mu.a.value
    mu.b=mu.b.value
    tau.b=tau.b.value
    tau.a=tau.a.value
    tau.u.nZ.a=tau.u.nZ.b=1
    a.var.Z=a.var.nZ=1
    Iter=iter.update

    for(iter in 1:Iter){
      dat <- list("N" = n,"y" = formuladata[,1], "nvar"=nvar, "x"=as.matrix(x[,-1]), "n.samp"=formuladata[,nvar+1],
                  "mu.a"=mu.a, "tau.a"=tau.a, "mu.b"=mu.b, "tau.b"=tau.b,
                  "tau.u.nZ.a"=tau.u.nZ.a, "tau.u.nZ.b"=tau.u.nZ.b
      )  # names list of numbers
      inits <- list(u.nZ = rep(0,n),
                    a = mu.a, b = mu.b, tau.u.nZ = 1)

      #Zero Inflated Binomial

      cat("model{
        # Likelihood

        for(i in 1:N){
          y[i] ~ dbin(miu[i], n.samp[i])
          miu[i] <- (1-zero[i])*phi[i]

          logit(phi[i]) <- b[1]+sum(b[2:nvar]*x[i,])+u.nZ[i]
          u.nZ[i] ~ dnorm(0,tau.u.nZ)

          zero[i] ~ dbern(pi[i])
          logit(pi[i]) <- a[1]+sum(a[2:nvar]*x[i,])
          m[i] <- (1-pi[i])*phi[i]
        }


        #prior

        for (k in 1:nvar){
          a[k] ~ dnorm(mu.a[k],tau.a[k])
          b[k] ~ dnorm(mu.b[k],tau.b[k])
        }

      	tau.u.nZ ~ dgamma(tau.u.nZ.a,tau.u.nZ.b)

      	a.var.nZ <- 1 / tau.u.nZ

      }", file="ZIB.HB.txt")

      jags.m <- rjags::jags.model(file = "ZIB.HB.txt", data=dat, inits=inits, n.chains=1, n.adapt=500)
      file.remove("ZIB.HB.txt")
      params <- c("m","a.var.nZ","a","b","tau.u.nZ")
      samps1 <- rjags::coda.samples(jags.m, params, n.iter=iter.mcmc, thin=thin)
      samps11 <- stats::window(samps1, start=burn.in+1, end=iter.mcmc)
      result_samps=summary(samps11)

      a.var.nZ=result_samps$statistics[nvar+1]

      alpha=result_samps$statistics[1:nvar,1:2]
      for (i in 1:nvar){
        mu.a[i]  = alpha[i,1]
        tau.a[i] = 1/(alpha[i,2]^2)
      }

      beta=result_samps$statistics[(nvar+2):(2*nvar+1),1:2]
      for (i in 1:nvar){
        mu.b[i]  = beta[i,1]
        tau.b[i] = 1/(beta[i,2]^2)
      }

      tau.u.nZ.a= result_samps$statistics[(2*nvar+n+2),1]^2/result_samps$statistics[(2*nvar+n+2),2]^2
      tau.u.nZ.b = result_samps$statistics[(2*nvar+n+2),1]/result_samps$statistics[(2*nvar+n+2),2]^2
    }

    result_samps = summary(samps11)

    a.varnames <- list()
    for (i in 1:(nvar)) {
      idx.a.varnames <- as.character(i-1)
      a.varnames[i] <- stringr::str_replace_all(paste("a[",idx.a.varnames,"]"),pattern=" ", replacement="")
    }

    result_mcmc.alpha <- samps11[,c(1:(nvar))]
    colnames(result_mcmc.alpha[[1]]) <- a.varnames

    b.varnames <- list()
    for (i in 1:(nvar)) {
      idx.b.varnames <- as.character(i-1)
      b.varnames[i] <- stringr::str_replace_all(paste("b[",idx.b.varnames,"]"),pattern=" ", replacement="")
    }

    result_mcmc.beta <- samps11[,c((2+nvar):(1+2*nvar))]
    colnames(result_mcmc.beta[[1]]) <- b.varnames

    a.var.nZ=result_samps$statistics[nvar+1,1:2]
    a.var = a.var.nZ

    alpha=result_samps$statistics[1:(nvar),1:2]
    rownames(alpha) <- a.varnames

    beta=result_samps$statistics[(2+nvar):(1+2*nvar),1:2]
    rownames(beta) <- b.varnames

    koef1 = as.data.frame(rbind(alpha, beta))

    mu=result_samps$statistics[(2+2*nvar):(1+2*nvar+n),1:2]

    Estimation=data.frame(mu)

    Quantiles <- as.data.frame(result_samps$quantiles)
    q_mu <- Quantiles[(2+2*nvar):(1+2*nvar+n),]

    q_alpha <- (Quantiles[1:(nvar),])
    rownames(q_alpha) <- a.varnames
    alpha <- cbind(alpha,q_alpha)

    q_beta <- (Quantiles[(2+nvar):(1+2*nvar),])
    rownames(q_beta) <- b.varnames
    beta <- cbind(beta,q_beta)

    koef2 = as.data.frame(rbind(q_alpha,q_beta))

    Estimation <- data.frame(Estimation,q_mu)
    koef = cbind(koef1, koef2)
    colnames(Estimation) <- c("MEAN","SD","2.5%","25%","50%","75%","97.5%")
    colnames(koef) <- c("MEAN","SD","2.5%","25%","50%","75%","97.5%")

  } else {

    #NON SAMPLED
    formuladata <- as.data.frame(formuladata)

    x <- as.matrix(formuladata[,2:nvar])
    n <- nrow(formuladata)

    mu.a=mu.a.value
    mu.b=mu.b.value
    tau.b=tau.b.value
    tau.a=tau.a.value
    tau.u.nZ.a=tau.u.nZ.b=1
    a.var.nZ=1
    Iter=iter.update

    formuladata$idx <- rep(1:n)
    data_sampled <- stats::na.omit(formuladata)
    data_nonsampled <- formuladata[-data_sampled$idx,]

    r=data_nonsampled$idx
    n1=nrow(data_sampled)
    n2=nrow(data_nonsampled)


    for(iter in 1:Iter){
      dat <- list("n1" = n1, "n2"=n2, "nvar"=nvar,"y_sampled" = data_sampled[,1],
                  "x_sampled"=as.matrix(data_sampled[,2:nvar]),
                  "x_nonsampled"=as.matrix(data_nonsampled[,2:nvar]),
                  "n.samp"=data_sampled[,(nvar+1)],
                  "mu.a"=mu.a, "tau.a"=tau.a, "mu.b"=mu.b, "tau.b"=tau.b,
                  "tau.u.nZ.a"=tau.u.nZ.a,"tau.u.nZ.b"=tau.u.nZ.b)  # names list of numbers
      inits <- list(u.nZ = rep(0,n1),  u.nZT = rep(0,n2),
                    a = mu.a, b = mu.b, tau.u.nZ = 1)

      cat("model{
    # Likelihood
    for(i in 1:n1){
      y_sampled[i] ~ dbin(miu[i],n.samp[i])
      miu[i] <- (1-zero[i])*phi[i]

      logit(phi[i]) <- b[1]+sum(b[2:nvar]*x_sampled[i,])+u.nZ[i]
      u.nZ[i] ~ dnorm(0,tau.u.nZ)

      zero[i] ~ dbern(pi[i])
      logit(pi[i]) <- a[1]+sum(a[2:nvar]*x_sampled[i,])

      m[i] <- (1-pi[i])*phi[i]
    }

    for(j in 1:n2){
      logit(phiT[j]) <- mu.b[1]+sum(mu.b[2:nvar]*x_nonsampled[j,])+u.nZT[j]
      u.nZT[j] ~ dnorm(0,tau.u.nZ)

      logit(piT[j]) <- mu.a[1]+sum(mu.a[2:nvar]*x_nonsampled[j,])

      miu_nonsampled[j] <- (1-piT[j])*phiT[j]
    }

    #prior

    for (k in 1:nvar){
      a[k] ~ dnorm(mu.a[k],tau.a[k])
      b[k] ~ dnorm(mu.b[k],tau.b[k])
    }

  	tau.u.nZ ~ dgamma(tau.u.nZ.a,tau.u.nZ.b)

  	a.var.nZ <- 1 / tau.u.nZ

  }", file="ZIB.HB.ns.txt")

      jags.m <- rjags::jags.model( file = "ZIB.HB.ns.txt", data=dat, inits=inits, n.chains=1, n.adapt=500 )
      file.remove("ZIB.HB.ns.txt")
      params <- c("m","miu_nonsampled","a.var.nZ","a","b","tau.u.nZ")
      samps1 <- rjags::coda.samples(jags.m, params, n.iter=iter.mcmc, thin=thin)
      samps11 <- stats::window(samps1, start=burn.in+1, end=iter.mcmc)
      result_samps=summary(samps11)

      a.var.nZ=result_samps$statistics[nvar+1]

      alpha=result_samps$statistics[1:nvar,1:2]
      for (i in 1:nvar){
        mu.a[i]  = alpha[i,1]
        tau.a[i] = 1/(alpha[i,2]^2)
      }

      beta=result_samps$statistics[(nvar+2):(2*nvar+2),1:2]
      for (i in 1:nvar){
        mu.b[i]  = beta[i,1]
        tau.b[i] = 1/(beta[i,2]^2)
      }

      tau.u.nZ.a= result_samps$statistics[(2*nvar+n+2),1]^2/result_samps$statistics[(2*nvar+n+2),2]^2
      tau.u.nZ.b = result_samps$statistics[(2*nvar+n+2),1]/result_samps$statistics[(2*nvar+n+2),2]^2
    }

    result_samps=summary(samps11)

    a.varnames <- list()
    for (i in 1:(nvar)) {
      idx.a.varnames <- as.character(i-1)
      a.varnames[i] <- stringr::str_replace_all(paste("a[",idx.a.varnames,"]"),pattern=" ", replacement="")
    }

    result_mcmc.alpha <- samps11[,c(1:(nvar))]
    colnames(result_mcmc.alpha[[1]]) <- a.varnames

    b.varnames <- list()
    for (i in 1:(nvar)) {
      idx.b.varnames <- as.character(i-1)
      b.varnames[i] <- stringr::str_replace_all(paste("b[",idx.b.varnames,"]"),pattern=" ", replacement="")
    }

    result_mcmc.beta <- samps11[,c((2+nvar):(1+2*nvar))]
    colnames(result_mcmc.beta[[1]]) <- b.varnames

    a.var.nZ  = result_samps$statistics[nvar+1,1:2]
    a.var    = a.var.nZ

    alpha=result_samps$statistics[1:(nvar),1:2]
    rownames(alpha) <- a.varnames

    beta=result_samps$statistics[(2+nvar):(1+2*nvar),1:2]
    rownames(beta) <- b.varnames

    koef1 = as.data.frame(rbind(alpha, beta))

    mu=result_samps$statistics[(2+2*nvar):(1+2*nvar+n1),1:2]
    mu_nonsampled=result_samps$statistics[(2+2*nvar+n1):(1+2*nvar+n),1:2]

    Estimation=matrix(rep(0,n),n,2)
    Estimation[r,]=mu_nonsampled
    Estimation[-r,]=mu
    Estimation=as.data.frame(Estimation)
    colnames(Estimation)=c("mean", "sd")

    Quantiles <- as.data.frame(result_samps$quantiles)
    q_mu <- Quantiles[(2+2*nvar):(1+2*nvar+n1),]
    q_mu_nonsampled <- Quantiles[(2+2*nvar+n1):(1+2*nvar+n),]

    q_Estimation <- matrix(0,n,5)
    for(i in 1:5){
      q_Estimation[r,i] <- q_mu_nonsampled[,i]
      q_Estimation[-r,i] <- q_mu[,i]
    }

    q_alpha <- (Quantiles[1:(nvar),])
    rownames(q_alpha) <- a.varnames
    alpha <- cbind(alpha,q_alpha)

    q_beta <- (Quantiles[(2+nvar):(1+2*nvar),])
    rownames(q_beta) <- b.varnames
    beta <- cbind(beta,q_beta)

    koef2= as.data.frame(rbind(q_alpha, q_beta))

    Estimation <- data.frame(Estimation,q_Estimation)
    koef =  cbind(koef1, koef2)
    colnames(Estimation) <- c("MEAN","SD","2.5%","25%","50%","75%","97.5%")
    colnames(koef) <- c("MEAN","SD","2.5%","25%","50%","75%","97.5%")

  }

  result$Est         = Estimation
  result$refVar      = a.var
  result$coefficient = koef
  result$plot_alpha   = list(grDevices::graphics.off() ,graphics::par(mar=c(2,2,2,2)),
                             coda::autocorr.plot(result_mcmc.alpha,col="brown2",lwd=2),
                             plot(result_mcmc.alpha,col="brown2",lwd=2))
  result$plot_beta   = list(grDevices::graphics.off() ,graphics::par(mar=c(2,2,2,2)),
                            coda::autocorr.plot(result_mcmc.beta,col="brown2",lwd=2),
                            plot(result_mcmc.beta,col="brown2",lwd=2))

  return(result)

}


