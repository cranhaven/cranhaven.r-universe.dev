#' Sequentially monitor patients using Beta-Binomial posterior probability
#'
#' Make animation plots to present sequential monitor the patients using Beta-Binomial Bayesian model
#'
#'
#' @usage
#' BB.aniplot(a, b, r, N=1, alpha=0.05, seed=1234, time.interval=1, output=TRUE)
#' @param r vector of number of response in each cohort, the value of each element should not exceed N
#' @param N the number of patients treated by the experimental drug at a certain stage of the trial.
#' @param a the hyperparameter (shape1) of the Beta prior for the experimental drug.
#' @param b the hyperparameter (shape2) of the Beta prior for the experimental drug.
#' @param alpha the siginificant level to determine the credible interval, set 0.05 by default.
#' @param seed a single integer value, random number generator (RNG) state for random number generation.
#' @param time.interval a positive number to set the time interval of the animation (unit in seconds); default to be 1.
#' @param output a logical value, whether to output the inference results of posterior distribution and mean, observed data and credible interval.
#' @return
#' animation plot of updating posterior as prior, and output the inference information of prior and posterior distribution if \code{output=TRUE}.
#' @references
#' Yin, G. (2012).
#' \emph{Clinical Trial Design: Bayesian and Frequentist Adaptive Methods.}
#' New York: Wiley.
#' @examples
#' # Using APL data
#' r=rep(0,6)
#' BB.aniplot(a=1,b=1,r=r, alpha=0.05, seed=1234)
#' # Simulate binomial data
#' B <- 10; N=1; p=0.3
#' r <- rbinom(n = B,size = N,prob = p)
#' BB.aniplot(a=1,b=1,r=r,time.interval = 0.2,output = FALSE)
#' @import animation
#' @importFrom stats dbeta
#' @export

BB.aniplot <- function(a, b, r, N=1, alpha=0.05, seed=1234, time.interval=1, output=TRUE){
  # a,b  shape parameters of beta prior
  # r is the vector of response, length should be M, each element value should not exceed N
  set.seed(seed)
  options(digits = 3)
  M <- length(r)
  animation::saveGIF({
    if (output==TRUE){  cat("Prior: Beta(",a ,",", b, ") \n\n", sep = "")}
  for ( i in 1:M){
    x <- seq(0,1,0.01)
    ## calculate the prior and posterior density
    y.prior <- dbeta(x,a,b)
    mu0 <- a/(a+b)
    a1 <- a+r[i]; b1 <- b+N-r[i]
    mu1 <- a1/(a1+b1)
    y.post <- dbeta(x,a1, b1)

    lh <-  stats::dbinom(r[i], N, x) * (N+1)

    if (output==TRUE){
    # Calculate the 95% credible interval of posterior estimation
    LB <- stats::qbeta(alpha/2,a1,b1); UB <- stats::qbeta(alpha/2,a1,b1,lower.tail = F)
    # Updating prior parameters
    cat("======== Cohort Number: ",i, " ======== \n", sep = "")
    cat("Observations -- Sample Size: ", i*N, "(",N,")", "  ||  Number of Response: ", sum(r[1:i]), "(",r[i],")", "  ||  Number of Failure: ", i*N-sum(r[1:i]), "(",N-r[i],")","\n", sep = "")
    cat("  Observed Response Rate: ", sum(r[1:i])/(i*N), "\n", sep = "")
    cat("Posterior: Beta(",a1 ,",", b1, ") \n", sep = "")
    cat("  Posterior Mean: ", mu1, "\n", sep = "")
    cat("  ",(1-alpha)*100,"% Credible Interval: (", LB, ",", UB,") \n\n", sep="")
}

    ylim <- range(0, min(20,max(y.prior)), min(20,max(y.post)), max(lh))
    graphics::plot(x, y.prior, type="l",  col="blue",xlim=c(0,1), ylim = ylim, xlab="Response Rate", ylab="Density" , main=paste(N*i,"patients are included",sep =" "))
    ## add a vertical line for the mean prior response probability
    #   abline(v=mu0,lty=1,col="blue")
    ## add comment text
    #   mtext( bquote(E(pi[prior]) == .(mu0)), side = 1, line = 0.5,at = mu0,col="blue")


    ## plot the prior density

    ## plot the posterior density
    graphics::lines(x,y.post, col = "red")
    ## add a vertical line for the estimation of the mean posterior response probability
    #   abline(v=mu1,lty=2, col = "red" )
    ## add comment text
    #      mtext( bquote(E(pi[post]) == .(mu1)), side = 3, line = 0,at =mu1,col="red")
    #      abline(v=stats::qbeta(alpha/2,a1,b1),lty=2, col='dark green')
    #      abline(v=stats::qbeta(alpha/2,a1,b1,lower.tail = F),lty=2, col='dark green')

    graphics::lines(x,lh, col="dark green")
    #      legend("topright",c("Prior", "Posterior","Cred.Int","Likelihood"), lty=c(1, 2, 2,1), col=c("blue","red", "dark green","dark green"))
    graphics::legend("topright",c("Prior", "Posterior","Likelihood"), lty=1, col=c("blue","red", "dark green"))


    a <- a1; b <- b1
    }
   }, interval = time.interval)
}



#' Bayesian design method for sequentially monitoring patients using Beta-Binomial posterior probability based on observing data
#'
#' Make animation plots to present sequential monitor stopping rule using Beta-Binomial Bayesian model
#'
#' @usage
#' bayes.design(a,b,r=0, stop.rule="futility", add.size=5, alpha=0.05,
#' p0 ,delta=0.2,tau1=0.9,tau2=0.9,tau3=0.9,tau4=0.9, time.interval =1)
#' @param r the maximum number of patients treated by the experimental drug.
#' @param stop.rule the hyperparameter (shape1) of the Beta prior for the experimental drug.
#' @param a the hyperparameter (shape1) of the Beta prior for the experimental drug.
#' @param b the hyperparameter (shape2) of the Beta prior for the experimental drug.
#' @param alpha the siginificant level to determine the credible interval, set 0.05 by default.
#' @param add.size a single integer value, random number generator (RNG) state for random number generation.
#' @param p0 the prespecified reseponse rate.
#' @param delta the minimally acceptable increment of the response rate.
#' @param tau1 threshold for stopping rule 1.
#' @param tau2 threshold for stopping rule 2.
#' @param tau3 threshold for stopping rule 3.
#' @param tau4 threshold for stopping rule 4.
#' @param time.interval a positive number to set the time interval of the animation (unit in seconds); default to be 1.
#' @return
#' animation plot of determination of stopping boundaries.
#' @references
#' Yin, G. (2012).
#' \emph{Clinical Trial Design: Bayesian and Frequentist Adaptive Methods.}
#' New York: Wiley.
#' @examples
#' # Using Multiple Myeloma (MM) data example
#' MM.r = rep(0,6); MM.mean = 0.1; MM.var = 0.0225
#' a <- MM.mean^2*(1-MM.mean)/MM.var - MM.mean; b <- MM.mean*(1-MM.mean)^2/MM.var - (1-MM.mean)
#' bayes.design(a=a,b=b,r=MM.r,stop.rule="futility",p0=0.1)
#'
#' # Using Acute Promyelocytic Leukaemia (APL) data example
#' APL.r <- c(0,1,0,0,1,1); APL.mean = 0.3; APL.var = 0.0191
#' a <- APL.mean^2*(1-APL.mean)/APL.var - APL.mean; b <- APL.mean*(1-APL.mean)^2/APL.var - (1-APL.mean)
#' bayes.design(a=a,b=b,r=APL.r,stop.rule="efficacy",p0=0.1)
#' @import animation
#' @export
#'
## create a function using parameters: prior mean (m) and varaince (v)
## n is the number of patients sequentially enrolled in trials; s is the number of response after inclusion of each new patient

bayes.design <- function(a,b,r=0, stop.rule="futility", add.size=5, alpha=0.05,
                         p0=NULL,delta=0.2,tau1=0.9,tau2=0.9,tau3=0.9,tau4=0.9, time.interval =1){
  # n: sample size of a cohort;  r: observed response from n patient
  ############################
  ## Part 1: Basic settings ##
  ############################
  r <- cumsum(r); n <- 1:length(r)
  postm <- (a+r)/(a+b+n)
  result <- list("para.a"=a, "para.b"=b, "poterior mean"= postm)
  print(result)



  ################################################################
  ## Part 2: Stopping rule based on pre-specified threshold  #####
  ################################################################


  # Rmax <- mu0 #APL data
  animation::saveGIF({
  if (stop.rule=="efficacy"){
    ### rule 1 for efficacy
    n1 <- min(which(pbeta(p0+delta,a+r, b+n-r, lower.tail = F) > tau1))
    print(paste("Stop the trial for efficacy after the inclusion of",n1, "patients."))

    ## could add a gradually animation

    for (i in seq_along(n)){
    graphics::plot(n[1:i],pbeta(p0+delta,a+r[1:i], b+n[1:i]-r[1:i], lower.tail=F),xlim=c(1,length(r)),ylim=c(0,1), type="o", xlab="Number of patients", ylab="Posterior Probability")
    graphics::abline(h=tau1, lty=2)
    graphics::legend("bottomright",c("Posterior tial prob","Stopping threshold"), lty=c(1,2),cex = 0.8)
    }
    graphics::abline(v=n1, lwd=5 , col="red")
   ## rule 3 for efficacy

    cdf <- VGAM::pbetabinom.ab(q = c(0:add.size),size = add.size, shape1 = a+max(r), shape2 = b+max(n)-max(r) )
    graphics::plot(c(0:add.size),cdf,type="S")
    ## rule 5
  }

  else if (stop.rule=="futility"){
    ## rule 2 for futility
    # Rmax = mu0  MM data
    n2 <- min(which(pbeta(p0,a+r, b+n-r) > tau2))
    print(paste("Stop the trial for futility after the inclusion of",n2, "patients."))


    for (i in seq_along(n)){
    graphics::plot(n[1:i],pbeta(p0,a+r[1:i], b+n[1:i]-r[1:i]), xlim=c(1,length(r)),ylim=c(0,1),type="o", xlab="Number of patients", ylab="Posterior Probability")
    graphics::abline(h=tau2, lty=2)
    graphics::legend("bottomright",c("Posterior tial probability","Stopping threshold"), lty=c(1,2),cex=0.8)
    }
    graphics::abline(v=n2, lwd=5 , col="red")
    ## rule 4 for futility
    cdf <- VGAM::pbetabinom.ab(q = c(0:add.size),size = add.size, shape1 = a+max(r), shape2 = b+max(n)-max(r) ) ## rule 3
    graphics::plot(c(0:add.size),cdf,type="S")
  }
  else{
    print("Warning: please assign a stopping rule!")
  }
  }, interval=time.interval)
}
