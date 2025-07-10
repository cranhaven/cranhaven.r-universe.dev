#' Network Meta-Analysis Using the hierarchical model
#' @description \code{nmadt.hsroc.MNAR} performs network meta-analysis of diagnostic tests using the HSROC (hierarchical summary receiver operating characteristic) model \insertCite{lian2018bayesian}{NMADiagT} based on the MNAR assumption.
#' @param nstu an integer indicating the number of studies included in the dataset.
#' @param K an integer indicating the number of candiate test in the dataset.
#' @param data a list conating the input dataset to be used for meta-analysis.
#' @param testname a string vector of the names of the candidate tests in the dataset in the same order as presetned in the dataset.
#' @param directory a string specifying the designated directory to save trace plots or potential scale reduction factors calculated in the function. The default is NULL.
#' @param eta a number indicating the mean of log(S) and log(P) which determines the covariance matrices of the cutoff values and accuracy values respectively. The default is 0.
#' @param xi_preci a number indicating the precision of log(S) and log(P) which determines the covariance matrices of the cutoff values and accuracy values respectively. The default is 1.25.
#' @param digits a positive integer he number of digits to the right of the decimal point to keep for the results; digits=4 by default.
#' @param gamma0 a vector indicating coefficients of study-specific specificity in the MNAR model.
#' @param gamma1 a vector indicating coefficients of study-specific sensitivity in the MNAR model.
#' @param mu_gamma a number specifying mean of intercept in the MNAR model. The default is 0.
#' @param preci_gamma a number specifying precision of intercept in the MNAR model. The default is 1.
#' @param n.adapt a positive integer indicating the number of iterations for adaptation. The default is 5,000.
#' @param n.iter a postive integer indicating the number of iterations in each MCMC chain. The default is 50,000.
#' @param n.burnin a positive integer indicating the number of burn-in iterations at the beginning of each chain without saving any of the posterior samples. The default is \code{floor(n.iter/2)}.
#' @param n.chains a postive interger indicating the number of MCMC chains. The default is 3.
#' @param n.thin the thinning rate for MCMC chains, which is used to save memory and computation time when \code{n.iter} is large.
#' For example, the algorithm saves only one sample in every nth iteration, where n is given by \code{n.thin}.
#' @param conv.diag a logical value specifying whether to compute potential scale reduction factors proposed for convergence diagnostics. The default is \code{FALSE}.
#' @param trace a string vector containing a subset of different quantities which can be chosen from prevalence(\code{"prev"}), sensitivity (\code{"Se"}), specificity (\code{"Sp"}), positive and negative predictive values (\code{"ppv"} and \code{"npv"} repectively), positive likelihood (\code{"LRpos"}), and negative likelihood (\code{"LRneg"}).
#' @param dic a logical value indicating whether the function will output the deviance information criterion (DIC) statistic. The default is \code{false}.
#' @param mcmc.samples a logical value indicating whether the coda samples generated in the meta-analysis. The default is \code{FALSE}.
#' @return A list with the raw output for graphing the results, the effect size estimates, which lists the posterior mean, standard deviation, median, and a $95$\% equal tail credible interval for the median.
#' @examples
#' \donttest{
#' kangdata<-read.csv(file=system.file("extdata","kangdata.csv",package="NMADiagT"),
#' header=TRUE, sep=",")
#' set.seed(9)
#' kangMNAR.out.hsroc <- nmadt.hsroc.MNAR(nstu=12, K=2, data=kangdata,
#' testname=c("D-dimer","Ultrasonography"),gamma1=c(-0.5,-0.5), gamma0=c(-0.5,-0.5))
#' }
#' @import rjags
#' @import coda
#' @import MASS
#' @import MCMCpack
#' @import utils
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{lian2018bayesian}{NMADiagT}
#' @export
nmadt.hsroc.MNAR=function(nstu, K, data, testname, directory = NULL, eta=0, xi_preci=1.25,digits = 4,
                          gamma1, gamma0, mu_gamma=0, preci_gamma=1,
                          n.adapt = 10000, n.iter = 50000, n.chains = 3,
                          n.burnin = floor(n.iter/2), n.thin = max(1, floor((n.iter - n.burnin)/1e+05)),
                          conv.diag = FALSE, trace = NULL, dic = FALSE, mcmc.samples = FALSE )
{
  #options(warn = 1)
  if (missing(data))
    stop("need to input dataset.")
  if (is.null(directory)& (!is.null(trace)|conv.diag!="FALSE"))
    stop("need to input directory for trace plots/conv.diag.")
  if (ncol(data)!=2*K+4|typeof(data)!="list")
    stop("dataset in incorrect format.")
  if (missing(nstu))
    stop("need to number of studies.")
  if (missing(K))
    stop("need to specify number of diagnostic tests.")
  if (K<1|K>5)
    stop("number of diagnostic tests out of bound (should be between 1-5).")

  R1 = matrix(0,nrow=K+1,ncol=K+1)                    # Parameter of Inverse wishart distribution
  R2 = matrix(0,nrow=K,ncol=K)
  diag(R1) = 1
  diag(R2) = 1
  R1 = solve(R1)
  R2 = solve(R2)
  N <- nrow(data)
  delta<-data[c(2:(K+2))]
  param = c("Se", "Sp","prev","ppv","npv","LRpos","LRneg")


  if(!isTRUE(all(delta == floor(delta)))|any(delta<0)|any(delta>1)|any(is.na(delta))){
    stop("missing indicator not in the right format(0 for missing data and 1 for nonmissing data)" )
  }
  y<-data[c((K+3):(2*K+3))]
  sid<-data[,1]
  n<-data[,ncol(data)]

  if(any(is.na(delta))){
    stop("missing test results should be denoted as 999 instead of NA)" )
  }
  if(!all(y) %in% c(0,1,999))
    stop("results not in the right format")
  n<-data[,ncol(data)]
  if(any(n<0)|!isTRUE(all(y == floor(y))))
    stop("counts should be positive integers")
  if(length(testname)!=K)
    stop("number of test names does not match number of tests")
  if (missing(testname)) {
    for(i in 1:K){
      testname[i]<-paste("Test", i, sep = "")
    }
  }
  if (!is.null(trace)) {
    if (!any(is.element(trace, param)))
      stop("at least one effect size in argument trace is not available.")
  }

  D<-delta[c(2:(K+1))]

  indicator=function(K,nstu,dat){
    newdat<-dat[c(1:(K+2))]
    dat1<-unique(newdat)
    indicatorm<-matrix(NA, nrow = nstu, ncol = K)
    i=0
    j=0
    for(i in 3:(K+2)){
      for(j in 1:nstu){
        if(dat1[j,i]==1) indicatorm[j,(i-2)]=0
        else indicatorm[j,(i-2)]=1
      }
    }
    return(indicatorm)
  }
  M <- indicator(K,nstu,data)
  monitor<-c("post.se","post.sp","beta","mu1","mu2","post.pi","loglik_dic","theta","pi","stud.se","stud.sp",'ppv','npv','LRpos','LRneg','gamma0')
  data.jags <- list('n' = n, 'delta'=delta,'N' = N,'y'=y,'K'=K,'R1'=R1,'R2'=R2,'nstu'=nstu,'sid'=sid,'eta'=eta, 'xi_preci'=xi_preci, 'mu_gamma'=mu_gamma,
                    'preci_gamma'=preci_gamma, 'gamma1'=gamma1, 'gamma2'=gamma0, 'M'=M)
  rng.seeds <- sample(1e+06, n.chains)
  init<-list()
  for(i in 1:n.chains){
    init[[i]]<-list(mu1=c((-3+i),rep((0.1*i),K)),mu2=rep((-1+i),K),beta=rep((-1+0.3*i),K),.RNG.name = "base::Wichmann-Hill",
                    .RNG.seed = rng.seeds[i])
  }
  message("Start running MCMC...\n")
  model<-system.file("JAGSmodels", "model3MNAR.txt", package="NMADiagT")
  jags.m <-jags.model(model, data = data.jags,inits=init, n.chains = n.chains, n.adapt = n.adapt)
  update(jags.m, n.iter = n.burnin)
  jags.out <- coda.samples(model = jags.m, variable.names = monitor,
                           n.iter = n.iter, thin = n.thin)
  out <- NULL
  out$model <- "HSROC"
  out$rawOutput<-jags.out
  smry <- summary(jags.out)
  smry <- cbind(smry$statistics[, c("Mean", "SD")], smry$quantiles[, c("2.5%", "50%", "97.5%")])

  Se.id <- grep("post.se", rownames(smry))
  Se.stat <- array(paste(format(round(smry[Se.id, "Mean"],
                                      digits = digits), nsmall = digits), " (", format(round(smry[Se.id,
                                                                                                  "SD"], digits = digits), nsmall = digits), ")", sep = ""),
                   dim = c(K, 1))
  colnames(Se.stat) <- "Mean (SD)"
  rownames(Se.stat) <- testname
  Se.quan <- array(paste(format(round(smry[Se.id, "50%"], digits = digits),
                                nsmall = digits), " (", format(round(smry[Se.id, "2.5%"],
                                                                     digits = digits), nsmall = digits), ", ", format(round(smry[Se.id,
                                                                                                                                 "97.5%"], digits = digits), nsmall = digits), ")", sep = ""),
                   dim = c(K, 1))
  colnames(Se.quan) <- "Median (95% CI)"
  rownames(Se.quan) <- testname
  out$Se <- list(Mean_SD = noquote(Se.stat), Median_CI = noquote(Se.quan))
  #specificity
  Sp.id <- grep("post.sp", rownames(smry))
  Sp.stat <- array(paste(format(round(smry[Sp.id, "Mean"],
                                      digits = digits), nsmall = digits), " (", format(round(smry[Sp.id,
                                                                                                  "SD"], digits = digits), nsmall = digits), ")", sep = ""),
                   dim = c(K, 1))
  colnames(Sp.stat) <- "Mean (SD)"
  rownames(Sp.stat) <- testname
  Sp.quan <- array(paste(format(round(smry[Sp.id, "50%"], digits = digits),
                                nsmall = digits), " (", format(round(smry[Sp.id, "2.5%"],
                                                                     digits = digits), nsmall = digits), ", ", format(round(smry[Sp.id,
                                                                                                                                 "97.5%"], digits = digits), nsmall = digits), ")", sep = ""),
                   dim = c(K, 1))
  colnames(Sp.quan) <- "Median (95% CI)"
  rownames(Sp.quan) <- testname
  out$Sp <- list(Mean_SD = noquote(Sp.stat), Median_CI = noquote(Sp.quan))
  #prevalence
  prev.id <- grep("post.pi", rownames(smry))
  prev.stat <- array(paste(format(round(smry[prev.id, "Mean"],
                                        digits = digits), nsmall = digits), " (", format(round(smry[prev.id,
                                                                                                    "SD"], digits = digits), nsmall = digits), ")", sep = ""),
                     dim = c(K, 1))
  colnames(prev.stat) <- "Mean (SD)"
  rownames(prev.stat) <- testname
  prev.quan <- array(paste(format(round(smry[prev.id, "50%"], digits = digits),
                                  nsmall = digits), " (", format(round(smry[prev.id, "2.5%"],
                                                                       digits = digits), nsmall = digits), ", ", format(round(smry[prev.id,
                                                                                                                                   "97.5%"], digits = digits), nsmall = digits), ")", sep = ""),
                     dim = c(K, 1))
  colnames(prev.quan) <- "Median (95% CI)"
  rownames(prev.quan) <- testname
  out$prevalence <- list(Mean_SD = noquote(prev.stat), Median_CI = noquote(prev.quan))
  #ppv
  ppv.id <- grep("ppv", rownames(smry))
  ppv.stat <- array(paste(format(round(smry[ppv.id, "Mean"],
                                       digits = digits), nsmall = digits), " (", format(round(smry[ppv.id,
                                                                                                   "SD"], digits = digits), nsmall = digits), ")", sep = ""),
                    dim = c(K, 1))
  colnames(ppv.stat) <- "Mean (SD)"
  rownames(ppv.stat) <- testname
  ppv.quan <- array(paste(format(round(smry[ppv.id, "50%"], digits = digits),
                                 nsmall = digits), " (", format(round(smry[ppv.id, "2.5%"],
                                                                      digits = digits), nsmall = digits), ", ", format(round(smry[ppv.id,
                                                                                                                                  "97.5%"], digits = digits), nsmall = digits), ")", sep = ""),
                    dim = c(K, 1))
  colnames(ppv.quan) <- "Median (95% CI)"
  rownames(ppv.quan) <- testname
  out$ppv <- list(Mean_SD = noquote(ppv.stat), Median_CI = noquote(ppv.quan))
  #npv
  npv.id <- grep("npv", rownames(smry))
  npv.stat <- array(paste(format(round(smry[npv.id, "Mean"],
                                       digits = digits), nsmall = digits), " (", format(round(smry[npv.id,
                                                                                                   "SD"], digits = digits), nsmall = digits), ")", sep = ""),
                    dim = c(K, 1))
  colnames(npv.stat) <- "Mean (SD)"
  rownames(npv.stat) <- testname
  npv.quan <- array(paste(format(round(smry[npv.id, "50%"], digits = digits),
                                 nsmall = digits), " (", format(round(smry[npv.id, "2.5%"],
                                                                      digits = digits), nsmall = digits), ", ", format(round(smry[npv.id,
                                                                                                                                  "97.5%"], digits = digits), nsmall = digits), ")", sep = ""),
                    dim = c(K, 1))
  colnames(npv.quan) <- "Median (95% CI)"
  rownames(npv.quan) <- testname
  out$npv <- list(Mean_SD = noquote(npv.stat), Median_CI = noquote(npv.quan))
  #LRpos
  LRpos.id <- grep("LRpos", rownames(smry))
  LRpos.stat <- array(paste(format(round(smry[LRpos.id, "Mean"],
                                         digits = digits), nsmall = digits), " (", format(round(smry[LRpos.id,
                                                                                                     "SD"], digits = digits), nsmall = digits), ")", sep = ""),
                      dim = c(K, 1))
  colnames(LRpos.stat) <- "Mean (SD)"
  rownames(LRpos.stat) <- testname
  LRpos.quan <- array(paste(format(round(smry[LRpos.id, "50%"], digits = digits),
                                   nsmall = digits), " (", format(round(smry[LRpos.id, "2.5%"],
                                                                        digits = digits), nsmall = digits), ", ", format(round(smry[LRpos.id,
                                                                                                                                    "97.5%"], digits = digits), nsmall = digits), ")", sep = ""),
                      dim = c(K, 1))
  colnames(LRpos.quan) <- "Median (95% CI)"
  rownames(LRpos.quan) <- testname
  out$LRpos <- list(Mean_SD = noquote(LRpos.stat), Median_CI = noquote(LRpos.quan))
  #LRneg
  LRneg.id <- grep("LRneg", rownames(smry))
  LRneg.stat <- array(paste(format(round(smry[LRneg.id, "Mean"],
                                         digits = digits), nsmall = digits), " (", format(round(smry[LRneg.id,
                                                                                                     "SD"], digits = digits), nsmall = digits), ")", sep = ""),
                      dim = c(K, 1))
  colnames(LRneg.stat) <- "Mean (SD)"
  rownames(LRneg.stat) <- testname
  LRneg.quan <- array(paste(format(round(smry[LRneg.id, "50%"], digits = digits),
                                   nsmall = digits), " (", format(round(smry[LRneg.id, "2.5%"],
                                                                        digits = digits), nsmall = digits), ", ", format(round(smry[LRneg.id,
                                                                                                                                    "97.5%"], digits = digits), nsmall = digits), ")", sep = ""),
                      dim = c(K, 1))
  colnames(LRneg.quan) <- "Median (95% CI)"
  rownames(LRneg.quan) <- testname
  out$LRneg <- list(Mean_SD = noquote(LRneg.stat), Median_CI = noquote(LRneg.quan))

  if (conv.diag) {
    message("Start calculating MCMC convergence diagnostic statistics...\n")
    conv.out <- gelman.diag(jags.out, multivariate = FALSE)
    conv.out <- conv.out$psrf
    conv <- file.path(directory,"ConvergenceDiagnostic.txt")
    write.table(conv.out, conv, row.names = rownames(conv.out),
                col.names = TRUE)
  }
  if (dic) {
    message("Start calculating deviance information criterion statistics...\n")
    dic.out <- dic.samples(model = jags.m, n.iter = n.iter,
                           thin = n.thin)
    dev <- sum(dic.out$deviance)
    pen <- sum(dic.out$penalty)
    if(is.nan(pen)) pen=0.0
    pen.dev <- dev + pen
    dic.stat <- rbind(dev, pen, pen.dev)
    rownames(dic.stat) <- c("D.bar", "pD", "DIC")
    colnames(dic.stat) <- ""
    out$DIC <- dic.stat
  }
  if (mcmc.samples) {
    out$mcmc.samples <- jags.out
  }
  if (!is.null(trace)) {
    message("Start saving trace plots...\n")
  }
  if (is.element("Se", trace)) {
    for (i in 1:K) {
      file<-file.path(directory, paste("TracePlot_Se_", testname[i], ".pdf", sep = ""))
      pdf(file)
      oldpar<-par(mfrow = c(n.chains, 1))
      on.exit(par(oldpar))
      for (j in 1:n.chains) {
        temp <- as.vector(jags.out[[j]][, paste("post.se[",
                                                i, "]", sep = "")])
        plot(temp, type = "l", col = "red", ylab = "Sensitivity",
             xlab = "Iterations", main = paste("Chain",
                                               j))
      }
      dev.off()
    }
  }
  if (is.element("Sp", trace)) {
    for (i in 1:K) {
      file<-file.path(directory, paste("TracePlot_Sp_", testname[i], ".pdf", sep = ""))
      pdf(file)
      oldpar<-par(mfrow = c(n.chains, 1))
      on.exit(par(oldpar))
      for (j in 1:n.chains) {
        temp <- as.vector(jags.out[[j]][, paste("post.sp[",
                                                i, "]", sep = "")])
        plot(temp, type = "l", col = "red", ylab = "Specificity",
             xlab = "Iterations", main = paste("Chain",
                                               j))
      }
      dev.off()
    }
  }
  if (is.element("LRpos", trace)) {
    for (i in 1:K) {
      file<-file.path(directory, paste("TracePlot_LRpose_", testname[i], ".pdf", sep = ""))
      pdf(file)
      oldpar<-par(mfrow = c(n.chains, 1))
      on.exit(par(oldpar))
      for (j in 1:n.chains) {
        temp <- as.vector(jags.out[[j]][, paste("LRpos[",
                                                i, "]", sep = "")])
        plot(temp, type = "l", col = "red", ylab = "LRpos",
             xlab = "Iterations", main = paste("Chain",
                                               j))
      }
      dev.off()
    }
  }
  if (is.element("LRneg", trace)) {
    for (i in 1:K) {
      file<-file.path(directory, paste("TracePlot_LRneg_", testname[i], ".pdf", sep = ""))
      pdf(file)
      oldpar<-par(mfrow = c(n.chains, 1))
      on.exit(par(oldpar))
      for (j in 1:n.chains) {
        temp <- as.vector(jags.out[[j]][, paste("LRneg[",
                                                i, "]", sep = "")])
        plot(temp, type = "l", col = "red", ylab = "LRneg",
             xlab = "Iterations", main = paste("Chain",
                                               j))
      }
      dev.off()
    }
  }
  if (is.element("prev", trace)) {
    file<-file.path(directory, paste("TracePlot_prevalence.pdf", sep = ""))
    pdf(file)
    oldpar<-par(mfrow = c(n.chains, 1))
    on.exit(par(oldpar))
    for (j in 1:n.chains) {
      temp <- as.vector(jags.out[[j]][, paste("post.pi",sep = "")])
      plot(temp, type = "l", col = "red", ylab = "prevalence",
           xlab = "Iterations", main = paste("Chain",
                                             j))
    }
    dev.off()
  }
  if (is.element("ppv", trace)) {
    for (i in 1:K) {
      file<-file.path(directory, paste("TracePlot_ppv_", testname[i], ".pdf", sep = ""))
      pdf(file)
      oldpar<-par(mfrow = c(n.chains, 1))
      on.exit(par(oldpar))
      for (j in 1:n.chains) {
        temp <- as.vector(jags.out[[j]][, paste("ppv[",
                                                i, "]", sep = "")])
        plot(temp, type = "l", col = "red", ylab = "ppv",
             xlab = "Iterations", main = paste("Chain",
                                               j))
      }
      dev.off()
    }
  }
  if (is.element("npv", trace)) {
    for (i in 1:K) {
      file<-file.path(directory, paste("TracePlot_npv_", testname[i], ".pdf", sep = ""))
      pdf(file)
      oldpar<-par(mfrow = c(n.chains, 1))
      on.exit(par(oldpar))
      for (j in 1:n.chains) {
        temp <- as.vector(jags.out[[j]][, paste("npv[",
                                                i, "]", sep = "")])
        plot(temp, type = "l", col = "red", ylab = "npv",
             xlab = "Iterations", main = paste("Chain",
                                               j))
      }
      dev.off()
    }
  }
  class(out) <- "nmadt"
  return(out)
  #options(warn = 0)
}

