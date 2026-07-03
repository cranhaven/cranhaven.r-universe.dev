\name{MAR_simulation_results}

\alias{MAR_simulation_results}

\docType{data}

\title{
  Missing at random (MAR) simulation results.
}

\description{
  Results of a simulation study on the effect of missing data on estimates of parameters of a hidden Markov model. In the simulation, 1000 datasets were simulated, each consisting of 100 time series of length 50. The model generating the data was a hidden Markov model with 3 states. Missing values were generated for 25\% of cases, independent of the hidden state (i.e. data can be considered missing at random.)
}

\usage{data("MAR_simulation_results")}
\format{
  A \code{data.frame} with the following variables:
  \describe{
    \item{\code{parameter}}{Parameter name}
    \item{\code{true}}{True value of parameter}
    \item{\code{MAR_estimates_mean}}{Average of parameter estimates for a model that assumes MAR.}
    \item{\code{MAR_estimates_sd}}{Standard deviation of parameter estimates for a model that assumes MAR.}
    \item{\code{MAR_estimates_MAE}}{Mean Absolute Error of parameter estimates for a model that assumes MAR.}
    \item{\code{MNAR_estimates_mean}}{Average of parameter estimates for a model that assumes MNAR.}
    \item{\code{MNAR_estimates_sd}}{Standard deviation of parameter estimates for a model that assumes MNAR.}
    \item{\code{MNAR_estimates_MAE}}{Mean Absolute Error of parameter estimates for a model that assumes MNAR.}
    \item{\code{percMAE}}{Relative MAE of the MNAR model over the MAR model (e.g. MAE_MNAR/MAE_MAR)}
  }
}

\details{

This data frame was generated with the following code

\preformatted{
library(depmixS4)

### Start simulation

nsim <- 1000
nrep <- 100
nt <- 50

set.seed(1234)
randomSeeds <- sample(seq(1,nsim*1000),nsim)
out <- rep(list(vector("list",3)),nsim)

prior <- c(8,1,1)
prior <- prior/sum(prior)
transition <- 5*diag(3) + 1
transition <- transition/rowSums(transition)
means <- c(-1,0,1)
sds <- c(3,3,3)
pmiss <- c(.25,.25,.25)

truepars1 <- c(prior,as.numeric(t(transition)),as.numeric(rbind(means,sds)))
truepars2 <- c(prior,as.numeric(t(transition)),as.numeric(rbind(means,sds,1-pmiss,pmiss)))

for(sim in 1:nsim) {
  set.seed(randomSeeds[sim])
  truestate <- matrix(nrow=nt,ncol=nrep) 
  for(i in 1:nrep) {
    truestate[1,i] <- sample(1:3,size=1,prob=prior)
    for(t in 2:nt) {
      truestate[t,i] <- sample(1:3,size=1,prob=transition[truestate[t-1,i],])
    }
  }
  dat <- data.frame(trueState=as.numeric(truestate),trial=1:nt)
  dat$trueResponse <- rnorm(nrow(dat),mean=means[dat$trueState],sd=sds[dat$trueState])
  dat$missing <- rbinom(nrow(dat),size=1,prob=pmiss[dat$trueState])
  dat$response <- dat$trueResponse
  dat$response[dat$missing==1] <- NA
  
  set.seed(randomSeeds[sim])
  mod <- depmix(list(response~1),family=list(gaussian()),data=dat,nstates=3,ntimes=rep(nt,nrep))
  mod <- setpars(mod,truepars1)
  ok <- FALSE
  ntry <- 1
  while(!ok && ntry <= 50) {
    fmod <- try(fit(mod,emcontrol=em.control(maxit = 5000, random.start=FALSE),verbose=FALSE))
    if(!inherits(fmod,"try-error")) {
      if(fmod@message == "Log likelihood converged to within tol. (relative change)") ok <- TRUE
    }
    ntry <- ntry + 1
  }
  out[[sim]][[1]] <- list(pars=getpars(fmod),logLik=logLik(fmod),viterbi=posterior(fmod)[,1],trueState=dat$trueState)
  
  set.seed(randomSeeds[sim])
  mod <- depmix(list(response~1,missing~1),family=list(gaussian(),multinomial("identity")),data=dat,nstates=3,ntimes=rep(nt,nrep))
  mod <- setpars(mod,truepars2)
  ok <- FALSE
  ntry <- 1
  while(!ok && ntry <= 50) {
    fmod <- try(fit(mod,emcontrol=em.control(maxit = 5000, random.start = FALSE),verbose=FALSE))
    if(!inherits(fmod,"try-error")) {
      if(fmod@message == "Log likelihood converged to within tol. (relative change)") ok <- TRUE
    }
    ntry <- ntry + 1
  }
  out[[sim]][[2]] <- list(pars=getpars(fmod),logLik=logLik(fmod),viterbi=posterior(fmod)[,1],trueState=dat$trueState)
}

### End simulation

### Process results
library(reshape2)
library(dplyr)
library(tidyr)

simi <- out

bias1 <- matrix(0.0,ncol=length(truepars1),nrow=nsim)
bias2 <- matrix(0.0,ncol=length(truepars2),nrow=nsim)

colnames(bias1) <- names(simi[[1]][[1]][[1]])
colnames(bias2) <- names(simi[[1]][[2]][[1]])

pcorstate1 <- rep(0.0,nsim)
pcorstate2 <- rep(0.0,nsim)

for(sim in 1:nsim) {
  tmp <- simi[[sim]][[1]][[1]]
  pr <- tmp[1:3]
  trt <- matrix(tmp[4:12],ncol=3)
  ms <- tmp[c(13,15,17)]
  sds <- tmp[c(14,16,18)] 
  ord <- order(ms)
  bias1[sim,] <- c(pr[ord],trt[ord,ord],as.numeric(rbind(ms[ord],sds[ord]))) - truepars1
  fsta <- recode(simi[[sim]][[1]]$viterbi,`1` = which(ord == 1), `2` = which(ord == 2), `3` = which(ord == 3))
  pcorstate1[sim] <- sum(fsta == simi[[sim]][[1]]$trueState)/(nrep*nt)
  
  tmp <- simi[[sim]][[2]][[1]]
  pr <- tmp[1:3]
  trt <- matrix(tmp[4:12],ncol=3)
  ms <- tmp[c(13,17,21)]
  sds <- tmp[c(14,18,22)]
  ps0 <- tmp[c(15,19,23)]
  ps1 <- tmp[c(16,20,24)]
  ord <- order(ms)
  bias2[sim,] <- c(pr[ord],trt[ord,ord],as.numeric(rbind(ms[ord],sds[ord],ps0[ord],ps1[ord]))) - truepars2
  fsta <- recode(simi[[sim]][[2]]$viterbi,`1` = which(ord == 1), `2` = which(ord == 2), `3` = which(ord == 3))
  pcorstate2[sim] <- sum(fsta == simi[[sim]][[2]]$trueState)/(nrep*nt)
}

sim3_bias1 <- bias1
sim3_bias2 <- bias2
sim3_est1 <- t(t(bias1) + truepars1)
sim3_est2 <- t(t(bias2) + truepars2)
sim3_pcorstate1 <- pcorstate1
sim3_pcorstate2 <- pcorstate2

tmp <- as.data.frame(sim3_est1)
colnames(tmp) <- c("$\\pi_1$","$\\pi_2$","$\\pi_3$","$a_{11}$","$a_{12}$","$a_{13}$","$a_{21}$","$a_{22}$","$a_{23}$","$a_{31}$","$a_{32}$", "$a_{33}$", "$\\mu_1$","$\\sigma_1$","$\\mu_2$","$\\sigma_2$","$\\mu_3$","$\\sigma_3$")
tmp$sim <- 1:nsim
tmp <- gather(tmp,key="param",value="estimate",-sim)
tmp$true <- rep(truepars1,each=nsim)
tmp$method <- "MAR"
tmp$variance <- "low"
tmp$missing <- "equal"
sim4_est1_long <- tmp

tmp <- as.data.frame(sim3_est2)
colnames(tmp) <- c("$\\pi_1$","$\\pi_2$","$\\pi_3$","$a_{11}$","$a_{12}$","$a_{13}$","$a_{21}$","$a_{22}$","$a_{23}$","$a_{31}$","$a_{32}$", "$a_{33}$", "$\\mu_1$","$\\sigma_1$","pnm_1","$p(M=1|S=1)$","$\\mu_2$","$\\sigma_2$","pnm_2","$p(M=1|S=2)$","$\\mu_3$","$\\sigma_3$","pnm_3","$p(M=1|S=3)$")
tmp$sim <- 1:nsim
tmp <- gather(tmp,key="param",value="estimate",-sim)
tmp$true <- rep(truepars2,each=nsim)
tmp$method <- "MNAR"
tmp$variance <- "low"
tmp$missing <- "equal"
sim4_est2_long <- tmp

all_long <- rbind(
  sim4_est1_long,
  subset(sim4_est2_long,!(param %in% c("pnm_1","pnm_2","pnm_3")))
)

my_table <- all_long %>% 
  group_by(param, method, variance, missing) %>%
  summarize(true = mean(true),
            mean = mean(estimate),
            sd = sd(estimate),
            bias = mean(abs(true - estimate)/abs(true)),
            MAE = mean(abs(true - estimate))) %>%
  gather(measure,value,-c(param,method,variance,missing)) %>%
  recast(param + variance + missing  ~  method + measure)

MAR_simulation_results <- my_table %>%
  filter(variance == "low" & missing == "equal") %>%
  dplyr::select(param,MAR_true,paste0("MAR_",c("mean","sd","MAE"),sep=""),
                paste0("MNAR_",c("mean","sd","MAE"),sep="")) %>%
  mutate(percMAE = MNAR_MAE/MAR_MAE) 

colnames(MAR_simulation_results) <- c("parameter", "true", "MAR_estimates_mean", 
  "MAR_estimates_sd", "MAR_estimates_MAE", "MNAR_estimates_mean",
  "MNAR_estimates_sd", "MNAR_estimates_MAE", "percMAE")
MAR_simulation_results <- MAR_simulation_results[c(1:3,7:9,4:6,10:21),]
}

}

\examples{
  data(MAR_simulation_results)
}
\keyword{datasets}
