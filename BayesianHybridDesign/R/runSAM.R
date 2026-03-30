#' Generating Operating Characteristics of SAM Priors
#'
#' This function is modified based on the get_OC function from the SAMprior R package version 1.1.1.
#'
#'
#' @param if.prior Informative prior constructed from historical data,
#' represented (approximately) as a mixture of conjugate distributions.
#' @param theta.h Estimate of the treatment effect based on historical data.
#' If missing, the default value is set to be the posterior mean estimate from
#' \code{if.prior}.
#' @param method.w Methods used to determine the mixture weight for SAM priors.
#' The default method is LRT (Likelihood Ratio Test), the alternative option can
#' be PPR (Posterior Probability Ratio). See \code{\link{SAM_weight}} for more
#' details.
#' @param prior.odds The prior probability of \eqn{H_0} being true compared to
#' the prior probability of \eqn{H_1} being true using PPR method. The default
#' value is 1. See \code{\link{SAM_weight}} for more details.
#' @param nf.prior Non-informative prior used for constructing the SAM prior
#' and robust MAP prior.
#' @param delta Clinically significant difference used for the SAM prior.
#' @param n Sample size for the control arm.
#' @param n.t Sample size for the treatment arm.
#' @param decision Decision rule to compare the treatment with the control;
#'   see \code{\link[RBesT]{decision2S}}.
#' @param ntrial Number of trials simulated.
#' @param if.MAP Whether to simulate the operating characteristics of the
#' robust MAP prior for comparison, the default value is \code{FALSE}.
#' @param weight Weight assigned to the informative prior component
#' (\eqn{0 \leq} \code{weight} \eqn{\leq 1}) for the robust MAP prior,
#' the default value is 0.5.
#' @param theta A vector of the response rate (binary endpoints) or mean
#' (continuous endpoints) for the control arm.
#' @param theta.t A vector of the response rate (binary endpoints) or mean
#' (continuous endpoints) for the treatment arm.
#' @param datamat A ntrial * 2 matrix of simulated binomial samples,
#' first column is study treament group treatment, second column is study control group
#' @param ... Additional parameters for continuous endpoints.
#'
#' @details
#' This function is modified based on the \code{get_OC} from the SAMprior R package version 1.1.1.
#' The modification is made in order that the function can take simulated data (the datamat argument) from outside the function rather than simulating data within the funciton
#' We used the same data for all methods for comparison in order to get a better comparison.
#'
#' The \code{runSAM} function is designed to generate the operating
#' characteristics of SAM priors (\emph{Yang, et al., 2023}), including the
#' relative bias, relative mean squared error, and type I error and power
#' under a two-arm comparative trial design. As an option, the operating
#' characteristic of robust MAP priors (\emph{Schmidli, et al., 2014})
#' can also be generated for comparison.
#'
#' The \code{runSAM} function is designed to generate the operating
#' characteristics of SAM priors, including the relative bias, relative
#' mean squared error, and type I error, and power under a two-arm
#' comparative trial design. As an option, the operating characteristics of
#' robust MAP priors (\emph{Schmidli, et al., 2014}) can also be generated for
#' comparison.
#'
#' The relative bias is defined as the difference between the bias of a method
#' and the bias of using a non-informative prior. The relative mean squared
#' error is the difference between the mean squared error (MSE) of a method and
#' the MES of using a non-informative prior.
#'
#' To evaluate type I error and power, the determination of whether the
#' treatment is superior to the control is calculated based on function
#' \code{\link[RBesT]{decision2S}}.
#'
#'
#' @return Returns a list
#' result - dataframe that contains the relative bias, relative MSE,
#' type I error, and power for both SAM priors, as well as robust MAP priors.
#' Additionally, the mixture weight of the SAM prior is also displayed.
#'
#' simulated.data - a matrix of two columns, first treatment, second control
#' post_theta_t_list - list of all replication, posterior distribution of treatment group
#' post_theta_c_list - list of all replication, posterior distribution of control group, non informative prior
#' post_theta_c_SAM_list - list of all replication, posterior distribution of control group, SAM prior
#' post_theta_c_MAP_list - list of all replication, posterior distribution of control group, MAP prior
#'
#'
#' @references Yang P, Zhao Y, Nie L, Vallejo J, Yuan Y.
#' SAM: Self-adapting mixture prior to dynamically borrow information from
#' historical data in clinical trials. \emph{Biometrics} 2023; 00, 1–12.
#' https://doi.org/10.1111/biom.13927
#' @references Schmidli H, Gsteiger S, Roychoudhury S, O'Hagan A, Spiegelhalter D, Neuenschwander B.
#' Robust meta-analytic-predictive priors in clinical trials with historical control information.
#' \emph{Biometrics} 2014; 70(4):1023-1032.
#'
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' ## Example of a binary endpoint
#' ## Consider a randomized comparative trial designed to borrow information
#' ## from historical data on the control. We assumed a non-informative prior
#' ## beta(1, 1) and an informative prior beta(30, 50) after incorporating
#' ## the historical data. The treatment is regarded as superior to the control
#' ## if Pr(RR.t > RR.c | data) > 0.95, where RR.t and RR.c are response rates
#' ##  of the treatment and control, respectively. The operating characteristics
#' ##  were assessed under the scenarios of (RR.c, RR.t) = (0.3, 0.36) and (0.3, 0.56).
#' ## OC <- runSAM(## Informative prior constructed based on historical data
#' ##              if.prior = mixbeta(c(1, 30, 50)),
#' ##              ## Non-informative prior used for constructing the SAM prior
#' ##              nf.prior = mixbeta(c(1,1,1)),
#' ##              delta    = 0.2,  ## Clinically significant difference
#' ##              n = 35,          ## Sample size for the control arm
#' ##              n.t = 70,        ## Sample size for the treatment arm
#' ##              ## Decision rule to compare the whether treatment is superior
#' ##              ## than the control
#' ##              decision = decision2S(0.95, 0, lower.tail=FALSE),
#' ##              ntrial   = 1000,  ## Number of trials simulated
#' ##              ## Weight assigned to the informative component for MAP prior
#' ##              weight = 0.5,
#' ##              ## A vector of response rate for the control arm
#' ##              theta    = c(0.3, 0.36),
#' ##              ## A vector of response rate for the treatment arm
#' ##              theta.t  = c(0.3, 0.56))
#' ## OC
#'
#' ## Example of continuous endpoint
#' ## Consider a randomized comparative trial designed to borrow information
#' ## from historical data on the control. We assumed a non-informative prior
#' ## N(0, 1e4) and an informative prior N(0.5, 2) after incorporating
#' ## the historical data. The treatment is regarded as superior to the control
#' ## if Pr(mean.t > mean.c | data) > 0.95, where mean.t and mean.c are mean
#' ##  of the treatment and control, respectively. The operating characteristics
#' ##  were assessed under the scenarios of (mean.c, mean.t) = (0.1, 0.1) and
#' ## (0.5, 1.0).
#' sigma      <- 2
#' prior.mean <- 0.5
#' prior.se   <- sigma/sqrt(100)
#' ## OC <- runSAM(## Informative prior constructed based on historical data
#' ##              if.prior = mixnorm(c(1, prior.mean, prior.se)),
#' ##              ## Non-informative prior used for constructing the SAM prior
#' ##              nf.prior = mixnorm(c(1, 0, 1e4)),
#' ##              delta    = 0.2 * sigma,  ## Clinically significant difference
#' ##              n = 100,                 ## Sample size for the control arm
#' ##              n.t = 200,               ## Sample size for the treatment arm
#' ##              ## Decision rule to compare the whether treatment is superior
#' ##              ## than the control
#' ##              decision = decision2S(0.95, 0, lower.tail=FALSE),
#' ##              ntrial   = 1000,  ## Number of trials simulated
#' ##              ## A vector of mean for the control arm
#' ##              theta    = c(0.1, 0.5),
#' ##              ## A vector of mean for the treatment arm
#' ##              theta.t  = c(0.1, 1.0),
#' ##              sigma = sigma)
#' ## OC
#' }
#'
#' @import Metrics
#' @import RBesT
#' @import assertthat
#' @import checkmate
#' @import ggplot2
#' @import stats
#'
#' @export
runSAM <- function(if.prior, theta.h, method.w, prior.odds, nf.prior, delta, n, n.t, decision, ntrial, if.MAP, weight, theta, theta.t, datamat = NULL, ...) UseMethod("runSAM")
#' @export
runSAM.default <- function(if.prior, theta.h, method.w, prior.odds, nf.prior, delta, n, n.t, decision, ntrial, if.MAP, weight, theta, theta.t, datamat = NULL, ...) "Unknown density"

#' @describeIn runSAM The function is designed to generate the operating
#' characteristics of SAM priors for binary endpoints.
#' @export
runSAM.betaMix <- function(if.prior, theta.h, method.w, prior.odds, nf.prior, delta, n, n.t, decision, ntrial, if.MAP, weight, theta, theta.t, datamat=NULL, ...) {

  ## Check if theta and theta.t is the same length
  if(length(theta) != length(theta.t)){
    stop('Theta under control and treatment should be the same length!')
  }

  if(missing(decision)){
    stop('Please input decision!')
  }

  if(missing(if.prior)){
    stop('Please input the informative prior!')
  }

  if(missing(n)){
    stop('Please input the sample size for control arm!')
  }

  if(missing(n.t)){
    stop('Please input the sample size for treatment arm!')
  }

  if(missing(ntrial)){
    stop('Please input the number of trials for simulation!')
  }

  if(missing(delta)){
    stop('Please input clinically significant difference!')
  }

  if(missing(theta.h)){
    message("Using the posterior mean from informative prior as the estimate of the treatment effect based on historical data.")
    theta.h <- summary(if.prior)['mean']
  }


  if(missing(method.w)){
    message("Using the LRT (Likelihood Ratio Test) as the default method to calculate mixture weight for SAM priors.")
    method.w = 'LRT'
  }

  if(method.w == 'PPR' & missing(prior.odds)){
    message("Missing the prior odds, set as 1.")
    prior.odds = 1
  }

  if(missing(nf.prior)) {
    message("Using default uniform prior as non-informative prior.")
    nf.prior <- mixbeta(nf.prior = c(1,1,1))
  }

  if(missing(if.MAP)){
    if.MAP <- FALSE
  }

  if(missing(weight)) {
    if(if.MAP) message("Using default weight 0.5 to the informative component of MAP prior")
    weight <- 0.5
  }

  n_grid <- length(theta)

  ## Across all grid of theta
  res_OC <- res_Bias <- res_RMSE <- array(0, c(n_grid, 2))
  res_weight <- rep(0, n_grid)

  ## Store the type I error or power for two results
  res_SAM <- res_Mix <- res_NP <- rep(0, n_grid)

  datamat2 = matrix(NA,ntrial,2)
  post_theta_t_list = list()
  post_theta_c_list = list()
  post_theta_c_SAM_list = list()
  post_theta_c_MAP_list = list()

  for(i in 1:n_grid){

    ## Treatment and control response
    theta_trt <- theta.t[i];
    theta_ctr <- theta[i]

    ## Store Bias and RMSE results for SAM and robust MAP prior
    theta_SAM <- theta_Mix <- theta_NP <- tmp_weight <- c()
    res_SAM_tmp <- res_Mix_tmp <- res_Non_tmp <- c()
    res_SAM_dist <- res_Mix_dist <- res_Non_dist <- c()

    for(s in 1:ntrial){

      ## Simulate control trial and treatment
      if(is.null(datamat)){
        x <- rbinom(n = 1, size = n, prob = theta_ctr)
      } else{
        x <- datamat[s,2]
      }
      datamat2[s,2] <- x
      ##-------------------------------
      ## Non-informative prior
      ##-------------------------------
      theta_NP <- c(theta_NP, (x + 1) / (n + 1 + 1))

      ##------------------------
      ## SAM prior
      ##------------------------

      ## Calculate SAM weight
      wSAM <- SAM_weight(if.prior = if.prior,
                         theta.h = theta.h,
                         method.w = method.w,
                         prior.odds = prior.odds,
                         delta = delta, n = n, r = x)

      tmp_weight <- c(tmp_weight, wSAM)

      ## Construct SAM prior
      SAM.prior <- SAM_prior(if.prior = if.prior,
                             nf.prior = nf.prior,
                             weight = wSAM,
                             delta = delta)

      ## Poerior inference
      SAM.post <- postmix(SAM.prior, n = n, r = x)

      theta_SAM <- c(theta_SAM, summary(SAM.post)['mean'])

      post_theta_c_SAM_list[[s]] = SAM.post


      ##------------------------
      ## Robust MAP prior
      ##------------------------
      if(if.MAP){
        prior.Mix <- robustify(priormix = if.prior, weight = 1 - weight,
                               mean = 1/2)

        posterior.Mix <- postmix(prior.Mix, n = n, r = x)

        theta_Mix <- c(theta_Mix, summary(posterior.Mix)['mean'])

        post_theta_c_MAP_list[[s]] = posterior.Mix
      }
      ##----------------------------
      ## Compute type I error/power
      ##----------------------------
      ## Simulate treatment trial
      if(is.null(datamat)){
        y <- rbinom(n = 1, size = n.t, prob = theta_trt)
      } else{
        y <- datamat[s,1]
      }
      datamat2[s,1] <- y

      ## Posterior of theta
      post_theta_t <- postmix(nf.prior, n = n.t, r = y)
      post_theta_c <- postmix(nf.prior, n = n, r = x)

      post_theta_t_list[[s]] = post_theta_t
      post_theta_c_list[[s]] = post_theta_c

      res_SAM_tmp <- c(res_SAM_tmp, decision(post_theta_t, SAM.post))
      res_SAM_dist <- c(res_SAM_dist, decision(post_theta_t, SAM.post, dist = TRUE))
      if(if.MAP){
        res_Mix_tmp <- c(res_Mix_tmp, decision(post_theta_t, posterior.Mix))
        res_Mix_dist <- c(res_Mix_dist, decision(post_theta_t, posterior.Mix, dist = TRUE))
      }
      res_Non_tmp <- c(res_Non_tmp, decision(post_theta_t, post_theta_c))
      res_Non_dist <- c(res_Non_dist, decision(post_theta_t, post_theta_c, dist = TRUE))


    }

    ## Store the type I error or power
    res_SAM[i] <- mean(res_SAM_tmp)
    if(if.MAP){
      res_Mix[i] <- mean(res_Mix_tmp)
    }
    res_NP[i]  <- mean(res_Non_tmp)

    ## Store the data for bias
    res_Bias[i,1]   <- mean(theta_SAM) - mean(theta_NP)
    if(if.MAP){
      res_Bias[i,2]   <- mean(theta_Mix) - mean(theta_NP)
    }
    ## Store the data for RMSE
    res_RMSE[i,1]   <- mse(theta_SAM, theta_ctr) - mse(theta_NP, theta_ctr)
    if(if.MAP){
      res_RMSE[i,2]   <- mse(theta_Mix, theta_ctr) - mse(theta_NP, theta_ctr)
    }
    res_weight[i] <- mean(tmp_weight)

  }

  if(if.MAP){

    return(list(result = data.frame(scenarios = 1:n_grid,
                                    Bias_SAM  = res_Bias[,1],
                                    Bias_rMAP = res_Bias[,2],
                                    RMSE_SAM  = res_RMSE[,1],
                                    RMSE_rMAP = res_RMSE[,2],
                                    wSAM      = res_weight,
                                    res_SAM   = res_SAM,
                                    res_rMAP  = res_Mix,
                                    res_NP    = res_NP),
                simulated.data = datamat2,
                post_theta_t_list = post_theta_t_list,
                post_theta_c_list = post_theta_c_list,
                post_theta_c_SAM_list = post_theta_c_SAM_list,
                post_theta_c_MAP_list = post_theta_c_MAP_list,
                res_SAM_dist = res_SAM_dist,
                res_rMAP_dist = res_Mix_dist,
                res_Non_dist = res_Non_dist))

  }else{

    return(list(result = data.frame(scenarios = 1:n_grid,
                               Bias_SAM = res_Bias[,1],
                               RMSE_SAM = res_RMSE[,1],
                               wSAM = res_weight,
                               res_SAM = res_SAM,
                               res_NP   = res_NP),
           simulated.data = datamat2,
           post_theta_t_list = post_theta_t_list,
           post_theta_c_list = post_theta_c_list,
           post_theta_c_SAM_list = post_theta_c_SAM_list,
           res_SAM_dist = res_SAM_dist,
           res_Non_dist = res_Non_dist))

  }

}

#' @describeIn runSAM The function is designed to generate the operating
#' characteristics of SAM priors for continuous endpoints.
#' @param sigma Variance to simulate the continuous endpoint under normality
#' assumption.
#' @export
runSAM.normMix <- function(if.prior, theta.h, method.w, prior.odds, nf.prior, delta, n, n.t, decision, ntrial, if.MAP, weight, theta, theta.t, datamat = NULL,  ..., sigma) {

  ## Check if theta and theta.t is the same length
  if(length(theta) != length(theta.t)){
    stop('Theta under control and treatment should be the same length!')
  }

  if(missing(decision)){
    stop('Please input decision!')
  }

  if(missing(if.prior)){
    stop('Please input the informative prior!')
  }

  if(missing(nf.prior)){
    stop('Please input the non-informative prior!')
  }

  if(missing(n)){
    stop('Please input the sample size for control arm!')
  }

  if(missing(n.t)){
    stop('Please input the sample size for treatment arm!')
  }

  if(missing(ntrial)){
    stop('Please input the number of trials for simulation!')
  }

  if(missing(delta)){
    stop('Please input clinically significant difference!')
  }

  if(!missing(method.w)){
    assertthat::assert_that(all(method.w %in% c('LRT', 'PPR')))
    assertthat::assert_that(length(method.w) == 1)
  }

  if(missing(theta.h)){
    message("Using the posterior mean from informative prior as the estimate of the treatment effect based on historical data.")
    theta.h <- summary(if.prior)['mean']
  }

  if(missing(method.w)){
    message("Using the LRT (Likelihood Ratio Test) as the default method to calculate mixture weight for SAM priors.")
    method.w = 'LRT'
  }

  if(method.w == 'PPR' & missing(prior.odds)){
    message("Missing the prior odds, set as 1.")
    prior.odds = 1
  }

  if(missing(if.MAP)){
    if.MAP <- FALSE
  }

  if(missing(weight)) {
    if(if.MAP) message("Using default weight 0.5 to the informative component of MAP prior")
    weight <- 0.5
  }

  if(missing(sigma)) {
    message("Using default prior reference scale ", RBesT::sigma(if.prior))
    sigma <- RBesT::sigma(if.prior)
  }
  if(missing(nf.prior)) {
    message(paste("Using default unit-information prior as non-informative prior"))
    nf.prior <- mixnorm(nf.prior = c(1,summary(if.prior)['mean'],sigma),
                        param = 'ms')
  }

  n_grid <- length(theta)

  ## Across all grid of theta
  res_OC <- res_Bias <- res_RMSE <- array(0, c(n_grid, 2))
  res_weight <- rep(0, n_grid)

  ## Store the type I error or power for two results
  res_SAM <- res_Mix <- res_NP <- rep(0, n_grid)

  for(i in 1:n_grid){

    ## Treatment and control response
    theta_trt <- theta.t[i];
    theta_ctr <- theta[i]

    ## Store Bias and RMSE results for SAM and robust MAP prior
    theta_SAM <- theta_Mix <- theta_NP <- tmp_weight <- c()
    res_SAM_tmp <- res_Mix_tmp <- res_Non_tmp <- c()

    for(s in 1:ntrial){

      ## Simulate control trial and treatment
      x <- rnorm(n, mean = theta_ctr, sd = sigma)
      theta_c_hat <- mean(x)

      ##-------------------------------
      ## Non-informative prior
      ##-------------------------------
      theta_NP <- c(theta_NP, n*sigma^2 *theta_c_hat / (n*sigma^2  + sigma^2))

      ##------------------------
      ## SAM prior
      ##------------------------

      ## Calculate SAM weight
      wSAM <- SAM_weight(if.prior = if.prior,
                         theta.h = theta.h,
                         method.w = method.w, prior.odds = prior.odds,
                         delta = delta, data = x)

      tmp_weight <- c(tmp_weight, wSAM)

      ## Construct SAM prior
      SAM.prior <- SAM_prior(if.prior = if.prior,
                             nf.prior = nf.prior,
                             weight = wSAM, sigma = sigma)

      ## Poerior inference
      SAM.post <- postmix(SAM.prior, data = x)

      theta_SAM <- c(theta_SAM, summary(SAM.post)['mean'])

      ##------------------------
      ## Robust MAP prior
      ##------------------------

      prior.Mix <- robustify(priormix = if.prior, weight = 1 - weight,
                             mean = summary(if.prior)[1], sigma = sigma)

      posterior.Mix <- postmix(prior.Mix, data = x)

      theta_Mix <- c(theta_Mix, summary(posterior.Mix)['mean'])

      ##----------------------------
      ## Compute type I error/power
      ##----------------------------
      ## Simulate treatment trial
      y <- rnorm(n = n.t, mean = theta_trt, sd = sigma)

      ## Posterior of theta
      post_theta_t <- postmix(nf.prior, data = y)
      post_theta_c <- postmix(nf.prior, data = x)

      res_SAM_tmp <- c(res_SAM_tmp, decision(post_theta_t, SAM.post))
      res_Mix_tmp <- c(res_Mix_tmp, decision(post_theta_t, posterior.Mix))
      res_Non_tmp <- c(res_Non_tmp, decision(post_theta_t, post_theta_c))

    }

    ## Store the type I error or power
    res_SAM[i] <- mean(res_SAM_tmp)
    res_Mix[i] <- mean(res_Mix_tmp)
    res_NP[i]  <- mean(res_Non_tmp)

    ## Store the data for bias
    res_Bias[i,1]   <- mean(theta_SAM) - mean(theta_NP)
    res_Bias[i,2]   <- mean(theta_Mix) - mean(theta_NP)

    ## Store the data for RMSE
    res_RMSE[i,1]   <- mse(theta_SAM, theta_ctr) - mse(theta_NP, theta_ctr)
    res_RMSE[i,2]   <- mse(theta_Mix, theta_ctr) - mse(theta_NP, theta_ctr)

    res_weight[i] <- mean(tmp_weight)

  }

  if(if.MAP){

    return(data.frame(scenarios = 1:n_grid,
                      Bias_SAM  = res_Bias[,1],
                      Bias_rMAP = res_Bias[,2],
                      RMSE_SAM  = res_RMSE[,1],
                      RMSE_rMAP = res_RMSE[,2],
                      wSAM      = res_weight,
                      res_SAM   = res_SAM,
                      res_rMAP  = res_Mix,
                      res_NP    = res_NP))

  }else{

    return(data.frame(scenarios = 1:n_grid,
                      Bias_SAM  = res_Bias[,1],
                      RMSE_SAM  = res_RMSE[,1],
                      wSAM      = res_weight,
                      res_SAM   = res_SAM,
                      res_NP    = res_NP
    ))

  }

}
