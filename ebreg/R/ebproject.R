#' Implements the empirical Bayes method in high-dimensional linear model setting for inference and prediction
#'
#' @description The function ebreg implements the method first presented in Martin, Mess, and Walker (2017) for
#' Bayesian inference and variable selection in the high-dimensional sparse linear regression problem.  The
#' chief novelty is the manner in which the prior distribution for the regression coefficients depends on data;
#' more details, with a focus on the prediction problem, are given in Martin and Tang (2019).
#' @author Yiqi Tang
#' @author Ryan Martin
#'
#' @import lars
#' @import stats
#'
#' @param y vector of response variables for regression
#' @param X matrix of predictor variables
#' @param XX vector to predict outcome variable, if pred=TRUE
#' @param standardized logical. If TRUE, the data provided has already been standardized
#' @param alpha numeric value between 0 and 1, likelihood fraction. Default is 0.99
#' @param gam numeric value between 0 and 1, conditional prior precision parameter. Default is 0.005
#' @param sig2 numeric value for error variance. If NULL (default), variance is estimated from data
#' @param prior logical. If TRUE, a prior is used for the error variance
#' @param igpar the parameters for the inverse gamma prior on the error variance. Default is (0.01,4)
#' @param log.f log of the prior for the model size
#' @param M integer value to indicate the Monte Carlo sample size (burn-in of size 0.2 * M automatically added)
#' @param sample.beta logical. If TRUE, samples of beta are obtained
#' @param pred logical. If TRUE, predictions are obtained
#' @param conf.level numeric value between 0 and 1, confidence level for the marginal credible interval if sample.beta=TRUE, and for the prediction interval if pred=TRUE
#'
#' @return A list with components
#' \itemize{
#'  \item beta - matrix with rows containing sampled beta, if sample.beta=TRUE, otherwise NULL
#'  \item beta.mean - vector containing the posterior mean of beta, if sample.beta=TRUE, otherwise NULL
#'  \item ynew - matrix containing predicted responses, if pred=TRUE, otherwise NULL
#'  \item ynew.mean - vector containing the predictions for the predictor values tested, XX, if pred=TRUE, otherwise NULL
#'  \item S - matrix with rows containing the sampled models
#'  \item incl.prob - vector containing inclusion probabilities of the predictors
#'  \item sig2 - estimated error variance, if prior=FALSE, otherwise NULL
#'  \item PI - prediction interval, confidence level specified by the user, if pred=TRUE, otherwise NULL
#'  \item CI - matrix containing marginal credible intervals, confidence level specified by the user, if sample.beta=TRUE, otherwise NULL
#' }
#'
#' @details Consider the classical regression problem
#' \deqn{y = X\beta + \sigma \epsilon,}{y = X\beta + \sigma \epsilon,}
#' where \eqn{y} is a \eqn{n}-vector of responses, \eqn{X} is a \eqn{n \times p}{n x p} matrix of predictor variables,
#' \eqn{\beta} is a \eqn{p}-vector of regression coefficients, \eqn{\sigma > 0} is a scale parameter, and
#' \eqn{\epsilon} is a \eqn{n}-vector of independent and identically distributed standard normal random errors.
#' Here we allow \eqn{p \ge n}{p \ge n} (or even \eqn{p \gg n}{p >> n}) and accommodate the high dimensionality by assuming
#' \eqn{\beta} is sparse in the sense that most of its components are zero.  The approach described in
#' Martin, Mess, and Walker (2017) and in Martin and Tang (2019) starts by decomposing the full \eqn{\beta}
#' vector as a pair \eqn{(S, \beta_S)} where \eqn{S} is a subset of indices \eqn{1,2,\ldots,p} that represents the
#' location of active variables and \eqn{\beta_S} is the \eqn{|S|}-vector of non-zero coefficients.  The approach
#' proceeds by specifying a prior distribution for \eqn{S} and then a conditional prior distribution for
#' \eqn{\beta_S}, given \eqn{S}.  This latter prior distribution here is taken to depend on data, hence "empirical".
#' A prior distribution for \eqn{\sigma^2} can also be introduced, and this option is included in the function.
#'
#' @examples
#' n <- 70
#' p <- 100
#' beta <- rep(1, 5)
#' s0 <- length(beta)
#' sig2 <- 1
#' d <- 1
#' log.f <- function(x) -x * (log(1) + 0.05 * log(p)) + log(x <= n)
#' X <- matrix(rnorm(n * p), nrow=n, ncol=p)
#' X.new <- matrix(rnorm(p), nrow=1, ncol=p)
#' y <- as.numeric(X[, 1:s0] %*% beta[1:s0]) + sqrt(sig2) * rnorm(n)
#'
#' o<-ebreg(y, X, X.new, TRUE, .99, .005, NULL, FALSE, igpar=c(0.01, 4),
#' log.f, M=5000, TRUE, FALSE, .95)
#'
#' incl.pr <- o$incl.prob
#' plot(incl.pr, xlab="Variable Index", ylab="Inclusion Probability", type="h", ylim=c(0,1))
#'
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{martin.mess.walker.eb}{ebreg}
#'
#' \insertRef{martin2019empirical}{ebreg}
#'
#'
#' @export
ebreg <- function(y, X, XX, standardized=TRUE, alpha=0.99, gam=0.005, sig2, prior=TRUE, igpar=c(0.01, 4),
                  log.f, M, sample.beta=FALSE, pred=FALSE, conf.level=.95) {

  n <- nrow(X)
  p <- ncol(X)
  d <- nrow(XX)
  Xbar <- colMeans(X)
  ybar <- mean(y)

  if(!standardized){
    X <- scale(X, center=T, scale=F)
    y <- scale(y, center=T, scale=F)
    XX <- scale(XX, center=Xbar, scale=F)
  }

  o.lars <- lars(X, y, normalize=FALSE, intercept=FALSE, use.Gram=FALSE)
  cv <- cv.lars(X, y, plot.it=T, se=FALSE, normalize=FALSE, intercept=FALSE, use.Gram=FALSE)
  b.lasso <- coef(o.lars, s=cv$index[which.min(cv$cv[1:ceiling(p/2)])], mode="fraction")

  S <- as.numeric(b.lasso != 0)
  X.S <- X[, S > 0]

  if(min(eigen(t(X.S)%*%X.S, only.values=TRUE)$values)<=1e-10){
    S[sample(which(S==1), round(min(sum(S)/2, n/2)))] <- 0
  }

  B <- round(0.2 * M)
  bb <- if(sample.beta) matrix(0, nrow=B + M, ncol=p) else NULL
  bb.mean <- matrix(0, nrow=B + M, ncol=p)
  SS <- matrix(0, nrow=B + M, ncol=p)
  ss <- numeric(B + M)
  ii <- integer(B + M)
  YY <- prediction <- if(pred) matrix(0, nrow=M, ncol=d) else NULL
  level.low <- (1 - conf.level)/2
  level.high <- 1 - (1 - conf.level)/2

  if(prior==TRUE){
    v <- 1 + alpha / gam
    SS[1,] <- S
    ss[1] <- s <- sum(S)
    ii[1] <- i <- iuS <- nuS <- 1
    out <- list()
    out[[1]] <- get.lm.stuff(S, y, X)
    lprior <- log.f(0:n) - lchoose(p, 0:n)
    lpost <- lprior[s+1] - s * log(v) / 2 - (alpha * n / 2 + igpar[1]) * log(igpar[2] + alpha * out[[1]]$sse / 2)
    for(m in 1:(B + M)) {
      S.new <- rprop(S, n)
      s.new <- sum(S.new)
      i.new <- compare.to.rows(as.matrix(SS[iuS,]), S.new)
      if(i.new == 0) o.new <- get.lm.stuff(S.new, y, X) else o.new <- out[[i.new]]
      lpost.new <- lprior[s.new+1] - s.new * log(v) / 2 - (alpha * n / 2 + igpar[1]) * log(igpar[2] + alpha * o.new$sse / 2)
      r <- exp(lpost.new-lpost)
      if(runif(1) <= exp(lpost.new - lpost)) {

        S <- S.new
        s <- s.new
        lpost <- lpost.new
        if(i.new == 0) {

          nuS <- nuS + 1
          i <- nuS
          iuS <- c(iuS, m)
          out[[nuS]] <- o.new

        } else i <- i.new

      }
      SS[m,] <- S
      ss[m] <- s
      ii[m] <- i

      PI  <- NULL
      beta.mean <- NULL
      CI <- NULL

      if(m > B) {

        g.star <- igpar[1] + alpha * n / 2
        h.star <- igpar[2] + alpha * out[[i]]$sse / 2
        if(pred){
          if(sum(S)==0 | is.null(out[[i]]$b.hat)) YY[m - B,] <- sqrt(h.star / g.star) * rt(d, df=2 * g.star)  else {

            if(length(out[[i]]$b.hat)==1) out.mean <- XX[,S>0] * out[[i]]$b.hat else out.mean <- XX[,S > 0] %*% out[[i]]$b.hat
            prediction[m - B,] <- out.mean + ybar
            if(d == 1) mat <- matrix(XX[,S > 0], nrow=1) else mat <- XX[,S > 0]
            out.scale <- diag(d) + mat %*% out[[i]]$U %*% t(out[[i]]$U) %*% t(mat) / (alpha + gam)
            YY[m - B,] <- out.mean + sqrt(h.star / g.star) * chol(out.scale) %*% rt(d, df=2 * g.star) + ybar
          }
        }

      }
      if(sample.beta) {
        g.star <- igpar[1] + alpha * n / 2
        h.star <- igpar[2] + alpha * out[[i]]$sse / 2
        sigma2.samp <- 1/rgamma(1, shape=g.star, scale=h.star)
        sq.v <- sqrt(sigma2.samp/(alpha + gam))

        b <- numeric(p)
        b.mean <- numeric(p)
        b[S > 0] <- out[[i]]$b.hat + sq.v * out[[i]]$U %*% rnorm(s)
        b.mean[S > 0] <- out[[i]]$b.hat
        bb[m,] <- b
        bb.mean[m,] <- b.mean

      }
    }


    if(sample.beta) {
      bb <- bb[-(1:B),]
      beta.mean <- colMeans(bb.mean[-(1:B),])
      CI <- apply(bb, 2, function(x) quantile(x, probs=c(level.low, level.high)))
    }
    if(pred) {
      PI <- apply(YY, 2, function(x) quantile(x, probs=c(level.low, level.high)))
      Yhat <- colMeans(prediction)
    }
    else{Yhat <- NULL}
    incl.prob <- colMeans(SS)
    return(list(beta=bb, beta.mean=beta.mean, ynew=YY, ynew.mean=Yhat, S=SS[-(1:B),], incl.prob=incl.prob,
                sig2 = NULL, PI=PI, CI=CI))
  }

  else{
    if(is.null(sig2)) {
      if (sum(S)==0) {z <- y}
      else{
        z <- as.numeric(y - X[, S > 0] %*% b.lasso[S > 0])
      }
      sig2 <- sum(z**2) / max(n - sum(S), 1)
    }

    v <- 1 + alpha / gam
    sq.v <- sqrt( sig2 / (alpha + gam))

    SS[1,] <- S
    ss[1] <- s <- sum(S)
    ii[1] <- i <- iuS <- nuS <- 1
    out <- list()
    out[[1]] <- get.lm.stuff(S, y, X)
    lprior <- log.f(0:n) - lchoose(p, 0:n)
    lpost <- lprior[s] - alpha * out[[1]]$sse / 2 / sig2 - s * log(v) / 2
    for(m in 1:(B + M)) {

      S.new <- rprop(S, n)
      s.new <- sum(S.new)
      i.new <- compare.to.rows(as.matrix(SS[iuS,]), S.new)
      if(i.new == 0) o.new <- get.lm.stuff(S.new, y, X) else o.new <- out[[i.new]]
      lpost.new <- lprior[s.new] - alpha * o.new$sse / 2 / sig2 - s.new * log(v) / 2
      if(runif(1) <= exp(lpost.new - lpost)) {

        S <- S.new
        s <- s.new
        lpost <- lpost.new
        if(i.new == 0) {

          nuS <- nuS + 1
          i <- nuS
          iuS <- c(iuS, m)
          out[[nuS]] <- o.new

        } else i <- i.new

      }
      SS[m,] <- S
      ss[m] <- s
      ii[m] <- i

      PI  <- NULL
      beta.mean <- NULL
      CI <- NULL

      if(pred){
        if(m > B) {

          out.mean <- XX[,S > 0] %*% out[[i]]$b.hat
          prediction[m - B,] <- XX[,S > 0] %*% out[[i]]$b.hat + ybar
          YY[m - B,] <- out.mean + sq.v * XX[,S > 0] %*% out[[i]]$U %*% rnorm(s) + sqrt(sig2) * rnorm(d) + ybar

        }
      }

      if(sample.beta) {

        b <- numeric(p)
        b.mean <- numeric(p)
        b[S > 0] <- out[[i]]$b.hat + sq.v * out[[i]]$U %*% rnorm(s)
        b.mean[S > 0] <- out[[i]]$b.hat
        bb[m,] <- b
        bb.mean[m,] <- b.mean
      }

    }
    if(sample.beta) {
      bb <- bb[-(1:B),]
      beta.mean <- colMeans(bb.mean[-(1:B),])
      CI <- apply(bb, 2, function(x) quantile(x, probs=c(level.low, level.high)))
    }
    if(pred) {
      PI <- apply(YY, 2, function(x) quantile(x, probs=c(level.low, level.high)))
      Yhat <- colMeans(prediction)
    }
    else{Yhat <- NULL}
    incl.prob <- colMeans(SS)
    return(list(beta=bb, beta.mean=beta.mean, ynew=YY, ynew.mean = Yhat, S=SS[-(1:B),], incl.prob=incl.prob,
                sig2 = sig2, PI=PI, CI=CI))
  }

}


rprop <- function(S, n) {

  s <- sum(S)
  if(s == n) { S[sample(which(S == 1), 1)] <- 0 }
  else if(s  == 1) { S[sample(which(S == 0), 1)] <- 1 }
  else {

    if(runif(1) <= 0.5) S[sample(which(S == 1), 1)] <- 0
    else S[sample(which(S == 0), 1)] <- 1

  }
  return(S)

}


compare.to.rows <- function(SS, S) {
  if (ncol(SS)==1){
    SS <- t(SS)
  }
  numrows <- nrow(SS)
  newS <- matrix(rep(S,numrows), nrow=numrows, byrow=TRUE)
  subtracted <- abs(newS-SS)
  o <- rowSums(subtracted)
  if(all(o > 0)) return(0) else return(which(o == 0)[1])
}



get.lm.stuff <- function(S, y, X) {

  if(sum(S)==0){
    return(list(sse=sum(y**2), b.hat=NULL, U=NULL))
  }
  else{
    X.S <- as.matrix(X[, S > 0])
    o <- lm.fit(X.S, y, singular.ok = T)
    sse <- sum(o$residuals**2)
    b.hat <- o$coefficients
    V <- chol2inv(qr.R(o$qr))
    U <- chol(V)
    return(list(sse=sse, b.hat=b.hat, U=t(U)))

  }
}

