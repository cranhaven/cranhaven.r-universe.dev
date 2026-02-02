#' Projection-based method for testing latent means equality
#'
#' Perform projection method for testing the equality of latent means without requiring the equality of cross-group intercepts to hold.
#'
#' @param ... The same arguments as for any lavaan model. See \code{lavaan::sem} for more information. All models fitted by Lavaan are estimated by fixing the variances of latent factors to 1.
#'
#' Users must explicitly specify the name of the input elements for this function to catch. For example, specify 'data = HolzingerSwineford' instead just 'HolzingerSwineford'.
#' @return A list is returned with:
#' \describe{
#' \item{\code{fit.metric}}{test of metric invariance (factor loadings). This is a prerequisite for testing equality of latent means.}
#' \item{\code{mvdif.test}}{t tests of the cross-group sample means for each variable.}
#' \item{\code{chi.stat}}{Three chi-square tests for intercepts, common factors, and specific factors. \code{chi.stat} will be needed for equivalence testing.}
#' \item{\code{common.test}}{t tests of common factors for each variable.}
#' \item{\code{specific.test}}{t tests of specific factors for each variable.}
#' \item{\code{latent.test}}{t tests of latent means}
#' \item{\code{V.index}}{validity index}
#' \item{\code{Pmat}}{projection matrix of intercepts into the space of common factors}
#' \item{\code{Qmat}}{projection matrix of intercepts into the space of specific factors}
#' }
#' @details Perform projection method for testing the equality of two latent means without requiring the cross-group intercepts to be the same. A validity index is provided as the proportion of the differences in manifest variables intercepts explained by latent mean differences as a gauge of the quality of measurements.
#' @references Yuan, K. H., & Chan, W. (2016). Measurement invariance via multigroup SEM: Issues and solutions with chi-square-difference tests. Psychological methods, 21(3), 405-426.
#' @export
#' @importFrom lavaan sem
#' @importFrom lavaan partable
#' @importFrom lavaan lavTech
#' @examples
#' data(HolzingerSwineford)
#' semmodel<-'
#' L1 =~ V1 + V2 + V3
#' L2 =~ V4 + V5 + V6
#' L3 =~ V7 + V8
#' L4 =~ V9 + V10 + V11
#' '
#' run.proj <- eqMI.projection(model = semmodel, data = HolzingerSwineford,
#'           group = "school", meanstructure = TRUE)
#'
eqMI.projection <- function(...) {
  dotdotdot <- list(...)

  #obtain sample means
  if(is.null(dotdotdot$sample.mean) & is.null(dotdotdot$data))
    stop("sample means must be provided for projection method")
  if (!is.null(dotdotdot$sample.mean)){
    sample.means <- dotdotdot$sample.mean
    sample.nobs <- dotdotdot$sample.nobs
  } else {
    group.ind <- match(dotdotdot$group, colnames(dotdotdot$data))
    dat.by.group <- split(dotdotdot$data[,-group.ind], f = dotdotdot$data[,group.ind])
    sample.means <- lapply(dat.by.group, colMeans)
    sample.nobs <- sapply(dat.by.group, nrow)
  }
  if (length(sample.means)!=2)
    stop("projection method only applies to two groups with this function")

  #fit model to data for the common loading matrix
  dotdotdot$group.equal <- c("loadings")
  lavaansem <- function(...) { lavaan::sem(...)}
  fit.metric <- do.call(lavaansem, dotdotdot) #output 1
  pttemplate <- lavaan::partable(fit.metric)
  varnames <- unique(pttemplate$rhs[pttemplate$op == "=~"])
  facnames <- unique(pttemplate$lhs[(pttemplate$op == "=~") & (pttemplate$rhs %in% varnames)])

  xbar_1 <- sample.means[[1]]
  xbar_2 <- sample.means[[2]]
  xbar_d <- xbar_2 - xbar_1
  #sigs <- lavTech(fit.metric, 'cov.ov')
  sigs <- list(lavTech(fit.metric, 'sampstat')[[1]]$cov*sample.nobs[[1]]/(sample.nobs[[1]]-1), lavTech(fit.metric, 'sampstat')[[2]]$cov*sample.nobs[[2]]/(sample.nobs[[2]]-1))
  omega <- sigs[[1]]/sample.nobs[[1]] + sigs[[2]]/sample.nobs[[2]]
  SD_1 <- sqrt(diag(sigs[[1]]))
  SD_2 <- sqrt(diag(sigs[[2]]))
  SE_d <- sqrt(diag(omega))

  lamb <- lavTech(fit.metric,what="est")$lambda
  temp <- qr(lamb)
  q_mat <- qr.Q(temp, complete = TRUE)
  q_mat <- -q_mat
  hlamb_1 <- q_mat[,1:length(facnames)]
  hlamb_t1 <- t(hlamb_1)
  lamb_or <- q_mat[,-(1:length(facnames))]
  lamb_ot <- t(lamb_or)

  #compute statistics
  xbar_d1 <- hlamb_t1%*%xbar_d
  xbar_d2 <- lamb_ot%*%xbar_d
  sig_d1 <- hlamb_t1%*%omega%*%hlamb_1
  sig_d2 <- lamb_ot%*%omega%*%lamb_or

  oT_mean1 <- t(xbar_d1)%*%solve(sig_d1)%*%xbar_d1	#for tau_1 <- tau_2
  oT_mean2 <- t(xbar_d2)%*%solve(sig_d2)%*%xbar_d2	#for nu_1 <- nu_2
  oT_mean3 <- t(xbar_d)%*%solve(omega)%*%xbar_d	#for mu_1 <- mu_2
  oT_all <- c(fit.mvmean=oT_mean3,fit.common=oT_mean1,fit.specific=oT_mean2)	#remember for bootstrap purpose
  df_tau <- length(facnames)
  df_mu <- length(varnames)
  df_nu <- df_mu - df_tau
  pv_1 <- 1-pchisq(oT_mean1,df_nu)
  pv_2 <- 1-pchisq(oT_mean2,df_tau)
  pv_3 <- 1-pchisq(oT_mean3,df_mu)

  #output 2: tests of means of observed variables
  mvdif.test <- data.frame(y_1 = xbar_1, y_2 = xbar_2, y_d=xbar_d, SE_d=SE_d, z_d=(xbar_d/SE_d))
  #output 3: chisquare tests of cross-group means, common scores, and specific scores
  proj.stat <- data.frame(Chisq=oT_all, Df=c(df_mu, df_tau, df_nu), pvalue=c(pv_3, pv_1, pv_2))

  #decomposition of sample means on common and specific factors under new setup
  Pmat_hlamb <- lamb%*%solve(t(lamb)%*%lamb)%*%(t(lamb))
  Qmat_hlamb <- diag(df_mu)-Pmat_hlamb
  Pmat_htau <- solve(t(lamb)%*%lamb)%*%(t(lamb))

  hmu1_lamb <- Pmat_hlamb%*%xbar_1
  hmu2_lamb <- Pmat_hlamb%*%xbar_2
  hmud_lamb <- hmu2_lamb-hmu1_lamb
  sigd_lamb <- Pmat_hlamb%*%omega%*%Pmat_hlamb
  SEd_lamb <- sqrt(diag(sigd_lamb))
  z_lamb <- hmud_lamb/SEd_lamb
  all_lamb <- data.frame(common_1=hmu1_lamb, common_2=hmu2_lamb,
                         common_d=hmud_lamb, SE_d=SEd_lamb, z_d=z_lamb)
  rownames(all_lamb) <- varnames

  hmu1_nu <- Qmat_hlamb%*%xbar_1
  hmu2_nu <- Qmat_hlamb%*%xbar_2
  hmud_nu <- hmu2_nu-hmu1_nu
  sigd_nu <- Qmat_hlamb%*%omega%*%Qmat_hlamb
  SEd_nu <- sqrt(diag(sigd_nu))
  z_nu <- hmud_nu/SEd_nu
  all_nu <- data.frame(specific_1=hmu1_nu, specific_2=hmu2_nu,
                       specific_d=hmud_nu, SE_d=SEd_nu, z_d=z_nu)
  rownames(all_nu) <- varnames

  htau_1 <- Pmat_htau%*%xbar_1
  htau_2 <- Pmat_htau%*%xbar_2
  htau_d <- htau_2-htau_1
  sigd_tau <- Pmat_htau%*%omega%*%(t(Pmat_htau))
  SEd_tau <- sqrt(diag(sigd_tau))
  z_tau <- htau_d/SEd_tau
  all_tau <- data.frame(latent_1=htau_1, latent_2=htau_2,
                        latent_d=htau_d, SE_d=SEd_tau, z_d=z_tau)
  rownames(all_tau) <- facnames

  #validity index
  tautau = t(hmud_lamb)%*%hmud_lamb
  nunu = t(hmud_nu)%*%hmud_nu
  ratio = tautau/(nunu+tautau)

  return(list(fit.metric = fit.metric, mvdif.test = mvdif.test, chi.stat = proj.stat, common.test = all_lamb, specific.test = all_nu, latent.test = all_tau, V.index = ratio, Pmat = Pmat_hlamb, Qmat = Qmat_hlamb))

}


#' Bootstrap procedure to test the equality of latent factor means using projection method
#'
#' @param ... The same arguments as for any lavaan model. See \code{lavaan::sem} for more information.
#' @param bootstrap If bootstrap resampling is used to obtain empirical p-value of the statistics.
#' @param B The number of bootstrap samples. Default at 100.
#' @param seed The initial seed to generate bootstrap samples. Default at 111.
#' @details Perform bootstrap procedure when testing the equality of latent means using projection method. Note that raw data must be available for bootstrap resampling to be performed. With the projection method, the cross-group intercepts are not required to be the same for further tests. If bootstrap resampling is used, the test statistics are not referred to chi-squared distributions but to bootstrapped empirical distributions for significance testing. Percentage bootstrap critical values are calculated. This process might be time-consuming if the model is complex or the number of bootstrap samples (B) is large.
#' @return bootstrap p-values of the tests of common and specific factors.
#' @references Yuan, K. H., & Chan, W. (2016). Measurement invariance via multigroup SEM: Issues and solutions with chi-square-difference tests. Psychological methods, 21(3), 405-426.
#' @importFrom stats pchisq
#' @importFrom lavaan lavTech
#' @importFrom stats runif
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @export
#' @examples
#' data(HolzingerSwineford)
#' semmodel<-'
#' L1 =~ V1 + V2 + V3
#' L2 =~ V4 + V5 + V6
#' L3 =~ V7 + V8
#' L4 =~ V9 + V10 + V11
#' '
#' \donttest{
#' run.bts <- eqMI.bootstrap(model = semmodel, data = HolzingerSwineford,
#'           group = "school", meanstructure = TRUE, B = 100, seed = 111)
#'}

eqMI.bootstrap <- function(..., B = 100, seed = 111) {
  dotdotdot <- list(...)

  if (is.null(dotdotdot$data)) {
    stop("bootstrap resampling can be performed only when raw data are available. provide raw data or turn bootstrap off by setting bootstrap=FALSE")}
  if (is.null(dotdotdot$group)) {
    stop("grouping variable must be pre-specified")}

  group.ind <- match(dotdotdot$group, colnames(dotdotdot$data))
  dat.by.group <- split(dotdotdot$data, f = dotdotdot$data[,group.ind])
  sample.means <- sapply(dat.by.group, colMeans)[-group.ind,]
  sample.nobs <- sapply(dat.by.group, nrow)

  if (length(sample.nobs)!=2) {
    stop("projection method only applies to two groups with this function")}

  message('bootstrap resampling...', '\n')

  proj.res <- do.call(eqMI.projection, dotdotdot)
  fit.metric <- proj.res$fit.metric

  temp1 <- eigen(lavTech(fit.metric,what="sampstat")$`1`$cov)
  temp2 <- eigen(lavTech(fit.metric,what="sampstat")$`2`$cov)
  S1_inh <- (temp1$vector) %*% diag(1/sqrt(temp1$values)) %*% t(temp1$vector)
  S2_inh <- (temp2$vector) %*% diag(1/sqrt(temp2$values)) %*% t(temp2$vector)
  sigs <- lavTech(fit.metric, 'cov.ov')
  temp11 <- eigen(sigs[[1]])
  temp22 <- eigen(sigs[[2]])
  Sig1_h <- (temp11$vector) %*% diag(sqrt(temp11$values)) %*% t(temp11$vector)
  Sig2_h <- (temp22$vector) %*% diag(sqrt(temp22$values)) %*% t(temp22$vector)

  y_bar <- apply(apply(sample.means, 1, '*', sample.nobs), 2, sum)/sum(sample.nobs)
  y01 <- Sig1_h %*% S1_inh %*% (t(dat.by.group[[1]][,-group.ind])-sample.means[,1]) + y_bar
  y02 <- Sig2_h %*% S2_inh %*% (t(dat.by.group[[2]][,-group.ind])-sample.means[,2]) + y_bar

  set.seed(seed)
  seed.bank <- round(runif(B, 1, 1E+6))
  stat_b <- matrix(NA, B, 2)
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  for (b in 1:B){
    Sys.sleep(0.01)
    setTxtProgressBar(pb, b)

    set.seed(seed.bank[b])
    y_b01 <- t(y01)[sample(1:sample.nobs[[1]], sample.nobs[[1]], replace = T), ]
    y_b02 <- t(y02)[sample(1:sample.nobs[[2]], sample.nobs[[2]], replace = T), ]

    dat_b <- dotdotdot$data
    dat_b[,-group.ind] <- rbind(y_b01, y_b02)
    dotdotdot$data <- dat_b

    proj_b <- do.call(eqMI.projection, c(dotdotdot))
    stat_b[b, ] <- proj_b$chi.stat[2:3,1]
  }
  message('\n')
  stat.st <- apply(stat_b, 2, sort)
  pval1 <- 1 - findInterval(proj.res$chi.stat[2,1], stat.st[,1])/B
  pval2 <- 1 - findInterval(proj.res$chi.stat[3,1], stat.st[,2])/B

  return(boot.pval = cbind(pval1, pval2))
}
