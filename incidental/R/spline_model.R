
#' Initialize spline parameters (beta)
#' 
#' Initialize spline parameters (beta) using a standard Gaussian distribution.
#' @param num_params Integer size of desired parameter vector
#' @return vector of size num_params
#' @export
init_params <- function(num_params) {
  beta <- stats::rnorm(num_params)
  return(beta)
}

#' Create spline basis matrix
#' 
#' This function creates basis matrix for spline model using cubic splines.
#' @param dof An integer degrees of freedom.
#' @param tgrid A grid of time values.
#' @return A matrix of cubic spline basis values with `length(tgrid)` x `dof` entries.
#' @export
make_spline_basis <-function(dof, tgrid){
	res <- dlnm::cr(tgrid, df=dof, intercept=TRUE)
	return(res)
}

#' Make delay likelihood matrix
#'
#' This function creates a matrix such that P[t, s] = P(C = t | I = s) = theta_{t-s} for s <= t and 0 otherwise.
#' 
#' @inheritParams fit_incidence
#' @return A matrix of size n x n
#' @export
make_likelihood_matrix <- function(delay_dist){
	P <- stats::toeplitz(delay_dist)
	P[upper.tri(P, diag=FALSE)] = 0
	return(P)
}

#' Compute log likelihood of incidence model
#'
#' This function computes log likelihood of incidence model given parameters and observations.
#'
#' @param beta parameter vector of num_params
#' @param Q spline basis matrix, of size Tmod x num_params
#' @param Tobs maximum observed time point
#' @return I Tobs-length vector that models log incidence curve
#' @export
compute_log_incidence <- function(beta, Q, Tobs){
	Imod = Q %*% beta
	return(Imod[1:Tobs])
}


#' Compute expected cases
#'
#' This function computes expected cases given incidence curve parameters and a delay distribution. 
#'
#' @param beta parameter vector of num_params
#' @param Q spline basis matrix, of size Tmod x num_params
#' @param lnPmat matrix size Tobs x Tobs, log of make_likelihood_matrix
#' @param Tobs maximum observed time point
#' @return A Tobs-length vector that models expected cases
#' @export
compute_expected_cases <- function(beta, Q, lnPmat, Tobs){
  # compute incidence from parameters
  lnI = compute_log_incidence(beta, Q, Tobs)
  # compute marginal likleihood from incidence and delay dist
  lnf_tilde = t(lnI + t(lnPmat))
  lnf = matrixStats::rowLogSumExps(lnf_tilde, margin=1)
  return(exp(lnf))
}

#' Marginal log likelihood 
#
#' This function computes the marginal probability of Pr(reported | beta).  Note that
#' lnPmat must be zero padded enough (or censored) to match the length of 
#' reported cases vector.
#'
#' @inheritParams fit_incidence
#' @param beta spline parameter vector length num_params
#' @param Q spline basis matrix Tmod x num_params
#' @param lnPmat matrix size Tobs x Tobs, log of make_likelihood_matrix
#' @return A scalar log likelihood value.
#' @export
marg_loglike_poisson <- function(beta, reported, Q, lnPmat){

  # make sure beta doesn't creep above exp(20)
  beta = pmax(-20, pmin(beta, 20))

  # compute incidence from parameters
  Tobs = length(reported)
  lnI = compute_log_incidence(beta, Q, Tobs)

  # compute marginal likleihood from incidence and delay dist
  lnf_tilde = t(lnI + t(lnPmat))
  lnf = matrixStats::rowLogSumExps(lnf_tilde, margin=1)

  # apply poisson loglike
  ll <- stats::dpois(reported, exp(lnf), log=T)
  #ll <- reported*lnf - exp(lnf)
  return(sum(ll))
}

#' Marginal log likelihood gradient
#'
#' This function computes the gradient of the log likelihood term with respect to beta.
#'
#' @inheritParams fit_incidence
#' @param beta spline parameter vector length num_params
#' @param Q spline basis matrix Tmod x num_params
#' @param lnPmat matrix size Tobs x Tobs, log of make_likelihood_matrix
#' @return A numeric vector, gradient of log likelihood value with respect to beta.
#' @export
marg_loglike_poisson_grad <- function(beta, reported, Q, lnPmat){
    # forward pass
    # make sure beta doesn't creep above exp(20)
    beta = pmax(-20, pmin(beta, 20))
    # compute incidence from parameters
    Tobs = length(reported)
    lnI = compute_log_incidence(beta, Q, Tobs)
    # compute marginal likleihood from incidence and delay dist
    lnf_tilde = t(lnI + t(lnPmat))
    lnf = matrixStats::rowLogSumExps(lnf_tilde, margin=1)
    # backward pass
    dloss_dlnf  = reported - exp(lnf)
    dlnf_dlnI   = t(exp(lnf_tilde - lnf))
    dlnI_dbeta  = t(Q)[,1:Tobs]
    dloss_dbeta = dlnI_dbeta %*% dlnf_dlnI %*% dloss_dlnf
    return(unname(dloss_dbeta[,1]))
}

#' Marginal log likelihood Fisher information matrix
#'
#' This function computes the Fisher information matrix log likelihood term with
#' respect to beta.
#'
#' @inheritParams fit_incidence
#' @param beta A spline parameter vector length num_params.
#' @param Q A spline basis matrix Tmod x num_params.
#' @param lnPmat A matrix size Tobs x Tobs, log of make_likelihood_matrix.
#' @return A numeric vector, gradient of log likelihood value with respect to beta.
#' @export
marg_loglike_poisson_fisher <- function(beta, reported, Q, lnPmat){
    beta = pmax(-20, pmin(beta, 20))
    # compute incidence from parameters
    Tobs = length(reported)
    lnI = compute_log_incidence(beta, Q, Tobs)
    # compute marginal likleihood from incidence and delay dist
    lnf_tilde = t(lnI + t(lnPmat))
    lnf = matrixStats::rowLogSumExps(lnf_tilde, margin=1)
    # backward pass
    dloss_dlnf  = reported - exp(lnf)
    dlnf_dlnI   = t(exp(lnf_tilde - lnf))
    dlnI_dbeta  = t(Q)[,1:Tobs]
    A = dlnI_dbeta %*% dlnf_dlnI
    D = diag(exp(lnf))
    fisher_info_matrix = A %*% D %*% t(A)
    return(fisher_info_matrix)
}

#' Beta regularization function
#'
#' This function computes regularization penalty term based on the betas and a difference.
#'
#' @inheritParams fit_incidence
#' @param beta A spline parameter vector length num_params.
#' @return A scalar regularization value.
#' @export
regfun <- function(beta, regularization_order=2){
  if(regularization_order == 0){ return(sum(beta**2)) }
  beta_diff = diff(beta, differences=regularization_order)
  return(sum(beta_diff**2))
}

#' Beta regularization function gradient
#'
#' This function computes regularization penalty term gradient based on the betas and difference order.
#'
#' @inheritParams fit_incidence
#' @param beta spline parameter vector length num_params
#' @return scalar regularization value
#' @export
regfun_grad <- function(beta, regularization_order=2){
    if(regularization_order==0){ return(2*beta) }
    if(regularization_order==1){
        beta_diff = 2*diff(beta, differences=regularization_order)
        dfwd = c(rep(0, regularization_order), beta_diff)
        dbwd = c(beta_diff, rep(0, regularization_order))
        return(diff_trans(beta_diff))
    }
    if (regularization_order==2) {
        beta_diff = 2*diff(beta, differences=regularization_order)
        return(diff_trans(diff_trans(beta_diff)))
    }
    stop("regularization_order not implemented")
}

#' Transpose of the 1st difference operator
#'
#' This function computes a transpose of the 1st difference operator. 
#' @param a A vector of inputs
#' @return The transpose of the first difference operator
diff_trans <- function(a) {
    return(c(-a[1], -diff(a, differences=1), a[length(a)]))
}

#' Beta regularization function Hessian
#'
#' This function computes regularization penalty term Hessian based on the betas and differencing order.
#'
#' @param regularization_order An integer (typically 0, 1, 2), indicating differencing order for L2 
#'        regularization of spline parameters. Default is 2 for second derivative penalty.
#' @param beta spline parameter vector length num_params
#' @return scalar regularization value
#' @export
regfun_hess <- function(beta, regularization_order=2){
    # numeric derivative --- add jitter for PSD 
    H = numDeriv::hessian(function(beta){ regfun(beta, regularization_order) }, beta)
    H[abs(H) < 1e-6] = 0
    H = H + diag(nrow(H))*1e-6
    return(H)
}

#' Poisson objective function 
#'
#' This function computes Poisson objective function including regularizer.
#'
#' @inheritParams fit_incidence
#' @param beta spline parameter vector length num_params
#' @param lam positive scalar regularization strength
#' @param Q spline basis matrix Tmod x num_params
#' @param lnPmat matrix size Tobs x Tobs, log of make_likelihood_matrix
#' @return scalar objective function value
#' @export
poisson_objective <- function(beta, lam, reported, Q, lnPmat, regularization_order){
  Nobs = sum(reported)
  ll = marg_loglike_poisson(beta, reported, Q, lnPmat)
  rr = lam*regfun(beta, regularization_order)
  val = -1*ll/Nobs + rr
  return(val)
}

#' Poisson objective function gradient
#' 
#' This function computes the Poisson objective function (including regularizer) gradient.
#'
#' @inheritParams fit_incidence
#' @param beta spline parameter vector length num_params
#' @param lam positive scalar regularization strength
#' @param Q spline basis matrix Tmod x num_params
#' @param lnPmat matrix size Tobs x Tobs, log of make_likelihood_matrix
#' @return scalar objective function value
#' @export
poisson_objective_grad <- function(beta, lam, reported, Q, lnPmat, regularization_order){
  Nobs = sum(reported)
  dll_dbeta = marg_loglike_poisson_grad(beta, reported, Q, lnPmat)
  drr_dbeta = lam*regfun_grad(beta, regularization_order)
  dval_dbeta = -1*dll_dbeta/Nobs + drr_dbeta
  return(dval_dbeta)
}

#' Compute Fisher information matrix for Poisson objective
#' 
#' This function computes the Fisher information matrix for a regularized Poisson objective function.
#' 
#' @param beta A vector of spline parameters.
#' @param lam A regularization penalty parameter.
#' @param reported A vector of reported values.
#' @param Q A spline basis matrix.
#' @param lnPmat A matrix size Tobs x Tobs, log of make_likelihood_matrix.
#' @param regularization_order An integer that specifies the regularization order.
#' @return Fisher information matrix of a regularized Poisson objective function.
poisson_objective_post_cov_approx <- function(beta, lam, reported, Q, lnPmat, regularization_order){
  # fisher approximates observation evidence covariance
  ll_fish  = marg_loglike_poisson_fisher(beta, reported, Q, lnPmat)
  # regularization is equivalent to a normal prior with precision a function of 
  # the hessian (scaled up b/c the solution was scaled down)
  rr_prior = lam*regfun_hess(beta, regularization_order)*sum(reported)
  # cov approx is 
  cov_approx = solve(rr_prior + ll_fish)
  return(cov_approx)
}


#' Train and validate model on reported data
#'
#' This function fit models with selected hyperparameters on reported data and return a matrix of posterior Laplace samples.
#' 
#' @inheritParams fit_incidence
#' @inheritParams scan_spline_lam
#' @inheritParams scan_spline_dof
#' @param beta0 (optional) Initial setting of spline parameters (before optimization)
#' @return A list of results of train and validate, including: \itemize{ 
#'	\item train_ll = training log likelihood
#'	\item val_ll = validation log likelihood (if `reported_val` is not `NULL`)
#'  \item Isamps = samples of the incidence curve from a Laplace approximation
#'	\item Ihat = MAP estimate of the incidence curve
#' 	\item Chat = expected cases given MAP incidence curve
#'	\item beta_hat = MAP estimate of spline parameters
#'	\item beta_cov = covariance of spline parameters
#'	\item beta_hess = Hessian of spline parameters}
#' @export
train_and_validate <- function(
    reported,
    delay_dist,
    lam,
    dof,
    beta0=NULL,
    regularization_order=2,
    reported_val=NULL,
    end_pad_size=0,
    fisher_approx_cov=TRUE,
    num_samps_per_ar = 10) {

    # set up optimization objective
    Tobs = length(reported)
    Tmod = Tobs + end_pad_size

    # make sure delay distribution is equal to reported
    if(length(delay_dist) > length(reported)) {
        delay_dist = delay_dist[1:Tobs]
    } else if(length(delay_dist) < length(reported)) {
        size_diff = Tobs - length(delay_dist)
        delay_dist = c(delay_dist, rep(0, size_diff))
    }

    # set up objective function
    Q    = make_spline_basis(dof, 1:Tmod)
    Pmat = make_likelihood_matrix(delay_dist)
    lnPmat = log(Pmat)
    obj_fun = function(beta){
        poisson_objective(beta, lam, reported, Q, lnPmat, regularization_order)
    }
    obj_fun_grad = function(beta){
        poisson_objective_grad(beta, lam, reported, Q, lnPmat, regularization_order)
    }

    # optimize
    if(is.null(beta0))
        beta0 = init_params(dof)
    res = stats::optim(
      beta0, 
      obj_fun,
      gr=obj_fun_grad,
      method='L-BFGS-B',
      hessian=TRUE,
      control=list(maxit=100000, factr=1e7, pgtol=1e-8))
    beta_hat = res$par

    # posterior approximation to the covariance
    if(fisher_approx_cov) {
      fisherCov= poisson_objective_post_cov_approx(beta_hat, lam, reported, Q, lnPmat, regularization_order)
      beta_cov = fisherCov
    } else {
      lapCov   = solve(res$hessian) / sum(reported)
      beta_cov = lapCov
    }

    # compute train and validation statistics
    train_ll = marg_loglike_poisson(beta_hat, reported, Q, lnPmat)

    # validation -- if passed in compute validation stats
    val_ll = NaN
    if(!is.null(reported_val)){
        val_ll = marg_loglike_poisson(beta_hat, reported_val, Q, lnPmat)
    }

    # laplace samples of incidence curve
    logI_samps = sample_laplace_log_incidence_poisson(
        beta_hat, beta_cov, reported, Q, num_samps_per_ar)
    Isamps = exp(logI_samps)
    Ihat  = exp(compute_log_incidence(beta_hat, Q, Tobs))

    # push forward to show expected cases
    Chat = compute_expected_cases(beta_hat, Q, lnPmat, Tobs)

    # return model info
    return(list(train_ll = train_ll,
                val_ll   = val_ll,
                Isamps   = Isamps,
                Ihat     = Ihat,
                Chat     = Chat,
                beta_hat = beta_hat,
                beta_hess= res$hessian,
                beta_cov = beta_cov))
}

#' Generate Laplace samples of incidence
#' 
#' This function generates Laplace samples of posterior distribution for a vector of reported incidence.
#'
#' @inheritParams fit_incidence
#' @param beta_hat Maximum likelihood solution for beta parameter.
#' @param beta_cov Covariance of objective solution (either Fisher information or Hessian inverse).
#' @param Q Spline basis matrix.
#' @param num_samps_per_ar Number of Laplace samples to return for each AR path.
#' @return A matrix of `num_samps_per_ar` log incidence curve samples from laplace approximation of distribution.
#' @export
sample_laplace_log_incidence_poisson <- function(
    beta_hat,
    beta_cov,
    reported,
    Q,
    num_samps_per_ar=10) {

    # rescale laplace covariance matrix
    lapCov = beta_cov

    # check to make sure eigenvalues are positive
    Tobs = length(reported)
    eigres = eigen(lapCov)
    if(any(eigres$values < 0)) {
        warning("bad laplace hessian --- returning mean!")
        logI_samps = t(compute_log_incidence(beta_hat, Q, Tobs))
        return(logI_samps)
    }

    # generate spline parameter samples
    beta_samps = MASS::mvrnorm(n=num_samps_per_ar, mu=beta_hat, Sig=lapCov)

    # convert beta samples into log incidence samples
    Tobs = length(reported)
    logI_samps = apply(beta_samps, 1,
                       function(bs) { compute_log_incidence(bs, Q, Tobs) })

    # return num_samps x Tobs matrix
    return(t(logI_samps))
}


#' Scan spline degrees of freedom
#' 
#' This function holds the regularization parameter value fixed and scans spline degrees of freedom.
#'
#' @inheritParams fit_incidence
#' @inheritParams scan_spline_lam
#' @param lam A fixed value for the beta parameter regularization strength.
#' @return A list of degree of freedom fit statistics: \itemize{
#'	\item best_dof =  best degrees of freedom
#'	\item dof_resdf = data frame of fit statistics (lambda, dof, aic, bic, val_lls, train_lls)}
#' @export
scan_spline_dof <- function(
    reported,
    delay_dist,
    dof_grid,
    method = "bic",
    lam=0,
    regularization_order=2,
    reported_val=NULL,
    end_pad_size=0,
    fisher_approx_cov=FALSE) {

    # return singular value if grid
    if(length(dof_grid) == 1){
        return(list(best_dof=dof_grid[1], dof_resdf=NULL))
    }

    # check validation method
    valid_methods = c("aic", "bic", "val")
    if( !(method %in% valid_methods) ){
        stop(sprintf("scan dof method '%s' not in list %s",
                     method, paste(valid_methods, collapse="")))
    }
    if( (method == "val") & (is.null(reported_val))){
        stop("scan dof method 'val' needs 'reported_val' to be non NULL")
    }

    # scan each DoF value
    res_list = lapply(dof_grid,
        function(dof) {
            res = train_and_validate(reported, delay_dist, lam, dof,
                                     reported_val=reported_val,
                                     regularization_order=regularization_order,
                                     end_pad_size=end_pad_size,
                                     fisher_approx_cov=fisher_approx_cov)
            res$aic = 2*(dof+1)-2*res$train_ll
            res$bic = log(sum(reported))*dof - 2*res$train_ll
            return(res)
        })

    # choose best dof
    aics = unlist(lapply(res_list, function(x){x$aic}))
    bics = unlist(lapply(res_list, function(x){x$bic}))
    vals = unlist(lapply(res_list, function(x){x$val_ll}))
    if(method=="aic"){
        best_dof = dof_grid[ which.min(aics) ]
    } else if(method=="bic"){
        best_dof = dof_grid[ which.min(bics) ]
    } else if(method=="val"){
        best_dof = dof_grid[ which.max(vals) ]
    }

    # results for monitoring fit statistics
    dof_resdf = data.frame(
        lambda    = lam,
        dof       = dof_grid,
        aic       = aics,
        bic       = bics,
        val_lls   = vals,
        train_lls = unlist(lapply(res_list, function(x){x$train_ll}))
    )

    # return best AIC and statistics
    return(list(best_dof  = best_dof,
                dof_resdf = dof_resdf))
}


#' Scan spline regularization parameter
#'
#' This function holds degrees of freedom fixed and scans regularization parameter values.
#'
#' @inheritParams fit_incidence
#' @param dof Degrees of freedom for spline basis.
#' @param method Metric to choose "best" dof: 'aic', 'bic', 'val'.
#'        If method='val', reported_val must be non NULL and match reported size.
#' @param reported_val Validation time series of equal size
#'        to reported vector for use with 'val' method. Default is NULL.
#' @return List of outputs:\itemize{
#'	\item best_lam = best lambda
#'	\item lam_resdf = data frame of fit statistics (lambda, dof, aic, bic, val_lls, train_lls)}
#' @export
scan_spline_lam <- function(
    reported,
    delay_dist,
    lam_grid,
    method = "val",
    percent_thresh=2,
    dof    = 10,
    regularization_order=2,
    reported_val=NULL,
    end_pad_size=0,
    fisher_approx_cov=TRUE) {

    # return singular value if grid
    if(length(lam_grid) == 1){
        return(list(best_lam=lam_grid[1], lam_resdf=NULL))
    }

    # make sure lam_grid is decreasing --- stronger values first
    lam_grid = sort(lam_grid, decreasing=TRUE)

    # check validation method
    valid_methods = c("aic", "bic", "val")
    if( !(method %in% valid_methods) ){
        stop(sprintf("scan lam method '%s' not in list %s",
                     method, paste(valid_methods, collapse="")))
    }
    if( (method == "val") & (is.null(reported_val))){
        stop("scan lam method 'val' needs 'reported_val' to be non NULL")
    }

    # scan each DoF value
    beta0 = init_params(dof)
    res_list = lapply(lam_grid,
        function(lam) {
            res = train_and_validate(reported, delay_dist, lam, dof,
                                     reported_val=reported_val,
                                     beta0 = beta0,
                                     regularization_order=regularization_order,
                                     end_pad_size=end_pad_size,
                                     fisher_approx_cov=fisher_approx_cov)
            res$aic = 2*(dof+1)-2*res$train_ll
            res$bic = log(sum(reported))*dof - 2*res$train_ll
            return(res)
        })

    # choose best lam
    aics = unlist(lapply(res_list, function(x){x$aic}))
    bics = unlist(lapply(res_list, function(x){x$bic}))
    vals = unlist(lapply(res_list, function(x){x$val_ll}))
    if(method=="aic"){
        best_lam = lam_grid[ which.min(aics) ]
    } else if(method=="bic"){
        best_lam = lam_grid[ which.min(bics) ]
    } else if(method=="val"){
        percent_within_max = 100*abs(max(vals)-vals)/abs(max(vals))
        best_lam = lam_grid[ which.max(percent_within_max <= percent_thresh) ]
    }

    # results for monitoring fit statistics
    lam_resdf = data.frame(
        lam       = lam_grid,
        dof       = dof,
        aic       = aics,
        bic       = bics,
        val_lls   = vals,
        train_lls = unlist(lapply(res_list, function(x){x$train_ll}))
    )

    # return best lambda and statistics
    return(list(best_lam  = best_lam,
                lam_resdf = lam_resdf))
}

