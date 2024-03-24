
#' Covariance matrix of covariate change
#'
#' Given the covariance matrix of parameters, it return the covariance
#' matrix associated to the change in covariate values in the space
#' of covariantes.
#'
#' @param vcovB variance-covariance matrix of \eqn{vec(B)}.
#' @param vdelta numeric vector of covariate change.
#'
#' @returns Squared numeric matrix of order two.
#'
#' @keywords internal
vcovB2vcovDeltaB <- function(vcovB, vdelta) {
  if (!is.vector(vdelta)) { vdelta %<>% as.vector}
  x <- kronecker(diag(2), vdelta)
  return(crossprod(x, vcovB) %*% x)
}



#' It computes the confidence region in the ternary space
#'
#' Given the parameters of the confidence ellipse in the space of covariate
#' coefficients, it returns the confidence region of the effect.
#'
#' @param mu centre of the ellipse.
#' @param Sig covariance matrix of the ellipse.
#' @param conf confidence level of the region.
#' @param npoints number of points of the border.
#'
#' @returns `data.frame` with three columns (named `p1`, `p2`, and `p3`)
#' with ternary coordinates of the points of the ellipse.
#'
#' @keywords internal
confregion <- function(mu, Sig, conf = 0.95, npoints = 100) {
  # Compute the ellipse
  ellipse::ellipse(x = Sig, centre = mu, level = conf) %>%
    # Compute the ternary coordinates
    linkinv_cat3logit %>%
    # Prepare the output
    data.frame %>%
    set_colnames(c('p1', 'p2', 'p3')) %>%
    return()
}





#' It adds the confidence regions to a "field3logit" object
#'
#' Given the confidence level, it computes the confidence regions
#' the effects for each arrow of the `field3logit` object.
#'
#' @inheritParams confregion
#' @param x an object of class `field3logit`.
#'
#' @returns Object of class `field3logit` with updated confidence regions.
#'
#' @keywords internal
add_confregions_field3logit <- function(x, conf = 0.95, npoints = 100) {
  # Check the input
  if (x$ordinal) {
  	warning('Confidence regions are not available for ordinal models.')
  	x$conf <- NA
  	return(x)
  }
  
  # Compute the covariance matrix of the ellipse
  SigMa <- vcovB2vcovDeltaB(x$vcovB, x$vdelta)
  
  # Compute the confidence regions
  x$effects %<>%
    lapply(function(w) lapply(w, function(y) {
    	  linkfun(y$to, x) %>%
        confregion(SigMa) %>%
        set_colnames(x$levels) -> y$confregion
        
      return(y)
    }))
  
  # Update other object attributes
  x$conf <- conf
  
  # Return the updated object
  return(x)
}





#' Compute the confidence regions of covariate effects
#'
#' Given the confidence level, it computes the confidence regions of the effects
#' for each arrow of the `field3logit` or `multifield3logit` object given in
#' input. If the `field3logit` or `multifield3logit` object already contains the
#' confidence regions, they will be updated if the value of `conf` is different.
#' 
#' Given a reference probability distribution \eqn{\pi_0} over the simplex
#' \eqn{S=\{(\pi^{(1)}, \pi^{(2)}, \pi^{(3)})\in[0,1]^3\colon \pi^{(1)}+\pi^{(2)}+\pi^{(3)}=1\}},
#' and a change \eqn{\Delta\in\mathbf{R}^k} of covariate values, the confidence
#' region of the probability distribution resulting from the covariate change
#' \eqn{\Delta} is computed by means of the Wald statistics
#' \insertCite{severini2000}{plot3logit}, which should satisfy the following
#' condition \insertCite{wooldridge2010}{plot3logit}:
#' \deqn{
#' (\delta-\hat\delta)^\top
#' [(I_2\otimes\Delta)^\top\,\hat\Xi\,(I_2\otimes\Delta)]^{-1}
#' (\delta-\hat\delta)
#' \leq\chi^2_2(1-\alpha)
#' }
#' where \eqn{\hat\delta=\hat{B}^\top\Delta\in\mathbf{R}^2} is the point
#' estimate of change of natural parameters associated to \eqn{\Delta},
#' \eqn{\hat{B}=[\beta^{(2)}, \beta^{(3)}]\in\mathbf{R}^{k\times 2}} is the
#' matrix of point estimates of regression coefficients, \eqn{I_2} is the
#' identity matrix of order two, \eqn{\otimes} is the Kronecker product,
#' \eqn{\hat\Xi\in\mathbf{R}^{2k\times2k}} is the covariance matrix of
#' \eqn{vec(\hat{B})}, and finally, \eqn{\chi^2_2(1-\alpha)} is the 
#' \eqn{(1-\alpha)} quantile of \eqn{\chi^2_2}.
#' 
#' The set of points which satisfy the previous inequality with equal sign
#' delimits the border of the confidence region for \eqn{\delta}.
#' 
#' If we denote with \eqn{\mathcal{R}_\delta} the set of points \eqn{\delta}
#' which satisfy the previous inequality, it is possible to obtain the
#' confidence region of the effect of the covariate change \eqn{\Delta} over the
#' simplex \eqn{S} as follows:
#' \deqn{
#' \mathcal{R}=\{g^\leftarrow(g(\pi_0)+\delta)\colon \delta\in\mathcal{R}_\delta\}
#' \subseteq S
#' }
#' where \eqn{g\colon S\to\mathbf{R}^2} and
#' \eqn{g^\leftarrow\colon\mathbf{R}^2\to S} are respectively the link function
#' of the trinomial logit model and its inverse. They are defined as follows:
#' \deqn{
#' g(\pi)= g([\pi^{(1)},\pi^{(2)},\pi^{(3)}]^\top)
#' =\left[\ln\frac{\pi^{(2)}}{\pi^{(1)}}\,,\quad\ln\frac{\pi^{(3)}}{\pi^{(1)}}\right]^\top
#' }
#' \deqn{
#' g^\leftarrow(\eta)=g^\leftarrow([\eta_2,\eta_3]^\top)
#' =\left[
#' \frac{1}{1+e^{\eta_2}+e^{\eta_3}}\,,\quad
#' \frac{e^{\eta_2}}{1+e^{\eta_2}+e^{\eta_3}}\,,\quad
#' \frac{e^{\eta_3}}{1+e^{\eta_2}+e^{\eta_3}}
#' \right]^\top\,.
#' }
#' For further details and notation see
#' \insertCite{santi2022;textual}{plot3logit} and 
#' \insertCite{santi2019;textual}{plot3logit}.
#'
#' @param x an object of class `field3logit` or `multifield3logit`.
#' @param conf confidence level of the regions.
#' @param npoints number of points of the borders of the regions.
#'
#' @returns Object of class `field3logit` or `multifield3logit` with updated
#' confidence regions.
#'
#' @references
#' \insertAllCited{}
#' 
#' @examples
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade,
#'   data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale')
#' field0
#' add_confregions(field0)
#'
#' @export
add_confregions <- function(x, conf = 0.95, npoints = 100) {

  # Check classes and compute confidence regions
  if (inherits(x, 'multifield3logit')) {
  	x %<>%
  	  lapply(add_confregions_field3logit, conf = conf, npoints = npoints) %>%
  	  structure(class = c('multifield3logit', 'Hfield3logit'))
  } else if (inherits(x, 'field3logit')) {
  	x %<>% add_confregions_field3logit(conf, npoints)
  } else {
  	stop('Only objects of class "field3logit" and "multifield3logit" are allowed')
  }

  # Return the updated object
  return(x)
}









