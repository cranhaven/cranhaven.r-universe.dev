#' Fit GEV distribution
#'
#' @param sl1 sample 1st l-moment
#' @param sl2 sample 2nd l-moment
#' @param st3 sample 3rd l-moment ratio
#'
#' @return A dataframe containing the location parameter, the scale parameter, the shape parameter, and the squared error of shape parameters.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom stats optim
#'
#' @examples
#' 
#' GEV_par <- fit_gev(sl1 = 10, sl2 = 0.5, st3 = 0.8)
#' 
fit_gev <- function(sl1, sl2, st3) {
  L1s <- sl1; L2s <- sl2; t3s <- st3
  GEV_t3 <- function(shape_1) {-3 + (2*(1 - (3)^shape_1))/(1 - (2)^shape_1)}
  GEV_location <- function(L1s, L2s, shape) {L1s - (L2s*((1 - gamma(1 - shape))/((1 - (2)^shape)*(gamma(1 - shape)))))}
  GEV_scale <- function(L1s, L2s, shape) {L2s/((1 - (2)^shape)*(gamma(-shape)))}
  #GEV_location <- function(L1s, t2s, shape) {(L1s*(-t2s - shape*gamma(-1*shape) + (2^shape)*shape*gamma(-1*shape) - shape*t2s*gamma(-1*shape)))/((-1 + (2^shape))*shape*gamma(-1*shape))}
  #GEV_scale <- function(L1s, t2s, shape) {-(L1s*t2s)/((-1 + (2^shape))*gamma(-1*shape))}
  objective_fn <- function(shape) {(t3s - GEV_t3(shape))^2}
  opt_res = stats::optim(0.1, objective_fn, method = 'BFGS', hessian = TRUE)
  shape <- opt_res$par
  se <- opt_res$value
  GEV_t3(shape)
  location <- GEV_location(L1s, L2s, shape)
  scale <- GEV_scale(L1s, L2s, shape)
  GEV_par <- data.frame(location, scale, shape, se)
  colnames(GEV_par) <- c("location", "scale", "shape", "se_shape")
  return(GEV_par)
}
