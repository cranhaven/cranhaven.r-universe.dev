#' Fit Generalized Gamma (GG) Distribution
#'
#' @param sl1 1st l-moments
#' @param st2 2nd l-moment ratio
#' @param st3 3rd l-moment ratio
#'
#' @return A dataframe containing the scale parameter, the shape1 parameter, the shape2 parameter, the squared error of scale parameter, and the squared error of shape parameters.
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom pracma quadgk
#' @importFrom stats optim
#' @importFrom stats qgamma
#'
#' @examples
#' 
#' GG_par_valid <- fit_GG(sl1 = 10, st2 = 0.4, st3 = 0.2)
#' GG_par_invalid <- fit_GG(sl1 = 1, st2 = 0.25, st3 = 0.25)
#' 
fit_GG <- function(sl1, st2, st3) {
  L1s <- sl1; t2s <- st2; t3s <- st3
  conditionx <- con_samlmom_lspace(samplelmom = c(L1s, -99, -99, -99, t2s, t3s, -99), Dist = "GG")
  if (conditionx == "lpoint_inside_lspace") {
    suppressWarnings({
      Q <- function(u, scale, shape_1, shape_2) {scale*qgamma(p = u, scale = 1, shape = shape_1 / shape_2)^(1 / shape_2 )} #GG quantile distribution function
      #Integral L-Moments (ILM)
      ILM_1 <- function(u, scale = scale, shape_1 = shape_1, shape_2 = shape_2) {Q(u, scale = scale, shape_1 = shape_1, shape_2 = shape_2)} #L1
      ILM_2 <- function(u, scale = scale, shape_1 = shape_1, shape_2 = shape_2) {Q(u, scale = scale, shape_1 = shape_1, shape_2 = shape_2)*(2 * u - 1)} #L2
      ILM_3 <- function(u, scale = scale, shape_1 = shape_1, shape_2 = shape_2) {Q(u, scale = scale, shape_1 = shape_1, shape_2 = shape_2)*(6 * (u^2) - 6 * u + 1)} #L3
      NLM_1 <- function(scale = scale, shape_1 = shape_1, shape_2 = shape_2) {
        pracma::quadgk(f = ILM_1, a = 0 + 1*10^-6, b = 1 - 1*10^-6, tol = .Machine$double.eps^0.5, scale = scale, shape_1 = shape_1, shape_2 = shape_2)}
      NLM_2 <- function(scale = scale, shape_1 = shape_1, shape_2 = shape_2) {
        pracma::quadgk(f = ILM_2, a = 0 + 1*10^-6, b = 1 - 1*10^-6, tol = .Machine$double.eps^0.5, scale = scale, shape_1 = shape_1, shape_2 = shape_2)}
      NLM_3 <- function(scale = scale, shape_1 = shape_1, shape_2 = shape_2) {
        pracma::quadgk(f = ILM_3, a = 0 + 1*10^-6, b = 1 - 1*10^-6, tol = .Machine$double.eps^0.5, scale = scale, shape_1 = shape_1, shape_2 = shape_2)}
      #Numeric L-ratios:
      NLR_1 <- function(scale = scale, shape_1 = shape_1, shape_2 = shape_2) {NLM_1(scale = scale, shape_1 = shape_1, shape_2 = shape_2)} #mean
      NLR_2 <- function(scale = scale, shape_1 = shape_1, shape_2 = shape_2) {NLM_2(scale = scale, shape_1 = shape_1, shape_2 = shape_2) / NLM_1(scale = scale, shape_1 = shape_1, shape_2 = shape_2)} #t2
      NLR_3 <- function(scale = scale, shape_1 = shape_1, shape_2 = shape_2) {NLM_3(scale = scale, shape_1 = shape_1, shape_2 = shape_2) / NLM_2(scale = scale, shape_1 = shape_1, shape_2 = shape_2)} #t3
      #%%%%%%%%%%%%%%%%%%%%%%%# fitting sellected distribution to each site #%%%%%%%%%%%%%%%%%%%%%%%%%%
      objective_fn <- function(shape) {(((NLR_2(scale = 1, shape_1 = shape[1], shape_2 = shape[2]) - t2s)^2) +
                                          ((NLR_3(scale = 1, shape_1 = shape[1], shape_2 = shape[2]) - t3s)^2))}
      #opt_res <- stats::optim(par = c(1, 2), fn = objective_fn, method = "BFGS") #default method is Nelder and Mead. good for non-defrentiable function
      opt_res <- stats::optim(par = c(1, 2), fn = objective_fn, method = "BFGS", control = list(reltol = 1e-8)) #default method is Nelder and Mead. good for non-defrentiable function
      shape_1 <- opt_res[[1]][1] #g1
      shape_2 <- opt_res[[1]][2] #g2
      se_shapes <- opt_res[[2]]
      
      objective_fn_2 <- function(scale_par) {(((NLR_1(scale = scale_par, shape_1 = shape_1, shape_2 = shape_2) - L1s)^2))}
      #opt_res_2 <- stats::optim(par = 1, fn = objective_fn_2, method = 'L-BFGS-B', hessian = TRUE, lower = 0) #default method is Nelder and Mead. good for non-defrentiable function
      opt_res_2 <- stats::optim(par = 1, fn = objective_fn_2, method = "BFGS") #default method is Nelder and Mead. good for non-defrentiable function
      #opt_res_2 <- stats::optimize(f = objective_fn_2, lower = 0.0000000001, upper = 10^10)
      scale <- opt_res_2$par #beta
      se_scale <- opt_res_2$value
      GG_par <- data.frame(scale, shape_1, shape_2, se_scale, se_shapes)
      colnames(GG_par) <- c("scale", "shape1", "shape2", "se_scale", "se_shapes")
    })
  } else {
    warning(paste0("Consider using another distribution, ", conditionx, " of GG distribution. Use com_sam_lspace() to check lmoment ratio diagram"))
    GG_par <- data.frame(-99, -99, -99, -99, -99)
    colnames(GG_par) <- c("scale", "shape1", "shape2", "se_scale", "se_shapes")
  }
  return(GG_par)
}
