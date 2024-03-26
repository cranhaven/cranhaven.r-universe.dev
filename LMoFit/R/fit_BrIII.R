#' Fit Burr Type-III (BrIII) Distribution
#'
#' @param sl1 1st l-moments
#' @param st2 2nd l-moment ratio
#' @param st3 3rd l-moment ratio
#'
#' @return A dataframe containing the scale parameter, the shape1 parameter, the shape2 parameter, the squared error of scale parameter, and the squared error of shape parameter
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom pracma quadgk
#' @importFrom stats optim
#'
#' @examples
#' 
#' BrIII_par_valid <- fit_BrIII(sl1 = 10, st2 = 0.25, st3 = 0.1)
#' BrIII_par_invalid <- fit_BrIII(sl1 = 10, st2 = 0.5, st3 = 0.8)
#' 
fit_BrIII <- function(sl1, st2, st3) {
  L1s <- sl1; t2s <- st2; t3s <- st3
  conditionx <- con_samlmom_lspace(samplelmom = c(L1s, -99, -99, -99, t2s, t3s, -99), Dist = "BrIII")
  if (conditionx == "lpoint_inside_lspace") {
    suppressWarnings({
      Q <- function(u, scale, shape_1, shape_2) {scale*((shape_1*((u^(-1/(shape_1*shape_2))) - 1))^(-1*shape_2))} #BrIII Distribution
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
      opt_res <- stats::optim(par = c(1, 0.5), fn = objective_fn) #default method is Nelder and Mead. good for non-defrentiable function
      shape_1 <- opt_res[[1]][1] #g1
      shape_2 <- opt_res[[1]][2] #g2
      se_shapes <- opt_res[[2]]
      if (shape_2 >= 1) { #constraint on shape_2 to always be < 1
        shape_2 <- 0.9999999 #g2
        objective_fn <- function(shape) {(((NLR_2(scale = 1, shape_1 = shape, shape_2 = shape_2) - t2s)^2) +
                                            ((NLR_3(scale = 1, shape_1 = shape, shape_2 = shape_2) - t3s)^2))}
        opt_res <- stats::optim(1, objective_fn, method = 'BFGS', hessian = TRUE)
        shape_1 <- opt_res$par #g1
        se_shapes <- opt_res$value
      }
      objective_fn_2 <- function(scale_par) {(((NLR_1(scale = scale_par, shape_1 = shape_1, shape_2 = shape_2) - L1s)^2))}
      opt_res_2 <- stats::optim(par = 1, fn = objective_fn_2, method = 'L-BFGS-B', hessian = TRUE, lower = 0) #default method is Nelder and Mead. good for non-defrentiable function
      scale <- opt_res_2$par #beta
      se_scale <- opt_res_2$value
      BurrIII_par <- data.frame(scale, shape_1, shape_2, se_scale, se_shapes)
      colnames(BurrIII_par) <- c("scale", "shape1", "shape2", "se_scale", "se_shapes")
    })
  } else {
    warning(paste0("Consider using another distribution, ", conditionx, " of BrIII distribution. Use com_sam_lspace() to check lmoment ratio diagram"))
    BurrIII_par <- data.frame(-99, -99, -99, -99, -99)
    colnames(BurrIII_par) <- c("scale", "shape1", "shape2", "se_scale", "se_shapes")
  }
  return(BurrIII_par)
}
