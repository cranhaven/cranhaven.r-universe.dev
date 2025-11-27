#' @keywords internal
"_PACKAGE"
#' @rdname eHOF
#' @name eHOF
#' @title Extended HOF (Huisman-Olff-Fresco) Models
#' @encoding UTF-8
#' @description  Extended and enhanced hierarchical logistic regression models (called Huisman-Olff-Fresco in biology, see Huisman et al. 1993 Journal of Vegetation Science <doi:10.1111/jvs.12050>) models. Response curves along one-dimensional gradients including no response, monotone, plateau, unimodal and bimodal models.
#'
#' @references
#' Jansen, F., Dengler, J (2011) Plant names in vegetation databases - a neglected source of bias,
#' Journal of vegetation science, 21(6), 1179-1186. http://dx.doi.org/10.1111/j.1654-1103.2010.01209.x
#'
#' @importFrom stats D binomial coef deriv deviance fitted glm logLik nlminb optim optimize plogis predict printCoefmat rbinom
#' @importFrom mgcv gam
#' @importFrom lattice xyplot panel.xyplot
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom grDevices gray grey rainbow
#' @importFrom graphics axTicks axis boxplot hist layout legend lines matlines mtext par plot points polygon text title
NULL
#> NULL
