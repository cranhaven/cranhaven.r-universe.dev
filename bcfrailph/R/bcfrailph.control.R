#'
#' @name bcfrailph.control
#' @title Arguments for controlling bcfrailph fits.
#' @description This is used to set various numeric parameters controlling a bcfrailph model fits.
#'
#' @param max.iter Maximum number of outer iterations. The default is 400.
#' @param tol A tolerance for convergence i.e the maximum differences of loglikelihood between succssive iterations.The default is 1e-04. 
#' @param iter.max argument used to control \link{nlminb} fits used.
#' @param trace argument used to control \link{nlminb} fits used.
#' @param abs.tol argument used to control \link{nlminb} fits used.
#' @param rel.tol argument used to control \link{nlminb} fits used.
#' @param x.tol   argument used to control \link{nlminb} fits used.
#' @param eval.max argument used to control \link{nlminb} fits used.
#' @param xf.tol argument used to control \link{nlminb} fits used.
#' @param step.min argument used to control \link{nlminb} fits used.
#' @param step.max argument used to control \link{nlminb} fits used.
#' @return A list of control parameters.
#'
#' @export bcfrailph.control
#'
#' @seealso \code{\link{bcfrailph}}
#'
bcfrailph.control<-function (max.iter=400,tol=1e-04,
eval.max=500,iter.max=500,trace=0,abs.tol=1e-20,rel.tol= 1e-10,x.tol=1.5e-8,
xf.tol= 2.2e-14,step.min=1,step.max=1){
res<-list(max.iter=max.iter,tol=tol,
nlminb_control=list(eval.max=eval.max,iter.max=iter.max,
trace=trace,abs.tol=abs.tol,rel.tol=rel.tol,
x.tol=x.tol,xf.tol=xf.tol,step.min=step.min,
step.max=step.max,sing.tol=rel.tol))
class(res) <- c("bcfrailph.control")
res
}

##
