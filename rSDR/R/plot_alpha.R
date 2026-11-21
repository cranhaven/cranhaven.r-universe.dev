#' Plot for the optimal alpha
#' @description
#' Plot for the mean with the standard deviation of cost function and alpha
#' @usage plot_alpha(opt_results)
#' @param opt_results opt_results is from either \code{\link[rSDR]{optimal_alpha_boot}} or \code{\link[rSDR]{optimal_alpha_cv}}
#' @returns No return value, showing the mean and standard deviation of cost function for each alpha value.
#' @export
#' @importFrom ggplot2 ggplot aes labs theme_classic theme rel element_text geom_pointrange
#'
#' @examples
#'
#' library(ManifoldOptim)
#' library(rSDR)
#' utils::data("ionosphere", package = "fdm2id")
#' X<-as.matrix(ionosphere[,c(1:33)])
#' Y<-ifelse(ionosphere[,34]=='b',0,1)
#' Y<-matrix(Y,length(Y),1)
#' set.seed(2435)
#' # plan(multisession) will launch parallel workers running in the background
#' # to save running time. To shut down background workers launched this way, call
#' # plan(sequential)
#' # use all local cores except one
#' # future::plan(future::multisession, workers = future::availableCores() - 1)
#' # use 2 cores for parallel
#' \donttest{
#' future::plan("multisession", workers = 2)
#' opt_results<-optimal_alpha_cv(alpha.v=c(0.3, 0.5, 0.7),X=X,Y=Y,d=3,kfolds=10)
#' plot_alpha(opt_results=opt_results)
#' }
plot_alpha<-function(opt_results){
  alpha.value=as.numeric(colnames(opt_results$f_test))
  mean.value=opt_results$f_test.mean
  sd.value=opt_results$f_test.sd
  datin<-data.frame(alpha.value=alpha.value,alpha.value=alpha.value,
                    sd.value=sd.value)

  min.no<-which.min(opt_results$opt.alpha)
  alpha_opt<-opt_results$opt.alpha[min.no]
  f_opt<-as.numeric(round(opt_results$f_test.mean[min.no],4))

  # cost function with 5 fold cross-validation
  ggplot2::ggplot(
    datin,
    ggplot2::aes(x = alpha.value, y = mean.value, ymin = mean.value-sd.value, ymax = mean.value+sd.value)
  ) + ggplot2::geom_pointrange()+
    ggplot2::labs(y='cost function',title=bquote(bold('Optimal '~alpha.value~'='~.(alpha_opt)~'with f='~.(f_opt))))+
    ggplot2::theme_classic()+
    ggplot2::theme(plot.title = element_text(size = rel(0.8)))
}
