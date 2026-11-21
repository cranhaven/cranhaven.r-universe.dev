#' Projected data plotting
#' @description
#' Function for plotting of projected_data from rSDR results.
#' @usage plot_rSDR(projected_data,Y,Y.name,colors=NULL)
#' @param projected_data projected data from rSDR results.
#' @param Y an \eqn{n \times 1} numeric matrix.
#' @param Y.name label for y-axis
#' @param colors Assign specific colors to each level of the response variable.
#' @return No return value, visualizing reduced-dimensional data using 1D, 2D, or 3D projections. When the reduced dimension exceeds three, pairwise scatter plots are automatically generated.
#' @export
#'
#' @importFrom graphics pairs
#' @importFrom scatterplot3d scatterplot3d
#' @importFrom stats quantile
#' @examples
#'
#' library(ManifoldOptim)
#' library(rSDR)
#' utils::data("ionosphere", package = "fdm2id")
#' X<-as.matrix(ionosphere[,c(1:33)])
#' Y<-ifelse(ionosphere[,34]=='b',0,1)
#' Y<-matrix(Y,length(Y),1)
#' ionosphere$V35<-factor(ionosphere$V35,levels=c('b','g'),labels=c('Bad','Good'))
#' set.seed(2435)
#' \donttest{
#' sdr_result<-rSDR(X=X, Y=Y, d=3, alpha=0.3,maxiter=1000,tol=1e-7)
#' plot_rSDR(projected_data=sdr_result$projected_data,Y=ionosphere$V35,
#' Y.name='group',colors=c("#374E55FF", "#DF8F44FF"))
#'}


plot_rSDR<- function(projected_data,Y,Y.name='group',colors=NULL) {

  if (length(unique(Y))<=4 & is.numeric(Y)){
    stop('Y should be a factor.')
  }

  datin<-projected_data
  # Determine the dimension d of the SDR subspace
  d <- ncol(projected_data)
  datin$group<-Y #for binary outcome
  # Check if Y is a factor (categorical variable)
  #label all color for each observation
  #Y_col is a nx1 for plot
  #colors is number of color


    title.name<-bquote(bold('SDR projection (d='~.(d)~')'))


  if (!is.factor(Y)) {
    # If Y is continuous, discretize it into quantile bins to enable categorical coloring.
    Y_factor <- base::cut(Y, breaks = stats::quantile(Y, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
    if (is.null(colors)){
      Y_col <- as.numeric(Y_factor)
      colors<-unique(Y_col)
    } else{
      Y_col <- colors[as.numeric(Y)]
    }
    legend_labels <- levels(Y_factor)
  } else {

    if (is.null(colors)){
      Y_col <- as.numeric(Y)
      colors<-unique(Y_col)
    } else{
      Y_col <- colors[as.numeric(Y)]
    }
    legend_labels <- levels(Y)
  }


  if (d == 1) {
    # For d = 1, create a scatter plot of the projected data vs Y
    base::plot(x=datin$V1, y=Y, xlab = "Projected Data", ylab = "Response",
         main = title.name,
         col = Y_col, pch = 16,cex.main=1)
  } else if (d == 2) {
    fun_2d_plot(datin=datin,colors=colors,title.name=title.name,group.name=Y.name)

  } else if (d == 3) {
    scatterplot3d::scatterplot3d(x=datin[,'V1'],y=datin[,'V2'],z=datin[,'V3'],pch = 16, color=Y_col,
                                 main=title.name,
                                 xlab = "First SDR Direction",
                                 ylab = "Second SDR Direction",
                                 zlab = "Third SDR Direction")
  } else {
    # For higher dimensions, use pairs plot or advise dimensionality reduction
    graphics::pairs(as.data.frame(projected_data), col = Y_col, pch = 16,
          main = paste("Pairs Plot of SDR Projections (d=", d, ")", sep = ""))
  }

  # list(projected_data,beta=beta,X=X)

}
