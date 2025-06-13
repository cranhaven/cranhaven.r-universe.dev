###################################################################################
##' Function for visualizing the clustering results
##'
##' @param res object return by function \link{clusvis} or \link{clusvis}
##' @param dim   numeric. This vector of size two choose the axes to represent.
##' @param threshold   numeric. It contains the thersholds used for computing the level curves.
##' @param add.obs   boolean. If TRUE, coordinnates of the observations are plotted.
##' @param positionlegend  character. It specifies the legend location.
##' @param xlim  numeric. It specifies the range of x-axis.
##' @param ylim  numeric. It specifies the range of y-axis.
##' @param colset  character. It specifies the colors of the observations per class.
##'
##'
##' @return NULL
##' @examples
##' \dontrun{
##'  # Package loading
##'  require(Rmixmod)
##'
##'  # Data loading (categorical data)
##'  data("congress")
##'  # Model-based clustering with 4 components
##'  set.seed(123)
##'  res <- mixmodCluster(congress[,-1], 4, strategy = mixmodStrategy(nbTryInInit = 500, nbTry=25))
##'
##'  # Inference of the parameters used for results visualization
##'  # (specific for Rmixmod results)
##'  # It is better because probabilities of classification are generated
##'  # by using the model parameters
##'  resvisu <- clusvisMixmod(res)
##'
##'  # Component interpretation graph
##'  plotDensityClusVisu(resvisu)
##'
##'  # Scatter-plot of the observation memberships
##'  plotDensityClusVisu(resvisu,  add.obs = TRUE)
##' }
##' @export
##'
plotDensityClusVisu <- function(res,
                                dim=c(1,2),
                                threshold=0.95,
                                add.obs=FALSE,
                                positionlegend="topright",
                                xlim=NULL,
                                ylim=NULL, 
                                colset=c("darkorange1", "dodgerblue2", "black", "chartreuse2", "darkorchid2", "gold2", "deeppink2", "deepskyblue1", "firebrick2", "cyan1", "red", "yellow")){
  if (res$error){
   warning("The number of components must be at least 3 to use this function") 
  }else{
    if (add.obs){
      input <- list()
      if (is.null(xlim)) xlim <- c(min(c(res$centers[,dim[1]], res$y[,dim[1]]))-1, max(c(res$centers[,dim[1]], res$y[,dim[1]])) + 1)
      if (is.null(ylim)) ylim <- c(min(c(res$centers[,dim[2]], res$y[,dim[2]]))-1, max(c(res$centers[,dim[2]], res$y[,dim[2]])) + 1)
      input$xval <- seq(xlim[1], xlim[2], length.out = 400)
      input$yval <- seq(ylim[1], ylim[2], length.out = 400)
      tmp <- sapply(1:length(res$prop), function(k) as.numeric(outer(input$xval, input$yval, dmixtmvnorm, mu=res$centers[k,dim], prop=res$prop[k])))
      input$z  <- t(matrix(rowSums(tmp), length(input$xval), length(input$yval)))
      input$z <- (matrix(apply(sweep(tmp, 1, rowSums(tmp), "/"), 1, max), length(input$xval), length(input$yval)))
      input$z[which(input$z<0.55)] <- 0.4
      contour(input,
              main = paste0("Difference between entropies: ", round(res$EM - res$EV,2)),
              levels=c(0.95,0.8,0.5),
              col=c("gray30","gray30",1),
              lwd=c(1,1,2),
              lty=c(2,2,1),
              labcex = 0.8,
              xlab=paste0("Dim1 (", 100*round(res$inertia[dim[1]]/sum(res$inertia), 4), "%)"),
              ylab=paste0("Dim2 (", 100*round(res$inertia[dim[2]]/sum(res$inertia), 4), "%)"))
      if (length(colset) < ncol(res$logtik.obs)) colset <- rep("black", ncol(res$logtik.obs))
      points(res$y[,dim[1]],
             res$y[,dim[2]],
             pch=20,
             cex=0.7,
             col=colset[apply(res$logtik.obs, 1, which.max)])
      if (!is.null(positionlegend)&(length(colset) >= ncol(res$logtik.obs))) legend(x = positionlegend, legend = paste0("Compo.", 1:ncol(res$logtik.obs)), col = colset[1:ncol(res$logtik.obs)], pch = 20)
      
      
    }else{
      if (is.null(xlim)) xlim <- c(min(res$centers[,dim[1]])-4, max(res$centers[,dim[1]]) + 4)
      if (is.null(ylim)) ylim <- c(min(res$centers[,dim[2]])-4, max(res$centers[,dim[2]]) + 4)
      xval <- seq(xlim[1], xlim[2], length.out = 400)
      yval <- seq(ylim[1], ylim[2], length.out = 400)
      tmp <- sapply(1:length(res$prop), function(k) as.numeric(outer(xval, yval, dmixtmvnorm, mu=res$centers[k,dim], prop=res$prop[k])))
      z <- t(matrix(rowSums(tmp), length(xval), length(yval)))
      tikmax <- t(matrix(apply(sweep(tmp, 1, rowSums(tmp), "/"), 1, max), length(xval), length(yval)))
      class <- t(matrix(apply(sweep(tmp, 1, rowSums(tmp), "/"), 1, which.max), length(xval), length(yval)))
      tmp <- optimize(function(alpha, z, threshold) abs(sum(z*(z>alpha))/sum(z) - threshold),
                      interval = c(0, max(z)),
                      z=z,
                      threshold=threshold)
      bound <- min(as.numeric(tikmax)[which(as.numeric(z)>tmp$minimum)])
      
      tikmax <- tikmax *(z>tmp$minimum)
      image(xval,yval,t(tikmax),
            main =  paste0("Difference between entropies: ", round(res$EM - res$EV,2)),
            col=c("white","gray30","gray60","gray80"), breaks = c(0,0.001,0.8,0.95,1),
            xlab=paste0("Dim1 (", 100*round(res$inertia[dim[1]]/sum(res$inertia), 4), "%)"),
            ylab=paste0("Dim2 (", 100*round(res$inertia[dim[2]]/sum(res$inertia), 4), "%)")
      )
      if (!is.null(positionlegend)) legend(legend = c("0.95<Pr. Classif.", "0.8<Pr. Classif.<0.95", "Pr. Classif.<0.8", "outside the conf. level."), x = positionlegend, fill = c("gray80","gray60","gray30", "white"), cex=0.7)
      input <- list(x=xval, y=yval, z=t(tikmax))
      
      contour(input, add=TRUE, levels=c(0.95,0.8,0.001), drawlabels = F,
              lwd=c(1,1,2),
              lty=c(2,2,1),
              labcex = 0.8)
      text(res$centers[,dim[1]], res$centers[,dim[2]], as.character(1:nrow(res$centers)), lwd=1.2)
    }    
  }
}