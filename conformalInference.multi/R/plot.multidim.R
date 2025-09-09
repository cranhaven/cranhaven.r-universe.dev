#' Plot Confidence Regions obtained with Split Conformal
#'
#' @param out The output of a prediction function.
#' @param same.scale Should I force the same scale for all the y-axis ? Default
#' is FALSE.
#' @return g_list A list of ggplots (output[[i]] is the i-th observation confidence region).
#' @details It exploits the package \code{\link{ggplot2}}, \code{\link{gridExtra}}
#' and \code{\link{hrbrthemes}} to better visualize the results.
#' @example inst/examples/ex.split.R
#' @example inst/examples/ex.full.R
#' @export plot_multidim



plot_multidim=function(out,same.scale=FALSE){

  full<-!is.null(out$valid_points)
  split<-!is.null(out$pred) & (!full)
  case="m_jack"

  if(full)
    case="full"

  if(split)
    case="split"

  switch(case,
         "full"={plots=plot_multidim_full(out)},
         "split"={plots=plot_multidim_split(out,same.scale)},
         "m_jack"={plots=plot_multidim_msplit(out,same.scale)},)

  return(plots)

}


plot_multidim_full=function(full){

  #Get Data
  valid_points = full$valid_points
  pred = full$pred
  name<-colnames(valid_points[[1]])


  n0 = length(valid_points)

  plots<- lapply(1:n0, function(k){

    df=valid_points[[k]]
    colnames(df)<-c("one","two","pval")
    g_plot =ggplot2::ggplot(data=df, ggplot2::aes(one,two)) + ggplot2::geom_raster(ggplot2::aes(fill = -pval)) + ggplot2::scale_fill_distiller(palette = "Reds") + hrbrthemes::theme_ipsum() + ggplot2::xlab("y1") + ggplot2::ylab("y2") + ggplot2::geom_point(data=pred[k,],ggplot2::aes(x=X1,y=X2),shape=8, size=10)+ggplot2::theme(legend.text=ggplot2::element_text(size=10),legend.title=ggplot2::element_text(size=20), text = ggplot2::element_text(size=20))

    if(n0>1){
      g_plot<-g_plot+ ggplot2::ggtitle(paste("Test Observation",k) )+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }
    else
      g_plot<-g_plot+ ggplot2::ggtitle("Full Conformal")+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    return(g_plot)

  })

  return(plots)
}


utils::globalVariables(c("Var1", "Var2", "X1", "X2"))



plot_multidim_split=function(out, same.scale = FALSE){


  #Get Data
  lo = out$lo
  up = out$up
  pred = out$pred
  x0=out$x0

  # Find bounds for the plots

  if(same.scale){

    y_up = max(up) +0.01 * sd(up)
    y_lo = min(lo) -0.01 * sd(lo)

  }

  # Define dimensions
  p<-ncol(x0)
  q<-ncol(lo)
  g_list<-vector("list",p*q)



  gl <- lapply(1:p,function(ii) lapply(1:q, function(jj){

    df=data.frame(xd=x0[,ii],yg=pred[,jj],y_min=lo[,jj], y_max=up[,jj])

    ggg<- ggplot2::ggplot(df, ggplot2::aes(x=xd,y = yg)) + ggplot2::geom_pointrange(ggplot2::aes(ymin = y_min, ymax = y_max), color ="red") + ggplot2::xlab(paste("x ",ii)) + ggplot2::ylab(paste("y ",jj))


    if(same.scale)
      ggg = ggg + ggplot2::ylim(y_up,y_lo)

    return(ggg)

  }))



  glist <- do.call(c, gl)
  return(do.call(gridExtra::"grid.arrange", c(glist, ncol=q,top="Confidence Intervals")))
}



plot_multidim_msplit=function(out, same.scale = FALSE){


  #Get Data
  lo = out$lo
  up = out$up
  x0 = out$x0

  # Find bounds for the plots

  if(same.scale){

    y_up = max(up) +0.01 * sd(up)
    y_lo = min(lo) -0.01 * sd(lo)

  }

  # Define dimensions
  p<-ncol(x0)
  q<-ncol(lo)
  g_list<-vector("list",p*q)



  gl <- lapply(1:p,function(ii) lapply(1:q, function(jj){

    df=data.frame(xd=x0[,ii],y_min=lo[,jj], y_max=up[,jj],yg=(lo[,jj]+up[,jj])/2)

    ggg<- ggplot2::ggplot(df,ggplot2::aes(x=xd,y=yg)) + ggplot2::geom_pointrange(ggplot2::aes(ymin = y_min, ymax = y_max), color ="red") + ggplot2::xlab(paste("x ",ii)) + ggplot2::ylab(paste("y ",jj))


    if(same.scale)
      ggg = ggg + ggplot2::ylim(y_up,y_lo)

    return(ggg)

  }))



  glist <- do.call(c, gl)
  return(do.call(gridExtra::"grid.arrange", c(glist, ncol=q,top="Confidence Intervals")))

}


utils::globalVariables(c( "xd", "y_max", "y_min", "yg","one","two"))

