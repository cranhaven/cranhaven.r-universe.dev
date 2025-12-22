
#' @title Plot the results of the  Phase I and the Phase II of the FRTM
#' @description This function provides plots of the Hotelling's \eqn{T^{2}} and SPE control charts.
#' @param x The output of  either `FRTM_PhaseI` or `FRTM_PhaseII`.
#' @param ... A variable \code{ind_sel} could be provided to select some observations from either the tuning or monitoring set.
#' @return No return value, called for side effects.
#' @rdname plot.FRTM_PhaseI
#' @method plot FRTM_PhaseI
#' @export
#' @inherit FRTM_PhaseI return examples
plot.FRTM_PhaseI<-function(x,...){
  aa<-list(...)
  mod=x
  seq_x= if(methods::is(mod,"FRTM_PhaseI"))mod$par_reg$par.rtr$seq_x else mod$mod_phaseI$par_reg$par.rtr$seq_x
  oldpar <- graphics::par(no.readonly = TRUE)
if(is.null(aa$ind_sel))ind_sel=1:length(mod$T2_fd) else ind_sel=aa$ind_sel
  base::on.exit( graphics::par(oldpar))
  graphics::par(mfrow=c(1,3))
    # par(mfrow=c(1,3))
  T_2_mat<-sapply(1:length(ind_sel),function(ii)fda::eval.fd(seq(mod$T2_fd[[ind_sel[ii]]]$basis$rangeval[1],mod$T2_fd[[ind_sel[ii]]]$basis$rangeval[2],l=30),mod$T2_fd[[ind_sel[ii]]]))
  SPE_mat<-sapply(1:length(ind_sel),function(ii)fda::eval.fd(seq(mod$SPE_fd[[ind_sel[ii]]]$basis$rangeval[1],mod$SPE_fd[[ind_sel[ii]]]$basis$rangeval[2],l=30),mod$SPE_fd[[ind_sel[ii]]]))
    plot_lfd(mod$T2_fd[ind_sel],ylim=c(0,max(T_2_mat,na.rm = T)+1),xlim=c(0,max(seq_x)),ylab=expression("T"^{"2"}),xlab="t")
    graphics::legend("topleft",lty=c(1),lwd=2,col=2,legend = c("CL"))
    graphics::lines(mod$CL_T2,lwd=2,col=2)
    plot_lfd(mod$SPE_fd[ind_sel],ylim=c(0,max(SPE_mat,na.rm = T)+1),xlim=c(0,max(seq_x)),ylab="SPE",xlab="t")
    graphics::lines(mod$CL_SPE,lwd=2,col=2)
    graphics::legend("topleft",lty=c(1),lwd=2,col=2,legend = c("CL"))
    plot_lfd(mod$x_list_smooth[ind_sel],xlim=range(unlist(mod$grid_i)),ylim=range(unlist(mod$x_err)),ylab="X",xlab="t" )
    # sapply((2:length(data_tuning$grid_i)),function(ii)lines(data_tuning$grid_i[[ii]],data_tuning$x_err[[ii]],col=ii))


}
#' @rdname plot.FRTM_PhaseI
#' @method plot FRTM_PhaseII
#' @export
#'
plot.FRTM_PhaseII<-function(x,...){
  plot.FRTM_PhaseI(x,...)
}


plot_lfd<-function(x_fd_list,...){
  n_obs<-length(x_fd_list)
  cols=1:n_obs
  lty=1:n_obs
  fda::plot.fd(x_fd_list[[1]],col=cols[1],lty=lty[1],...)
  for(ii in 1:n_obs)
    graphics::lines(x_fd_list[[ii]],col=cols[ii],lty=lty[1],...)
}

# lines.lfd<-function(x_fd_list,...){
#   n_obs<-length(x_fd_list)
#   graphics::lines(x_fd_list[[1]],type="l",...)
#   for(ii in 1:n_obs)
#     graphics::lines(x_fd_list[[ii]],...)
# }

#' @title Plot the results of the  Mixed Functional Principal Component Analysis (mFPCA)
#' @description This function provides plots of the principal components of the mFPCA.
#' @param x The output of   `mFPCA`.
#' @param ... A variable \code{type} could be provided that can assume two values. If \code{type="single"}, the principal components are plotted separately.
#'  If \code{type="all"}, the principal components are plotted together.
#' @return No return value, called for side effects.
#' @method plot mFPCA
#' @export
#' @inherit mFPCA return examples
plot.mFPCA<-function(x,...){
  aa<-list(...)
  mod=x
  oldpar <- graphics::par(no.readonly = TRUE)
  base::on.exit( graphics::par(oldpar))
  weights=mod$k_weights
  K=dim(mod$scores)[2]
  if(is.null(aa$type))type="single" else type=aa$type
  nvar<-length((mod$eigfun_fd))
  if(!is.null(mod$eigvect_sc))n_scal<-dim(mod$eigvect_sc)[1]
  if(type=="single"){
    if(!is.null(mod$eigvect_sc)) graphics::layout(matrix(1:(K*(nvar+1)),K,nvar+1,byrow=TRUE), widths = c(rep(1,nvar),0.4)) else  graphics::layout(matrix(1:(K*(nvar)),K,nvar,byrow=TRUE), widths = rep(1,nvar))
    tot_var<-round(mod$varprop*100,digits = 2)
    for (ii in 1:K) {
      per_fd<-sapply(1:nvar,function(iii)fda::inprod(mod$eigfun_fd[[iii]][ii],mod$eigfun_fd[[iii]][ii])*weights[iii])
      per_sc<-if(!is.null(mod$eigvect_sc))diag(mod$eigvect_sc[,ii]%*%t(mod$eigvect_sc[,ii])%*%diag(weights[(nvar+1):length(weights)])) else NULL
      per<-round(c(per_fd,per_sc)/sum(c(per_fd,per_sc))*100,digits = 2)
      for(kk in 1:nvar){
        plot(mod$eigfun_fd[[kk]][ii],ylab="",xlab="")
        graphics::title(paste("PC",ii,"Variability:",per[kk],"% of ",tot_var[ii], "%"  ))
      }
      if(!is.null(mod$eigvect_sc)) {
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        label<-paste("Scalar_variable",1:length(mod$eigvect_sc[,ii]),"=",round(mod$eigvect_sc[,ii],digits = 4),"\nVariability",per[(nvar+1):length(per)],"% of",tot_var[ii], "%" )
        graphics::text(x = 0.5,y=seq(0.8,0.2,length.out = length(mod$eigvect_sc[,ii])) ,  labels=label, cex = 1.3, col = "black")
      }
    }
  }
  if(type=="all"){

    if(!is.null(mod$eigvect_sc)) graphics::layout(matrix(1:(nvar+1),1,nvar+1,byrow=TRUE), widths = c(rep(1,nvar),rep(0.4,1))) else  graphics::layout(matrix(1:(nvar),1,nvar,byrow=TRUE), widths = c(rep(1,nvar)))
    for(kk in 1:nvar){

      plot(mod$eigfun_fd[[kk]])
    }
    if(!is.null(mod$eigvect_sc)) {
      x<-rep(1:n_scal,each=K)
      col<-rep(1:K,n_scal)
      plot(x,mod$eigvect_sc,xlim=c(0.8,n_scal+0.2),col=col,xlab="",xaxt="n")
      graphics::axis(1,at=1:n_scal,labels = 1:n_scal)
    }

  }

}

plot_pcahy.fit<-function(mod,K=NULL,type="single"){
  weights=mod$weights
  if(is.null(K))K=dim(mod$scores)[2]
  nvar<-length((mod$eigfun_fd))
  if(!is.null(mod$eigvect_sc))n_scal<-dim(mod$eigvect_sc)[1]
  if(!is.null(mod$eigvect_sc)) graphics::layout(matrix(1:((nvar+1)),1,nvar+1,byrow=TRUE), widths = c(rep(1,nvar),0.4)) else  graphics::layout(matrix(1:((nvar)),1,nvar,byrow=TRUE), widths = rep(1,nvar))

  for(kk in 1:nvar){
   plot_lfd(mod$fit$fit_fd[kk],ylab="",xlab="")
  }
  if(!is.null(mod$eigvect_sc)) {
    plot(c(0, 1), c(0, 1), ann = F,type="n",  xaxt = 'n' ,xlim=c(-0.5,1.5),ylim=c(-0.25+min(mod$fit$fit_sc),max(mod$fit$fit_sc)+0.25))# plot( rep(0,length(mod$eigvect_sc[,1])),mod$eigvect_sc[,1],xlim=c(-0.1,0.1))
    graphics::axis(1,(1:n_scal)-1,1:n_scal)
    graphics::points(rep((1:n_scal)-1,each=dim(mod$sc_mat)[1]),mod$fit$fit_sc)
  }

}
