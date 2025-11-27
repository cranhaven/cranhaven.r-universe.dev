
#' @title Plot the results of  the Sas-funclust method
#' @description This function provides plots of the estimated cluster mean functions and of the classified curves when applied to the output of `sasfclust`, whereas
#'  provides the cross-validation plots when applied to the output of `sasfclust_cv`. In the latter case the first plot displays the CV values as a function of  \code{G}, \code{lambda_s} and \code{lambda_l};
#'  the second plot displays the CV values as a function of \code{lambda_s} and \code{lambda_l} for \code{G} fixed at its optimal value;
#'   the third plot displays the CV values as a function of \code{lambda_l} for \code{G} and \code{lambda_s}   fixed at their optimal value.
#'
#' @param x The output of  either `sasfclust` or `sasfclust_cv`.
#' @param ... No additional parameters, called for side effects.
#' @return No return value, called for side effects.
#' @rdname plot.sasfclust
#' @method plot sasfclust_cv
#' @export
#' @examples
#' \donttest{
#' library(sasfunclust)
#' train<-simulate_data("Scenario I",n_i=20,var_e = 1,var_b = 0.5^2)
#' lambda_s_seq=10^seq(-4,-3)
#' lambda_l_seq=10^seq(-1,0)
#' G_seq=2
#' mod_cv<-sasfclust_cv(X=train$X,grid=train$grid,G_seq=G_seq,
#' lambda_l_seq = lambda_l_seq,lambda_s_seq =lambda_s_seq,maxit = 20,K_fold = 2,q=10)
#' plot(mod_cv)
#' mod<-sasfclust(X=train$X,grid=train$grid,lambda_s = 10^-6,lambda_l =10,G = 2,maxit = 20,q=10)
#' plot(mod)
#' }

plot.sasfclust<-function(x,...){
  mod=x
  G<-dim(mod$mean_fd$coefs)[2]
  range<-mod$mean_fd$basis$rangeval
  grid_eval<-seq(range[1],range[2],length.out = 500)
  eval_mu<- fda::eval.fd(grid_eval,mod$mean_fd)

  oldpar <- graphics::par(no.readonly = TRUE)
  base::on.exit( graphics::par(oldpar))
  graphics::par(mfrow=c(1,2))

  graphics::matplot(grid_eval,eval_mu,ylab = "",xlab="",lty=1:G,type="l",xlim=range,ylim=c(min(mod$mod$data$x),max(mod$mod$data$x)))
  graphics::abline(h=0,col=grDevices::adjustcolor("grey", alpha = 0.6))
  graphics::title("Cluster means")
  graphics::legend("topright",legend = paste0("Cluster ",1:G),lty=1:G,col=1:G)

  base::plot(0,type="n",xlim=range,ylim=c(min(mod$mod$data$x),max(mod$mod$data$x)),xlab="",ylab="")
  graphics::title("Classified observations")
  for(ii in 1:length(unique(mod$mod$data$curve))){
    graphics::lines(mod$mod$grid[mod$mod$data$timeindex[which(mod$mod$data$curve==ii)]],mod$mod$data$x[which(mod$mod$data$curve==ii)],col=mod$clus[[1]][ii],lty=mod$clus[[1]][ii])
  }
  graphics::legend("topright",legend = paste0("Cluster ",1:G),lty=1:G,col=1:G)

  return(NULL)
}




#' @rdname plot.sasfclust
#' @method plot sasfclust
#' @export
#'
plot.sasfclust_cv<-function(x,...){

  mod=x
  comb_list_i<-mod$comb_list
  CV_i<-mod$CV
  sd_i<-mod$CV_sd
  zeros_i<-mod$zeros

  oldpar <- graphics::par(no.readonly = TRUE)
  base::on.exit( graphics::par(oldpar))
  graphics::par(mar=c(6,6,6,5))

  x<-seq(1,length(comb_list_i[,1]))
  labels<-lapply(1:length(comb_list_i[,1]),function(ii){a<-as.character(signif(comb_list_i[ii,],digits = 1));    paste(a[1],a[2],a[3])})
  graphics::layout(matrix(rbind(c(1,1),c(2,3)),2,2))
  base::plot(CV_i,pch=16,cex=0.5,col=2,type="l",xaxt="n",xlab="",ylab="CV",ylim=c(min(CV_i)-1.01*max(sd_i),max(CV_i)+1.01*max(sd_i)))
  graphics::points(CV_i,pch=16,cex=0.5,col=2)
  graphics::segments(x-0.1,CV_i+sd_i,x+0.1)
  graphics::segments(x-0.1,CV_i-sd_i,x+0.1)
  graphics::mtext(text=labels,side=1,at=x,las=2,cex=0.75)
  graphics::mtext(text=as.character(round(zeros_i*100)),side=3,at=x,las=2,cex=0.75)
  graphics::abline(v=which(CV_i==max(CV_i)))
  graphics::abline(h=max(CV_i))
  lamb_s<-unique(comb_list_i[,2])
  lamb_L<-unique(comb_list_i[,3])
  num_cluster<-unique(comb_list_i[,1])
  par<-CV_i
  sds<-sd_i
  zeros<-zeros_i
  comb_list<-comb_list_i
  m1<-mod$ms[1]
  m2<-mod$ms[2]
  m3<-mod$ms[3]

  kk=1
  max_vec_nc<-sd_vec_nc<-zero_vec<-numeric()
  new_comb_list<-matrix(0,length(lamb_s)*length(lamb_L),3)

  for (jj in 1:length(lamb_L)) {
    for (ii in 1:length(lamb_s)) {
      indexes<-which(comb_list[,2]==lamb_s[ii]&comb_list[,3]==lamb_L[jj])
      par_index<-par[indexes]
      sd_index<-sds[indexes]
      zero_index<-zeros[indexes]
      max<-which.max(par_index)
      if(m1*sd_index[max]>0.5*abs(max(par_index)-min(par_index)))lim=0.5*abs(max(par_index)-min(par_index)) else lim=m1*sd_index[max]
      onese<-which(par_index[1:(max)]>=par_index[max]-lim)[1]
      max_vec_nc[kk]<-par_index[onese]
      sd_vec_nc[kk]<-sd_index[onese]
      zero_vec[kk]<-zero_index[onese]
      new_comb_list[kk,]<-as.numeric(comb_list[indexes[onese],])
      kk=kk+1

    }
  }
  x<-seq(1,length(new_comb_list[,1]))
  labels<-lapply(1:length(new_comb_list[,1]),function(ii){a<-as.character(signif(new_comb_list[ii,],digits = 1));    paste(a[1],a[2],a[3])})
  base::plot(max_vec_nc,pch=16,cex=0.5,col=2,type="l",xaxt="n",xlab="",ylab="CV fixed G",ylim=c(min(max_vec_nc)-1.01*max(sd_vec_nc),max(max_vec_nc)+1.01*max(sd_vec_nc)))
  graphics::points(max_vec_nc,pch=16,cex=0.5,col=2)
  graphics::segments(x-0.1,max_vec_nc+sd_vec_nc,x+0.1)
  graphics::segments(x-0.1,max_vec_nc-sd_vec_nc,x+0.1)
  graphics::mtext(text=labels,side=1,at=x,las=2,cex=0.75,)
  graphics::mtext(text=as.character(round(zero_vec*100)),side=3,at=x,las=2,cex=0.75)

  kk=1
  max_vec_s<-sd_vec_s<-zero_vec2<-numeric()
  new_comb_list2<-matrix(0,length(lamb_L),3)
  for (ii in 1:length(lamb_L)) {
    indexes<-which(new_comb_list[,3]==lamb_L[ii])
    par_index<-max_vec_nc[indexes]
    sd_index<-sd_vec_nc[indexes]
    zero_index<-zero_vec[indexes]
    max<-which.max(par_index)
    onese<-max(which(par_index>=par_index[max]-m2*sd_index[max]))
    max_vec_s[kk]<-par_index[onese]
    sd_vec_s[kk]<-sd_index[onese]
    zero_vec2[kk]<-zero_index[onese]
    new_comb_list2[kk,]<-as.numeric(new_comb_list[indexes[onese],])
    kk=kk+1
  }
  x<-seq(1,length(new_comb_list2[,1]))
  labels_L<-lapply(1:length(new_comb_list2[,1]),function(ii){a<-as.character(signif(new_comb_list2[ii,],digits = 1));    paste(a[1],a[2],a[3])})
  base::plot(max_vec_s,pch=16,cex=0.5,col=2,type="l",xaxt="n",xlab="",ylab="CV fixed G and lambda_s",ylim=c(min(max_vec_s)-max(sd_vec_s),max(max_vec_s)+max(sd_vec_s)))
  graphics::points(max_vec_s,pch=16,cex=0.5,col=2)
  graphics::segments(x-0.1,max_vec_s+sd_vec_s,x+0.1)
  graphics::segments(x-0.1,max_vec_s-sd_vec_s,x+0.1)
  graphics::mtext(text=labels_L,side=1,at=x,las=2,cex=0.75)
  graphics::mtext(text=as.character(round(zero_vec2*100)),side=3,at=x,las=2,cex=0.75)

}


