
#' @title Plot the results of  the AdaSS method
#' @description This function provides plots of the AdaSS coefficient function estimate when applied to the output of `adass.fr`.
#' @param x The output of   `adass.fr`.
#' @param ... No additional parameters, called for side effects.
#' @return No return value, called for side effects.
#' @rdname plot.slasso
#' @export
#' @examples
#' library(adass)
#' data<-simulate_data("Scenario HAT",n_obs=100)
#' X_fd=data$X_fd
#' Y_fd=data$Y_fd
#' basis_s <- fda::create.bspline.basis(c(0,1),nbasis = 10,norder = 4)
#' basis_t <- fda::create.bspline.basis(c(0,1),nbasis = 10,norder = 4)
#' mod_adass <- adass.fr(Y_fd,X_fd,basis_s = basis_s, basis_t = basis_t,
#'  tun_par=c(10^-6,10^-6,0,0,0,0))
#' plot(mod_adass)

plot.adass<-function(x,...){
  mod=x
  length_grid=200
  rangevals<-mod$Beta_hat_fd$sbasis$rangeval
  rangevalt<-mod$Beta_hat_fd$tbasis$rangeval
  A=fda::eval.bifd(seq(rangevals[1],rangevals[2],length.out = length_grid),seq(rangevalt[1],rangevalt[2],length.out = length_grid),mod$Beta_hat_fd)
  names(A)<-c("s","t")
  oldpar <- graphics::par(no.readonly = TRUE)
  base::on.exit( graphics::par(oldpar))
  graphics::par(mfrow=c(1,2),pty="s")
  plot3D::image2D(z=A,x=seq(rangevals[1],rangevals[2],length.out = length_grid),y=seq(rangevalt[1],rangevalt[2],length.out = length_grid),xlab="s",ylab="t",pty="s")
  graphics::persp(A,x=seq(rangevals[1],rangevals[2],length.out = length_grid),y=seq(rangevalt[1],rangevalt[2],length.out = length_grid),zlab="",xlab="s",ylab="t",ticktype="detailed",col="lightblue",shade = 0.75,border = NA)

}


