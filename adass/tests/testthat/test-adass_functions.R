

case<-"Scenario HAT"

data<-simulate_data(case,n_obs=100)
X_fd <- data$X_fd
Y_fd <- data$Y_fd
Beta_vero_fd<-data$Beta_vero_fd

domain=c(0,1)
basis_s <- fda::create.bspline.basis(c(0,1),nbasis = 10,norder = 4)
basis_t <- fda::create.bspline.basis(c(0,1),nbasis = 10,norder = 4)
mod_smooth <-adass.fr(Y_fd,X_fd,basis_s = basis_s,basis_t = basis_t,tun_par=c(10^-7,10^-8,0,0,0,0))

grid_s<-seq(0,1,length.out = 10)
grid_t<-seq(0,1,length.out = 10)
beta_der_eval_s<-fda::eval.bifd(grid_s,grid_t,mod_smooth$Beta_hat_fd,sLfdobj = 2)
beta_der_eval_t<-fda::eval.bifd(grid_s,grid_t,mod_smooth$Beta_hat_fd,tLfdobj = 2)
mod_adass_eaass<-adass.fr_eaass(Y_fd,X_fd,basis_s,basis_t,
                      beta_ders=beta_der_eval_s, beta_dert=beta_der_eval_t,
                      rand_search_par=list(c(-8,4),c(-8,4),c(0,0.1),c(0,4),c(0,0.1),c(0,4)), grid_eval_ders=grid_s,
                      grid_eval_dert=grid_t, popul_size = 2,ncores=1,iter_num=1)

mod_adass <-adass.fr(Y_fd, X_fd, basis_s = basis_s, basis_t = basis_t,
                   tun_par=mod_adass_eaass$tun_par_opt,beta_ders = beta_der_eval_s,
                   beta_dert = beta_der_eval_t,grid_eval_ders=grid_s,grid_eval_dert=grid_t )
plot(mod_adass)
