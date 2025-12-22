set.seed(1)

data <- simulate_data_FRTM(n_obs=100)
X <- sapply(1:100,function(ii) data$x_true[[ii]])
x_fd <- fda::smooth.basis(y = X,argvals =  data$grid,fda::create.bspline.basis(c(0,1),30))$fd

H <- sapply(1:100,function(ii) data$h[[ii]])
h_fd <- fda::smooth.basis(y = H,argvals =  data$grid,fda::create.bspline.basis(c(0,1),30))$fd

mod_mFPCA <- mFPCA(x_fd,h_fd,ncom="ptv",par_ncom=0.95)
plot(mod_mFPCA)
