test_that("simulate_data", {
  expect_is(simulate_data("Scenario II",n_obs=500),"list")}
)
data<-simulate_data("Scenario II",n_obs=15)
X_fd=data$X_fd
Y_fd=data$Y_fd
domain=c(0,1)
n_basis_s<-15
n_basis_t<-15
breaks_s<-seq(0,1,length.out = (n_basis_s-2))
breaks_t<-seq(0,1,length.out = (n_basis_t-2))
basis_s <- fda::create.bspline.basis(domain,breaks=breaks_s)
basis_t <- fda::create.bspline.basis(domain,breaks=breaks_t)

lambda_L_vec=10^c(0,1,2) 
lambda_s_vec=10^seq(-6,-5) 
lambda_t_vec=10^seq(-5,-5) 
test_that("slasso.fr_cv",
{ expect_is(slasso.fr_cv(Y_fd = Y_fd,X_fd=X_fd,basis_s=basis_s,basis_t=basis_t,lambda_L_vec = lambda_L_vec,lambda_s_vec = lambda_s_vec,lambda_t_vec =lambda_t_vec,max_iterations=2,K=2,invisible=1,ncores=1),"slasso_cv")
}
)
test_that("slasso.fr",
          { expect_is(slasso.fr(Y_fd = Y_fd,X_fd=X_fd,basis_s=basis_s,basis_t=basis_t,lambda_L = 1,lambda_s = 10^-5,lambda_t =  10^-5,invisible=1,max_iterations=2),"slasso")
          }
)
# mod=slasso.fr(Y_fd = Y_fd,X_fd=X_fd,basis_s=basis_s,basis_t=basis_t,lambda_L = 10^-1,lambda_s = 10^-5,lambda_t =  10^-5,invisible=1,max_iterations=1000)
# plot(mod)
