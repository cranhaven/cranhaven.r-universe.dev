train<-simulate_data("Scenario I",n_i=20,var_e = 1,var_b = 0.5^2)
lambda_s_seq=10^seq(-4,-3)
lambda_l_seq=10^seq(-1,0)
G_seq=2
mod_cv<-sasfclust_cv(X=train$X,grid=train$grid,G_seq=G_seq,
                     lambda_l_seq = lambda_l_seq,lambda_s_seq =lambda_s_seq,maxit = 5,K_fold = 2,q=10)
mod<-sasfclust(X=train$X,grid=train$grid,G=mod_cv$G_opt,
          lambda_l = 10^0,lambda_s =10^-4,maxit = 5,q=10)

test_that("plot.sasfclust", {
  expect_is(plot(mod),"NULL")}
)
n_obs<-dim(train$X)[2]
test_that("plot.sasfclust_cv", {
  expect_is(plot(mod_cv),"NULL")}
)
