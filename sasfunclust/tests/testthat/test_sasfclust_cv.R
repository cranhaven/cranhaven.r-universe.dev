
test_that("simulate_data", {
  expect_is(simulate_data("Scenario I",n_i=20,var_e = 1,var_b = 0.5^2),"list")}
)

train<-simulate_data("Scenario I",n_i=20,var_e = 1,var_b = 0.5^2)
lambda_s_seq=10^seq(-4,-3)
lambda_l_seq=10^seq(-1,0)
G_seq=2
test_that("sasfclust_cv model with regular grid", {
  expect_is(sasfclust_cv(X=train$X,grid=train$grid,G_seq=G_seq,
                         lambda_l_seq = lambda_l_seq,lambda_s_seq =lambda_s_seq,maxit = 5,K_fold = 2,q=10),"sasfclust_cv")}
)
n_obs<-dim(train$X)[2]
test_that("sasfclust_cv model with irregular grid", {
  expect_is(sasfclust_cv(X=as.numeric(vec(train$X)),timeindex=rep(1:length(train$grid),n_obs),curve=rep(1:n_obs,each=length(train$grid)),
                      grid=train$grid,G_seq=G_seq,lambda_l_seq = lambda_l_seq,lambda_s_seq =lambda_s_seq,maxit = 5,K_fold = 2,q=10),"sasfclust_cv")}
)
