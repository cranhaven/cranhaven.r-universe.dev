
test_that("simulate_data", {
  expect_is(simulate_data("Scenario I",n_i=20,var_e = 1,var_b = 0.5^2),"list")}
)
train<-simulate_data("Scenario I",n_i=20,var_e = 1,var_b = 0.5^2)
test_that("sasfclust model with regular grid", {
  expect_is(sasfclust(X=train$X,grid=train$grid,G=2,
               lambda_l = 10^0,lambda_s =10^-4,maxit = 5,q=10),"sasfclust")}
  )
n_obs<-dim(train$X)[2]
test_that("sasfclust model with irregular grid", {
  expect_is(sasfclust(X=as.numeric(vec(train$X)),timeindex=rep(1:length(train$grid),n_obs),curve=rep(1:n_obs,each=length(train$grid)),
                         grid=train$grid,G=2,lambda_l = 10^0,lambda_s =10^-4,maxit = 5,q=10),"sasfclust")}
)

