train<-simulate_data("Scenario I",n_i=20,var_e = 1,var_b = 0.5^2)
lambda_s_seq=10^seq(-4,-3)
lambda_l_seq=10^seq(-1,0)
G_seq=2
mod_cv<-sasfclust_cv(X=train$X,grid=train$grid,G_seq=G_seq,
                     lambda_l_seq = lambda_l_seq,lambda_s_seq =lambda_s_seq,maxit = 10,K_fold = 2,q=10)
plot(mod_cv)
