#Function for calculating cost function f using testing data set based on the optimal C from training data.
f_opt_testing<-function(alpha,d,X_train,Y_train,X_test,Y_test,maxiter,tol){
  C_value.a<-rSDR(X=X_train, Y=Y_train, d, alpha=alpha,maxiter=maxiter,tol=tol)$C_value
  f_test<-cost_fun_esting(C_value=C_value.a,X_train=X_train, X_test=X_test,Y_test=Y_test,alpha=0.5)
  f_test
}
