
objective.include <- '
Rcpp::NumericVector objective_fun(SEXP B_mat,SEXP env) {

/*B=matrix(B,nrow=n_basis_x,ncol=n_basis_y,byrow = FALSE)
sum(diag( t(Y_coef_tra-X_coef_tra%*%B)%*%(Y_coef_tra-X_coef_tra%*%B)))+lambda*delta_x^2*sum(abs(t(basis_values_x)%*%B%*%basis_values_y))*/

Rcpp::Environment e = Rcpp::as<Rcpp::Environment>(env);
int n_basis_x = Rcpp::as<int>(e["n_basis_x"]);
int n_basis_y = Rcpp::as<int>(e["n_basis_y"]);
double lambda_x_opt=Rcpp::as<double>(e["lambda_x_opt"]);
double lambda_y_opt=Rcpp::as<double>(e["lambda_y_opt"]);


Rcpp::NumericVector B_nv =Rcpp::as<Rcpp::NumericVector>(B_mat);

arma::mat B_vec=Rcpp::as<Rcpp::NumericVector>(B_nv);

arma::mat Y_newc= Rcpp::as<arma::mat>(e["Y_newc"]);
arma::mat X_newc = Rcpp::as<arma::mat>(e["X_newc"]);


arma::mat W_X = Rcpp::as<arma::mat>(e["W_X"]);
arma::mat W_Y = Rcpp::as<arma::mat>(e["W_Y"]);
arma::mat R_X = Rcpp::as<arma::mat>(e["R_X"]);
arma::mat R_Y = Rcpp::as<arma::mat>(e["R_Y"]);

arma::mat B(n_basis_x,n_basis_y);

B=arma::reshape(B_vec,n_basis_x,n_basis_y);

Rcpp::NumericVector out =Rcpp::as<Rcpp::NumericVector>(wrap(trace(-2*trans(Y_newc)*X_newc*B)+trace(W_Y*trans(B)*trans(X_newc)*X_newc*B)+trace(W_Y*trans(Y_newc)*Y_newc)
+lambda_x_opt*trace(trans(B)*R_X*B*W_Y)+lambda_y_opt*trace(trans(B)*W_X*B*R_Y)));

return out;
}'



gradient.include <- 'Rcpp::NumericVector grad_fun(SEXP B_mat, SEXP env) {
/* B<-matrix(B,nrow=n_basis_x,ncol=n_basis_y,byrow = FALSE)
f<-function(x,y){as.numeric(sign(t(x)%*%B%*%y))*x%o%y}
sum<-Reduce("+",Map(f,basis_values_x_list,basis_values_y_list))
c(-2*t(X_coef_tra)%*%Y_coef_tra+2*t(X_coef_tra)%*%X_coef_tra%*%B+lambda* delta_x^2*sum) */


Rcpp::Environment e = Rcpp::as<Rcpp::Environment>(env);
int n_basis_x = Rcpp::as<int>(e["n_basis_x"]);
int n_basis_y = Rcpp::as<int>(e["n_basis_y"]);

double lambda_x_opt=Rcpp::as<double>(e["lambda_x_opt"]);
double lambda_y_opt=Rcpp::as<double>(e["lambda_y_opt"]);


Rcpp::NumericVector B_nv =Rcpp::as<Rcpp::NumericVector>(B_mat);

arma::mat B_vec=Rcpp::as<Rcpp::NumericVector>(B_nv);

arma::mat Y_newc = Rcpp::as<arma::mat>(e["Y_newc"]);
arma::mat X_newc = Rcpp::as<arma::mat>(e["X_newc"]);

arma::mat  W_X = Rcpp::as<arma::mat >(e["W_X"]);
arma::mat  W_Y = Rcpp::as<arma::mat >(e["W_Y"]);
arma::mat  R_X = Rcpp::as<arma::mat >(e["R_X"]);
arma::mat  R_Y = Rcpp::as<arma::mat >(e["R_Y"]);

arma::mat B(n_basis_x,n_basis_y);

B=arma::reshape(B_vec,n_basis_x,n_basis_y);

Rcpp::NumericVector out =Rcpp::as<Rcpp::NumericVector>(wrap(-2*trans(X_newc)*Y_newc+2*trans(X_newc)*X_newc*B*W_Y
 +lambda_x_opt*2*R_X*B*W_Y+lambda_y_opt*2*W_X*B*R_Y));

return out;
}'


objective.body <- '
typedef Rcpp::NumericVector (*funcPtr)(SEXP,SEXP);
return(XPtr<funcPtr>(new funcPtr(&objective_fun)));'
gradient.body <- '
typedef Rcpp::NumericVector (*funcPtr)(SEXP,SEXP);
return(XPtr<funcPtr>(new funcPtr(&grad_fun)));'

objective <- cxxfunplus::cxxfunctionplus(signature(), body=objective.body,
                         inc=objective.include, plugin="RcppArmadillo",save.dso=TRUE)

gradient <- cxxfunplus::cxxfunctionplus(signature(), body=gradient.body,
                        inc=gradient.include, plugin="RcppArmadillo",save.dso=TRUE)


cxxfunctionplus2<-function (sig = character(), body = character(), plugin = "default", 
          includes = "", settings = inline::getPlugin(plugin), save.dso = FALSE, 
          ..., verbose = FALSE) 
{
  fx <- inline::cxxfunction(sig = sig, body = body, plugin = plugin, 
                    includes = includes, settings = settings, ..., verbose = verbose)
  dso.last.path <- dso.path2(fx)
  dso.bin <- if (save.dso) 
    read.dso2(dso.last.path)
  else raw(0)
  dso.filename <- sub("\\.[^.]*$", "", basename(dso.last.path))
  if (!is.list(sig)) {
    sig <- list(sig)
    names(sig) <- dso.filename
  }
  dso <- methods::new("cxxdso", sig = sig, dso.saved = save.dso, dso.filename = dso.filename, 
             dso.bin = dso.bin, system = R.version$system, .MISC = new.env())
  assign("cxxfun", fx, envir = dso@.MISC)
  assign("dso.last.path", dso.last.path, envir = dso@.MISC)
  return(dso)
}

read.dso2<-function (path) 
{
  n <- base::file.info(path)$size
  readBin(path, what = "raw", n = n)
}
dso.path2<-function (fx) 
{
  dllinfo <- inline::getDynLib(fx)
  dllinfo[["path"]]
}



lbfgsw<- function(call_eval, call_grad,  vars, environment = NULL, ...,
                  invisible = 0, m = 6, epsilon = 1e-5, past = 0, delta = 0, 
                  max_iterations = 0, linesearch_algorithm = "LBFGS_LINESEARCH_DEFAULT", 
                  max_linesearch = 20, min_step = 1e-20, max_step = 1e+20, ftol = 1e-4, 
                  wolfe = 0.9, gtol = 0.9,lambda=0, weights = rep(1,length(vars)), orthantwise_start = 0,
                  orthantwise_end = length(vars)) {
  
  # Verify homogeneity of function input
  if (is.function(call_eval) != is.function(call_grad)){
    stop("Function and gradient must be either both R functions or 
      both pointers to compiled C++ functions.")
  } 
  
  # Initialize environment if NULL
  if(!methods::hasArg(environment)) environment <- new.env()
  
  # Set up parameters
  N <- length(vars)
  xtol <- .Machine$double.eps
  
  # Check OWL-QN indexing
  if (orthantwise_end > N){
    stop("Orthantwise_end cannot be greater than the number of parameters. \n
          Note that the parameters count is zero-indexed.")
  }
  
  # Linesearch algorithm swith structure
  if (linesearch_algorithm=="LBFGS_LINESEARCH_DEFAULT"){
    linesearch <- 0
  } else if (linesearch_algorithm=="LBFGS_LINESEARCH_MORETHUENTE"){
    linesearch <- 0
  } else if (linesearch_algorithm=="LBFGS_LINESEARCH_BACKTRACKING_ARMIJO"){
    linesearch <- 1
  } else if (linesearch_algorithm=="LBFGS_LINESEARCH_BACKTRACKING"){
    linesearch <- 2
  } else if (linesearch_algorithm=="LBFGS_LINESEARCH_BACKTRACKING_WOLFE"){
    linesearch <- 2
  } else if (linesearch_algorithm=="LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE"){
    linesearch <- 3
  } else{
    stop("Invalid linesearch algorithm name. Check documentation.")
  }
  
  # Default to backtrack if OWL-WN is invoked
  if (linesearch_algorithm == "LBFGS_LINESEARCH_DEFAULT" & 
      lambda!=0){
    linesearch <- 2
  }
  
  # Throw error for OWL-QN/More-Thuente combination
  if (linesearch_algorithm == "LBFGS_LINESEARCH_MORETHUENTE" &
      lambda!=0){
    stop("Cannot use More-Thuente linesearch algorithm with OWL-QN.")
  }
  
  # Call main C++ routine
  ret <- lbfgsOptim(call_eval, call_grad, vars, 
                    environment, N, 
                    invisible, m, epsilon,
                    past, delta, max_iterations, 
                    linesearch, max_linesearch,
                    min_step, max_step, ftol, 
                    wolfe, gtol, xtol, 
                    lambda,
                    weights, 
                    orthantwise_start, 
                    orthantwise_end)
  
  # Construct output
  parameters <- c()
  for(i in seq(2, N + 1)){
    parameters <- c(parameters, ret[i])
  }
  out <- list()
  out$value <- ret[1]
  out$par <- parameters
  convergence <- ret[N + 2]
  out$convergence <- convergence
  
  # Catch errors
  if(convergence == 1){
    out$message = "The execution of the function was halted."
  } else if(convergence == 2){
    out$message = "The initial variables already minimize the objective function."
  } else if(convergence == -1204){
    out$message = "Unknown error."
  } else if(convergence == -1203){
    out$message = "Logic error."
  } else if(convergence == -1202){
    out$message = "Insufficient memory."
  } else if(convergence == -1201){
    out$message = "The minimization process has been canceled."
  } else if(convergence == -1200){
    out$message = "Invalid number of variables specified."
  } else if(convergence == -1199){
    out$message = "Invalid number of variables (for SSE) specified."
  } else if(convergence == -1198){
    out$message = "The array x must be aligned to 16 (for SSE)."
  } else if(convergence == -1197){
    out$message = "Invalid epsilon parameter specified."
  } else if(convergence == -1196){
    out$message = "Invalid past parameter specified."
  } else if(convergence == -1195){
    out$message = "Invalid delta parameter specified."
  } else if(convergence == -1194){
    out$message = "Invalid linesearch parameter specified."
  } else if(convergence == -1193){
    out$message = "Invalid min_step parameter specified."
  } else if(convergence == -1192){
    out$message = "Invalid max_step parameter specified."
  } else if(convergence == -1191){
    out$message = "Invalid ftol parameter specified."
  } else if(convergence == -1190){
    out$message = "Invalid wolfe parameter specified."
  } else if(convergence == -1189){
    out$message = "Invalid gtol parameter specified."
  } else if(convergence == -1188){
    out$message = "Invalid xtol parameter specified."
  } else if(convergence == -1187){
    out$message = "Invalid max_linesearch parameter specified."
  } else if(convergence == -1186){
    out$message = "Invalid orthantwise_c parameter specified."
  } else if(convergence == -1185){
    out$message = "Invalid orthantwise_start parameter specified."
  } else if(convergence == -1184){
    out$message = "Invalid orthantwise_end parameter specified."
  } else if(convergence == -1183){
    out$message = "The line-search step went out of the interval of uncertainty."
  } else if(convergence == -1182){
    out$message = "A logic error occurred; alternatively, the interval of uncertainty became too small."
  } else if(convergence == -1181){
    out$message = "A rounding error occurred; 
    alternatively, no line-search step satisfies the sufficient decrease and curvature conditions."
  } else if(convergence == -1180){
    out$message = "The line-search step became smaller than min_step."
  } else if(convergence == -1179){
    out$message = "The line-search step became larger than max_step."
  } else if(convergence == -1178){
    out$message = "The line-search routine reaches the maximum number of evaluations."
  } else if(convergence == -1177){
    out$message = "The algorithm routine reaches the maximum number of iterations."
  } else if(convergence == -1176){
    out$message = "Relative width of the interval of uncertainty is at most xtol."
  } else if(convergence == -1175){
    out$message = "A logic error (negative line-search step) occurred."
  } else if(convergence == -1174){
    out$message = "The current search direction increases the objective function value."
  }
  
  # Return output
  return(out)
}

