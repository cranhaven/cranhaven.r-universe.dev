#' Print method for msdpd class
#' @param x msdpd class
#' @param ... other parameters
#' @method print msdpd
#' @details Print method for msdpd class
#' @return A data.frame containing the coefficients and the corresponding standard error.
#' @examples 
#' \donttest{
#' data(data_n, data_nw)
#' result <- msdpd(y = data_n$y, x = data_n$x, w1 = data_nw)
#' result
#' }
#' @export


print.msdpd = function(x, ...){
  k = length(x$coefficient$beta)
  switch(x$model,
         "full" = {
           output = data.frame(matrix(,ncol=2, nrow = 5+k),row.names = c("lambda1", "lambda2", "lambda3", "rho", paste0("beta", seq(1,k)), "sigma2"))
           colnames(output) = c("coef", "se")
           output[1,1] = x$coefficient$lambda1
           output[1,2] = x$coefficient$lambda1_se
           output[2,1] = x$coefficient$lambda2
           output[2,2] = x$coefficient$lambda2_se
           output[3,1] = x$coefficient$lambda3
           output[3,2] = x$coefficient$lambda3_se
           output[4,1] = x$coefficient$rho
           output[4,2] = x$coefficient$rho_se
           output[5:(4+k),1] = x$coefficient$beta
           output[5:(4+k),2] = x$coefficient$beta_se
           output[5+k,1] = x$coefficient$sigma2
           output[5+k,2] = x$coefficient$sigma2_se
         },
         "slm" = {
           output = data.frame(matrix(,ncol=2, nrow = 3+k),row.names = c("lambda1", "rho", paste0("beta", seq(1,k)), "sigma2"))
           colnames(output) = c("coef", "se")
           output[1,1] = x$coefficient$lambda1
           output[1,2] = x$coefficient$lambda1_se
           output[2,1] = x$coefficient$rho
           output[2,2] = x$coefficient$rho_se
           output[3:(2+k),1] = x$coefficient$beta
           output[3:(2+k),2] = x$coefficient$beta_se
           output[3+k,1] = x$coefficient$sigma2
           output[3+k,2] = x$coefficient$sigma2_se
         },
         "sem" = {
           output = data.frame(matrix(,ncol=2, nrow = 3+k),row.names = c("lambda3", "rho", paste0("beta", seq(1,k)), "sigma2"))
           colnames(output) = c("coef", "se")
           output[1,1] = x$coefficient$lambda3
           output[1,2] = x$coefficient$lambda3_se
           output[2,1] = x$coefficient$rho
           output[2,2] = x$coefficient$rho_se
           output[3:(2+k),1] = x$coefficient$beta
           output[3:(2+k),2] = x$coefficient$beta_se
           output[3+k,1] = x$coefficient$sigma2
           output[3+k,2] = x$coefficient$sigma2_se
         },
         "sltl" = {
           output = data.frame(matrix(,ncol=2, nrow = 4+k),row.names = c("lambda1", "lambda2", "rho", paste0("beta", seq(1,k)), "sigma2"))
           colnames(output) = c("coef", "se")
           output[1,1] = x$coefficient$lambda1
           output[1,2] = x$coefficient$lambda1_se
           output[2,1] = x$coefficient$lambda2
           output[2,2] = x$coefficient$lambda2_se
           output[3,1] = x$coefficient$rho
           output[3,2] = x$coefficient$rho_se
           output[4:(3+k),1] = x$coefficient$beta
           output[4:(3+k),2] = x$coefficient$beta_se
           output[4+k,1] = x$coefficient$sigma2
           output[4+k,2] = x$coefficient$sigma2_se
         },
         stop("Undefined model"))
  print(output)
}

#' Print method for msdpdth class
#' @param x msdpdth class
#' @param ... other parameters
#' @method print msdpdth
#' @details Print method for msdpdth class
#' @return A data.frame containing the coefficients and the corresponding standard error.
#' @examples
#' \donttest{
#' data(data_th, data_w)
#' result <- msdpdth(y = data_th$y, x = data_th$x, w1 = data_w, th = data_th$th)
#' result
#' }
#' @export


print.msdpdth = function(x, ...){
  k = length(x$coefficient$beta)
  switch(x$model,
         "full" = {
           output = data.frame(matrix(,ncol=2, nrow = 9+k),row.names = c("lambda1_1", "lambda1_2", "lambda2_1", "lambda2_2", "lambda3_1", "lambda3_2", "rho_1", "rho_2", paste0("beta", seq(1,k/2), "_1"), paste0("beta", seq(1,k/2), "_2"), "sigma2"))
           colnames(output) = c("coef", "se")
           output[1,1] = x$coefficient$lambda1_1
           output[1,2] = x$coefficient$lambda1_1_se
           output[2,1] = x$coefficient$lambda1_2
           output[2,2] = x$coefficient$lambda1_2_se
           
           output[3,1] = x$coefficient$lambda2_1
           output[3,2] = x$coefficient$lambda2_1_se
           output[4,1] = x$coefficient$lambda2_2
           output[4,2] = x$coefficient$lambda2_2_se
           
           output[5,1] = x$coefficient$lambda3_1
           output[5,2] = x$coefficient$lambda3_1_se
           output[6,1] = x$coefficient$lambda3_2
           output[6,2] = x$coefficient$lambda3_2_se
           
           output[7,1] = x$coefficient$rho_1
           output[7,2] = x$coefficient$rho_1_se
           output[8,1] = x$coefficient$rho_2
           output[8,2] = x$coefficient$rho_2_se
           
           output[9:(8+k/2),1] = x$coefficient$beta[1:(k/2)]
           output[9:(8+k/2),2] = x$coefficient$beta_se[1:(k/2)]
           output[(9+k/2):(8+k),1] = x$coefficient$beta[(1+k/2):k]
           output[(9+k/2):(8+k),2] = x$coefficient$beta_se[(1+k/2):k]
           
           output[9+k,1] = x$coefficient$sigma2
           output[9+k,2] = x$coefficient$sigma2_se
         },
         "slm" = {
           output = data.frame(matrix(,ncol=2, nrow = 5+k),row.names = c("lambda1_1", "lambda1_2", "rho_1", "rho_2", paste0("beta", seq(1,k/2), "_1"), paste0("beta", seq(1,k/2), "_2"), "sigma2"))
           colnames(output) = c("coef", "se")
           output[1,1] = x$coefficient$lambda1_1
           output[1,2] = x$coefficient$lambda1_1_se
           output[2,1] = x$coefficient$lambda1_2
           output[2,2] = x$coefficient$lambda1_2_se
           
           output[3,1] = x$coefficient$rho_1
           output[3,2] = x$coefficient$rho_1_se
           output[4,1] = x$coefficient$rho_2
           output[4,2] = x$coefficient$rho_2_se
           
           output[5:(4+k/2),1] = x$coefficient$beta[1:(k/2)]
           output[5:(4+k/2),2] = x$coefficient$beta_se[1:(k/2)]
           output[(5+k/2):(4+k),1] = x$coefficient$beta[(1+k/2):k]
           output[(5+k/2):(4+k),2] = x$coefficient$beta_se[(1+k/2):k]
           
           output[5+k,1] = x$coefficient$sigma2
           output[5+k,2] = x$coefficient$sigma2_se
         },
         "sem" = {
           output = data.frame(matrix(,ncol=2, nrow = 5+k),row.names = c("lambda3_1", "lambda3_2", "rho_1", "rho_2", paste0("beta", seq(1,k/2), "_1"), paste0("beta", seq(1,k/2), "_2"), "sigma2"))
           colnames(output) = c("coef", "se")
           output[1,1] = x$coefficient$lambda3_1
           output[1,2] = x$coefficient$lambda3_1_se
           output[2,1] = x$coefficient$lambda3_2
           output[2,2] = x$coefficient$lambda3_2_se
           
           output[3,1] = x$coefficient$rho_1
           output[3,2] = x$coefficient$rho_1_se
           output[4,1] = x$coefficient$rho_2
           output[4,2] = x$coefficient$rho_2_se
           
           output[5:(4+k/2),1] = x$coefficient$beta[1:(k/2)]
           output[5:(4+k/2),2] = x$coefficient$beta_se[1:(k/2)]
           output[(5+k/2):(4+k),1] = x$coefficient$beta[(1+k/2):k]
           output[(5+k/2):(4+k),2] = x$coefficient$beta_se[(1+k/2):k]
           
           output[5+k,1] = x$coefficient$sigma2
           output[5+k,2] = x$coefficient$sigma2_se
         },
         "sltl" = {
           output = data.frame(matrix(,ncol=2, nrow = 7+k),row.names = c("lambda1_1", "lambda1_2", "lambda2_1", "lambda2_2", "rho_1", "rho_2", paste0("beta", seq(1,k/2), "_1"), paste0("beta", seq(1,k/2), "_2"), "sigma2"))
           colnames(output) = c("coef", "se")
           output[1,1] = x$coefficient$lambda1_1
           output[1,2] = x$coefficient$lambda1_1_se
           output[2,1] = x$coefficient$lambda1_2
           output[2,2] = x$coefficient$lambda1_2_se
           
           output[3,1] = x$coefficient$lambda2_1
           output[3,2] = x$coefficient$lambda2_1_se
           output[4,1] = x$coefficient$lambda2_2
           output[4,2] = x$coefficient$lambda2_2_se
           
           output[5,1] = x$coefficient$rho_1
           output[5,2] = x$coefficient$rho_1_se
           output[6,1] = x$coefficient$rho_2
           output[6,2] = x$coefficient$rho_2_se
           
           output[7:(6+k/2),1] = x$coefficient$beta[1:(k/2)]
           output[7:(6+k/2),2] = x$coefficient$beta_se[1:(k/2)]
           output[(7+k/2):(6+k),1] = x$coefficient$beta[(1+k/2):k]
           output[(7+k/2):(6+k),2] = x$coefficient$beta_se[(1+k/2):k]
           
           output[7+k,1] = x$coefficient$sigma2
           output[7+k,2] = x$coefficient$sigma2_se
         },
         stop("Undefined model"))
  print(output)
}