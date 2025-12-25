#' M-estimator for spatial dynamic panel data model
#'
#' Estimating the spatial dynamic panel data model with M-estimator
#'
#' @import rJava
#' @import rCMA
#' @importFrom matrixcalc is.singular.matrix
#' 
#' @param y matrix, containing regional index (first column), time index (second column, numeric) and dependent variable (third column, numeric).
#' @param x matrix, containing regional index (first column), time index (second column, numeric) and regressors (numeric).
#' @param w1 matrix, the spatial weight matrix. If w2 and w3 are supplied, the spatial weight matrix for spatial lag.
#' @param correction logical, whether to use adjusted score function. Default value is TRUE.
#' @param hessian_er logical, whether to output hessian based se. Ignored if correction is set to False. Default value is FALSE.
#' @param true_range logical, whether to used the accurate stationary check. Default value is FALSE due to performance reasons.
#' @param max_try integer, maximum attempt for the solver. Default value is 5.
#' @param w2 matrix, the spatial weight matrix for spatio-temporal lag. Default value is the same as w1.
#' @param w3 matrix, the spatial weight matrix for spatial error. Default value is the same as w1.
#' @param no_tf logical, whether to account for time effect. Default value is TRUE.
#' @param model character, indicates the model used for estimation, can be "full", "slm", "sem", "sltl". See Details.
#' @param rcpp logical, whether to use the rcpp implementation to calculate the score function. Default value is TRUE.
#' @param cma_pop_multi integer, multiplier for the population size used in CMA-ES. Default value is 1.
#' 
#' @details Estimating the spatial dynamic panel data model with Yang(2018)'s M-estimator
#' \deqn{	y_{ti} = \mu_{i}+\alpha_t + x_{ti}\beta + \rho y_{t-1,i} + \lambda_1 \sum_{j =1}^{n}w_{1,ij}y_{tj} + \lambda_2 \sum_{j =1}^{n}w_{2,ij}y_{t-1,j} +  u_{ti},\\ 
#' u_{ti} = \lambda_3\sum_{j =1}^{n}w_{3,ij}u_{tj} + v_{ti}, i=1,\ldots,n,t=1,\ldots,T}
#' The minimum number of time-periods is 4. Make sure the rows and columns of w1, w2, and w3 are lined up with the regional index. 
#' Sub-models can be specified by argument "model"
#' \itemize{
#' \item{"full"} {Full model}
#' \item{"slm"} {\eqn{\lambda_2 = \lambda_3 = 0}}
#' \item{"sem"} {\eqn{\lambda_1 = \lambda_2 = 0}}
#' \item{"sltl"} {\eqn{\lambda_3 = 0}}
#' }
#' Some suggestions when the optimizer fails: 
#' \itemize{
#' \item{} {Increase max_try}
#' \item{} {Increase cma_pop_multi}
#' \item{} {try a different submodel}
#' }
#' 
#' @return A list of estimation results of S3 class "msdpd"
#' \itemize{
#' \item{"coefficient"} {list, coefficients and standard errors}
#' \item{"model"} {character, model used for estimation}
#' \item{"vc_mat"} {matrix, variance-covariance matrix}
#' \item{"hessian"} {matrix, optional, hessian matrix}
#' }
#' 
#' @references Yang, Z. (2018). Unified M-estimation of fixed-effects spatial dynamic models with short panels. Journal of Econometrics, 205(2), 423-447.
#' 
#' @examples 
#' \donttest{
#' data(data_n, data_nw)
#' result <- msdpd(y = data_n$y, x = data_n$x, w1 = data_nw)
#' }
#' 
#' @export
#' 
#' 

msdpd = function(y,
                 x, 
                 w1, 
                 correction = TRUE,
                 hessian_er = FALSE,
                 true_range = FALSE, 
                 max_try = 5, 
                 w2 = w1, 
                 w3 = w1, 
                 no_tf = FALSE,
                 model = "full", 
                 rcpp = TRUE, 
                 cma_pop_multi = 1){
  p = length(w1[1,])
  y = df_data(input_order(y))
  x = df_data(input_order(x))
  tp = dim(y)[1]
  t = tp / p
  if (t < 3) stop("Time period less than 4") 
  t1 = t-1
  tp1 = t1*p
  y_1 = as.matrix(y[-c((tp1+1):tp),-c(1,2)])
  y = as.matrix(y[-c(1:p),-c(1,2)])
  x = as.matrix(x[,-c(1,2)])
  x = x[-c(1:p),,drop = F]
  k_o = dim(x)[2]
  if (!no_tf){
    #individual (time)
    tif = as.matrix(rep(1, p))
    tif = kronecker(diag(t1), tif)
    # tif = tif[, -(length(tif[1, ]))]
    x = cbind(x, tif)
  }
  k = dim(x)[2]
  mat_c = diag(2, t1, t1)
  mat_c[(row(mat_c)==col(mat_c)+1)|(row(mat_c)==col(mat_c)-1)] = -1
  inv_c = solve(mat_c)
  alp_lbound = 1/min(Re(eigen(w3)$values[abs(Im(eigen(w3)$values)) < 1e-6]))
  if (true_range){
    const_rcma_r = function(para, w, w_er, w_lam, alp_lbound){
      rho = para[1]
      alp = para[2]
      theta = para[3]
      lam = para[4]
      p = dim(w)[1]
      if (alp >= 1| alp <= alp_lbound | norm((theta*diag(p) + lam*w_lam)%*%solve(diag(p) - rho*w)) >= 1){
        return(F)
      } else{
        return(T)
      }
    }
    const_rcma = function(para) {const_rcma_r(para = para, w = w1, w_er = w3, w_lam = w2, alp_lbound = alp_lbound)}
  } else {
    const_rcma = function(para){
      rho = para[1]
      alp = para[2]
      theta = para[3]
      lam = para[4]
      if (abs(alp) >= 1| abs(theta) + abs(rho) + abs(lam) >= 1){
        return(F)
      } else{
        return(T)
      }
    }
  }
  switch (model,
          "full" = {
            if(rcpp){
              objfun_rcma = function(par) {msdpd_aqs(para = par, y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, w_er = w3, w_lam = w2)}
            }else{
              objfun_rcma = function(par) {full_aqs(para = par, y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, w_er = w3, w_lam = w2)}
            }
            for (i in 1:max_try){
              cma_obj = cmaNew()
              cmaSetPopulationSize(cma_obj, cmaGetPopulationSize(cma_obj)*cma_pop_multi)
              cmaInit(cma_obj, dimension = 4)
              optim_res = cmaOptimDP(cma_obj, objfun_rcma, isFeasible = const_rcma, maxDimPrint = 4)
              if (optim_res$bestFitness < 1e-12) break
            }
            output = list(solve = optim_res$bestFitness < 1e-12, coefficient = list())
            output$coefficient = list(lambda1 = optim_res$bestX[1],
                                      lambda3 = optim_res$bestX[2],
                                      rho = optim_res$bestX[3],
                                      lambda2 = optim_res$bestX[4]
            )
            output$coefficient = c(output$coefficient, full_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "beta_sigs", w_er = w3, w_lam = w2))
            output$coefficient$beta = output$coefficient$beta[1:k_o]
            if (correction){
              if (hessian_er){
                all_se = full_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = hessian_er, w_er = w3, w_lam = w2)
                vc_er = sqrt(diag(all_se$vc_mat))
                hes_er = sqrt(diag(all_se$hes_mat))
                output$coefficient[["beta_se"]] = vc_er[1:k_o]
                output$coefficient[["sigma2_se"]] = vc_er[k+1]
                output$coefficient[["lambda1_se"]] = vc_er[k+3]
                output$coefficient[["lambda3_se"]] = vc_er[k+5]
                output$coefficient[["rho_se"]] = vc_er[k+2]
                output$coefficient[["lambda2_se"]] = vc_er[k+4]
                output$coefficient[["beta_se_hes"]] = hes_er[1:k_o]
                output$coefficient[["sigma2_se_hes"]] = hes_er[k+1]
                output$coefficient[["lambda1_se_hes"]] = hes_er[k+3]
                output$coefficient[["lambda3_se_hes"]] = hes_er[k+5]
                output$coefficient[["rho_se_hes"]] = hes_er[k+2]
                output$coefficient[["lambda2_se_hes"]] = hes_er[k+4]
                output$vc = all_se$vc_mat[-c((k_o+1):k), -c((k_o+1):k)]
                output$hessian = all_se$hes_mat[-c((k_o+1):k), -c((k_o+1):k)]
              }else{
                all_se = full_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = hessian_er, w_er = w3, w_lam = w2)
                vc_er = sqrt(diag(all_se$vc_mat))
                output$coefficient[["beta_se"]] = vc_er[1:k_o]
                output$coefficient[["sigma2_se"]] = vc_er[k+1]
                output$coefficient[["lambda1_se"]] = vc_er[k+3]
                output$coefficient[["lambda3_se"]] = vc_er[k+5]
                output$coefficient[["rho_se"]] = vc_er[k+2]
                output$coefficient[["lambda2_se"]] = vc_er[k+4]
                output$vc = all_se$vc_mat[-c((k_o+1):k), -c((k_o+1):k)]
              }
            } else {
              all_se = full_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = T, w_er = w3, w_lam = w2)
              hes_er = sqrt(diag(all_se$hes_mat))
              output$coefficient[["beta_se"]] = hes_er[1:k_o]
              output$coefficient[["sigma2_se"]] = hes_er[k+1]
              output$coefficient[["lambda1_se"]] = hes_er[k+3]
              output$coefficient[["lambda3_se"]] = hes_er[k+5]
              output$coefficient[["rho_se"]] = hes_er[k+2]
              output$coefficient[["lambda2_se"]] = hes_er[k+4]
              output$hessian = all_se$hes_mat[-c((k_o+1):k), -c((k_o+1):k)]
            }
          },
          "slm" = {
            if(rcpp){
              objfun_rcma = function(par) {
                pars = numeric(4)
                pars[1] = par[1]
                pars[3] = par[2]
                msdpd_aqs(para = pars, y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, w_er = matrix(0,p,p), w_lam = matrix(0,p,p))
              }
            }else{
              objfun_rcma = function(par) {slm_aqs(para = par, y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction)}
            }
            const_rcma_slm = function(x){
              pars = numeric(4)
              pars[1] = x[1]
              pars[3] = x[2]
              return(const_rcma(pars)) 
            }
            for (i in 1:max_try){
              cma_obj = cmaNew()
              cmaSetPopulationSize(cma_obj, cmaGetPopulationSize(cma_obj)*cma_pop_multi)
              cmaInit(cma_obj, dimension = 2)
              optim_res = cmaOptimDP(cma_obj, objfun_rcma, isFeasible = const_rcma_slm, maxDimPrint = 2)
              if (optim_res$bestFitness < 1e-12) break
            }
            output = list(solve = optim_res$bestFitness < 1e-12, coefficient = list())
            output$coefficient = list(lambda1 = optim_res$bestX[1],
                                      rho = optim_res$bestX[2]
            )
            
            output$coefficient = c(output$coefficient, slm_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "beta_sigs"))
            output$coefficient$beta = output$coefficient$beta[1:k_o]
            
            if (correction){
              if (hessian_er){
                all_se = slm_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = hessian_er)
                vc_er = sqrt(diag(all_se$vc_mat))
                hes_er = sqrt(diag(all_se$hes_er))
                output$coefficient[["beta_se"]] = vc_er[1:k_o]
                output$coefficient[["sigma2_se"]] = vc_er[k+1]
                output$coefficient[["lambda1_se"]] = vc_er[k+3]
                output$coefficient[["rho_se"]] = vc_er[k+2]
                output$coefficient[["beta_se_hes"]] = hes_er[1:k_o]
                output$coefficient[["sigma2_se_hes"]] = hes_er[k+1]
                output$coefficient[["lambda1_se_hes"]] = hes_er[k+3]
                output$coefficient[["rho_se_hes"]] = hes_er[k+2]
                output$vc = all_se$vc_mat[-c((k_o+1):k), -c((k_o+1):k)]
                output$hessian = all_se$hes_mat[-c((k_o+1):k), -c((k_o+1):k)]
              }else{
                all_se = slm_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = hessian_er)
                vc_er = sqrt(diag(all_se$vc_mat))
                output$coefficient[["beta_se"]] = vc_er[1:k_o]
                output$coefficient[["sigma2_se"]] = vc_er[k+1]
                output$coefficient[["lambda1_se"]] = vc_er[k+3]
                output$coefficient[["rho_se"]] = vc_er[k+2]
                output$vc = all_se$vc_mat[-c((k_o+1):k), -c((k_o+1):k)]
              }
            } else {
              all_se = slm_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = T)
              hes_er = sqrt(diag(all_se$hes_mat))
              output$coefficient[["beta_se"]] = hes_er[1:k_o]
              output$coefficient[["sigma2_se"]] = hes_er[k+1]
              output$coefficient[["lambda1_se"]] = hes_er[k+3]
              output$coefficient[["rho_se"]] = hes_er[k+2]
              output$hessian = all_se$hes_mat[-c((k_o+1):k), -c((k_o+1):k)]
            }
          },
          "sem" = {
            if(rcpp){
              objfun_rcma = function(par) {
                pars = numeric(4)
                pars[2] = par[1]
                pars[3] = par[2]
                msdpd_aqs(para = pars, y = y, x_ = x, y1 = y_1, inv_c = inv_c, correction = correction, w_er = w3, w = matrix(0,p,p), w_lam = matrix(0,p,p))
              }
            }else{
              objfun_rcma = function(par) {sem_aqs(para = par, y = y, x_ = x, y1 = y_1, inv_c = inv_c, correction = correction, w_er = w3)}
            }
            const_rcma_sem = function(x){
              pars = numeric(4)
              pars[2] = x[1]
              pars[3] = x[2]
              return(const_rcma(pars)) 
            }
            for (i in 1:max_try){
              cma_obj = cmaNew()
              cmaSetPopulationSize(cma_obj, cmaGetPopulationSize(cma_obj)*cma_pop_multi)
              cmaInit(cma_obj, dimension = 2)
              optim_res = cmaOptimDP(cma_obj, objfun_rcma, isFeasible = const_rcma_sem, maxDimPrint = 2)
              if (optim_res$bestFitness < 1e-12) break
            }
            output = list(solve = optim_res$bestFitness < 1e-12, coefficient = list())
            output$coefficient = list(lambda3 = optim_res$bestX[1],
                                      rho = optim_res$bestX[2]
            )
            output$coefficient = c(output$coefficient, sem_aqs(para = optim_res$bestX,  y = y, x_ = x, y1 = y_1, inv_c = inv_c, correction = correction, mode = "beta_sigs", w_er = w3))
            output$coefficient$beta = output$coefficient$beta[1:k_o]
            if (correction){
              if (hessian_er){
                all_se = sem_aqs(para = optim_res$bestX,  y = y, x_ = x, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = hessian_er, w_er = w3)
                vc_er = sqrt(diag(all_se$vc_mat))
                hes_er = sqrt(diag(all_se$hes_mat))
                output$coefficient[["beta_se"]] = vc_er[1:k_o]
                output$coefficient[["sigma2_se"]] = vc_er[k+1]
                output$coefficient[["lambda3_se"]] = vc_er[k+3]
                output$coefficient[["rho_se"]] = vc_er[k+2]
                output$coefficient[["beta_se_hes"]] = hes_er[1:k_o]
                output$coefficient[["sigma2_se_hes"]] = hes_er[k+1]
                output$coefficient[["lambda3_se_hes"]] = hes_er[k+3]
                output$coefficient[["rho_se_hes"]] = hes_er[k+2]
                output$vc = all_se$vc_mat[-c((k_o+1):k), -c((k_o+1):k)]
                output$hessian = all_se$hes_mat[-c((k_o+1):k), -c((k_o+1):k)]
              }else{
                all_se = sem_aqs(para = optim_res$bestX,  y = y, x_ = x, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = hessian_er, w_er = w3)
                vc_er = sqrt(diag(all_se$vc_mat))
                output$coefficient[["beta_se"]] = vc_er[1:k_o]
                output$coefficient[["sigma2_se"]] = vc_er[k+1]
                output$coefficient[["lambda3_se"]] = vc_er[k+3]
                output$coefficient[["rho_se"]] = vc_er[k+2]
                output$vc = all_se$vc_mat[-c((k_o+1):k), -c((k_o+1):k)]
              }
            } else {
              all_se = sem_aqs(para = optim_res$bestX,  y = y, x_ = x, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = T, w_er = w3)
              hes_er = sqrt(diag(all_se$hes_mat))
              output$coefficient[["beta_se"]] = hes_er[1:k_o]
              output$coefficient[["sigma2_se"]] = hes_er[k+1]
              output$coefficient[["alpha_se"]] = hes_er[k+3]
              output$coefficient[["rho_se"]] = hes_er[k+2]
              output$hessian = all_se$hes_mat[-c((k_o+1):k), -c((k_o+1):k)]
            }
          },
          "sltl" = {
            if(rcpp){
              objfun_rcma = function(par) {
                pars = numeric(4)
                pars[1] = par[1]
                pars[3] = par[2]
                pars[4] = par[3]
                msdpd_aqs(para = pars, y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, w_lam = w2, w_er = matrix(0,p,p))
              }
            }else{
              objfun_rcma = function(par) {sltl_aqs(para = par, y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, w_lam = w2)}
            }
            const_rcma_sem = function(x){
              pars = numeric(4)
              pars[1] = x[1]
              pars[3] = x[2]
              pars[4] = x[3]
              return(const_rcma(pars)) 
            }
            for (i in 1:max_try){
              cma_obj = cmaNew()
              cmaSetPopulationSize(cma_obj, cmaGetPopulationSize(cma_obj)*cma_pop_multi)
              cmaInit(cma_obj, dimension = 3)
              optim_res = cmaOptimDP(cma_obj, objfun_rcma, isFeasible = const_rcma_sem, maxDimPrint = 3)
              if (optim_res$bestFitness < 1e-12) break
            }
            output = list(solve = optim_res$bestFitness < 1e-12, coefficient = list())
            output$coefficient = list(lambda1 = optim_res$bestX[1],
                                      rho = optim_res$bestX[2],
                                      lambda2 = optim_res$bestX[3]
            )
            output$coefficient = c(output$coefficient, sltl_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "beta_sigs", w_lam = w2))
            output$coefficient$beta = output$coefficient$beta[1:k_o]
            if (correction){
              if (hessian_er){
                all_se = sltl_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = hessian_er, w_lam = w2)
                vc_er = sqrt(diag(all_se$vc_mat))
                hes_er = sqrt(diag(all_se$hes_mat))
                output$coefficient[["beta_se"]] = vc_er[1:k_o]
                output$coefficient[["sigma2_se"]] = vc_er[k+1]
                output$coefficient[["lambda1_se"]] = vc_er[k+3]
                output$coefficient[["rho_se"]] = vc_er[k+2]
                output$coefficient[["lambda2_se"]] = vc_er[k+4]
                output$coefficient[["beta_se_hes"]] = hes_er[1:k_o]
                output$coefficient[["sigma2_se_hes"]] = hes_er[k+1]
                output$coefficient[["lambda1_se_hes"]] = hes_er[k+3]
                output$coefficient[["rho_se_hes"]] = hes_er[k+2]
                output$coefficient[["lambda2_se_hes"]] = hes_er[k+4]
                output$vc = all_se$vc_mat[-c((k_o+1):k), -c((k_o+1):k)]
                output$hessian = all_se$hes_mat[-c((k_o+1):k), -c((k_o+1):k)]
              }else{
                all_se = sltl_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = hessian_er, w_lam = w2)
                vc_er = sqrt(diag(all_se$vc_mat))
                output$coefficient[["beta_se"]] = vc_er[1:k_o]
                output$coefficient[["sigma2_se"]] = vc_er[k+1]
                output$coefficient[["lambda1_se"]] = vc_er[k+3]
                output$coefficient[["rho_se"]] = vc_er[k+2]
                output$coefficient[["lambda2_se"]] = vc_er[k+4]
                output$vc = all_se$vc_mat[-c((k_o+1):k), -c((k_o+1):k)]
              }
            } else {
              all_se = sltl_aqs(para = optim_res$bestX,  y = y, x_ = x, w = w1, y1 = y_1, inv_c = inv_c, correction = correction, mode = "opmd",  hessian_er = T, w_lam = w2)
              hes_er = sqrt(diag(all_se$hes_mat))
              output$coefficient[["beta_se"]] = hes_er[1:k_o]
              output$coefficient[["sigma2_se"]] = hes_er[k+1]
              output$coefficient[["lambda1_se"]] = hes_er[k+3]
              output$coefficient[["rho_se"]] = hes_er[k+2]
              output$coefficient[["lambda2_se"]] = hes_er[k+4]
              output$hessian = all_se$hes_mat[-c((k_o+1):k), -c((k_o+1):k)]
            }
          },
          stop("Undefined model")
  )
  output$model = model
  class(output) = "msdpd"
  return(output)
}
