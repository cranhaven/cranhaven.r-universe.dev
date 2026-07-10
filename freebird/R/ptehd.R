#' Proportion of treatment effect explained by high-dimensional surrogates
#'
#' Estimates the proportion of the treatment effect explained by the indirect effect via high-dimensional surrogates.
#'
#' @param Yt The n-dmensional outcome vector in the treatment group.
#' @param Yc The n-dmensional outcome vector in the control group.
#' @param St The n x p matrix of surrogates in the treatment group.
#' @param Sc The n x p matrix of surrogates in the treatment group.
#' @param lambda_range Min and max of range of range of tuning parameter to use during the constrained l1 optimization step.
#'
#' @return A list with components
#' \describe{
#' \item{est_id}{Estimate of indirect effect, defined as \eqn{\int E(Y | S = s, Z = 1) dF(s | Z = 1) - \int E(Y | S = s, Z = 0) dF(s | Z = 0)}}
#' \item{sd_id}{Standard deviation of indirect effect estimate}
#' \item{est_total}{Estimate of total effect}
#' \item{sd_total}{Standard deviation of total effect estimate}
#' \item{V}{Covariance matrix of (est_id, est_total)}
#' \item{est_R}{Estimate of proportion of treatment effect explained by surrogates}
#' \item{sd_R}{Standard deviation of proportion estimate}
#' }
#' 
#' @author
#' Ruixuan Zhou
#'
#' @export
#' 
#' @importFrom stats lm cov sd
#' @importFrom scalreg scalreg
#' @importFrom Rmosek mosek
#' @importFrom Matrix Matrix
#'
#' @examples
#' n = 10
#' St = replicate(n, rnorm(20, mean = 1))
#' Sc = replicate(n, rnorm(20))
#' Yt = 1 + rowSums(St) / 2 + rnorm(n)
#' Yc = rowSums(Sc) / 3 + rnorm(n)
#' out = ptehd(Yt, Yc, St, Sc)

ptehd <- function(Yt, Yc, St, Sc, lambda_range = c(0, 1)) {

    ## satisfy conditions C2 and C3
    sgn = sign(mean(Yt) - mean(Yc))
    Yt = sgn * Yt
    Yc = sgn * Yc
    sgns = sign(colMeans(St) - colMeans(Sc))
    St = t(t(St) * sgns)
    Sc = t(t(Sc) * sgns)
    
    nt = length(Yt)
    nc = length(Yc)
    p = dim(St)[2]
    
    St_tilde = scale(St, center = TRUE, scale = FALSE)
    Sc_tilde = scale(Sc, center = TRUE, scale = FALSE)
    
    Yt_tilde = scale(Yt, center = TRUE, scale = FALSE)
    Yc_tilde = scale(Yc, center = TRUE, scale = FALSE)
    
    Sigmast_hat = cov(St)
    Sigmasc_hat = cov(Sc)
    
    scalt = scalreg(St_tilde, Yt_tilde)
    scalc = scalreg(Sc_tilde, Yc_tilde)
    sigmat1_hat = scalt$hsigma
    sigmac1_hat = scalc$hsigma
    
    betastar_hat = scalt$coefficients
    beta1_hat = scalc$coefficients
    alpha1_hat = apply(St, 2, mean) - apply(Sc, 2, mean)
    
    sigmat_hat = sd(Yt)
    sigmac_hat = sd(Yc)
    
    ## Estimate Omega
    out.omega = func_mosek_lp(A = Sigmast_hat, b = alpha1_hat, lambda_range = lambda_range)
    Omega_hat =  out.omega$Omega_hat

    ## calculate effect estimates and sd estimates
    est_id = Omega_hat %*% t(St_tilde) %*% (Yt_tilde - St_tilde %*% betastar_hat) / nt + alpha1_hat %*% betastar_hat
    est_total = mean(Yt) - mean(Yc)
        
    var_total = sigmat_hat^2 / nt + sigmac_hat^2 / nc
        
    sigmat2_hat = sqrt(max(0, sigmat_hat^2 - sigmat1_hat^2))
    sigmac2_hat = sqrt(max(0, sigmac_hat^2 - sigmac1_hat^2))

    
    var_id = ( sigmat2_hat^2 + sigmat1_hat^2 * Omega_hat %*% Sigmast_hat %*% t(Omega_hat)) / nt +
        ( t(betastar_hat) %*% Sigmasc_hat %*% betastar_hat ) / nc
    
    cov_did = ( sigmat2_hat^2 ) / nt + ( t(beta1_hat) %*% Sigmasc_hat %*% betastar_hat) / nc
    
    sd_id = sqrt(var_id)
    
    ##delta method
    Gradient = c(1 / est_total, - est_id / (est_total^2))
    W = matrix(c(var_id, cov_did, cov_did, var_total),2,2)
    var_R = Gradient %*% W %*% t(t(Gradient))
    
    est_R = est_id / est_total
    sd_R = sqrt(var_R)
    
    sd_total = sqrt(var_total)
    
    return(list = list(est_id = est_id,
                       sd_id = sd_id,
                       est_total = est_total,
                       sd_total = sd_total,
                       V = W,
                       est_R = est_R,
                       sd_R = sd_R,
                       lambda_used = out.omega$lambda_used))
    
}


func_mosek_lp <- function(A, b, lp_method = 'mosek_find', lambda_range = c(0,1)) {
    
    if (lp_method == 'mosek_find') {
        
        p = dim(A)[1]
        
        zero = matrix(0,p,p)
        f.obj <- c(rep(0,p),rep(1,p))
        f.con <- rbind( rbind(cbind(diag(p),-diag(p)),cbind(-diag(p),-diag(p))) , rbind(cbind(A,zero),cbind(-A,zero)) )
        f.dir <- rep("<=",4*(p))
        
        Omega_hat = matrix(0,1,p)
        
        lambda1 = lambda_range[1]
        lambda2 = lambda_range[2]
        f.rhs1 <- c( rep(0,2*(p)), (lambda1 + b),(lambda1 - b))
        f.rhs2 <- c( rep(0,2*(p)), (lambda2 + b),(lambda2 - b))
        
        ## use mosek
        lo1 <- function(f.rhs)
        {
            prob <- list()
            
            ## Objective sense (maximize or minimize)
            prob$sense <- "min"
            
            ## Objective coefficients
            prob$c <- f.obj
            ## Specify matrix 'A' in sparse format.
            prob$A <- Matrix( f.con, sparse = TRUE)
            
            ## Bound values for constraints
            prob$bc <- rbind(blc = rep(-Inf, 4*p), 
                             buc = f.rhs)
            
            ## Bound values for variables
            prob$bx <- rbind(blx = rep(-Inf, 2*p),  
                             bux = rep(Inf, 2*p))
            
            ## Solve the problem
            r <- mosek(prob)
            
            ## Return the solution
            r$sol
        }
        
        out1 = lo1(f.rhs = f.rhs1)
        out2 = lo1(f.rhs2)
        
        flag1 = I( max(f.con %*% out1$bas$xx - f.rhs1) <= 0.001)
        flag2 = I( max(f.con %*% out2$bas$xx - f.rhs2) <= 0.001)
        
        while (!(flag1) & (lambda2 - lambda1) > 0.01) {
            lambda_middle = (lambda1 + lambda2) / 2
            lambda_middle
            f.rhs_middle <- c( rep(0,2*(p)), (lambda_middle + b),(lambda_middle - b))
            out_middle = lo1(f.rhs_middle)
            flag_middle = I( max(f.con %*% out_middle$bas$xx - f.rhs_middle) <= 0.001)
            
            if (flag_middle) { #need smaller lambda
                lambda2 = lambda_middle
                flag2 = flag_middle
            } else { #need bigger lambda
                lambda1 = lambda_middle
                flag1 = flag_middle
            }
        }
        
        if (flag1) {
            f.rhs1 <- c( rep(0,2*(p)), (lambda1 + b),(lambda1 - b))
            out1 = lo1(f.rhs1)
            Omega_hat = t(out1$bas$xx[1:p])
            count_youjie = I( max(f.con %*% out1$bas$xx - f.rhs1) <= 0.001) #equal to flag1
            lambda_used = lambda1
        } else {
            f.rhs2 <- c( rep(0,2*(p)), (lambda2 + b),(lambda2 - b))
            out2 = lo1(f.rhs2)
            Omega_hat = t(out2$bas$xx[1:p])
            count_youjie = I( max(f.con %*% out2$bas$xx - f.rhs2) <= 0.001) #equal to flag2
            lambda_used = lambda2
        }
        
    }
    
    return(list = list(Omega_hat = Omega_hat,
                       lambda_used = lambda_used))
}

