
#' @title Default arm-select function
#' 
#' @description Default arm-select function for selecting arm to the next stage.
#' 
#' @param W_2   Response rate for arm 2 (high dose)
#' @param W_1   Response rate for arm 1 (low dose)
#' @param delta Least difference to decide superiority of arm 2 (high dose)
#' 
#' @return The function is \eqn{g(W_2, W_1; \Delta) = 
#' 2I(W_2 - W_1 - \Delta > 0) + I(W_2 - W_1 - \Delta \leq 0)}. 
#' It returns the following values: 
#' 1: arm 1 (low dose) is selected; 
#' 2: arm 2 (high dose) is selected.
#' 
#' @examples
#' sel_g_func_default(W_2 = 0.5, W_1 = 0.3, delta = 0.05)
#' 
#' @export
sel_g_func_default = function(W_2, W_1, delta){
    temp = W_2 - W_1 - delta
    2*(temp > 0) + (temp <= 0)
}

#' @title Generate normal approximated test statistics for drop-the-losers (DTL) design
#' 
#' @description Generate normal approximated test statistics for drop-the-losers (DTL) design
#' 
#' @param nsim  Number of replicates
#' @param n     Sample size per arm at DTL look
#' @param q     Response rate under the null
#' @param t     A vector of information fraction of final stage
#' @param rho   Fixed correlation coefficient
#' 
#' @return Data frame of the simulated test statistics
#' 
#' @examples
#' \donttest{
#' dtl_sim_stat(nsim = 1000, n = 80, q = 0.3, t = c(0.3, 1), rho = c(0.5, 0.3))
#' }
#' 
#' @export
dtl_sim_stat = function(nsim, n, q, t, rho){
    
    mu = rep(0, 2*(1+length(t)))
    mu[1:2] = q
    
    Sigma_diag  = diag(1, nrow=length(mu))
    Sigma_upper = matrix(0, nrow=length(mu), ncol=length(mu))
    Sigma_lower = Sigma_upper
    
    Sigma_diag[1, 1] = q*(1-q)/n
    Sigma_diag[2, 2] = Sigma_diag[1,1] 
    
    Sigma_upper[1, 1+(1:length(t))*2] = rho*sqrt(q*(1-q)/n)
    Sigma_upper[2, 2+(1:length(t))*2] = Sigma_upper[1, 1+(1:length(t))*2]
    
    for (i in 3:length(mu)){
        for (j in i:length(mu)){
            if (i != j){
                temp = if_else(i%%2==1, (1/2)^((j-2)%%2==0), (1/2)^((j-2)%%2==1))
                Sigma_upper[i, j] = sqrt(t[ceiling((i-2)/2)]/t[ceiling((j-2)/2)])*temp
            }
        }
    }
    
    Sigma_lower = t(Sigma_upper)
    Sigma       = Sigma_lower + Sigma_upper + Sigma_diag
    
    data_stat = rmvnorm(n = nsim, mean=mu, sigma=Sigma) 
    
    W_names = c("W_1", "W_2")
    Z_names = c(sapply(1:length(t), function(x){paste0(c("Z_1", "Z_2"), x)}))
    colnames(data_stat) = c(W_names, Z_names)
    
    data.frame(data_stat)
    
}

#' @title Theoretical family-wise type I error rate (FWER) given a fixed 
#' correlation coefficient under drop-the-losers (DTL) design
#' 
#' @description Get the theoretical FWER alpha given fixed correlation coefficient
#' 
#' @param n        Sample size per arm at DTL look
#' @param t        A vector of information fraction of final stage
#' @param rho      Fixed correlation coefficient
#' @param q        Response rate under the null
#' @param alpha_s  Significance level for the final stage
#' @param delta    Least difference to decide superiority of high dose
#' 
#' @return Theoretical FWER alpha
#' 
#' @examples
#' \donttest{
#' # Without interim analysis
#' dtl_tier_the(n = 80, t = 1, rho = 0.4, q = 0.3, alpha_s = 0.025, delta = 0.05)
#' 
#' # With interim analysis
#' dtl_tier_the(n = 80, t = c(0.5, 1), rho = c(0.4, 0.2), q = 0.3, alpha_s = 0.025, delta = 0.05)
#' }
#' 
#' @export
dtl_tier_the <- function(n, t, rho, q, alpha_s, delta){

    if (length(t) == 1){

        c = qnorm(1-alpha_s)

        int_prob = function(z){
            sigma_11 = sqrt(2*q*(1-q)/n)
            d_1 = (sqrt(2)*delta/sigma_11 + z*rho)/sqrt(2-rho^2)
            d_2 = (sqrt(2)*delta/sigma_11 - z*rho)/sqrt(2-rho^2)
            int_prob = (pnorm(d_1) - pnorm(d_2))*dnorm(z)
            return(int_prob)
        }

        Type_I_Error =
            alpha_s +
            integrate(int_prob, lower = c, upper = Inf)$value

    } else if (length(t) > 1){

        OF_Design = gsDesign(k = length(t), test.type=1, sfu="OF", alpha = alpha_s, timing = t)
        c         = OF_Design$upper$bound

        int_prob = function(z){
            sigma_11 = sqrt(2*q*(1-q)/n)
            Sigma_22 = diag(length(t))
            for (i in 1:length(t)){
                for (j in i:length(t)){
                    Sigma_22[i,j] = sqrt(t[i]/t[j])
                    Sigma_22[j,i] = Sigma_22[i,j]
                }
            }
            d_1 = (sqrt(2)*delta/sigma_11 + t(rho)%*%solve(Sigma_22)%*%z)/sqrt(2-t(rho)%*%solve(Sigma_22)%*%rho)
            d_2 = (sqrt(2)*delta/sigma_11 - t(rho)%*%solve(Sigma_22)%*%z)/sqrt(2-t(rho)%*%solve(Sigma_22)%*%rho)
            int_prob = (pnorm(d_1) - pnorm(d_2))*dmvnorm(z, mean = rep(0,dim(Sigma_22)[1]), sigma = Sigma_22)
            return(int_prob)
        }

        Type_I_Error = alpha_s
        for (i in 1:length(t)){
            lower            = rep(-Inf, length(t))
            upper            = rep(Inf, length(t))
            lower[i]         = c[i]
            if (i != 1) {
                upper[1:(i-1)] = c[1:(i-1)]
            }
            Type_I_Error = Type_I_Error +
                cubature::cubintegrate(int_prob, lower = lower, upper = upper)$integral
        }

    }

    return(Type_I_Error)

}

dtl_get_tier = function(data_stat, g, t, alpha_s){
    
    sum_all = NULL
    
    if (length(t) == 1){
        c = qnorm(1-alpha_s)
    } else if (length(t) > 1){
        OF_Design = gsDesign(k = length(t), test.type=1, sfu="OF", alpha = alpha_s, timing = t)
        c         = OF_Design$upper$bound
    }
    
    Type_I_Error = sum(
        data_stat %>%
            mutate(case_when(g==2 ~ t(t(data_stat[2+(1:length(t))*2]) > c),
                             g==1 ~ t(t(data_stat[1+(1:length(t))*2]) > c), 
                             g==0 ~ FALSE)) %>%
            dplyr::select(-(1:(2+2*length(t)))) %>% 
            mutate(sum_all = rowSums(across(everything())) > 0) %>%
            dplyr::select(sum_all)
    ) / nrow(data_stat)
    
    return(Type_I_Error)
    
}

#' @title Simulated family-wise type I error rate (FWER) given a fixed 
#' correlation coefficient under drop-the-losers (DTL) design
#' 
#' @description Get the simulated FWER alpha given fixed correlation coefficient
#' 
#' @param nsim       Number of replicates
#' @param n          Sample size per arm at DTL look
#' @param t          A vector of information fraction of final stage
#' @param rho        Fixed correlation coefficient
#' @param q          Response rate under the null
#' @param alpha_s    Significance level for the final stage
#' @param sel_g_func Arm-select function. The default function is 
#'                   sel_g_func_default(W_2, W_1, delta). Users can define 
#'                   their own arm-select function. The format of 
#'                   the function must be function_name(W_2, W_1, ...). The
#'                   return values must be 1 (arm 1 is selected) or 2 (arm 2 
#'                   is selected) or 0 (stop for futility).
#' @param ...        Other arguments from sel_g_func.
#' 
#' @return Simulated FWER alpha
#' 
#' @examples
#' \donttest{
#' # Without interim analysis
#' dtl_tier_sim(nsim = 1000, n = 80, t = 1, rho = 0.4, q = 0.3, 
#'              alpha_s = 0.025, delta = 0.05)
#' 
#' # With interim analysis
#' dtl_tier_sim(nsim = 1000, n = 80, t = c(0.5, 1), rho = c(0.4, 0.2), q = 0.3, 
#'              alpha_s = 0.025, delta = 0.05)
#' }
#' 
#' @export
dtl_tier_sim = function(nsim, n, t, rho, q, alpha_s, 
                        sel_g_func = sel_g_func_default, ...){
    
    data_stat    = dtl_sim_stat(nsim, n, q, t, rho)
    g            = sel_g_func(W_2 = data_stat[2], W_1 = data_stat[1], ...)
    Type_I_Error = dtl_get_tier(data_stat, g, t, alpha_s)
    
    return(Type_I_Error)
    
}

#' @title Significance level given a fixed correlation coefficient for the 
#' final stage under drop-the-losers (DTL) design
#' 
#' @description Get significant level alpha_s based on a pre-specified FWER alpha 
#' given a fixed correlation coefficient for the final stage 
#' (reverse calculation of dtl_tier_the())
#' 
#' @param n      Sample size per arm at DTL look
#' @param t      A vector of information fraction of final stage
#' @param rho    Fixed correlation coefficient
#' @param q      Response rate under the null
#' @param alpha  A pre-specified FWER
#' @param delta  Least difference to decide superiority of high dose
#'
#' @return Significance level alpha_s for the final stage
#' 
#' @examples
#' # Without interim analysis
#' dtl_get_alpha_s(n = 80, t = 1, rho = 0.4, q = 0.3, alpha = 0.025, delta = 0.05)
#' 
#' @export
dtl_get_alpha_s = function(n, t, rho, q, alpha, delta){
    alpha_s = uniroot(
        function(x, alpha, n, t, rho, q, delta){
            dtl_tier_the(n, t, rho, q, x, delta) - alpha
        }, 
        alpha = alpha,
        n     = n,
        t     = t, 
        rho   = rho,
        q     = q, 
        delta = delta,
        lower = 0.001, 
        upper = 0.5)$root
    
    return(alpha_s)
}

#' @title Numerical significance level given a fixed correlation coefficient for the 
#' final stage under drop-the-losers (DTL) design
#' 
#' @description Get the numerical significant level alpha_s based on a pre-specified FWER alpha 
#' given a fixed correlation coefficient for the final stage by simulation
#' (reverse calculation of dtl_tier_sim())
#' 
#' @param nsim       Number of replicates
#' @param n          Sample size per arm at DTL look
#' @param t          A vector of information fraction of final stage
#' @param rho        Fixed correlation coefficient
#' @param q          Response rate under the null
#' @param alpha      A pre-specified FWER
#' @param sel_g_func Arm-select function. The default function is 
#'                   sel_g_func_default(W_2, W_1, delta). Users can define 
#'                   their own arm-select function. The format of 
#'                   the function must be function_name(W_2, W_1, ...). The
#'                   return values must be 1 (arm 1 is selected) or 2 (arm 2 
#'                   is selected) or 0 (stop for futility).
#' @param ...        Other arguments from sel_g_func.
#'
#' @return Significance level alpha_s for the final stage
#' 
#' @examples
#' \donttest{
#' # Without interim analysis
#' dtl_get_alpha_s_sim(nsim = 100000, n = 80, t = 1, rho = 0.4, q = 0.3, 
#'                     alpha = 0.025, delta = 0.05)
#' }
#' 
#' @export
dtl_get_alpha_s_sim = function(nsim = 100000, n, t, rho, q, alpha, 
                               sel_g_func = sel_g_func_default, ...){
    
    data_stat = dtl_sim_stat(nsim, n, q, t, rho)
    g         = sel_g_func(W_2 = data_stat[2], W_1 = data_stat[1], ...)
    
    alpha_s = uniroot(
        function(x, data_stat, g, t, alpha){
            dtl_get_tier(data_stat, g, t, x) - alpha
        }, 
        alpha      = alpha,
        data_stat  = data_stat,
        g          = g,
        t          = t,
        lower      = 0.001,
        upper      = 0.5)$root
    
    return(alpha_s)
}

#' @title Theoretical upper bound of correlation coefficient between 
#' time-to-event primary endpoint and binary surrogate endpoint
#' 
#' @description Get theoretical upper bound of correlation coefficient
#'
#' @param tau_k  Equals n/n_k, where n is the number of patients per treatment 
#'               arm at the DTL look and n_k is the number of patients in both 
#'               selected and control arms at the kth interim analysis.
#' @param pi_ar  Allocation rate of treatment and control (0.5 by default)
#' @param q      Response rate under the null
#' @param gamma  Hazards ratio of responders and non-responders
#' 
#' @return Theoretical upper bound of correlation coefficient
#' 
#' @examples
#' dtl_cor_the_PH_upper_bound(tau_k = 0.4, pi_ar = 0.5, q = 0.3, gamma = 0.2)
#' 
#' @export
dtl_cor_the_PH_upper_bound <- function(tau_k, pi_ar = 0.5, q, gamma){

    integrand = function(u) {
        return(1 / ( (1+q/(1-q)*u^(1-1/gamma))^2 * (q+(1-q)/gamma*u^(1/gamma-1)) ))
    }

    int = integrate(integrand, lower = 0, upper = 1)$value
    the = sqrt((1-pi_ar) * q/(1-q) * tau_k) * (1/gamma-1) * sqrt(int)
    return(the)

}
