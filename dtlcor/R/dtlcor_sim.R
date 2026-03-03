
#' @title Minimum significance level for the final stage under drop-the-losers 
#' (DTL) design
#' 
#' @description Get minimum significance level alpha_t (minimum of alpha_s) 
#' for the final analysis considering the ranges of response rate q and 
#' hazard ratio of responders and non-responders gamma given a pre-specified 
#' FWER alpha
#'
#' @param n          Number of patients per treatment arm at the DTL look.
#' @param N          Total number of patients in both selected and control arms 
#'                   at final analysis.
#' @param delta      Least difference to decide superiority of high dose.
#' @param q_seq      A vector of response rates under the null (can be 95% CI).
#' @param gamma_seq  A vector of hazards ratios of responders and non-responders
#'                   (can be 95% CI).
#' @param alpha      A pre-specified FWER.
#' @param fix_rho    Use fixed correlation coefficient or use theoretical upper
#'                   bound to get alpha_t. If = NULL, then it uses upper bound;
#'                   else if = real number between 0 and 1, then it use such 
#'                   number as fixed correlation coefficient.
#' 
#' @return A list of two data frames for minimum significance level alpha_t and 
#' significance level alpht_s given all combinations of q_seq and gamma_seq.
#' 
#' @examples
#' # Inputs
#' n         = 80    
#' N         = 152   
#' q_seq     = seq(0.19, 0.32, 0.01) 
#' gamma_seq = seq(0.14, 0.34, 0.01) 
#' alpha     = 0.025
#' delta     = 0.05  
#' 
#' # Use fixed correlation coefficient
#' dtl_app_get_alpha_t(n, N, q_seq, gamma_seq, alpha, fix_rho = 1, delta)
#' 
#' # Use theoretical upper bound 
#' dtl_app_get_alpha_t(n, N, q_seq, gamma_seq, alpha, fix_rho = NULL, delta)
#' 
#' @export
dtl_app_get_alpha_t = function(n, N, q_seq, gamma_seq, alpha, fix_rho = NULL, delta){
    
    alpha_s = NULL
    
    # sample size calculation (naive and ignoring correlation)
    seq_all = expand.grid(q_seq, gamma_seq)
    
    alpha_s_all = apply(seq_all, 1, function(x){
        q_s     = x[1]
        gamma_s = x[2]
        
        # calculate rho
        if (!is.null(fix_rho)){
            rho = fix_rho
        } else{
            rho = dtl_cor_the_PH_upper_bound(tau_k = n / N,
                                             pi_ar = 0.5,
                                             q     = q_s, 
                                             gamma = gamma_s)
        }
        alpha_s  = dtl_get_alpha_s(n, 1, rho, q_s, alpha, delta) # no interim t = 1
        data.frame(q_s, gamma_s, rho, alpha_s)
    })
    rst_alpha_s = data.frame(t(sapply(1:length(alpha_s_all), function(x){as.numeric(alpha_s_all[[x]])})))
    colnames(rst_alpha_s) = c("q", "gamma", "rho", "alpha_s")
    
    rst_alpha_t = unique(rst_alpha_s %>%
                             filter(alpha_s == min(rst_alpha_s$alpha_s)) %>%
                             rename_with(~"alpha_t", "alpha_s"))
    
    list(rst_alpha_t = rst_alpha_t, rst_alpha_s = rst_alpha_s)
    
}

#' @title Numerical minimum significance level for the final stage under 
#' drop-the-losers (DTL) design
#' 
#' @description Get numerical minimum significance level alpha_t (minimum of alpha_s) 
#' for the final analysis considering the ranges of response rate q and 
#' hazard ratio of responders and non-responders gamma given a pre-specified 
#' FWER alpha
#'
#' @param nsim       Number of replicates.
#' @param n          Number of patients per treatment arm at the DTL look
#' @param N          Total number of patients in both selected and control arms 
#'                   at final analysis.
#' @param q_seq      A vector of response rates under the null (can be 95% CI).
#' @param gamma_seq  A vector of hazards ratios of responders and non-responders
#'                   (can be 95% CI).
#' @param alpha      A pre-specified FWER.
#' @param fix_rho    Use fixed correlation coefficient or use theoretical upper
#'                   bound to get alpha_t. If = NULL, then it uses upper bound;
#'                   else if = real number between 0 and 1, then it use such 
#'                   number as fixed correlation coefficient.
#' @param sel_g_func Arm-select function. The default function is 
#'                   sel_g_func_default(W_2, W_1, delta). Users can define 
#'                   their own arm-select function. The format of 
#'                   the function must be function_name(W_2, W_1, ...). The
#'                   return values must be 1 (arm 1 is selected) or 2 (arm 2 
#'                   is selected) or 0 (stop for futility).
#' @param ...        Other arguments from sel_g_func.
#' 
#' @return A list of two data frames for numerical minimum significance level 
#' alpha_t and significance level alpht_s given all combinations of q_seq 
#' and gamma_seq.
#' 
#' @examples
#' \donttest{
#' # Inputs
#' set.seed(1000)
#' nsim      = 100000
#' n         = 80    
#' N         = 152   
#' q_seq     = seq(0.19, 0.32, 0.01) 
#' gamma_seq = seq(0.14, 0.34, 0.01) 
#' alpha     = 0.025
#' delta     = 0.05  
#' 
#' # Use fixed correlation coefficient
#' dtl_app_get_alpha_t_sim(nsim, n, N, q_seq, gamma_seq, alpha, 
#'                         fix_rho = 1, delta = delta)
#' 
#' # Use theoretical upper bound 
#' dtl_app_get_alpha_t_sim(nsim, n, N, q_seq, gamma_seq, alpha, 
#'                         fix_rho = NULL, delta = delta)
#' }
#' 
#' @export
dtl_app_get_alpha_t_sim = function(nsim = 100000, n, N, q_seq, gamma_seq, alpha, fix_rho = NULL, 
                                   sel_g_func = sel_g_func_default, ...){
    
    alpha_s = NULL
    
    # sample size calculation (naive and ignoring correlation)
    seq_all = expand.grid(q_seq, gamma_seq)
    
    alpha_s_all = apply(seq_all, 1, function(x){
        q_s     = x[1]
        gamma_s = x[2]
        
        # calculate rho
        if (!is.null(fix_rho)){
            rho = fix_rho
        } else{
            rho = dtl_cor_the_PH_upper_bound(tau_k = n / N,
                                             pi_ar = 0.5,
                                             q     = q_s,
                                             gamma = gamma_s)
        }
        alpha_s  = dtl_get_alpha_s_sim(nsim, n, 1, rho, q_s, alpha, sel_g_func, ...) # no interim t = 1
        c(q_s, gamma_s, rho, alpha_s)
    })
    
    rst_alpha_s = data.frame(t(alpha_s_all))
    colnames(rst_alpha_s) = c("q", "gamma", "rho", "alpha_s")
    
    rst_alpha_t = unique(rst_alpha_s %>%
                             filter(alpha_s == min(rst_alpha_s$alpha_s)) %>%
                             rename_with(~"alpha_t", "alpha_s"))
    
    list(rst_alpha_t = rst_alpha_t, rst_alpha_s = rst_alpha_s)
    
}

# get survival function of non-responder
get_S0 = function(mPFS, q, gamma, t){
    
    if (length(mPFS) == 1 & length(q) == 1){
        
        sapply(t, function(x){
            S = 1 - pexp(x, log(2) / mPFS)
            
            solve_S0 = function(y){
                q*y^gamma + (1-q)*y - S
            }
            
            uniroot(solve_S0, interval = c(0, 1), tol = 10^(-6))$root
        })
        
    } else if (length(t) == length(mPFS) & length(mPFS) == length(q)){
        
        mapply(function(x, y, z){
            S = 1 - pexp(x, log(2) / y)
            
            solve_S0 = function(y){
                z*y^gamma + (1-z)*y - S
            }
            
            uniroot(solve_S0, interval = c(0, 1), tol = 10^(-6))$root
        }, x = t, y = mPFS, z = q)
        
    } else{
        stop("Dimensions of mPFS, q, t are not consistent.")
    }
    
}

# get density function of non-responder
get_f0 <- function(mPFS, q, gamma, t){
    S0 = get_S0(mPFS, q, gamma, t)
    f0 = dexp(t, log(2) / mPFS) / (q*gamma*S0^(gamma-1) + (1-q))
    f0
}

#' @title Simulate a single drop-the-losers (DTL) trial.
#' 
#' @description Simulate a single trial based on the DTL design
#'
#' @param D          Total number of events.
#' @param N          Total number of patients in both selected and control arms 
#'                   at final analysis.
#' @param n          Number of patients per treatment arm at the DTL look.
#' @param mPFS       A 3-entry vector of median progression-free survival times
#'                   for control, low dose and high dose arms (assume 
#'                   exponential time-to-event outcome for all arms and the 
#'                   conditional distribution for responders and non-responders
#'                   can be uniquely identified given q and gamma).
#' @param q          A 3-entry vector of response rates under the null.
#' @param gamma      Hazards ratio of responders and non-responders.
#' @param drop_rate  Annual drop-out rate.
#' @param enroll     Annual Enrollment rate.
#' @param interim_t  A vector of information fractions of final stage.
#' @param sel_g_func Arm-select function. The default function is 
#'                   sel_g_func_default(W_2, W_1, delta). Users can define 
#'                   their own arm-select function. The format of 
#'                   the function must be function_name(W_2, W_1, ...). The
#'                   return values must be 1 (arm 1 is selected) or 2 (arm 2 
#'                   is selected) or 0 (stop for futility).
#' @param ...        Other arguments from sel_g_func.
#'              
#' @return A list including (1) a data frame of response rates of low dose and 
#' high dose W_1, W_2 and the log-rank test statistics Z_jk at kth interim 
#' analysis if the jth arm is selected at DTL look; (2) data frames of 
#' simulated data at DTL look; (3) data frames of simulated data at 
#' interim or final analyses.
#' 
#' @examples
#' # Inputs
#' set.seed(1000)
#' D           = 162
#' N           = 152  
#' n           = 80    
#' mPFS        = c(180, 276, 300)
#' q           = c(0.2, 0.4, 0.5)
#' gamma       = 0.15
#' drop_rate   = 0.05
#' enroll      = 20 * 12
#' interim_t   = c(0.5, 1)
#' delta       = 0.05  
#' 
#' # Run function
#' dtl_app_sim_single(D, N, n, mPFS, q, gamma, drop_rate, enroll, interim_t, delta = delta)
#'                   
#' @export
dtl_app_sim_single <- function(D, N, n, mPFS, q, gamma, drop_rate, enroll, interim_t, 
                               sel_g_func = sel_g_func_default, ...){
    
    tt     = NULL
    censor = NULL
    
    accr_time = N / enroll * 365.25 # day per arm
    
    q_rep    = rep(q, each = N)
    mPFS_rep = rep(mPFS, each = N)
    lambda_C = -log(1-drop_rate) / 365.25 # censor survival at 1 year equals 1 - drop_rate
    
    ID       = 1:(3*N)
    arm      = rep(0:2, each = N)
    
    Eve_Time = rexp(3*N, log(2) / mPFS_rep)
    
    f0       = get_f0(mPFS_rep, q_rep, gamma, Eve_Time)
    q_t      = 1 - (1-q_rep)*f0 / dexp(Eve_Time, log(2)/mPFS_rep)
    X        = rbinom(3*N, 1, q_t)
    
    Cen_Time = rexp(3*N, lambda_C)
    tt_accr  = runif(3*N, 0, accr_time)
    tt_eve   = tt_accr + Eve_Time
    tt_cen   = tt_accr + Cen_Time
    
    dat_final_temp = tibble(ID, arm, X, Eve_Time, Cen_Time, tt_accr, tt_eve, tt_cen) %>%
        mutate(censor = case_when(tt_eve < tt_cen ~ 0,
                                  tt_cen <= tt_eve ~ 1),
               tt     = case_when(censor == 0 ~ tt_eve,
                                  censor == 1 ~ tt_cen)) %>%
        arrange(tt) %>%
        mutate(Time   = tt - tt_accr,
               Delta  = if_else(censor == 0, 1, 0))
    
    
    # DTL stage
    dat_DTL = dat_final_temp %>%
        arrange(tt_accr) %>% 
        group_by(arm) %>% 
        slice(1:n) %>%
        ungroup() %>%
        mutate(tt_end = max(tt_accr),
               censor = if_else(tt_end <= tt, 2, censor),
               tt     = if_else(censor == 2, tt_end, tt),
               Time   = tt - tt_accr,
               Delta  = if_else(censor == 0, 1, 0)) %>%
        arrange(tt)
    
    rst_W   = dat_DTL %>% 
        filter(arm!=0) %>% 
        group_by(arm) %>% 
        summarise(W = mean(X))
    
    W_names   = c("W_1", "W_2")
    W_dat_all = data.frame(rbind(rst_W$W))
    colnames(W_dat_all) = W_names
    
    arm_sel = sel_g_func(W_dat_all[2], W_1 = W_dat_all[1], ...)
    
    if (arm_sel == 2){
        dat_final_temp_2 = dat_final_temp %>% filter(arm != 1)
        
    } else if (arm_sel == 1){
        dat_final_temp_2 = dat_final_temp %>% filter(arm != 2)
        
    } else if (arm_sel == 0){ # stop for futility
        return(list(dat_WZ = W_dat_all, dat_DTL = dat_DTL, dat_final = NULL))
        
    }
    
    # Final stage
    dat_final_temp_3 = dat_final_temp_2 %>%
        mutate(D_cumsum = cumsum(censor==0))
    
    t_length  = length(interim_t)
    dat_final = list()
    Z_all     = NULL
    for (k in 1:t_length){
        
        D_k = ceiling(D*interim_t[k])
        
        if (max(dat_final_temp_3$D_cumsum) < D_k){
            tt_end = max(dat_final_temp_3$tt)
        } else{
            tt_end = min(dat_final_temp_3$tt[dat_final_temp_3$D_cumsum == D_k])
        }
        
        dat_final[[k]] = dat_final_temp_3 %>%
            mutate(tt_end = tt_end,
                   censor = if_else(tt > tt_end, 2, censor),
                   tt     = if_else(censor != 2, tt, tt_end),
                   Time   = tt - tt_accr,
                   Delta  = if_else(censor == 0, 1, 0))
        
        rst_test = logrank_test(Surv(Time, Delta) ~ factor(arm), data = dat_final[[k]])
        Z        = -rst_test@statistic@teststatistic
        
        Z_all = c(Z_all,
                  c(if_else(1 %in% dat_final[[k]]$arm, Z, NA),
                    if_else(2 %in% dat_final[[k]]$arm, Z, NA)))
        
    }
    
    Z_names = apply(expand.grid(1:2, 1:t_length), 1, function(x){
        paste0("Z_", x[1], x[2])
    })
    
    Z_dat_all           = data.frame(rbind(Z_all))
    rownames(Z_dat_all) = NULL
    colnames(Z_dat_all) = Z_names
    
    dat_WZ = data.frame(W_dat_all, Z_dat_all)
    
    return(list(dat_WZ = dat_WZ, dat_DTL = dat_DTL, dat_final = dat_final))
    
}

dtl_app_ana <- function(dat_all, interim_t, interim_c){
    
    if (is.null(dat_all$dat_final)){
        
        dat_DTL = dat_all$dat_DTL
        
        arm_sel  = 0
        rej      = 0
        
        cen_rate = mean(dat_DTL$censor!=0)
        cen_1    = mean(dat_DTL$censor==1) 
        cen_2    = mean(dat_DTL$censor==2)
        dur      = dat_DTL$tt_end[1] - min(dat_DTL$tt_accr)
        
        rst = data.frame(t = rbind(interim_t), c = rbind(interim_c), arm_sel, rej, cen_rate, cen_1, cen_2, dur)
        rownames(rst) = NULL
        
        return(rst)
        
    } else {
        
        t_length     = length(interim_t)
        dat_WZ       = dat_all$dat_WZ
        dat_DTL      = dat_all$dat_DTL
        dat_final    = dat_all$dat_final[[length(dat_all$dat_final)]]
        
        stop_interim = if_else(1 %in% dat_final$arm,
                               which(dat_WZ[1, 1 + 2*(1:t_length)] > interim_c)[1],
                               which(dat_WZ[1, 2 + 2*(1:t_length)] > interim_c)[1])
        stop_interim = if_else(is.na(stop_interim), length(dat_all$dat_final), stop_interim)
        
        arm_sel = if_else(2 %in% dat_final$arm, 2, 1)
        rej     = case_when(1 %in% dat_final$arm & !all(dat_WZ[1, 1 + 2*(1:t_length)] <= interim_c) ~ 1,
                            2 %in% dat_final$arm & !all(dat_WZ[1, 2 + 2*(1:t_length)] <= interim_c) ~ 2,
                            .default = 0)
        
        cen_rate  = mean(dat_final$censor!=0)
        cen_1     = mean(dat_final$censor==1)
        cen_2     = mean(dat_final$censor==2)
        dur       = dat_all$dat_final[[stop_interim]]$tt_end[1] - min(dat_DTL$tt_accr)
        
        rst = data.frame(t = rbind(interim_t), c = rbind(interim_c), rej, dur, arm_sel, cen_rate, cen_1, cen_2)
        rownames(rst) = NULL
        
    }
    
    return(rst)
    
}

#' @title Simulation study for drop-the-losers (DTL) trial.
#' 
#' @description Simulation study for a trial based on the DTL design
#'
#' @param nsim       Number of replicates.
#' @param alpha_t    significance level for the final stage (recommend to 
#'                   use minimum significance level alpha_t to control 
#'                   family-wise type I error rate).
#' @param D          Total number of events.
#' @param N          Total number of patients in both selected and control arms 
#'                   at final analysis. 
#' @param n          Number of patients per treatment arm at the DTL look.
#' @param mPFS       A 3-entry vector of median progression-free survival times
#'                   (in days) for control, low dose and high dose arms. 
#' @param q          A 3-entry vector of response rates under the null.
#' @param gamma      Hazards ratio of responders and non-responders.
#' @param drop_rate  Annual drop-out rate.
#' @param enroll     Annual enrollment rate.
#' @param interim_t  A vector of information fractions of final stage.
#' @param sel_g_func Arm-select function. The default function is 
#'                   sel_g_func_default(W_2, W_1, delta). Users can define 
#'                   their own arm-select function. The format of 
#'                   the function must be function_name(W_2, W_1, ...). The
#'                   return values must be 1 (arm 1 is selected) or 2 (arm 2 
#'                   is selected) or 0 (stop for futility).
#' @param ...        Other arguments from sel_g_func.
#'                  
#' @return A one row data frame of simulation results, including the parameter
#' settings, the O'Brien-Fleming boundaries for interim and final analyses: 
#' c.1, c.2, the overall censoring rate: cen_rate, the mean study duration: dur,
#' the probability of selecting high dose / low dose / no dose: prob_sel_2, 
#' prob_sel_1, prob_sel_0, the probability of rejecting 
#' H_1 or H_2: rej_12, the probability of rejecting H_1 only: rej_1, 
#' the probability of rejecting H_2 only: rej_2.
#' 
#' @examples
#' \donttest{
#' # Inputs
#' set.seed(1000)
#' nsim        = 1000
#' alpha_t     = 0.018
#' D           = 162
#' N           = 152  
#' n           = 80    
#' mPFS        = c(180, 276, 300)
#' q           = c(0.2, 0.4, 0.5)
#' mPFS_null   = rep(180, 3)
#' q_null      = rep(0.2, 3)
#' gamma       = 0.15
#' drop_rate   = 0.05
#' enroll      = 20 * 12
#' interim_t   = c(0.5, 1)
#' delta       = 0.05  
#' 
#' # Type I Error
#' dtl_app_sim(nsim, alpha_t, D, N, n, mPFS_null, q_null, gamma, drop_rate, 
#'             enroll, interim_t, delta = delta)
#' 
#' # Power
#' dtl_app_sim(nsim, alpha_t, D, N, n, mPFS, q, gamma, drop_rate, enroll, 
#'             interim_t, delta = delta)
#' }
#' 
#' @export
dtl_app_sim <- function(nsim, alpha_t,
                        D, N, n, mPFS, q, gamma,
                        drop_rate, enroll, interim_t, 
                        sel_g_func = sel_g_func_default, ...){
    
    arm_sel  = NULL
    rej      = NULL
    dur      = NULL
    cen_rate = NULL
    cen_1    = NULL
    cen_2    = NULL
    
    t_length  = length(interim_t)
    
    if (t_length > 1){
        OF_Design = gsDesign(k = t_length, test.type=1, sfu="OF", alpha = alpha_t, timing = interim_t)
        interim_c = OF_Design$upper$bound
    } else if (t_length == 1){
        interim_c = qnorm(1 - alpha_t)
    }
    
    rst_all = NULL
    for (i in 1:nsim){
        # simulate data
        dat_all  = dtl_app_sim_single(D, N, n, mPFS, q, gamma, drop_rate, enroll, interim_t, 
                                      sel_g_func, ...)
        
        # analysis data
        rst     = dtl_app_ana(dat_all, interim_t, interim_c)
        rst_all = rbind(rst_all, 
                        data.frame(rep     = i, 
                                   alpha_t = alpha_t,
                                   rst))
    }
    
    rst_final = rst_all %>%
        mutate(prob_sel_2    = mean(arm_sel == 2),
               prob_sel_1    = mean(arm_sel == 1),
               prob_sel_0    = mean(arm_sel == 0),
               rej_12        = mean(rej == 1 | rej ==2),
               rej_1         = mean(rej == 1),
               rej_2         = mean(rej == 2),
               dur_mean      = mean(dur),
               cen_rate_mean = mean(cen_rate),
               cen_1_mean    = mean(cen_1),
               cen_2_mean    = mean(cen_2)) %>%
        select(-c(rep, arm_sel, rej, dur, cen_rate, cen_1, cen_2)) %>%
        unique()
    
    rst_dtl = data.frame(mPFS_0       = mPFS[1],
                         mPFS_1       = mPFS[2],
                         mPFS_2       = mPFS[3],
                         q_0          = q[1],
                         q_1          = q[2],
                         q_2          = q[3],
                         gamma        = gamma,
                         drop_rate    = drop_rate,
                         enroll       = enroll,
                         D            = D,
                         N            = N,
                         n            = n,
                         rst_final)
    
    return(rst_dtl)
}

