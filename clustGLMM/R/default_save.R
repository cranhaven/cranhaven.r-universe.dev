default_save <- function(...){
  save <-
      c(beta_num_fix = TRUE, beta_num = TRUE,
        prec_num = TRUE, sd_num = TRUE, var_num = TRUE,
        beta_poi_fix = TRUE, beta_poi = TRUE,
        beta_bin_fix = TRUE, beta_bin = TRUE,
        beta_ord_fix = TRUE, beta_ord = TRUE,
        c_ord = TRUE, a_ord = FALSE, pi_ord = TRUE,
        beta_cat_fix = TRUE, beta_cat = TRUE,
        InvSigma = TRUE, Sigma = TRUE, sdSigma = TRUE,
        corSigma = TRUE, detInvSigma = FALSE,
        InvQ = FALSE, Q = FALSE, detInvQ = FALSE,
        b = FALSE,
        w = TRUE, ng = TRUE,
        loglik = TRUE, pUig = FALSE, U = TRUE,
        Gplus = TRUE, e0 = TRUE,
        naY = FALSE
  )
  save <- set_dots(save, ..., type = "logical")
  return(save)
}
