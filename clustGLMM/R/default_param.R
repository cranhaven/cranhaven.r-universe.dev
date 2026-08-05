set_dots <- function(x, ..., type = "numeric") {
  dots <- list(...)
  if (length(dots) > 0) {
      if (!all(names(dots) %in% names(x)))
          stop("Value(s) ",
               paste(sQuote(setdiff(names(dots), names(x))), collapse = ", "),
               " cannot be set.")

      if (!all(lengths(dots) == 1) || !all(vapply(dots, is, FALSE, type)) ||
          any(vapply(dots, anyNA, TRUE)))
          stop("Value(s) need to be specified using ", type, " scalars.")
      x[names(dots)] <- unlist(dots)
  }
  x
}

default_param <- function(sparse = TRUE, ...){
  param <- list(InvSigma_df = 1,
                InvQ_df = 1,
                prec_num_shp=1, 
                prec_num_rte=10,
                beta_num_fix_sd = 1,
                beta_num_sd = 1,
                beta_poi_fix_sd = 1,
                beta_poi_sd = 1,
                beta_bin_fix_sd = 1,
                beta_bin_sd = 1,
                beta_ord_fix_sd = 1,
                beta_ord_sd = 1,
                api_prior = 1,
                beta_cat_fix_sd = 1,
                beta_cat_sd = 1,
                init_b_sd = 0.01,
                e0_shp = ifelse(sparse, 1, 4), 
                e0_rte = ifelse(sparse, 100, 1),
                InvV = 0.01)
  param <- set_dots(param, ...)
  return(param)
}
