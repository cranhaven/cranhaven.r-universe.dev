default_tuning <- function(experience = FALSE, ...){
  tuning <- list()
  tuning$integer <- list(
    freq_proposal_update = 10,
    times_proposal = ifelse(experience, 10, 1),
    maxiter = 25,
    maxnrep = 100,
    kspec_bi_cat = 0
  )
  tuning$double <- list(
    const_proposal_beta_poi_fix = 1,
    const_proposal_beta_poi = 1,
    const_proposal_beta_bin_fix = 1,
    const_proposal_beta_bin = 1,
    const_proposal_beta_ord_fix = ifelse(experience, 0.5, 1),
    const_proposal_beta_ord = ifelse(experience, 0.5, 1),
    const_proposal_beta_cat_fix = 1,
    const_proposal_beta_cat = 1,
    const_proposal_a_ord = ifelse(experience, 0.5, 1),
    const_proposal_b = ifelse(experience, 0.5, 1),
    const_proposal_e0 = 1,
    tolerance = 1e-7
  )
  dots <- list(...)
  dots_int <- intersect(names(dots), names(tuning$integer))
  dots_double <- intersect(names(dots), names(tuning$double))
  if (length(dots) != length(dots_int) + length(dots_double))
      stop("Value(s) ",
           paste(sQuote(setdiff(names(dots),
                                c(names(tuning$integer),
                                  names(tuning$double)))),
                 collapse = ", "),
           " cannot be set.")
  tuning$integer <- do.call("set_dots", c(list(x = tuning$integer),
                                          dots[dots_int]))
  tuning$double <- do.call("set_dots", c(list(x = tuning$double),
                                         dots[dots_double]))
  return(tuning)
}
