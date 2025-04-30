#' @keywords internal
#' @noRd

quantilep <- function(x, perc_str) {
  p <- as.numeric(sub(".*q", "",perc_str))/100
  p_names <- purrr::map_chr(p, ~ paste0("q",.x*100))
  p_funs <- purrr::map(p, ~ purrr::partial(stats::quantile, probs = .x, na.rm = TRUE)) %>%
    purrr::set_names(nm = p_names)
  return(p_funs)
}



