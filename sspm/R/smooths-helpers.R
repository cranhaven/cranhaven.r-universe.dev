
# Helpers for smoothing functions -----------------------------------------

# Creates a lag matrix given a parameter for offset k, and add the proper
# columns for model ingestion
make_lag_matrix <- function(data_frame, k, boundaries, time){

  boundary_col <- spm_boundary(boundaries)

  lag_mat <- as.data.frame(matrix(-(1:k), nrow = nrow(data_frame),
                                  ncol = k, byrow = TRUE)) %>%
    dplyr::rename_all(.funs = gsub, pattern = "V", replacement = "lag") %>%
    dplyr::mutate(!!time := data_frame[[time]],
                  !!boundary_col := data_frame[[boundary_col]],
                  "patch_id" = data_frame[["patch_id"]]) %>%
    dplyr::select(dplyr::contains('lag')) %>%
    as.matrix()

  return(lag_mat)
}

# Make a by matrix with multilag
make_by_matrix <- function(data_frame, k, boundaries, time, var){

  boundary_col <- spm_boundary(boundaries)

  by_mat <- data_frame %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select("patch_id", !!boundary_col, !!time, !!var) %>%
    dplyr::nest_by(.data$patch_id, !!boundary_col := .data[[boundary_col]]) %>%
    dplyr::mutate(lags = list(multilag(variable = .data$data[[var]],
                                       n_lags = k,
                                       # TODO: assuming in-group mean as default
                                       default = mean(.data$data[[var]],
                                                      na.rm = T)))) %>%
    tidyr::unnest(cols = c("lags", "data")) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::contains('lag')) %>%
    dplyr::select(-dplyr::contains(var)) %>%
    as.matrix()

  return(by_mat)

}

# Accessory functions -----------------------------------------------------

# This functions turns the args_and_vars returned by ICAR (and potentially any
# any other functions like ICAR) into a call to a smooth (s, ti, etc...)
assemble_smooth <- function(s_type, args) {

  checkmate::assert_character(s_type)
  checkmate::assert_list(args)

  deparse(rlang::call2(s_type, !!!args),
          width.cutoff = 500, nlines = 1)
}

# Dispatch the correct function based on the name of the method
dispatch_smooth <- function(smooth_method) {

  checkmate::assert_character(smooth_method)

  if (smooth_method == "ICAR") {
    return(ICAR)
  } else if (smooth_method == "LINPRED") {
    return(LINPRED)
  } else {
    cli::cli_alert_danger(paste0("Smoothing method '", smooth_method,
                                 "' is not part of the supported methods."))
    cli::cli_alert_info("See `?spm_smooth_methods()`")
  }
}

# This function generates multilag values for a given vector
multilag <- function(variable, n_lags, default = NA) {
  out_mat <- sapply(1:n_lags, FUN = dplyr::lag, x = variable, default = default)
  colnames(out_mat) <- paste0("lag", 1:n_lags)
  as.data.frame(out_mat)
}
