

#' @keywords internal

prep_data_fitting <- function(
  data, 
  model_file,
  id,
  condition
) {
  
  data$id <- data[[id]]
  data$condition <- data[[condition]]
  col_freq <- get_eqn_categories(model_file)
  
  out <- list(
    conditions = unique(data[[condition]]),
    parameters = as.character(MPTinR::check.mpt(model_file)$parameters),
    col_freq = col_freq,
    trees = get_eqn_trees(model_file = model_file),
    freq_list = split(data[, col_freq], f = data[[condition]]),
    cols_ci = paste0("ci_", getOption("MPTmultiverse")$ci_size),
    data = data
  )
  out
}
