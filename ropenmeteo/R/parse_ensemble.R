pivot_ensemble_forecast <- function(df){
  df |>
    tidyr::pivot_longer(-time, names_to = "variable_ens", values_to = "prediction") |>
    dplyr::mutate(
      variable = stringr::str_split(
        variable_ens,
        pattern = "_member",
        n = 2,
        simplify = TRUE
      )[, 1],
      ensemble = stringr::str_split(
        variable_ens,
        pattern = "_member",
        n = 2,
        simplify = TRUE
      )[, 2],
      ensemble = ifelse(ensemble == "", "00",ensemble)) |>
    dplyr::select(-variable_ens)
}


