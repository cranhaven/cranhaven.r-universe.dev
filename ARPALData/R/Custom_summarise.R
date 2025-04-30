#' @keywords internal
#' @noRd

Custom_summarise <- function(grouped_data,var_vec,fns_vec) {

  '%notin%' <- Negate('%in%')

  # Wind direction can only be averaged
  if(sum(var_vec %in% c("Wind_direction","Wind_direction_gust") & fns_vec != "mean") > 0) {
    stop("Error: on the Wind_direction and Wind_direction_gust is possible to calculate only the average value. Use 'mean' in 'Fns_vec.'",
         call. = FALSE)
  }

  # Wind speed can only be averaged, maximized or minimized
  if(sum(var_vec %in% c("Wind_speed","Wind_speed_gust") & fns_vec %notin% c("mean","min","max")) > 0) {
    stop("Error: on the Wind_speed and Wind_speed_gust is possible to calculate only mean, max or min values. Use 'mean' or 'max' or 'min' in 'Fns_vec.'",
         call. = FALSE)
  }

  # Checks if all the selected variables are available for the actual dataset
  if (all(dplyr::all_of(var_vec) %in% names(grouped_data)) == F) {
    stop("Error: one ore more measures are not avaiable for the selected stations! Change the values of 'Var_vec'",
         call. = FALSE)
  }

  vv_vec <- (duplicated(var_vec,fromLast = T) | duplicated(var_vec,fromLast = F))

  summ_data <- grouped_data %>%
    dplyr::summarise(dplyr::across(var_vec[vv_vec & var_vec %in% c("Wind_speed","Wind_speed_gust") & fns_vec=="mean"], ~ Wind_averaging(Wind_speed,Wind_direction)$Wind_speed,.names = "{.col}_mean"),
                     dplyr::across(var_vec[!vv_vec & var_vec %in% c("Wind_speed","Wind_speed_gust") & fns_vec=="mean"], ~ Wind_averaging(Wind_speed,Wind_direction)$Wind_speed),
                     dplyr::across(var_vec[vv_vec & var_vec %in% c("Wind_direction","Wind_direction_gust") & fns_vec=="mean"], ~ Wind_averaging(Wind_speed,Wind_direction)$Wind_direction,.names = "{.col}_mean"),
                     dplyr::across(var_vec[!vv_vec & var_vec %in% c("Wind_direction","Wind_direction_gust") & fns_vec=="mean"], ~ Wind_averaging(Wind_speed,Wind_direction)$Wind_direction),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="mean"], ~ mean(.x, na.rm=T),.names = "{.col}_mean"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="mean"], ~ mean(.x, na.rm=T)),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="median"], ~ median(.x, na.rm=T),.names = "{.col}_median"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="median"], ~ median(.x, na.rm=T)),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & grepl('\\bq[0-9]+$',fns_vec)],
                                     list(!!!quantilep(.data$.x,fns_vec[vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & grepl('\\bq[0-9]+$',fns_vec)])),
                                   .names = "{.col}_{.fn}"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & grepl('\\bq[0-9]+$',fns_vec)],
                                     list(!!!quantilep(.data$.x,fns_vec[!vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & grepl('\\bq[0-9]+$',fns_vec)]))),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="sum"], ~ sum(.x, na.rm=T),.names = "{.col}_cum"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="sum"], ~ sum(.x, na.rm=T)),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_direction","Wind_direction_gust") & fns_vec=="min"], ~ min(.x, na.rm=T),.names = "{.col}_min"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_direction","Wind_direction_gust") & fns_vec=="min"], ~ min(.x, na.rm=T)),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_direction","Wind_direction_gust") & fns_vec=="max"], ~ max(.x, na.rm=T),.names = "{.col}_max"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_direction","Wind_direction_gust") & fns_vec=="max"], ~ max(.x, na.rm=T)),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="sd"], ~ sd(.x, na.rm=T),.names = "{.col}_sd"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="sd"], ~ sd(.x, na.rm=T)),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="var"], ~ var(.x, na.rm=T),.names = "{.col}_var"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="var"], ~ var(.x, na.rm=T)),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="vc"], ~ sd(.x, na.rm=T)/mean(.x, na.rm=T),.names = "{.col}_vc"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="vc"], ~ sd(.x, na.rm=T)/mean(.x, na.rm=T)),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="skew"], ~ mom_skew(.x, na.rm=T),.names = "{.col}_skew"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="skew"], ~ mom_skew(.x, na.rm=T)),
                     dplyr::across(var_vec[vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="kurt"], ~ mom_kurt(.x, na.rm=T),.names = "{.col}_kurt"),
                     dplyr::across(var_vec[!vv_vec & var_vec %notin% c("Wind_speed","Wind_direction","Wind_speed_gust","Wind_direction_gust") & fns_vec=="kurt"], ~ mom_kurt(.x, na.rm=T))) %>%
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))

  # Drop non-useful columns concerning the quantiles
  if (sum(grepl('\\q[0-9]+$',names(summ_data))) > 0) {
    '%notin%' <- Negate('%in%')
    names_full <- names(summ_data)[grepl('\\q[0-9]+$',names(summ_data))]
    to_drop <- names_full[names_full %notin% paste0(var_vec,"_",fns_vec)]
    summ_data <- summ_data %>%
      dplyr::select(-to_drop)
  }

  return(summ_data)
}
