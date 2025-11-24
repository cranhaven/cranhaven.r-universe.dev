
# Aggregation helpers -----------------------------------------------------

spm_aggregate_routine <- function(dataset_data, boundaries, group_by, level,
                                  time_col, boundary, variable, fun, fill,
                                  apply_to_df, ...){

  # Group data
  dataset_data_tmp <- group_data(dataset_data, group_by, level,
                                 time_col, boundary)

  args_list <- list(...)

  # Two ways to aggregate: by dataframe or by list. We get the correct
  # meta-function from this function
  the_fun <- get_apply_fun(apply_to_df)

  # Then we apply it
  dataset_data_tmp <- dataset_data_tmp %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_modify(.f = the_fun,
                        ... = list(variable = variable,
                                   fun = fun,
                                   args = args_list)) %>%
    dplyr::ungroup() %>%
    unique()

  # We determine how best to clean up if need be
  if (checkmate::test_logical(fill)) {
    if (is.na(fill)){
      do_completion <- TRUE
      fill_value <- fill
    } else {
      if (fill) {
        do_completion <- FALSE
        warning("No fill values provided, completion skipped")
      } else {
        do_completion <- FALSE
      }
    }
  } else {
    if (checkmate::test_function(fill)){
      do_completion <- TRUE
      fill_value <- do.call(fill,
                            append(list(dataset_data[[variable]]),
                                   args_list))
    } else {
      do_completion <- TRUE
      fill_value <- fill
    }
  }

  # If we need to complete, we go ahead and do so
  if (do_completion) {

    old_units <- units(dataset_data_tmp$temp)

    if (level == "patch"){

      dataset_data_tmp <- dataset_data_tmp %>%
        dplyr::mutate(temp = as.numeric(.data$temp)) %>%
        tidyr::complete(.data[[time_col]], .data$patch_id,
                        fill = list(temp = as.numeric(fill_value)))

    } else if (level == "boundary"){

      dataset_data_tmp <- dataset_data_tmp %>%
        dplyr::mutate(temp = as.numeric(.data$temp)) %>%
        tidyr::complete(.data[[time_col]], .data[[boundary]],
                        fill = list(temp = as.numeric(fill_value)))

    }

    units(dataset_data_tmp$temp) <- old_units

  }

  # Rename before returning
  dataset_data_tmp <- dataset_data_tmp %>%
    dplyr::rename(!!variable := "temp")

  return(dataset_data_tmp)

}

# This functions takes care of applying the proper grouping onto the data
group_data <- function(dataset_data, group_by, level, time_col, boundary){
  if (group_by == "spacetime"){

    if (level == "patch"){

      grouped_data <- dataset_data %>%
        dplyr::group_by(.data[[time_col]], .data$patch_id)

    } else if (level == "boundary"){

      grouped_data <- dataset_data %>%
        dplyr::group_by(.data[[time_col]], .data[[boundary]])

    }

  } else if (group_by == "space"){

    if (level == "patch"){

      grouped_data <- dataset_data %>%
        dplyr::group_by(.data$patch_id)

    } else if (level == "boundary"){

      grouped_data <- dataset_data %>%
        dplyr::group_by(.data[[boundary]])

    }

  } else if (group_by == "time"){

    grouped_data <- dataset_data %>%
      dplyr::group_by(.data[[time_col]])

  }
  return(grouped_data)
}

# Returns on demand one of two functions for aggregation
get_apply_fun <- function(apply_to_df){

  if (apply_to_df) {

    the_fun <- function(df, groups, ...){
      all_args <- list(...)[[1]]
      data.frame(do.call(all_args$fun,
                         append(list(df), all_args$args)))
    }

  } else {

    the_fun <- function(df, groups, ...){
      all_args <- list(...)[[1]]
      data.frame(temp = do.call(all_args$fun,
                                append(list(df[[all_args$variable]]),
                                       all_args$args)))
    }

  }

  return(the_fun)
}
