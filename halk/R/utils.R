#' Check for age/length data in the data being estimated or predicted
#'
#' These are just simple helper functions used within other functions that check
#' to make sure that ages and lengths are present in the data and stop the
#' fucntion call if they are missing
#'
#' @param data A data.frame
#' @param age_col Character. The column name for the age column in \code{data}
#'
#' @return NULL. An error will be called if age/length data is missing
#'
#' @name check_agelen_data
check_age_data <- function(data, age_col) {
  if (!(age_col %in% colnames(data))) {
    stop(
      "You are missing an age column in your data. ",
      "It's hard to create an age estimator without some known ages.\n",
      "Perhaps try setting the age_col argument."
    )
  }
}

#' @rdname check_agelen_data
#' @param size_col Character. The column name for the size column in \code{data}
check_length_data <- function(data, size_col) {
  if (!(size_col %in% colnames(data))) {
    stop(
      "You are missing a size column in your data. ",
      "I won't know how to age anything without sizes.\n",
      "Perhaps try setting the size_col argument"
    )
  }
}

#' Check for species in columns and/or levels and add to levels if present
#'
#' These helper functions just check to see if a species column exists in the
#' data (designated as 'spp' or 'species'). If one of those columns exists,
#' but the column name is not in the levels argument it will get added to
#' levels.
#'
#' @param data A data.frame with length-at-age data
#' @param levels The levels argument passed from \code{\link{make_halk}}
#'
#' @name spp_levels
#'
#' @return A character vector of levels possibly with 'spp' or 'species' added
is_spp_in_levels <- function(levels) {
  return(any(grepl("^spp$|^species$", levels, ignore.case = TRUE)))
}

#' @rdname spp_levels
is_spp_in_data <- function(data) {
  return(any(grepl("^spp$|^species$", names(data), ignore.case = TRUE)))
}

#' @rdname spp_levels
spp_level <- function(levels) {
  if (is_spp_in_levels(levels)) {
    return(levels[grep("^spp$|^species$", levels, ignore.case = TRUE)])
  } else {
    return(NULL)
  }

}

#' @rdname spp_levels
rm_spp_level <- function(levels) {
  if (is_spp_in_levels(levels)) {
    out <- levels[-grep("^spp$|^species$", levels, ignore.case = TRUE)]
    if (length(out) == 0) {
      return(NULL)
    } else {
      return(out)
    }
  } else {
    return(levels)
  }
}

#' @rdname spp_levels
add_spp_level <- function(data, levels) {
  spp_col <- names(data)[grepl("^spp$|^species$", names(data), ignore.case = TRUE)]
  if (length(spp_col) == 1) {
    unique_spp <- unique(data[[spp_col]])
    if (length(unique_spp) == 1) {
      return(levels)
    } else if (!(spp_col %in% levels)) {
      levels <- c(spp_col, levels)
      warning(
        "You presumably have a species column designated as ", spp_col,
        ". I'm adding it to levels.\n",
        "You can add it to levels manually to suppress this warning."
      )
    }
  } else if (length(spp_col) > 1) {
    unique_spp <- unique(data[[spp_col[1]]])
    if (length(unique_spp) == 1) {
      return(levels)
    } else if (!(spp_col %in% levels)) {
      levels <- c(spp_col[1], levels)
      warning(
        "You have multiple columns designated for species.\n",
        paste(paste0("  *", spp_col), collapse = "\n"),
        ".\nI'm adding the first to levels. ",
        "Manually add one or more of these to levels to suppress this warning."
      )
    }
  }
  return(levels)
}

#' Convert ages from/to ordered factor
#'
#' In order for the machine learning models to properly predict ages, the
#' known ages should be converted to an ordered factor during model fitting.
#' This will ensure that the predict.* functions return age values that actually
#' make sense.
#'
#' @param data A data.frame with a column corresopnding to \code{age_col} or
#' a vector of values
#' @param age_col Character. The name of the column that contains ages
#'
#' @return A data.frame with the values in \code{age_col} converted to an
#' ordered factor
#'
#' @name ages_as_ordered
ages_as_ordered_factor <- function(data, age_col = "age") {
  if (is.vector(data)) {
    out <- as.ordered(data)
  } else {
    out <-
      data %>%
      dplyr::mutate_at(dplyr::vars(!!rlang::sym(age_col)), as.ordered)
  }
  return(out)
}

#' @rdname ages_as_ordered
ages_as_integer <- function(data, age_col = "est.age") {
  if (is.ordered(data) || is.factor(data)) {
    out <- as.integer(as.character(data))
  } else {
    out <-
      data %>%
      dplyr::mutate_at(dplyr::vars(!!rlang::sym(age_col)), function(x) {
        as.integer(as.character(x))
      })
  }
  return(out)
}

#' Check the model type and return standardized version
#'
#' This is a non-exported function to check whether the model type specified is
#' available and return a standardized version of the model name. This
#' standardized version will then feed into a S3 method for the given model.
#'
#' @param model A character string naming the model
#'
#' @return A standardized version of the model name, or an error if
#' \code{model} doesn't exist yet
check_model_type <- function(model) {
  model_standard <- gsub("\\s+|-", "_", trimws(model))
  if (!(model_standard %in% model_types$model)) {
    stop("Can't find a model type for ", model)
  } else {
    model_name <- model_types$model_name[model_types$model == model_standard]
    return(model_name)
  }
}

#' Adjusts data to account for plus group or minimum age
#'
#' These functions performs two tasks. It lumps all ages greater than the
#' plus group into that age, and it filters data only to those greater than
#' or equal to the minimum age. \code{adjust_plus_min_ages} works on a vector
#' whereas \code{adjust_plus_min_ages_df} words on a data.frame
#'
#' @param data Data with age as a column, or a numeric vector of ages
#' @param minage Numeric. The minimum age; everything else is excluded
#' @param pls_grp Numeric. The plus group; all ages older will be lumped into
#' this group
#'
#' @name adjust_ages
#'
#' @return A data.frame similar to \code{data}, but with ages less than
#' \code{minage} excluded and ages >= \code{plus_group} aggregated into that age
adjust_plus_min_ages_df <- function(data, minage = NULL, pls_grp = NULL) {
  if (!is.null(minage)) {
    data <-
      data %>%
      dplyr::filter(.data$age >= minage)
  }
  if (!is.null(pls_grp)) {
    data <-
      data %>%
      dplyr::mutate(age = dplyr::case_when(
        .data$age > pls_grp ~ as.integer(pls_grp),
        TRUE ~ as.integer(.data$age)
      ))
  }
  return(data)
}

#' @rdname adjust_ages
#' @param age_vec A vector of ages
adjust_plus_min_ages <- function(age_vec, minage = NULL, pls_grp = NULL) {
  if (!is.null(minage)) {
    age_vec[age_vec < minage] <- minage
  }
  if (!is.null(pls_grp)) {
    age_vec[age_vec > pls_grp] <- pls_grp
  }
  return(age_vec)
}


#' Count number of length-at-age samples or age groups at each level and return
#' those with greater than equal to the minimum desired number
#'
#' These are helper shortcut functions to determine if data meet the minimum
#' desired number of age groups and/or sample sizes.
#'
#' @param data Data.frame with length-at-age data
#' @param sub_levels The levels at which to check
#' @inheritParams make_alk
#'
#' @return A data.frame just like \code{data}, but with samples excluded that
#' don't meet the required number of samples in \code{min_sample_size}
#'
#' @name min_samples
min_count_laa_data <- function(data, sub_levels = NULL,
                               min_age_sample_size = NULL,
                               min_total_sample_size = NULL,
                               min_age_groups = NULL) {
  if (is.null(data)) {
    return(NULL)
  }
  if (!is.null(min_total_sample_size)) {
    if (is.null(sub_levels)) {
      if (nrow(data) >= min_total_sample_size) {
        data <- data
      } else {
        return(NULL)
      }
    } else {
      sub_grouping <-
        sub_levels %>%
        rlang::syms()
      laa_counts <-
        data %>%
        dplyr::count(!!!sub_grouping) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!!rlang::syms(sub_levels)) %>%
        dplyr::filter(.data$n >= min_total_sample_size) %>%
        dplyr::ungroup()
      data <-
        data %>%
        dplyr::inner_join(laa_counts, by = sub_levels) %>%
        dplyr::select(-"n")
    }
  }
  if (!is.null(min_age_sample_size)) {
    sub_grouping <-
      sub_levels %>%
      c("age") %>%
      unlist() %>% rlang::syms()
    laa_counts <-
      data %>%
      dplyr::count(!!!sub_grouping) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!!rlang::syms(sub_levels)) %>%
      # changed this to include any data that meets minimum standards
      # even if certain age groups don't
      # (i.e. older age groups with single individual) - PNF 11/22/2023
      dplyr::mutate(age_grps_gt_min = sum(.data$n >= min_age_sample_size)) %>%
      dplyr::filter(.data$age_grps_gt_min >= min_age_groups) %>%
      # dplyr::filter(dplyr::if_any(.data$n, ~.x >= min_age_sample_size)) %>%
      dplyr::ungroup()
    data <-
      data %>%
      dplyr::inner_join(laa_counts, by = c(sub_levels, "age")) %>%
      dplyr::select(-"n")
  }
  if (nrow(data) == 0) {
    return(NULL)
  } else {
    return(data)
  }
}

#' @rdname min_samples
#' @param min_age_grps The minimum number of age groups that must be present
#' in data to create an ALK
min_age_groups <- function(data, sub_levels = NULL, min_age_grps) {
  if (is.null(data)) {
    return(NULL)
  }
  if (is.null(sub_levels)) {
    if (length(unique(data$age)) >= min_age_grps) {
      return(data)
    } else {
      return(NULL)
    }
  } else {
    sub_grouping <- rlang::syms(sub_levels)
    age_grp_counts <-
      data %>%
      dplyr::group_by(!!!sub_grouping) %>%
      dplyr::summarize(n_age_grps = length(unique(.data$age)), .groups = "drop") %>%
      dplyr::filter(.data$n_age_grps >= min_age_grps)
    temp_age_data <-
      data %>%
      dplyr::inner_join(age_grp_counts, by = sub_levels) %>%
      dplyr::select(-"n_age_grps")
    if (nrow(temp_age_data) == 0) {
      return(NULL)
    } else {
      return(temp_age_data)
    }
  }
}

#' Convert a vector of lengths into binned values
#'
#' This will take a vector of numeric values and bin them according to the value
#' specified in binwidth
#'
#' @param x Numeric vector of values
#' @param binwidth Numeric vector specifying how wide the length bins should be
#' @param include_upper Logical. Append the upper value of the bin and return
#' the length range as a character string (TRUE), or return the lower value as
#' numeric (FALSE, default)
#' @param ... Additional arguments passed onto \code{\link[base]{cut}}
#'
#' @return A vector of values the same length as x, but binned to the values
#' according to binwidth
#' @export
#'
#' @examples
#' bin_lengths(length_data$length, binwidth = 2)
bin_lengths <- function(x, binwidth, include_upper = FALSE, ...) {
  if (length(x) == 0) {
    stop("You have no length data to bin")
  }
  bins <- seq(0, ceiling(max(x, na.rm = TRUE) + binwidth), by = binwidth)
  if (include_upper) {
    cut_bins <- cut(x, bins, right = FALSE, ...)
    bin_levels <- levels(cut_bins)
    brackets_regex <- "^\\[|^\\(|\\]$|\\)$"
    bin_levels <- gsub(brackets_regex, "", bin_levels)
    bin_levels <- gsub(",", "-", bin_levels)
    tmp <- gsub(brackets_regex, "", as.character(cut(x, bins)))
    out <- gsub(",", "-", tmp)
    out <- ordered(out, levels = bin_levels)
  } else {
    tmp <- gsub(",.*$", "", as.character(cut(x, bins, right = FALSE, ...)))
    out <- as.numeric(gsub("^\\[", "", tmp))
  }
  return(out)
}

check_laa_data <- function(df, quiet = FALSE) {
  if (is.null(df) || nrow(df) == 0) {
    if (!quiet) {
      warning(
        "You have no data to make an alk with. ",
        "Consider change plus group or minimum age."
      )
    }
    return(NULL)
  } else {
    return(df)
  }
}

#' Simple helper function to rename size and age column names to age and length
#'
#' @param data Any data.frame with some columns representing age and size
#' @param size_col Character. The name of the column containing sizes
#' @param age_col Character. The name of the column containing ages
#' @param num_col Character. The name of the column containing number of
#' individuals
#' @param goback Logical. Reverse names once they've already been renamed
#'
#' @return A data.frame the same as \code{data}, but with names changed
rename_laa_cols <- function(data,
                            size_col = "length",
                            age_col = "age",
                            num_col = NULL,
                            goback = FALSE) {
  out <-
    data %>%
    rename_size_col(sc = size_col, back = goback) %>%
    rename_age_col(ac = age_col, back = goback)
  if (!is.null(num_col)) {
    out <- rename_num_col(out, num_col, back = goback)
  }
  return(out)
}

# simple helper function to remove NA values from length-at-age data
sanitize_laa_data <- function(data) {
  out <-
    data %>%
    dplyr::filter(!is.na(.data$length), !is.na(.data$age))
  return(out)
}



# keeping this in case I screwed anything up parsing this fucntion out
# PNF - 04/22/2022
# rename_laa_cols_bkp <- function(data,
#                             size_col = "length",
#                             age_col = "age",
#                             num_col = NULL,
#                             goback = FALSE) {
#   if (goback) {
#     size_col_ind <- grep("^length$", names(data))
#     age_col_ind <- grep("^age", names(data))
#     names(data)[size_col_ind] <- size_col
#     names(data)[age_col_ind] <- gsub("age", age_col, names(data)[age_col_ind])
#     if (!is.null(num_col)) {
#       num_col_ind <- grep("^n$", names(data))
#       names(data)[num_col_ind] <- num_col
#     }
#   } else {
#     size_col_ind <- grep(paste0("^", size_col, "$"), names(data))
#     age_col_ind <- grep(paste0("^", age_col, "$"), names(data))
#     names(data)[size_col_ind] <- "length"
#     names(data)[age_col_ind] <- gsub(age_col, "age", names(data)[age_col_ind])
#     if (!is.null(num_col)) {
#       num_col_ind <- grep(num_col, names(data))
#       names(data)[num_col_ind] <- "n"
#     }
#   }
#   return(data)
# }

rename_size_col <- function(data, sc = "length", back = FALSE) {
  if (back) {
    size_col_ind <- grep("^length$", names(data))
    names(data)[size_col_ind] <- sc
  } else {
    size_col_ind <- grep(paste0("^", sc, "$"), names(data))
    names(data)[size_col_ind] <- "length"
  }
  return(data)
}


rename_age_col <- function(data, ac = "age", back = FALSE) {
  if (back) {
    age_col_ind <- grep("^age", names(data))
    names(data)[age_col_ind] <- gsub("age", ac, names(data)[age_col_ind])
  } else {
    age_col_ind <- grep(paste0("^", ac, "$"), names(data))
    names(data)[age_col_ind] <- gsub(ac, "age", names(data)[age_col_ind])
  }
  return(data)
}

rename_num_col <- function(data, nc = "n", back = FALSE) {
  if (back) {
    num_col_ind <- grep("^n$", names(data))
    names(data)[num_col_ind] <- nc
  } else {
    num_col_ind <- grep(nc, names(data))
    names(data)[num_col_ind] <- "n"
  }
  return(data)
}



#' Assign associated age-length key attributes to a data.frame
#'
#' This is just a helper function to assign the needed attributes and classes
#' to a data.frame that is produced by either \code{\link{make_alk}} or
#' \code{\link{make_halk}}.
#'
#' @param data A data.frame
#' @param size_col Character. Name of the column representing sizes
#' @param age_col Character. Name of the column representing ages
#' @param autobin Logical to set the attribute of autobin
#' @param size_bin Numeric. What is the width of size bins
#' @param min_age Numeric. The minimum age that was included in the alk
#' @param plus_group Numeric. The age that represents the plus group
#' @param alk_n Numeric. The number of samples that went into creating the alk
#' @param classes Character. The class that should get prepended to the
#' data.frame class(es)
#' @param dnorm_params The value of parameters that went into creating the
#' normal distributions on the age groups
#' @param levels Character vector of the levels used. This creates the "levels"
#' attribute if present
#'
#' @return A data.frame with associated attributes assigned
assign_alk_attributes <- function(data,
                                  size_col = "length",
                                  age_col = "age",
                                  autobin = TRUE,
                                  size_bin = 1,
                                  min_age = NULL,
                                  plus_group = NULL,
                                  alk_n = NULL,
                                  classes = "alk",
                                  dnorm_params = NULL,
                                  levels = NULL) {
  attr(data, "size_col") <- size_col
  attr(data, "age_col") <- age_col
  attr(data, "autobin") <- autobin
  attr(data, "size_bin") <- size_bin
  attr(data, "min_age") <- min_age
  attr(data, "plus_group") <- plus_group
  attr(data, "alk_n") <- alk_n
  if (!is.null(levels)) {
    attr(data, "levels") <- levels
  }
  if (!is.null(dnorm_params)) {
    stopifnot(autobin & size_bin <= 1)
    attr(data, "dnorm_params") <- dnorm_params
  }
  class(data)  <- c(classes, class(data))
  return(data)
}

#' Simple function that returns NA values
#'
#' A vector of NA will be returned that is the length of \code{x}
#'
#' @param x Any vector of any length
#'
#' @return A vector the same length as x containing only NA values
assign_na_age <- function(x) {
  return(NA)
}

