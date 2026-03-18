
#' @export
predict.alk <- function(object, newdata,
                        numbers_col = NULL,
                        age_below_minlen = min,
                        age_above_maxlen = max,
                        ...) {
  if (is.null(object)) {
    return(NULL)
  }
  # stopifnot("data.frame" %in% class(newdata))
  size_col <- attr(object, "size_col")
  age_col <- attr(object, "age_col")
  size_bin <- attr(object, "size_bin")
  autobin <- attr(object, "autobin")
  min_age <- attr(object, "min_age")
  plus_group <- attr(object, "plus_group")
  if (is.vector(newdata)) {
    newdata_as_vector <- TRUE
    newdata <- data.frame(
      length = newdata,
      order_vector = 1:length(newdata)
    )
  } else if (is.data.frame(newdata)) {
    newdata_as_vector <- FALSE
    check_length_data(newdata, size_col)
    newdata <-
      rename_laa_cols(newdata, size_col = size_col) %>%
      dplyr::mutate(order_vector = 1:dplyr::n())
    if (!is.null(numbers_col)) {
      newdata <-
        newdata %>%
        tidyr::uncount(!!rlang::sym(numbers_col)) %>%
        dplyr::mutate(order_vector = 1:dplyr::n())
    }
  }
  object <- rename_laa_cols(object, size_col, age_col, num_col = numbers_col)
  if (all(is.na(newdata$length))) {
    out <-
      newdata %>%
      dplyr::mutate(est.age = NA, alk.n = NA)
  } else if (!autobin) {
    out <-
      newdata %>%
      dplyr::group_by(.data$length) %>%
      # get the total number in each length group
      dplyr::mutate(n = dplyr::n()) %>%
      tidyr::nest(data = -.data$length) %>%
      dplyr::left_join(object, by = c("length")) %>%
      tidyr::nest(ages = tidyselect::starts_with("age")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(est.age = purrr::map2(
        .data$data, .data$ages, function(x, y) {
          n <- unique(x$n)
          if (length(n) > 1) {
            stop("Something went wrong in predicting ages from an alk")
          }
          if (all(is.na(y))) {
            return(rep(NA, n))
          } else{
            est_ages <- floor(n * y)
            leftovers <- n - sum(est_ages)
            age_vec <- rep(names(est_ages), est_ages)
            if (leftovers > 0) {
              leftover_ages <- sample(
                names(y),
                size = leftovers,
                replace = TRUE,
                prob = y
              )
              age_vec <- c(age_vec, leftover_ages)
            }
            out <- as.numeric(gsub("age|\\+", "", age_vec))
            return(out)
          }
        })
      ) %>%
      tidyr::unnest(cols = c("data", "est.age")) %>%
      dplyr::select(-"ages", -"n")
    missing_ages <- dplyr::filter(out, is.na(.data$est.age))
    if (nrow(missing_ages) > 1) {
      warning(
        "You have length data that are missing from your ALK. ",
        "Consider changing the size_bin during ALK creation."
      )
    }
  } else {
    out <-
      newdata %>%
      dplyr::mutate(
        rounded_length = bin_lengths(.data$length, size_bin)
      ) %>%
      dplyr::group_by(.data$rounded_length) %>%
      # get the total number in each rounded length
      dplyr::mutate(n = dplyr::n()) %>%
      tidyr::nest(data = -"rounded_length") %>%
      dplyr::left_join(object, by = c("rounded_length" = "length")) %>%
      tidyr::nest(ages = tidyselect::starts_with("age")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(est.age = purrr::map2(
        .data$data, .data$ages, function(x, y) {
          n <- unique(x$n)
          if (length(n) > 1) {
            stop("Something went wrong in predicting ages from an alk")
          }
          if (all(is.na(y))) {
            # if no ages are present for length, assign NA for now
            # will assign later based on probability if defaults were used for alk
            return(rep(NA, n))
          } else{
            est_ages <- floor(n * y)
            leftovers <- n - sum(est_ages)
            age_vec <- rep(names(est_ages), est_ages)
            if (leftovers > 0) {
              leftover_ages <- sample(
                names(y),
                size = leftovers,
                replace = TRUE,
                prob = y
              )
              age_vec <- c(age_vec, leftover_ages)
            }
            out <- as.numeric(gsub("age|\\+", "", age_vec))
            return(out)
          }
        })
      ) %>%
      tidyr::unnest(cols = c("data", "est.age")) %>%
      dplyr::select(-"ages", -"n")
    missing_ages <- dplyr::filter(out, is.na(.data$est.age))
    out <- dplyr::filter(out, !is.na(.data$est.age))
    if (nrow(missing_ages) > 0) {
      if (autobin & size_bin <= 1) {
        normal_params <- attr(object, "dnorm_params")
        # get min and max lengths and ages
        minlen <- min(object$length)
        maxlen <- max(object$length)
        # assign age to missing rows based on normal params from alk inputs
        non_missing_ages <-
          missing_ages %>%
          dplyr::mutate(est.age = purrr::map_dbl(.data$length, function(x) {
            if (is.na(x)) {
              return(NA)
            } else if (x < minlen) {
              return(age_below_minlen(normal_params$age))
            } else if (x > maxlen) {
              return(age_above_maxlen(normal_params$age))
            } else {
              prob <- dnorm(x, normal_params$mean, normal_params$sd)
              if (all(prob < 6.691511e-05)) {
                closest_age_ind <- which.min(abs(x - normal_params$mean))
                return(normal_params$age[closest_age_ind])
              } else {
                return(normal_params$age[which.max(prob)])
              }
            }
          }))
      } else {
        warning(
          "Some lengths in newdata were not represented in your age-length key.",
          "\nConsider different bin sizes."
        )
        non_missing_ages <-
          missing_ages %>%
          dplyr::mutate(est.age = NA)
      }
      out <- dplyr::bind_rows(out, non_missing_ages)
    }
    out <-
      dplyr::select(out, -"rounded_length") %>%
      rename_laa_cols(
        size_col = size_col,
        age_col = age_col,
        num_col = numbers_col,
        goback = TRUE
      )
  }
  out <-
    out %>%
    dplyr::mutate(alk.n = attr(object, "alk_n")) %>%
    dplyr::arrange("order_vector") %>%
    dplyr::select(-"order_vector")
  if (newdata_as_vector) {
    out <- out$est.age
  }
  return(out)
}

#' Assign ages to non-aged data based on a fitted age model
#'
#' @param object An object of class "alk", "halk_fit" as
#' produced by \code{\link{make_alk}} or \code{\link{make_halk}}
#' @param newdata A vector or data.frame with size/length measurements
#' @param ... Additional parameters to pass to the S3 object methods
#'
#' @return A data.frame the same as \code{newdata}, but with ages assigned based
#' on the model provided in \code{object}
#' @export
#'
#' @examples
#' spp_alk <- make_halk(spp_data, levels = "spp")
#' spp_est_ages <- assign_ages(spp_data, spp_alk)
assign_ages <- function(newdata, object, ...) {
  UseMethod("assign_ages", object)
}

#' @export
assign_ages.alk <- function(newdata, object, ...) {
  est_ages <- predict(object, newdata)
  attr(est_ages, "age_levels") <- NULL
  return(est_ages)
}

#' @export
assign_ages.halk_fit <- function(newdata, object, ...) {
  levels <- attr(object, "levels")
  if (is.null(levels)) {
    est_ages <- predict(object, newdata)
  } else {
    # pull levels off attributes and reverse loop through them to predict age
    est_ages <- tibble::as_tibble(newdata[FALSE, ])
    unaged_data <- newdata
    temp_levels <- levels
    level_alks <- object
    for (i in rev(seq_along(levels))) {
      if (nrow(unaged_data) == 0) {
        next
      }
      temp_level_no_spp <- rm_spp_level(temp_levels)
      temp_nonlevel_names <- names(newdata)[!(names(newdata) %in% temp_levels)]
      est_level_ages <-
        unaged_data %>%
        # removed 3/29/2022 due to deprecation of across within filter
        # shouldn't be needed anyways because the inner join should remove rows
        # dplyr::filter(
        #   dplyr::across(tidyselect::all_of(temp_levels), ~ !is.na(.x))
        # ) %>%
        tidyr::nest(data = tidyselect::all_of(temp_nonlevel_names)) %>%
        dplyr::inner_join(
          level_alks %>%
            dplyr::select(dplyr::all_of(temp_levels), "alk") %>%
            dplyr::distinct(),
          by = temp_levels
        )
      if (nrow(est_level_ages) == 0) {
        # prep levels and alks for next iteration
        temp_levels <- temp_levels[-i]
        level_alks <-
          level_alks %>%
          dplyr::filter(is.na(!!rlang::sym(levels[i]))) %>%
          dplyr::select(tidyselect::all_of(temp_levels), "alk")
        next
      } else {
        est_level_ages <-
          est_level_ages %>%
          dplyr::mutate(est.age = purrr::map2(
            .data$data, .data$alk, function(x, y) {
              # there's a problem here with the predict function returning
              # more rows than what it's given -- at least for bluegill
              return(predict(y, x))
            })
          ) %>%
          tidyr::unnest(.data$est.age) %>%
          dplyr::select(-"data", -"alk") %>%
          dplyr::mutate(alk = levels[i]) %>%
          dplyr::relocate("alk.n", .after = "alk")
      }

      est_ages <- dplyr::bind_rows(est_ages, est_level_ages)
      unaged_data <- dplyr::anti_join(
        unaged_data,
        est_level_ages,
        by = temp_levels
      )
      # prep levels and alks for next iteration
      temp_levels <- temp_levels[-i]
      level_alks <-
        level_alks %>%
        dplyr::filter(is.na(!!rlang::sym(levels[i]))) %>%
        dplyr::select(tidyselect::all_of(temp_levels), "alk")
    }
  }
  attr(est_ages, "age_levels") <- levels
  return(est_ages)
}


#' @export
assign_ages.NULL <- function(newdata, object, ...) {
  return(NULL)
}
