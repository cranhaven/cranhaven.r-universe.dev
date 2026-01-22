#' Initialise the tidyindex pipeline
#'
#' Initialise an index table object with a data frame or a tibble.
#' @param data a tibble or data frame to be converted into a index object
#' @param ... arguments to give variables roles, recorded in the \code{paras}
#' element of the index table object, currently used for \code{id} and
#' \code{time}
#' @return an index table object
#' @export
#' @rdname init
#' @examples
#' init(hdi)
#' init(gggi)
init <- function(data, ...){
  dots <- dplyr::enquos(...)
  check_tibble_or_df(data)
  paras <- dplyr::tibble(variables = colnames(data))

  if (length(dots) != 0){
    dot_dfr <- purrr::map_dfr(
      dots, ~tibble(variables = names(tidyselect::eval_select(.x, data)))) |>
      cbind(roles = names(dots))
    paras <- paras |> dplyr::left_join(dot_dfr, by = "variables")

  }

  steps <- dplyr::tibble()
  res <- list(data = dplyr::as_tibble(data), paras = paras, steps = steps)
  class(res) <- "idx_tbl"
  attr(res, "data") <- data
  return(res)
}

#' Add parameters to an index table object
#'
#' The function joins the parameter table to the `paras` element of an index
#' table object.
#'
#' @param data a \code{idx_tbl} object
#' @param para_tbl a tibble or data frame object with parameter of variables
#' @param by a single column name (support tidyselect) in the `para_tbl` that
#' maps to the variable name in the data
#' @return an index object
#' @export
#' @examples
#' init(gggi) |> add_paras(gggi_weights, by = "variable")
add_paras <- function(data, para_tbl, by){
  check_idx_tbl(data)
  by <- enquo(by)

  if (rlang::quo_is_missing(by)) {
    data[["paras"]] <- data[["paras"]] |> dplyr::full_join(para_tbl)
  } else{
    by <- by |> rlang::quo_name()
    lhs_by <- colnames(data[["paras"]])[1]
    data[["paras"]] <- data[["paras"]] |>
      dplyr::full_join(para_tbl, by = stats::setNames(by, lhs_by))
  }


  return(data)
}


#' The print methods for an index object
#'
#' @param x an index object
#' @export
#' @rdname init
print.idx_tbl <- function(x, ...){
  check_idx_tbl(x)
  cat("Index pipeline: \n")

  if (nrow(x$steps) ==0){
    cli::cli_text(NULL, default = " Summary: {.field NULL}")
  } else{
    cat("\n")
    cat("Steps: \n")
    i <- 1
    for (i in seq_len(nrow(x$steps))){
      tmp <- x$steps[i,]
      cli::cli_text("{.pkg {tmp$module}}: {.fn {tmp$op}} -> {.field {tmp$name}}")
      i <- i + 1
    }

  }

  cat("\n")
  cat("Data: \n")
  print(x$data)
}


get_id <- function(data){
  check_idx_tbl(data)
  id <- NULL
  if ("roles" %in% colnames(data$paras)){
    id <- data$paras |>
      dplyr::filter(roles == "id") |>
      dplyr::pull(variables)
  }
  return(id)
}

get_boot_id <- function(data){
  check_idx_tbl(data)
  id <- NULL
  if ("roles" %in% colnames(data$paras)){
    id <- data$paras |>
      dplyr::filter(roles == "boot_id") |>
      dplyr::pull(variables)
  }
  return(id)
}

get_temporal_index <- function(data){
  check_idx_tbl(data)
  time <- NULL
  if ("roles" %in% colnames(data$paras)){
    time <- data$paras |>
      dplyr::filter(roles == "time") |>
      dplyr::pull(variables)
  }
  return(time)
}

get_group_var <- function(data){
  check_idx_tbl(data)
  group <- NULL
  if ("roles" %in% colnames(data$paras)){
    group <- data$paras |>
      dplyr::filter(roles == "group") |>
      dplyr::pull(variables)
  }
  return(group)
}

globalVariables(c("roles"))
