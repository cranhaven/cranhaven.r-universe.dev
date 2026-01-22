#' The temporal processing module
#'
#' The temporal processing module is used to aggregate data along the temporal
#' dimension. Current available aggregation recipe includes
#'  \code{temporal_rolling_window}.
#'
#' @param data an index table object, see [tidyindex::init()]
#' @param ... an temporal processing object of class \code{temporal_agg}
#' @param var the variable to aggregate
#' @param scale numeric, the scale (window) of the aggregation
#' @param .before,.step,.complete see \code{\link[slider]{slide_dbl}}
#' @param rm.na logical, whether to remove the first few rows with NAs
#'
#' @return an index table object
#' @rdname temporal-aggregate
#' @export
#' @examples
#' tenterfield |>
#'   init(time = ym) |>
#'   temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 12))
#'
#' # multiple ids (groups), and multiple scales
#' queensland |>
#'   dplyr::filter(id %in% c("ASN00029038", "ASN00029127")) |>
#'   init(id = id, time = ym) |>
#'   temporal_aggregate(temporal_rolling_window(prcp, scale = c(12, 24)))
temporal_aggregate <- function(data, ...){
  dot <- rlang::list2(...)
  dot_mn <- names(dot)
  if (!is.null(names(dot))){
    a <- map(dot, ~names(.x)) |> unlist()
    names(dot[[1]]) <- paste0(names(dot), gsub("rolling_window", "", a))
  }

  if (inherits(dot[[1]], "list")) dot <- dot[[1]]

  check_idx_tbl(data)
  map(dot, check_temp_agg_obj)

  # find if there is a column called roles in data$paras
  # if so, find if there is a variable with role of id in data$paras
  id <- get_id(data)
  check_temporal_index(data)
  if (length(id) == 1) {
    dt <- data$data |>  dplyr::group_by(!!!dplyr::syms(id))
  }  else{
    dt <- data$data
  }
  res <- compute_temp_agg(dt, dot, id)
  data$data <- data$data |> dplyr::bind_cols(purrr::reduce(res, cbind))


  if (attr(dot[[1]], "args")$rm.na){
    data$data <- data$data |>
      dplyr::group_by(id) |>
      dplyr::filter(dplyr::row_number() >= attr(dot[[1]], "scale")) |>
      dplyr::ungroup()
  }

  if (inherits(dot, "list")){
    i <- 1
    name <- names(dot)
    id <- nrow(data$steps) + 1
    for (i in seq_len(length(dot))){
      data$steps <- data$steps |>
        rbind(dplyr::tibble(
          id = id,
          module = "temporal",
          op = dot[i],
          name = name[i]))
      i <- i + 1
    }

  }

  return(data)
}

compute_temp_agg <- function(data, dot, id){
  purrr::map2(
    dot, names(dot),
    ~{data <- data |>
        dplyr::transmute(!!.y := do.call(
          attr(.x, "fn"),
          c(attr(.x, "args"),
            var = attr(.x, "var"),
            scale = attr(.x, "scale")
            )));
      if (length(id) == 1) {
        data <- data |>
          dplyr::ungroup(!!dplyr::sym(id)) |>
          dplyr::select(-!!dplyr::sym(id))
      }
    return(data)
    })
}

#' @rdname temporal-aggregate
#' @export
temporal_rolling_window <- function(var, scale, .before = 0L, .step = 1L,
                                    .complete = TRUE, rm.na = TRUE, ...){
  check_slider_installed()
  fn <- function(var, scale, .before, .step, .complete, ...) {
    slider::slide_dbl(.x = var, .f = "sum", .before = scale - 1,
                      .complete = .complete)
  }

  if (length(scale) > 1){
    map(scale, ~{
      new_temporal_agg("rolling_window", var = enquo(var), fn, scale = .x,
                       .before = .before, .step = .step, .complete = .complete,
                       rm.na = TRUE)
    }) |>
      rlang::set_names(paste0("rolling_window_", scale))
  } else{
    new_temporal_agg("rolling_window", var = enquo(var), fn, scale = scale,
                     .before = .before,  .step = .step, .complete = .complete,
                     rm.na = TRUE)
  }
}


new_temporal_agg <- function(type, var, fn, scale, ...){
  dots <- rlang::list2(...)
  name <- type
  attr(name, "var") <- var
  attr(name, "scale") <- scale
  attr(name, "fn") <- fn
  attr(name, "args") <- dots
  class(name) <- "temporal_agg"
  name
}

