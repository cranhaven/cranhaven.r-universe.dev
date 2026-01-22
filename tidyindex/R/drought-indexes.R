#' Drought-related index functions
#'
#' The functions are used for quick computing of some common drought indexes
#' built from wrappers of the underlying modules. For more customised needs,
#' users may build their own indexes from the modules.
#'
#' @param data an \code{id_tbl} object
#' @param .tavg,.lat,.prcp variables to be used in the index calculation, see Details
#' @param .scale the temporal aggregation scale, see
#'  [tidyindex::temporal_aggregation()]
#' @param .dist the distribution used for distribution fit, see
#'  [tidyindex::distribution_fit()]
#' @param .pet_method the method used for calculating potential
#'  evapotranspitation, currently only \code{trans_thornthwaite()}
#' @param var the variable to be transformed, see [tidyindex::variable_trans()]
#' and [SPEI::thornthwaite()]
#' @param lat,na.rm,verbose see [SPEI::thornthwaite]
#'
#' @details
#'
#' Below explains the steps wrapped in each index and the intermediate
#' variables created.
#'
#' The \code{idx_spi()} function performs
#'  \enumerate{
#'    \item{a temporal aggregation on the input precipitation series,
#'    \code{.prcp}, as \code{.agg},}
#'    \item{a distribution fit step on the aggregated precipitation
#'    , \code{.agg}, as \code{.fit}, and}
#'    \item{a normalising step on the fitted values, \code{.fit}, as
#'      \code{.index}}
#'  }
#'
#' The \code{idx_spei()} function performs
#'  \enumerate{
#'    \item{a variable transformation step on the inut average temperature,
#'      \code{.tavg}, to obtain the potential evapotranspiration, \code{.pet},}
#'    \item{a dimension reduction step to calculate difference series,
#'      \code{.diff}, between the input precipitation series, \code{.prcp},
#'      and \code{.pet},}
#'    \item{a temporal aggregation step on the difference series, \code{.diff},
#'      as \code{.agg}, }
#'    \item{a distribution fit step on the aggregated series, \code{.agg},
#'      as \code{.fit}, and}
#'    \item{a normalising step on the fitted value, \code{.fit}, to
#'      obtain \code{.index}.}
#'  }
#'
#' The \code{idx_rdi()} function performs
#'  \enumerate{
#'    \item{a variable transformation step on the input average temperature,
#'      \code{.tavg}, to obtain potential evapotranspiration \code{.pet},}
#'    \item{a dimension reduction step to calculate the ratio of input
#'      precipitation, \code{.prcp}, to \code{.pet} as \code{.ratio},}
#'    \item{a temporal aggregation step on the ratio series, \code{.ratio}, as
#'      \code{.agg}}
#'    \item{a variable transformation step to take the log10 of the aggregated
#'      series, \code{.agg}, as \code{.y}, and}
#'    \item{a rescaling step to rescale \code{.y} by zscore to obtain
#'      \code{.index}.}
#'  }
#'
#' The \code{idx_edi()} function performs
#'  \enumerate{
#'    \item{a dimension reduction step to aggregate the input precipitation
#'      series, \code{prcp}, as \code{.mult},}
#'    \item{a temporal aggregation step on the aggregated precipitation series
#'      (\code{.mult}) as \code{.ep}, and}
#'    \item{a rescaling step to rescale \code{.ep} by zscore to obtain
#'      \code{.index}.}
#'  }
#'
#' @rdname drought-idx
#' @return an index table object
#' @examples
#' library(dplyr)
#' library(lmomco)
#' dt <- tenterfield |>
#'   mutate(month = lubridate::month(ym)) |>
#'   init(id = id, time = ym, group = month)
#'
#' dt |> idx_spi()
#' dt |> idx_spi(.scale = c(12, 24))
#' dt |> idx_spei(.lat = lat, .tavg = tavg)
#' dt |> idx_rdi(.lat = lat, .tavg = tavg)
#' dt |> idx_edi(.lat = lat, .tavg = tavg)
#'
trans_thornthwaite <- function(var, lat, na.rm = FALSE, verbose = TRUE){
  lat <- enquo(lat) |> rlang::quo_get_expr()
  if (! rlang::is_symbol(lat)){
    lat <- eval(lat)
  }

  if (!requireNamespace("SPEI", quietly = TRUE)){
    stop("SPEI package is required for computing Thornthwaite PET") # nocov
  }

  fn <- function(var, lat, na.rm = FALSE, verbose = TRUE){
    SPEI::thornthwaite(
      Tave = var, lat = unique(lat), na.rm = na.rm, verbose = verbose
    ) |>
      unclass() |> as.vector()
  }

  new_trans("trans_thornthwaite", var = enquo(var), fn = fn, lat = lat)
}

#' @export
#' @rdname drought-idx
idx_spi <- function(data, .prcp, .dist = dist_gamma(), .scale = 12){
  prcp <- enquo(.prcp)
  check_idx_tbl(data)
  if (length(.dist) == 1) {
    map(list(.dist), check_dist_fit_obj)
    .dist <- as.character(substitute(.dist))
  } else{
    map(.dist, check_dist_fit_obj)
    .dist <- as.list(substitute(.dist))[-1] |> map(as.character)
  }

  res <- map(.dist, function(d) {
    data |>
    temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = .scale)) |>
    distribution_fit(.fit = do.call(d, list(var = ".agg", method = "lmoms")))|>
    normalise(.index = norm_quantile(.fit))
    })

  res2 <- tibble(.dist = gsub("dist_", "", unlist(.dist))) |>
    dplyr::mutate(values = res)
  class(res2) <- c("idx_res", class(res2))

  res2 |>
    augment(.id = ".dist") |>
    dplyr::mutate(.index = as.numeric(gsub(".index_", "", .index))) |>
    dplyr::rename(.scale = .index) |>
    dplyr::filter(is.finite(.value))
}

#' @export
#' @rdname drought-idx
idx_spei <- function(data, .tavg, .lat, .prcp, .pet_method = trans_thornthwaite(),
                     .scale = 12, .dist = dist_glo()){
  tavg <- enquo(.tavg)
  lat <- enquo(.lat)
  prcp <- enquo(.prcp)

  check_idx_tbl(data)

  pet_method <- as.character(substitute(.pet_method))

  if (length(.dist) == 1) {
    map(list(.dist), check_dist_fit_obj)
    .dist <- as.character(substitute(.dist))
  } else{
    map(.dist, check_dist_fit_obj)
    .dist <- as.list(substitute(.dist))[-1] |> map(as.character)
  }

  res <- map(.dist, function(d){
    data |>
      variable_trans(.pet = do.call(pet_method, list(tavg, lat = lat))) |>
      dimension_reduction(.diff = aggregate_manual(~prcp - .pet)) |>
      temporal_aggregate(.agg = temporal_rolling_window(.diff, scale = .scale)) |>
      distribution_fit(.fit = do.call(d, list(var = ".agg", method = "lmoms"))) |>
      normalise(.index = norm_quantile(.fit))
  })

  res2 <- tibble(.dist = gsub("dist_", "", unlist(.dist))) |>
    dplyr::mutate(values = res)
  class(res2) <- c("idx_res", class(res2))
  res2 |>
    augment(.id = ".dist") |>
    dplyr::mutate(.index = as.numeric(gsub(".index_", "", .index))) |>
    dplyr::rename(.scale = .index) |>
    dplyr::filter(is.finite(.value))
}



#' @export
#' @rdname drought-idx
idx_rdi <- function(data, .tavg, .lat, .prcp, .pet_method = trans_thornthwaite(),
                    .scale = 12){

  tavg <- enquo(.tavg)
  lat <- enquo(.lat)
  prcp <- enquo(.prcp)
  pet_method <- as.character(substitute(.pet_method))

  check_idx_tbl(data)

  res <- data |>
    variable_trans(.pet = do.call(pet_method, list(tavg, lat = lat))) |>
    dimension_reduction(.ratio = aggregate_manual(~prcp/.pet)) |>
    temporal_aggregate(.agg = temporal_rolling_window(.ratio, scale = .scale)) |>
    variable_trans(.y = trans_log10(.agg)) |>
    rescaling(.index = rescale_zscore(.y)) |>
    list()

  res2 <- tibble(.scale = .scale) |>
    dplyr::mutate(values = res)
  class(res2) <- c("idx_res", class(res2))
  res2 |>
    augment(.id = ".dist") |>
    dplyr::select(-.dist) |>
    dplyr::mutate(.index = as.numeric(gsub(".index_", "", .index))) |>
    dplyr::rename(.scale = .index) |>
    dplyr::filter(is.finite(.value))
}

#' @export
#' @rdname drought-idx
idx_edi <- function(data, .tavg, .lat, .prcp, .scale = 12){

  tavg <- enquo(.tavg)
  lat <- enquo(.lat)
  prcp <- enquo(.prcp)

  check_idx_tbl(data)

  res <- data |>
    dimension_reduction(.mult = aggregate_manual(
      ~prcp *rev(digamma(dplyr::row_number() + 1) - digamma(1))
      )) |>
    temporal_aggregate(.ep = temporal_rolling_window(.mult, scale = .scale)) |>
    rescaling(.index = rescale_zscore(.ep)) |>
    list()

  res2 <- tibble(.scale = .scale) |>
    dplyr::mutate(values = res)
  class(res2) <- c("idx_res", class(res2))
  res2 |>
    augment(.id = ".dist") |>
    dplyr::select(-.dist) |>
    dplyr::mutate(.index = as.numeric(gsub(".index_", "", .index))) |>
    dplyr::rename(.scale = .index) |>
    dplyr::filter(is.finite(.value))
}



globalVariables(c(".prcp", ".mult", ".ep", ".tavg", ".pet", ".ratio", ".y",
                  ".lat", ".diff", ".agg", ".fit", ".dist", ".index", ".value"))


