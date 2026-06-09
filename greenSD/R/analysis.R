#' exposure
#' @description
#' Computes population-weighted greenspace fraction or human exposure to
#' greenspace based on a population-weighted exposure model (Chen et al., 2022),
#' using population data from the Global Human Settlement Layer (GHSL;
#' Pesaresi et al., 2024).
#' See **Details** for the underlying method and assumptions.
#'
#' @param r A SpatRaster with single/multiple greenspace layer(s), either
#' fractional or binary (where non-green = 0 and green = 1), typically
#' the output from [get_gsdc()], [get_esa_wc()], or [get_tile_green()].
#' @param res numeric vector of length 2. The actual spatial resolution (in meters).
#' Default is `c(10, 10)`.
#' @param pop_year numeric. Year of the GHSL dataset to use.
#' Must be one of: 2015, 2020, 2025, or 2030. Default is 2020.
#' @param radius numeric. Buffer radius (in meters) used for local averaging.
#' Default is `500`.
#' @param grid_size numeric. Optional. If provided, output is aggregated to grid cells
#' of this size (in meters) and returned as an `sf` object.
#' @param height logical. Whether to compute greenspace volume for population-weighted
#' greenspace fraction or human exposure to greenspace using Meta's global canopy
#' height map (Tolan et al., 2024). (The default is FALSE)
#' @param pop_out logical. Whether return population layer.
#' @param quiet logical. Whether show progress bars for some process.
#'
#' @return SpatRaster or sf. A `SpatRaster` (if `grid_size` is `NULL`) with
#' layers `pwgf_*`, or an `sf` object with columns `pwge_*` representing
#' population-weighted greenspace exposure values aggregated to each grid polygon.
#'
#' @details
#' This function implements the population-weighted greenspace exposure (PWGE) model:
#'
#' \enumerate{
#'   \item Start with a population raster. Each pixel \( i \) has a population value \( P_i \).
#'   \item Create a circular buffer of radius \( d \) around each pixel center.
#'   \item For each buffer, calculate greenspace fraction:
#'     \deqn{G_i^d = \frac{\text{Area of greenspace within buffer}}{\text{Total buffer area}}}
#'   \item Repeat for all \( i = 1, 2, ..., N \) grid cells.
#'   \item Compute overall exposure:
#'     \deqn{GE^d = \frac{\sum_i P_i \cdot G_i^d}{\sum_i P_i}}
#' }
#'
#' @references
#' Chen, B., Wu, S., Song, Y. et al. Contrasting inequality in human exposure to
#' greenspace between cities of Global North and Global South. Nat Commun 13,
#' 4636 (2022). https://doi.org/10.1038/s41467-022-32258-4
#'
#' Pesaresi, M., Schiavina, M., Politis, P., Freire, S., Krasnodębska, K.,
#' Uhl, J. H., … Kemper, T. (2024). Advances on the Global Human Settlement
#' Layer by joint assessment of Earth Observation and population survey data.
#' International Journal of Digital Earth, 17(1).
#' https://doi.org/10.1080/17538947.2024.2390454
#'
#' Tolan, J., Yang, H. I., Nosarzewski, B., Couairon, G., Vo, H. V., Brandt,
#' J., ... & Couprie, C. (2024). Very high resolution canopy height maps from
#' RGB imagery using self-supervised vision transformer and convolutional
#' decoder trained on aerial lidar. Remote Sensing of Environment, 300, 113888.
#'
#' @examples
#' sample_data <- terra::rast(system.file("extdata", "detroit_gs.tif", package = "greenSD"))
#' pwgf <- compute_exposure(
#'   # r = sample_data,
#'   pop_year = 2020,
#'   radius = 1500
#' )
#' @importFrom terra focal focalMat project resample cellSize expanse rasterize
#' @importFrom terra rast vect crs ext as.polygons extract set.names names ifel
#' @importFrom sf st_as_sfc st_bbox st_crs st_transform st_as_sf
#importFrom terra lapp buffer as.points zonal crs res as.polygons
#importFrom terra rasterize names set.names ifel ext centroids project
#importFrom utils
#' @export
compute_exposure <- function(r = NULL,
                             res = c(10, 10),
                             pop_year = 2020,
                             radius = 500,
                             grid_size = NULL,
                             height = FALSE,
                             pop_out = FALSE,
                             quiet = TRUE) {
  start_time <- Sys.time()
  if (is.null(r)) {
    return(NULL)
  }
  if (!pop_year %in% c(2015, 2020, 2025, 2030)) {
    stop("`pop_year` must be one of 2015, 2020, 2025, or 2030.")
  }
  if (radius <= 0) stop("`radius` must be > 0.")
  if (quiet) {
    terra::terraOptions(progress=0)
    on.exit(terra::terraOptions(progress=3), add = TRUE)
  }

  # ---------- 1) Prep AOI and Population ----------
  bbox_wgs84 <- sf::st_as_sfc(sf::st_bbox(as.vector(terra::ext(r))))
  sf::st_crs(bbox_wgs84) <- 4326
  bbox_wgs84 <- sf::st_transform(bbox_wgs84, crs = 4326)

  # download GHSL population covering the greenspace raster extent (WGS84)
  pop_wgs84 <- download_GHSL(bbox_wgs84, pop_year)

  # ---------- 2) Choose a metric CRS (meters) & project inputs ----------
  utm_epsg <- get_utm_crs(bbox_wgs84)      # user-provided helper that returns an EPSG integer
  metric_crs <- paste0("EPSG:", utm_epsg)

  cli::cli_alert_info("Projecting inputs to a metric CRS (meters) for buffer radius...")
  r_m <- terra::project(r, metric_crs, method = "near")  # greenspace is mask/fraction; use nearest
  pop_m <- terra::project(pop_wgs84, metric_crs, method = "bilinear")

  # force population grid to align to r_m (so Pi and Gi^d are co-located)
  # Use nearest to avoid introducing fractional artifacts from bilinear on counts.
  pop_m <- terra::resample(pop_m, r_m, method = "near")

  # Optional height (CHM) — project & align
  if (height) {
    cli::cli_alert_info("Downloading canopy height data ...")
    bbox_vct <- as.vector(sf::st_bbox(bbox_wgs84))
    chm <- suppressMessages(
      dsmSearch::get_dsm_30(bbox = bbox_vct, datatype = "metaCHM")
    )
    chm_m <- terra::project(chm, metric_crs, method = "bilinear")
    chm_m <- terra::resample(chm_m, r_m, method = "bilinear")
  }

  # ---------- 3) Build circular kernel (in meters) ----------
  w <- terra::focalMat(r_m, radius, type = "circle")  # weights sum to ~number of cells in radius

  # ---------- 4) Compute local greenspace metric G_i^d ----------
  # For standard fraction: mean of r within circle.
  # For height mode: sum(green * height * cell_area) / buffer_area.
  n_layers <- terra::nlyr(r_m)
  layer_names <- if (is.null(terra::names(r_m))) paste0("layer_", seq_len(n_layers)) else terra::names(r_m)

  if (!height) {
    cli::cli_alert_info("Computing local greenspace fraction with a circular window...")
    # mean inside the window (fractional or binary input OK)
    G_stack <- terra::focal(r_m, w = w, fun = mean, na.policy = "omit", expand = TRUE, filename = "", na.rm = TRUE)
  } else {
    cli::cli_alert_info("Computing height-weighted greenspace volume per unit area...")
    # per-cell area (m^2) in metric CRS
    cell_area_m2 <- terra::cellSize(r_m, unit = "m")

    # buffer area (m^2): exact circle area in meters
    buffer_area <- pi * (radius^2)

    # volume per cell: green_fraction * height * cell_area
    # (if r is binary/fractional, this yields green canopy volume in m^3 per cell)
    vol_stack <- r_m * chm_m * cell_area_m2

    # sum volumes within the circle
    vol_sum <- terra::focal(vol_stack, w = w, fun = sum, na.policy = "omit", expand = TRUE, filename = "", na.rm = TRUE)

    # normalize to per-unit-area metric (m of "effective green height")
    G_stack <- vol_sum / buffer_area
  }
  terra::set.names(G_stack, paste0("G_", seq_len(n_layers)))

  # ---------- 5) Population-weight each pixel (P_i * G_i^d) ----------
  cli::cli_alert_info("Weighting by population...")
  PW_stack <- G_stack * pop_m
  terra::set.names(PW_stack, paste0("pwgf_", seq_len(n_layers)))

  # Optionally include population as a layer
  if (isTRUE(pop_out)) {
    PW_stack <- c(PW_stack, pop_m)
    terra::set.names(PW_stack, c(terra::names(PW_stack)[seq_len(n_layers)], "population"))
  }

  # ---------- 6) Optional aggregation to a grid (PWGE) ----------
  if (is.null(grid_size)) {
    cli::cli_alert_info("Returning per-cell population-weighted greenspace layers (pwgf_*).")
    if (exists("report_time", mode = "function")) report_time(start_time)
    return(PW_stack)
  }

  cli::cli_alert_info("Aggregating to regular grid and computing PWGE ...")
  # template grid in metric CRS
  r_template <- terra::rast(terra::ext(r_m), resolution = grid_size, crs = terra::crs(r_m))
  grid_v   <- terra::as.polygons(r_template)
  grid_sf  <- sf::st_as_sf(grid_v)

  # Sum numerator (sum of P_i * G_i^d) in each grid cell
  num_df <- terra::extract(PW_stack[[seq_len(n_layers)]], grid_v, fun = sum, na.rm = TRUE)
  # Sum denominator (sum of P_i) in each grid cell
  den_df <- terra::extract(pop_m, grid_v, fun = sum, na.rm = TRUE)

  # Build sf with ratios
  out_sf <- grid_sf
  # avoid zero division
  den_vec <- den_df[, 2]
  den_vec[is.na(den_vec)] <- 0
  den_vec_safe <- ifelse(den_vec <= 0, NA_real_, den_vec)

  for (k in seq_len(n_layers)) {
    num_vec <- num_df[, k + 1]
    out_sf[[paste0("pwge_", k)]] <- num_vec / den_vec_safe
  }
  if (isTRUE(pop_out)) {
    out_sf[["population"]] <- den_vec
  }

  if (exists("report_time", mode = "function")) report_time(start_time)
  return(out_sf)
}

# compute_exposure <- function(r = NULL,
#                     res = c(10,10),
#                     pop_year = 2020,
#                     radius = 500,
#                     grid_size = NULL,
#                     height = FALSE,
#                     pop_out = FALSE) {
#   start_time <- Sys.time()
#   if (is.null(r)) {
#     return(NULL)
#   }
#
#   bbox <- sf::st_as_sfc(sf::st_bbox(as.vector(terra::ext(r))))
#   sf::st_crs(bbox) <- 4326
#   bbox <- sf::st_transform(bbox, crs = 4326)
#
#   # download population layer
#   pop <- download_GHSL(bbox, pop_year)
#
#   cli::cli_alert_info('Computing greenspace area')
#   # calculate greenspace area
#   r <- r * res[1] * res[2]
#
#   # download chm and compute average height in each cell
#   if (height) {
#     cli::cli_alert_info('Downloading canopy height data ...')
#     bbox_vct <- as.vector(sf::st_bbox(bbox))
#     chm <- suppressMessages(
#       dsmSearch::get_dsm_30(bbox = bbox_vct,
#                             datatype = 'metaCHM')
#     )
#     chm <- terra::project(chm, 'EPSG:4326', method = 'bilinear')
#     avg_chm <- terra::resample(x = chm, y = r, method = 'average')
#     r <- r * avg_chm
#   }
#
#   cli::cli_alert_info('Calculating the average greenspace fraction')
#   # Within the buffer, calculate the average greenspace fraction
#   # Area of greenspace within buffer / Total area of buffer
#   pop_pts <- terra::as.points(pop, values = TRUE)
#   buffers <- terra::buffer(pop_pts, width = radius + 50)
#   buffer_area <- pi * (radius + 50)^2
#   zonal_buffers <- terra::zonal(x = r, z = buffers, fun = "sum", as.polygons = TRUE)
#   n_name <- length(terra::names(zonal_buffers))
#   terra::set.names(zonal_buffers,
#                    c('population', paste('g_area_', 1:(n_name - 1), sep = "")))
#   zonal_buffers_sf <- sf::st_as_sf(terra::centroids(zonal_buffers))
#
#   cli::cli_alert_info('Weighted based on population')
#   # population-weighted
#   buffer_green_fraction <- zonal_buffers_sf[, -c(1)]
#   for (i in 1:(ncol(buffer_green_fraction) - 1)) {
#     buffer_green_fraction[, i] <- buffer_green_fraction[[i]] / buffer_area * zonal_buffers_sf[[i]]
#   }
#
#   # to raster
#   buffer_green_fraction_vect <- terra::vect(sf::st_as_sf(buffer_green_fraction))
#   temp_raster <- terra::rast(terra::ext(pop),
#                              resolution = terra::res(pop),
#                              crs = terra::crs(pop),
#                              nlyrs = n_name - 1)
#   layer_names <- names(buffer_green_fraction_vect)
#   raster_list <- lapply(layer_names, function(colname) {
#     temp_r <- terra::rasterize(buffer_green_fraction_vect,
#                                temp_raster,
#                                field = colname)
#     terra::set.names(temp_r, colname)
#     return(temp_r)
#   })
#   r_stack <- terra::rast(raster_list)
#
#   if (is.null(grid_size)) {
#     cli::cli_alert_info('Return the population-weighted greenspace fraction (pwgf)')
#     # return the population-weighted greenspace fraction
#     out <- r_stack
#     terra::set.names(out, paste('pwgf_', 1:(n_name - 1), sep = ""))
#     report_time(start_time)
#     return(out)
#   } else {
#     cli::cli_alert_info('Computing population-weighted greenspace exposure (pwge)')
#     # generate a grid over layers
#     utm_crs <- get_utm_crs(bbox)
#     r_proj <- terra::project(temp_raster, paste0("EPSG:", utm_crs))
#     template <- terra::rast(terra::ext(r_proj),
#                             resolution = grid_size,
#                             crs = terra::crs(r_proj))
#     grid <- terra::as.polygons(template)
#     # grid$id <- 1:nrow(grid)
#     grid_wgs84 <- terra::project(grid, "EPSG:4326")
#
#     # zonal statistics
#     sum_pop <- terra::zonal(x = pop, z = grid_wgs84, fun = "sum",
#                             as.polygons = TRUE, na.rm = TRUE)
#     sum_pwgf <- terra::zonal(x = r_stack, z = grid_wgs84, fun = "sum",
#                              as.polygons = TRUE, na.rm = TRUE)
#     terra::set.names(sum_pop, 'population')
#     terra::set.names(sum_pwgf, paste('pwge_', 1:(n_name - 1), sep = ""))
#     sum_pop_sf <- sf::st_as_sf(sum_pop)
#     sum_pwgf_sf <- sf::st_as_sf(sum_pwgf)
#     sum_pop_sf$population[sum_pop_sf$population < 1e-6] <- 1.0000001
#     for (i in 1:(ncol(sum_pwgf_sf)-1)) {
#       sum_pwgf_sf[, i] <- sum_pwgf_sf[[i]] / sum_pop_sf[[1]]
#     }
#     report_time(start_time)
#     return(sum_pwgf_sf)
#   }
# }


#' ndvi_to_sem
#' @description
#' Convert ndvi raster data into semantic vegetation areas
#' @param r A SpatRaster with single greenspace layer, typically
#' the output from [get_esa_wc()], or [get_s2a_ndvi()].
#' @param threshold numeric vector of two. Thresholds, defaulting to `c(0.2, 0.5)`,
#' for classify two types of vegetation areas according to Hashim et al. (2019):
#' (1) Non-vegetation (Development and bare land): NDVI values generally below `0.2`.
#' (2) Low vegetation (Shrub and grassland): NDVI values generally between `0.2` and `0.5`.
#' (2) High vegetation (Temperate and Tropical urban forest ): NDVI values generally
#' between `0.5` and `1.0`.
#' @param quiet logical. Whether show progress bars for some process.
#'
#' @return SpatRaster. A raster, where `0` represents non-green area, `1` represents
#' shrub and grassland, and `2` represents trees.
#'
#' @examples
#' sample_data <- terra::rast(system.file("extdata", "detroit_gs.tif", package = "greenSD"))
#' seg <- ndvi_to_sem(sample_data$`25_NDVI`, threshold = c(0.2, 0.6))
#'
#' @references
#' Hashim, H., Abd Latif, Z., & Adnan, N. A. (2019). Urban vegetation classification
#' with NDVI threshold value method with very high resolution (VHR) Pleiades imagery.
#' The International Archives of the Photogrammetry, Remote Sensing and Spatial
#' Information Sciences, 42, 237-240.
#'
#' @export
ndvi_to_sem <- function(r = NULL, threshold = c(0.2, 0.5), quiet = FALSE) {
  if (is.null(r)) {
    return(NULL)
  }
  if (quiet) {
    terra::terraOptions(progress=0)
    on.exit(terra::terraOptions(progress=3), add = TRUE)
  }
  r <- terra::ifel(r > threshold[1], r, 0)
  r <- terra::ifel(r >= threshold[1] & r <= threshold[2], 10, r)
  r <- terra::ifel(r > threshold[2] & r <= 1, 20, r)
  r <- r/10
  return(r)
}

#' compute_morphology
#' @description
#' Compute greenspace morphology metrics at patch (Nowosad & Stepinski, 2019)
#' or landscape level (see details), including average size (AREA_MN), fragmentation (PD),
#' connectedness (COHESION), aggregation (AI), and complexity of the shape (SHAPE_AM),
#' related to public health (Wang et al., 2024)
#' @param r SpatRaster. A single-band binary greenspace raster, where 0 or NA
#' represents non-green areas and 1 represents green areas.
#' @param directions numeric. The number of directions in which patches should be
#' connected: 4 (default) or 8.
#' @param grid_size numeric or sf polygons. (Optional) If specified, morphology metrics
#' at grid level will be computed based on the size (in meters) of given grid cells or
#' input (sf) polygons.
#' @param quiet logical. Whether show progress bars for some process.
#'
#' @return
#' A SpatVector object contains indivisual patches with metrics at patch level,
#' when `grid_size = NULL`.
#'
#' A SpatVector object contains landscape-level value of metrics,
#' when `grid_size` is not `NULL`.
#'
#' @details
#' To get information of metrics, please use `landscapemetrics::list_lsm()`.
#'
#' @examples
#' green <- get_tile_green(
#'                         # bbox = c(-83.087174,42.333373,-83.042542,42.358748),
#'                         provider = "esri",
#'                         zoom = 16)
#' # p <- terra::ifel(green$green == 0, NA, 1)
#' m <- compute_morphology(
#'                        #r = p
#'                        directions = 8)
#'
#' @references
#' Nowosad J., TF Stepinski. 2019. Information theory as a consistent framework for
#' quantification and classification of landscape patterns.
#' https://doi.org/10.1007/s10980-019-00830-x
#'
#' Wang, H., & Tassinary, L. G. (2024). Association between greenspace morphology
#' and prevalence of non-communicable diseases mediated by air pollution and physical
#' activity. Landscape and Urban Planning, 242, 104934.
#'
#' @importFrom terra classify as.polygons
#' @importFrom cli cli_progress_done cli_progress_update cli_progress_bar
#' @export
compute_morphology <- function(r = NULL, directions = 4, grid_size = NULL, quiet = TRUE) {
  if (is.null(r)) return(NULL)
  if (!directions %in% c(4, 8)) stop("directions must be 4 or 8")
  if (quiet) {
    terra::terraOptions(progress=0)
    on.exit(terra::terraOptions(progress=3), add = TRUE)
  }
  start_time <- Sys.time()

  bbox <- sf::st_as_sfc(sf::st_bbox(as.vector(terra::ext(r[[1]]))))
  sf::st_crs(bbox) <- 4326
  bbox <- sf::st_transform(bbox, crs = 4326)

  utm_crs <- get_utm_crs(bbox)
  r_proj <- terra::project(r, paste0("EPSG:", utm_crs))

  # generate a grid over patches
  grid <- NULL
  if (!is.null(grid_size)) {
    if (is.numeric(grid_size)) {
      template <- terra::rast(terra::ext(r_proj),
                              resolution = grid_size,
                              crs = terra::crs(r_proj))
      grid <- terra::as.polygons(template)
      grid <- sf::st_as_sf(grid)
    } else if (inherits(grid_size, "sf")) {
      grid <- sf::st_transform(grid, paste0("EPSG:", utm_crs))
    }
  }

  r_proj <- terra::ifel(r_proj == 0, NA, 1)
  if (is.null(grid_size)) {
    patches <- landscapemetrics::get_patches(r_proj, directions = directions)[[1]][[1]]
    names(patches) <- "patch_id"

    cli::cli_alert_info('Computing metrics at patch level ...')
    metrics_df <- compute_landscape_metrics_parallel(r_proj, directions)
    cli::cli_alert_info('Finished computing metrics.')

    layers <- list()
    layers[['patch_id']] <- patches

    cli::cli_progress_bar(
      name = "Rasterizing metrics ",
      total = length(names(metrics_df)[-1])
    )
    for (col in names(metrics_df)[-1]) {
      cli::cli_progress_update()
      rcl <- metrics_df[, c("id", col)]
      temp <- terra::classify(patches, rcl = rcl, include.lowest = TRUE)
      names(temp) <- col
      layers[[col]] <- temp
    }
    cli::cli_progress_done()

    patch_metrics <- terra::rast(layers)
    patch_polygons <- terra::as.polygons(patch_metrics$patch_id, aggregate = TRUE)
    patch_data <- terra::extract(patch_metrics, patch_polygons, fun = mean, na.rm = TRUE)
    patch_cells <- terra::extract(r_proj, patch_polygons, fun = sum, na.rm = TRUE)
    names(patch_cells) <- c("id", "total_cells")
    patch_polygons <- cbind(patch_polygons, patch_data[,-1])
    patch_polygons <- cbind(patch_polygons, patch_cells[,-1])
    report_time(start_time)
    return(sf::st_as_sf(patch_polygons))
  } else {
    cli::cli_alert_info('Computing metrics at landscape level ...')
    results <- compute_landscape_l_metrics(r_proj, grid)
    report_time(start_time)
    return(results)
  }
}
