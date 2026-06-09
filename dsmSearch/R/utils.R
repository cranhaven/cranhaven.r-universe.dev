#' @import sf
#' @importFrom graphics par
#' @importFrom grDevices rgb
#' @importFrom utils download.file
#' @importFrom httr2 req_url_query
#' @importFrom httr2 req_headers
#' @import cli
#' @import aws.s3
#' @import stringr


#' @noMd
pt2bbox <- function(x, y, r, proj, longlat){
  coor <- data.frame(lon=x, lat=y)
  suppressWarnings(
    pt <- sp::SpatialPoints(coor, proj4string=longlat)
  )
  suppressWarnings(
    pt <- sp::spTransform(pt, proj)
  )
  xmin <- pt@coords[1,1] - r
  xmax <- pt@coords[1,1] + r
  ymin <- pt@coords[1,2] - r
  ymax <- pt@coords[1,2] + r
  coor_ <- data.frame(lon=c(xmin, xmax), lat=c(ymin, ymax))
  suppressWarnings(pt_ <- sp::SpatialPoints(coor_, proj))
  suppressWarnings(
    pt_ <- sp::spTransform(pt_, CRSobj=longlat)
  )
  return(list(c(pt_@coords[1,1], pt_@coords[1,2], pt_@coords[2,1], pt_@coords[2,2]),
              c(xmin, ymin, xmax, ymax)))
}

#' @noMd
convertBbox <- function(bbox, proj, longlat) {
  coor <- data.frame(lon=c(bbox[1],bbox[3]),
                      lat=c(bbox[2],bbox[4]))
  pt <- sp::SpatialPoints(coor, proj4string=longlat)
  pt <- sp::spTransform(pt, proj)
  xmin <- pt@coords[1,1]
  ymin <- pt@coords[1,2]
  xmax <- pt@coords[2,1]
  ymax <- pt@coords[2,2]
  return(list(bbox,c(xmin, ymin, xmax, ymax)))
}

#' @noMd
# create a request of the TNMAccess API
return_response <- function(bbox, max_return) {
  api1 <- 'https://tnmaccess.nationalmap.gov/api/v1/products?bbox='
  api2 <- paste0(bbox[1], ",",
                 bbox[2], ",",
                 bbox[3], ",",
                 bbox[4])
  api3 <- paste0('&datasets=Lidar%20Point%20Cloud%20(LPC)&max=',
                 max_return,
                 '&prodFormats=LAS,LAZ')
  json <- httr2::request(paste0(api1, api2, api3)) %>%
    httr2::req_timeout(10000) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  items <- length(json$items)
  titles <- c()
  sourceId <- c()
  metaUrl <- c()
  sizeInBytes <- c()
  startYear <- c()
  previewGraphicURL <- c()
  downloadLazURL <- c()
  if (items >= 1) {
    for (i in 1:items) {
      item <- json[[2]][[i]]
      titles <- c(titles, item$title)
      sourceId <- c(sourceId, item$sourceId)
      url <- paste0(item$metaUrl, "?format=json")
      metaUrl <- c(metaUrl, url)
      sizeInBytes <- c(sizeInBytes, item$sizeInBytes)
      startYear <- c(startYear, find_year(url))
      previewGraphicURL <- c(previewGraphicURL, item$previewGraphicURL)
      downloadLazURL <- c(downloadLazURL, item$downloadLazURL)
    }
    df <- data.frame(titles, sourceId,
                     metaUrl, sizeInBytes,
                     startYear, previewGraphicURL,
                     downloadLazURL)
    return(df)
  }
}

#' @noMd
# find year
find_year <- function(url) {
  j <- httr2::request(url) %>%
    httr2::req_timeout(10000) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  date <- j$dates[[2]]$dateString %>% strsplit("-") %>% unlist()
  return(as.integer(date[1]))
}

#' @noMd
# Terms of Use: https://opentopography.org/usageterms#:~:text=You%20agree%20to%2C%20and%20will,and%20their%20OpenTopography%20accounts%20closed.&text=We%20retain%20the%20right%20to,who%20abuse%20the%20system%20intentionally.
# Version: As of May 24th 2021 OpenTopography is supplying V3.2 (Jan 2021) from:
# ftp://ftp.eorc.jaxa.jp//pub/ALOS/ext1/AW3D30/release_v2012_single_format/
# Data downloaded prior to May 24th 2021 was in format: May 2016: Global terrestrial region
# (within approx. 82 deg. of N/S latitudes) of Version 1 released (approx. 22,100 tiles)
return_response2 <- function(bbox, key, datatype) {
  if (datatype %in% c('AW3D30','SRTMGL1')) {
    url_ <- "https://portal.opentopography.org/API/globaldem"
  } else if (datatype %in% c('USGS1m','USGS10m', 'USGS30m')) {
    url_ <- "https://portal.opentopography.org/API/usgsdem"
  }
  response <- httr2::request(url_) %>%
    httr2::req_url_query(demtype = datatype,
                         south = bbox[2],
                         north = bbox[4],
                         west = bbox[1],
                         east = bbox[3],
                         outputFormat = "GTiff",
                         API_Key = key) %>%
    httr2::req_headers(accept = "*/*") %>%
    httr2::req_perform()
  return(response)
}

#' @noMd
retry_download <- function(url, destination, method = "auto", retries = 5, quiet = TRUE) {
  attempt <- 1
  success <- FALSE

  while (attempt <= retries && !success) {
    try({
      download.file(url, destination, method = method, quiet = quiet)
      success <- TRUE
    }, silent = TRUE)

    if (!success) {
      # message(paste("Attempt", attempt, "failed for URL:", url))
      Sys.sleep(5) # Wait before retrying, to avoid hammering the server
    }
    attempt <- attempt + 1
  }

  if (success) {
    return(1)
  } else {
    return(0)
  }
}

# This fucntion is sourced from `forestdata` package by Cidre González Adrián
# Data may be freely used for research, study, or teaching, but be cited
# appropriately (see references below):
# <https://registry.opendata.aws/dataforgood-fb-forests/>
#' @noMd
canopy_height_meta <- function(x = NULL,
                               lon = NULL,
                               lat = NULL,
                               crop = FALSE,
                               mask = FALSE,
                               merge_tiles = FALSE,
                               quiet = TRUE) {
  # 1) Validate inputs
  if (is.null(x) && (is.null(lon) || is.null(lat))) {
    cli::cli_abort("Provide either {.arg x} (sf/sfc) or both {.arg lon} and {.arg lat}.")
  }
  if (!exists("meta_tiles_sf", inherits = TRUE)) {
    cli::cli_abort("Internal dataset {.val meta_tiles_sf} not found. Include it in package data.")
  }

  # Build query geometry in WGS84 (EPSG:4326)
  if (!is.null(lon) && !is.null(lat)) {
    xwgs84 <- sf::st_as_sf(
      data.frame(lon = lon, lat = lat),
      coords = c("lon", "lat"),
      crs = 4326
    )
  } else {
    if (!inherits(x, c("sf", "sfc"))) {
      cli::cli_abort("{.arg x} must be an {.cls sf}/{.cls sfc} object.")
    }
    xwgs84 <- sf::st_transform(x, 4326)
  }

  # 2) Select intersecting tiles (base extraction to avoid NOTES)
  tiles_sf <- sf::st_filter(meta_tiles_sf, xwgs84)
  tiles_df <- sf::st_drop_geometry(tiles_sf)
  tile_vec <- unique(tiles_df[["tile"]])

  if (length(tile_vec) == 0) {
    cli::cli_abort("No tiles have been found in the selected area.")
  }

  # 3) Download tiles to tempdir (OS-safe paths)
  out_files <- file.path(tempdir(), paste0(tile_vec, ".tif"))

  if (!quiet) {
    cli::cli_alert_info("Downloading {length(tile_vec)} tile{?s}...")
    download_pb <- cli::cli_progress_bar(
      "Downloaded tiles",
      total = length(tile_vec),
      type = "tasks",
      format_done = "{.alert-success Download completed {.timestamp {cli::pb_elapsed}}}",
      clear = FALSE
    )
  }

  for (i in seq_along(out_files)) {
    if (!file.exists(out_files[i])) {
      # swallow errors; we'll check file existence after
      try(
        aws.s3::save_object(
          object = paste0("forests/v1/alsgedi_global_v6_float/chm/", tile_vec[i], ".tif"),
          bucket = "dataforgood-fb-data",
          file   = out_files[i],
          region = "us-east-1"
        ),
        silent = TRUE
      )
    }
    if (!file.exists(out_files[i])) {
      if (!quiet) cli::cli_progress_done(id = download_pb)
      cli::cli_abort("Failed to retrieve tile {.val {tile_vec[i]}}. Service might be unavailable.")
    }
    if (!quiet) cli::cli_progress_update(id = download_pb)
  }
  if (!quiet) cli::cli_progress_done(id = download_pb)

  # 4) Read rasters
  r_list <- lapply(out_files, terra::rast)

  # 5) Optional crop/mask (only meaningful with x)
  if ((crop || mask) && is.null(x)) {
    cli::cli_warn("Ignoring {.arg crop}/{.arg mask} because {.arg x} was not supplied.")
  }
  if (!is.null(x)) {
    x3857 <- sf::st_transform(x, 3857)
    v     <- terra::vect(x3857)
    if (isTRUE(crop)) r_list <- lapply(r_list, function(rr) terra::crop(rr, v))
    if (isTRUE(mask)) r_list <- lapply(r_list, function(rr) terra::mask(rr, v))
  }

  # 6) Merge or return collection
  if (isTRUE(merge_tiles) && length(r_list) > 1) {
    if (!quiet) {
      cli::cli_alert_info("Merging {length(r_list)} tile{?s}...")
      merge_pb <- cli::cli_progress_bar(
        "Merged tiles",
        total = length(r_list) - 1,
        type = "tasks",
        format_done = "{.alert-success Merge completed {.timestamp {cli::pb_elapsed}}}",
        clear = FALSE
      )
    }
    r_final <- r_list[[1]]
    for (i in 2:length(r_list)) {
      r_final <- terra::merge(r_final, r_list[[i]])
      if (!quiet) cli::cli_progress_update(id = merge_pb)
    }
    if (!quiet) cli::cli_progress_done(id = merge_pb)
  } else if (!isTRUE(merge_tiles) && length(r_list) > 1) {
    r_final <- terra::sprc(r_list)
  } else {
    r_final <- r_list[[1]]
  }

  names(r_final) <- "canopy_height"
  if (!quiet) {
    cli::cli_alert_success(
      "Cite this dataset: {.url https://doi.org/10.1016/j.rse.2023.113888}"
    )
  }
  r_final
}

# This fucntion is sourced from `forestdata` package by Cidre González Adrián
# Data may be freely used for research, study, or teaching, but be cited
# appropriately (see references below):
# Lang, Nico, Walter Jetz, Konrad Schindler, and Jan Dirk Wegner.
# "A high-resolution canopy height model of the Earth."
# arXiv preprint arXiv:2204.08322 (2022).
#' @noMd
canopy_height_eth <- function(x = NULL,
                              lon = NULL,
                              lat = NULL,
                              layer = "chm",
                              crop = FALSE,
                              mask = FALSE,
                              merge_tiles = FALSE,
                              quiet = TRUE) {

  layer <- match.arg(layer)

  # Basic deps
  if (!exists("gch_tbl", inherits = TRUE)) {
    cli::cli_abort("Internal dataset {.val gch_tbl} not found. Include it in package data.")
  }

  # Validate inputs
  if (is.null(x) && (is.null(lon) || is.null(lat))) {
    cli::cli_abort("Provide either {.arg x} (sf/sfc) or both {.arg lon} and {.arg lat}.")
  }
  if (!is.null(x) && !inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("{.arg x} must be an {.cls sf}/{.cls sfc} object.")
  }

  # Build tile indices (3-degree grid) either from point or from bbox of x
  if (!is.null(lon) && !is.null(lat)) {
    lon_seq <- floor(lon / 3) * 3
    lat_seq <- floor(lat / 3) * 3
  } else {
    xwgs84 <- sf::st_transform(x, 4326)
    bb     <- sf::st_bbox(xwgs84)
    lon_seq <- seq(floor(bb["xmin"] / 3) * 3, floor(bb["xmax"] / 3) * 3, by = 3)
    lat_seq <- seq(floor(bb["ymin"] / 3) * 3, floor(bb["ymax"] / 3) * 3, by = 3)
  }

  # Select tiles from gch_tbl (base subsetting to avoid NSE NOTES)
  if (!all(c("lat", "lon", "url") %in% names(gch_tbl))) {
    cli::cli_abort("{.val gch_tbl} must have columns {.val lat}, {.val lon}, {.val url}.")
  }
  tile_tbl <- gch_tbl[gch_tbl[["lon"]] %in% lon_seq & gch_tbl[["lat"]] %in% lat_seq, , drop = FALSE]
  if (nrow(tile_tbl) == 0) {
    cli::cli_abort("No tiles have been found for the selected area.")
  }

  # Derive layer type if missing; standard deviation tiles typically have "_SD" in URL
  has_layer_col <- "layer" %in% names(tile_tbl)
  if (!has_layer_col) {
    tile_tbl[["layer"]] <- ifelse(grepl("_SD", tile_tbl[["url"]], fixed = TRUE), "std", "chm")
  } else {
    # normalize layer values
    tile_tbl[["layer"]] <- tolower(trimws(tile_tbl[["layer"]]))
    tile_tbl[["layer"]] <- ifelse(tile_tbl[["layer"]] %in% c("std", "_sd", "sd"), "std", "chm")
  }

  # Filter by requested layer
  if (layer != "all") {
    tile_tbl <- tile_tbl[tile_tbl[["layer"]] == layer, , drop = FALSE]
    if (nrow(tile_tbl) == 0) {
      cli::cli_abort("No tiles found after filtering for layer {.val {layer}}.")
    }
  }

  ids <- tile_tbl[["url"]]
  if (!quiet) {
    cli::cli_alert_info("Downloading {length(ids)} tile{?s}...")
    pb <- cli::cli_progress_bar(
      "Downloaded tiles",
      total = length(ids),
      type = "tasks",
      format_done = "{.alert-success Download completed {.timestamp {cli::pb_elapsed}}}",
      clear = FALSE
    )
  }

  # Silence terra's progress bar temporarily
  old_terra_opts <- terra::terraOptions(progress = 0) # returns previous
  on.exit(terra::terraOptions(progress = old_terra_opts$progress), add = TRUE)

  tiles_list <- vector("list", length(ids))
  for (i in seq_along(ids)) {
    tiles_list[[i]] <- fdi_download_raster(
      url   = ids[i],
      start = 38,  # keep your helper's defaults; adjust if your helper changes
      end   = 80
    )
    if (is.null(tiles_list[[i]])) {
      if (!quiet) cli::cli_progress_done(id = pb)
      cli::cli_abort("Failed to retrieve tile: {.url {ids[i]}}. Service might be unavailable.")
    }
    if (!quiet) cli::cli_progress_update(id = pb)
  }
  if (!quiet) cli::cli_progress_done(id = pb)

  # Optional crop/mask (meaningful only with x)
  if ((crop || mask) && is.null(x)) {
    cli::cli_warn("Ignoring {.arg crop}/{.arg mask} because {.arg x} was not supplied.")
  }
  if (!is.null(x)) {
    # Reproject x to raster CRS before crop/mask
    target_crs <- try(terra::crs(tiles_list[[1]]), silent = TRUE)
    x_proj <- tryCatch(
      sf::st_transform(x, target_crs),
      error = function(e) sf::st_transform(x, 4326) # fallback
    )
    v <- terra::vect(x_proj)
    if (isTRUE(crop)) tiles_list <- lapply(tiles_list, function(rr) terra::crop(rr, v))
    if (isTRUE(mask)) tiles_list <- lapply(tiles_list, function(rr) terra::mask(rr, v))
  }

  # Assemble output
  make_merged <- function(lst) {
    if (length(lst) == 1L) return(lst[[1L]])
    Reduce(terra::merge, lst)
  }
  make_sprc <- function(lst) {
    if (length(lst) == 1L) return(lst[[1L]])
    terra::sprc(lst)
  }

  if (layer == "all") {
    is_std <- tile_tbl[["layer"]] == "std"
    chm_list <- tiles_list[!is_std]
    std_list <- tiles_list[ is_std]

    if (length(chm_list) == 0 || length(std_list) == 0) {
      cli::cli_abort("Requested {.val layer = 'all'}, but CHM/STD pairing not available for selection.")
    }

    if (isTRUE(merge_tiles)) {
      chm <- make_merged(chm_list)
      std <- make_merged(std_list)
      out <- c(chm, std)
      names(out) <- c("chm", "std")
    } else {
      out <- list(
        chm = make_sprc(chm_list),
        std = make_sprc(std_list)
      )
    }

  } else {
    if (isTRUE(merge_tiles)) {
      out <- make_merged(tiles_list)
      names(out) <- layer
    } else {
      if (length(tiles_list) == 1L) {
        out <- tiles_list[[1L]]
        names(out) <- layer
      } else {
        out <- terra::sprc(tiles_list)
        names(out) <- layer
      }
    }
  }

  if (!quiet) {
    cli::cli_alert_success(
      "Cite this dataset: {.url https://doi.org/10.1038/s41559-023-02206-6}"
    )
  }
  out
}

# This fucntion is sourced from `forestdata` package by Cidre González Adrián
#' @noMd
fdi_download_raster <- function(url, start = NULL, end = NULL, timeout = 5000, quiet = TRUE) {

  ## 1. File name
  if (is.null(start) & is.null(end)) {
    url_path <- stringr::str_glue("{tempdir()}/{basename(url)}")
  } else {
    url_path <- stringr::str_glue("{tempdir()}/{basename(url) |> stringr::str_sub(start, end)}")
  }

  tryCatch(
    {
      if (!file.exists(url_path)) {
        ## Check for user's timeout
        old_timeout <- getOption("timeout")
        on.exit(options(timeout = old_timeout))
        ## Download file
        options(timeout = max(timeout, getOption("timeout")))
        download.file(
          url      = url,
          destfile = url_path,
          quiet    = quiet,
          mode     = "wb"
        )
      }
      ## 1.3. Read file
      r <- terra::rast(url_path)
      return(r)
    },
    error = function(e) {
      return(invisible(NULL))
    }
  ) |> suppressWarnings()

}

# This fucntion is sourced from `forestdata` package by Cidre González Adrián
#' @noMd
canopy_height <- function(x     = NULL,
                          lon   = NULL,
                          lat   = NULL,
                          model = NULL,
                          layer = "chm",
                          crop  = FALSE,
                          mask  = FALSE,
                          merge = FALSE,
                          quiet = TRUE) {
  # 0. Handle errors
  ## 0.1. Handle all NULL
  if (is.null(x) & is.null(lon) & is.null(lat)) cli::cli_abort("No coordinates or object were specified")
  ## 0.2. Handle non-existing coordinates
  if (!is.null(lon) & !is.null(lat)) {
    if (lon > 180 | lon < -180) cli::cli_abort("Invalid longitude coordinate value")
    if (lat > 80 | lat < -80) cli::cli_abort("Invalid latitude coordinate value")
  } else {
    if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  }
  ## 0.3. Handle incompatible arguments
  if (!is.null(lon) & !is.null(lat) & !is.null(x)) {
    cli::cli_abort("Both coordinates (`lon` and `lat`) and object (`x`) were specified. Specify only one of them.")
  }
  ## 0.4. Handle incompatible arguments (crop = TRUE & coords)
  if ((crop | mask | merge) & !is.null(lon) & !is.null(lat)) {
    v <- if (crop) "crop" else if (mask) "mask" else if (merge) "merge"
    cli::cli_abort("`{v} = TRUE` is only available when `x` is specified.")
  }
  ## 0.5. Handle model names
  if (!model %in% c("eth", "meta")) cli::cli_abort("model argument is not valid. Please, use either <eth> or <meta>.")

  # 1. Get data based on model
  if (model == "eth") {
    canopy_height_eth(x = x, lon = lon, lat = lat, layer = layer, crop = crop, mask = mask, merge_tiles = merge, quiet = quiet)
  } else if (model == "meta") {
    canopy_height_meta(x = x, lon = lon, lat = lat, crop = crop, mask = mask, merge_tiles = merge, quiet = quiet)
  }
}
