#' @importFrom sf read_sf st_intersects
#' @importFrom utils download.file
#' @importFrom terra rast merge nlyr rev map.pal project hist
#' @importFrom cli cli_alert_info cli_alert_success cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom magick image_read image_animate
#' @importFrom dplyr "%>%" mutate pull
#' @importFrom stringr str_pad
#' @importFrom grDevices dev.off png
#' @importFrom rlang .data
#' @importFrom rstac post_request stac ext_filter


#' @title Get all of the urban areas in the Greenspace Seasonality Data Cube
#' @name check_available_urban
#' @description This function returns all of the urban areas
#' in the Greenspace Seasonality Data Cube dataset.
#' @param test logical. (ignored) Only for testing.
#' @return dataframe
#' @references
#' Wu, S., Song, Y., An, J. et al. High-resolution greenspace dynamic
#' data cube from Sentinel-2 satellites over 1028 global major cities.
#' Sci Data 11, 909 (2024). https://doi.org/10.1038/s41597-024-03746-7
#' @note
#' You can explore all available urban areas in an interacive map at:
#' \url{https://github.com/billbillbilly/greenSD/blob/main/scripts/city_urban_boundaries.geojson}
#' @examples
#' check_available_urban(test = TRUE)
#' @export
check_available_urban <- function(test = FALSE) {
  if (isTRUE(test)) {
    return(NULL)
  }
  cli::cli_alert_info("You can also check all available cities in an interacive map here: https://github.com/billbillbilly/greenSD/blob/main/scripts/city_urban_boundaries.geojson")
  return(available_cities)
}

#' @title Get an urban area boundary based on the UID
#' @name check_urban_boundary
#' @description This function returns a polygon of a city boundary based on the UID
#' @param uid numeric. Urban area ID. To check the ID of an available urban area,
#' use [check_available_urban()]
#' @param plot logical. Whether to plot city boundary
#' @param test logical. (ignored) Only for testing.
#' @return sf
#' @references
#' Wu, S., Song, Y., An, J. et al. High-resolution greenspace dynamic
#' data cube from Sentinel-2 satellites over 1028 global major cities.
#' Sci Data 11, 909 (2024). https://doi.org/10.1038/s41597-024-03746-7
#' @examples
#' check_urban_boundary(test = TRUE)
#' @export
check_urban_boundary <- function(uid = NULL, plot = TRUE, test = FALSE) {
  if (isTRUE(test)) {
    return(NULL)
  }
  boundary <- suppressMessages(
    sf::read_sf('https://raw.githubusercontent.com/billbillbilly/greenSD/main/scripts/city_urban_boundaries.geojson')
  )
  b <- boundary[boundary$UID == uid, ]
  plot(b$geometry)
  return(b)
}

#' @title Convert A Multi-layer Raster to GIF
#' @description Export a multi-layer raster (`SpatRaster`) or vector layer (`sf`)
#' with multiple numeric value columns to an animated GIF.
#' @param r SpatRaster or sf. A SpatRaster with multiple layers or an sf object
#' with multiple numeric value columns.
#' @param fps numeric. Frames per second (default 5).
#' @param width numeric. Width of output GIF in pixels.
#' @param height numeric. Height of output GIF in pixels.
#' @param axes logical. Draw axes?
#' @param title_prefix character or character vector.
#' @param border character. Color of polygon border(s); using NA hides them.
#' Only optional when `r` is an sf object.
#' @return An animated magick image object (GIF).
#' @examples
#' sample_data <- terra::rast(system.file("extdata", "detroit_gs.tif", package = "greenSD"))
#' gif <- to_gif(sample_data)
#'
#' @export
to_gif <- function (r, fps = 5, width = 600, height = 600,
                    axes = TRUE, title_prefix = NULL, border = FALSE) {
  # Check type
  is_sf <- inherits(r, "sf")
  is_raster <- inherits(r, "SpatRaster")

  if (!is_sf && !is_raster) {
    stop("Input must be a SpatRaster or an sf object.")
  }
  if (is_raster) {
    stopifnot(inherits(r, "SpatRaster"), terra::nlyr(r) > 1)
  }

  temp_dir <- tempdir()
  img_paths <- character()

  on.exit(unlink(img_paths, recursive = TRUE), add = TRUE)

  if (is_raster) {
    n_frames <- terra::nlyr(r)
  } else if (is_sf) {
    cols <- names(r)[sapply(r, is.numeric) & names(r) != attr(r, "sf_column")]
    n_frames <- length(cols)
  }

  for (i in 1:n_frames) {
    png_file <- file.path(temp_dir, sprintf("frame_%02d.png", i))
    grDevices::png(png_file, width = width, height = height)

    if (is_raster) {
      # Handle dynamic or static title
      title <- if (is.null(title_prefix)) {
        paste("Day", i * 10)
      } else if (length(title_prefix) == 1) {
        paste(title_prefix, i)
      } else {
        title_prefix[i]
      }

      terra::plot(r[[i]],
                  col = terra::rev(terra::map.pal('viridis', 100)),
                  main = title,
                  axes = axes)
    } else if (is_sf) {
      # Handle dynamic or static title
      title <- if (is.null(title_prefix)) {
        cols[i]
      } else if (length(title_prefix) == 1) {
        paste(title_prefix, i)
      } else {
        title_prefix[i]
      }

      plot(r[cols[i]],
           key.pos = 4,
           main = title,
           border = border,
           axes = axes)
    }

    grDevices::dev.off()
    img_paths[i] <- png_file
  }

  frames <- magick::image_read(img_paths)
  animation <- magick::image_animate(frames, fps = fps)
  return(animation)
}

#' @title Get band index based on time period
#' @name get_band_index_by_time
#' @description
#' Converts a date string in `"MM-DD"` format to the corresponding band index
#' for the Greenspace Seasonality Data Cube, which contains 36 bands representing
#' 10-day intervals over a year.
#'
#' @param year numeric. (required) The year of interest.
#' @param time Character vector of length 2. (optional) Start and end dates in `"MM-DD"` format
#' (e.g., `c("03-20", "10-15")`). Used to subset the 10-day interval data cube by time.
#'
#' @return Interger. a band index.
#'
#' @examples
#' get_band_index_by_time(c("03-20", "10-15"), year = 2020)
#'
#' @details
#' The Greenspace Data Cube is organized into 36 bands per year, each representing a 10-day interval.
#' This function calculates which of those bands a given date falls into by converting the MM-DD
#' string into the day-of-year (DOY) and dividing by 10 (rounded up).
#' @export
get_band_index_by_time <- function(time, year) {
  date_obj <- as.Date(paste0(year, '-', time))
  day_of_year <- as.integer(format(date_obj, "%j"))
  band_index <- ((day_of_year - 1) %/% 10) + 1
  return(as.integer(band_index))
}

#' @noMd
check_overlap <- function(geometry) {
  boundary <- suppressMessages(
    sf::read_sf('https://raw.githubusercontent.com/billbillbilly/greenSD/main/scripts/city_urban_boundaries.geojson')
  )
  intersecting <- boundary[sf::st_intersects(boundary, geometry, sparse = FALSE), ]
  return(intersecting$UID[1])
}

#' @noMd
get_data_with_uid <- function(id, y) {
  dict <- data_dictionary[data_dictionary$year == y, ]
  dict <- dict[dict$uid == id, ]
  return(dict$download_url)
}

#' @noMd
download_data <- function(urls) {
  original_timeout <- getOption('timeout')
  options(timeout=9999)

  # check os
  d_mode <- if (Sys.info()[["sysname"]] == "Windows") "wb" else "auto"

  result_list <- list()
  temp_paths <- c()
  on.exit({
    options(timeout = original_timeout)
    unlink(temp_paths, recursive = TRUE)
  }, add = TRUE)

  cli::cli_alert_info('Start downloading seasonal greenspace data ...')
  if (length(urls) >= 2) {
    cli::cli_alert_info(
      "There are {length(urls)} tiles to download and process. This may take more than 5 minutes."
    )
  }
  cli::cli_progress_bar("Downloading", total = length(urls))
  for (i in seq_len(length(urls))) {
    cli::cli_progress_update()
    temp_tif <- tempfile(fileext = ".tif")
    utils::download.file(urls[i],
                         destfile = temp_tif,
                         mode = d_mode,
                         quiet = TRUE)
    rast_data <- terra::rast(temp_tif)
    result_list[[length(result_list) + 1]] <- rast_data
    temp_paths <- c(temp_paths, temp_tif)
  }
  cli::cli_progress_done()
  cli::cli_alert_success('Finished downloading data')

  if (length(result_list) == 1) {
    r <- result_list[[1]]
  } else {
    cli::cli_alert_info('Merging multiple tiles ...')
    r <- do.call(terra::merge, result_list)
  }
  cli::cli_alert_success("Data successfully processed.")
  return(r/1000)
}

#' @noMd
get_esa_tile_names <- function(lat_min, lat_max, lon_min, lon_max) {
  lat_range <- floor(lat_min):floor(lat_max)
  lon_range <- floor(lon_min):floor(lon_max)

  base::expand.grid(lat = lat_range, lon = lon_range) %>%
    dplyr::mutate(
      lat_label = ifelse(.data$lat >= 0, paste0("N", stringr::str_pad(.data$lat, 2, pad = "0")),
                         paste0("S", stringr::str_pad(abs(.data$lat), 2, pad = "0"))),
      lon_label = ifelse(.data$lon >= 0, paste0("E", stringr::str_pad(.data$lon, 3, pad = "0")),
                         paste0("W", stringr::str_pad(abs(.data$lon), 3, pad = "0"))),
      tile_name = paste0(.data$lat_label, .data$lon_label)
    ) %>%
    dplyr::pull(.data$tile_name)
}

#' @noMd
get_GHSurl <- function(year, id, type) {
  if (type == 'pop') {
    # source: https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
    return(
      paste0(
        'https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E',
        year,
        '_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E',
        year,
        '_GLOBE_R2023A_54009_100_V1_0_',
        id,
        '.zip'
      )
    )
  } else if (type == 'b_surf') {
    return(
      list(
        paste0(
          'https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_E',
          year,
          '_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_BUILT_S_E',
          year,
          '_GLOBE_R2023A_54009_100_V1_0_',
          id,
          '.zip'
        ),
        paste0(
          'https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_NRES_E',
          year,
          '_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_BUILT_S_NRES_E',
          year,
          '_GLOBE_R2023A_54009_100_V1_0_',
          id,
          '.zip'
        )
      )
    )
  }
}

#' @noMd
download_GHSL <- function(bbox, year) {
  d_mode <- 'auto'
  # check os
  os <- Sys.info()[["sysname"]]
  d_mode <- if (Sys.info()[["sysname"]] == "Windows") 'wb' else 'auto'
  years <- c(2030, 2025, 2020, 2015)
  result_list <- list()
  temp_paths <- c()
  on.exit(unlink(temp_paths, recursive = TRUE), add = TRUE)

  cli::cli_alert_info('Start downloading population data from the GHSL dataset ...')
  if (year %in% years) {
    intersected_tiles <- ghsl_tiles[sf::st_intersects(ghsl_tiles, bbox, sparse = FALSE), ]
    for (i in seq_len(nrow(intersected_tiles))) {
      temp_zip <- tempfile(fileext = ".zip")
      url_ <- get_GHSurl(year, intersected_tiles$tile_id[i], 'pop')
      utils::download.file(url_,
                           destfile = temp_zip,
                           mode = d_mode,
                           quiet = TRUE)
      unzip_dir <- tempfile()
      utils::unzip(temp_zip, exdir = unzip_dir)
      tif_files <- list.files(unzip_dir, pattern = "\\.tif$", full.names = TRUE)
      if (length(tif_files) == 0) next
      rast_data <- terra::rast(tif_files[1])
      result_list[[length(result_list) + 1]] <- rast_data
      temp_paths <- c(temp_paths, temp_zip, unzip_dir)
    }
    if (length(result_list) == 0) {
      base::warning("No population rasters downloaded. Returning original polygons.")
      return(NULL)
    }
    cli::cli_alert_success('Finished downloading population data')

    # Combine all into one terra raster object
    cli::cli_alert_info('Start peocessing population data ...')

    pop <- if (length(result_list) == 1) result_list[[1]] else do.call(terra::merge, c(result_list, list(algo = 3)))
    res <- terra::res(pop)
    cli::cli_alert_info('Cropping ...')
    bbox_moll <- terra::project(terra::vect(bbox), terra::crs(pop))
    # pop <- terra::mask(pop, bbox_moll)
    pop <- terra::crop(pop, terra::ext(bbox_moll))
    pop <- terra::mask(pop, bbox_moll)
    cli::cli_alert_info('Re-projecting ...')
    pop <- terra::project(pop, paste0('EPSG:', 4326), method = 'bilinear')

    cli::cli_alert_success('Finished cropping and re-projecting population data')
    return(pop)
  } else {
    stop('Wrong year.')
  }
}

#' @noMd
download_sentinel <- function (bbox, start_date, end_date,
                               cloud_cover = 10, vege_perc = 0) {
  original_timeout <- getOption('timeout')
  options(timeout=9999)
  on.exit(options(timeout = original_timeout))

  bbox <- as.vector(sf::st_bbox(bbox))
  polygon <- list(
    type = "Polygon",
    coordinates = list(
      matrix(c(bbox[1], bbox[2],
               bbox[3], bbox[2],
               bbox[3], bbox[4],
               bbox[1], bbox[4],
               bbox[1], bbox[2]),
             ncol = 2, byrow = TRUE)
    )
  )

  res <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
    rstac::ext_filter(
      collection == "sentinel-2-l2a" &&
        `eo:cloud_cover` <= !!cloud_cover &&
        `s2:vegetation_percentage` >= !!vege_perc &&
        anyinteracts(datetime, interval(!!start_date, !!end_date)) &&
        s_intersects(geometry, {{polygon}})
    ) %>%
    rstac::post_request()
  return(res$features)
}

#' @noMd
compute_ndvi <- function(b04, b08) {
  (b08 - b04) / (b08 + b04)
}

#' @noMd
get_the_date <- function(which, dates) {
  dfs <- lapply(dates, function(x) {
      date_split <- strsplit(x, "-")[[1]]
      data.frame(
        year = date_split[1],
        month = date_split[2],
        day = date_split[3]
      )
    }
  )
  combined_df <- do.call(rbind, dfs)
  if (which == 'earliest') {
    combined_df <- combined_df[combined_df[,1] == min(combined_df$year), ]
    combined_df <- combined_df[combined_df[,2] == min(combined_df$month), ]
    combined_df <- combined_df[combined_df[,3] == min(combined_df$day), ]
  } else if (which == 'latest') {
    combined_df <- combined_df[combined_df[,1] == max(combined_df$year), ]
    combined_df <- combined_df[combined_df[,2] == max(combined_df$month), ]
    combined_df <- combined_df[combined_df[,3] == max(combined_df$day), ]
  }
  return(paste0(combined_df[1,1], "-",
                combined_df[1,2], "-",
                combined_df[1,3])
         )
}

# This function was done by Xiaojiang Li, Ian Seiferling, Marwa Abdulhai, Senseable City Lab, MIT
#' @noMd
graythresh <- function(r, lv = 0.1) {
  r <- r*255
  r <- terra::ifel(r < 0, 0, r)
  hist_info <- terra::hist(terra::values(r), breaks = 0:256, plot = FALSE)
  p_hist <- hist_info$counts / sum(hist_info$counts)
  omega <- base::cumsum(p_hist)
  temp <- 0:255
  mu <- p_hist * (temp + 1)
  mu <- base::cumsum(mu)
  mu_t <- mu[length(mu)]
  sigma_b_squared <- (mu_t * omega - mu)^2 / (omega * (1 - omega))
  indInf <- which(is.infinite(sigma_b_squared))
  CIN <- length(indInf)
  IsAllInf <- CIN == 256
  threshold <- NULL
  if (!IsAllInf) {
    maxval <- max(sigma_b_squared, na.rm = TRUE)
    index <- which(sigma_b_squared == maxval)
    idx <- mean(index)
    threshold <- (idx - 1) / 255.0
  } else {
    threshold <- lv
  }
  if (is.na(threshold)) {
    threshold <- lv
  }
  return(threshold)
}

#' @noMd
write_eox_wms_xml <- function(bbox, year = 2024, zoom = 15) {
  if (length(bbox) != 4) stop("bbox must be a vector of 4 numbers: xmin, ymin, xmax, ymax")
  if (!year %in% 2016:2024) stop("Year must be between 2016 and 2024")
  if (zoom < 0 || zoom > 22) stop("Zoom should be between 0 and 22")

  layer_name <- paste0("s2cloudless-", year)
  xmin <- bbox[1]; ymin <- bbox[2]; xmax <- bbox[3]; ymax <- bbox[4]
  res_deg <- 360 / (256 * 2^zoom)
  sizex <- ceiling((xmax - xmin) / res_deg)

  xml_lines <- c(
    '<GDAL_WMS>',
    '  <Service name="WMS">',
    '    <Version>1.1.1</Version>',
    '    <ServerUrl>https://tiles.maps.eox.at/wms?</ServerUrl>',
    paste0('    <Layers>', layer_name, '</Layers>'),
    '    <SRS>EPSG:4326</SRS>',
    '    <Format>image/jpeg</Format>',
    '  </Service>',
    '  <DataWindow>',
    paste0('    <UpperLeftX>', bbox[1], '</UpperLeftX>'),
    paste0('    <UpperLeftY>', bbox[4], '</UpperLeftY>'),
    paste0('    <LowerRightX>', bbox[3], '</LowerRightX>'),
    paste0('    <LowerRightY>', bbox[2], '</LowerRightY>'),
    paste0('    <SizeX>', sizex, '</SizeX>'),
    paste0('    <SizeY>', sizex, '</SizeY>'),
    '  </DataWindow>',
    '  <Projection>EPSG:4326</Projection>',
    '  <BandsCount>3</BandsCount>',
    '</GDAL_WMS>'
  )
  return(xml_lines)
}

#' @noMd
scale2_0_1 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#' @importFrom future multisession availableCores plan
#' @importFrom purrr reduce
#' @importFrom furrr future_map
#' @importFrom dplyr full_join
#' @importFrom landscapemetrics lsm_p_perim lsm_p_area lsm_p_contig lsm_p_enn
#' @importFrom landscapemetrics lsm_p_core lsm_p_frac lsm_p_shape lsm_p_para
#' @importFrom landscapemetrics lsm_p_ncore lsm_p_gyrate lsm_p_circle lsm_p_cai
#' @noMd
compute_landscape_metrics_parallel <- function(r,
                                               directions = 4,
                                               metric_names = c("perim", "area", "contig",
                                                                "enn", "core", "frac",
                                                                "shape", "para", "ncore",
                                                                "gyrate", "circle", "cai"),
                                               progress = FALSE) {
  raster_path <- tempfile(fileext = ".tif")
  on.exit(unlink(raster_path), add = TRUE)
  terra::writeRaster(r, raster_path, overwrite = TRUE)
  old_plan <- future::plan()
  future::plan(future::multisession, workers = future::availableCores() - 1)
  on.exit(future::plan(old_plan), add = TRUE)

  compute_one_metric <- function(metric_name, raster_path, directions) {
    r_local <- terra::rast(raster_path)
    fname <- paste0("lsm_p_", metric_name)
    fn <- getExportedValue("landscapemetrics", fname)
    df <- fn(r_local, directions = directions)
    df <- df[, c("id", "value")]
    names(df)[2] <- toupper(metric_name)
    df
  }

  result_list <- furrr::future_map(
    metric_names,
    compute_one_metric,
    raster_path = raster_path,
    directions = directions,
    .progress = progress
  )

  results <- purrr::reduce(result_list, dplyr::full_join, by = "id")
  return(results)
}

#' @noMd
#' @importFrom landscapemetrics sample_lsm
#' @importFrom tidyr pivot_wider
compute_landscape_l_metrics <- function(r, grid) {
  metr <- landscapemetrics::list_lsm()
  metric_names = as.vector(metr[metr$level=='landscape', 1])
  landscapes <- suppressWarnings(
    landscapemetrics::sample_lsm(r, grid, what = paste0("lsm_l_", metric_names$metric))
  )
  landscapes_w <- tidyr::pivot_wider(landscapes,
                                     id_cols = plot_id,
                                     names_from = metric,
                                     values_from = value)
  return(cbind(grid, landscapes_w))
}

#' @noMd
report_time <- function(start_time) {
  end_time <- Sys.time()
  process_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  if (process_time >= 60) {
    cli::cli_alert_success(paste0("Completed. Time taken: ", round(process_time / 60), " minutes."))
  } else {
    cli::cli_alert_success(paste0("Completed. Time taken: ", round(process_time), " seconds."))
  }
}

#' @noMd
get_utm_crs <- function(bbox) {
  centroid <- sf::st_centroid(sf::st_union(bbox))
  coords <- sf::st_coordinates(centroid)
  lon <- coords[1]
  lat <- coords[2]
  zone <- floor((lon + 180) / 6) + 1
  epsg <- if (lat >= 0) {
    32600 + zone
  } else {
    32700 + zone
  }
  return(epsg)
}
