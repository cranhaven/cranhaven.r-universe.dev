#' Apply a method by direction using a constant field of view
#'
#' Applies a method to each set of pixels defined by a direction and a constant
#' field of view (FOV). By default, several built-in methods are available
#' (see `method`), but a custom function can also be provided via the `fun`
#' argument.
#'
#' @param spacing numeric vector of length one. Angular spacing (in degrees)
#'   between directions to process.
#' @param method character vector of length one. Built-in method to apply.
#'   Available options are `"thr_isodata"`, `"detect_bg_dn"`,
#'   `"fit_coneshaped_model"`, `"fit_trend_surface_np1"`, and
#'   `"fit_trend_surface_np6"`. Ignored if `fun` is provided.
#' @param fov numeric vector. Field of view in degrees. If more than one value
#'   is provided, they are tried in order when a method fails.
#' @param fun `NULL` (default) or a function accepting `r`, `z`, `a`, and `m` as
#'   input and returning a single-layer [terra::SpatRaster-class] object  with
#'   the same number of rows and columns as its first input, `r`.
#' @param parallel logical vector of length one. If `TRUE`, operations are
#'   executed in parallel.
#'
#' @inheritParams fisheye_to_equidistant
#' @inheritParams binarize_with_thr
#' @inheritParams sky_grid_segmentation
#' @inheritParams compute_canopy_openness
#' @inheritParams rem_outliers
#'
#' @return [terra::SpatRaster-class] object with two layers: `"dn"` for digital
#'   number values and `"n"` for the number of valid pixels used in each
#'   directional estimate.
#'
#' @note
#' This function is part of a manuscript currently under preparation.
#'
#' @export
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' r <- caim$Blue
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#' m <- !is.na(z)
#'
#' # Automatic sky brightness estimation
#' sky <- apply_by_direction(r, z, a, m, spacing = 10, fov = c(30, 60),
#'                           method = "detect_bg_dn", parallel = TRUE)
#' plot(sky$dn)
#' plot(r / sky$dn)
#'
#' # Using cone-shaped model
#' sky_cs <- apply_by_direction(caim, z, a, m, spacing = 15, fov = 60,
#'                              method = "fit_coneshaped_model", parallel = TRUE)
#' plot(sky_cs$dn)
#'
#' # Using trend surface model
#' sky_s <- apply_by_direction(caim, z, a, m, spacing = 15, fov = 60,
#'                             method = "fit_trend_surface_np1", parallel = TRUE)
#' plot(sky_s$dn)
#'
#' # Using a custom thresholding function
#' thr <- apply_by_direction(r, z, a, m, 15, fov = c(30, 40, 50),
#'   fun = function(r, z, a, m) {
#'     thr <- tryCatch(thr_twocorner(r[m])$tm, error = function(e) NA)
#'     r[] <- thr
#'     r
#'   },
#'   parallel = TRUE
#' )
#' plot(thr$dn)
#' plot(binarize_with_thr(r, thr$dn))
#' }
apply_by_direction <- function(r, z, a, m,
                               spacing = 10,
                               laxity = 2.5,
                               fov = c(30, 40, 50),
                               method = c("thr_isodata",
                                          "detect_bg_dn",
                                          "fit_coneshaped_model",
                                          "fit_trend_surface_np1",
                                          "fit_trend_surface_np6"),
                               fun = NULL,
                               parallel = FALSE) {

  .check_r_z_a_m(r, z, a, m, r_type = "any")
  .check_vector(spacing, "numeric", 1, sign = "positive")
  .check_vector(laxity, "numeric", 1, sign = "positive")
  .check_vector(fov, "numeric", sign = "positive")
  .check_vector(parallel, "logical", 1)

  if (is.null(fun) &&
      method %in% c("fit_coneshaped_model", "fit_trend_surface_np6",
                    "fit_trend_surface_np1") &&
      !all(names(r) == c("Red", "Green", "Blue"))) {
    stop(paste0("Method '", method, "' only works with a three-layer raster with
              layers named 'Red', 'Green', and 'Blue'."))
  }

  .thr_twocorner_up <- function(x) {
    for (gamma in c(1, 1.8, 2.2, 2.6)) {
      result <- tryCatch(
        (thr_twocorner(x^(1/gamma), sigma = 2, method = "prominence",
                       slope_reduction = TRUE)$up)^gamma,
        error = function(e) NULL)
      if (!is.null(result)) return(result)
    }
    if (is.null(result)) {
      for (gamma in c(1, 1.8, 2.2, 2.6)) {
        result <- tryCatch(
          (thr_twocorner(x^(1/gamma), sigma = 2,  method = "prominence",
                         slope_reduction = FALSE)$up)^gamma,
          error = function(e) NULL)
        if (!is.null(result)) return(result)
      }
    }
    for (gamma in c(1, 1.8, 2.2, 2.6)) {
      result <- tryCatch(
        (thr_twocorner(x^(1/gamma), sigma = 3, method = "prominence",
                       slope_reduction = TRUE)$up)^gamma,
        error = function(e) NULL)
      if (!is.null(result)) return(result)
    }
    if (is.null(result)) {
      for (gamma in c(1, 1.8, 2.2, 2.6)) {
        result <- tryCatch(
          (thr_twocorner(x^(1/gamma), sigma = 3,  method = "prominence",
                         slope_reduction = FALSE)$up)^gamma,
          error = function(e) NULL)
        if (!is.null(result)) return(result)
      }
    }
    return(NA)
  }

  radians_denom <- c(
    2,   4,   6,   8,  10,  12,  14,  16,  18,  20,  22,  24,  26,  28,
    30,  32,  34,  36,  38,  40,  42,  44,  46,  48,  50,  52,  54,  56,
    58,  60,  62,  64,  66,  68,  70,  72,  74,  76,  78,  80,  82,  84,
    86,  88,  90,  92,  94,  96,  98, 100, 102, 104, 106, 108, 110, 112,
    114, 116, 118, 120, 122, 124, 126, 128, 130, 132, 134, 136, 138, 140,
    142, 144, 146, 148, 150, 152, 154, 156, 158, 160, 162, 164, 166, 168,
    170, 172, 174, 176, 178, 180, 182, 184, 186, 188, 190, 192, 194, 196,
    198, 200, 202, 204, 206, 208, 210, 212, 214, 216, 218, 220, 222, 224,
    226, 228, 230, 232, 234, 236, 238, 240, 242, 244, 246, 248, 250, 252,
    254, 256, 258, 260, 262, 264, 266, 268, 270, 272, 274, 276, 278, 280,
    282, 284, 286, 288, 290, 292, 294, 296, 298, 300, 302, 304, 306, 308,
    310, 312, 314, 316, 318, 320, 324, 326, 328, 330, 332, 334, 336, 340,
    342, 344, 346, 348, 352, 354, 356, 358, 360
  )
  .rebuild_rasters <- function(m_fov) {
    rows <- row_vals[m_fov] - min(row_vals[m_fov], na.rm = TRUE) + 1
    cols <- col_vals[m_fov] - min(col_vals[m_fov], na.rm = TRUE) + 1
    r <- terra::rast(nrows = max(rows, na.rm = TRUE),
                     ncols = max(cols, na.rm = TRUE))
    terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
    # https://spatialreference.org/ref/sr-org/7589/
    terra::crs(r) <- "epsg:7589"
    # r[terra::cellFromRowCol(r, rows, cols)] <- r_vals[m_fov, "Blue"]

    r <- lapply(1:ncol(r_vals), function(i) {
      r[terra::cellFromRowCol(r, rows, cols)] <- r_vals[m_fov, i]
      r
    })
    r <- rast(r)
    names(r) <- names_of_r

    m <- !is.na(r[[1]])

    z <- a <- rast(m)
    z[m] <- z_vals[m_fov] %>% .radian2degree()
    names(z) <- "Zenith image"
    a[m] <- a_vals[m_fov] %>% .radian2degree()
    names(a) <- "Azimuth image"

    com <- complementary_gradients(r)
    r <- r$Blue
    mem <- max(com$yellow_blue, com$red_cyan)
    mem <- mean(normalize_minmax(mem), normalize_minmax(r^(1/2.2)))

    valid_angle_width <- 180/radians_denom
    diff <- abs(attr(m_fov, "fov")/15 - valid_angle_width)
    valid_angle_width <- valid_angle_width[order(diff)]
    j <- 0
    repeat {
      j <- j + 1
      g <- tryCatch(sky_grid_segmentation(z, a, valid_angle_width[j]),
                    error = function(e) NULL)
      if (!is.null(g)) break
      if (j == length(length(valid_angle_width))) break
    }
    if (is.null(g)) {
      g <- chessboard(r, round((min(ncol(r), nrow(r)) / 15)))
    }
    g[!m] <- 0

    thr <- tryCatch(thr_isodata(mem[m]), error = function(e) return(NA))
    bin <- binarize_with_thr(mem, thr)

    # like Macfarlane2011 eq1
    DN_uc <- thr_isodata(r[m])
    DN_lc <- min(r[m])
    mn <- DN_lc + (DN_uc - DN_lc) * 0.25

    sky_points <- extract_sky_points(mem, bin, g, 3)
    rr <- extract_rr(r, z, a, sky_points)
    list(mn = mn, z = z, a = a, m = m, rr = rr)
  }


# Extract vectors ---------------------------------------------------------

  row_vals <- terra::rowFromCell(r, terra::cells(r))
  col_vals <- terra::colFromCell(r, terra::cells(r))
  r_vals <- terra::values(r)
  z_vals <- terra::values(z) %>% .degree2radian()
  a_vals <- terra::values(a) %>% .degree2radian()
  m_vals <- terra::values(m)
  names_of_r <- names(r)

  sky_points <- sky_grid_centers(z, a, spacing / 3)
  sky_points <- rem_nearby_points(sky_points, NULL, z, a, spacing,
                                  space = "spherical")

  i <- extract_dn(m, sky_points, use_window = FALSE)[,3]
  sky_points <- sky_points[i, ]
  rr <- extract_rr(z, z, a, sky_points, NULL, FALSE)
  n_directions <- nrow(sky_points)

  acc <- NULL
  half_fovs <- .degree2radian(fov / 2)

  .thr_isodata <- function(m_fov) {
    v <- r_vals[m_fov]
    thr <- tryCatch(return(thr_isodata(v)), error = function(e) NULL)
    if (is.null(thr)) return(NA)
    thr
  }
  .detect_bg_dn <- function(m_fov) {
    v <- r_vals[m_fov]
    thr <- tryCatch(.thr_twocorner_up(v), error = function(e) NULL)
    if (is.null(thr)) return(NA)
    thr
  }
  .fit_coneshaped_model <- function(m_fov) {
    l <- .rebuild_rasters(m_fov)
    z <- l$z
    a <- l$a
    m <- l$m
    rr <- l$rr
    mn <- l$mn
    repeat {
      model <- tryCatch(fit_coneshaped_model(rr$sky_points),
                        warning = function(w) NA,
                        error = function(e) NA)
      if (any(is.na(model))) break
      sky_cs <- model$fun(z, a) * rr$zenith_dn
      if (all(sky_cs[m][,] >= mn, na.rm = TRUE)) break
      rr$sky_points <-  rr$sky_points[-which.min(rr$sky_points$dn),]
    }
    if (any(is.na(model))) return(NA)
    return(sky_cs[m][,])
  }
  .fit_trend_surface_np1 <- function(m_fov) {
    l <- .rebuild_rasters(m_fov)
    m <- l$m
    rr <- l$rr
    mn <- l$mn
    repeat {
      sky_s <- tryCatch(fit_trend_surface(rr$sky_points, m,
                                          np = 1, col_id = "dn",
                                          extrapolate = TRUE),
                        error = function(e) NA)
      if (any(is.na(sky_s))) break
      if (!is(sky_s$raster, "SpatRaster")) break
      if (all(sky_s$raster[m][,] >= mn, na.rm = TRUE)) break
      rr$sky_points <-  rr$sky_points[-which.min(rr$sky_points$dn),]
    }
    if (any(is.na(sky_s))) return(NA)
    if (!is(sky_s$raster, "SpatRaster")) return(NA)
    sky_s$raster[m][,]
  }
  .fit_trend_surface_np6 <- function(m_fov) {
    l <- .rebuild_rasters(m_fov)
    m <- l$m
    rr <- l$rr
    mn <- l$mn
    sky_s <- tryCatch(fit_trend_surface(rr$sky_points, m,
                                        np = 6, col_id = "dn",
                                        extrapolate = FALSE),
                      error = function(e) NA)
    if (any(is.na(sky_s))) return(NA)
    if (!is(sky_s$raster, "SpatRaster")) return(NA)
    if (any(sky_s$raster[m][,] <= mn, na.rm = TRUE)) return(NA)
    sky_s$raster[m][,]
  }

  .extract_fov_n_compute_fun <- function(i) {
    chi <- calc_spherical_distance(z_vals,
                                   a_vals,
                                   as.numeric(rr$sky_points[i, "z"]) %>%
                                     .degree2radian(),
                                   as.numeric(rr$sky_points[i, "a"]) %>%
                                     .degree2radian())
    u <- 0
    repeat {
      u <- u + 1
      if (is.na(half_fovs[u])) break
      half_fovs[u]
      m_fov <- chi <= half_fovs[u]
      m_fov[!m_vals] <- FALSE
      attr(m_fov, "fov") <- .radian2degree(half_fovs[u] * 2)
      delta_star <- tryCatch(.fun(m_fov), error = function(e) NA)
      # delta_star <- .fun(m_fov)
      if (any(!is.na(delta_star))) break
    }
    if (all(is.na(delta_star))) return(NULL)
    idx <- which(m_fov, arr.ind = FALSE)
    ds <- data.frame(cell = idx, value = delta_star)
    ds[!is.na(ds$value),]
  }

  if (is.null(fun)) {
    .fun <- switch(method,
                   thr_isodata = .thr_isodata,
                   detect_bg_dn = .detect_bg_dn,
                   fit_coneshaped_model = .fit_coneshaped_model,
                   fit_trend_surface_np1 = .fit_trend_surface_np1,
                   fit_trend_surface_np6 = .fit_trend_surface_np6)
  } else {
    .fun <- function(m_fov) {
      rows <- row_vals[m_fov] - min(row_vals[m_fov], na.rm = TRUE) + 1
      cols <- col_vals[m_fov] - min(col_vals[m_fov], na.rm = TRUE) + 1
      r <- terra::rast(nrows = max(rows, na.rm = TRUE),
                       ncols = max(cols, na.rm = TRUE))
      terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
      # https://spatialreference.org/ref/sr-org/7589/
      terra::crs(r) <- "epsg:7589"

      r <- lapply(1:ncol(r_vals), function(i) {
        r[terra::cellFromRowCol(r, rows, cols)] <- r_vals[m_fov, i]
        r
      })
      r <- rast(r)
      names(r) <- names_of_r

      m <- !is.na(r[[1]])

      z <- a <- rast(m)
      z[m] <- z_vals[m_fov] %>% .radian2degree()
      a[m] <- a_vals[m_fov] %>% .radian2degree()

      r <- fun(r, z, a, m)
      r[m][,]
    }
  }

  if (parallel) {
    cores <- .cores()
    acc <- .with_cluster(cores, {
      i_chunks <- split(seq_len(n_directions), 1:cores) %>% suppressWarnings()

      # Only to avoid note from check, code is OK without this line.
      j <- NA

      foreach::foreach(j = 1:cores,
                       .combine = rbind,
                       .packages = c("terra")) %dopar% {
                           do.call(rbind, lapply(i_chunks[[j]],
                                                .extract_fov_n_compute_fun))
                        }
    })
  } else {
    acc <- do.call(rbind, lapply(seq_len(n_directions),
                                 .extract_fov_n_compute_fun))
  }

  if (is.null(acc)) {
    warning("Method failed. Revise your input or try another.")
    return(NULL)
  }

  if (parallel) {
    .with_cluster(cores, {
      cell_list <- split(acc$cell, acc$cell) %>% suppressWarnings()
      value_list <- split(acc$value, acc$cell) %>% suppressWarnings()
      cell_list_chunks <- split(cell_list, 1:cores) %>% suppressWarnings()
      value_list_chunks <- split(value_list, 1:cores) %>% suppressWarnings()

      # Only to avoid note from check, code is OK without this line.
      j <- NA

      results <- foreach::foreach(j = 1:cores, .combine = rbind) %dopar% {
        acc2 <- data.frame(cell = cell_list_chunks[[j]] %>% unlist,
                           value = value_list_chunks[[j]] %>% unlist)
        med <- tapply(acc2$value, acc2$cell, median)
        mad <- tapply(acc2$value, acc2$cell, mad)
        count <- tapply(acc2$value, acc2$cell, length)

        dev <- abs(acc2$value - med[as.character(acc2$cell)]) / mad[as.character(acc2$cell)]
        acc2 <- acc2[is.na(dev) | dev <= laxity, ]

        final_vals <- tapply(acc2$value, acc2$cell, mean)

        data.frame(cell = as.integer(names(final_vals)),
                   value = final_vals,
                   n = count[names(final_vals)])
      }

      sky_dn <- n <- rast(m)
      sky_dn[results$cell] <- results$value
      n[results$cell] <- results$n
    })

  } else {
    med <- tapply(acc$value, acc$cell, median)
    mad <- tapply(acc$value, acc$cell, mad)
    count <- tapply(acc$value, acc$cell, length)

    dev <- abs(acc$value - med[as.character(acc$cell)]) / mad[as.character(acc$cell)]
    acc <- acc[is.na(dev) | dev <= laxity, ]

    final_vals <- tapply(acc$value, acc$cell, mean)

    acc_final <- data.frame(cell = as.integer(names(final_vals)),
                            value = final_vals)

    sky_dn <- n <- rast(m)
    sky_dn[acc_final$cell] <-  acc_final$value
    n[row.names(count) %>% as.integer] <- data.frame(count)
  }
  sky <- c(sky_dn, n)
  names(sky) <- c("dn", "n")
  return(sky)
}
