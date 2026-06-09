.was_normalized <- function(r, name = deparse(substitute(r))) {
  if (.get_max(r) > 1)
    warning(sprintf("Check if '%s' was correctly normalized.", name))
}

.this_requires_EBImage <- function() {
  if (!requireNamespace("EBImage", quietly = TRUE)) {
    stop(paste("Package \"EBImage\" needed for this function to work.",
               "Please install it. Find instructions here:",
               "https://bioconductor.org/packages/release/bioc/html/EBImage.html"
    ),
    call. = FALSE)
  }
}

.is_sky_grid <- function(g) {
  if (!inherits(g, "SpatRaster")) return(FALSE)

  angle_width <- attr(g, "angle_width")
  ring_flag   <- attr(g, "first_ring_different")

  if (!is.numeric(angle_width) || length(angle_width) != 1 ||
      !is.finite(angle_width)) return(FALSE)
  if (!is.logical(ring_flag) || length(ring_flag) != 1 ||
      anyNA(ring_flag)) return(FALSE)

  nm <- names(g)
  if (is.null(nm)) return(FALSE)

  all(nm == paste0("Sky grid, ", angle_width, " degrees"))
}

.assert_sky_grid <- function(g, name = deparse(substitute(g))) {
  if (!.is_sky_grid(g)) {
    stop(sprintf("`%s` must be the output of `sky_grid_segmentation()`.", name),
         call. = FALSE)
  }
  invisible(TRUE)
}

.check_vector <- function(x,
                         type = c("numeric", "character", "logical",
                                  "integerish", "even_integerish"),
                         length = NULL,
                         allow_null = FALSE,
                         sign = c("any", "positive", "nonnegative",
                                  "negative", "nonpositive"),
                         name = deparse(substitute(x))
                         ) {
  .is_integerish <- function(v) {
    if (is.list(v)) return(FALSE)
    if (!is.vector(v) || !is.numeric(v)) return(FALSE)
    if (any(is.infinite(v))) return(FALSE)
    all(round(v) == v)
  }
  .is_even <- function(v) {
    if (!is.vector(v) || !is.numeric(v)) return(FALSE)
    all(round(v/2) == v/2)
  }

  if (is.null(x)) {
    if (allow_null) return(invisible(TRUE))
    stop(sprintf("`%s` cannot be `NULL`.", name), call. = FALSE)
  }
  if (is.list(x)) {
    stop(sprintf("`%s` must be an atomic vector, not a list.", name),
         call. = FALSE)
  }

  type <- match.arg(type)
  sign <- match.arg(sign)

  ok_type <- switch(
    type,
    numeric         = is.vector(x) && is.numeric(x),
    character       = is.vector(x) && is.character(x),
    logical         = is.vector(x) && is.logical(x),
    integerish      = .is_integerish(x),
    even_integerish = .is_integerish(x) && .is_even(x)
  )

  # Mensaje base por tipo
  type_msg <- switch(
    type,
    numeric         = "a numeric vector",
    character       = "a character vector",
    logical         = "a logical vector",
    integerish      = "an integer-like numeric vector",
    even_integerish = "an even integer-like numeric vector"
  )

  # Frase de longitud
  len_msg <- if (!is.null(length)) sprintf(" of length %d", length) else ""

  # Frase de signo
  sign_msg <- switch(
    sign,
    any          = "",
    positive     = " with positive values only",
    nonnegative  = " with non-negative values only",
    negative     = " with negative values only",
    nonpositive  = " with non-positive values only"
  )

  # Error de tipo
  if (!ok_type) {
    stop(sprintf("`%s` must be %s%s%s.", name, type_msg, len_msg, sign_msg),
         call. = FALSE)
  }

  # Error de longitud
  if (!is.null(length) && length(x) != length) {
    stop(sprintf("`%s` must be %s%s%s.", name, type_msg, len_msg, sign_msg),
         call. = FALSE)
  }

  # Chequeo de signo (solo tipos numéricos o integerish)
  if (sign != "any" && type %in% c("numeric","integerish","even_integerish")) {
    vals <- if (is.numeric(x)) x else as.numeric(x)
    ok_sign <- switch(
      sign,
      positive     = all(vals > 0),
      nonnegative  = all(vals >= 0),
      negative     = all(vals < 0),
      nonpositive  = all(vals <= 0)
    )
    if (!isTRUE(ok_sign)) {
      stop(sprintf("`%s` must be %s%s%s.", name, type_msg, len_msg, sign_msg),
           call. = FALSE)
    }
  }

  invisible(TRUE)
}



.assert_spatraster <- function(x, name = deparse(substitute(x))) {
  if (!inherits(x, "SpatRaster"))
    stop(sprintf("`%s` must be a SpatRaster class.", name), call. = FALSE)
  invisible(TRUE)
}

.assert_single_layer <- function(x, name = deparse(substitute(x))) {
  .assert_spatraster(x, name)
  if (terra::nlyr(x) != 1)
    stop(sprintf("`%s` must have a single layer.", name), call. = FALSE)
  invisible(TRUE)
}

.assert_rgb3 <- function(x, name = deparse(substitute(x))) {
  .assert_spatraster(x, name)
  if (terra::nlyr(x) != 3)
    stop(sprintf("`%s` must have three layers named `Red`, `Green`, `Blue`.", name), call. = FALSE)
  nm <- names(x)
  if (!identical(nm, c("Red","Green","Blue")))
    stop(sprintf("`%s` layers must be named `Red`, `Green`, `Blue`.", name), call. = FALSE)
  invisible(TRUE)
}

.assert_same_geom <- function(x, y,
                              x_name = deparse(substitute(x)),
                              y_name = deparse(substitute(y))) {
  if (!terra::compareGeom(x, y, stopOnError = FALSE, messages = FALSE))
    stop(sprintf("`%s` and `%s` must have the same geometry.", x_name, y_name), call. = FALSE)
  invisible(TRUE)
}

.assert_logical_mask <- function(m, name = deparse(substitute(m))) {
  .assert_single_layer(m, name)
  if (!is.logical(m[]))
    stop(sprintf("`%s` must be logical.", name), call. = FALSE)
  if (anyNA(m[]))
    stop(sprintf("`%s` must be NA-free.", name), call. = FALSE)
  invisible(TRUE)
}

.check_r_z_a_m <- function(r = NULL, z = NULL, a = NULL, m = NULL,
                           r_type = c("single", "any", "rgb"),
                           r_name = deparse(substitute(r))) {
  # --- helpers ---
  .assert_spatraster <- function(x, name) {
    if (!inherits(x, "SpatRaster"))
      stop(sprintf("`%s` must be a SpatRaster class.", name), call. = FALSE)
  }
  .assert_single_layer <- function(x, name) {
    .assert_spatraster(x, name)
    if (terra::nlyr(x) != 1)
      stop(sprintf("`%s` must have a single layer.", name), call. = FALSE)
  }
  .assert_rgb3 <- function(x, name) {
    .assert_spatraster(x, name)
    if (terra::nlyr(x) != 3)
      stop(sprintf("`%s` must have three layers named `Red`, `Green`, `Blue`.", name), call. = FALSE)
    if (!identical(names(x), c("Red","Green","Blue")))
      stop(sprintf("`%s` layers must be named `Red`, `Green`, `Blue`.", name), call. = FALSE)
  }
  .assert_same_geom <- function(x, y, xn, yn) {
    if (!terra::compareGeom(x, y, stopOnError = FALSE, messages = FALSE))
      stop(sprintf("`%s` and `%s` must have the same geometry.", xn, yn), call. = FALSE)
  }
  .assert_logical_mask <- function(x, name) {
    .assert_single_layer(x, name)
    v <- x[]
    if (!is.logical(v))
      stop(sprintf("`%s` must be logical.", name), call. = FALSE)
    if (anyNA(v))
      stop(sprintf("`%s` must be NA-free.", name), call. = FALSE)
  }

  # --- z / a checks ---
  if (!is.null(z)) {
    .assert_single_layer(z, "z")
    if (!identical(names(z), "Zenith image"))
      stop("`z` must be the output of `zenith_image()`.", call. = FALSE)
  }
  if (!is.null(a)) {
    .assert_single_layer(a, "a")
    if (!identical(names(a), "Azimuth image"))
      stop("`a` must be the output of `azimuth_image()`.", call. = FALSE)
  }
  if (!is.null(z) && !is.null(a)) .assert_same_geom(z, a, "z", "a")

  # --- r checks (optional) ---
  if (!is.null(r)) {
    r_type <- match.arg(r_type)
    .assert_spatraster(r, r_name)

    if (r_type == "single") {
      .assert_single_layer(r, r_name)
    } else if (r_type == "rgb") {
      .assert_rgb3(r, r_name)
    } else { # any
      if (terra::nlyr(r) == 1) {
        # ok
      } else {
        .assert_rgb3(r, r_name)
      }
    }

    .assert_same_geom(r[[1]], z, r_name, "z")
    if (!is.null(a)) .assert_same_geom(r[[1]], a, r_name, "a")

    if (!is.null(m)) {
      .assert_logical_mask(m, "m")
      .assert_same_geom(r[[1]], m, r_name, "m")
    }
  } else {
    if (!is.null(m) && !is.null(z)) {
      .assert_logical_mask(m, "m")
      .assert_same_geom(m, z, "m", "z")
    }
    if (!is.null(m) && !is.null(a)) {
      .assert_logical_mask(m, "m")
      .assert_same_geom(m, a, "m", "a")
    }
  }

  invisible(TRUE)
}

.check_sky_points <- function(sky_points) {
  if (!is.data.frame(sky_points)) {
    stop("`sky_points` must be a data frame with columns 'row' and 'col'.")
  }
  if (ncol(sky_points) != 2 || !all(c("row", "col") %in% names(sky_points))) {
    stop("`sky_points` must have two columns named 'row' and 'col'.")
  }
}

.assert_file_exists <- function(path, name = deparse(substitute(path))) {
  if (is.null(path))
    stop(sprintf("`%s` cannot be `NULL`.", name), call. = FALSE)
  if (!is.character(path) || length(path) == 0)
    stop(sprintf("`%s` must be a character vector of file path(s).", name), call. = FALSE)
  if (anyNA(path))
    stop(sprintf("`%s` cannot contain `NA`.", name), call. = FALSE)

  ok <- file.exists(path)
  if (!all(ok)) {
    missing <- path[!ok]
    if (length(missing) == 1) {
      stop(sprintf("File '%s' does not exist.", missing), call. = FALSE)
    } else {
      stop(sprintf("These files do not exist: %s",
                   paste(sprintf("'%s'", missing), collapse = ", ")),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

.assert_choice <- function(x, allowed,
                           name = deparse(substitute(x)),
                           allow_null = FALSE,
                           multiple = FALSE,
                           case_sensitive = TRUE) {
  # sanity on allowed
  if (is.null(allowed) || !is.character(allowed) || length(allowed) < 1 || anyNA(allowed))
    stop("`allowed` must be a non-empty character vector without `NA`.", call. = FALSE)

  q <- function(v) sprintf('"%s"', v)
  collapse_or <- function(v) {
    vq <- q(v)
    n <- length(vq)
    if (n == 1) vq
    else if (n == 2) paste(vq, collapse = " or ")
    else paste0(paste(vq[-n], collapse = ", "), ", or ", vq[n])
  }

  if (is.null(x)) {
    if (allow_null) return(invisible(TRUE))
    stop(sprintf("`%s` cannot be `NULL`.", name), call. = FALSE)
  }

  if (!is.character(x))
    stop(sprintf("`%s` must be a character vector.", name), call. = FALSE)
  if (anyNA(x))
    stop(sprintf("`%s` cannot contain `NA`.", name), call. = FALSE)

  if (!multiple && length(x) != 1)
    stop(sprintf("`%s` must be a character vector of length one.", name), call. = FALSE)

  # case handling
  if (!case_sensitive) {
    x <- tolower(x)
    allowed <- tolower(allowed)
  }

  ok <- if (multiple) all(x %in% allowed) else x %in% allowed

  if (!ok) {
    if (multiple) {
      stop(sprintf("`%s` must contain only allowed values: %s.",
                   name, paste(q(allowed), collapse = ", ")), call. = FALSE)
    } else {
      msg <- if (length(allowed) == 2)
        sprintf("either %s", collapse_or(allowed))
      else
        sprintf("one of: %s", paste(q(allowed), collapse = ", "))
      stop(sprintf("`%s` must be %s.", name, msg), call. = FALSE)
    }
  }

  invisible(TRUE)
}

