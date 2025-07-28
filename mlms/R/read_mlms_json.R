#' Read MLMS Data in JSON Format
#'
#' @description Read Multilevel Monitoring System (MLMS) data in JSON format.
#'   Requires that the \pkg{inldata} package is available.
#'
#' @param path 'character' string.
#'   Path to the JSON file to read.
#'
#' @return A list of data frame components.
#'   See [`wells`], [`zones`], and [`ports`] datasets for example output.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @examples
#' l <- system.file("extdata/ex-mlms.json", package = "mlms") |>
#'   read_mlms_json()
#' str(l, max.level = 1)

read_mlms_json <- function(path) {

  # check packages
  if (!requireNamespace("inldata", quietly = TRUE)) {
    stop("Reading MLMS data requires the 'inldata' package.", call. = FALSE)
  }

  # check arguments
  checkmate::assert_string(path)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  checkmate::assert_file_exists(path, access = "r", extension = "json")

  # read raw data
  message("Reading JSON file:\n  ", path)
  json <- jsonlite::read_json(path = path) |>
    null_to_na()
  names(json) <- vapply(json,
    FUN = function(x) x$site_nm,
    FUN.VALUE = character(1)
  )
  json <- json[order(names(json))]

  # parse well data

  d <- inldata::sites
  d <- d[!duplicated(d$site_nm), ]

  site_nm <- vapply(json, FUN = function(x) x$site_nm, FUN.VALUE = character(1))
  idxs <- match(site_nm, d$site_nm)
  d <- d[idxs, ]

  d$coord_acy_va <- as.numeric(d$coord_acy_va)

  d$etape_cf_va <- vapply(json, FUN = function(x) x$etape_cf_va, FUN.VALUE = numeric(1))
  d$deviation_cf_va <- vapply(json, FUN = function(x) x$deviation_cf_va, FUN.VALUE = numeric(1))
  d$stickup_va <- vapply(json, FUN = function(x) x$stickup_va, FUN.VALUE = numeric(1))
  d$mp_b_va <- vapply(json, FUN = function(x) x$mp_b_va, FUN.VALUE = numeric(1))
  d$mp_c_va <- vapply(json, FUN = function(x) x$mp_c_va, FUN.VALUE = numeric(1))
  d$hole_depth_va <- vapply(json, FUN = function(x) x$hole_depth_va, FUN.VALUE = numeric(1))
  d$well_depth_va <- vapply(json, FUN = function(x) x$well_depth_va, FUN.VALUE = numeric(1))
  d$install_dt <- vapply(json, FUN = function(x) x$install_dt, FUN.VALUE = character(1)) |> as.Date()
  d$construction_dt <- vapply(json, FUN = function(x) x$construction_dt, FUN.VALUE = character(1)) |> as.Date()
  d$system_tp <- vapply(json, FUN = function(x) x$system_tp, FUN.VALUE = character(1)) |> as.factor()

  cols <- c(
    "site_nm",
    "coord_acy_va",
    "alt_va",
    "alt_acy_va",
    "etape_cf_va",
    "deviation_cf_va",
    "stickup_va",
    "mp_b_va",
    "mp_c_va",
    "hole_depth_va",
    "well_depth_va",
    "construction_dt",
    "install_dt",
    "system_tp"
  )
  d <- d[, cols]
  rownames(d) <- NULL
  wells <- d

  # parse port data

  l <- lapply(json,
    FUN = function(x) {
      d <- do.call(rbind.data.frame, x$ports)
      idx <- match(x$site_nm, wells$site_nm)
      bc_alt <- wells$alt_va[idx]
      mp_c <- wells$mp_c_va[idx]
      wl_depth <- d$wl_depth_va - wells$etape_cf_va[idx] - wells$deviation_cf_va[idx] - wells$stickup_va[idx]
      press_head <- calc_press_head(d$press_compl_va, d$baro_compl_va, d$temp_compl_va)
      d$tp_depth_va <- (wl_depth + press_head) |> round_usgs(digits = 2)
      d$port_depth_va <- (d$tp_depth_va - mp_c) |> round_usgs(digits = 2)
      d$port_alt_va <- (bc_alt - d$port_depth_va) |> round_usgs(digits = 2)
      d$site_nm <- x$site_nm
      cols <- c(
        "site_nm",
        "port_nu",
        "site_no",
        "mp_a_va",
        "wl_depth_va",
        "baro_compl_va",
        "temp_compl_va",
        "press_compl_va",
        "tp_depth_va",
        "port_depth_va",
        "port_alt_va"
      )
      d <- d[, cols]
    }
  )
  d <- do.call(rbind, args = l)

  idxs <- order(d$site_nm, d$port_nu, decreasing = c(FALSE, TRUE), method = "radix")
  d <- d[idxs, ]

  rownames(d) <- NULL
  ports <- d

  # calculate zone data

  is <- !is.na(ports$mp_a_va)
  d <- ports[is, c("site_nm", "port_nu", "tp_depth_va", "mp_a_va")]
  d$zone_nu <- unique(d$site_nm) |>
    lapply(
      FUN = function(x) {
        seq(from = sum(d$site_nm == x), to = 1)
      }
    ) |>
    unlist()

  d$zone_top_va <- (d$tp_depth_va - d$mp_a_va) |> round_usgs(digits = 2)

  d$zone_bot_va <- unique(d$site_nm) |>
    lapply(
      FUN = function(x) {
        is <- wells$site_nm == x
        hole_depth <- wells$hole_depth[is]
        mp_b <- wells$mp_b_va[is]
        is <- d$site_nm == x
        zone_bots <- d$zone_top_va[is] - mp_b
        c(zone_bots[-1], hole_depth)
      }
    ) |>
    unlist() |>
    round_usgs(digits = 2)

  d$zone_len_va <- (d$zone_bot_va - d$zone_top_va) |> round_usgs(digits = 2)

  bc_alts <- wells$alt_va[match(d$site_nm, wells$site_nm)]
  d$zone_top_alt_va <- (bc_alts - d$zone_top_va) |> round_usgs(digits = 2)
  d$zone_bot_alt_va <- (bc_alts - d$zone_bot_va) |> round_usgs(digits = 2)

  cols <- c(
    "site_nm",
    "zone_nu",
    "zone_top_va",
    "zone_bot_va",
    "zone_top_alt_va",
    "zone_bot_alt_va",
    "zone_len_va"
  )
  d <- d[, cols]
  rownames(d) <- NULL
  zones <- d

  # add zone count to wells
  tbl <- table(zones$site_nm)
  idxs <- names(tbl) |> match(wells$site_nm)
  wells$nzones <- as.integer(tbl)[idxs]

  # add port count to wells
  tbl <- table(ports$site_nm)
  idxs <- names(tbl) |> match(wells$site_nm)
  wells$nports <- as.integer(tbl)[idxs]

  # add zone number to ports
  ports$zone_nu <- apply(ports,
    MARGIN = 1,
    FUN = function(x) {
      d <- zones[zones$site_nm == x["site_nm"], ]
      z <- as.numeric(x["port_depth_va"])
      if (is.na(z)) return(NA_integer_)
      is <- d$zone_top_va < z & d$zone_bot_va >= z
      d$zone_nu[is]
    }
  )

  # add port count to zones
  zones$nports <- apply(zones,
    MARGIN = 1,
    FUN = function(x) {
      is <- ports$site_nm %in% x["site_nm"] &
        ports$zone_nu %in% as.integer(x["zone_nu"])
      sum(is)
    }
  )

  # return datasets
  list(wells = wells, zones = zones, ports = ports)
}
