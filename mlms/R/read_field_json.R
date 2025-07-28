#' Read Field Data in JSON Format
#'
#' @description Read Multilevel Monitoring System (MLMS) field data in JSON format.
#'
#' @param paths 'character' vector.
#'   Paths to the JSON files to read.
#' @param wells 'data.frame' table.
#'   MLMS well data, see [`wells`] dataset for data structure.
#' @param ports 'data.frame' table.
#'   MLMS measurement port data, see [`ports`] dataset for data structure.
#' @param tz 'character' string.
#'   Time zone specification.
#'   Defaults to America/Denver.
#'
#' @return A list of data frame components.
#'   See [`visits`] and [`heads`] datasets for example output.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso [`write_field_json`] function for writing field data to file in JSON format.
#'
#' @export
#'
#' @examples
#' l <- system.file("extdata/ex-field.json", package = "mlms") |>
#'   read_field_json()
#' str(l, max.level = 1)

read_field_json <- function(paths,
                            wells = mlms::wells,
                            ports = mlms::ports,
                            tz = "America/Denver") {

  # check arguments
  checkmate::assert_character(paths, any.missing = FALSE, min.len = 1)
  checkmate::assert_class(wells, classes = c("sf", "data.frame"))
  checkmate::assert_class(ports, classes = "data.frame")
  checkmate::assert_choice(tz, choices = OlsonNames())

  if (checkmate::test_directory_exists(paths, access = "r")) {
    paths <- list.files(path = paths, pattern = "\\.json$", full.names = TRUE)
  }
  paths <- normalizePath(paths, winslash = "/")

  jsons <- lapply(paths,
    FUN = function(path) {
      message("Reading JSON file:\n  ", path)
      jsonlite::read_json(path = path) |>
        null_to_na()
    }
  )

  l <- lapply(jsons,
    FUN = function(json) {
      do.call(rbind,
        lapply(json,
          FUN = function(x) {
            is <- names(x) %in% "profile"
            d <- as.data.frame(x[!is])
            fmt <- x$date_format_cd
            d$stime_dt <- as.POSIXct(d$stime_dt, tz = tz, format = fmt)
            d$etime_dt <- as.POSIXct(d$etime_dt, tz = tz, format = fmt)
            d$operators <- stringi::stri_trans_toupper(d$operators)
            opts <- stringi::stri_opts_brkiter(type = "sentence")
            d$weather <- stringi::stri_trans_totitle(d$weather, opts_brkiter = opts)
            d$comment_tx <- stringi::stri_trans_totitle(d$comment_tx, opts_brkiter = opts)
            d
          }
        )
      )
    }
  )
  visits <- do.call(rbind, args = l)

  cols <- c(
    "site_nm",
    "stime_dt",
    "etime_dt",
    "baro_id",
    "baro_start_va",
    "baro_end_va",
    "sensor_id",
    "press_start_va",
    "press_end_va",
    "temp_start_va",
    "temp_end_va",
    "operators",
    "sheet_version_tx",
    "weather",
    "comment_tx"
  )
  idxs <- order(visits$stime_dt, decreasing = TRUE)
  visits <- visits[idxs, cols]
  rownames(visits) <- NULL

  l <- lapply(jsons,
    FUN = function(json) {
      do.call(rbind,
        lapply(json,
          FUN = function(x) {
            is <- wells$site_nm %in% x$site_nm
            if (!any(is)) {
              stop("Unrecognized site name: ", x$site_nm)
            }
            alt_va <- wells$alt_va[is]

            d <- do.call(rbind.data.frame, x$profile)

            d$replicate_fl <- duplicated(d$port_nu)

            fmt <- x$date_format_cd
            d$press_dt <- as.POSIXct(d$press_dt, tz = tz, format = fmt)
            d$stime_dt <- as.POSIXct(x$stime_dt, tz = tz, format = fmt)

            ports <- ports[ports$site_nm == x$site_nm, ]
            d <- merge(d, ports, by = "port_nu")

            d$baro_va <- round_usgs(d$baro_va, digits = 3)
            d$press_head_va <- calc_press_head(d$press_va, d$baro_va, d$temp_va) |> round_usgs(digits = 2)

            idxs <- match(d$site_no, ports$site_no) |> stats::na.omit()
            d$tp_depth_va <- ports$tp_depth_va[idxs]

            d$total_head_va <- (alt_va - d$tp_depth_va + d$press_head_va) |> round_usgs(digits = 1)
            d$press_in_diff_va <- abs(d$press_in_1_va - d$press_in_2_va) |> round_usgs(digits = 2)

            d
          }
        )
      )
    }
  )
  heads <- do.call(rbind, args = l)

  cols <- c(
    "site_nm",
    "port_nu",
    "site_no",
    "stime_dt",
    "press_dt",
    "temp_va",
    "baro_va",
    "press_va",
    "press_head_va",
    "total_head_va",
    "press_in_1_va",
    "press_in_2_va",
    "press_in_diff_va",
    "replicate_fl",
    "comment_tx"
  )
  idxs <- order(heads$press_dt, decreasing = TRUE)
  heads <- heads[idxs, cols]
  rownames(heads) <- NULL

  list(visits = visits, heads = heads)
}
