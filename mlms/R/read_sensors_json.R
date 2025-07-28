#' Read Sensor Data in JSON Format
#'
#' @description Read electronic sensor data in a JSON format.
#'
#' @param path 'character' string.
#'   Path to the JSON file to read.
#'
#' @return A list of data frame components.
#'   See [`sensors`] and [`calibrations`] datasets for example output.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @examples
#' l <- system.file("extdata/ex-sensors.json", package = "mlms") |>
#'   read_sensors_json()
#' str(l, max.level = 1)

read_sensors_json <- function(path) {

  checkmate::assert_string(path)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  checkmate::assert_file_exists(path, access = "r", extension = "json")

  message("Reading JSON file:\n ", path)
  json <- jsonlite::read_json(path = path) |>
    null_to_na()
  names(json) <- vapply(json,
    FUN = function(x) x$sensor_id,
    FUN.VALUE = character(1)
  )
  json <- json[order(names(json))]

  l <- lapply(json,
    FUN = function(x) {
      is <- names(x) %in% "calibrations"
      as.data.frame(x[!is])
    }
  )
  d <- do.call(rbind, args = l)
  rownames(d) <- NULL
  sensors <- d

  l <- lapply(json,
    FUN = function(x) {
      d <- do.call(rbind,
        lapply(x$calibrations,
          FUN = function(x) {
            is <- names(x) %in% "records"
            as.data.frame(x[!is])
          }
        )
      )
      d <- data.frame(sensor_id = x$sensor_id, d)
      d$cal_dt <- as.Date(d$cal_dt)
      d$ts_dt <- as.Date(d$ts_dt)
      models <- lapply(x$calibrations,
        FUN = function(cal) {
          d <- do.call(rbind.data.frame, cal$records)
          sw <- calc_specific_weight(cal$ref_temp_va) # specific weight in lb/ft^3
          d$head_error_va <- d$press_error_va * 144 / sw # measured error from psi to ft
          d$head_measure_va <- d$press_true_va + d$press_error_va # measured pressure head in ft
          stats::lm(
            formula = head_error_va ~ head_measure_va + I(head_measure_va^2) + I(head_measure_va^3),
            data = d
          )
        }
      )
      d$r2 <- vapply(models,
        FUN = function(x) {
          summary(x)$r.squared |>
            as.numeric()
        },
        FUN.VALUE = numeric(1)
      )
      d$p_value <- vapply(models,
        FUN = get_p_value,
        FUN.VALUE = numeric(1)
      )
      d
    }
  )
  d <- do.call(rbind, args = l)
  rownames(d) <- NULL

  d$cal_tp <- as.factor(d$cal_tp)
  d$lab_standard <- as.factor(d$lab_standard)

  calibrations <- d

  list(sensors = sensors, calibrations = calibrations)
}
