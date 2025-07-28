#' Retrieve Discrete Sample Data
#'
#' @description Retrieve discrete sample water-quality data from the \pkg{inldata} package.
#'   See [`inldata::samples`] for a description of the source dataset.
#'   Requires that the \pkg{indata} package is available.
#'
#' @param site_no 'character' vector.
#'   USGS site identification number.
#'   The site numbers of measurement ports in the Multilevel Monitoring System (MLMS) wells by default.
#' @param pcode 'character' vector.
#'   USGS 5-digit parameter code.
#'   For a list of the parameter codes used by default, refer to the Details section.
#' @param ports 'data.frame' table.
#'   MLMS measurement port data, see [`ports`] dataset for data structure.
#' @param visits 'data.frame' table.
#'   Field visits, see [`visits`] dataset for data structure.
#'
#' @details The default parameters included are:
#'   \describe{
#'     \item{`00930`}{Sodium, water, filtered, in milligrams per liter (mg/L).}
#'     \item{`00940`}{Chloride, water, filtered, in mg/L.}
#'     \item{`00945`}{Sulfate, water, filtered, in mg/L.}
#'     \item{`00950`}{Fluoride, water, filtered, in mg/L.}
#'     \item{`01030`}{Chromium, water, filtered, in micrograms per liter.}
#'     \item{`00618`}{Nitrate, water, filtered, in mg/L as nitrogen.}
#'     \item{`07000`}{Tritium, water, unfiltered, in picocuries per liter (pCi/L).}
#'     \item{`13501`}{Strontium-90, water, unfiltered, in pCi/L.}
#'     \item{`22012`}{Plutonium-238, water, unfiltered, in pCi/L.}
#'     \item{`28401`}{Cesium-137, water, unfiltered, in pCi/L.}
#'     \item{`63018`}{Gross alpha radioactivity, water, unfiltered, Th-230 curve, in pCi/L.}
#'     \item{`80049`}{Gross beta radioactivity, water, unfiltered, Sr-90/Y-90 curve, in pCi/L.}
#'     \item{`00095`}{Specific conductance, water, unfiltered, in microsiemens per centimeter at 25 degrees Celsius.}
#'   }
#'
#' @return A data frame, see [`samples`] dataset for table structure.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @examples
#' \donttest{
#'   d <- get_samples(site_no = "433409112570503", pcode = "07000")
#' }

get_samples <- function(site_no = NULL,
                        pcode = NULL,
                        ports = mlms::ports,
                        visits = mlms::visits) {

  # check packages
  if (!requireNamespace("inldata", quietly = TRUE)) {
    stop("Retrieving discrete sample data requires the 'inldata' package", call. = FALSE)
  }

  # check arguments
  checkmate::assert_character(site_no,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_subset(site_no, choices = inldata::sites$site_no)
  checkmate::assert_subset(pcode, choices = inldata::parameters$pcode)
  checkmate::assert_data_frame(ports)
  checkmate::assert_data_frame(visits)

  # get site numbers
  if (is.null(site_no)) {
    site_no <- ports$site_no
  }

  # get sample data
  is <- inldata::samples$site_no %in% site_no
  d <- inldata::samples[is, ]

  # extract specified parameters
  if (!is.null(pcode)) {
    is <- d$pcode %in% pcode
    d <- d[is, ]
  }

  # assign port number
  idxs <- match(d$site_no, ports$site_no)
  d$port_nu <- ports$port_nu[idxs]

  # assign profile start time
  d$stime_dt <- rep(NA, nrow(d)) |> as.POSIXct()
  for (site_nm in unique(d$site_nm)) {
    is <- visits$site_nm == site_nm
    stimes <- visits$stime[is] |> unique() |> sort()
    is <- d$site_nm == site_nm & d$sample_dt >= stimes[1]
    idxs <- d$sample_dt[is] |> findInterval(vec = stimes)
    d$stime_dt[is] <- stimes[idxs]
  }
  durations <- difftime(d$sample_dt, d$stime_dt, units = "days")
  max_duration <- 10 # in days
  is <- is.finite(durations) & durations > max_duration
  d$stime_dt[is] <- NA

  # sort rows by collection time
  idxs <- order(d$sample_dt, decreasing = TRUE)
  d <- d[idxs, ]

  # subset columns
  cols <- c(
    "site_nm",
    "port_nu",
    "site_no",
    "stime_dt",
    "sample_dt",
    "parm_short_nm",
    "unit_cd",
    "pcode",
    "remark_cd",
    "result_va",
    "lab_li_va",
    "lab_ui_va",
    "dqi_cd",
    "sample_type_cd"
  )
  d <- d[, cols]

  # remove row names
  rownames(d) <- NULL

  # remove categories that are not used
  d <- droplevels(d)

  d
}
