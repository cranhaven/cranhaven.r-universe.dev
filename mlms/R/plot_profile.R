#' Plot Profile Data
#'
#' @description Plot pressure, temperature, or water-quality data for a site visit.
#'
#' @param site_nm 'character' string.
#'   Local site name for a MLMS well.
#' @param time_dt 'POSIXct' or 'character' string.
#'   Estimated field visit time, the closest time in the vector of field visit start times.
#'   Note that water-quality samples may be collected up to one week after
#'   the site visit for pressure profiling.
#' @param type 'character' string.
#'   Plot type, choose "head" for hydraulic head, "temp" for fluid temperature,
#'   or a USGS 5-digit parameter code (`pcode` in the [`samples`] dataset).
#'   Hydraulic head is plotted by default.
#' @param replicates 'logical' flag.
#'   Whether to include replicate pressure measurements.
#' @param position 'character' string.
#'   Location used to position the legend.
#'   Choose a single keyword from the list
#'   "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#' @param file 'character' string.
#'   PDF file to send plot graphics.
#'   Defaults to the active graphics window.
#' @param ...
#'   Additional arguments to be passed to the [`pdf`][grDevices::pdf] function.
#'   Only relevant if the `file` argument is specified.
#'
#' @return Invisibly `NULL`
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso [`get_profile`] function is used to retrieve profile data.
#'
#' @export
#'
#' @examples
#' site_nm <- "USGS 133"
#' time_dt <- "2008-08-27"
#' plot_profile(site_nm, time_dt, position = "topleft")
#'
#' plot_profile(site_nm, time_dt, type = "temp")
#'
#' plot_profile(site_nm, time_dt, type = "07000", position = "bottomright")
#'
#' site_nm <- "USGS 131A"
#' time_dt <- "2012-10-24"
#' plot_profile(site_nm, time_dt)
#'
#' plot_profile(site_nm, time_dt, type = "temp")
#'
#' plot_profile(site_nm, time_dt, type = "07000")
#'
#' file <- tempfile("test-profile-", fileext = ".pdf")
#' plot_profile(site_nm, time_dt, file = file, pointsize = 10)
#'
#' unlink(file)

plot_profile <- function(site_nm,
                         time_dt,
                         type = "head",
                         replicates = FALSE,
                         position = NULL,
                         file = NULL,
                         ...) {

  # load datasets
  utils::data("visits", package = "mlms")

  # check arguments
  checkmate::assert_string(site_nm)
  if (is.character(time_dt)) {
    checkmate::assert_string(time_dt)
  } else {
    checkmate::assert_posixct(time_dt, any.missing = FALSE, len = 1)
  }
  checkmate::assert_string(type)
  checkmate::assert_flag(replicates)
  checkmate::assert_string(position, null.ok = TRUE)
  checkmate::assert_string(file, null.ok = TRUE)

  # assign parameter code
  pcode <- if (type %in% c("head", "temp")) NULL else type

  # get profile data
  d <- get_profile(site_nm, time_dt = time_dt, pcode = pcode)

  # remove replicate measurements
  if (!replicates) {
    d <- d[!d$replicate_fl, ]
  }

  # set pressure data
  if (type == "head") {
    main <- "Pressure Profile"
    xlab <- "Hydraulic head, in feet above the NAVD 88"
    x <- d$total_head_va

  # set temperature data
  } else if (type == "temp") {
    main <- "Temperature Profile"
    xlab <- "Fluid temperature, in degree Celsius"
    x <- d$temp_va

  # set water-quality data
  } else {
    d <- d[!is.na(d$sample_dt), ]
    main <- "Water-Quality Profile"
    xlab <- d$parm_short_nm[1]
    x <- d$result_va
  }

  # set variable data
  xli <- d$lab_li_va
  xui <- d$lab_ui_va
  xat <- c(x, xli, xui) |> pretty()

  # set altitude data
  y <- d$port_alt_va
  yli <- d$zone_bot_alt_va
  yui <- d$zone_top_alt_va
  yat <- c(y, yli, yui) |> pretty()

  # start graphics device
  if (!is.null(file)) {
    file <- path.expand(file) |> normalizePath(winslash = "/", mustWork = FALSE)
    checkmate::assert_path_for_output(file, overwrite = TRUE, extension = "pdf")
    grDevices::pdf(file, ...)
    on.exit(grDevices::dev.off())
  }

  # plot data
  plotrix::plotCI(
    x = x,
    y = y,
    ui = yui,
    li = yli,
    slty = 1,
    scol = "#050505",
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    xlab = xlab,
    ylab = "Altitdue, in feet",
    main = main,
    xlim = range(xat),
    ylim = range(yat)
  )

  # plot error bars
  is_err <- !is.null(xli) & !is.null(xui)
  if (is_err) {
    plotrix::plotCI(
      x = x,
      y = y,
      ui = xui,
      li = xli,
      err = "x",
      add = TRUE,
      slty = 1,
      scol = "#FF0000"
    )
  }

  # add legend
  if (!is.null(position)) {
    legend <- c("Port measurement", "Monitoring zone")
    pch <- c(21, NA)
    lty <- c(NA, 1)
    col <- c("#050505", "#050505")
    if (is_err) {
      legend <- c(legend, "Measurement uncertainty")
      pch <- c(pch, NA)
      lty <- c(lty, 1)
      col <- c(col, "#FF0000")
    }
    graphics::legend(position,
      legend = legend,
      pch = pch,
      lty = lty,
      col = col,
      box.lwd = 0.5,
      inset = 0.05
    )
  }

  # add axes
  xlabels <- formatC(xat, format = "f", big.mark = ",", drop0trailing = TRUE)
  ylabels <- formatC(yat, format = "f", big.mark = ",", drop0trailing = TRUE)
  graphics::axis(side = 1, at = xat, lwd = -1, lwd.ticks = 0.5, labels = xlabels)
  graphics::axis(side = 2, at = yat, lwd = -1, lwd.ticks = 0.5, labels = ylabels)

  # add framed border around plot
  graphics::box(lwd = 0.5)

  # add sub-title to plot
  stime_dt <- as.Date(d$stime_dt[1])
  sprintf("Well %s on %s", site_nm, stime_dt) |>
    graphics::mtext(side = 3, line = 0.5, cex = 0.8)

  invisible(NULL)
}
