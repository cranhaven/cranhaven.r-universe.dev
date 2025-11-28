#' @title
#'  Flow rate drop in pipe
#'
#' @family district heating
#'
#' @description
#'  Calculate \emph{drop} or \emph{recovery} of flow rate in pipe using
#'  geometric factors.
#'
#'  The calculated value may be positive or negative. When it is positive they
#'  have the \emph{drop}, i.e. the decrease of flow rate in the outlet of pipe
#'  under consideration. When the calculated value is negative they have the
#'  \emph{recovery}, i.e. the increase of flow rate in the outlet of pipe under
#'  consideration. In both cases to calculate flow rate on the outlet of pipe
#'  under consideration simply subtract the calculated value from the
#'  sensor-measured flow rate on the inlet.
#'
#' @param adj
#'  diameters of adjacent pipes through which discharges to and recharges from
#'  network occur, [\emph{mm}].
#'
#'  Types:
#'
#'  \describe{
#'    \item{\code{\link{assert_double}}}{
#'      total diameter of all adjacent pipes (total diameter case)}
#'    \item{\code{\link{assert_list}} of \code{\link{assert_double}}}{a set of
#'       diameters of adjacent pipes (particular diameter case)}
#'  }
#'
#'  Positive values of diameters of adjacent pipes correspond to discharging
#'  process through those pipe, whereas negative values of diameters mean
#'  recharging. See \strong{Details} and \strong{Examples} for further explanations.
#'
#' @param d
#'   diameter of pipe under consideration, [\emph{mm}]. Type: \code{\link{assert_double}}.
#'
#' @param flow_rate
#'   sensor-measured amount of heat carrier (water) that is transferred through
#'   the inlet of pipe during a period, [\emph{ton/hour}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  flow rate \emph{drop} or \emph{recovery} at the outlet of pipe,
#'  [\emph{ton/hour}], numeric vector. The value is positive for \emph{drop},
#'  whereas for \emph{recovery} it is negative. In both cases to calculate
#'  flow rate on the outlet of pipe under consideration simply subtract the
#'  calculated value from the sensor-measured flow rate on the inlet.
#'  Type: \code{\link{assert_double}}.
#'
#' @details
#'  It is common that sensor-measured flow rate undergoes discharges to
#'  network and recharges from it. For calculation of flow rate \emph{drop} or
#'  \emph{recovery} the next configuration of district heating network segment is
#'  assumed:
#'
#'  \figure{dropg.png}
#'
#'  Usually, there are no additional sensors that could measure flow rate in
#'  each flow fork. In that case they only may operate with geometric
#'  factors, i.e. assuming that flow rate is proportional to square of pipe
#'  diameter.
#'
#'  The simple summation of flow rates over all adjacent pipes produces
#'  the required flow rate \emph{drop} or \emph{recovery} located on the
#'  outlet of the pipe under consideration. Since there is concurrency between
#'  discharges and recharges the diameters of discharge pipes are regarded
#'  positive whereas diameters of recharge pipes must be negative.
#'
#'  Be careful when dealing with geometric factors for large amount of recharges
#'  from network: there are no additional physical constraints and thus the
#'  calculated value of \emph{recovery} may have non-sense.
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#' # Let consider pipes according to network segment scheme depicted in figure
#' # in [?dropg] help-page.
#'
#' # Typical large diameters of pipes under consideration, [mm]:
#' d <- as.double(unique(subset(pipenostics::m325nhldata, diameter > 700)$diameter))
#'
#' # Let sensor-measured flow rate in the inlet of pipe
#' # under consideration be proportional to d, [ton/hour]:
#' flow_rate <- .125*d
#'
#' # Let consider total diameter case when total diameters of adjacent pipes are no
#' # more than d, [mm]:
#' adj <- c(450, -400, 950, -255, 1152)
#'
#' # As at may be seen for the second and fourth cases they predominantly have
#' # recharges from network.
#' # Let calculate flow rate on the outlet of the pipe under consideration,
#' # [ton/hour]
#'
#' result <- flow_rate - dropg(adj, d, flow_rate)
#' print(result)
#'
#' # [1]  75.96439 134.72222  65.70302 180.80580  78.05995
#'
#' # For more clarity they may perform calculations in `data.table`.
dropg <- function(adj = 0, d = 700, flow_rate = 250) {
  UseMethod("dropg", adj)
}

#' @export
dropg.list <- function(adj = 0, d = 700, flow_rate = 250){
  checkmate::assert_list(adj, types = "double", any.missing = FALSE, min.len = 1L)
  checkmate::assert_double(d, lower = 25, upper = 2500, finite = TRUE,
                           any.missing = FALSE, min.len = 1L)
  checkmate::assert_double(flow_rate, lower = 1e-3, upper = 1e5,
                           finite = TRUE, min.len = 1L)
  adj <- vapply(adj, sum, FUN.VALUE = .1)
  NextMethod("dropg")
}

#' @export
dropg.default <- function(adj = 0, d = 700, flow_rate = 250){
  checkmate::assert_double(adj, finite = TRUE,
                           any.missing = FALSE, min.len = 1L)
  checkmate::assert_double(d, lower = 25, upper = 2500, finite = TRUE,
                           any.missing = FALSE, min.len = 1L)
  checkmate::assert_double(flow_rate, lower = 1e-3, upper = 1e5,
                           finite = TRUE, min.len = 1L)
  checkmate::assert_true(commensurable(c(
    length(adj), length(d), length(flow_rate)
  )))

  sign(adj) * flow_rate * adj^2/(adj^2 * (adj > 0) + d^2)
}

