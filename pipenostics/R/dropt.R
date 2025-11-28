#' @title
#'  Temperature drop in cylindrical steel pipe due heat loss
#'
#' @family district heating
#'
#' @description
#'  Calculate temperature drop in steel pipe of \emph{district heating system}
#'  (where water is a heat carrier) that is a result of heat loss through
#'  pipe wall and insulation.
#'
#' @param temperature
#'  temperature of heat carrier (water) inside the pipe measured at the
#'  inlet of pipe, [\emph{°C}]. Type: \code{\link{assert_double}}.
#'
#' @param pressure
#'  \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'  of heat carrier (water) inside the pipe, [\emph{MPa}]. Type: \code{\link{assert_double}}.
#'
#' @param flow_rate
#'  amount of heat carrier (water) that is transferred by pipe during a period,
#'  [\emph{ton/hour}]. Type: \code{\link{assert_double}}.
#'
#' @param loss_power
#'  power of heat loss - heat loss through area of pipe wall per hour, [\emph{kcal/hour}].
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  temperature drop at the outlet of pipe, [\emph{°C}]. Type: \code{\link{assert_double}}.
#'
#' @details
#'   Specific isobaric \href{https://en.wikipedia.org/wiki/Heat_capacity}{heat capacity}
#'   used in calculations is calculated according to
#'   \href{http://www.iapws.org/relguide/IF97-Rev.pdf}{IAPWS R7-97(2012)}
#'   for \strong{Region 1} since it is assumed that state of water in
#'   \emph{district heating system} is always in that region.
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  # Calculate normative temperature drop based on Minenergo-325 for pipe segment
#'  pipeline <- list(
#'    year   = 1968,
#'    laying = "channel",
#'    d      = 700, # [mm]
#'    len    = 1000 # [m]
#'  )
#'
#'  regime <- list(
#'    temperature = c(130, 150), # [°C]
#'    pressure    = .588399,     # [MPa]
#'    flow_rate   = 250          # [ton/hour]
#'  )
#'
#'  pipe_loss_power <- do.call(
#'      m325nhl,
#'      c(pipeline, temperature = list(regime[["temperature"]]), duration = 1)  # [kcal/hour]
#'  )
#'
#'  temperature_drop <- dropt(
#'    temperature = regime[["temperature"]], # [°C]
#'    loss_power  = pipe_loss_power          # [kcal/hour]
#'  )                                        # [°C]
#'
#'  print(temperature_drop)
#'
#'  # [1] 1.366806 1.433840

dropt <- function(
  temperature =        130, # [°C]
  pressure    = mpa_kgf(6), # [MPa]
  flow_rate   =        250, # [ton/hour]
  loss_power  =       7000  # [kcal/hour]
){

  checkmate::assert_double(
    temperature, lower = 0, upper = 350, finite = TRUE,  any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    pressure, lower = 8.4e-2, upper = 100, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    flow_rate, lower = 1e-3, upper = 1e5, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    loss_power, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
   length(temperature), length(pressure), length(flow_rate), length(loss_power)
  )))

  JOULE <- 0.2388458966                     # [cal/J]
  loss_power_J <- loss_power/JOULE          # [kJ/hour]
  g <- flow_rate * 1e3                      # [kg/hour]
  loss_power_J / g / unname(iapws::if97(what = "cp", t = pipenostics::k_c(temperature), pressure)[, 1])  # [°C]=[°K]
}
