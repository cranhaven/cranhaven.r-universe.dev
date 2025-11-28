#' @title
#'  Pressure drop in pipe
#'
#' @family district heating
#'
#' @description
#'  Calculate \href{https://en.wikipedia.org/wiki/Pressure_drop}{pressure drop}
#'  in straight cylidrical steel pipe of \emph{district heating system} (where
#'  water is a heat carrier) that is a result of pipe orientation in space
#'  (hydrostatic component), and friction between water and internal wall of pipe.
#'
#' @param temperature
#'  temperature of heat carrier (water) inside the pipe, [\emph{°C}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param pressure
#'  \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'  of heat carrier (water) measured at the
#'  entrance (inlet) of pipe, [\emph{MPa}]. Type: \code{\link{assert_double}}.
#'
#' @param flow_rate
#'  amount of heat carrier (water) that is transferred by pipe during a period,
#'  [\emph{ton/hour}]. Type: \code{\link{assert_double}}.
#'
#' @param d
#'  internal diameter of pipe, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param len
#'  pipe length, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param roughness
#'  roughness of internal wall of pipe, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param inlet
#'  elevation of pipe inlet, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param outlet
#'  elevation of pipe outlet, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param method
#'  method of determining \emph{Darcy friction factor}.
#'  Type: \code{\link{assert_choice}}.
#'  (see \strong{Details})
#'
#' @details
#'  The underlying engineering model for calculation of pressure drop considers
#'  only two contributions (components):
#'  \enumerate{
#'    \item Pressure drop due to gravity (hydrostatic component).
#'    \item Pressure drop due to friction.
#'  }
#'
#'  The model does not consider any size changes of pipe and
#'  presence of fittings.
#'
#'  For the first component that depends on pipe position in space the next
#'  figure illustrates adopted disposition of pipe.
#'
#'  \figure{dropp.png}
#'
#'  So, the expression for the first component can be written as:
#'
#'  \deqn{g \rho (outlet - inlet)}
#'
#'  where \code{g} - is gravity factor, \eqn{m/s^2}, and \eqn{\rho} - density
#'  of water (heat carrier), \eqn{kg/m^3}; \code{inlet} and \code{outlet}
#'  are appropriate pipe elevations (under sea or any other adopted level),
#'  \eqn{m}.
#'
#'  The second component comes from
#'  \href{https://en.wikipedia.org/wiki/Darcy-Weisbach_equation}{Darcy–Weisbach equation}
#'  and is calculated using heating carrier regime parameters (\code{temperature},
#'  \code{pressure}, \code{flow_rate}). Temperature and pressure values of
#'  heat carrier define water properties according to
#'  \href{http://www.iapws.org/}{IAPWS} formulation.
#'
#'  Several methods for calculating of
#'  \emph{Darcy friction factor} are possible and limited to the next
#'  direct approximations of
#'  \href{https://en.wikipedia.org/wiki/Darcy_friction_factor_formulae#Brkić-Praks_solution}{Colebrook equation}:
#'
#'  \describe{
#'   \item{romeo}{Romeo, Royo and Monzon, 2002}
#'   \item{vatankhan}{Vatankhan and Kouchakzadeh, 2009}
#'   \item{buzelli}{Buzzelli, 2008}
#'  }
#'
#'  According to \emph{Brkic, 2011} approximations errors of those methods do not
#'  exceed \code{0.15} \% for the most combinations of
#'  \href{https://en.wikipedia.org/wiki/Reynolds_number}{Reynolds} numbers and
#'  actual values of internal
#'  wall \href{https://en.wikipedia.org/wiki/Surface_roughness}{roughness} of pipe.
#'
#' @return
#'  pressure drop at the outlet of pipe, [\emph{MPa}]. Type: \code{\link{assert_double}}.
#'
#' @references
#'  \itemize{
#'    \item W.Wagner et al. \emph{The IAPWS Industrial Formulation 1997 for the Thermodynamic
#'          Properties of Water and Steam}, J. Eng. Gas Turbines Power. Jan 2000,
#'          \strong{122}(1): \emph{150-184} (35 pages)
#'
#'    \item M.L.Huber et al.\emph{New International Formulation for the
#'          Viscosity of \eqn{H_2O}}, Journal of Physical and Chemical Reference Data
#'          \strong{38}, 101 (2009);
#'
#'    \item D.Brkic. \emph{Journal of Petroleum Science and Engineering}, Vol. \strong{77},
#'          \emph{Issue 1}, April 2011, Pages \emph{34-48}.
#'
#'    \item Romeo, E., Royo, C., Monzon, A., 2002. \emph{Improved explicit equation for
#'          estimation of friction factor in rough and smooth pipes.}
#'          Chem. Eng. J. \strong{86} (3), \emph{369–374}.
#'
#'    \item Vatankhah, A.R., Kouchakzadeh, S., 2009. \emph{Discussion: Exact equations
#'          for pipeflow problems, by P.K. Swamee and P.N. Rathie}. J. Hydraul. Res.
#'          IAHR \strong{47} (7), \emph{537–538}.
#'
#'    \item Buzzelli, D., 2008. \emph{Calculating friction in one step}.
#'          Mach. Des. \strong{80} (12), \emph{54–55}.
#'  }
#'
#' @seealso
#'  \code{\link{dropt}} for calculating temperature drop in pipe
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#' # Typical pressure drop for horizontal pipeline segments
#' # in high-way heating network in Novosibirsk
#' dropp(len = c(200, 300))
#'
#' #[1] 0.0007000666 0.0010500999
#'
dropp <- function(temperature = 130., pressure = mpa_kgf(6), flow_rate = 1276.,
                  d = 1., len = 1., roughness = 6e-3,
                  inlet = 0., outlet = 0.,
                  method = "romeo"){
  checkmate::assert_double(
    temperature, lower = 0, upper = 350, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    pressure, lower = 8.4e-2, upper = 100, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    flow_rate, lower = 1e-3, upper = 1e5, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    d, lower = 25e-3, upper = 2.0, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    len, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    roughness, lower = 0, upper = .2, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    inlet, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    outlet, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(temperature), length(pressure), length(flow_rate), length(d),
    length(len), length(roughness), length(inlet), length(outlet)
  )))
  checkmate::assert_choice(method, c("romeo", "vatankhan", "buzelli"))
  dh <- outlet - inlet
  checkmate::assert_true(all(abs(dh) < len))

  state <- iapws::if97(what = c("rho", "eta"), t = pipenostics::k_c(temperature), p = pressure)
  rho   <- unname(state[, "rho"])
  u     <- flow_rate*1e3/rho/(.25*pi*d^2)/3600  # [ton/hour]*[kg/ton]/[kg/m^3]/[m^2]/[s/hour] == [m/s]
  fric  <- get(sprintf("fric_%s", method))

  # Friction component, [MPa]:
  dpf <- fric(
    reynolds = re_u(
      d,
      mu = unname(state[, "eta"])*1e-6, # [kg/m/s]
      u = u,
      rho = rho
  ),  # []
    roughness = roughness/d
  ) * len/d*rho*u^2/2 * 1e-6  # [kg/m/s^2]*1e-6 == [MPa]

  # Hydrostatic component, [MPa]:
  dph <- rho*9.80665*dh * 1e-6  # [MPa]

  # Total pressure drop, [MPa]:
  dpf + dph
}