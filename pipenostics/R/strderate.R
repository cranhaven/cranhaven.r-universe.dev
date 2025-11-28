#' @title
#'  DNV-RP-F101. De-rate yield stress and tensile strength of pipe due to
#'  temperature
#'
#' @family DNV-RP-F101 functions
#'
#' @description
#'   Temperature is highly influence on pipe material properties and especially
#'   on its strength. Since in
#'   \href{https://law.resource.org/pub/us/cfr/ibr/002/api.5l.2004.pdf}{API SPECIFICATION 5L}
#'   values of \emph{SMYS} or \emph{UTS} are postulated at room conditions, in
#'   case of higher temperature magnitudes they should be corrected. For that
#'   purpose
#'   \href{https://www.dnv.com/oilgas/download/dnv-rp-f101-corroded-pipelines/}{DNV-RP-F101}
#'   offers linear de-rating for \emph{SMYS} or \emph{SMYS} according to
#'   figure 2-3.
#'
#' @param x
#'   specified minimum yield of stress (\emph{SMYS}), or ultimate tensile
#'   strength (\emph{UTS}), or specified minimum tensile strength (\emph{SMTS})
#'   as a characteristic of steel strength \strong{at room temperature},
#'   [\emph{MPa}]. Type: \code{\link{assert_double}}.
#'
#' @param temperature
#'   temperature of pipe wall, [\emph{°C}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'   de-rated value of \emph{x}, i.e. of appropriate pipe material property,
#'   [\emph{MPa}] .
#'   Type: \code{\link{assert_double}}.
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#' with(api5l3t, {
#' print(strderate(mpa_psi(smys), 53))
#' print(
#'   strderate(mpa_psi(uts),seq(0, 250, length.out = length(smys)))
#' )
#'})
#' # [1] 170.5689 205.0427 239.5165 287.7798 315.3588 356.7274 384.3064 411.8854 446.3592 480.8330
#' # [11] 549.7806
#' # [1] 310.2641 330.9483 413.6854 398.6854 404.3697 415.0540 439.5278 457.1068 460.8963 485.3701
#' # [11] 530.5282
#'
#'
strderate <- function(x, temperature = 24.3){
  checkmate::assert_double(x,
    lower = 0, upper = 5e3, finite = TRUE, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    temperature,
    lower = 0, upper = 350, finite = TRUE, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_true(commensurable(c(
    length(x), length(temperature)
  )))

  # Stress de-rating according to DNV-RP-F101, figure 2-3:
  derating <-
    .0 + (-30 + .6*temperature)*(temperature > 50)*(temperature <= 100) +
    (-10 + .4*temperature)*(temperature > 100)
  x - derating
}

