#' @title
#'   Minenergo-278. Normative heat loss of pipe in channel
#'
#' @family Minenergo
#'
#' @description
#'  Calculate normative heat loss of the supplying pipe mounted in underground channel
#'  as a function of construction, operation, and technical condition
#'  specifications according to
#'  Appendix 5.1 of \href{https://docs.cntd.ru/document/1200035568}{Minenergo Method 278}.
#'
#'  This type of calculations is usually made on design stage of district
#'  heating network (where water is a heat carrier) and is closely related
#'  to building codes and regulations.
#'
#' @param t1
#'   temperature of heat carrier (water) inside the supplying pipe, [\emph{°C}].
#'   Type: \code{\link{assert_double}}.
#' @param t2
#'   temperature of heat carrier (water) inside the returning pipe, [\emph{°C}].
#'   Type: \code{\link{assert_double}}.
#' @param t0
#'   temperature of environment, [\emph{°C}]. In case of channel laying this is
#'   the temperature of subsoil. Type: \code{\link{assert_double}}.
#' @param insd1
#'   thickness of the insulator which covers the supplying pipe, [\emph{m}].
#'   Type: \code{\link{assert_double}}.
#' @param insd2
#'   thickness of the insulator which covers the returning pipe, [\emph{m}].
#'   Type: \code{\link{assert_double}}.
#' @param d1
#'   outside diameter of supplying pipe, [\emph{m}]. Type: \code{\link{assert_double}}.
#' @param d2
#'   outside diameter of returning pipe, [\emph{m}]. Type: \code{\link{assert_double}}.
#' @param lambda1
#'   thermal conductivity of insulator which covers the supplying pipe
#'   [\emph{W/m/°C}]. Type: \code{\link{assert_double}}.
#' @param lambda2
#'   thermal conductivity of insulator which covers the returning pipe
#'   [\emph{W/m/°C}]. Type: \code{\link{assert_double}}.
#' @param k1
#'   technical condition factor for insulator of supplying pipe, [].
#'   Type: \code{\link{assert_double}}.
#' @param k2
#'   technical condition factor for insulator of returning pipe, [].
#'   Type: \code{\link{assert_double}}.
#' @param lambda0
#'   thermal conductivity of environment, [\emph{W/m/°C}]. In case of channel
#'   laying this is the thermal conductivity of subsoil. Type: \code{\link{assert_double}}.
#' @param z
#'   channel laying depth, [\emph{m}]. Type: \code{\link{assert_double}}.
#' @param b
#'   channel width, [\emph{m}]. Type: \code{\link{assert_double}}.
#' @param h
#'   channel height, [\emph{m}]. Type: \code{\link{assert_double}}.
#' @param len
#'  length of supplying pipe, [\emph{m}]. Type: \code{\link{assert_double}}.
#' @param duration
#'  duration of heat loss, [\emph{hour}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  Normative heat loss of supplying cylindrical pipe mounted in channel during \code{duration}, [\emph{kcal}].
#'  If \code{len} of pipe is 1 \emph{m} (meter) as well as \code{duration} is set to
#'  1 \emph{h} (hour) (default values) then the return value is also the
#'  \emph{specific heat loss power}, [\emph{kcal/m/h}] and so comparable with those
#'  prescribed by \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'  Type: \code{\link{assert_double}}.
#'
#' @details
#'   \code{k1} and \code{k2} factor values equal to \code{1} mean the best technical
#'   condition of insulation of appropriate pipes, whereas for poor technical
#'   state factor values tends to \code{5} or more.
#'
#'   Nevertheless, when \code{k1} and \code{k2} both equal to \code{1} the calculated
#'   \emph{specific heat loss power} [\emph{kcal/m/h}] is sometimes higher than that listed in
#'   \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'   One should consider that situation when choosing method for heat loss
#'   calculations.
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  m278hlcha()
#'  #
#'
#'  ## Naive way to find out technical state (factors k1 and k2) for pipe
#'  ## segments constructed in 1980:
#'    optim(
#'      par = c(1.5, 1.5),
#'      fn = function(x) {
#'      # functional to optimize
#'        abs(
#'            m278hlcha(k1 = x[1], k2 = x[2]) -
#'            m325nhl(year = 1980, laying = "channel", d = 250, temperature = 110)
#'        )
#'      },
#'      method = "L-BFGS-B",
#'      lower = 1.01, upper = 4.4
#'    )$par
#'    # [1] 4.285442 4.323628
#'
m278hlcha <-
  function(t1 = 110, t2 = 60, t0 = 5, insd1 = 0.1, insd2 = insd1, d1 = .25,
           d2 = d1, lambda1 = 0.09, lambda2 = 0.07, k1 = 1, k2 = k1,
           lambda0 = 1.74, z = 2, b = 0.5, h = 0.5, len = 1, duration = 1) {
    checkmate::assert_double(
      t1,
      lower = 0,
      upper = 450,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      t2,
      lower = 0,
      upper = 450,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      t0,
      lower = -15,
      upper = 30,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      insd1,
      lower = 0,
      upper = .5,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      insd2,
      lower = 0,
      upper = .5,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      d1,
      lower = .2,
      upper = 1.5,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      d2,
      lower = .2,
      upper = 1.5,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      lambda1,
      lower = 1e-3,
      upper = 1,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      lambda2,
      lower = 1e-3,
      upper = 1,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      k1,
      lower = 1,
      upper = 4.5,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      k2,
      lower = 1,
      upper = 4.5,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      lambda0,
      lower = 1e-3,
      upper = 3,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      z,
      lower = .1,
      upper = 10,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      b,
      lower = min(d1, d2),
      upper = 10,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      h,
      lower = min(d1, d2),
      upper = 10,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(
      len,
      lower = 0,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_double(duration,
      lower = 0,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1L
    )
    checkmate::assert_true(commensurable(c(
      length(t1), length(t2), length(t0), length(insd1), length(insd2),
      length(d1), length(d2), length(lambda1), length(lambda2), length(k1),
      length(k2), length(lambda0), length(z), length(b), length(h), length(len),
      length(duration)
    )))

    R0 <- log(3.5 * z / h * (h / b) ^ .25) / lambda0 / (5.7 + .5 * b / h)
    d <- 2 * b * h / (b + h)
    R_chan_air <- 1 / (8 * pi * d)
    R1_air <- 1 / (8 * pi * (d1 + 2 * insd1))
    R2_air <- 1 / (8 * pi * (d2 + 2 * insd2))
    R1_ins <- log(1 + 2 * insd1 / d1) / (2 * pi * k1 * lambda1)
    R2_ins <- log(1 + 2 * insd2 / d2) / (2 * pi * k2 * lambda2)
    t_chan <-
      (t1 / (R1_ins + R1_air) + t2 / (R2_ins + R2_air) + t0 / (R_chan_air + R0)) /
      (1 / (R1_ins + R1_air) + 1 / (R2_ins + R2_air) + 1 / (R_chan_air + R0))
    q <- (t_chan - t0) / (R_chan_air + R0)

    q * len * duration
}