#' @title
#'  Minenergo-325. Normative heat loss of pipe
#'
#' @family Minenergo
#'
#' @description
#'  Calculate normative heat loss of pipe that is legally affirmed by
#'  \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'
#' @param year
#'   year when the pipe is put in operation after laying or total overhaul.
#'   Type: \code{\link{assert_integerish}}
#'
#' @param laying
#'  type of pipe laying depicting the position of pipe in space:
#'  \itemize{
#'    \item \code{air},
#'    \item \code{channel},
#'    \item \code{room},
#'    \item \code{tunnel},
#'    \item \code{underground}.
#'  }
#'  Type: \code{\link{assert_subset}}.
#'
#' @param exp5k
#'  pipe regime flag: is pipe operated more that 5000 hours per year?
#'  Type: \code{\link{assert_logical}}.
#'
#' @param insulation
#'  insulation that covers the exterior of pipe:
#'  \describe{
#'    \item{\code{0}}{no insulation}
#'    \item{\code{1}}{foamed polyurethane or analogue}
#'    \item{\code{2}}{polymer concrete}
#'  }
#'  Type: \code{\link{assert_integer}} and \code{\link{assert_subset}}.
#'
#' @param d
#'   internal diameter of pipe, [\emph{mm}]. Type: \code{\link{assert_double}}.
#'
#' @param temperature
#'  temperature of heat carrier (water) inside the pipe, [\emph{°C}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param len
#'  length of pipe, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param duration
#'  duration of heat loss, [\emph{hour}]. Type: \code{\link{assert_double}}.
#'
#' @param beta
#'  should they consider additional heat loss of fittings?
#'  Type: \code{\link{assert_logical}}.
#'
#' @param extra
#'   number of points used for temperature extrapolation: \code{2}, \code{3},
#'   or \code{4}. Type: \code{\link{assert_choice}}.
#'
#' @return
#'  Normative heat loss of cylindrical pipe during \code{duration}, [\emph{kcal}].
#'  If \code{len} of pipe is 1 \emph{m} (meter) as well as \code{duration} is set to
#'  1 \emph{h} (hour) (default values) then the return value is also the
#'  \emph{specific heat loss power}, [\emph{kcal/m/h}], prescribed by
#'  \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'  Type: \code{\link{assert_double}}.
#'
#' @details
#'  Temperature extrapolation and pipe diameter interpolation are leveraged
#'  for better accuracy. Both are linear as it dictated by
#'  \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'  Nevertheless, one could control the extrapolation behavior by \code{extra}
#'  argument: use lower values of \code{extra} for soft curvature near extrapolation
#'  edges, and higher values for more physically reasoned behavior in far regions
#'  of extrapolation.
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  ## Consider a 1-meter length pipe with
#'  pipe_diameter <-  700.0  # [mm]
#'  pipe_dating   <- 1980
#'  pipe_laying   <- "underground"
#'
#'
#'  ## Linear extrapolation adopted in Minenergo's Order 325 using last two points:
#'  operation_temperature <- seq(0, 270, 10)
#'
#'  qs <- m325nhl(
#'    year = pipe_dating, laying = pipe_laying, d = pipe_diameter,
#'    temperature = operation_temperature
#'  )  # [kcal/m/h]
#'
#'  plot(
#'      operation_temperature,
#'      qs,
#'      type = "b",
#'      main = "Minenergo's Order 325. Normative heat loss of pipe",
#'      sub = sprintf(
#'        "%s pipe of diameter %i [mm] laid in %i",
#'         pipe_laying, pipe_diameter, pipe_dating
#'      ),
#'      xlab = "Temperature, [°C]",
#'      ylab = "Specific heat loss power, [kcal/m/h]"
#'    )
#'
#'
#'  ## Consider heat loss due fittings:
#'  operation_temperature <- 65  # [°C]
#'
#'  fittings_qs <- m325nhl(
#'    year = pipe_dating, laying = pipe_laying, d = pipe_diameter,
#'    temperature = operation_temperature, beta = c(FALSE, TRUE)
#'  )  # [kcal/m/h]
#'
#'  print(fittings_qs); stopifnot(all(round(fittings_qs ,1)  == c(272.0, 312.8)))
#'
#'  # [1] 272.0 312.8  # [kcal/m/h]
#'
#'
#'
#'  ## Calculate heat flux:
#'  operation_temperature <- c(65, 105)  # [°C]
#'
#'  qs <- m325nhl(
#'    year = pipe_dating, laying = pipe_laying, d = pipe_diameter,
#'    temperature = operation_temperature
#'  )  # [kcal/m/h]
#'  print(qs)
#'
#'  # [1] 272.00 321.75  # [kcal/m/h]
#'
#'  pipe_diameter <- pipe_diameter * 1e-3          # [m]
#'  factor        <- 2.701283                      # [kcal/h/W]
#'
#'  flux <- qs/factor/pipe_diameter -> a  # heat flux, [W/m^2]
#'  print(flux)
#'
#'  # [1] 143.8470 170.1572  # [W/m^2]
#'
#'  ## The above line is equivalent to:
#'
#'  flux <- flux_loss(qs, pipe_diameter) -> b
#'
#'  stopifnot(all.equal(a, b, tolerance = 5e-6))
#'

m325nhl <- function(year        =          1986,
                    laying      = "underground",
                    exp5k       =          TRUE,

                    insulation  =             0,
                    d           =           700,
                    # [mm]
                    temperature =           110,
                    # [°C]
                    len         =             1,
                    # [m]
                    duration    =             1,
                    beta        =         FALSE,
                    extra       =             2) {
  norms <- pipenostics::m325nhldata
  checkmate::assert_integerish(
    year,
    lower = 1900L,
    upper = max(norms[["epoch"]]),
    any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_subset(laying, choices = unique(norms[["laying"]]),
                           empty.ok = FALSE)
  checkmate::assert_logical(exp5k, any.missing = FALSE, min.len = 1L)
  checkmate::assert_subset(insulation, choices = unique(norms[["insulation"]]))
  checkmate::assert_double(
    d,
    lower = min(norms[["diameter"]]),
    upper = max(norms[["diameter"]]),
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    temperature,
    lower = 0,
    upper = max(norms[["temperature"]]),
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
  checkmate::assert_double(
    duration,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_logical(beta, any.missing = FALSE, min.len = 1L)
  checkmate::assert_true(commensurable(
    c(
      length(year),
      length(laying),
      length(exp5k),
      length(insulation),
      length(d),
      length(temperature),
      length(len),
      length(duration),
      length(beta)
    )
  ))

  checkmate::assert_choice(extra, c(2, 3, 4))

  worker <-
    function(year_value,
             laying_value,
             exp5k_value,
             insulation_value,
             d_value,
             temperature_value,
             len_value,
             duration_value,
             beta_value) {
      epoch <- with(list(epochs = unique(norms[["epoch"]])), {
        epochs[[findInterval(year_value, epochs, left.open = TRUE) + 1L]]
      })
      norms <- norms[norms[["epoch"]] == epoch &
                       norms[["laying"]] == laying_value &
                       norms[["exp5k"]] == exp5k_value &
                       norms[["insulation"]] == insulation_value,]
      neighbor_diameter <-
        with(list(norms_diameter = unique(norms[["diameter"]])), {
          checkmate::assert_double(
            d_value,
            lower = min(norms_diameter),
            upper = max(norms_diameter),
            finite = TRUE,
            any.missing = FALSE,
            unique = TRUE
          )
          n <- findInterval(d_value, norms_diameter)
          norms_diameter[c(n, n + 1L)]
        })
      unit_loss <-
        vapply(neighbor_diameter, function(x, t_carrier) {
          if (is.na(x))
            return(NA_real_)
          with(norms[norms[["diameter"]] == x, c("temperature", "loss")], {
            zone <- findInterval(t_carrier, range(temperature),
                                 rightmost.closed = TRUE)
            if (zone == 1L) {
              stats::approx(x = temperature,
                            y = loss,
                            xout = t_carrier)[["y"]]
            } else {
              j <- order(temperature, decreasing = as.logical(zone))[1:extra]
              drop(coef(lsfit(temperature[j], loss[j])) %*% c(1., t_carrier))
            }
          })
        }, FUN.VALUE = 1., t_carrier = temperature_value)
      unit_loss <-
        if (all(!is.na(unit_loss)))
          stats::approx(neighbor_diameter, unit_loss, d_value)[["y"]]
      else
        unit_loss[!is.na(unit_loss)][[1L]]
      unit_loss * len_value * duration_value * (pipenostics::m325beta(laying_value, d_value) * beta_value+!beta_value)
    }
  unlist(Map(
    worker,
    year,
    laying,
    exp5k,
    insulation,
    d,
    temperature,
    len,
    duration,
    beta
  ))
}
