#' @title
#'  ASME B31G. Basic computer program CRVL.BAS
#'
#' @family ASME B31G functions
#'
#' @description
#'  Imitation of \emph{CVRL.BAS} computer program presented in
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}
#'  \emph{Appendix A} for determining allowable length and allowable operating
#'  pressure
#'
#' @param maop
#'  maximum allowable operating pressure - \emph{MAOP}, [\emph{PSI}]. Type: \code{\link{assert_double}}.
#'
#' @param d
#'  nominal outside diameter of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param smys
#'  specified minimum yield of stress (\emph{SMYS}) as a
#'  characteristics of steel strength, [\emph{PSI}]. Type: \code{\link{assert_double}}.
#'
#' @param def
#'  appropriate (combined) design factor from
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.4.2002.pdf}{ASME B31.4},
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.8.2003.pdf}{ASME B31.8},
#'  or \href{https://www.asme.org/codes-standards/find-codes-standards/b31-11-slurry-transportation-piping-systems}{ASME B31.11},
#'  []. Type: \code{\link{assert_double}}.
#'
#' @param depth
#'   measured maximum depth of the corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param l
#'  measured maximum longitudinal length of the corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  Object of \emph{S3}-class \emph{crvl} which is a \emph{data.frame} with
#'  the next numeric columns:
#'
#'  \describe{
#'   \item{maop}{maximum allowable operating pressure - \emph{MAOP}, [\emph{PSI}]. Type: \code{\link{assert_double}}.}
#'   \item{d}{nominal outside diameter of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.}
#'   \item{wth}{nominal wall thickness of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.}
#'   \item{smys}{specified minimum yield of stress (\emph{SMYS}) as a
#'               characteristics of steel strength, [\emph{PSI}]. Type: \code{\link{assert_double}}.}
#'   \item{def}{appropriate (combined) design factor from
#'              \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.4.2002.pdf}{ASME B31.4},
#'              \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.8.2003.pdf}{ASME B31.8},
#'              or \href{https://www.asme.org/codes-standards/find-codes-standards/b31-11-slurry-transportation-piping-systems}{ASME B31.11}, [].
#'              Type: \code{\link{assert_double}}.}
#'   \item{depth}{measured maximum depth of the corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.}
#'   \item{l}{measured maximum longitudial length of corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.}
#'   \item{status}{Operational status of pipe:
#'                 \emph{1} - excellent,
#'                 \emph{2} - monitoring is recommended,
#'                 \emph{3} - alert! replace the pipe immediately!
#'        Type: \code{\link{assert_numeric}}.}
#'   \item{design_pressure}{design pressure of pipe, [\emph{PSI}]. Type: \code{\link{assert_double}}.}
#'   \item{safe_pressure}{safe maximum pressure for the corroded area, [\emph{PSI}]. Type: \code{\link{assert_double}}.}
#'   \item{pressure_exceeding}{whether operator's action is required to reduce
#'                             \emph{MOAP} lower than the maximum safe pressure
#'                             of the corroded area. Type: \code{\link{assert_logical}}.}
#'   \item{allowed_corrosion_depth}{allowable depth of the corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.}
#'   \item{A}{intermediate factor related to the geometry of the corroded area, []. Type: \code{\link{assert_double}}.}
#'   \item{allowed_corrosion_length}{allowable length of the corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.}
#'   \item{AP}{another intermediate factor related to the geometry of the corroded area, []. Type: \code{\link{assert_double}}.}
#'  }
#'
#' @details
#'   Columns \emph{maop}, \emph{d}, \emph{wth}, \emph{smys}, \emph{def},
#'   \emph{depth}, \emph{l} in the output \emph{data.frame} come from
#'   function's input, other columns are calculated.
#'
#'   For univariate case (when lengths of all input vectors are one) messages
#'   that imitate \emph{CRVL.BAS} console output are printed.
#'
#' @references
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31 G-1991}.
#'  Manual for determining the remaining strength of corroded pipelines. A
#'  supplement to \emph{ASME B31G} code for pressure piping.
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' ## Further examples are inspired by those used in Appendix A of
#' ## ASME B31G-1991 to verify correct entry of CRVL.BAS source code
#'
#' ## Example 1
#' b31crvl(maop = 910, d = 30, wth = .438, smys = 52000, def  = .72, depth = .1, l = 7.5)
#' #
#' # -- Calculated data --
#' # Intermediate factor (A) = 1.847
#' # Design pressure = 1093 PSI; Safe pressure = 1093 PSI
#' # Pipe may be operated safely at MAOP, 910 PSI
#' # With corrosion length 7.500 inch, maximum allowed corrosion depth is 0.2490 inch; A = 1.847
#' # With corrosion depth 0.100 inch, maximum allowed corrosion length is Inf inch; A = 5.000
#'
#'
#' ## Example 2
#' b31crvl(maop = 400, d = 20, wth = .25, smys = 35000, def  = 0.5, depth = 0.18, l = 10)
#' #
#' # -- Calculated data --
#' # Intermediate factor (A) = 3.993
#' # Design pressure = 438 PSI; Safe pressure = 284 PSI
#' # Reduce operating pressure so it will not exceed 284 PSI, and so operate legally and safely
#' # With corrosion length 10.000 inch, maximum allowed corrosion depth is 0.0790 inch; A = 3.993
#' # With corrosion depth 0.180 inch, maximum allowed corrosion length is 2.0180 inch; A = 0.806
#'
#'
#' ## Example 3
#' b31crvl(maop = 910, d = 24, wth = .432, smys = 52000, def  = .72, depth = 0.13, l = 30)
#' #
#' # -- Calculated data --
#' # Intermediate factor (A) = 8.320
#' # Design pressure = 1348 PSI; Safe pressure = 1037 PSI
#' # Pipe may be operated safely at MAOP, 910 PSI
#' # With corrosion length 30.000 inch, maximum allowed corrosion depth is 0.1670 inch; A = 8.320
#' # With corrosion depth 0.130 inch, maximum allowed corrosion length is Inf inch; A = 5.000
#'
#'
#' ## Example 4
#' b31crvl(maop = 910, d = 24, wth = .432, smys = 52000, def  = .72, depth = .3, l = 30)
#' #
#' # -- Calculated data --
#' # Intermediate factor (A) = 8.320
#' # Design pressure = 1348 PSI; Safe pressure = 453 PSI
#' # Reduce operating pressure so it will not exceed 453 PSI, and so operate legally and safely
#' # With corrosion length 30.000 inch, maximum allowed corrosion depth is 0.1670 inch; A = 8.320
#' # With corrosion depth 0.300 inch, maximum allowed corrosion length is 12.8670 inch; A = 3.568
#'
#'
#' ## Example 5
#' b31crvl(maop = 731, d = 24, wth = .281, smys = 52000, def  = 0.72, depth = 0.08, l = 15)
#' #
#' # -- Calculated data --
#' # Intermediate factor (A) = 5.158
#' # Design pressure = 877 PSI; Safe pressure = 690 PSI
#' # Reduce operating pressure so it will not exceed 690 PSI, and so operate legally and safely
#' # With corrosion length 15.000 inch, maximum allowed corrosion depth is 0.0680 inch; A = 5.158
#' # With corrosion depth 0.080 inch, maximum allowed corrosion length is 11.6340 inch; A = 4.000
#'
#'
#' ## Example 6
#' b31crvl(maop = 1e3, d = 36, wth = .5, smys = 52000, def  = 0.72, depth = 0.41, l = 100)
#' # Alert! Corrosion depth exceeds 80 % of pipe wall! Pipe must be replaced!
#' # -- Calculated data --
#' # Intermediate factor (A) = 21.048
#' # Design pressure = 1040 PSI; Safe pressure = 206 PSI
#' # Repair or replace pipe because corrosion depth exceeds 80 % of pipe wall!
#' # Reduce operating pressure so it will not exceed 206 PSI, and so operate legally and safely
#' # With corrosion length 100.000 inch, maximum allowed corrosion depth is 0.0630 inch; A = 21.048
#' # With corrosion depth 0.410 inch, maximum allowed corrosion length is 2.5560 inch; A = 0.538
#' # But 0.410 inch exceeds allowable corrosion depth!!!
#'
#'
#' ## Example 7
#' b31crvl(maop = 877, d = 12.625, wth = .5, smys = 35000, def  = .4, depth = .035, l = 3)
#' # Corrosion depth is less than 10 % of pipe wall. No resrictions on operation
#' # -- Calculated data --
#' # Intermediate factor (A) = 1.066
#' # Design pressure = 1109 PSI; Safe pressure = 1109 PSI
#' # Pipe may be operated safely at MAOP, 877 PSI
#' # With corrosion length 3.000 inch, maximum allowed corrosion depth is 0.4000 inch; A = 1.066
#' # With corrosion depth 0.035 inch, maximum allowed corrosion length is Inf inch; A = 5.000
#'
#'
#' ## Example 8
#' b31crvl(maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .125, l = 12)
#' #
#' # -- Calculated data --
#' # Intermediate factor (A) = 3.093
#' # Design pressure = 875 PSI; Safe pressure = 845 PSI
#' # Pipe may be operated safely at MAOP, 790 PSI
#' # With corrosion length 12.000 inch, maximum allowed corrosion depth is 0.1790 inch; A = 3.093
#' # With corrosion depth 0.125 inch, maximum allowed corrosion length is 15.5190 inch; A = 4.000
#'
#'
#' ## TEST #1
#' b31crvl(maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .179, l = 12)
#' #
#' #-- Calculated data --
#' # Intermediate factor (A) = 3.093
#' # Design pressure = 875 PSI; Safe pressure = 791 PSI
#' # Pipe may be operated safely at MAOP, 790 PSI
#' # With corrosion length 12.000 inch, maximum allowed corrosion depth is 0.1790 inch; A = 3.093
#' # With corrosion depth 0.179 inch, maximum allowed corrosion length is 12.1820 inch; A = 3.140
#'
#'
#' ## TEST #1A
#' b31crvl(maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .179, l = 12.182)
#' #
#' # -- Calculated data --
#' # Intermediate factor (A) = 3.140
#' # Design pressure = 875 PSI; Safe pressure = 790 PSI
#' # Pipe may be operated safely at MAOP, 790 PSI
#' # With corrosion length 12.182 inch, maximum allowed corrosion depth is 0.1780 inch; A = 3.140
#' # With corrosion depth 0.179 inch, maximum allowed corrosion length is 12.1820 inch; A = 3.140
#'
#'
#' ## TEST #1B
#' b31crvl(maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .180, l = 12.182)
#' #
#' # -- Calculated data --
#' # Intermediate factor (A) = 3.140
#' # Design pressure = 875 PSI; Safe pressure = 789 PSI
#' # Reduce operating pressure so it will not exceed 789 PSI, and so operate legally and safely
#' # With corrosion length 12.182 inch, maximum allowed corrosion depth is 0.1780 inch; A = 3.140
#' # With corrosion depth 0.180 inch, maximum allowed corrosion length is 11.9610 inch; A = 3.083
#'
#'
#' ## TEST #2
#' b31crvl(maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .179, l = 12.297)
#' #
#' # -- Calculated data --
#' # Intermediate factor (A) = 3.170
#' # Design pressure = 875 PSI; Safe pressure = 789 PSI
#' # Reduce operating pressure so it will not exceed 789 PSI, and so operate legally and safely
#' # With corrosion length 12.297 inch, maximum allowed corrosion depth is 0.1780 inch; A = 3.170
#' # With corrosion depth 0.179 inch, maximum allowed corrosion length is 12.1820 inch; A = 3.140
#'
#'
#' ## All examples at once:
#' data(b31gdata)
#' examples <- with(b31gdata, b31crvl(maop, d, wth, smys, def, depth, l))
#'
b31crvl <- function(maop, d, wth, smys, def = .72, depth, l) {
  checkmate::assert_double(
    maop, lower = 25.4, upper = 1.27e5, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    d, lower = 3.93e-2, upper = 1.27e5, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    wth, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    smys, lower = 1e3, upper = 3e5, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    def, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    depth, lower = 0, upper = 2.54e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    l, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(maop), length(d), length(wth), length(smys), length(def),
    length(depth), length(l)
  )))

  pipe <- data.frame(
     maop  = maop
    ,d     = d
    ,wth   = wth
    ,smys  = smys
    ,def   = def
    ,depth = depth
    ,l      = l

    ,design_pressure = trunc(b31gdep(d, wth, smys, def))
    ,status          = b31gops(wth, depth)
    ,A               = b31gafr(d, wth, l)
  )

  pipe[["safe_pressure"]] <- b31gsap(pipe[["design_pressure"]], d, wth, depth, l)
  pipe[["allowed_corrosion_length"]] <- b31gacl(
     pipe[["design_pressure"]], maop, d, wth, depth, l
  )
  pipe[["allowed_corrosion_depth"]] <- b31gacd(
     pipe[["design_pressure"]], maop, d, wth, l
  )
  pipe[["pressure_exceeding"]] <- maop > pipe[["safe_pressure"]]
  pipe[["AP"]] <- ifelse(
        is.infinite(pipe[["allowed_corrosion_length"]]),
        5,
        round(pipe[["allowed_corrosion_length"]] / sqrt(d * wth) / 1.12, 3)
  )
  class(pipe) <- c("crvl", class(pipe))
  return(pipe)
}

#' @export
print.crvl <- function(x, ...){
  if (nrow(x) == 1) {
    cat(c(
      c(
        paste("Corrosion depth is less than 10 % of pipe wall.",
              "No resrictions on operation"),
        "",
        "Alert! Corrosion depth exceeds 80 % of pipe wall! Pipe must be replaced!"
      )[ x[["status"]] ],
      "-- Calculated data --",
      sprintf("Intermediate factor (A) = %02.3f", x[["A"]]),
      sprintf("Design pressure = %02i PSI; Safe pressure = %02i PSI",
              x[["design_pressure"]], x[["safe_pressure"]]),
      c(
        "Repair or replace pipe because corrosion depth exceeds 80 % of pipe wall!",
        sprintf("Pipe may be operated safely at MAOP, %02i PSI", x[["maop"]]),
        "MAOP exceeds design pressure P. Verify that this variance is valid",
        sprintf(
          paste("Reduce operating pressure so it will not exceed %02i PSI,",
                "and so operate legally and safely", collapse = ""),
          x[["safe_pressure"]])
      )[
        c(x[["depth"]] > .8*x[["wth"]],
          x[["safe_pressure"]] >= x[["maop"]],
          x[["maop"]] > x[["design_pressure"]],
          x[["pressure_exceeding"]])
        ],
      sprintf(
        paste("With corrosion length %02.3f inch, maximum allowed corrosion",
              "depth is %02.4f inch; A = %2.3f"),
        x[["l"]], x[["allowed_corrosion_depth"]], x[["A"]]),
      sprintf(
        paste("With corrosion depth %02.3f inch, maximum allowed corrosion",
              "length is %02.4f inch; A = %02.3f"),
        x[["depth"]], x[["allowed_corrosion_length"]], x[["AP"]]),
      if (x[["depth"]] > .8*x[["wth"]])
        sprintf("But %02.3f inch exceeds allowable corrosion depth!!!",
                x[["depth"]])
    ), sep = "\n")} else NextMethod('print')
}
