# Balls to Overs ----------------------------------------------------------

#' Convert Balls to Overs (Six Ball)
#'
#' Convert numbers of balls as it equates in terms of six ball overs.
#'
#' @param balls number of balls bowled/faced.
#'
#' @return number of six ball overs this equates too.
#' @export
#'
#' @examples
#' balls_to_overs(balls = 6)
#' balls_to_overs(balls = 17)
#'
balls_to_overs <- function(balls) {

  # get over number using integer division
  over_no <- calc_int_div(value = balls, divisor = 6)

  # get balls remaining in over using modulo
  balls_no <- calc_mod(value = balls, divisor = 6)


  over_no + (balls_no / 10)
}


# Overs to Balls ----------------------------------------------------------

#' Convert Overs (Six Ball) to Balls
#'
#' @param overs number of six ball overs bowled/faced.
#'
#' @return number of six ball overs this equates too.
#' @export
#'
#' @examples
#' overs_to_balls(overs = 8.2)
#' overs_to_balls(overs = 10)
#'
overs_to_balls <- function(overs) {

  # get over number using integer division
  over_no <- calc_int_div(value = overs, divisor = 1)

  # get balls remaining in over using modulo
  balls_no <- calc_mod(value = overs, divisor = 1)

  if (balls_no > 0.6) {
    mes <- paste(
      "Oops... an over can only contain 6 legitimate balls. You supplied",
      as.integer(balls_no * 10),
      "balls which is impossible. Please check your input!"
    )

    rlang::abort(message = mes)
  }

  (over_no) * 6 + (balls_no) * 10
}
