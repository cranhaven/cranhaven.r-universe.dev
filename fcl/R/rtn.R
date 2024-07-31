#' @export
`[[.RRtn` <- `$.RRtn`

#' Create a Return Object
#'
#' By providing a "group" (`ids`) of `dates`, `mvs` and `pls`,
#' calucating the Time-weighted Rate of Return (TWRR) or Modified
#' Dietz Rate of Return (DIETZ).
#'
#' @param date a Date vector, the reference date of each row
#' @param mv,pl a double vector, the market value and the 'PnL' (Profit and Loss) of each day
#' @param id an integer vector, the ID of each row belongs to
#' @section Cash flow handling:
#'   * The cash flow is not provided externally. Instead, it's deducted via
#'     market value and PnL, with the equation \eqn{\Delta MV = \Delta PnL + CF}.
#'   * The cash inflow is treating as if it happens at the beginning of the day,
#'     while the cash outflow is at the end of the day. The reasons are two.
#'     The first is to reduce the possibility of having a close-to-zero denominator.
#'     The second is the cash is usually not usable for the whole day.
#'   * The calculation is based on calendar days. No business calendar or weekday
#'     considers. You can't change the calculation frequency, either. However,
#'     this is possible in the future version.
#' @references
#' Modified Dietz Method: https://en.wikipedia.org/wiki/Modified_Dietz_method
#'
#' Time weighed Return: https://en.wikipedia.org/wiki/Time-weighted_return
#' @note All the input vector must be 1 or the same length.
#' @return A list of functions, with signature of `from`, `to` and `id`, all of
#'   which are only allowed to accept a scalar. They all return an `xts` object
#'   with one column.
#'   * `twrr_cr`: the cumulative Time-weighted Return
#'   * `twrr_dr`: the daily Time-weighted Return
#'   * `dietz`: the Modified Dietz Return
#'   * `dietz_avc`: the denominator used to calculate the 'Modifie Dietz Return
#'   * `cum_pl`: the cumulative PnL
#' @examples
#' rtn <- make_rtn(date = c(210101, 210105, 210110), mv = c(100, 123, 140), pl = c(0, 3, 7))
#' rtn$twrr_cr(210102, 210110)
#' rtn$twrr_dr(210102, 210110)
#' rtn$dietz(210102, 210110)
#' rtn$dietz_avc(210102, 210110)
#' @export
make_rtn <- function(date, mv, pl, id = 1L) {
  args <- prepare_args(
    ymd(date), as.double(mv), as.double(pl), as.integer(id)
  )
  obj <- do.call(RRtn$new, args)
  out <- new.env()
  out$.self <- obj
  id_default <- if (length(unique(id)) == 1L) {
    unique(id)
  }
  vars <- c("twrr_cr", "twrr_dr", "dietz", "dietz_avc", "cum_pl")
  lapply(vars, function(var) {
    fun <- function(from, to, id) {
      if (missing(id) && !is.null(id_default)) {
        id <- id_default
      }
      args <- prepare_args(
        from = ymd(from), to = ymd(to), id = as.integer(id), .len = 1L
      )
      .self <- out$.self
      with(args, xts::xts(
        matrix(.self[[var]](from, to, id), ncol = 1L, dimnames = list(NULL, toupper(var))),
        .self$dates(from, to)
      ))
    }
    assign(var, fun, envir = out)
  })
  out
}
