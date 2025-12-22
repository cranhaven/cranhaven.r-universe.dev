#' Get Candles for a Set of Securities
#'
#' REST path:
#' `/engines/[engine]/markets/[market]/boards/[board]/securities/[security]/candles`
#' (see <http://iss.moex.com/iss/reference/46>).
#'
#' To get the `engine-market-board` path a separate [get_security_info] query is
#' made and the board with `is_primary = 1` is selected.
#'
#' All candles for the specified period will be fetched, see [fetching_fully].
#'
#' @param secid A vector of security ID's.
#' @param from A date or a datetime object, or something that can be coerced to
#'   it with [readr::parse_date] or [readr::parse_datetime].
#' @param till A date or a datetime object, or something that can be coerced to
#'   it with [readr::parse_date] or [readr::parse_datetime], or `NULL`.  If
#'   `NULL`, interpreted by ISS as "today".
#' @param interval A character value specifying the candle duration (see
#'   `moexer.candle.intervals` option.)
#' @param ... Further arguments to [query_iss].
#'
#' @return A tibble as with candles in OHLCV format, prepended with a column
#'   containing the corresponding security ID.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get daily candles for `SBER`, `FXGD` from 2020-01-01 until today. Note that
#' # an unknown symbol `XXXX` is skipped with a warning.
#' get_candles(
#'     secid = c('XXXX', 'SBER', 'FXGD'),
#'     from = '2020-01-01',
#'     debug_output = TRUE
#' )
#'
#' # Get SBER minute candles for one trading day (all 526 of them)
#' get_candles(
#'     secid = 'SBER',
#'     from = '2020-01-10',
#'     till = '2020-01-10',
#'     interval = 'per_minute'
#' )
#'
#' # Get SBER minute candles for the specified time period (1 hour)
#' get_candles(
#'     secid = 'SBER',
#'     from = '2020-01-10 11:00:00',
#'     till = '2020-01-10 12:00:00',
#'     interval = 'per_minute'
#' )
#' }
get_candles <- function(secid, from, till = NULL, interval = 'monthly', ...) {
    from_str <- process_instant(from)
    till_str <- process_instant(till)

    interval_names <- names(getOption('moexer.candle.intervals'))
    if (!interval %in% interval_names) {
        abort(glue('Possible interval values: {paste0(interval_names, collapse = ", ")}'))
    }
    interval_num <- getOption('moexer.candle.intervals')[[interval]]

    get_secid_candles <- function(secid) {
        bp <- determine_primary_board_path(secid = secid, ...)
        if (is.null(bp)) {
            cli_alert_warning('Skipping secid = {secid}: cannot determine the primary board path.')
            return(NULL)
        }
        iss_response <- fetching_fully(query_iss)(
            rest_path = glue(
                'engines/{bp$engine}/markets/{bp$market}',
                '/boards/{bp$boardid}/securities/{secid}/candles'
            ),
            params = list(
                from = URLencode(from_str),
                till = URLencode(till_str),
                interval = interval_num
            ),
            ...
        )
        iss_response$candles |>
            add_column(secid = secid, .before = 1)
    }

    secid |>
        map_dfr(get_secid_candles) |>
        append_class('MoexCandles')
}


process_instant <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }

    assert_that(
        !is.na(x),
        is.date(x) | is.time(x) | is.string(x)
    )

    if (is.string(x)) {
        x_parsed <- readr::parse_date(x) |> suppressWarnings()
        if (is.na(x_parsed)) {
            # Probably a datetime?
            x_parsed <- readr::parse_datetime(x) |> suppressWarnings()
            if (is.na(x_parsed)) {
                cli_abort('Failed to parse {.val {x}} as date/datetime.')
            }
        }
        x <- x_parsed
    }

    if (is.time(x)) {
        format(x, '%Y-%m-%d %H:%M:%S')
    } else {
        format(x, '%Y-%m-%d')
    }
}


#' Get Possible Candle `from-till` Values for a Security
#'
#' REST path:
#' `/engines/[engine]/markets/[market]/boards/[board]/securities/[security]/candleborders`
#' (see <http://iss.moex.com/iss/reference/48>).
#'
#' To get the `engine-market-board` path a separate [get_security_info] query is
#' made and the board with `is_primary = 1` is selected.
#'
#' @param secid A vector of security ID's.
#' @param ... Further arguments to [query_iss].
#'
#' @return A tibble with possible `from-till` values for each interval;
#'   additionally the intervals-durations mapping tibble is joined.
#' @export
#'
#' @examples
#' \dontrun{
#' get_candle_borders(secid = c('SBER', 'FXGD'))
#' }
get_candle_borders <- function(secid, ...) {
    get_secid_candle_borders <- function(secid) {
        bp <- determine_primary_board_path(secid = secid, ...)
        if (is.null(bp)) {
            cli_alert_warning('Skipping secid = {secid}: cannot determine the primary board path.')
            return(NULL)
        }
        iss_response <- query_iss(
            rest_path = glue(
                'engines/{bp$engine}/markets/{bp$market}/boards/{bp$board}',
                '/securities/{secid}/candleborders'
            ),
            ...
        )
        iss_response$borders |>
            left_join(iss_response$durations, by = 'interval') |>
            add_column(secid = secid, .before = 1)
    }

    secid |> map_dfr(get_secid_candle_borders)
}



#' Get Candle Durations-Intervals Mapping
#'
#' REST path: `/index?iss.only=durations` (see http://iss.moex.com/iss/reference/28)
#'
#' @param ... Further arguments to [query_iss].
#'
#' @return A tibble with the durations-intervals mapping.
#' @export
#'
#' @examples
#' \dontrun{
#' get_candle_durations()
#' }
get_candle_durations <- function(...) {
    iss_response <- query_iss(
        rest_path = 'index',
        params = list(iss.only = 'durations'),
        ...
    )
    iss_response$durations
}