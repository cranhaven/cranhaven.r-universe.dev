#' Submit a REST Query to ISS
#'
#' See <http://iss.moex.com/iss/reference/> for the list of available endpoints.
#'
#' @param rest_path A REST path appended to `iss_base_url`.
#' @param params A HTTP GET query parameters string, passed as a `list` (not
#'   URL-escaped!)
#' @param debug_output Print REST URLs as they are queried.
#'
#' @return A list of tibbles, corresponding to the blocks in the response.
#' @export
#'
#' @examples
#' \dontrun{
#' query_iss(
#'     rest_path = 'securities/SBER',
#'     params = list(iss.only = 'description'),
#'     debug_output = TRUE
#' )
#' }
query_iss <- function(
        rest_path,
        params = list(),
        debug_output = getOption('moexer.debug')
    ) {
    iss_base_url <- getOption('moexer.iss.base_url')
    iss_query_url <- glue('{iss_base_url}/{rest_path}.json')
    iss_param_str <-
        params |>
        compact() |>
        imap_chr(\(val, name) glue('{name}={val}')) |>
        paste0(collapse = '&')
    if (str_length(iss_param_str) > 0) {
        iss_query_url <- glue('{iss_query_url}?{iss_param_str}')
    }
    if (debug_output) {
        inform(iss_query_url)
    }
    httr::GET(iss_query_url) |>
        httr::content('text') |>
        jsonlite::fromJSON(simplifyVector = TRUE, simplifyMatrix = FALSE) |>
        map(parse_iss_response_section)
}


parse_iss_response_section <- function(iss_response_section) {
    parse_type <- function(col, col_name) {
        if (!is.character(col)) {
            return(col)
        }
        data_type <- iss_response_section$metadata[[col_name]]$type
        parse_fn <-
            switch(
                data_type,
                string = parse_character,
                int32 = parse_double,
                int64 = parse_double,
                # int32 = function(x) x,
                # int64 = function(x) x,
                date = parse_date,
                datetime = parse_datetime,
                double = parse_double,
                time = parse_time,
                undefined = {
                    cli_alert_warning(glue(
                        '"{col_name}": Undefined type (parsed as character)'
                    ))
                    parse_character
                },
                abort(glue('"{col_name}": Unknown type "{data_type}"'))
            )
        parse_fn(col)
    }

    iss_response_section$data |>
        transpose(.names = iss_response_section$columns) |>
        imap(function(list_column, column_name) {
            list_column |>
                unlist() |>
                parse_type(column_name)
        }) |>
        as_tibble()
}


#' A Decorator for Fetching All Available Data
#'
#' A decorator for [query_iss] to increase the `start` parameter in a loop until
#' no more data is received.
#'
#' @param query_iss_fn A function object corresponding to [query_iss].
#' @return Decorated `query_iss_fn`.
#' @examples
#' \dontrun{
#' fetching_fully(query_iss)(
#'     'engines/stock/markets/shares/boards/TQBR/securities/SBER/candles',
#'     params = list(
#'         from = URLencode('2020-01-10 10:00:00'),
#'         till = URLencode('2020-01-10 23:59:59'),
#'         interval = 1,
#'         start = 10
#'     )
#' )
#' }
#' @export
fetching_fully <- function(query_iss_fn) {
    function(...) {
        query_params <- list(...)$params |> discard_at('iss.only') %||% list()
        start_init <- query_params$start %||% 0
        query_params <- query_params |> discard_at('start')
        fn_params <- list(...) |> discard_at('params')
        query_iss_fn(...) |>
            imap(\(sec_df, sec_name) {
                sec_list <- list(sec_df)
                cur_pos <- start_init + nrow(sec_df)
                repeat {
                    query_params <- c(
                        query_params,
                        iss.only = sec_name,
                        start = cur_pos
                    )
                    sec_new_df <- do.call(
                        query_iss_fn,
                        c(fn_params, list(params = query_params))
                    )[[sec_name]]
                    if (nrow(sec_new_df) == 0) {
                        break
                    }
                    if (identical(last(sec_list), sec_new_df)) {
                        cli_alert_warning(c(
                            'Received identical sections: ',
                            'the endpoint probably does not support ',
                            '{.arg start} parameter'
                        ))
                        break
                    }
                    cur_pos <- cur_pos + nrow(sec_new_df)
                    sec_list[[length(sec_list) + 1]] <- sec_new_df
                }
                sec_list |>
                    list_rbind()
            })
    }
}


#' A Decorator for Following Response Cursor
#'
#' Iterative queries will be issued to fetch all section pages as indicated by
#' `<section>.cursor`; the cursor section itself will be removed from the
#' response.
#'
#' @param query_iss_fn A function object corresponding to [query_iss].
#' @return Decorated `query_iss_fn`.
#' @examples
#' \dontrun{
#' following_cursor(query_iss)(
#'     'history/engines/stock/markets/shares/securities/MOEX',
#'     params = list(
#'         from = '2021-09-01',
#'         till = '2021-12-31',
#'         start = 10
#'     )
#' )
#' }
#' @export
following_cursor <- function(query_iss_fn) {
    function(...) {
        query_params <- list(...)$params |> discard_at('iss.only') %||% list()
        start_init <- query_params$start %||% 0
        query_params <- query_params |> discard_at('start')
        fn_params <- list(...) |> discard_at('params')
        resp <- query_iss_fn(...)
        resp |>
            imap(\(sec_df, sec_name) {
                # Filter out cursor sections
                if (str_detect(sec_name, '.*\\.cursor')) {
                    return(NULL)
                }
                # Skip sections without cursor
                cursor_df <- resp[[paste0(sec_name, '.cursor')]]
                if (is.null(cursor_df)) {
                    return(sec_df)
                }
                n_pages <- ceiling(cursor_df$TOTAL / cursor_df$PAGESIZE) - 1
                sec_remaining_df <-
                    seq_len(n_pages) |>
                    map_dfr(function(page_n) {
                        query_params <- c(
                            query_params,
                            iss.only = sec_name,
                            start = start_init + page_n * cursor_df$PAGESIZE
                        )
                        do.call(
                            query_iss_fn,
                            c(fn_params, list(params = query_params))
                        )[[sec_name]]
                    })
                bind_rows(sec_df, sec_remaining_df)
            }) |>
            compact()
    }
}
