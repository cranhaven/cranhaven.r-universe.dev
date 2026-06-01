#' @title Récupère des valeurs d'un ts
#'
#' @description La fonction `get_value_ts` permet de récupérer des valeurs.
#'
#' @param series un objet ts unidimensionnel conforme aux règles de assert_ts
#' @param date_from un vecteur numérique, de préférence `integer` au format
#' `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param date_to un vecteur numérique, de préférence `integer` au format
#' `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param n un entier
#'
#' @returns En sortie, la fonction retourne un vecteur (atomic) de même type que
#' `series` avec les valeurs extraites.
#'
#' @details
#' Il faut qu'exactement 2 arguments parmi `date_to`, `date_to` et `n` soient
#' renseignés. L'argument `n` combiné avec `date_to` ou `date_from` permet de
#' déterminer combien de période seront retourné à partir de ou jusqu'à la date
#' renseignée.
#'
#' @export
#'
#' @examples
#'
#' ts1 <- ts(1:100, start = 2012L, frequency = 12L)
#' ts2 <- ts(letters, start = 2014L, frequency = 4L)
#' ts3 <- ts(exp(-(1:50)), start = 2015L, frequency = 12L)
#'
#' get_value_ts(series = ts1, date_from = c(2015L, 7L), date_to = c(2018L, 6L))
#' get_value_ts(series = ts2, date_from = c(2018L, 4L), n = 4L)
#' get_value_ts(series = ts3, date_to = c(2018L, 4L), n = 14L)
#'
get_value_ts <- function(series, date_from, date_to, n) {

    coll <- NULL

    # Check de l'objet series
    assert_ts(series, add = coll, .var.name = "series")

    frequency_ts <- as.integer(stats::frequency(series))

    if ((missing(date_to) + missing(date_from) + missing(n)) != 1L) {
        stop(c("Exactement 2 des arguments `date_from`, `date_to` et ",
               "`n` doivent \u00eatre renseign\u00e9s et un manquant."))
    } else if (missing(date_from)) {
        # Check l'argument n
        n <- assert_scalar_natural(n, add = coll, .var.name = "n")
        # Check du format date_ts
        date_to <- assert_date_ts(
            x = date_to, frequency_ts = frequency_ts,
            .var.name = "date_from"
        )

        date_from <- previous_date_ts(
            date_ts = date_to,
            frequency_ts = frequency_ts, lag = n - 1L
        )
    } else if (missing(date_to)) {
        # Check l'argument n
        n <- assert_scalar_natural(n, add = coll, .var.name = "n")
        # Check du format date_ts
        date_from <- assert_date_ts(
            x = date_from, frequency_ts = frequency_ts,
            .var.name = "date_from"
        )

        date_to <- next_date_ts(
            date_ts = date_from,
            frequency_ts = frequency_ts, lag = n - 1L
        )
    } else if (missing(n)) {
        # Check du format date_ts
        date_from <- assert_date_ts(
            x = date_from, frequency_ts = frequency_ts,
            .var.name = "date_from"
        )
        # Check du format date_ts
        date_to <- assert_date_ts(
            x = date_to, frequency_ts = frequency_ts,
            .var.name = "date_from"
        )
        checkmate::assert_true(is_before(
            a = date_from, b = date_to,
            frequency_ts = frequency_ts, strict = FALSE
        ))
    }

    end_ts <- normalize_date_ts(
        date_ts = stats::end(series),
        frequency_ts = frequency_ts,
        test = FALSE
    )
    start_ts <- normalize_date_ts(
        date_ts = stats::start(series),
        frequency_ts = frequency_ts,
        test = FALSE
    )

    output_value <- series

    if (is.raw(series)) {

        if (is_before(a = date_to,
                      b = start_ts,
                      frequency_ts = frequency_ts,
                      strict = TRUE) || is_before(a = end_ts,
                                                  b = date_from,
                                                  frequency_ts = frequency_ts,
                                                  strict = TRUE)) {
            return(rep(x = as.raw(0x00), times = diff_periode(
                a = date_to,
                b = date_from,
                frequency_ts = frequency_ts
            )))
        }

        if (is_before(a = date_to,
                      b = end_ts,
                      strict = FALSE,
                      frequency_ts = frequency_ts)) {
            output_value <- stats::window(
                x = output_value,
                end = date_to
            )
            after <- 0L
        } else {
            after <- diff_periode(
                a = date_to,
                b = end_ts,
                frequency_ts = frequency_ts
            ) - 1L
        }

        if (is_before(a = start_ts,
                      b = date_from,
                      strict = FALSE,
                      frequency_ts = frequency_ts)) {
            output_value <- stats::window(
                x = output_value,
                start = date_from
            )
            before <- 0L
        } else {
            before <- diff_periode(
                a = start_ts,
                b = date_from,
                frequency_ts = frequency_ts
            ) - 1L
        }

        attributes(output_value) <- NULL
        output_value <- c(rep(x = as.raw(0x00), times = before),
                          output_value,
                          rep(x = as.raw(0x00), times = after))

    } else {
        output_value <- stats::window(
            x = output_value,
            start = date_from,
            end = date_to, extend = TRUE
        )
        attributes(output_value) <- NULL
    }


    return(output_value)
}
