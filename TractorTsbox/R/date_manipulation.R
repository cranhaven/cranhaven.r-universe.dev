#' @title Obtenir la date précédente
#'
#' @param date_ts un vecteur numérique, de préférence `integer` au format
#' `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param frequency_ts un entier qui vaut `4L` (ou `4.0`) pour les séries
#' trimestrielles et `12L` (ou `12.0`) pour les séries mensuelles.
#' @param lag un entier
#'
#' @returns En sortie, la fonction retourne un vecteur d'entier qui représente la
#' date à la période passée au format `date_ts`.
#'
#' @details Lorsqu'on parle de date précédente, on parle de date passée.
#' L'argument `lag` est entier et désigne le nombre de décalage que l'on affecte
#' à notre date.
#' Par exemple pour des lag positif (1L, 2L, 10L) on désigne le décalage de la
#' période précédente, celle d'avant et celle d'il y a 10 périodes.
#' Cependant, lorsque l'argument `lag` vaut zéro, la fonction retourne la
#' `date` inchangée. Aussi lorsque l'argument `lag` est négatif, la fonction se
#' comporte comme la fonction `next_date_ts` et retourne les périodes futures et
#' non passées.
#' @export
#'
#' @seealso `next_date_ts`
#'
#' @examples
#'
#' previous_date_ts(c(2020L, 4L), frequency_ts = 4L, lag = 2L)
#' previous_date_ts(c(2021L, 1L), frequency_ts = 4L, lag = -2L)
#'
#' previous_date_ts(c(2020L, 4L), frequency_ts = 12L, lag = 2L)
#' previous_date_ts(c(2022L, 6L), frequency_ts = 12L, lag = 12L)
previous_date_ts <- function(date_ts, frequency_ts, lag = 1L) {

    coll <- checkmate::makeAssertCollection()

    # Check de la fréquence
    frequency_ts <- assert_frequency(frequency_ts, add = coll,
                                     .var.name = "frequency_ts")

    # Check du format date_ts
    if (isTRUE(check_frequency(frequency_ts, warn = FALSE))) {
        date_ts <- assert_date_ts(x = date_ts, frequency_ts, add = coll,
                                  .var.name = "date_ts")
    }

    # Check l'argument lag
    lag <- assert_scalar_integer(lag, add = coll, .var.name = "lag")

    checkmate::reportAssertions(coll)

    year <- date_ts[1L]
    period <- date_ts[2L] - lag

    return(normalize_date_ts(
        date_ts = c(year, period),
        frequency_ts = frequency_ts,
        test = FALSE
    ))
}

#' @title Obtenir la date suivante
#'
#' @inheritParams previous_date_ts
#'
#' @returns En sortie, la fonction retourne un vecteur d'entier qui représente la
#' date à la période future au format `date_ts`.
#'
#' @details Lorsqu'on parle de date suivante, on parle de date future.
#' L'argument `lag` est entier et désigne le nombre de décalage que l'on affecte
#' à notre date.
#' Par exemple pour des lag positif (1L, 2L, 10L) on désigne le décalage de la
#' période suivante, celle d'après et celle dans 10 périodes.
#' Cependant, lorsque l'argument `lag` vaut zéro, la fonction retourne la
#' `date` inchangée. Aussi lorsque l'argument `lag` est négatif, la fonction
#' se comporte comme la fonction `previous_date_ts` et retourne les périodes
#' passées et non futures.
#' @export
#'
#' @seealso `previous_date_ts`
#'
#' @examples
#'
#' next_date_ts(c(2020L, 4L), frequency_ts = 4L, lag = 2L)
#' next_date_ts(c(2021L, 1L), frequency_ts = 4L, lag = -2L)
#'
#' next_date_ts(c(2020L, 4L), frequency_ts = 12L, lag = 2L)
#' next_date_ts(c(2022L, 6L), frequency_ts = 12L, lag = 12L)
next_date_ts <- function(date_ts, frequency_ts, lag = 1L) {

    coll <- checkmate::makeAssertCollection()

    # Check de la fréquence
    frequency_ts <- assert_frequency(frequency_ts, add = coll,
                                     .var.name = "frequency_ts")

    # Check du format date_ts
    if (isTRUE(check_frequency(frequency_ts, warn = FALSE))) {
        date_ts <- assert_date_ts(x = date_ts, frequency_ts, add = coll,
                                  .var.name = "date_ts")
    }

    # Check l'argument lag
    lag <- assert_scalar_integer(x = lag, add = coll, .var.name = "lag")

    checkmate::reportAssertions(coll)

    year <- date_ts[1L]
    period <- date_ts[2L] + lag

    return(normalize_date_ts(
        date_ts = c(year, period),
        frequency_ts = frequency_ts,
        test = FALSE
    ))
}

#' @title Première date non NA
#'
#' @description Cette fonction calcule la première date pour laquelle l'objet
#' `series` ne vaut pas NA.
#'
#' @param series un objet ts unidimensionnel conforme aux règles de assert_ts
#'
#' @returns En sortie, la fonction retourne un objet au format `date_ts` (`AAAA`,
#' `c(AAAA, MM)` ou `c(AAAA, TT)`)
#'
#' @details La date retournée en output est au format `date_ts`. Si l'objet
#' `series` ne contient que des NAs, la fonction retourne une erreur.
#'
#' @export
#'
#' @seealso `last_date`
#'
#' @examples
#'
#' ts1 <- ts(c(NA, NA, NA, 1:10, NA), start = 2000, frequency = 12)
#' ts2 <- ts(c(1:10, NA), start = 2020, frequency = 4)
#'
#' stats::start(ts1)
#' first_date(ts1)
#'
#' stats::start(ts1)
#' first_date(ts2)
#'
first_date <- function(series) {
    # Check de l'objet series
    assert_ts(series, .var.name = "series")

    time_ts <- stats::time(na_trim(series))
    first_time <- time_ts[1L]
    frequency_ts <- as.integer(stats::frequency(series))
    return(c(first_time %/% 1L, (first_time %% 1L) * frequency_ts + 1L))
}

#' @title Dernière date non NA
#'
#' @description Cette fonction calcule la dernière date pour laquelle l'objet
#' `series` ne vaut pas NA.
#'
#' @inheritParams first_date
#'
#' @returns En sortie, la fonction retourne un objet au format `date_ts` (`AAAA`,
#' `c(AAAA, MM)` ou `c(AAAA, TT)`)
#'
#' @details La date retournée en output est au format `date_ts`. Si l'objet
#' `series` ne contient que des NAs, la fonction retourne une erreur.
#'
#' @export
#'
#' @seealso `first_date`
#'
#' @examples
#'
#' ts1 <- ts(c(NA, NA, NA, 1:10, NA), start = 2000, frequency = 12)
#' ts2 <- ts(c(1:10), start = 2020, frequency = 4)
#'
#' stats::end(ts1)
#' last_date(ts1)
#'
#' stats::end(ts1)
#' last_date(ts2)
#'
last_date <- function(series) {
    # Check de l'objet series
    assert_ts(series, .var.name = "series")

    time_ts <- stats::time(na_trim(series))
    last_time <- time_ts[length(time_ts)]
    frequency_ts <- as.integer(stats::frequency(series))

    if (frequency_ts == 12L) {
        return(as_yyyymm(last_time))
    } else if (frequency_ts == 4L) {
        return(as_yyyytt(last_time))
    }
}

#' @title Comparaison de 2 date_ts
#'
#' @inheritParams diff_periode
#' @param strict un booleen (default `FALSE`)
#'
#' @returns En sortie, la fonction retourne un booleen (de longueur 1) qui
#' indique si la date `a` est antérieure à la date `b`.
#'
#' @details Les dates `a` et `b` sont au format date_ts. L'argument frequency_ts
#' est nécessaire pour interpréter les dates.
#' Ainsi, si je souhaite comparer la date `a = c(2023L, 4L)` et la date `b
#' = c(2023L, -2L)`. Dans le cas d'une fréquence mensuelle, la date `a` est
#' antérieure à la date `b`. Dans le cas d'une fréquence mensuelle, c'est
#' l'inverse. Si `strict` vaut `TRUE`, la fonction compare strictement les dates
#' `a` et `b` (`<`).
#' @export
#'
#' @examples
#'
#' is_before(a = c(2020L, 3L), b = c(2022L, 4L), frequency_ts = 12L)
#' is_before(a = c(2022L, 3L), b = c(2010L, 1L), frequency_ts = 4L)
#'
#' is_before(a = c(2022L, 4L), b = c(2022L, 4L), frequency_ts = 12L)
#' is_before(a = c(2022L, 4L), b = c(2022L, 4L),
#'     frequency_ts = 12L, strict = TRUE)
#'
#' # Importance de la fréquence
#' is_before(a = c(2022L, -3L), b = c(2021L, 8L), frequency_ts = 12L)
#' is_before(a = c(2022L, -3L), b = c(2021L, 8L), frequency_ts = 4L)
#'
is_before <- function(a, b, frequency_ts, strict = FALSE) {

    coll <- checkmate::makeAssertCollection()

    # Check de strict
    checkmate::assert_flag(strict, add = coll, .var.name = "replace_na")

    # Check de la fréquence
    frequency_ts <- assert_frequency(frequency_ts, add = coll,
                                     .var.name = "frequency_ts")

    if (isTRUE(check_frequency(frequency_ts, warn = FALSE))) {
        # Check du format date_ts a
        a <- assert_date_ts(x = a, frequency_ts, add = coll, .var.name = "a")

        # Check du format date_ts b
        b <- assert_date_ts(x = b, frequency_ts, add = coll, .var.name = "b")
    }

    checkmate::reportAssertions(coll)

    tu_a <- date_ts2timeunits(a, frequency_ts = frequency_ts)
    tu_b <- date_ts2timeunits(b, frequency_ts = frequency_ts)

    if (strict) {
        return(tu_a < tu_b)
    } else {
        return(tu_a <= tu_b)
    }
}


#' @title Intervalle entre 2 dates
#'
#' @param a un objet date_ts, c'est-à-dire un vecteur numérique, de préférence
#' `integer` au format `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param b un objet date_ts, c'est-à-dire un vecteur numérique, de préférence
#' `integer` au format `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param frequency_ts un entier qui vaut `4L` (ou `4.0`) pour les séries
#' trimestrielles et `12L` (ou `12.0`) pour les séries mensuelles.
#'
#' @returns En sortie, la fonction retourne un entier qui désigne le nombre de
#' période (mois ou trimestres) qui sépare les 2 dates `a` et `b`.
#'
#' @details On travaille ici avec des dates au format date_ts, c'est-à-dire qui
#' passe le test de la fonction `assert_date_ts`. Lorsqu'on parle d'intervalle
#' et de nombre de période entre `a` et `b`, les bornes sont incluses. Ainsi
#' `diff_periode(2020L, 2020L, 12L)` retourne bien 1L et non 2L ou 0L.
#'
#' @export
#'
#' @examples
#' # Une seule période
#' diff_periode(a = 2020L, b = 2020L, frequency_ts = 4L)
#'
#' diff_periode(a = c(2000L, 1L), b = c(2020L, 4L), frequency_ts = 4L)
#'
#' # Ordre chronologique respecté
#' diff_periode(a = c(2021L, 5L), b = c(2023L, 8L), frequency_ts = 12L)
#'
#' # Date inversées
#' diff_periode(a = c(2023L, 8L), b = c(2021L, 5L), frequency_ts = 12L)
#'
diff_periode <- function(a, b, frequency_ts) {

    coll <- NULL

    # Check de la fréquence
    frequency_ts <- assert_frequency(frequency_ts, add = coll,
                                     .var.name = "frequency_ts")
    # Check du format date_ts a
    a <- assert_date_ts(x = a, frequency_ts, add = coll, .var.name = "a")
    # Check du format date_ts b
    b <- assert_date_ts(x = b, frequency_ts, add = coll, .var.name = "b")

    if (!is_before(a, b, frequency_ts)) {
        return(diff_periode(b, a, frequency_ts))
    }
    if (length(a) == 1L) {
        a <- c(a, 1L)
    }
    if (length(b) == 1L) {
        b <- c(b, 1L)
    }
    return((b[1L] - a[1L]) * frequency_ts + b[2L] - a[2L] + 1L)
}
