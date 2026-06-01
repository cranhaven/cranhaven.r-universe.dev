#' @title Conversion au format date_ts
#'
#' @description Les fonctions `as_yyyytt` et `as_yyyymm` convertissent une date
#' du format TimeUnits au format `date_ts`.
#'
#' @param timeunits une date en année (Par exemple 2015.25 pour le 2ème
#' trimestre 2015 ou `2021.83333333333` pour novembre 2021)
#'
#' @returns En sortie, ces fonctions retournent la date au format `date_ts`
#' (c'est-à-dire un vecteur d'entiers de la forme `AAAA`, `c(AAAA, MM)` ou
#' `c(AAAA, TT)`)
#' @details
#' La fonction `as_yyyytt` retourne la date par trimestre et la fonction
#' `as_yyyymm` retourne la date par mois.
#'
#' @export
#'
#' @examples
#' as_yyyytt(2019.75) # 4ème trimestre 2019
#' as_yyyytt(2020) # 1er trimestre 2020
#' as_yyyytt(2022 + 1 / 4) # 2ème trimestre 2022
#'
#' as_yyyymm(2019.75) # Octobre 2019
#' as_yyyymm(2020) # Janvier 2020
#' as_yyyymm(2020 + 1 / 12) # Février 2020
#' as_yyyymm(2020 + 12 / 12) # Janvier 2021
#'
as_yyyytt <- function(timeunits) {
    # Check de l'objet TimeUnits
    assert_timeunits(timeunits, frequency_ts = 4L, .var.name = "timeunits")

    return(as.integer(round(c(timeunits %/% 1L, (timeunits %% 1L) * 4L + 1L))))
}

#' @name as_yyyytt
#'
#' @export
#'
as_yyyymm <- function(timeunits) {
    # Check de l'objet TimeUnits
    assert_timeunits(timeunits, frequency_ts = 12L, .var.name = "timeunits")

    return(as.integer(round(c(timeunits %/% 1L, (timeunits %% 1L) * 12L + 1L))))
}

#' @title Conversion entre date mensuelle et trimestrielle
#'
#' @description Les fonctions `trim2mens` et `mens2trim` convertissent une
#' `date_ts` du format mensuelle `c(AAAA, MM)` au format trimestrielle `c(AAAA,
#' TT)`.
#'
#' @param date_ts un vecteur numérique, de préférence `integer`, au format
#' `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#'
#' @returns En sortie, la fonction retourne la date toujours au format `date_ts`.
#' @export
#'
#' @examples
#' trim2mens(c(2019L, 4L)) # 4ème trimestre 2019 --> Octobre 2019
#' trim2mens(c(2020L, 1L)) # 1er trimestre 2020 --> Janvier 2020
#'
#' mens2trim(c(2019L, 4L)) # Avril 2019 --> 2ème trimestre 2019
#' mens2trim(c(2020L, 11L)) # Novembre 2020 --> 4ème trimestre 2020
#'
trim2mens <- function(date_ts) {
    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency_ts = 4L,
                              .var.name = "date_ts")

    year <- date_ts[1L] + (date_ts[2L] - 1L) %/% 4L
    trim <- (date_ts[2L] - 1L) %% 4L + 1L
    return(as.integer(c(year, trim * 3L - 2L)))
}

#' @name trim2mens
#'
#' @export
#'
mens2trim <- function(date_ts) {
    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency_ts = 12L,
                              .var.name = "date_ts")

    year <- date_ts[1L] + (date_ts[2L] - 1L) %/% 12L
    month <- (date_ts[2L] - 1L) %% 12L + 1L
    return(as.integer(c(year, 1L + ((month - 1L) %/% 3L))))
}

#' @title Conversion d'une date du format date_ts au format TimeUnits
#'
#' @param date_ts un vecteur numérique, de préférence `integer` au format
#' `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param frequency_ts un entier qui vaut `4L` (ou `4.0`) pour les séries
#' trimestrielles et `12L` (ou `12.0`) pour les séries mensuelles.
#'
#' @returns En sortie, la fonction retourne la date au format `AAAA + TT/4` ou
#' `AAAA + MM/12` (un numeric de longueur 1).
#'
#' @details
#' `AAAA` signifie que l'année est au format numérique avec 4 chiffres
#' (Exemple : l'année deux mille vingt-deux s'écrit 2022 et non 22)
#' MM signifie que le mois est au format numérique (Exemple : le mois de mai
#' s'écrit 5, le moi de décembre s'écrit 12)
#' TT signifie que le trimestre est au format numérique (Exemple : le troisième
#' trimestre s'écrit 3)
#' @export
#'
#' @examples
#'
#' # Avril 2020
#' date_ts2timeunits(date_ts = c(2020L, 4L), frequency_ts = 12L)
#' # Novembre 2020
#' date_ts2timeunits(date_ts = c(2022L, 11L), frequency_ts = 12L)
#'
#' # 4ème trimestre de 2022
#' date_ts2timeunits(date_ts = c(2022, 4L), frequency_ts = 4L)
#' # 2ème trimestre de 1995
#' date_ts2timeunits(date_ts = c(1995L, 2L), frequency_ts = 4L)
#'
date_ts2timeunits <- function(date_ts, frequency_ts) {

    # Check de la fréquence
    frequency_ts <- assert_frequency(frequency_ts, add = NULL,
                                     .var.name = "frequency_ts")
    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency_ts  = frequency_ts,
                              add = NULL, .var.name = "date_ts")

    # date_ts must be of size 2 else:
    #   - an error would have been raised
    #   - or date_ts would have been modified

    return(date_ts[1L] + (date_ts[2L] - 1) / frequency_ts)
}

#' @title Conversion d'une date au format TS
#'
#' @description La fonction `date2date_ts` prend en argument une date au format
#' date (integer avec une class Date) et la convertit au format `date_ts` :
#' `c(AAAA, MM)` ou `c(AAAA, TT)` avec le mois ou trimestre en cours.
#'
#' @param date un objet de type Date
#' @param frequency_ts un entier qui vaut `4L` (ou `4.0`) pour les séries
#' trimestrielles et `12L` (ou `12.0`) pour les séries mensuelles.
#'
#' @returns En sortie, la fonction retourne la date au format `date_ts` (`c(AAAA,
#' MM)` ou `c(AAAA, TT)`) avec le mois ou trimestre en cours selon l'argument
#' `frequency_ts`.
#' @export
#'
#' @examples
#'
#' date2date_ts(as.Date("2000-01-01"))
#' date2date_ts(as.Date("2000-01-01"), frequency_ts = 12L)
#'
#' date2date_ts(as.Date("2021-10-01"), frequency_ts = 12L)
#' date2date_ts(as.Date("2021-10-01"), frequency_ts = 4L)
#'
date2date_ts <- function(date, frequency_ts = 12L) {

    coll <- checkmate::makeAssertCollection()

    # Check de l'objet date
    assert_scalar_date(date, add = coll, .var.name = "date")
    # Check de la fréquence
    frequency_ts <- assert_frequency(frequency_ts, add = coll,
                                     .var.name = "frequency_ts")

    checkmate::reportAssertions(coll)

    year <- as.integer(format(date, format = "%Y"))
    month <- as.integer(format(date, format = "%m"))

    if (frequency_ts == 4L) {
        month <- 1L + ((month - 1L) %/% 3L)
    }

    return(c(year, month))
}

#' @title Retire une année à une date
#'
#' @param date un objet de type Date
#' @param n un entier
#'
#' @description La fonction `substr_year` retire `n` annnée(s) à une date.
#' @returns En sortie, la fonction retourne un objet de type Date (atomic) de
#' longueur 1.
#' @export
#'
#' @examples
#'
#' substr_year(as.Date("2000-02-29"), n = 1L)
#' substr_year(as.Date("2000-02-29"), n = 3L)
#' substr_year(as.Date("2000-02-29"), n = 4L)
#' substr_year(as.Date("2000-02-29"), n = 16L)
#'
#' substr_year(as.Date("2023-01-25"), n = 10L)
#' substr_year(as.Date("2022-11-01"), n = 3L)
#'
substr_year <- function(date, n = 1L) {

    coll <- checkmate::makeAssertCollection()

    assert_scalar_date(date, add = coll, .var.name = "date")
    n <- assert_scalar_natural(n, add = coll, .var.name = "n")

    checkmate::reportAssertions(coll)

    date_1 <- as.Date(paste("2000", format(date, format = "%m-%d"), sep = "-"))
    date_2 <- as.Date("2000-02-28")
    before_leap <- date_1 <= date_2

    year <- as.integer(format(date, format = "%Y")) - before_leap
    years <- year:(year - n + 1L)
    leap_year <- sum(as.logical((years %% 4 == 0L) -
                                    (years %% 100 == 0L) +
                                    (years %% 400 == 0L))
    )

    return(date - 365 * n - leap_year)
}

#' @title Conversion d'une date du format TS au format date
#'
#' @param date_ts un vecteur numérique, de préférence `integer`, au format
#' `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param frequency_ts un entier qui vaut `4L` (ou `4.0`) pour les séries
#' trimestrielles et `12L` (ou `12.0`) pour les séries mensuelles.
#'
#' @returns En sortie, la fonction retourne un objet de type Date (atomic) de
#' longueur 1 qui correspond à l'objet `date_ts`.
#' @export
#'
#' @examples
#'
#' date_ts2date(date_ts = c(2020L, 11L), frequency_ts = 12L)
#' date_ts2date(date_ts = c(1995L, 2L), frequency_ts = 4L)
#'
date_ts2date <- function(date_ts, frequency_ts) {

    coll <- checkmate::makeAssertCollection()

    # Check de la fréquence
    frequency_ts <- assert_frequency(frequency_ts, add = coll,
                                     .var.name = "frequency_ts")
    # Check du format date_ts
    if (isTRUE(check_frequency(frequency_ts, warn = FALSE))) {
        date_ts <- assert_date_ts(x = date_ts, frequency_ts, add = coll,
                                  .var.name = "date_ts")
    }

    checkmate::reportAssertions(coll)

    year <- date_ts[1L]

    if (length(date_ts) == 2L) {
        if (frequency_ts == 4L) {
            month <- sprintf("%02d", date_ts[2L] * 3L - 2L)
        } else if (frequency_ts == 12L) {
            month <- sprintf("%02d", date_ts[2L])
        }
    }

    if (year < 0L) {
        return(substr_year(
            date = as.Date(paste("0000", month, "01", sep = "-")),
            n = -year
        ))
    } else {
        return(as.Date(paste(year, month, "01", sep = "-")))
    }
}

#' @keywords internal
ts2df <- function(x) {

    # Check de l'objet x
    assert_ts(x, .var.name = "x", allow_mts = TRUE)

    if (stats::is.mts(x)) {
        length_series <- nrow(x)
    } else {
        length_series <- length(x)
    }

    frequency_ts <- as.integer(stats::frequency(x))
    first_timeunits <- stats::time(x)[1L]

    if (frequency_ts == 12L) {
        first_date_ts <- as_yyyymm(first_timeunits)
    } else if (frequency_ts == 4L) {
        first_date_ts <- as_yyyymm(first_timeunits)
    }

    rownames_libelles <- libelles(date_ts = first_date_ts, frequency_ts = frequency_ts, n = length_series)
    x <- data.frame(date = rownames_libelles, x)

    return(x)
}
