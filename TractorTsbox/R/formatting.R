#' @title Ajuste un objet date_ts dans un format conforme.
#'
#' @param date_ts un vecteur numérique, de préférence `integer` au format
#' `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param frequency_ts un entier qui vaut `4L` (ou `4.0`) pour les séries
#' trimestrielles et `12L` (ou `12.0`) pour les séries mensuelles.
#' @param test un booléen (Default is TRUE)
#'
#' @returns En sortie, la fonction retourne une date au même format que l'objet
#' `date_ts` avec la période inclus entre 1 et la fréquence.
#' @details Ici le formattage correspond à une réécriture de la date sans
#' en changer la valeur. Alors que l'objet c(2020L, 12L) désigne le mois de
#' décembre 2020 et c(2021L, 1L) le mois de janvier 2021, on peut imaginer que
#' la date_ts c(2021L, 0L) peut aussi représenter le mois de décembre 2020.
#' Si l'argument `test` est mis à FALSE, alors aucun test ne sera effectué sur
#' les données en entrée.
#' @export
#'
#' @examples
#'
#' # Formattage inchangée
#' normalize_date_ts(c(2020L, 1L), frequency_ts = 4L) # 1er trimestre de 2020
#' normalize_date_ts(c(2020L, 8L), frequency_ts = 12L) # Aout 2020
#'
#' # Retour dans le passé
#' normalize_date_ts(c(2020L, 0L), frequency_ts = 4L) # 4ème trimestre de 2019
#' normalize_date_ts(c(2020L, -10L), frequency_ts = 12L) # février 2019
#'
#' # Avancée dans le futur
#' normalize_date_ts(c(2020L, 7L), frequency_ts = 4L) # 3ème trimestre de 2021
#' normalize_date_ts(c(2020L, 13L), frequency_ts = 4L) # janvier 2021
#'
normalize_date_ts <- function(date_ts, frequency_ts, test = TRUE) {

    if (test) {

        coll <- checkmate::makeAssertCollection()

        # Check de la fréquence
        frequency_ts <- assert_frequency(frequency_ts, add = coll,
                                         .var.name = "frequency_ts")
        # Check du format date_ts
        if (isTRUE(check_frequency(frequency_ts, warn = FALSE))) {
            date_ts <- assert_date_ts(x = date_ts, frequency_ts,
                                      add = coll, .var.name = "date_ts")
        }

        checkmate::reportAssertions(coll)
    }

    date_ts <- as.integer(date_ts)

    if (length(date_ts) == 2L) {
        year <- date_ts[1L]
        period <- date_ts[2L]
        return(c(
            year + ((period - 1L) %/% frequency_ts),
            1L + ((period - 1L) %% frequency_ts)
        ))
    } else {
        return(c(date_ts, 1L))
    }
}
