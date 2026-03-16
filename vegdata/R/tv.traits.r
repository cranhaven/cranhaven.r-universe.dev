tv.eco <- function (...) stop('This function is deprecated, use tv.traits instead.')


#' @title Load species traits from Turboveg reference list
#' @name tv.traits
#' @aliases tv.traits tv.eco
#'
#' @export
#' @param db Path name to the Turboveg database directory
#' @param trait.db Name of species trait dBase file, default is 'ecodbase'
#' @param refl Name of the taxonomic reference list, if veg is not loaded with tv.veg
#' @param \dots additional arguments for tv.traits
#'
#' @description Loading Turboveg ecodbase or any other specified dBase file in this directory and do basic data evaluation. Empty columns are eliminated.
#'
#' @details  You can use the final output line to make a summary statistic for attributes with potentially misleading '0' values.
#'
#' @return data.frame of ecological traits, see \code{metainfo(refl, eco=TRUE)}
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}

tv.traits <- function (db, trait.db = 'ecodbase.dbf', refl, ...) {
    tv_home <- tv.home()
    if(missing(refl))  refl <- if(missing(db)) tax.refl() else tax.refl(db = db)
    if(tools::file_ext(trait.db) != 'dbf') stop('Only implemented for dBas file format.')
    ecodb <- read.dbf(file.path(tv_home, 'Species', refl, trait.db), as.is = TRUE)
    names(ecodb) <- TCS.replace(names(ecodb))
    empty <- function(x) all(is.na(x) | x == 0)
    na <- apply(ecodb, 2, empty)
    if(any(na)) {
        ecodb <- ecodb[, !na]
                }
    ecoDB <- ecodb
    return(ecoDB)
}


