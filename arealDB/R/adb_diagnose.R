#' Diagnose databse contents
#'
#' work in progress, not yet useable
#' @param territory description
#' @param concept description
#' @param variable description
#' @param level description
#' @param year description
#' @importFrom checkmate assertList assertCharacter assertIntegerish
#' @export

adb_diagnose <- function(territory = NULL, concept = NULL, variable = NULL,
                         level = NULL, year = NULL){

  # # territory <- list(al1 = "Brazil"); concept = c(animal = "cattle"); variable = "number_headcount"; level = "al3"; year = 2010
  #
  # assertList(x = territory, types = "character", any.missing = FALSE, null.ok = TRUE)
  # assertList(x = concept, types = "character", any.missing = FALSE, null.ok = TRUE)
  # assertCharacter(x = variable, len = 1, any.missing = FALSE, null.ok = TRUE)
  # assertCharacter(x = level, len = 1, any.missing = FALSE, null.ok = TRUE)
  # assertIntegerish(x = year, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  #
  #
  # diag <- list()
  #
  # # visually diagnose some missing data
  # diag$gazID <- table |>
  #   filter(is.na(gazID)) |>
  #   distinct(gazMatch)
  # diag$ontoID <- table |>
  #   filter(is.na(ontoID)) |>
  #   distinct(ontoMatch)


}
