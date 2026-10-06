#' @title Mexican income data 2008
#'
#' @description Selection of Mexican income (survey) data and household characteristic for 2008.
#' Extracted from ENIGH (Household Income and Expenditure Survey).
#'
#' @details This data set is a selecion of the original dataset of the National Institute of
#' Statistics and Geography in Mexico (INEGI). The original contains 29468 observations and
#' 129 variables with information on the income and household characteristics in Mexico. This
#' selection is only meant to be used as a calculation example for the functions in this
#' package. Results will not represent the correct information on the Mexican situation.
#'
#' @format A data frame containing 5000 observations and 8 variables (a selection from the original).
#'  \describe{
#'   \item{hh_number}{Household ID.}
#'   \item{factor}{Population inflating weights.}
#'   \item{income}{Household income.}
#'   \item{hh_structure}{Household structure, factor with levels unipersonal, nuclear,
#'   ampliado, compuesto and coresidente.}
#'   \item{education}{Highest achieved education of the head of the household, factor with levels
#'   Sin instruccion, Preescolar, Primaria incompleta, Primaria completa, Secundaria incompleta,
#'   Secundaria completa, Preparatoria incompleta, Preparatoria completa, Profesional incompleta,
#'   Profesional completa, Posgrado.}
#'   \item{domicile_size}{Population of domicile, factor with levels <2500, 2500-15000,
#'   15000-100000, >100000.}
#'   \item{age}{age (integer) of the head of the household.}
#'   \item{age_cat}{age (categorical) of the head of the household , factor with levels <25,
#'   25-34, 35-44, 45-54, 55-64, 65-74, >=75.}
#' }
#'
#' @usage data(mex_inc_2008)
#'
#' @references INEGI (2009), \emph{ENIGH 2008 Nueva construcción. Ingresos y gastos de los hogares},
#' Aguascalientes: INEGI.
#'
#' @source \url{http://en.www.inegi.org.mx/proyectos/enchogares/regulares/enigh/nc/2008/default.html},
#' the whole data set can be obtained here.

"mex_inc_2008"



