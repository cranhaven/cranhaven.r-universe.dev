#' Corn and wheat prices from 1986 to 2014
#'
#' The to prices (in U.S. Dollars) per bushel and the log returns of corn and wheat from 1986-01-03 to 2014-10-10.
#' Each observation corresponds to the price on that day, but not all days are present in this dataset.
#'
#' @format
#' A data frame with 7,253 rows and 5 columns:
#' \describe{
#'   \item{date}{The date of the observation.}
#'   \item{corn.price, wheat.price}{ The price (in U.S. Dollars) per bushel of corn and wheat, respectively.}
#'   \item{corn.log.return, wheat.log.return }{The log returns for corn and wheat, respectively.}
#' }
#' @source \url{https://www.macrotrends.net/charts/commodities}
"cornWheat"

#' SARI data from Belo Horizonte
#'
#' A dataset containing reports from Severe Acute Respiratory Illness (SARI) from 2020 to April 2022 by week.
#'
#' @format A data frame with 65404 rows and 7 variables:
#' \describe{
#'   \item{ref.week}{The reference week, counting since the monitoring begun.}
#'   \item{reported.1.week}{The number of cases occurred in the period and reported until the 1 week after the reference week.}
#'   \item{reported.2.week}{The number of cases occurred in the period and reported until the 2 weeks after the reference week.}
#'   \item{reported.4.week}{The number of cases occurred in the period and reported until the 4 weeks after the reference week.}
#'   \item{reported.6.week}{The number of cases occurred in the period and reported until the 6 weeks after the reference week.}
#'   \item{reported.8.week}{The number of cases occurred in the period and reported until the 8 weeks after the reference week.}
#'   \item{reported.12.week}{The number of cases occurred in the period and reported until the 12 weeks after the reference week.}
#'   \item{occured}{The total number of cases reported (at any time).}
#'   ...
#' }
#' @source \url{https://datasus.saude.gov.br/informacoes-de-saude-tabnet/}
"noticeSARI"

#' Hospital admissions from gastroenteritis in Brazil
#'
#' A dataset containing the number of Hospital admissions from gastroenteritis in Brazil, per state, from 2010 to 2022 by month.
#'
#' @format A data frame with 4212 rows and 4 variables:
#' \describe{
#'   \item{UF}{The abbreviated state name.}
#'   \item{Date}{The date of the observation. Note that the day is only a placeholder and is just a placeholder.}
#'   \item{Admissions}{The number hospital admissions.}
#'   \item{Population}{The estimated population.}
#' }
#' @source Admissions: \url{https://datasus.saude.gov.br/informacoes-de-saude-tabnet/}
#' @source Population: \url{https://www.ibge.gov.br/estatisticas/sociais/populacao.html}
"gastroBR"


#' Hospital admissions by chicken pox in Brazil
#'
#' Monthly hospital admissions by chicken pox in Brazil from January 2010 to December 2019.
#'
#' @format
#' A data frame with 120 rows and 6 columns:
#' \describe{
#'   \item{date}{The date of the observations.}
#'   \item{< 5 year, 5 to 9 years, 10 to 14 years, 15 to 49 years, 50 years or more}{The number of admissions for each age group.}
#' }
#' @source \url{https://datasus.saude.gov.br/informacoes-de-saude-tabnet/}
"chickenPox"
