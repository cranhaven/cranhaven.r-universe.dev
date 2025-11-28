#' Maximum yearly temperature data from 1901 to 2022 (CRU TS v4)
#'
#'Maximum yearly temperature data from 1901 to 2022 in 11 countries in Southeast Asia and the average temperature of the
#'entire region extracted from  Climatic Research Unit gridded Time Series Version 4.
#'Data contains only temperatures for Malaysia, Thailand, Vietnam and the average regional temperature.
#' @format ## `SEAex`
#' A data frame with 122 rows and 4 columns:
#' \describe{
#'   \item{Malaysia,Thailand,Vietnam}{Yearly max temperatures in Celsius for each country over 122 years.}
#'   \item{avgRegion}{Average temperature in Celsius over the whole South East Asia region}
#' }
#' @examples
#'
#' data(SEAex)
#' head(SEAex)
#' @keywords datasets
#' @source <https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/crucy.2304181636.v4.07/countries/>
"SEAex"
