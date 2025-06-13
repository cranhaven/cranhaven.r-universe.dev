#' Biomass estimative
#'
#' @description
#' The function estimates biomass through biovolume data and applies different cell density values as conversion methods.
#' See details \sQuote{Details}:
#'
#' @usage biomass(biovolume, method = "michaels")
#' @param biovolume numeric value, object or data.frame with cell living volume values.
#' @param method The methods of conversion from biovolume to biomass are listed below, default is \code{'michaels'}:
#' \itemize{
#'   \item \code{'saidova'} : adopted cell density of 1.027 g/cm3;
#'   \item \code{'strathmann'} : measured cell density of 0.110 pgC[org]/um3;
#'   \item \code{'turley'} : estimated cell density of 0.132 pgC[org]/um3;
#'   \item \code{'putt'} : estimated cell density of 0.140 pgC[org]/um3;
#'   \item \code{'gerlach'} : adopted cell density of 1.13 g/cm3 wet mass, assuming 10 percent as living organic carbon;
#'   \item \code{'michaels'} : calculated cell density of 0.089 pgC[org]/um3.
#'
#'
#' }
#' @details For biomass estimates based on biovolume is usual the application of a cell density value, to retrieve the amount of organic carbon in the organism.
#' The function made available distinct options of conversion factor which are based in several authors.
#' These factors have been applied to a wide diversity of nano, micro, and macro-organisms, some applied to foraminifera and other nearby groups.
#'
#' @return An `data.frame` or numeric object, consisting of calculated biomass in ugC[org]/ind.
#'
#' @author Thaise R. Freitas \email{thaisericardo.freitas@@gmail.com}
#' @references
#' \itemize{
#'   \item Saidova, K. (1966). The biomass and quantitative distribution of live foraminifera in the Kurile-Kamchatka trench area. \emph{DOKLADY AKAD. NAUK SSSR}, 174(1), 216–217.
#'   \item Strathmann, R. (1967). Estimating the Organic Carbon Content of Phytoplankton from Cell Volume or Plasma Volume. \emph{Limnology and Oceanography}, 12, 411–418. \emph{doi:10.4319/lo.1967.12.3.0411}
#'   \item Turley, C., Newell, R., & Robins, D. (1986). Survival Strategies of 2 Small Marine Ciliates and Their Role in Regulating Bacterial Community Structure Under Experimental Conditions. \emph{Marine Ecology Progress Series}, 33(1), 59–70. \emph{doi:10.3354/meps033059}
#'   \item Putt, M., & Stoecker, D. K. (1989). An experimentally determined carbon : volume ratio for marine ‘oligotrichous’ ciliates from estuarine and coastal waters. \emph{Limnology and Oceanography}, 34(6), 1097–1103. \emph{doi:10.4319/lo.1989.34.6.1097}
#'   \item Gerlach, S. A., Hahn, A., & Schrage, M. (1985). Size spectra of benthic biomass and metabolism . \emph{Marine Ecology Progress Series}, 26, 161–173. \emph{doi:10.3354/meps026161}
#'   \item Michaels, A. F., Caron, D. A., Swanberg, N. R., Howse, F. A., & Michaels, C. M. (1995). Planktonic sarcodines (Acantaria, Radiolaria, Foraminifera) in surface waters near Bermuda: abundance, biomass and vertical flux. \emph{Journal of Plankton Research}, 17(0), 131–163. \emph{doi:10.1093/plankt/17.1.131}
#' }
#'
#' @seealso \code{\link{bio.volume}}, \code{\link{volume.total}}
#' @importFrom magrittr %>%
#' @export
#' @examples
#' #Ammonia biomass calculation
#' data(ammonia)
#'
#' #calculate test volume and biovolume
#' df <- bio.volume(data = ammonia, genus = "ammonia")
#' df
#'
#' #calculate individual biomass with choosen method
#' res <- biomass(df, method = 'michaels')
#' res
#'
#'

biomass <- function(biovolume, method = "michaels"){

  x <- data.frame(biovolume)

  if (is.null(method)) {
    method <- "michaels"
  }

  if (method == "saidova" | method == "Saidova") {

    b <- x$biovol * 0.1027

    result <- x %>%
      tibble::as_tibble() %>%
      dplyr::mutate(biomass = b/1.0e6)
    result

    } else if (method == "strathmann" | method == "Strathmann") {

      b <- x$biovol * 0.110

      result <- x %>%
        tibble::as_tibble() %>%
        dplyr::mutate(biomass = b/1.0e6)
      result

    } else if (method == "gerlach" | method == "Gerlach") {

      b <- x$biovol * 0.113

      result <- x %>%
        tibble::as_tibble() %>%
        dplyr::mutate(biomass = b/1.0e6)
      result

    } else if (method == "turley" | method == "Turley") {

      b <- x$biovol * 0.132

      result <- x %>%
        tibble::as_tibble() %>%
        dplyr::mutate(biomass = b/1.0e6)
      result

    } else if (method == "putt" | method == "Putt") {

      b <- x$biovol * 0.140

      result <- x %>%
        tibble::as_tibble() %>%
        dplyr::mutate(biomass = b/1.0e6)
      result

    } else if (method == "michaels" | method == "Michaels") {

      b <- x$biovol * 0.089

      result <- x %>%
        tibble::as_tibble() %>%
        dplyr::mutate(biomass = b/1.0e6)
      result

    }

  return(result)

}

