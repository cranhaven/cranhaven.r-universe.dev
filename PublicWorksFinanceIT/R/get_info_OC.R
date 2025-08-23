#' Retrieve information about regional and theme codes for the Open Coesione dataset.
#'
#' The \code{get_info_OC} function allows to get information regarding the codes to use to retrieve data from the Open Coesione database.
#'
#' @param info   character. The argument can be set to \code{"region"} if the data to be downloaded are based on regional codes, or it can be set to \code{"theme"} if the data to be downloaded are based on project's theme.
#'
#' @details
#' The information obtained can be used in the functions \code{get_data_OC} or \code{get_theme_OC}
#'
#' @returns
#' Return a vector of characters.
#' Regions:
#' \itemize{
#'  \item{VDA \strong{=>} Valle d'Aosta}
#'  \item{PIE \strong{=>} Piemonte}
#'  \item{LOM \strong{=>} Lombardia}
#'  \item{TN_BZ \strong{=>} Trentino Alto Adige (Bolzano)}
#'  \item{VEN \strong{=>} Veneto}
#'  \item{FVG \strong{=>} Friuli di Venezia Giulia}
#'  \item{LIG \strong{=>} Liguria}
#'  \item{EMR \strong{=>} Emilia Romagna}
#'  \item{TOS \strong{=>} Toscana}
#'  \item{UMB \strong{=>} Umbria}
#'  \item{MAR \strong{=>} Marche}
#'  \item{LAZ \strong{=>} Lazio}
#'  \item{ABR \strong{=>} Abruzzo}
#'  \item{CAM \strong{=>} Campania}
#'  \item{MOL \strong{=>} Molise}
#'  \item{PUG \strong{=>} Puglia}
#'  \item{CAL \strong{=>} Calabria}
#'  \item{BAS \strong{=>} Basilicata}
#'  \item{SIC \strong{=>} Sicilia}
#'  \item{SAR \strong{=>} Sardegna}
#'  \item{NAZ \strong{=>} National Level}
#'  \item{EST \strong{=>} Estero (Abroad)}
#'  }
#'
#'  Themes:
#'  \itemize{
#'  \item{RICERCA_INNOVAZIONE \strong{=>} Research and Innovation}
#'  \item{RETI_SERVIZI_DIGITALI \strong{=>} Digital Services}
#'  \item{COMPETITIVITA_IMPRESE \strong{=>} Firms Competition}
#'  \item{ENERGIA \strong{=>} Energy}
#'  \item{AMBIENTE \strong{=>} Environment}
#'  \item{CULTURA_TURISMO \strong{=>} Culture and Tourism}
#'  \item{TRASPORTI" \strong{=>} Transports}
#'  \item{OCCUPAZIONE \strong{=>} Employment}
#'  \item{INCLUSIONE_SOCIALE_SALUTE \strong{=>} Social Inclusion and Health}
#'  \item{ISTRUZIONE_FORMAZIONE \strong{=>} Education}
#'  \item{CAPACITA_AMMINISTRATIVA \strong{=>} Administrative Capacity}
#'  }
#'
#'@references \href{https://opencoesione.gov.it/en/dati/}{Open Coesione}
#'
#'@author Lorena Ricciotti
#'
#' @examples
#' \donttest{get_info_OC("region")}
#'
#'
#'@export
get_info_OC <- function(info) {
  if (info %in% c("region", "theme")) {
    url <- if (info == "region") {
      "https://opencoesione.gov.it/it/opendata/#!progetti_regione_section"
    } else {
      "https://opencoesione.gov.it/it/opendata/#!progetti_tema_section"
    }

    page <- rvest::read_html(url)

    if (info == "region") {
      data <- page %>%
        rvest::html_nodes("div#progetti_regione_section table.table tr") %>%
        sapply(function(x) {
          link <- rvest::html_nodes(x, "td a.download:nth-child(1)")
          if (length(link) > 0) {
            region_code <- sub('.*progetti_esteso_(\\w+)\\.zip.*', '\\1', rvest::html_attr(link[[1]], "href"))
          } else {
            region_code <- NA
          }
          return(region_code)
        })%>%
       as.character(na.rm = T)
    }else {
      data <- page %>%
        rvest::html_nodes("div#progetti_tema_section table.table tr td a.download") %>%
        rvest::html_attr("href")

      data <- sub(".*/progetti_esteso_(.*?)\\.zip", "\\1", data)
      data <- grep("^[^/]", data, value = TRUE)
    }

    return(data)
  } else {
    stop("The argument 'info' must be 'region' or 'theme'")
  }
}
