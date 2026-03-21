#' Heat Map of Occurrence Frequency by Country
#'
#' Creates a heatmap of the frequency of an occurrence by country/region.
#'
#' @export
#' @importFrom graphics par
#' @importFrom dplyr %>%
#' @param df A dataframe in FishNet2 standard format with column labeled 'Country'
#' @param name Value in 'ScientificName' or 'Family' column
#' @return heatmap showing frequency by country
#'
#' @examples
#' heatmap_world(ictaluridae)
#'
heatmap_world <- function(df, name="none"){
  # check for 'Country' column existence
  if ("Country" %in% colnames(df) == FALSE) {
    stop("'Country' column does not exist.")
  }
  # check that level is either species, genus, or family
  if("ScientificName" %in% colnames(df) == FALSE | "Family" %in% colnames(df) == FALSE ) {
    stop("'ScientificName' or 'Family' column does not exist.")
  }
  if (name != "none"){
    df <- subset(df, ScientificName == name | Family == name)
  }

  # select rows with name only and check if empty
  if (nrow(df)==0) {
    stop("Name given does not exist in 'ScientificName' or 'Family' columns")
  }
  # map country names
  info <- df %>% count(Country)
  info <- dplyr::rename(info, Countries = n)
  map_info <- rworldmap::joinCountryData2Map(dF=info, joinCode = "NAME", nameJoinColumn = 'Country', verbose=TRUE)

  # plot map
  par(mai=c(0,0,0.2,0), xaxs="i",yaxs="i")
  rworldmap::mapCountryData(map_info, nameColumnToPlot = "Countries",catMethod="categorical")

}
