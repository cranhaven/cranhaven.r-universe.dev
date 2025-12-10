#' @title EFIPLUS data used to develop ecological sensitivity parameters for riverine species in European streams and rivers.
#'
#' @description A \code{tibble}
#'
#' @docType data
#'
#' @details BQEs sensitivity to global/climate change in European rivers:
#' implications for reference conditions and pressure-impact-recovery chains (Logez et al. 2012). An extract has been made for
#' usage in this package but for more information write to ihg@boku.ac.at
#'
#' @usage data(efidata)
#'
#' @keywords European wide dataset
#'
#' @format A \code{tibble} 99 rows and 23 columns.
#'
#' @examples
#'
#' data("efidata")
#' efidata
#'
#' @references Logez M, Belliard J, Melcher A, Kremser H, Pletterbauer F, Schmutz S, Gorges G, Delaigue O, Pont D. 2012.
#' Deliverable D5.1-3: BQEs sensitivity to global/climate change in European rivers: implications for reference conditions
#' and pressure-impact-recovery chains.
#'
"efidata"

#' @title Joint Danube Survey Data
#'
#' @description A \code{tibble} Data on a five year periodic data collection within the Danube River Basin.
#' For more information, please visit https://www.danubesurvey.org/jds4/about
#'
#' @docType data
#'
#' @details Species ecological parameters such as ecological ranges both native and alien
#'
#' @usage data(jdsdata)
#'
#' @keywords freshwater information platform datasets
#'
#' @format A \code{tibble} 98 rows and 24 columns.
#'
#'
#' @examples
#'
#' data("jdsdata")
#' jdsdata
#'
#'
#' @references https://www.danubesurvey.org/jds4/about
#'
"jdsdata"


#' @title Sequential fences constants
#'
#' @description A \code{tibble} data with k constants for sequential fences method.
#'
#' @docType data
#'
#' @details k constants fro flagging outliers with several chnages in the fences.
#'
#' @usage data(kdat)
#'
#' @keywords constants sequential fences
#'
#' @format A \code{tibble} 101 rows and 2 columns.
#'
#'
#' @examples
#'
#' data("kdat")
#' kdat
#'
#' @references Schwertman NC, de Silva R. 2007. Identifying outliers with sequential fences.
#' Computational Statistics and Data Analysis 51:3800–3810.
#'
"kdat"


#' @title mth datasets with constant at each confidence interval levels.
#'
#' @description A \code{tibble} The data consist the
#'
#' @docType data
#'
#' @details The data is extracted from \code{(Schwertman & de Silva 2007)}.
#'
#' @usage data(mth)
#'
#' @keywords Standard dataset with constants used to compute outlier at a particular thresholds.
#'
#' @format A \code{tibble} 7 rows and 9 columns.
#'
#'
#' @examples
#' data("mth")
#' mth
#'
#'
#'
#' @references Schwertman NC, de Silva R. 2007. Identifying outliers with sequential fences.
#' Computational Statistics and Data Analysis 51:3800–3810.
#'
"mth"


#@title Dataset with species NATURA2000 categories.
#
#@description A \code{tibble} The data consist of 388303 rows and 19 columns which represents the
#       different species on the list. The list was downloaded from the EEA website and manually
#       archived in the package for use. For more information and citation
#
#@docType data
#
#@details The data is extracted from \code{(https://www.eea.europa.eu/themes/biodiversity/natura-2000)}.
#
#@usage data(naturalist)
#
#@keywords NATURA2000 NATURA2000 fish.
#
#@format A \code{tibble} 388303 rows and 5 columns.
#
#
#@examples
#
#
#data("naturalist")
#naturalist
#
#@references https://www.eea.europa.eu/themes/biodiversity/natura-2000.

#"naturalist"


#' @title Alburnoides bipunctatus species data from GBIF and iNaturalist
#'
#' @description A \code{tibble} Data from GBIF (https://www.gbif.org/) and iNaturalist (https://www.inaturalist.org/)
#'
#' @docType data
#'
#' @details The species data was collated from the Global Biodiversity Information Facility and iNaturalist
#'
#' @usage data(abdata)
#'
#' @keywords freshwater information platform datasets
#'
#' @format A \code{tibble} 2130 rows and 3 columns.
#'
#'
#' @examples
#'
#' data("abdata")
#' abdata
#'
#'
"abdata"

#' @title Thymallus thymallus species data from GBIF and iNaturalist
#'
#' @description A \code{tibble} Data from GBIF (https://www.gbif.org/) and iNaturalist (https://www.inaturalist.org/)
#'
#' @docType data
#'
#' @details The species data was collated from the Global Biodiversity Information Facility and iNaturalist
#'
#' @usage data(ttdata)
#'
#' @keywords freshwater information platform datasets
#'
#' @format A \code{tibble} 100 rows and 8 columns.
#'
#' @examples
#'
#' data("ttdata")
#' ttdata
#'
#'
"ttdata"





