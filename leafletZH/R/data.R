#' province data for China
#' @description
#' This dataset contains spatial (sf) data for provinces in China, including various attributes related to each province.
#' @source http://datav.aliyun.com/tools/atlas/
#'
#' \describe{
#'   \item{name}{The name of the province }
#'   \item{adcode}{The administrative code for the province, a unique identifier (e.g., "110000")}
#'   \item{childrenNum}{The number of administrative divisions (e.g., counties) within the province}
#'   \item{level}{The administrative level of the area, which is generally "province" for the entries in this dataset}
#'   \item{subFeatureIndex}{An index representing the sub-features within the province}
#'   \item{centroid}{The geographical centroid of the province, represented as a string of coordinates }
#'   \item{center}{The center point of the province, also represented as a string of coordinates}
#'   \item{parent}{A JSON string representing the parent administrative entity, usually the country-level data}
#'   \item{acroutes}{A JSON array of administrative codes that represent the administrative hierarchy leading to the province}
#'   \item{geometry}{Spatial geometry of the province, stored as an sf object in MULTIPOLYGON format}
#' }


"china_province"

#' city data for China
#' @description
#' This dataset contains spatial (sf) data for city in China, with various attributes specific to each district.
#' @source http://datav.aliyun.com/tools/atlas/
#'
#' \describe{
#'   \item{name}{The name of the district }
#'   \item{adcode}{The administrative code for the district, a unique identifier (e.g., "110101")}
#'   \item{childrenNum}{The number of lower-level administrative divisions within the district (usually 0 for districts)}
#'   \item{level}{The administrative level of the area, which is "district" for all entries in this dataset}
#'   \item{subFeatureIndex}{An index representing the sub-features within the district}
#'   \item{centroid}{The geographical centroid of the district, represented as a string of coordinates }
#'   \item{center}{The center point of the district, also represented as a string of coordinates}
#'   \item{parent}{A JSON string representing the parent administrative entity, usually the province-level data}
#'   \item{acroutes}{A JSON array of administrative codes that represent the full administrative hierarchy leading to the district}
#'   \item{geometry}{Spatial geometry of the district, stored as an sf object in MULTIPOLYGON format}
#' }

"china_city"
