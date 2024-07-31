#' Idaho National Laboratory Facilities
#'
#' @description Federal research facilities at the Idaho National Laboratory ([INL](https://inl.gov/)).
#'   The INL facilities have been the primary source of radioactive and
#'   chemical waste constituents in the water from the eastern Snake River Plain aquifer
#'   and in perched groundwater zones at or near the INL.
#'   This is due to the wastewater disposal practices at the INL facilities.
#'
#' @format A [simple feature](https://r-spatial.github.io/sf/articles/sf1.html) with fields:
#'   \describe{
#'     \item{`name`}{Facility name}.
#'     \item{`id`}{Facility identifier.
#'       Facility abbreviations and descriptions are as follows:
#'          "TAN" Test Area North,
#'          "NRF" Naval Reactors Facility,
#'          "MRF" Materials and Fuels Complex,
#'          "ATRC" Advanced Test Reactor Complex,
#'          "INTEC" Idaho Nuclear Technology and Engineering Center,
#'          "CFA" Central Facilities Area
#'          "RWMC" Radioactive Waste Management Complex.}
#'     \item{`geometry`}{Set of polygons, where a polygon is a geometry with a positive area (two-dimensional).}
#'   }
#'
#' @source A GeoJSON file of the facilities was created by the U.S. Geological Survey
#'   [Idaho National Laboratory Project Office](https://www.usgs.gov/centers/idaho-water-science-center/science/idaho-national-laboratory-project-office)
#'
#' @keywords datasets
#'
#' @examples
#' print(facilities)
"facilities"
