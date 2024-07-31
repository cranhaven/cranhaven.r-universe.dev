#' Parameter Information
#'
#' @description Parameter information for selected chemical constituents,
#'   organic compounds, and radionuclides measured for in water samples collected from
#'   monitoring sites in the U.S. Geological Survey (USGS) water-quality monitoring network,
#'   Idaho National Laboratory and vicinity, Idaho.
#'
#' @format A data frame with columns:
#'   \describe{
#'     \item{`pcode`}{USGS 5-digit parameter code.}
#'     \item{`parm_group_nm`}{Parameter group name.
#'       Abbreviations and descriptions are as follows:
#'         "Information" includes information about the water sample such as
#'           the date and time of collection, the location of the sample, and the method of analysis used;
#'         "Inorganics, Major, Metals" includes major inorganic ions and metals,
#'         "Inorganics, Major, Non-metals" includes major inorganic non-metallic ions;
#'         "Inorganics, Minor, Metals" includes minor inorganic metallic ions;
#'         "Inorganics, Minor, Non-metals" includes minor inorganic non-metallic ions;
#'         "Nutrient" includes nutrients such as nitrogen and phosphorus that can be present in water samples;
#'         "Organics, Other" includes organic compounds that are not classified as pesticides or PCBs;
#'         "Organics, Pesticide" includes organic compounds that are used as pesticides,
#'           such as insecticides, herbicides, and fungicides;
#'         "Physical" includes physical characteristics of water;
#'         "Radiochemical" includes radioactive isotopes that can be present in water samples; and
#'         "Stable Isotopes" includes non-radioactive isotopes of elements that can be
#'           used to trace the movement of water through the hydrologic cycle.}
#'     \item{`parm_nm`}{Long parameter name,
#'       such as "Strontium-90, water, unfiltered, picocuries per liter".}
#'     \item{`casrn`}{Chemical Abstracts Service
#'       ([CAS](https://www.cas.org/cas-data/cas-registry)) registry number,
#'       such as "10098-97-2" for Strontium-90.}
#'     \item{`srsname`}{Substance Registry Services
#'       ([SRS](https://sor.epa.gov/sor_internet/registry/substreg/home/overview/home.do)) name,
#'       such as "Strontium-90".}
#'     \item{`unit_cd`}{Units of measurement, see [`units`] dataset for unit descriptions.}
#'     \item{`min_dt`}{Collection date of first sample analyzed for the parameter.}
#'     \item{`max_dt`}{Collection date of last sample analyzed for the parameter.}
#'     \item{`nrecords`}{Number of records associated with the parameter.}
#'     \item{`nsites`}{Number of sampling sites where the parameter was observed.}
#'   }
#'
#' @source USGS water data acquired from the National Water Information System (U.S. Geological Survey, 2023).
#'   The [SRS](https://sor.epa.gov/sor_internet/registry/substreg/home/overview/home.do) name (`srsname`)
#'   for "Trihalomethanes (four), total, from SDWA NPDWR" was shorten to its preferred acronym "TTHM4".
#'
#' @references U.S. Geological Survey, 2023, National Water Information System---web services,
#'   accessed April 7, 2023 from \doi{10.5066/F7P55KJN}.
#'
#' @keywords datasets
#'
#' @examples
#' str(parameters)
"parameters"
