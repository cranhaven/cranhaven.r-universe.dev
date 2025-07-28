#' Background Concentrations
#'
#' @description Water-quality background concentrations for selected radionuclides, organic compounds,
#'   and chemical constituents that were analyzed for in water from the eastern Snake River Plain aquifer
#'   at and near the Idaho National Laboratory (INL).
#'   The background concentrations are defined as groundwater influenced by western tributary recharge
#'   in the western INL and by eastern regional recharge in the eastern INL.
#'   These concentrations are either naturally occurring or anthropogenic
#'   (substances present in the environment as a result of human activities)
#'   and are not influenced by waste and wastewater disposal at the INL,
#'   according to Bartholomay and Hall (2016).
#'
#' @format A data frame with columns:
#'   \describe{
#'     \item{`parm_nm`}{Long parameter name,
#'       such as "Strontium-90, water, unfiltered, picocuries per liter".}
#'     \item{`pcode`}{U.S. Geological Survey 5-digit parameter code used to identify the constituent measured,
#'       see [`parameters`] dataset for details. For example, the parameter code for Tritium is "07000".}
#'     \item{`bkgrd_min`}{Minimum limit of background concentration.}
#'     \item{`bkgrd_max`}{Maximum limit of background concentration.}
#'     \item{`reference`}{Source of background concentration limits.
#'       Reference citations are as follows:
#'       "Bartholomay and Hall (2016)",
#'       "Knobel and others (1992)",
#'       "Michel (1989)", and
#'       "Orr and others (1991)".}
#'   }
#'
#' @source Idaho National Laboratory Project Office
#'
#' @references Bartholomay, R.C., and Hall, L.F., 2016, Evaluation of background concentrations
#'   of selected chemical and radiochemical constituents in groundwater in the
#'   eastern Snake River Plain aquifer at and near the Idaho National Laboratory, Idaho:
#'   U.S. Geological Survey Scientific Investigations Report 2016--5056, (DOE/ID--22237), 19 p.,
#'   \doi{10.3133/sir20165056}.
#' @references Knobel, L.L., Orr, B.R., and Cecil, L.D., 1992, Summary of background concentrations of
#'   selected radiochemical and chemical constituents in groundwater from the Snake River
#'   Plain aquifer, Idaho: estimated from an analysis of previously published data:
#'   Journal of the Idaho Academy of Science, v. 28, no. 1, p. 48--61.
#' @references Michel, R.L., 1989, Tritium deposition in the continental United States, 1953--83:
#'   U.S. Geological Survey Water Resources Investigations Report 89--4072, 46 p.,
#'   \doi{10.3133/wri894072}.
#' @references Orr, B.R., Cecil, L.D., and Knobel, L.L., 1991, Background concentrations of
#'   selected radionuclides, organic compounds, and chemical constituents in
#'   ground water in the vicinity of the Idaho National Engineering Laboratory:
#'   U.S. Geological Survey Water-Resources Investigations Report 91--4015 (DOE/ID--22094), 52 p.,
#'   \doi{10.3133/wri914015}.
#'
#' @keywords datasets
#'
#' @examples
#' str(background)
"background"
