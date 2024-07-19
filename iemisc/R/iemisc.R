#' iemisc: Irucka Embry's miscellaneous functions
#'
#' iemisc provides many useful functions. There are statistical analysis [RMS,
#' approximate and relative error, maximum mean relative error (MAXRE), and
#' mean relative error (MRE), coefficient of variation (CV), range, harmonic
#' mean, geometric mean], engineering economics (benefit-cost, future value,
#' present value, annual value, gradients, interest, periods, etc.), geometry
#' (polygon area, sphere volume, and right triangle validation), civil &
#' environmental/water resources engineering [Concrete Mix Design for Normal
#' Strength & Structural Lightweight Concrete, Construction Measurements with
#' and without Fractions, Engineering Surveying Calculations, Air Stripping,
#' Saturated Vapor Pressure for Ice and Water, Density of Water, Dynamic and
#' Kinematic Viscosity of Water, Specific Gravity and Volume, Unit Weight,
#' Surface Tension of Water, Weighted C Factor, Weighted Curve Number, Darcy
#' friction factor (f), the Reynolds number, Manning's n,
#' Gauckler-Manning-Strickler equations for open channel flow], quick search,
#' return character vectors in order, mortality rate calculations, proportion
#' solver, sum of all digits in a vector to a single integer, string
#' manipulation, a version of linear interpolation for use with NAs, Python
#' compatible floor division function, GNU Octave/MATLAB compatible
#' trigonometric functions in degrees, GNU Octave/MATLAB compatible section
#' properties, remainder, modulus, number of dimensions, row vector and column
#' vector tests, fractional differences, size, numel, and length functions.
#'
#' @import iemiscdata
#' @import USA.state.boundaries
#' @import chem.databases
#' @importFrom foreach foreach %do%
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom ggplot2 ggplot aes geom_polygon
#' @importFrom ggpubr ggtexttable ttheme ggarrange
#' @importFrom pracma nthroot interp1 interp2 zeros asec acsc newtonRaphson
#' @importFrom matlab repmat fix
#' @importFrom ramify mat
#' @importFrom fpCompare %==%
#' @importFrom rivr critical_depth
#' @importFrom measurements conv_unit
#' @importFrom roperators %+%
#' @importFrom berryFunctions l2df traceCall getColumn
#' @importFrom gsubfn list
#' @importFrom round round_r3
#' @importFrom sjmisc is_empty
#' @importFrom lubridate %--% dyears
#' @importFrom anytime anydate
#' @importFrom geosphere midPoint
#' @importFrom qdapRegex rm_white
#' @importFrom mgsub mgsub
#' @importFrom data.table as.data.table data.table setnames setattr copy setkey setDF %chin% .I .EACHI := setDT between melt rbindlist
#' @importFrom units set_units make_units drop_units
#' @importFrom assertthat assert_that is.scalar
#' @importFrom checkmate qtest testDataTable testCharacter test_data_frame testString
#' @importFrom stringi stri_replace_all_fixed stri_detect_regex stri_trim_both stri_replace_all_regex stri_detect_fixed stri_replace_first_fixed stri_split_fixed stri_count_regex stri_length stri_extract_first_regex
#' @importFrom stats approx uniroot sd na.omit
#' @importFrom zoo na.trim
#' @importFrom utils data
#' @importFrom methods is
#' @importFrom signal fftfilt
#' @importFrom matlab2r nargin
#'
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"
