#' Discrete Sample Data
#'
#' @description Water-quality information for both groundwater and surface water
#'   collected from monitoring stations in and around the Idaho National Laboratory, Idaho.
#'   The water samples were collected in the field and analyzed in a laboratory to obtain water-quality data.
#'   The dataset was obtained from the National Water Information System (NWIS),
#'   which is maintained by the U.S. Geological Survey.
#'   The NWIS is a comprehensive and distributed application that supports the acquisition,
#'   processing, and long-term storage of water data.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{`site_nm`}{Local site name.}
#'   \item{`sample_dt`}{Date and time the sample was collected, in "America/Denver" time zone.
#'     Missing values of time were substituted with "12:00".}
#'   \item{`parm_short_nm`}{Parameter short name assigned by the USGS, such as "pH".}
#'   \item{`unit_cd`}{Units of measurement, see [`units`] dataset for unit descriptions.}
#'   \item{`remark_cd`}{Remark code (result level) used to qualify the parameter value.
#'     The codes and their meanings are as follows:
#'     [NA] (missing value) is a quantified value;
#'     "<" is where the actual value is known to be less than the value reported,
#'       that is, the measured concentration is below the reporting limit (RL)
#'       and represented as a censored (or nondetection) value.
#'       For censored values, the value reported is the RL;
#'     "E" is an estimated value, that is, the actual value is greater than the minimum detection limit (MDL)
#'       and less than the laboratory reporting level (LRL);
#'     "R" is a nondetect, result less than sample-specific critical level; and
#'     "U" is a material specifically analyzed for but not detected.}
#'   \item{`result_va`}{Parameter value.}
#'   \item{`lab_sd_va`}{Laboratory standard deviation (SD).
#'     For radiochemical data, SD is typically determined from the counting error.
#'     Prior to January 1, 2008, counting error was reported as two SD (Bartholomay and others, 2020, p. 27),
#'     therefore, these values were divided by 2.}
#'   \item{`lab_li_va`}{Lower confidence interval of the `result` value based on laboratory analysis.}
#'   \item{`lab_ui_va`}{Upper confidence interval of the `result` value based on laboratory analysis.
#'      In cases where the upper and lower limits are identical, the parameter is expressed as an exact value.}
#'   \item{`rpt_lev_va`}{Laboratory reporting limit in effect for the parameter and
#'     analytical method at the time the measurement was made.}
#'   \item{`rpt_lev_cd`}{Reporting level code that identifies the analytical reporting level
#'     appropriate for the analytical method.
#'     The codes and their meanings are as follows:
#'     "DLBLK" detection limit by blank data;
#'     "DLDQC" detection limit by DQCALC, lowest concentration that with 90 percent confidence will be
#'       exceeded no more than 1 percent of the time when a blank sample is measured;
#'     "IRL" interim reporting level, a temporary reporting level;
#'     "LRL" laboratory reporting level, equal to twice the yearly-determined LT-MDL;
#'     "LT-MDL" long-term method detection limit,
#'       a detection level derived by determining the standard deviation of
#'       a minimum of 24 MDL spike sample measurements over an extended period of time;
#'     "MDL" method detection limit, minimum concentration of a substance that can be measured
#'       and reported with a 99 percent confidence that the analyte concentration is greater than zero;
#'     "PQL" practical quantitation limits;
#'     "MRL" minimum reporting level, smallest measured concentration that can be reliably measured using
#'       a given analytical method;
#'     "RLDQC" reporting limit by DQCALC, is greater than or equal to two times the DLDQC;
#'     "SSLC" sample-specific critical level, the calculated and reported value is below
#'       which the radiochemistry result is considered a non-detect; and
#'     "SSMDC" sample-specific minimum detectable concentration, a reporting level that varies
#'       for each sample and is primarily used in radiochemical analyses.}
#'   \item{`medium_cd`}{Medium code that identifies the material type
#'     and quality assurance type of the sample.
#'     The codes and their meanings are as follows:
#'     "OAQ" is a blank sample collected for QC purposes;
#'     "WG" is water below land surface contained in the saturated zone (groundwater);
#'     "WGQ" is a groundwater quality-control (QC) sample;
#'     "WS" is water on the surface of the Earth (surface water); and
#'     "WSQ" surface water QC sample.}
#'   \item{`anl_ent_cd`}{Analyzing entity code of the organizational unit that
#'     performed the sample analysis used to obtain the result.}
#'   \item{`dqi_cd`}{Data quality indicator code that indicates the review status of a result.
#'     The codes and their meanings are as follows:
#'     "A" historical data,
#'     "R" reviewed and accepted, and
#'     "S" provisional (presumed satisfactory).}
#'   \item{`meth_cd`}{Method code, the codes are documented in the NWIS Method Code Dictionary.}
#'   \item{`sample_type_cd`}{Sample type code that identifies the quality-assurance (QA) type of a sample.
#'     The codes and their meanings are as follows:
#'     "2" is a blank sample;
#'     "6" is a reference material sample;
#'     "7" is a replicate sample taken from the environment;
#'     "9" is a regular sample taken from the environment;
#'     "B" is a unspecified QA sample; and
#'     "H" is a composite (time) sample.}
#'   \item{`db_no`}{2-digit NWIS database number.
#'     The codes and their meanings are as follows:
#'     "01" is the environmental database, and
#'     "10" is the QA database.}
#'   \item{`sample_id`}{Unique identifier for the water sample.
#'     The sample code is a concatenation of the site number, medium code, and date-time the sample was collected.}
#'   \item{`site_no`}{USGS site identification number.}
#'   \item{`pcode`}{USGS 5-digit parameter code.
#'     For example, the parameter code for Tritium is "07000".}
#'   \item{`rep_pair_id`}{Unique identifier used for matching pairs of replicate samples for a specific parameter.
#'     Replicate pairs are identified by matching a replicate sample (`sample_type_cd` equal to 7) with
#'     its corresponding regular environmental sample (`sample_type_cd` equal to 9).}
#'   \item{`result_tx`}{Remark about the water quality result}
#'   \item{`remark`}{Remarks pertaining to changes applied after the records were obtained from NWIS.}
#'   \item{`anl_dt`}{Result analysis date.}
#' }
#'
#' @source Data were obtained from the NWIS-QWDATA database on January 22, 2024,
#' in tab-delimited output-format using the QWDATA system (U.S. Geological Survey, 2024).
#' The following steps were taken to process the data:
#' - **Column Name Translation:** Column names were switched from NWIS alpha codes to NWIS parameter codes.
#' - **Class Conversion:** Classes for each column were converted to the appropriate type (numeric, POSIX, factor).
#' - **Column Removal:** Unnecessary columns were removed.
#' - **Duplicate Removal:** Duplicate records in the NWIS and QWDATA databases were removed.
#' - **Data Cleaning:** Corrupted results were removed.
#'   A column was added for data processing remarks.
#'   Zero and negative results were reported as nondetects.
#' - **Radiochemical Parameter Identification:** Radiochemical parameter codes were identified.
#'   Nondetects were reported as less than the reporting level.
#' - **Sample Type Reporting:** In the absence of a sample type code,
#'   results were classified as environmental samples.
#'   Data from blank, spiked, and reference samples,
#'   as well as composite samples (collected over a period of time), were excluded.
#' - **Sample Identifier Creation:** Site number and date/time were combined to create a unique sample identifier.
#' - **Error Conversion:** Counting error was converted to laboratory standard deviation.
#'   Data entered into NWIS with an uncertainty of twice the standard deviation (data entered before 2008),
#'   were converted to one standard deviation.
#' - **Record Removal:** Records associated with counting error parameter codes were removed.
#' - **Replicate Pairing:** Replicate samples were paired based on the
#'   parameter code, site number, and time difference between sample collection.
#' - **Data Adjustment:** The detection limit, left-censored values, and non-positive lower limit were accounted for.
#'   Interval censored data were represented using upper and lower limits of the
#'   95-percent confidence interval of the parameter value.
#'
#' @references Bartholomay, R.C., Maimer, N.V., Rattray, G.W., and Fisher, J.C., 2020,
#'   An update of hydrologic conditions and distribution of selected constituents in water,
#'   Eastern Snake River Plain Aquifer and perched groundwater zones, Idaho National Laboratory,
#'   Idaho, emphasis 2016-18:
#'   U.S. Geological Survey Scientific Investigations Report 2019-5149 (DOE/ID-22251), 82 p.,
#'   \doi{10.3133/sir20195149}.
#' @references U.S. Geological Survey, 2024, National Water Information System---Water-Quality System (QWDATA)
#'   data retrieval program.
#'
#' @keywords datasets
#'
#' @examples
#' str(samples)
#'
#' poi <- as.POSIXct(c("1989-01-01", "2019-01-01")) # period of interest
#' is_poi <- samples$sample_dt >= poi[1] & samples$sample_dt < poi[2]
#' is_stc <- samples$sample_type_cd %in% c("7", "9")
#'
#' site_no <- "433253112545901" # well USGS 20
#' pcode <- "07000" # tritium, water, unfiltered, picocuries per liter
#' is <- is_poi & is_stc & samples$site_no == site_no & samples$pcode == pcode
#' d <- samples[is, ]
#' plotrix::plotCI(
#'   x = d$sample_dt,
#'   y = d$result_va,
#'   li = d$lab_li_va,
#'   ui = d$lab_ui_va
#' )
#'
#' site_no <- "433322112564301" # well USGS 38
#' pcode <- "01030" # chromium, water, filtered, micrograms per liter
#' is <- is_poi & is_stc & samples$site_no == site_no & samples$pcode == pcode
#' d <- samples[is, ]
#' plotrix::plotCI(
#'   x = d$sample_dt,
#'   y = d$result_va,
#'   li = d$lab_li_va,
#'   ui = d$lab_ui_va
#' )
"samples"
