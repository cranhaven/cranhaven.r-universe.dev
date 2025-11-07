# data documentation
# march 2020


#' VPR data including CTD and ROI information
#'
#' An oce formatted CTD object with VPR CTD and ROI data from package example
#' data set.
#'
#' @format An oce package format, a 'CTD' object with VPR CTD and ROI data (1000
#'   data rows)
#'
"ctd_roi_oce"


#' VPR size information dataframe
#'
#' A sample data frame of size information from Visual Plankton outputs,
#' processed using \code{\link{vpr_ctdroisize_merge}}
#'
#'  @format A dataframe with 14 variables including
#'  \describe{
#'   \item{frame_ID}{Unique identifier for each VPR frame}
#'   \item{pressure}{Pressure measured from the VPR CTD in decibars}
#'   \item{temperature}{Temperature measured from the VPR CTD in celsius}
#'   \item{salinity}{Salinity measured from the VPR CTD}
#'   \item{sigmaT}{Density calculated from temperature, salinity and pressure}
#'   \item{fluorescence_mv}{Fluorescence measured by the VPR CTD in millivolts
#'   (uncalibrated)}
#'   \item{turbidity_mv}{Turbidity measured by the VPR CTD in
#'   millivolts (uncalibrated)}
#'   \item{roi}{Unique ROI identification number - 10
#'   digits, 8 digit millisecond time stamp and two unique digits to denote
#'   multiple ROIs within a millisecond}
#'   \item{taxa}{Category in which ROI has
#'   been classified by Visual Plankton}
#'   \item{day_hour}{Day and hour in which
#'   data was collected, from AutoDeck processing}
#'   \item{long_axis_length}{The
#'   length of the longest axis of the ROI image, measured by Visual Plankton}
#'   \item{station}{Station identifier provided during processing}
#'   \item{time_ms}{Time stamp when ROI was collected (milliseconds)}
#'   \item{roi_ID}{ROI identification number- 8 digit time stamp, without unique
#'   2 digit ending}
#'}
#'
"size_df_f"


#' A binned data frame of concentration data per category
#'
#' A 'binned' dataframe from sample VPR data, including concentrations
#' of each category, where each data point represents a 5 metre bin of
#'  averaged VPR data. Produced using \code{\link{vpr_roi_concentration}}
#'
#' @format A dataframe with 21 variables
#' \describe{
#'     \item{depth}{Depth calculated from pressure in metres}
#'     \item{min_depth}{The minimum depth of the bin in metres}
#'     \item{max_depth}{The maximum depth of the bin in metres}
#'     \item{depth_diff}{The difference between minimum and maximum bin depth in metres}
#'     \item{min_time_s}{The minimum time in seconds of the bin}
#'     \item{max_time_s}{The maximum time in seconds of the bin}
#'     \item{time_diff_s}{The difference between minimum and maximum time in a bin, in seconds}
#'     \item{n_roi_bin}{The number of ROI observations in a bin}
#'     \item{conc_m3}{The concentration of ROIs in a bin, calculated based on image volume and number of frames per bin}
#'     \item{temperature}{Temperature measured from the VPR CTD in celsius (averaged within the bin)}
#'     \item{salinity}{Salinity measured from the VPR CTD (averaged within the bin)}
#'     \item{density}{sigma T density calculated from temperature, salinity and pressure (averaged within the bin)}
#'     \item{fluorescence}{Fluorescence measured by the VPR CTD in millivolts
#'   (uncalibrated) (averaged within the bin)}
#'    \item{turbidity}{Turbidity measured by the VPR CTD in
#'   millivolts (uncalibrated) (averaged within the bin)}
#'   \item{avg_hr}{The mean time in which bin data was collected, in hours}
#'   \item{n_frames}{The number of frames captured within a bin}
#'   \item{vol_sampled_bin_m3}{The volume of the bin sampled in metres cubed}
#'   \item{toyo}{Identifier of the tow-yo section which bin is a part of, either ascending or descending, appended by a number}
#'   \item{max_cast_depth}{The maximum depth of the entire VPR cast}
#'   \item{taxa}{The category in which ROIs in bin have been classified by Visual Plankton}
#'   \item{station}{Station identifier provided during processing}
#' }
"taxa_conc_n"


#' VPR CTD data combined with tabulated ROIs
#'
#' A dataframe representing CTD data which has been merged with tabulated
#' ROIs in each category, produced by \code{\link{vpr_ctdroi_merge}}
#'
#' @format A dataframe with 28 variables
#'   \describe{
#'      \item{time_ms}{Time stamp when ROI was collected (milliseconds)}
#'      \item{conductivity}{Conductivity collected by the VPR CTD}
#'      \item{pressure}{Pressure measured from the VPR CTD in decibars}
#'      \item{temperature}{Temperature measured from the VPR CTD in celsius}
#'      \item{salinity}{Salinity measured from the VPR CTD}
#'      \item{fluor_ref}{A reference fluorescence baseline provided in millivolts by the VPR CTD for calibrating fluorescence_mv data}
#'      \item{fluorescence_mv}{Fluorescence in millivolts from the VPR CTD (uncalibrated)}
#'      \item{turbidity_ref}{A reference turbidity baseline provided in millivolts for calibrating turbidity_mv}
#'      \item{turbidity_mv}{Turbidity in millivolts from the VPR CTD (uncalibrated)}
#'      \item{altitude_NA}{Altitude data from the VPR CTD}
#'      \item{day}{Day on which VPR data was collected (from AutoDeck)}
#'      \item{hour}{Hour during which VPR data was collected (from AutoDeck)}
#'      \item{station}{Station identifier provided during processing}
#'      \item{sigmaT}{Density caluclated from temperature, pressure and salinity data}
#'      \item{depth}{Depth in metres caluclated form pressure}
#'      \item{roi}{ROI identification number}
#'      \item{categories}{For each category name (eg. bad_image_blurry, Calanus, krill), there is a line in the dataframe representing the number of ROIs identified in this category}
#'      \item{n_roi_total}{Total number of ROIs in all categories for each CTD data point}
#'
#'   }
"ctd_roi_merge"


#' VPR measurement data calculated by Visual Plankton
#'
#' A data frame of measurement information for each ROI in the sample
#' data set including long axis length, perimeter and area, produced by
#' \code{\link{vpr_autoid_read}}
#'
#' @format A data frame with 12 variables
#'   \describe{
#'     \item{roi}{Unique ROI identifier - 10 digit}
#'     \item{taxa}{Category in which ROI has been classified by Visual Plankton}
#'     \item{day_hour}{day and hour in which data was collected (from Autodeck)}
#'     \item{Perimeter}{The perimeter of the ROI in millimeters}
#'     \item{Area}{The area of the ROI in millimeters}
#'     \item{width1}{Width at a first point of the ROI in millimetres (defined in more detail in VPR manual)}
#'     \item{width2}{Width at a second point of the ROI in millimetres (defined in more detail in VPR manual)}
#'     \item{width3}{Width at a third point of the ROI in millimetres (defined in more detail in VPR manual)}
#'     \item{short_axis_length}{The length in millimeters of the ROI along the shorter axis}
#'     \item{long_axis_length}{The length in millimeters of the ROI along the longer axis}
#'     \item{station}{Station identifier provided in processing}
#'     \item{time_ms}{Time stamp when ROI was collected in milliseconds}
#'   }
"roimeas_dat_combine"

#' VPR CTD data
#'
#' A dataframe including all CTD parameters from the VPR CTD,
#'  produced by \code{\link{vpr_ctd_read}}
#'
#' @format A dataframe with 15 variables
#'    \describe{
#'      \item{time_ms}{Time stamp when ROI was collected (milliseconds)}
#'      \item{conductivity}{Conductivity collected by the VPR CTD}
#'      \item{pressure}{Pressure measured from the VPR CTD in decibars}
#'      \item{temperature}{Temperature measured from the VPR CTD in celsius}
#'      \item{salinity}{Salinity measured from the VPR CTD}
#'      \item{fluor_ref}{A reference fluorescence baseline provided in millivolts by the VPR CTD for calibrating fluorescence_mv data}
#'      \item{fluorescence_mv}{Fluorescence in millivolts from the VPR CTD (uncalibrated)}
#'      \item{turbidity_ref}{A reference turbidity baseline provided in millivolts for calibrating turbidity_mv}
#'      \item{turbidity_mv}{Turbidity in millivolts from the VPR CTD (uncalibrated)}
#'      \item{altitude_NA}{Altitude data from the VPR CTD}
#'      \item{day}{Day on which VPR data was collected (from AutoDeck)}
#'      \item{hour}{Hour during which VPR data was collected (from AutoDeck)}
#'      \item{station}{Station idnetifier provided during processing}
#'      \item{sigmaT}{Density caluclated from temperature, pressure and salinity data}
#'      \item{depth}{Depth in metres caluclated form pressure}
#'    }
"ctd_dat_combine"


#' VPR ROI data
#'
#' A dataframe including VPR ROI data from the sample dataset, produced by
#' \code{\link{vpr_autoid_read}}
#'
#'
#' @format A dataframe with 13 variables
#'   \describe{
#'      \item{roi}{Unique ROI identifier - 8 digit}
#'      \item{categories}{For each category name (eg. bad_image_blurry, Calanus, krill), there is a line in the dataframe representing the number of ROIs identified in this category}
#'      \item{time_ms}{Time stamp when ROI was collected (milliseconds)}
#'   }
#'
"roi_dat_combine"




