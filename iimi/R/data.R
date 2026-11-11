#' Nucleotide information of virus segments
#'
#' A data set containing the GC content and other information about the virus
#' segments from the official Virtool virus data base (version 1.4.0). The
#' variables are as follows:
#'
#' @format A data frame with 7 variables:
#' \describe{
#'   \item{virus_name}{The virus name}
#'   \item{iso_id}{The virus isolate ID}
#'   \item{seg_id}{The virus segment ID}
#'   \item{A_percent}{The percentage of A nucleotides in the virus segment}
#'   \item{C_percent}{The percentage of C nucleotides in the virus segment}
#'   \item{T_percent}{The percentage of T nucleotides in the virus segment}
#'   \item{GC_percent}{The percentage of G and C nucleotides in the virus
#'   segment (GC content)}
#'   \item{seg_len}{The length of the virus segment}
#'   \item{1_4_0}{The version number of the virus database}
#'   \item{1_5_0}{The version number of the virus database}
#'   }
"nucleotide_info"



#' The unreliable regions of the virus segments
#'
#' A data frame of unmappable regions and regions of CG% and A% over 60% and 45%
#'  respectively for the virus segments. It is worth to note that if a
#'  virus segment does not have any unreliable regions, that virus segment is not
#'  shown in this data frame.
#'
#' @format A data frame of unreliable regions in the run-length encoding format
#'  for virus segments.
#' \describe{
#'   \item{Start}{The start position of the region that is considered unreliable}
#'   \item{End}{The end position of the region that is considered unreliable}
#'   \item{Virus segment}{The virus segment ID}
#'   \item{Categories}{The category that this unrelible region belong to, which
#'   are Unmappable regions (host), Unmappable regions (virus), CG% > 60%, A% > 45%}
#'   \item{1_4_0}{The version number of the virus database}
#'   \item{1_5_0}{The version number of the virus database}
#' }

"unreliable_regions"





#' A trained model using the default Random Forest settings
#'
#'

"trained_rf"



#' A trained model using the default XGBoost settings
#'
#'
#'

"trained_xgb"



#' A trained model using the default Elastic Net settings
#'
#'
#'

"trained_en"


#' Known diagnostics result of virus segments
#'
#' A matrix containing the known truth about the diagnostics result (using virus
#' database version 1.4.0) for each plant sample for the example data. It
#' records whether the sample is infected with a virus segment. Each column is a
#' sample, and each row is a virus segment's diagnostics status for three samples.
#'
#' @format A matrix with 3 columns:
#' \describe{
#'   \item{S1}{Sample one}
#'   \item{S2}{Sample two}
#'   \item{S3}{Sample three}
#'   }
"example_diag"


#' Coverage profiles of three plant samples.
#'
#' A list of coverage profiles for three plant samples. This is only a toy sample.
#' You can use it for running the examples in the vignette. We recommend using
#' more data to train the model, the more the better.
#'
#' @format A list of 3 run length encoding (RLE) lists for 3 plant samples.
#'  Each RLE list has the RLE vector of a virus segment
"example_cov"
