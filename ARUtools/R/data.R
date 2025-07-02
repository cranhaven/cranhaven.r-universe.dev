#' Example recording files
#'
#' A vector of examples ARU recording files.
#'
#' @format ## `example_files`
#' A vector with 42 file paths
#' @source data-raw/data_test.R
"example_files"



#' Example long-term deployment recording files
#'
#' A vector of examples ARU recording files. Uses the
#' `example_sites` data, but deploys them for a longer deployment
#'
#' @format ## `example_files_long`
#' A vector with 614 file paths
#' @source data-raw/data_long_deployment.R
"example_files_long"


#' Example site-level meta data
#'
#' A data frame with examples of incorrectly formatted site-level data.
#'
#' @format ## `example_sites`
#' A data frame with 10 rows and 8 columns:
#' \describe{
#'   \item{Sites}{Site ids}
#'   \item{Date_set_out}{Deployment start date}
#'   \item{Date_removed}{Deployment end date}
#'   \item{ARU}{ARU ids}
#'   \item{lon}{Longitude in decimal degrees}
#'   \item{lat}{Latitude in decimal degrees}
#'   \item{Plots}{Hypothetical extra plot column}
#'   \item{Subplot}{Hypothetical extra subplot column}
#' }
#' @source data-raw/data_test.R
"example_sites"

#' Example cleaned site-level meta data
#'
#' A data frame with examples of correctly formatted site-level data.
#'
#' @format ## `example_sites_clean`
#' A data frame with 10 rows and 8 columns:
#' \describe{
#'   \item{site_id}{Site ids}
#'   \item{aru_id}{ARU ids}
#'   \item{date_time_start}{Deployment start date/time}
#'   \item{date_time_end}{Deployment end date/time}
#'   \item{date_start}{Deployment start date}
#'   \item{date_end}{Deployment end date}
#'   \item{longitude}{Latitude in decimal degrees}
#'   \item{latitude}{Longitude in decimal degrees}
#' }
#' @source data-raw/data_test.R
"example_sites_clean"

#' Example cleaned recording meta data
#'
#' A data frame with examples of correctly formatted metadata with added
#' site-level information
#'
#' @format ## `example_clean`
#' A data frame with 42 rows and 10 columns:
#' \describe{
#'   \item{file_name}{Name of the file}
#'   \item{type}{File type}
#'   \item{path}{Relative file path including file name}
#'   \item{aru_type}{ARU model}
#'   \item{aru_id}{ARU ids}
#'   \item{site_id}{Site ids}
#'   \item{date_time}{Recording date/time}
#'   \item{date}{Recording date}
#'   \item{longitude}{Latitude in decimal degrees}
#'   \item{latitude}{Longitude in decimal degrees}
#' }
#' @source data-raw/data_test.R
"example_clean"




#' Example template of tasks for WildTrax
#'
#' A data frame with tasks generated from `example_clean` using
#' the wildRtrax::wt_make_aru_tasks() function. Allows updating of
#' tasks on WildTrax <https://wildtrax.ca/>.
#'
#' @format ## `task_template`
#' A data frame with 14 rows and 13 columns:
#' \describe{
#'   \item{location}{Site location name}
#'   \item{recording_date_time}{Date time of the recording}
#'   \item{method}{Method of interpretation (generally '1SPT')}
#'   \item{taskLength}{Length of recording in seconds}
#'   \item{transcriber}{Transcriber ID, to be filled in with function}
#'   \item{rain}{Empty character for filling in WildTrax}
#'   \item{wind}{Empty character for filling in WildTrax}
#'   \item{industryNoise}{Empty character for filling in WildTrax}
#'   \item{audioQuality}{Empty character for filling in WildTrax}
#'   \item{taskComments}{Empty character for filling in WildTrax}
#'   \item{internal_task_id}{Empty character for filling in WildTrax}
#' }
#' @source data-raw/data_wt_assign_tasks.R
"task_template"




#' Example template of tasks for WildTrax
#'
#' A data frame showing example observers and their effort
#'
#' @format ## `template_observers`
#' A data frame with 4 rows and 2 columns:
#' \describe{
#'   \item{transcriber}{Interpreter name in Wildtrax system}
#'   \item{hrs}{Number of hours to assign to interpreter}
#' }
#' @source data-raw/data_wt_assign_tasks.R
"template_observers"
