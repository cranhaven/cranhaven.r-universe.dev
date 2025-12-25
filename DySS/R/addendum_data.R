##' Data Example: Univariate Longitudinal Data
##' @title A simulated dataset with univariate data
##' @docType data
##' @description A simulated univariate longitudinal dataset for demonstration.
##' @usage data(data_example_long_1d)
##' @keywords datasets
##' @return A list of the following components
##' \item{$data_matrix_IC}{The data matrix for IC data.}
##' \item{$time_matrix_IC}{The time matrix for IC data.}
##' \item{$nobs_IC}{Number of observations for each IC process.}
##' \item{$data_matrix_OC}{The data matrix for OC data.}
##' \item{$time_matrix_OC}{The time matrix for OC data.}
##' \item{$nobs_OC}{Number of observations for each OC process.}
##' \item{$design_interval}{The design interval.}
##' \item{$n_time_units}{Number of time units in the design interval.}
##' \item{$time_unit}{The time unit.}
##' @examples
##' data(data_example_long_1d)
"data_example_long_1d"

##' Data Example: Multivariate Longitudinal Data
##' @title A simulated dataset with multivariate longitudinal data
##' @docType data
##' @description A simulated univariate longitudinal dataset for demonstration.
##' @usage data(data_example_long_md)
##' @keywords datasets
##' @return A list of the following components
##' \item{$data_array_IC}{The data array for IC data.}
##' \item{$time_matrix_IC}{The time matrix for IC data.}
##' \item{$nobs_IC}{Number of observations for each IC process.}
##' \item{$data_array_OC}{The data array for OC data.}
##' \item{$time_matrix_OC}{The time matrix for OC data.}
##' \item{$nobs_OC}{Number of observations for each OC process.}
##' \item{$design_interval}{The design interval.}
##' \item{$n_time_units}{Number of time units in the design interval.}
##' \item{$time_unit}{The time unit.}
##' @examples
##' data(data_example_long_md)
"data_example_long_md"

##' Data Example: Longitudinal and Survival Data
##' @title A simulated dataset with longitudinal and survival data
##' @docType data
##' @description A simulated univariate longitudinal dataset for demonstration.
##' @usage data(data_example_long_surv)
##' @keywords datasets
##' @return A list of the following components
##' \item{$data_array_IC}{The data array for IC data.}
##' \item{$time_matrix_IC}{The time matrix for IC data.}
##' \item{$nobs_IC}{Number of observations for each IC process.}
##' \item{$starttime_IC}{Start time of monitoring for IC processes.}
##' \item{$survtime_IC}{End time of monitoring for IC processes.}
##' \item{$survevent_IC}{Survival events of IC processes.}
##' \item{$data_array_OC}{The data array for OC data.}
##' \item{$time_matrix_OC}{The time matrix for OC data.}
##' \item{$nobs_OC}{Number of observations for each OC process.}
##' \item{$starttime_OC}{Start time of monitoring for OC processes.}
##' \item{$survtime_OC}{End time of monitoring for OC processes.}
##' \item{$survevent_OC}{Survival events of OC processes.}
##' \item{$design_interval}{The design interval.}
##' \item{$n_time_units}{Number of time units in the design interval.}
##' \item{$time_unit}{The time unit.}
##' @examples
##' data(data_example_long_surv)
"data_example_long_surv"

##' Real Data Example: Stroke Data
##' @title A real data example on stroke
##' @docType data
##' @description In this dataset, there are 27 subjects with stroke and 1028 subjects without stroke. 
##' Three risk factors, systolic blood pressures, diastolic blood pressures, cholesterol levels, 
##' are collected over time at different ages.
##' @usage data(data_stroke)
##' @keywords datasets
##' @return A list of the following components
##' \item{$systolic_ctrl}{A matrix of systolic blood pressures for controls. The [i,j] element is the jth observation of the ith control.}
##' \item{$diastolic_ctrl}{A matrix of diastolic blood pressures for controls. The [i,j] element is the jth observation of the ith control.}
##' \item{$cholesterol_ctrl}{A matrix of cholesterol levels for controls. The [i,j] element is the jth observation of the ith control.}
##' \item{$age_ctrl}{A matrix of the age of observations for controls. The [i,j] element is the age of jth observation for the ith control.}
##' \item{$systolic_case}{A matrix of systolic blood pressures for cases. The [i,j] element is the jth observation of the ith case.}
##' \item{$diastolic_case}{A matrix of diastolic blood pressures for cases. The [i,j] element is the jth observation of the ith case.}
##' \item{$cholesterol_case}{A matrix of cholesterol levels for cases. The [i,j] element is the jth observation of the ith case.}
##' \item{$age_case}{A matrix of the age of observations for cases. The [i,j] element is the age of jth observation for the ith case.}
##' @examples
##' data(data_stroke)
"data_stroke"
