#' Post Traumatic Stress Disorder data
#'
#' This data of 316 patients who survived the fire, each patient was measured at 3, 6 and 12 months after the fire.
#'
#' @docType data
#'
#' @format
#' \describe{
#'    \item{subject}{Patient number}
#'    \item{control}{Self-control (A numeric vector)}
#'    \item{problems}{The number of life problems ( A numeric vector )}
#'    \item{stress}{The number of stress events ( A numeric vector )}
#'    \item{cohesion}{Family cohesion ( A numeric vector )}
#'    \item{time}{Measured at 3, 6 and 12 months after the fire ( 1 : 3 months, 2 : 6 months, 3 : 12 months )}
#'    \item{ptsd}{Post traumatic stress disorder, Outcome variable ( Categorical vector ) ( 0 : No, 1 : Yes)}
#' }
#'
#' @source Allison (1991, chapter 8).
#' @references Allison, P. D. (2001). \emph{Logistic Regression Using the SAS System, Theory and Application.} SAS Institute Inc.
#' @details {
#' Control, problems, and stress were divided into upper and lower levels based on 3,
#' and cohesion was divided into upper and lower levels based on 6. ( 0 : Low, 1 : high )\cr
#' A data frame with 948 rows and 7 variables
#' \tabular{rccccccc}{
#' \strong{ } \tab \strong{subject} \tab \strong{control}  \tab \strong{problems} \tab \strong{stress} \tab \strong{cohesion} \tab \strong{time} \tab \strong{ptsd}\cr
#' 1     \tab 15       \tab 3.22    \tab 5.62      \tab 1        \tab 8        \tab 1    \tab 0    \cr
#' 2     \tab 15       \tab 3.17    \tab 5.38      \tab 0        \tab 8        \tab 2    \tab 0    \cr
#' 3     \tab 15       \tab 3.28    \tab 3.75      \tab 1        \tab 8        \tab 3    \tab 0    \cr
#' 4     \tab 18       \tab 2.56    \tab 9.25      \tab 0        \tab 8        \tab 1    \tab 1    \cr
#' 5     \tab 18       \tab 3.44    \tab 4.38      \tab 0        \tab 8        \tab 2    \tab 0    \cr
#' 6     \tab 18       \tab 3.33    \tab 2.38      \tab 0        \tab 8        \tab 3    \tab 0    \cr
#' 7     \tab 19       \tab 2.72    \tab 7.75      \tab 1        \tab 8        \tab 1    \tab 1    \cr
#' 8     \tab 19       \tab 2.78    \tab 7.75      \tab 1        \tab 7        \tab 2    \tab 1    \cr
#' 9     \tab 19       \tab 2.78    \tab 7.50      \tab 1        \tab 7        \tab 3    \tab 0    \cr
#' \code{.} \tab\code{.}\tab\code{.}\tab\code{.}\tab \code{.} \tab\code{.}\tab\code{.}\tab\code{.} \cr
#' \code{.} \tab\code{.}\tab\code{.}\tab\code{.}\tab \code{.} \tab\code{.}\tab\code{.}\tab\code{.} \cr
#' \code{.} \tab\code{.}\tab\code{.}\tab\code{.}\tab \code{.} \tab\code{.}\tab\code{.}\tab\code{.} \cr
#' 943   \tab 570      \tab 3.72    \tab 2.75      \tab 0        \tab 7        \tab 1    \tab 0    \cr
#' 944   \tab 570      \tab 3.89    \tab 2.25      \tab 0        \tab 7        \tab 2    \tab 0    \cr
#' 945   \tab 570      \tab 3.67    \tab 1.25      \tab 0        \tab 7        \tab 3    \tab 0    \cr
#' 946   \tab 571      \tab 3.56    \tab 3.00      \tab 0        \tab 7        \tab 1    \tab 0    \cr
#' 947   \tab 571      \tab 2.94    \tab 1.88      \tab 0        \tab 7        \tab 2    \tab 0    \cr
#' 948   \tab 571      \tab 3.50    \tab 2.75      \tab 0        \tab 7        \tab 3    \tab 0    \cr
#'  }
#' }
#' @usage data(PTSD)
"PTSD"



#' Supplementary data to be added to PTSD data
#'
#' Artificially created data to add drinking level to PTSD data.
#'
#' @docType data
#'
#' @format
#' \describe{
#'    \item{Drinking.0}{A value of 1 indicates that the degree of drinking is low}
#'    \item{Drinking.1}{A value of 1 indicates that the degree of drinking is high}
#' }
#' @details {
#' The degree of drinking (low, high) that can affect PTSD is added to the columns corresponding to the first to third time points for 316 patients. \cr
#' Indicator matrix of 948 rows and 2 dummy variables.
#' \tabular{rcc}{
#' \strong{ } \tab  \strong{Drinking.0}  \tab \strong{Drinking.0}\cr
#' 1         \tab 1        \tab 0    \cr
#' 2         \tab 1        \tab 0    \cr
#' 3         \tab 1        \tab 0    \cr
#' 4         \tab 0        \tab 1    \cr
#' 5         \tab 1        \tab 0    \cr
#' 6         \tab 1        \tab 0    \cr
#' 7         \tab 0        \tab 1    \cr
#' 8         \tab 0        \tab 1    \cr
#' 9         \tab 0        \tab 1    \cr
#' 10        \tab 1        \tab 0    \cr
#' \code{.}  \tab \code{.} \tab \code{.} \cr
#' \code{.}  \tab \code{.} \tab \code{.} \cr
#' \code{.}  \tab \code{.} \tab \code{.} \cr
#' 944       \tab 1        \tab 0    \cr
#' 945       \tab 1        \tab 0    \cr
#' 946       \tab 1        \tab 0    \cr
#' 947       \tab 1        \tab 0    \cr
#' 948       \tab 1        \tab 0    \cr
#'  }
#' }
#' @usage data(PTSD_column)
"PTSD_column"



#' Supplementary data to be added to PTSD data
#'
#' Artificially created data to add variables after 18 months to PTSD data.
#'
#' @docType data
#'
#' @format
#' \describe{
#'    \item{control.0}{A value of 1 indicates low control}
#'    \item{control.1}{A value of 1 indicates high control}
#'    \item{problems.0}{A value of 1 indicates that the degree of problems is low}
#'    \item{problems.1}{A value of 1 indicates that the degree of problems is high}
#'    \item{stress.0}{A value of 1 indicates that the degree of stress is low}
#'    \item{stress.1}{A value of 1 indicates that the degree of stress is high}
#'    \item{cohesion.0}{A value of 1 indicates that the degree of stress is low}
#'    \item{cohesion.1}{A value of 1 indicates that the degree of stress is high}
#'    \item{time.1}{Zero vector (All elements is zero)}
#'    \item{time.2}{Zero vector (All elements is zero)}
#'    \item{time.3}{Zero vector (All elements is zero)}
#'    \item{ptsd.0}{A value of 1 indicates a low post-traumatic stress disorder}
#'    \item{ptsd.1}{A value of 1 indicates a high post-traumatic stress disorder}
#' }
#'
#' @details {
#' This data is a long form of control, problem, stress, stress, stress and PTSD added to the row, and is intended for 316 patients after 18 months. \cr
#' Indicator matrix of 316 rows and 13 dummy variables.
#' \tabular{rcccccccccccc}{
#' \strong{ } \tab \strong{control.0}  \tab \strong{control.1}  \tab \strong{.} \tab \strong{.} \tab \strong{.} \tab \strong{time.1} \tab \strong{time.2} \tab \strong{time.3} \tab \strong{ptsd.0} \tab \strong{ptsd.1}\cr
#' 1       \tab 0       \tab 1        \tab \code{.} \tab \code{.} \tab \code{.} \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 2       \tab 0       \tab 1        \tab \code{ } \tab \code{} \tab \code{ } \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 3       \tab 0       \tab 1        \tab \code{.} \tab \code{.} \tab \code{.} \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 4       \tab 0       \tab 1        \tab \code{ } \tab \code{} \tab \code{ } \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 5       \tab 0       \tab 1        \tab \code{.} \tab \code{.} \tab \code{.} \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 6       \tab 0       \tab 1        \tab \code{ } \tab \code{} \tab \code{ } \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 7       \tab 0       \tab 1        \tab \code{.} \tab \code{.} \tab \code{.} \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 8       \tab 0       \tab 1        \tab \code{ } \tab \code{} \tab \code{ } \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 9       \tab 0       \tab 1        \tab \code{.} \tab \code{.} \tab \code{.} \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 10      \tab 0       \tab 1        \tab \code{ } \tab \code{} \tab \code{ } \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' \code{.}\tab \code{.}\tab \code{.} \tab \code{.} \tab \code{} \tab \code{ } \tab \code{.}  \tab \code{.}\tab \code{.} \tab \code{.} \tab \code{.}\cr
#' \code{.}\tab \code{.}\tab \code{.} \tab \code{ } \tab \code{.}\tab \code{ } \tab \code{.}  \tab \code{.}\tab \code{.} \tab \code{.} \tab \code{.}\cr
#' \code{.}\tab \code{.}\tab \code{.} \tab \code{} \tab \code{} \tab \code{.} \tab \code{.}  \tab \code{.}\tab \code{.} \tab \code{.} \tab \code{.}\cr
#' 312     \tab 1       \tab 1        \tab \code{.} \tab \code{.} \tab \code{.} \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 313     \tab 1       \tab 1        \tab \code{} \tab \code{} \tab \code{ } \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 314     \tab 1       \tab 1        \tab \code{.} \tab \code{.} \tab \code{.} \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 315     \tab 1       \tab 1        \tab \code{ } \tab \code{} \tab \code{ } \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#' 316     \tab 1       \tab 1        \tab \code{.} \tab \code{.}\tab \code{.} \tab 0    \tab 0    \tab 0    \tab 1    \tab 0    \cr
#'  }
#' }
#' @usage data(PTSD_row)
"PTSD_row"





#' Depression data
#'
#' Data comparing two drugs to treat patients suffering from depression.
#'
#' @docType data
#'
#' @format
#'
#' \describe{
#'    \item{Case}{Number of patients}
#'    \item{Diagnosis}{Classification based on the initial symptoms of depression ( 1 : Mild, 2 : Severe )}
#'    \item{Drug}{Drugs given to patients ( 1 : New, 2 : Standard )}
#'    \item{1week}{Depression symptoms 1week after taking the drug ( 1 : Abnormal, 2 : Normal )}
#'    \item{2weeks}{Depression symptoms 2weeks after taking the drug ( 1 : Abnormal, 2 : Normal )}
#'    \item{4weeks}{Depression symptoms 4weeks after taking the drug ( 1 : Abnormal, 2 : Normal )}
#' }
#' @references Koch, G. G., Landis, J. R., Freeman, J. L., Freeman, D. H. and Lehnen, R. C.(1977). A General Methodology for the Analysis of Experiments with Repeated Measurement of Categorical Data. \emph{Biometrics}, \bold{33}, 133-158.
#' @details {
#' Patients in each group were randomly assigned to standard or new drugs,
#' and the degree of each patient suffering from depression was classified as
#' normal or abnormal after 1 week, 2 weeks, and 4 weeks of treatment.\cr
#' A data frame with 800 rows and 6 variables
#' \tabular{rccccc}{
#' \strong{Case}  \tab \strong{Diagnosis}  \tab \strong{Drug} \tab \strong{1week} \tab \strong{2weeks} \tab \strong{4weeks}\cr
#' 1       \tab 1        \tab 2        \tab 2      \tab 2        \tab 2   \cr
#' 2       \tab 1        \tab 2        \tab 2      \tab 2        \tab 2   \cr
#' 3       \tab 1        \tab 2        \tab 2      \tab 2        \tab 2   \cr
#' 4       \tab 1        \tab 2        \tab 2      \tab 2        \tab 2   \cr
#' 5       \tab 1        \tab 2        \tab 2      \tab 2        \tab 2   \cr
#' 6       \tab 1        \tab 2        \tab 2      \tab 2        \tab 2   \cr
#' 7       \tab 1        \tab 2        \tab 2      \tab 2        \tab 2   \cr
#' 8       \tab 1        \tab 2        \tab 2      \tab 2        \tab 2   \cr
#' 9       \tab 1        \tab 2        \tab 2      \tab 2        \tab 2   \cr
#' 10      \tab 1        \tab 2        \tab 2      \tab 2        \tab 2   \cr
#' \code{.}\tab\code{.}  \tab \code{.} \tab \code{.}\tab\code{.} \tab \code{.}\cr
#' \code{.}\tab\code{.}  \tab \code{.} \tab \code{.}\tab\code{.} \tab \code{.}\cr
#' \code{.}\tab\code{.}  \tab \code{.} \tab \code{.}\tab\code{.} \tab \code{.}\cr
#' 796     \tab 2        \tab 1        \tab 1      \tab 1        \tab 1   \cr
#' 797     \tab 2        \tab 1        \tab 1      \tab 1        \tab 1   \cr
#' 798     \tab 2        \tab 1        \tab 1      \tab 1        \tab 1   \cr
#' 799     \tab 2        \tab 1        \tab 1      \tab 1        \tab 1   \cr
#' 800     \tab 2        \tab 1        \tab 1      \tab 1        \tab 1   \cr
#'  }
#' }
#' @usage data(Depression)
"Depression"



#' Supplementary data to be added to Depression data
#'
#' Artificially created data to add the degree and sex of depression after 6 weeks.
#' @docType data
#'
#' @format
#' \describe{
#'    \item{6week.1}{A value of 1 indicates that depression is "Abnormal" after 6 weeks}
#'    \item{6week.2}{A value of 1 indicates that depression is "Normal" after 6 weeks}
#'    \item{sex.1}{A value of 1 indicates that the gender is "Male"}
#'    \item{sex.2}{A value of 1 indicates that the gender is "Female"}
#' }
#' @details {
#' As supplementary variables, for 800 patients, response at fourth time point (after 6 weeks) and gender that could affect depression were added to the columns. \cr
#' Indicator matrix of 800 rows and 4 dummy variables.
#' \tabular{rccccc}{
#' \strong{ }  \tab \strong{6weeks.1}  \tab \strong{6weeks.2} \tab \strong{sex.1} \tab \strong{sex.2} \cr
#' 1       \tab 0        \tab 1        \tab 1      \tab 0     \cr
#' 2       \tab 0        \tab 1        \tab 1      \tab 0     \cr
#' 3       \tab 0        \tab 1        \tab 1      \tab 0     \cr
#' 4       \tab 0        \tab 1        \tab 1      \tab 0     \cr
#' 5       \tab 0        \tab 1        \tab 1      \tab 0     \cr
#' 6       \tab 0        \tab 1        \tab 1      \tab 0     \cr
#' 7       \tab 0        \tab 1        \tab 1      \tab 0     \cr
#' 8       \tab 0        \tab 1        \tab 1      \tab 0     \cr
#' 9       \tab 0        \tab 1        \tab 1      \tab 0     \cr
#' 10      \tab 0        \tab 1        \tab 1      \tab 0     \cr
#' \code{.}\tab \code{.} \tab \code{.} \tab\code{.}\tab\code{.}\cr
#' \code{.}\tab \code{.} \tab \code{.} \tab\code{.}\tab\code{.}\cr
#' \code{.}\tab \code{.} \tab \code{.} \tab\code{.}\tab\code{.}\cr
#' 796     \tab 0        \tab 1        \tab 0      \tab 1     \cr
#' 797     \tab 0        \tab 1        \tab 0      \tab 1     \cr
#' 798     \tab 0        \tab 1        \tab 0      \tab 1     \cr
#' 799     \tab 0        \tab 1        \tab 0      \tab 1     \cr
#' 800     \tab 0        \tab 1        \tab 0      \tab 1     \cr
#'  }
#' }
#'
#' @usage data(Depression_column)
"Depression_column"



#' Supplementary data to be added to Depression data
#'
#' Artificially generated data to add a placebo effect that affects the degree of depression.
#'
#' @docType data
#'
#' @format
#' \describe{
#'    \item{Diagnosis.1}{A value of 1 indicates the "Severe" of depression}
#'    \item{Diagnosis.2}{A value of 1 indicates the "Mild" of depression}
#'    \item{Drug.1}{A value of 1 indicates that the drug being taken is a “New drug”}
#'    \item{Drug.2}{A value of 1 indicates that the drug being taken is a “Standard drug”}
#'    \item{1week.1}{A value of 1 indicates that depression is "Abnormal" after 1 week}
#'    \item{1week.2}{A value of 1 indicates that depression is "Normal" after 1 week}
#'    \item{2week.1}{A value of 1 indicates that depression is "Abnormal" after 2 weeks}
#'    \item{2week.2}{A value of 1 indicates that depression is "Normal" after 2 weeks}
#'    \item{4week.1}{A value of 1 indicates that depression is "Abnormal" after 4 weeks}
#'    \item{4week.2}{A value of 1 indicates that depression is "Normal" after 4 weeks}
#' }
#' @details {
#' Supplementary 100 objects are patients who take placebo. \cr
#' Indicator matrix of 100 rows and 10 dummy variables
#'  \tabular{rcccccccccc}{
#' \strong{ }\tab \strong{Diag.1}\tab \strong{Diag.2}\tab \strong{Drug.1}\tab \strong{Drug.2}\tab \strong{.}\tab \strong{.}\tab \strong{.}\tab \strong{2week.2} \tab \strong{4week.1} \tab \strong{4week.2} \cr
#' 1       \tab 1       \tab 0      \tab 0      \tab 0      \tab \code{.}\tab\code{.}\tab\code{.}\tab 0    \tab 1    \tab 0 \cr
#' 2       \tab 1       \tab 0      \tab 0      \tab 0      \tab        \tab     \tab         \tab 0    \tab 1    \tab 0 \cr
#' 3       \tab 1       \tab 0      \tab 0      \tab 0      \tab\code{.}\tab\code{.} \tab\code{.}         \tab 0    \tab 1    \tab 0 \cr
#' 4       \tab 1       \tab 0      \tab 0      \tab 0      \tab        \tab     \tab         \tab 0    \tab 0    \tab 1 \cr
#' 5       \tab 1       \tab 0      \tab 0      \tab 0      \tab\code{.}\tab\code{.} \tab\code{.}         \tab 0    \tab 0    \tab 1 \cr
#' 6       \tab 1       \tab 0      \tab 0      \tab 0      \tab        \tab     \tab         \tab 0    \tab 0    \tab 1 \cr
#' 7       \tab 1       \tab 0      \tab 0      \tab 0      \tab\code{.}\tab\code{.} \tab\code{.}         \tab 0    \tab 0    \tab 1 \cr
#' 8       \tab 1       \tab 0      \tab 0      \tab 0      \tab        \tab     \tab         \tab 0    \tab 0    \tab 1 \cr
#' 9       \tab 1       \tab 0      \tab 0      \tab 0      \tab\code{.}\tab\code{.} \tab\code{.}         \tab 1    \tab 1    \tab 0 \cr
#' 10      \tab 1       \tab 0      \tab 0      \tab 0      \tab        \tab     \tab         \tab 1    \tab 1    \tab 0 \cr
#' \code{.}\tab\code{.} \tab\code{.}\tab\code{.}\tab\code{.}\tab\code{.}\tab \code{} \tab\code{}\tab\code{.}\tab\code{.} \tab\code{.}\cr
#' \code{.}\tab\code{.} \tab\code{.}\tab\code{.}\tab\code{.}\tab\code{}\tab \code{.} \tab\code{}\tab\code{.}\tab\code{.} \tab\code{.}\cr
#' \code{.}\tab\code{.} \tab\code{.}\tab\code{.}\tab\code{.}\tab\code{}\tab \code{} \tab\code{.}\tab\code{.}\tab\code{.} \tab\code{.}\cr
#' 96      \tab 0       \tab 1      \tab 0      \tab 0      \tab\code{.}\tab\code{.}\tab\code{.}\tab 1    \tab 0    \tab 1 \cr
#' 97      \tab 0       \tab 1      \tab 0      \tab 0      \tab        \tab     \tab         \tab 1    \tab 0    \tab 1 \cr
#' 98      \tab 0       \tab 1      \tab 0      \tab 0      \tab\code{.}\tab\code{.}\tab\code{.}\tab 1    \tab 0    \tab 1 \cr
#' 99      \tab 0       \tab 1      \tab 0      \tab 0      \tab        \tab     \tab         \tab 1    \tab 0    \tab 1 \cr
#' 100     \tab 0       \tab 1      \tab 0      \tab 0      \tab\code{.}\tab\code{.}\tab\code{.}\tab 1    \tab 0    \tab 1 \cr
#'  }
#' }
#' @usage data(Depression_row)
"Depression_row"


#' Patient's respiratory staus data
#'
#' This is part of the data on the patient's respiratory status.
#' @docType data
#'
#' @format
#' \describe{
#'    \item{subject}{Number of patients}
#'    \item{gender}{Patient gender ( 0 : Female, 1 : Male )}
#'    \item{age}{Patient age ( 0 : Under 30, 1 : Over 30 )}
#'    \item{month}{Measurement time ( 0 : before, 1 : 1 month, 2 : 2months )}
#'    \item{status}{Measurement status after taking placebo, response variable ( 0 : poor, 1 : good )}
#' }
#' @source Davis. (1977).
#' @references Davis, C. S. (1991). Semi-parametric and non-parametric methods for the analysis of repeated measurements with applications to clinical trials. \emph{Statistics in Medicine}, \bold{10}, 1959—1980.
#' @details {
#' 57 patients were measured for good and bad by taking 3 measurements before, 1 and 2 months after taking placebo
#' A data frame with 171 rows and 5 variables \cr
#' \tabular{rccccc}{
#' \strong{ } \tab  \strong{subject}  \tab \strong{gender}  \tab \strong{age}  \tab \strong{month}  \tab \strong{status}\cr
#' 1        \tab 1        \tab 0    \tab 1        \tab 0    \tab 0    \cr
#' 2        \tab 1        \tab 0    \tab 1        \tab 1    \tab 0    \cr
#' 3        \tab 1        \tab 0    \tab 1        \tab 2    \tab 0    \cr
#' 4        \tab 2        \tab 0    \tab 0        \tab 0    \tab 0    \cr
#' 5        \tab 2        \tab 0    \tab 0        \tab 1    \tab 0    \cr
#' 6        \tab 2        \tab 0    \tab 0        \tab 2    \tab 0    \cr
#' 7        \tab 4        \tab 0    \tab 1        \tab 0    \tab 1    \cr
#' 8        \tab 4        \tab 0    \tab 1        \tab 1    \tab 1    \cr
#' 9        \tab 4        \tab 0    \tab 1        \tab 2    \tab 1    \cr
#' \code{.} \tab\code{.}  \tab\code{.}\tab \code{.}\tab\code{.}\tab\code{.}\cr
#' \code{.} \tab\code{.}  \tab\code{.}\tab \code{.}\tab\code{.}\tab\code{.}\cr
#' \code{.} \tab\code{.}  \tab\code{.}\tab \code{.}\tab\code{.}\tab\code{.}\cr
#' 166      \tab 104      \tab 1    \tab 1        \tab 0    \tab 1    \cr
#' 167      \tab 104      \tab 1    \tab 1        \tab 1    \tab 0    \cr
#' 168      \tab 104      \tab 1    \tab 1        \tab 2    \tab 1    \cr
#' 169      \tab 106      \tab 1    \tab 1        \tab 0    \tab 0    \cr
#' 170      \tab 106      \tab 1    \tab 1        \tab 1    \tab 0    \cr
#' 171      \tab 106      \tab 1    \tab 1        \tab 2    \tab 0    \cr
#'  }
#' }
#' @usage data(Respiratory)
"Respiratory"


