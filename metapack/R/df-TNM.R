#' Triglycerides Network Meta (TNM) data
#' 
#' A systemically reviewed network meta data set on tryglyceride (TG) lowering drugs
#' 
#' @format A data frame with 73 rows and 15 variables
#' \describe{
#'		\item{trial}{trial identifier}
#'		\item{treat}{treatment indicator for placebo (PBO), simvastatin (S), atorvastatin (A), lovastatin (L), rosuvastatin (R), pravastatin (P), ezetimibe (E), simvastatin+ezetimibe (SE), atorvastatin+ezetimibe (AE), lovastatin+ezetimibe (LE), or pravastatin+ezetimibe (PE)}
#'		\item{n}{the number of participants in the study corresponding to the trial and treatment}
#'		\item{ptg}{percentage change from baseline in triglycerides (TG)}
#'		\item{sdtg}{sample standard deviation of percentage change in triglycerides (TG)}
#'		\item{bldlc}{baseline LDL-C}
#'		\item{bhdlc}{baseline HDL-C}
#'		\item{btg}{baseline triglycerides (TG)}
#'		\item{age}{age in years}
#'		\item{white}{the proportion of white participants}
#'		\item{male}{the proportion of male participants}
#'		\item{bmi}{body fat index}
#'		\item{potencymed}{the proportion of medium statin potency}
#'		\item{potencyhigh}{the proportion of high statin potency}
#'		\item{durat}{duration in weeks}
#' }
#' @usage data(TNM)
#' @keywords datasets
#' @examples
#' data(TNM)
"TNM"