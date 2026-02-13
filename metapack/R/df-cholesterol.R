#' 26 double-blind, randomized, active, or placebo-controlled clinical trials on patients with primary hypercholesterolemia sponsored by Merck & Co., Inc., Kenilworth, NJ, USA.
#' 
#' A data set containing clinical trial on hypercholesterolemia including 26 trials and 2 treatment arms each, and other attributes of the participants
#' 
#' @format A data frame with 52 rows and 19 variables
#' \describe{
#'		\item{study}{study identifier}
#'		\item{trial}{trial identifier}
#'		\item{treat}{treatment indicator for Statin or Statin+Ezetimibe}
#'		\item{n}{the number of participants in the study arms corresponding to the trial and treatment}
#'		\item{pldlc}{aggregate percentage change in LDL-C}
#'		\item{phdlc}{aggregate percentage change from baseline in HDL-C}
#'		\item{ptg}{aggregate percentage change from baseline in triglycerides (TG)}
#'		\item{sdldl}{sample standard deviation of percentage change in LDL-C}
#'		\item{sdhdl}{sample standard deviation of percentage change in HDL-C}
#'		\item{sdtg}{sample standard deviation of percentage change in triglycerides (TG)}
#'		\item{onstat}{whether the participants were on Statin prior to the trial}
#'		\item{bldlc}{baseline LDL-C}
#'		\item{bhdlc}{baseline HDL-C}
#'		\item{btg}{baseline triglycerides (TG)}
#'		\item{age}{age in years}
#'		\item{white}{the proportion of white participants}
#'		\item{male}{the proportion of male participants}
#'		\item{dm}{the proportion of participants with diabetes mellitus}
#'		\item{durat}{duration in weeks}
#' }
#' @usage data(cholesterol)
#' @keywords datasets
#' @examples
#' data(cholesterol)
"cholesterol"