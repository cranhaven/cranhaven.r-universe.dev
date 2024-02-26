#' @title Data of surgery procedures performed at multiple hospitals (simulated)
#'
#' @description Data about patients and their surgery procedure from 30 simulated hospitals with patient arrivals in the first 400 days after the start of the study. \cr
#' Patient failure times are exponentially distributed with exponential hazard rate \eqn{h_0(t, \lambda) e^\mu}{h_0(t, \lambda) exp(\mu)}. Some hospitals have an increased failure rate:
#' \itemize{
#' \item Hospitals 1-15: \eqn{e^\mu = 1}{exp(\mu) = 1}
#' \item Hospitals 16-30: \eqn{e^\mu = 2}{exp(\mu) = 2}
#' } which means that the hazard rate at hospitals 16-30 is twice higher than exponential(\eqn{\lambda}). \cr
#' The arrival rate \eqn{\psi}{\psi} of patients at a hospital differs. The arrival rates are:
#' \itemize{
#' \item Hospitals 1-5 & 16-20: 0.5 patients per day
#' \item Hospitals 6-10 & 21-25: 1 patient per day
#' \item Hospitals 11-15 & 26-30: 1.5 patients per day
#' } These are then respectively small, medium and large hospitals.
#'
#'
#' @format A \code{data.frame} with 12010 rows and 9 variables:
#' \describe{
#'   \item{entrytime}{numeric Time of entry of patient into study}
#'   \item{survtime}{numeric Time from entry until failure of patient}
#'   \item{censorid}{integer Censoring indicator (0 - right censored, 1 - observed)}
#'   \item{Hosp_num}{integer Hospital number at which patient received treatment}
#'   \item{expmu}{numeric True excess hazard used for generating patient survival}
#'   \item{psival}{numeric Poisson arrival rate at hospital which the patient was at}
#'   \item{age}{numeric Age of the patient}
#'   \item{sex}{factor Sex of the patient}
#'   \item{BMI}{numeric Body mass index of the patient}
#' }
"surgerydat"
