#' @title Mayo Clinic primary biliary cirrhosis data
#' @description This data is from the Mayo Clinic trial in primary biliary
#'   cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total of
#'   424 PBC patients, referred to Mayo Clinic during that ten-year interval met
#'   eligibility criteria for the randomized placebo controlled trial of the
#'   drug D-penicillamine, but only the first 312 cases in the data set
#'   participated in the randomized trial. Therefore, the data here are for the
#'   312 patients with largely complete data.
#' @usage data(pbc)
#' @name pbc
#' @format A data frame with 1945 observations on the following 20 variables:
#'   \describe{
#'   \item{\code{id}}{patients identifier; in total there are 312 patients.}
#'   \item{\code{years}}{number of years between registration and the earlier of death, transplantation, or study analysis time.}
#'   \item{\code{status}}{a factor with levels \code{alive}, \code{transplanted} and \code{dead}.}
#'   \item{\code{drug}}{a factor with levels \code{placebo} and \code{D-penicil}.}
#'   \item{\code{age}}{at registration in years.}
#'   \item{\code{sex}}{a factor with levels \code{male} and \code{female}.}
#'   \item{\code{year}}{number of years between enrollment and this visit date, remaining values on the line of data refer to this visit.}
#'   \item{\code{ascites}}{a factor with levels \code{No} and \code{Yes}.}
#'   \item{\code{hepatomegaly}}{a factor with levels \code{No} and \code{Yes}.}
#'   \item{\code{spiders}}{a factor with levels \code{No} and \code{Yes}.}
#'   \item{\code{edema}}{a factor with levels \code{No edema} (i.e. no edema and no diuretic therapy for edema), \code{edema no diuretics} (i.e. edema present without diuretics, or edema resolved by diuretics), and \code{edema despite diuretics} (i.e. edema despite diuretic therapy).}
#'   \item{\code{serBilir}}{serum bilirubin in mg/dl.}
#'   \item{\code{serChol}}{serum cholesterol in mg/dl.}
#'   \item{\code{albumin}}{albumin in mg/dl.}
#'   \item{\code{alkaline}}{alkaline phosphatase in U/liter.}
#'   \item{\code{SGOT}}{SGOT in U/ml.}
#'   \item{\code{platelets}}{platelets per cubic ml/1000.}
#'   \item{\code{prothrombin}}{prothrombin time in seconds.}
#'   \item{\code{histologic}}{histologic stage of disease.}
#'   \item{\code{status2}}{a numeric vector with the value 1 denoting if the patient was dead, and 0 if the patient was alive or transplanted.}
#'   }
#' @keywords datasets
#' @source \code{\link[survival]{pbc}}.
#' @references Fleming T, Harrington D. \emph{Counting Processes and Survival Analysis}.
#' 1991; New York: Wiley.
#' @references Therneau T, Grambsch P. \emph{Modeling Survival Data: Extending the Cox
#' Model}. 2000; New York: Springer-Verlag.
"pbc"

#' @rdname pbc
#' @usage data(pbc.merlin)
#' @note \code{pbc.merlin} is a version of the \code{pbc} dataset in \code{merlin} format.
"pbc.merlin"

#' @title Simulated 3-level survival data
#' @description Simulated 3-level survival data
#' @usage data(sim3)
#' @name sim3
#' @format A data frame...
#' @keywords datasets
"sim3"

#' @title Aortic valve replacement surgery data
#' @description This is longitudinal data on an observational study on detecting effects of different heart valves, differing on type of tissue, implanted in the aortic position.
#' The data consists of longitudinal measurements on three different heart function outcomes, after surgery occurred.
#' There are several baseline covariates available, and also survival data.
#' @usage data(heart.valve)
#' @format This is a data frame in the unbalanced format, that is, with one row per observation.
#' The data consists in columns for patient identification, time of measurements, longitudinal multiple longitudinal measurements, baseline covariates, and survival data.
#' The column names are identified as follows:
#'  \describe{
#'   \item{\code{num}}{number for patient identification.}
#'   \item{\code{sex}}{gender of patient (\code{0=}Male and \code{1=}Female).}
#'   \item{\code{age}}{age of patient at day of surgery (years).}
#'   \item{\code{time}}{observed time point, with surgery date as the time origin (years).}
#'   \item{\code{fuyrs}}{maximum follow up time, with surgery date as the time origin (years).}
#'   \item{\code{status}}{censoring indicator (\code{1=}died and \code{0=}lost at follow up).}
#'   \item{\code{grad}}{valve gradient at follow-up visit.}
#'   \item{\code{log.grad}}{natural log transformation of \code{grad}.}
#'   \item{\code{lvmi}}{left ventricular mass index (standardised) at follow-up visit.}
#'   \item{\code{log.lvmi}}{natural log transformation of \code{lvmi}.}
#'   \item{\code{ef}}{ejection fraction at follow-up visit.}
#'   \item{\code{bsa}}{preoperative body surface area.}
#'   \item{\code{lvh}}{preoperative left ventricular hypertrophy.}
#'   \item{\code{prenyha}}{preoperative New York Heart Association (NYHA) classification (\code{1=}I/II and \code{3=}III/IV).}
#'   \item{\code{redo}}{previous cardiac surgery.}
#'   \item{\code{size}}{size of the valve (millimetres).}
#'   \item{\code{con.cabg}}{concomitant coronary artery bypass graft.}
#'   \item{\code{creat}}{preoperative serum creatinine (\eqn{\mu}mol/mL).}
#'   \item{\code{dm}}{preoperative diabetes.}
#'   \item{\code{acei}}{preoperative use of ace inhibitor.}
#'   \item{\code{lv}}{preoperative left ventricular ejection fraction (LVEF) (\code{1=}good, \code{2=}moderate, and \code{3=}poor).}
#'   \item{\code{emergenc}}{operative urgency (\code{0=}elective, \code{1=}urgent, and \code{3=}emergency).}
#'   \item{\code{hc}}{preoperative high cholesterol (\code{0=}absent, \code{1=}present treated, and \code{2=}present untreated).}
#'   \item{\code{sten.reg.mix}}{aortic valve haemodynamics (\code{1=}stenosis, \code{2=}regurgitation, \code{3=}mixed).}
#'   \item{\code{hs}}{implanted aortic prosthesis type (\code{1=}homograft and \code{0=}stentless porcine tissue).}
#'   }
#' @keywords datasets
#' @source Mr Eric Lim (\url{http://www.drericlim.com})
#' @references Lim E, Ali A, Theodorou P, Sousa I, Ashrafian H, Chamageorgakis T, Duncan M, Diggle P, Pepper J. A longitudinal study of the profile and predictors of left ventricular mass regression after stentless aortic valve replacement. \emph{Ann Thorac Surg.} 2008; \strong{85(6)}: 2026-2029.
"heart.valve"

#' @rdname heart.valve
#' @usage data(heart.valve.merlin)
#' @note \code{heart.valve.merlin} is a version of the \code{heart.valve} dataset in \code{merlin} format.
"heart.valve.merlin"

