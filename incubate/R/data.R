#' Survival time of mice with glioma under different treatments
#'
#' This data set stems from an animal experiment described in Stankovic (2018).
#' In particular, the data in question is shown in Figure 6J and 6K.
#'
#' @details
#' The data were read directly from the survival plots in the publication with
#' the help of Plot Digitizer, version 2.6.9.
#'
#' @format A data frame with 45 rows and 5 variables:
#' \describe{
#'   \item{Figure}{The figure in the publication where the data is shown}
#'   \item{Time}{Survival in days}
#'   \item{Status}{Right-censor status: 1 means observed event}
#'   \item{Group}{Experimental group identifier}
#'   \item{Colour}{Colour used in the Stankovic publication to mark this group}
#' }
#' @source Dudvarski Stankovic N, Bicker F, Keller S, et al. EGFL7 enhances surface expression of integrin a5b1 to promote angiogenesis in malignant brain tumors. EMBO Mol Med. 2018;10(9):e8420. doi:10.15252/emmm.201708420 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6127886/
"stankovic"


#' Serial interval times for measles during a long journey on a sailing vessel
#'
#' Measles broke out during a sailing ship passage from England to Australia
#' involving six persons on the ship. Serial interval times, i.e. the time of
#' clinical onset between successive cases in a chain of transmission, are
#' provided under the assumption that all transmission pairs represent direct
#' transmissions, excluding co-primary cases and asymptomatic intermediaries.
#' The interval times for the first three cases was not observed
#' completely as they brought measles on board with serial interval times unknown.
#'
#' @details
#' In 1829, the British sailing vessel HMS America carried 176 prisoners from
#' England to New South Wales, Australia. Alexander Stewart, the ship surgeon,
#' recorded cases of measles during the passage in his medical journal.
#' The journey started on 4 March 1829. A guard, who embarked from Chatham
#' (England), was the first measles case when the ship berthed at Woolwich
#' (England) on 28 March 1829. The measles began affecting children of the
#' guards on 31 March 1829 and spreading to some of the soldiers, later.
#'
#' In the medical journal, it is not clearly said if clinical onset is
#' defined as fever or rash. The first generation of measles on the sailer
#' comprises three cases for which we assume the minimum serial interval time
#' for measles which is generally estimated to be six days (for both,
#' fever-to-fever and for rash-to-rash).
#'
#' @format A data frame with 6 rows and 4 variables:
#' \describe{
#'   \item{generation}{Disease generation on sailer. Source cases are generation 0.}
#'   \item{symptomOnset}{Days of first symptoms since the start of the journey}
#'   \item{serialInterval}{Days between successive cases in chain of
#'   transmission, from symptom to symptom}
#'   \item{status}{Status indicator for serial interval time: 0 right-censored
#'   vs 1 for observed}
#' }
#' @references Paterson BJ, Kirk MD, Cameron AS, et al. Historical data and
#'   modern methods reveal insights in measles epidemiology, BMJ Open
#'   2013;3:e002033. \doi{10.1136/bmjopen-2012-002033}
#' @references Fine PE. The interval between successive cases of an infectious
#'   disease. Am J Epidemiol. 2003;158(11):1039-1047. \doi{10.1093/aje/kwg251}
#' @source Records of the Admiralty, Naval Forces, Royal Marines, Coastguard,
#'   and related bodies, ADM 101/2/3,
#'   `https://discovery.nationalarchives.gov.uk/details/r/C4106406`
"measles_sailer"


#' Relapse-free survival of melanoma patients under adjuvant treatment
#'
#' Data stem from a double-blind, placebo-controlled phase 3 trial where n=870
#' patients with completely resected, stage III melanoma with BRAF V600E or
#' V600K mutations were randomly assigned to receive oral Dabrafenib plus
#' Trametinib (combination therapy, 438 patients) or two matched placebo tablets
#' (432 patients) for 12 months.
#'
#' @details
#' Unfortunately, the data set of the clinical trial is not publicly available.
#' Instead, the data were digitized by Sean Devlin based on the published
#' survival plot (see Fig 1A in the original publication of the trial results).
#' Therefore, the data given here do **not** claim to be a 100% faithful
#' representation of the clinical trial. Some deviations are to be
#' expected.
#'
#' @format A data frame with 870 rows and 4 variables:
#' \describe{
#'   \item{ID}{artificially generated patient ID}
#'   \item{time}{Time to relapse-free survival in months}
#'   \item{status}{Status of observation, encoded as 0 for right-censoring vs 1 for RFS-event}
#'   \item{trtmt}{Treatment group: Dabrafenib+Trametinib vs Placebo}
#' }
#' @source Long GV, Hauschild A, Santinami M, et al. Adjuvant Dabrafenib plus Trametinib in Stage III BRAF-Mutated Melanoma. N Engl J Med. 2017;377(19):1813-1823. doi:10.1056/NEJMoa1708539
#' @source Devlin SM and O'Quigley J, The nph2ph-transform: applications to the statistical analysis of completed clinical trials, arXiv:2407.18905, 2024. doi:10.48550/arXiv.2407.18905.
"long2017"


#' Small data sets from miscellaneous publications
#'
#' @description
#' Most data sets come from publications about parameter estimation in Weibull
#' models. See the references below, in section "Source".
#'
#' @aliases rockette fatigue susquehanna pollution graphite
#' @details
#' The following small data sets are provided as numeric vectors.
#' \describe{
#'   \item{`rockette`:}{Artificial sample of length 4 given by Rockette. The maximum likelihood function has two stationary points, none of them is the global maximum.}
#'   \item{`fatigue`:}{Fatigue times of ten bearings of a specific type in hours.}
#'   \item{`susquehanna`:}{Maximum flood levels (in millions of cubic feet per second) for the Susquehanna River of Harrisburg (Pennsylvania, USA) over 20 4-year periods.}
#'   \item{`pollution`:}{Beach pollution levels in South Wales (measured in number of coliform per 100 ml) on 20 days over a 5-week period.}
#'   \item{`graphite`:}{Breaking stress (in MPa x 10^6) of 41 beam specimens cut from a single graphite H590 block, from a reliability study reported by Margetson & Cooper (1984), cited by Cheng & Stephen (1989)}
#' }
#'
#' @source Different publications related to estimating Weibull data.
#' @references McCool, J.I., 1974. Inferential techniques for Weibull populations. Technical Report TR 74-0180, Wright Patterson Air Force Base, Ohio.
#' @references Rockette, H., 1974. Maximum Likelihood Estimation with the Weibull Model.
#' @references Dumonceaux, R. and Antle, C. E., 1973. Discrimination between the lognormal and the Weibull distributions. Technometrics, 15, 923-926.
#' @references Steen, P. J. and Stickler, D. J., 1976. A Sewage Pollution Study of Beaches from Cardiff to Ogmore. Report January 1976, Cardiff: Department of Applied Biology, UWIST.
#' @references Cheng, R.C.H. and Stephen, M.A., 1989. A Goodness of Fit Test Using Moran’s Statistic with Estimated Parameters. Biometrika, 76, 386-392.
"rockette74"


#' @rdname rockette74
"fatigue"

#' @rdname rockette74
"susquehanna"

#' @rdname rockette74
"pollution"

#' @rdname rockette74
"graphite"
