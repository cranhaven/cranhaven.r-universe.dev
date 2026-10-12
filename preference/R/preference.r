
#' @title Design and Analysis of Two-stage Randomized Clinical Trials
#' @name preference-package
#' @docType package
#' @rdname preference-package
#' @aliases preference-package
#' @description The \pkg{preference} package is used for the design and 
#' analysis of two-stage randomized trials with a continuous outcome 
#' measure. In this study, patients are first randomized to either a 
#' random or choice arm. Patients 
#' initially randomized to the choice arm are allowed to select their preferred
#' treatment from the available treatment options; patients initially randomized
#' to the random arm undergo a second randomization procedure to one of the 
#' available treatment options. The design has also been extended to include
#' important stratification variables; the functions provided in this package
#' can accommodate both the unstratified and stratified designs.
#' 
#' In this study, there are three effects that may be of interest. The treatment
#' effect captures the difference in outcome between patients randomized to 
#' treatment A and treatment B (similar to a traditional RCT). The selection 
#' effect captures the difference in outcome between patients that prefer 
#' treatment A and patients that prefer treatment B, regardless of the 
#' treatment that is actually received. Finally, the preference effect compares
#' the outcomes of patients who receive their preferred treatment (either 
#' treatment A or treatment B) and patients who do not receive their preferred
#' treatment. 
#' 
#' To aid in the design of these two-stage randomized studies, sample size 
#' functions are provided to determine the necessary sample size to 
#' detect a particular selection, preference, and/or treatment effect. If the 
#' sample size is fixed prior to the start of the study, functions are provided
#' to calculate the study power to detect each effect. Finally, the 
#' \code{optimal_proportion} function can be used to determine the optimal 
#' proportion of patients randomized to the choice arm in the initial 
#' randomization. 
#' 
#' To analyze the data from the two-stage randomized trial, two analysis
#' functions are provided. The function \code{preference} computes the 
#' test statistic and p-value for each effect given provided raw study data. 
#' The function \code{fit_preference_summary} uses provided summary data (mean, 
#' variance, and sample size) of each study group to compute the test statistic
#' and p-value of each effect. The test statistics can be accessed from
#' the models using the \code{summary()} function.
#' 
#' Preference Trial Function Calls:
#' \itemize{
#'    \item{preference.trial: }{construct a \code{preferene.trial} based
#'    on effect and sample sizes.}
#'    \item{pt_from_power: }{construct a \code{preference.trial} based on
#'    power and  effect size.}
#'    \item{pt_from_ss: }{construct a \code{preference.trial} based on
#'    sample size}
#' }
#' 
#' Analysis Function Calls
#' \itemize{
#' \item{preference and fit_preference: }{computes test statistic and 
#' p-value for observed #' selection, preference, and treatment effects 
#' using provided raw data}
#' \item{fit_preference_summary: }{computes test statistic and p-value for 
#' observed selection, preference, and treatment effects using provided 
#' summary data (mean, variance, sample size)}
#' }
#' 
#' Other Function Calls
#' \itemize{
#' \item{treatment_effect_size: }{computes the treatment effect that can be 
#' detected given a specified sample size and power}
#' \item{optimal_proportion: }{computes the optimal proportion randomized to 
#' choice arm (defined for unstratified design only)}
#' \item{effects_from_means: }{computes the treatment, selection, and 
#' preference effect sizes provided the study means in each treatment arm}
#' }
#' 
#' Data Sets
#' \itemize{
#' \item{imap: }{summary SF36 outcome data for the two-stage randomized IMAP 
#' study}
#' \item{imap_strat: }{summary SF36 outcome data for the two-stage randomized 
#' IMAP study stratified by high vs. low STAI score}
#' }
#' 
#' Acknowledgments: This work was partially supported through a 
#' Patient-Centered Outcomes Research Institute (PCORI) Award (ME-1511-32832) 
#' and Yale's CTSA Award (Ul1TR001863).  We would also like to thank the IMAP 
#' team for sharing their data to demonstrate this package.
#'
#' Disclaimer: All statements in this report, including its findings and 
#' conclusions, are solely those of the authors and do not necessarily 
#' represent the views of the Patient-Centered Outcomes Research Institute 
#' (PCORI), its Board of Governors or Methodology Committee.
#' 
#' @references Rucker G (1989). "A two-stage trial design for testing treatment,
#' self-selection and treatment preference effects." \emph{Stat Med}, 
#' \strong{8}(4):477-485. 
#' (\href{https://pubmed.ncbi.nlm.nih.gov/2727471}{PubMed})
#' @references McCaffery et al. (2010) "Psychosocial outcomes of three triage 
#' methods for the management of borderline abnormal cervical smears: an open 
#' randomised trial." \emph{BMJ}, \strong{340}:b4491.
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2827716/}{PubMed})
#' @references Walter et. al. (2011). "Optimal allocation of participants for
#' the estimation of selection, preference and treatment effects in the 
#' two-stage randomised trial design." \emph{Stat Med}, 
#' \strong{31}(13):1307-1322.
#' (\href{https://pubmed.ncbi.nlm.nih.gov/22362374/}{PubMed})
#' @references McCaffery et al. (2011) "Determining the Impact of Informed 
#' Choice: Separating Treatment Effects from the Effects of Choice and Selection
#' in Randomized Trials." \emph{Med Decis Making}, \strong{31}(2):229-236.
#' (\href{https://pubmed.ncbi.nlm.nih.gov/21041538/}{PubMed})
#' @references Turner RM, et al. (2014). "Sample Size and Power When Designing
#'  a Randomized Trial for the Estimation of Treatment, Selection, and 
#'  Preference Effects." \emph{Medical Decision Making}, \strong{34}:711-719.
#' (\href{https://pubmed.ncbi.nlm.nih.gov/24695962}{PubMed})
#' @references Cameron B, Esserman D (2016). "Sample Size and Power for a 
#' Stratified Doubly Randomized Preference Design." 
#' \emph{Stat Methods Med Res}.  
#' (\href{https://pubmed.ncbi.nlm.nih.gov/27872194}{PubMed})
NULL

#' @name imap_stratified_summary
#' @rdname imap
NULL

#' @name imap_summary
#' @rdname imap
NULL

#' Data from the IMAP study
#'
#' @name imap 
#' @aliases imap_stratified_summary imap_summary
#' @docType data
#' @rdname imap
#' @description 
#' The ``Improving Management of Abnormal Pap Smears'' study used a 
#' two-stage randomized preference trial design to evaluate psychosocial
#' outcomes in women found to have atypical cells in a Pap Smear.
#' Two systems for managing the atypical cells were tested (repeated Pap smears 
#' or HCV triage) and a doubly randomized design was used to evaluate the role 
#' of patient preference. The data set provides mean, standard deviation and 
#' sample sizes of the SF36 outcome for each treatment in both the choice and 
#' random arms.
#'
#' Three data sets are provided with the preference package based on the
#' IMAP study. The first, \code{imap_summary} provides summary statistics
#' of the entire trial. The second \code{imap_summary_stratified}, summary
#' statistics of the study per strata. The third \code{imap} is a resampled
#' version of the individual level data including stratification. Each
#' of these data sets are compatible with the analysis functions 
#' \code{fit_preference_summary}, \code{fit_preference}, and 
#' \code{preference}, provided in this package. The examples sections
#' in the documentation illustrate their use.
#
#' @references McCaffery et al. (2010) "Psychosocial outcomes of three triage 
#' methods for the management of borderline abnormal cervical smears: an open 
#' randomised trial." \emph{BMJ}, \strong{340}:b4491.
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2827716/}{PubMed})
#' @references McCaffery et al. (2011) "Determining the Impact of Informed 
#' Choice: Separating Treatment Effects from the Effects of Choice and Selection
#' in Randomized Trials." \emph{Med Decis Making}, \strong{31}(2):229-236.
#' (\href{https://pubmed.ncbi.nlm.nih.gov/21041538}{PubMed})
#' @keywords data
NULL


