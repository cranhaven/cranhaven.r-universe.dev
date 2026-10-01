#' Simulated cross-study data
#'
#' Data are simulated as part of a larger study 
#' (Curran et al., 2016; Curran et al., under review).
#' Meant to simulate a dataset pooled across two studies, with 
#' 12 indicators and 3 moderators (age, gender, and study).
#' Impact and DIF exist on the basis of these moderators.
#'
#' @docType data
#'
#' @usage data(xstudy)
#'
#' @format A data frame with 500 rows and 25 columns. The 25 variables are:
#' \describe{
#' \item{ID}{Unique identifier}
#' \item{AGE}{Age in years, centered around age 13}
#' \item{GENDER}{Effect-coded gender}
#' \item{STUDY}{Effect-coded study membership}
#' \item{STUDYAGE}{Interaction between age and study}
#' \item{TRUEETA}{True score on latent variable for each subject -- not used in analysis}
#' \item{STUDYETA}{Interaction between study and score -- not used in analysis}
#' \item{ZETA}{Deviation score -- not used in analysis}
#' \item{W}{External covariate for original simulation -- not used in analysis}
#' \item{Z1}{External outcome for original simulation -- not used in analysis}
#' \item{Z2}{External outcome for original simulation -- not used in analysis}
#' \item{Z3}{External outcome for original simulation -- not used in analysis}
#' \item{Z4}{External outcome for original simulation -- not used in analysis}
#' \item{BIN_1}{Binary item 1}
#' \item{BIN_2}{Binary item 2}
#' \item{BIN_3}{Binary item 3}
#' \item{BIN_4}{Binary item 4}
#' \item{BIN_5}{Binary item 5}
#' \item{BIN_6}{Binary item 6}
#' \item{BIN_7}{Binary item 7}
#' \item{BIN_8}{Binary item 8}
#' \item{BIN_9}{Binary item 9}
#' \item{BIN_10}{Binary item 10}
#' \item{BIN_11}{Binary item 11}
#' \item{BIN_12}{Binary item 12}
#' }
#'
#' @keywords datasets
#'
#' @references Curran et al., 2016 Structural Equation Modeling 23(6), 827-844.
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5526637/}{PubMed})
#'
#' @source Curran et al., 2016 (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5526637/}{PubMed})
#'
"xstudy"