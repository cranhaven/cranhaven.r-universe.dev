#' Master's students' employment condition
#'
#' `data.frame` with 3282 cross-sectional observations of 7 variables about 
#' employment condition of master's students one year after graduation. Data
#' are used in \insertCite{santi2019;textual}{plot3logit} and refer to students
#' graduated at the University of Trento (Italy) between 2009 and 2013.
#'
#' @name cross_1year
#' @docType data
#'
#' @format
#' `data.frame` with 3282 observations of 7 variables:
#' \describe{
#' \item{employment_sit:}{employment situation, a
#'   factor with three levels: *Employed*, *Unemployed*, *Trainee*.}
#' \item{gender:}{gender, a factor with two levels: *Male*, *Female*.}
#' \item{finalgrade:}{final grade degree, a factor with three levels:
#'   *Low*, *Average*, *High*.}
#' \item{duration:}{duration of studies, a factor with three levels:
#'   *Short*, *Average*, *Long*.}
#' \item{social_class:}{social class, a factor with five levels:
#'   *Working class*, *White-collar workers*, *Lower middle class*,
#'   *Upper middle class*, *Unclassified*.}
#' \item{irregularity:}{irregularity indicator of student's studies,
#'   a factor with three levels: *Low*, *Average*, *High*.}
#' \item{hsscore:}{high school final score, a numeric between 60 and
#'   100.}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @keywords data
NULL





#' Self-reported votes from VOTER Survey in 2016
#'
#' Dataset based on self-reported votes from 2016 VOTER Survey by
#' \insertCite{dfvsg2017;textual}{plot3logit}, as used in the examples in
#' \insertCite{santi2022;textual}{plot3logit}.
#' 
#' Object `USvote2016` includes only
#' few variables based on the result of the survey, which are publicly available
#' online. See file `"data-raw/USvote2016_prepare.R"` in the GitHub repository
#' `"f-santi/plot3logit"` (\url{https://github.com/f-santi/plot3logit}), where
#' it is documented how the dataset `USvote2016` has been generated.
#' 
#' @name USvote2016
#' @docType data
#'
#' @format
#' `tibble` (`data.frame`) with 8000 observations of 7 variables:
#' \describe{
#' \item{idcode:}{voter identifier (integer).}
#' \item{vote:}{declared vote, afactor with three levels: *Clinton*, *Trump*,
#'   *Other*.}
#' \item{race:}{race, a factor with six levels: *White*, *Black*, *Hispanic*,
#'   *Asian*, *Mixed*,
#'   *Other*.}
#' \item{educ:}{level of education, a factor with six levels: *No high school*,
#'   *High school grad.*, *Some college*, *2-year college*, *4-year college*,
#'   *Post-grad*.}
#' \item{gender:}{gender, a factor with four levels: *Male*, *Female*,
#'   *Skipped*, *Not Asked*.}
#' \item{birthyr:}{decades when the voter was born, a factor with six levels:
#'   *[1920,1940)*, *[1940,1950)*, *[1950,1960)*, *[1960,1970)*, *[1970,1980)*,
#'   *[1980,2000)*.}
#' \item{famincome:}{income (in USD) of voter's family, a factor with five
#'   levels: *[0; 30,000)*, *[30,000; 60,000)*, *[60,000; 100,000)*,
#'   *[100,000; 150,000)*, *[150,000; Inf)*.}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @keywords data
NULL




