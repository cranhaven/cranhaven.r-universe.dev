## DOCVIS ======================================================================
#' U.S. Medical Expenditure Panel Survey
#' 
#' Cross sectional sample for 2003.
#' 
#' @format A data frame with 3677 rows and 22 variables.
#' \describe{
#'  \item{offer}{Binary: employer offers insurance}
#'  \item{ssiratio}{Ratio of SSI income to total income}
#'  \item{age}{Age in years}
#'  \item{educyr}{Years of education}
#'  \item{physician}{Number of visits to doctor}
#'  \item{nonphysician}{Number of visits to health professional (not doctor)}
#'  \item{medicaid}{Binary: has medicaid public insurance}
#'  \item{private}{Binary: has private supplementary insurance}
#'  \item{female}{Binary: female}
#'  \item{phylim}{Binary: physical limitation}
#'  \item{actlim}{Binary: activity limitation}
#'  \item{income}{Income (in thousands)}
#'  \item{totchr}{Number of chronic conditions}
#'  \item{insured}{Same as `private`}
#'  \item{age2}{`age` squared}
#'  \item{linc}{\code{log(income)}}
#'  \item{bh}{Binary: black or hispanic}
#'  \item{docvis}{Number of doctor visits}
#'  \item{ldocvis}{\code{log(docvis)} if \code{docvis > 0}}
#'  \item{ldocvisa}{\code{log(docvis + .01)}}
#'  \item{one}{Constant term (1)}
#'  \item{docbin}{Binary: \code{docvis > 0}}
#' }
#' 
#' @source \url{https://www.stata-press.com/data/mus2.html} \code{mus220mepsdocvis.dta}
#' 
#' @references 
#' Cameron, A. C., and P. K. Trivedi. 2022. \emph{Microeconometrics Using Stata, 
#' Second Edition}. Volumes I and II. College Station, TX: Stata Press.
#' 
"docvis"

## MROZ ========================================================================
#' University of Michigan Panel Study of Income Dynamics (PSID)
#' 
#' Sample of 753 between the ages of 30 and 60 in 1975 (PSID interview year 1976)
#' 
#' @format A data frame with 753 observations on 22 variables.
#' \describe{
#'    \item{inlf}{Binary: in labor force in 1975}
#'    \item{hours}{Hours worked in 1975}
#'    \item{kidslt6}{Number of children less than 6 years old}
#'    \item{kidsge6}{Number of children 6 years old or older}
#'    \item{age}{Woman's age in years}
#'    \item{educ}{Woman's years of education}
#'    \item{wage}{Woman's estimated average hourly earnings}
#'    \item{repwage}{Representative wage in 1976 (interview year)}
#'    \item{hushrs}{Hours worked by husband in 1975}
#'    \item{husage}{Husband's age in years}
#'    \item{huseduc}{Husband's years of education}
#'    \item{huswage}{Husband's estimated average hourly earnings}
#'    \item{faminc}{Total family income}
#'    \item{mtr}{Woman's Federal marginal income tax rate}
#'    \item{motheduc}{Mother's years of education}
#'    \item{fatheduc}{Father's years of education}
#'    \item{unem}{Unemployment rate in county of residence}
#'    \item{city}{Binary: lives in Standard Metropolitan Statistical Area (SMSA)}
#'    \item{exper}{Years of labor market experience}
#'    \item{nwifeinc}{Income from other sources than the wife's labor, in thousands}
#'    \item{lwage}{\code{log(wage)}}
#'    \item{expersq}{\code{exper^2}}
#' }
#' 
#' @references 
#' Wooldridge, J. M. (2020). \emph{Introductory Econometrics: A Modern Approach}. 
#' 7th Edition. Boston, MA: Cengage Learning.
#' 
#' Mroz, T. A. (1987). "The Sensitivity of an Empirical Model of Married Women's 
#' Hours of Work to Economic and Statistical Assumptions." 
#' \emph{Econometrica} 55, 765-799.
#'
#' @source \url{https://cran.r-project.org/package=wooldridge}
"mroz"

## PW401K ======================================================================
#' 401(k) Participation Rates
#'
#' A dataset containing participation rates and firm characteristics used
#' in Papke and Wooldridge (1996).
#'
#' @format A data frame with 4734 rows and 5 variables:
#' \describe{
#'   \item{prate}{participation rate, proportion}
#'   \item{mrate}{matching rate, proportion}
#'   \item{totemp}{total number of employees}
#'   \item{age}{years the plan has been in place}
#'   \item{sole}{binary indicating if it's sole plan offered by employer}
#' }
#' 
#' @source Papke, L. E. and Wooldridge, J. M. (1996).
#' 
#' @references
#' Papke, L. E., & Wooldridge, J. M. (1996). "Econometric methods for fractional 
#' response variables with an application to 401(k) plan participation rates." 
#' \emph{Journal of Applied Econometrics}, 11(6), 619-632. 
#' \doi{10.1002/(SICI)1099-1255(199611)11:6<619::AID-JAE418>3.0.CO;2-1}
"pw401k"

## SMOKE =======================================================================
#' 1979 National Health Interview Survey
#' 
#' Sample of males in 1979 and early 1980 from the smoking supplement.
#' 
#' @format A data frame with 807 observations and 10 variables.
#' \describe{
#'    \item{educ}{Years of education}
#'    \item{cigpric}{State's cigarette price (cents per pack)}
#'    \item{white}{Binary: white}
#'    \item{age}{Age in years}
#'    \item{income}{Annual income in dollars}
#'    \item{cigs}{Number of cigarettes smoked per day}
#'    \item{restaurn}{Binary: restaurant restrictions on smoking}
#'    \item{lincome}{\code{log(income)}}
#'    \item{agesq}{\code{age^2}}
#'    \item{lcigpric}{\code{log(cigprice)}}
#' }
#' 
#' @references 
#' Wooldridge, J. M. (2020). \emph{Introductory Econometrics: A Modern Approach}. 
#' 7th Edition. Boston, MA: Cengage Learning.
#' 
#' Mullahy, J. (1997). "Instrumental-Variable Estimation of Count Data Models: 
#' Applications to Models of Cigarette Smoking and Infant Health." 
#' \emph{Review of Economics and Statistics} 79, 586-593.
#'
#' @source \url{https://cran.r-project.org/package=wooldridge}
"smoke"