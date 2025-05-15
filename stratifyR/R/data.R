#' Micronutrient data on Anaemia in Fiji
#'
#' The Anaemia data comes from the Fiji National Nutritional Survey
#' in 2004 on the "Micronutrient Status of Women in Fiji".
#'
#' @usage data(anaemia)
#'
#' @format A population data frame with 724 rows on some of the key
#' components collected in the survey. The variables are:
#'
#' \describe{
#'   \item{\code{Haemoglobin}}{Level of Haemoglobin (mmol/L)}
#'   \item{\code{Iron}}{Level of Iron (ng/mL)}
#'   \item{\code{Folate}}{Level of Folate (mmol/L)}
#' }
#'
#' @source This survey was conducted by the Ministry of Heath in Fiji. More details can be found at: \url{https://ghdx.healthdata.org/record/fiji-national-nutrition-survey-2004}

#'
#' @examples data(anaemia)
#' head(anaemia)
#' Iron <- anaemia$Iron
#' min(Iron); max(Iron)
#' hist(anaemia$Haemoglobin)
#' boxplot(anaemia$Folate)
#'
"anaemia"
#' Household Income Expenditure Survey (HIES) in Fiji
#'
#' The hies data comes from the HIES survey conducted
#' in Fiji in the year 2010. The data contains only two aspects
#' of the survey.
#'
#' @usage data(hies)
#'
#' @format A data frame with 3566 observations on two of
#' the major quantities collected in the survey. The
#' variables are:
#'
#' \describe{
#'   \item{\code{Expenditure}}{Level of expenditure (FJD)}
#'   \item{\code{Income}}{Level of income (FJD)}
#' }
#'
#' @source This survey was conducted in 2010 by the Bureau of Statistics (FIBoS) - Fiji Government.
#'
#' @examples data(hies$Income)
#' min(hies$Income); max(hies$Income)
#' hist(hies$Income)
#' boxplot(hies$Income)
#'
"hies"
#' Mathematics Marks for First-year University Students
#'
#' The data contains the mathematics coursework marks, final
#' examination marks and grades obtained by students in a
#' first year mathematics course at The University
#' level in the year 2010 in Fiji.
#'
#' @usage data(math)
#'
#' @format A data frame with 353 observations which represent
#' mathematics marks and grades for first year math students at
#' university level. The variable is as follows:
#'
#' \describe{
#' \item{\code{cw}}{Coursework marks in 1st year mathematics (0-50)}
#' \item{\code{end_exam}}{The end of semester examination marks
#' maths (0-50)}
#' \item{\code{final_marks}}{Final examination marks in
#' maths, which is an addition of the cw and end_exam (0-100)}
#' \item{\code{grade}}{The grade obtained by the student based on the final marks}}
#'
#' @source The data was obtained by a masters students at USP, Fiji.
#'
#' @examples data(math)
#' min(math$final_marks); max(math$final_marks)
#' hist(math$final_marks)
#' boxplot(math$final_marks)
#'
"math"
#' Sugarcane Farming Data in Fiji
#'
#' The sugarcane data shows the disposition area (land
#' area under cane) for individual sugarcane farms
#' and their cane productions with the incomes/earnings
#' for the year 2010 in Fiji.
#'
#' @usage data(sugarcane)
#'
#' @format A data frame with 13894 observations corresponding to individual farms.
#' The following are the variables:
#'
#' \describe{
#' \item{\code{DispArea}}{Disposition area (or land area under cane) (hactares)}
#' \item{\code{Production}}{The amount of sugarcane produced in the farm (tonnes)}
#' \item{\code{Income}}{Net income or money paid to farmers) (in FJD)}}
#'
#' @source This data was obtained from the Fiji Sugar Corporation in Fiji.
#'
#' @examples data(sugarcane$Production)
#' head(sugarcane$Production)
#' Production <- sugarcane$Production
#' min(Production); max(Production)
#' hist(Production)
#' boxplot(Production)
#'
"sugarcane"
