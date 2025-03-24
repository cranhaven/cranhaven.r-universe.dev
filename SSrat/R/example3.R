#' Example 3 of rating data with more assessors than assessed
#' 
#' Number of assessors (rows) is larger then the number of assessed
#' (columns).\cr This data set has missing ratings. It is the result of
#' \code{readratdatafixed("<example3.rat.txt>")}. A 7-point rating scale has
#' been used. Each respondent is identified by a schoolid, a group id and a
#' respondent id. The rows contain the assessors, the columns contain the
#' assessed. When rater equals ratee (diagonal), the rating is NA.
#' 
#' 
#' @name example3.rat
#' @docType data
#' @format This dataframe has 7 rated persons, who are assessed by 10 raters.
#' \describe{ \item{schoolid}{a numeric vector, identifying the second
#' group level} \item{groupid}{a numeric vector, identifying the first
#' group level.} \item{respid}{a numeric vector, identifying the
#' individual.} \item{r01}{ratings received by respondent 1.}
#' \item{r02}{ratings received by respondent 2.}
#' \item{r03}{ratings received by respondent 3.}
#' \item{r04}{ratings received by respondent 4.}
#' \item{r05}{ratings received by respondent 5.}
#' \item{r06}{ratings received by respondent 6.}
#' \item{r07}{ratings received by respondent 7.} }
#' @note Rating data can be entered directly into a SSrat compliant dataframe,
#' using \code{\link{edit}}. Colums needed are: "schoolid", "groupid",
#' "respid", and for <n> raters "r01", "r02".."r<n>". Optionally, a column
#' named "resplabel" can be entered, containing an additional identifier of the
#' raters/assessed. The raters (assessors) are in rows and assessed in columns.
#' For example: \cr mydata=data.frame(schoolid=numeric(0), groupid=numeric(0),
#' respid=numeric(0),\cr r01=numeric(0), r02=numeric(0), r03=numeric(0));
#' mydata=edit(mydata)
#' @seealso \code{\link{readratdatafixed}} \code{\link{example1.rat}}
#' \code{\link{example1a.rat}} \code{\link{example2.rat}}
#' \code{\link{example3.rat}} \code{\link{example4.rat}}
#' \code{\link{example5.rat}} \code{\link{example6.rat}}
#' \code{\link{example7.rat}} \code{\link{klas2.rat}}
#' @keywords datasets
#' @examples
#' 
#' data(example3.rat)
#' 
NULL