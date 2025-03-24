#' Example klas2 of rating data with names of the raters (assessed) and some
#' missing values
#' 
#' Ratings with the names of the raters (and of the assessed as the members of
#' the group assess each other). Furthermore, some of these ratings are missing
#' (NA).\cr The result of \code{readratdatafixed("<klas2.rat.txt>")}. A 3-point
#' rating scale has been used. Numbers of assessors and assessed are equal
#' (11). Each respondent is identified by a respname, schoolid, a group id and
#' a respondent id. The rows contain the assessors, the columns contain the
#' assessed. When rater equals assessed (diagonal), the rating is NA.
#' 
#' 
#' @name klas2.rat
#' @docType data
#' @format A data frame with 10 observations of 9 ratings.  \describe{
#' \item{resplabel}{a string vector with an optional identifier of the
#' raters (and assessed).} \item{schoolid}{a numeric vector,
#' identifying the second group level} \item{groupid}{a numeric vector,
#' identifying the first group level.} \item{respid}{a numeric vector,
#' identifying the individual.} \item{r01}{ratings received by
#' respondent 1.} \item{r02}{ratings received by respondent 2.}
#' \item{r03}{ratings received by respondent 3.}
#' \item{r04}{ratings received by respondent 4.}
#' \item{r05}{ratings received by respondent 5.}
#' \item{r06}{ratings received by respondent 6.}
#' \item{r07}{ratings received by respondent 7.}
#' \item{r08}{ratings received by respondent 8.}
#' \item{r09}{ratings received by respondent 9.}
#' \item{r10}{ratings received by respondent 10.}
#' \item{r11}{ratings received by respondent 11.} }
#' @note Rating data can be entered directly into a SSrat compliant dataframe,
#' using \code{\link{edit}}. Colums needed are: "schoolid", "groupid",
#' "respid", and for <n> raters "r01", "r02".."r<n>". Optionally, a column
#' named "resplabel" can be entered, containing an extra identifier of the
#' raters/assessed. The raters (assessors) are in rows and assessed in columns.
#' For example: \cr mydata=data.frame(schoolid=numeric(0), groupid=numeric(0),
#' respid=numeric(0),\cr r01=numeric(0), r02=numeric(0), r03=numeric(0));
#' mydata=edit(mydata)\cr To allow for the combination of groups with different
#' sizes in a single file, it is important to enumerate the n respondents from
#' 1 to n, respectively use the columnnames r01 to rn for the received ratings.
#' These column names are padded with a zero (r01, r02 etc.) to allow for easy
#' ordening of these columns after a merge.
#' @seealso \code{\link{readratdatafixed}} \code{\link{calcallgroups}}
#' \code{\link{calcgroup}} \code{\link{example1.rat}}
#' \code{\link{example1a.rat}} \code{\link{example2.rat}}
#' \code{\link{example3.rat}} \code{\link{example4.rat}}
#' \code{\link{example5.rat}} \code{\link{example6.rat}}
#' \code{\link{example7.rat}} %%\code{\link{klas2.rat}}
#' @keywords datasets
#' @examples
#' 
#' data(klas2.rat)
#' 
NULL
