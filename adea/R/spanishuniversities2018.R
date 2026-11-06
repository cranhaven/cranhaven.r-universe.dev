#'  A data set of Spanish public universities
#' 
#' Data set \code{spanishuniversities2018} belongs to 47 public Spanish universities, all but \code{Pais Vasco} because its staff data is missing.
#' 
#' @name spanishuniversities2018
#' @docType data
#' @keywords data
#' @seealso See also \code{\link{adea-package}}.
#' 
#' @section Usage:
#' data('spanishuniversities2018')
#' 
#' @format 
#' \code{TeachingStaff}: number of people teaching and researching with a doctor degree.
#' 
#' \code{FPU2018}: number of predoctoral contracts with state funds done in 2018.
#'
#' \code{FPI2018}: number of postdoctoral contracts with state funds done in 2018.
#'
#' \code{Patents}: number of patents registered from 2013 and 2017.
#' 
#' \code{PhDThesis}: number of PhD Thesis in 2017/2018 academic year.
#' 
#' \code{JCR}: number of papers published in journals included in JCR index
#' 
#' \code{Six}: number of positive reports obtained in the state research evaluation program. All permanent teaching staff from Spanish universities can apply every six year for a program to get a positive evaluation of their research activities. Positive evaluations are considered merits for promotion and represent an increase in salary.
#' 
#' \code{Projects}: number of research projects financed with state funds obtained in competitive calls.
#' 
#' @details
#' The purpose of this dataframe is to be used in DEA (Data Envelopment Analysis).
#' 
#' The only one input variable is \code{TeachingStaff}.
#' 
#'
#' The output variables are: \code{TeachingStaff}, \code{FPU2018}, \code{FPI2018}, \code{Patens}, \code{PhDThesis}, \code{JCR}, \code{Six} and \code{Projects}.
#' 
#' @source
#' Data are taken from public available information systems. 
NULL
