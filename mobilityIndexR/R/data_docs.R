#' @title Example Income Dataset
#'
#' @description This is an example income dataset to measure income mobility.
#'
#' @format A \code{data.frame} with 13 columns:
#' \describe{
#'  \item{id}{A unique integer id for each row representing an individual}
#'  \item{cohort}{Year when first income is observed}
#'  \item{t0}{Income at time 0}
#'  \item{t1}{Income at time 1}
#'  \item{t2}{Income at time 2}
#'  \item{t3}{Income at time 3}
#'  \item{t4}{Income at time 4}
#'  \item{t5}{Income at time 5}
#'  \item{t6}{Income at time 6}
#'  \item{t7}{Income at time 7}
#'  \item{t8}{Income at time 8}
#'  \item{t9}{Income at time 9}
#'  \item{t10}{Income at time 10}
#'}
'incomeMobility'

#' @title Example Zero-Inflated Income Dataset
#'
#' @description These is an example income dataset to measure income mobility with an inflated number of zeros.
#' Zero-inflated data is often observed in practice and will cause difficulties for some transition matrix and mobility
#' index approaches.
#'
#' @format A \code{data.frame} with 13 columns:
#' \describe{
#'  \item{id}{A unique integer id for each row representing an individual}
#'  \item{cohort}{Year when first income is observed}
#'  \item{t0}{Income at time 0}
#'  \item{t1}{Income at time 1}
#'  \item{t2}{Income at time 2}
#'  \item{t3}{Income at time 3}
#'  \item{t4}{Income at time 4}
#'  \item{t5}{Income at time 5}
#'  \item{t6}{Income at time 6}
#'  \item{t7}{Income at time 7}
#'  \item{t8}{Income at time 8}
#'  \item{t9}{Income at time 9}
#'  \item{t10}{Income at time 10}
#' }
'incomeZeroInfMobility'

#' @title Example Grades Dataset
#'
#' @description This is an example grade dataset for measuring grade mobility.
#'
#' @format A \code{data.frame} with 13 columns:
#' \describe{
#'  \item{id}{A unique integer id for each row representing a student}
#'  \item{cohort}{Class of student}
#'  \item{t0}{Grade at time 0}
#'  \item{t1}{Grade at time 1}
#'  \item{t2}{Grade at time 2}
#'  \item{t3}{Grade at time 3}
#'  \item{t4}{Grade at time 4}
#'  \item{t5}{Grade at time 5}
#'  \item{t6}{Grade at time 6}
#'  \item{t7}{Grade at time 7}
#'  \item{t8}{Grade at time 8}
#'  \item{t9}{Grade at time 9}
#'  \item{t10}{Grade at time 10}
#'  }
'gradeMobility'
