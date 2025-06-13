#' An Experimental Survey Measuring Plagiarism Using the Crosswise Model
#'
#' A dataset containing the responses to sensitive questions about plagiarism
#' and other attributes of 812 students. The crosswise model (CM) and direct questioning (DQ)
#' were utilized to gather the data. Each row holds the response to one question for one student.
#' The variables are as follows:
#'
#' \itemize{
#'   \item id. Identification code of the student
#'   \item question. Which question was asked (1 and 3: Partial Plagiarism, 2 and 4: Severe Plagiarism)
#'   \item response. Binary randomized response
#'   \item gender. Gender of the student (0: male, 1: female)
#'   \item age. Age in years
#'   \item nationality. Nationality of the student (0: German or Swiss, 1: other)
#'   \item no_papers. Number of papers
#'   \item uni. Location of data collection (1: ETH Zurich, 2: LMU Munich, 3: University Leipzig)
#'   \item course. Course in which the data was collected
#'   \item Aspired_Degree. Aspired degree of the student
#'   \item Semester. semesters enrolled
#'   \item ur_none. Used resources: none
#'   \item ur_books. Used resources: books
#'   \item ur_art. Used resources: articles
#'   \item ur_int. Used resources: internet
#'   \item ur_fsp. Used resources: fellow students' papers
#'   \item ur_other. Used resources: other
#'   \item preading. Proofreading
#'   \item gradesf. Satisfaction with grades
#'   \item pp. Plagiarism indicator (0: Severe Plagiarism, 1: Partial Plagiarism)
#'   \item RR. Randomized Response indicator (0: DQ, 1: Crosswise)
#'   \item RRp1. Randomized Response parameter p1
#'   \item RRp2. Randomized Response parameter p2
#'   \item RRmodel. Randomized Response Model
#' }
#'
#' @docType data
#' @keywords datasets
#' @name Plagiarism
#' @author Ben Jann and Laurcence Brandenberger
#' @references \doi{10.7892/boris.51190}
#' @usage data(Plagiarism)
#' @format A data frame in long format with 812 rows and 24 variables
NULL
