#' Krackhardt High-Tech Managers Advice Network
#'
#' Advice-seeking relationships among 21 managers in a high-tech company.
#' Data represents who seeks advice from whom (directed network).
#'
#' @format A data frame with 441 rows and 3 variables:
#' \describe{
#'   \item{from}{Integer, source node ID (1-21)}
#'   \item{to}{Integer, target node ID (1-21)}
#'   \item{value}{Integer, 1 if advice relationship exists, 0 otherwise}
#' }
#' @source Krackhardt, D. (1987). Cognitive social structures. Social Networks, 9(2), 109-134.
#' @references 
#' Krackhardt, D. (1987). Cognitive social structures. Social Networks, 9(2), 109-134.
"krackhardt_advice"

#' Krackhardt High-Tech Managers Friendship Network
#'
#' Friendship relationships among 21 managers in a high-tech company.
#' Data represents who considers whom a friend (directed network).
#'
#' @format A data frame with 441 rows and 3 variables:
#' \describe{
#'   \item{from}{Integer, source node ID (1-21)}
#'   \item{to}{Integer, target node ID (1-21)}
#'   \item{value}{Integer, 1 if friendship exists, 0 otherwise}
#' }
#' @source Krackhardt, D. (1987). Cognitive social structures. Social Networks, 9(2), 109-134.
"krackhardt_friendship"

#' Krackhardt High-Tech Managers Reporting Network
#'
#' Formal reporting relationships among 21 managers in a high-tech company.
#' Data represents who reports to whom (directed network).
#'
#' @format A data frame with 441 rows and 3 variables:
#' \describe{
#'   \item{from}{Integer, source node ID (1-21)}
#'   \item{to}{Integer, target node ID (1-21)}
#'   \item{value}{Integer, 1 if reporting relationship exists, 0 otherwise}
#' }
#' @source Krackhardt, D. (1987). Cognitive social structures. Social Networks, 9(2), 109-134.
"krackhardt_reports"

#' Krackhardt High-Tech Managers Attributes
#'
#' Node attributes for the 21 managers in Krackhardt's high-tech company study.
#' Contains demographic and organizational information for each manager.
#'
#' @format A data frame with 21 rows and 5 variables:
#' \describe{
#'   \item{ID}{Integer, manager ID (1-21)}
#'   \item{AGE}{Numeric, age of the manager}
#'   \item{TENURE}{Numeric, tenure of the manager in the company (in years)}
#'   \item{LEVEL}{Factor, hierarchical level of the manager (e.g., "Top", "Middle", "Lower")}
#'   \item{DEPT}{Factor, department of the manager (e.g., "R&D", "Marketing", etc.)}
#' }
#' @source Krackhardt, D. (1987). Cognitive social structures. Social Networks, 9(2), 109-134.
"krackhardt_attributes"

#' Krackhardt Friendship Network Perception Errors
#'
#' Individual perceptions of the Krackhardt friendship network containing
#' systematic perception errors and biases.
#'
#' @format A list of 21 matrices (21x21 each) representing individual perceptions
#' @source Generated perception errors based on Krackhardt friendship data
"krackhardt_friendship_perceptions"

#' Krackhardt Advice Network Perception Errors  
#'
#' Individual perceptions of the Krackhardt advice network containing
#' systematic perception errors and biases.
#'
#' @format A list of 21 matrices (21x21 each) representing individual perceptions
#' @source Generated perception errors based on Krackhardt advice data
"krackhardt_advice_perceptions"