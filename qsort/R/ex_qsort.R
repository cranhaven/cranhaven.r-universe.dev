#'Example datasets for qsort package
#'
#'A list containing four hypothetical Q-sort datasets:
#'Attachment Q-set (aqs; version 3.0) (Waters, 1995);
#'California Child Q-set (ccq; Block and Block, 1969);
#'Maternal Behaviour Q-set (version 3.1) (mbqs; Pederson et al., 1999);
#'Preschool Q-set (pq; Baumrind, 1968 revised by Wanda Bronson).
#'
#'For these examples, items were given random scores (1 to 9; least to most
#'characteristic respectively) following a rectangular distribution. For the ccq
#'Q-set this corresponds to placing 11 items in each of the 9 categories, plus
#'an additional one in the center category (i.e., 12 items in category 5). For
#'the remaining Q-sets all the 9 categories include the same number of items
#'(aqs = 10, mbqs = 10, pq = 8).
#'
#' @format Data frames with 10 rows and a variable number of columns, depending
#'   on the number of Q-set items (aqs = 90, ccq = 100, mbqs = 90, pq = 72)
#'   \describe{
#'   \item{participant}{Subject identification.}
#'   \item{classroom}{Group identification.}
#'   \item{qset1 to ...}{Items' scores.}
#'   }
#'
#'@references Baumrind, D. (1968). Manual for the Preschool Behaviour Q-set.
#'  Parental Research Project. Berkeley, CA: Institute of Human Development,
#'  University of California.
#'
#'  Block, J. H., & Block, J. (1969). The California Child Q-Set. Berkeley, CA:
#'  Institute of Human Development, University of California.
#'
#'  Pederson, D. R., Moran, G., & Bento, S. (1999). Maternal Behaviour
#'  Q-sort (version 3.1). London, ON: Psychology Department, Western University.
#'
#'  Waters, E. (1995). Appendix A: The attachment Q-set (Version 3. 0).
#'  Monographs of the Society for Research in Child Development, 60, 234-246.
"ex_qsort"



