#' Female spotted hyena dominance data
#' 
#' Data structures for inferring the ranks of adult female spotted hyenas
#' (Crocuta crocuta) from a single social group in the Maasai Mara National
#' Reserve in southern Kenya. Data are from the Talek clan collected between
#'  1988 and 1995 by the Mara Hyena Project.
#' 
#' @format List of 3 elements:
#'  \describe{
#'    \item{initial.ranks}{Starting order of females in 1988.}
#'    \item{contestants}{Dataframe of 182 rows and 4 variables.
#'    There is one row per female per study year. \strong{id} is the identity 
#'    of the contestant. \strong{period} is the study year. \strong{convention1} is
#'    the mother of the contestant, because female spotted hyena hierarchies are 
#'    structured by maternal rank inheritance. \strong{convention2} is the
#'    intra-litter rank for contestants who were part of a twin litter.}
#'    \item{interactions}{Dataframe of 2043 rows and 3 variables. 
#'    Each row corresponds to the outcome of one aggressive interaction between 
#'    adult females.}
#'  }
#'  
"C.crocuta.female"