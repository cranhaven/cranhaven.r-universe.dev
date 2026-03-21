#' Male spotted hyena dominance data
#' 
#' Data structures for inferring the ranks of adult male spotted hyenas
#' (Crocuta crocuta) from a single social group in the Maasai Mara National
#' Reserve in southern Kenya. Data are from the Talek clan collected between
#'  1988 and 1995 by the Mara Hyena Project.
#' 
#' @format List of 3 elements:
#'  \describe{
#'    \item{initial.ranks}{Starting order of males in 1988.}
#'    \item{contestants}{Dataframe of 143 rows and 3 variables.
#'    There is one row per male per study year. \strong{id} is the identity 
#'    of the contestant. \strong{period} is the study year. \strong{convention1} is
#'    the date that the male joined the clan, because male spotted hyena hierarchies
#'    are structured by a tenure-based convention.}
#'    \item{interactions}{Dataframe of 474 rows and 3 variables. 
#'    Each row corresponds to the outcome of one aggressive interaction between 
#'    adult males.}
#'  }
#'  
"C.crocuta.male"