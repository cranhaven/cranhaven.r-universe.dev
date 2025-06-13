#' Data sets for the mouse body weight example
#'
#' A list of 3 data frames that contains the genotype, gene expression and body-mass index of 
#' 294 mice. The data is already screened for quality control. 
#' For the raw data see the links below. For more details see the reference. 
#'
#' @format A list of 3 data frames:
#' \describe{
#'   \item{geno}{Mouse genotype. A data frame of 294 rows and 163 columns.}
#'   \item{expr}{Mouse gene expression. A data frame of 294 rows and 215 columns.}
#'   \item{bmi}{Mouse body-mass index. A data frame of 294 rows and 1 column.}
#' }
#' 
#' @examples
#' ############## Mouse body weight example ######################
#' data(mouse)
#' expr <- scale(as.matrix(mouse$expr))
#' geno <- scale(as.matrix(mouse$geno))
#' bmi <- as.matrix(mouse$bmi)
#' mouse.X <- list(X1 = expr, X2 = geno)
#' \dontrun{
#'  mouse.cvr <- CVR(bmi, mouse.X, rankseq = 2, etaseq = 0.04, family = "g", penalty = "L1")
#'  plot(mouse.cvr)
#'  plot(expr %*% mouse.cvr$solution$W[[1]][, 2], geno %*% mouse.cvr$solution$W[[2]][, 2])
#'  cor(expr %*% mouse.cvr$solution$W[[1]], geno %*% mouse.cvr$solution$W[[2]]) 
#'  }
#' 
#' @references Chongliang Luo, Jin Liu, Dipak D. Dey and Kun Chen (2016) Canonical variate regression. 
#'               Biostatistics, doi: 10.1093/biostatistics/kxw001.
#' @source  Mouse genotype: \url{http://www.genetics.org/cgi/content/full/genetics.110.116087/DC1}
#'          Mouse gene expression: \url{ftp://ftp.ncbi.nlm.nih.gov/pub/geo/DATA/SeriesMatrix/GSE2814/}
#'          Mouse body-mass index: \url{http://labs.genetics.ucla.edu/horvath/CoexpressionNetwork/MouseWeight/}.
"mouse"