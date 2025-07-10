#' Synthetic dataset containing single nucleotide polymorphisms (SNP)
#'
#' The raw.data is the simulated dataset which consists of 3,000 independent SNPs
#' and 1,004 individuals belonging to one of 5 populations (200 individuals
#' each) and 4 outlying individuals. The matrix \code{raw.data} contains the number 0, 1,
#' and 2 representing SNP in additive coding. The pairwise genetic distance between
#' populations are listed below (see Balding, 1995):
#' \tabular{cccccc}{
#'      \tab pop1   \tab pop2   \tab  pop3  \tab  pop4  \tab  pop5  \cr
#' pop1 \tab        \tab 0.0040 \tab 0.0059 \tab 0.0085 \tab 0.0101 \cr
#' pop2 \tab 0.0040 \tab        \tab 0.0055 \tab 0.0082 \tab 0.0099 \cr
#' pop3 \tab 0.0059 \tab 0.0055 \tab        \tab 0.0104 \tab 0.0119 \cr
#' pop4 \tab 0.0085 \tab 0.0082 \tab 0.0104 \tab        \tab 0.0139 \cr
#' pop5 \tab 0.0101 \tab 0.0099 \tab 0.0119 \tab 0.0139 \tab
#' }
#'
#' @name raw.data
#' @docType data
#' @format A matrix with 3,000 columns and 1,004 rows
#' @seealso \code{\link{label}} and \code{\link{PC}}
#' @usage data(ipcaps_example)
#' @keywords raw.data
#' @md
#' @references
#' Balding, D.J., and Nichols, R.A. (1995). A method for quantifying
#' differentiation between populations at multi-allelic loci and its
#' implications for investigating identity and paternity. Genetica 96, 3-12.
"raw.data"





#' Synthetic dataset containing population labels for the dataset \code{raw.data}
#'
#' A dataset contains a character vector of 1,004 elements containing labels or
#' populations of 1,004 individuals which they belong. Five populations and
#' outliers were labeled as "pop1", "pop2", "pop3", "pop4", "pop5", and "outlier".
#'
#' @name label
#' @docType data
#' @format A vector with 1,004 elements.
#' @seealso \code{\link{raw.data}} and \code{\link{PC}}
#' @usage data(ipcaps_example)
#' @keywords label
"label"





#' Synthetic dataset containing the top 10 principal components (PC) from the
#' dataset \code{raw.data}
#'
#' A dataset contains a numeric matrix of 1,004 rows and 10 columns of top 10
#' PCs calculated from the dataset \code{raw.data}. The PCs were calculated
#' using linear principal component analysis (PCA), see more datails at
#' \code{KRIS::cal.pc.linear}
#'
#' @name PC
#' @docType data
#' @format A matrix with 10 columns and 1,004 rows
#' @seealso \code{\link{raw.data}} and \code{\link{label}}
#' @usage data(ipcaps_example)
#' @keywords PC
"PC"

NULL
