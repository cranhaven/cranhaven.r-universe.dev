#' Filtering datasets for subpopulations with low sample sizes
#' 
#' Simplifies the process of eliminating subpopulations with low sample sizes.
#' 
#' @param data Matrix containing genotype data with individuals as rows and
#' loci as columns. Genotypes should be coded as 0 (homozygous), 1 (heterozygous),
#' or 2 (homozygous). Rownames must be subpopulation names and column names
#' should be marker names.
#' @param minsample An integer representing the smallest number of individuals a 
#' subpopulation must contain to be included in analysis.
#' 
#' @return filtered_data The original dataset minus the subpopulations that fail
#' to meet the sample size threshold.
#' 
#' @examples 
#' test <- matrix(round(runif(400,1,2)), nrow = 100)
#' rownames(test) <- c(rep(c('A','B','C'),each=25), rep(c('D','E'), each=5), rep('F', 15))
#' dim(test)
#'
#' #The 'D' and 'E' subpopulations have only five members each and should be removed
#' filtered_test <- dfilter(test,12)
#' 
#' dim(filtered_test)	# New dataset is reduced by 10 rows (five for 'D' and five for 'E')
#' 
#' @export
dfilter <- function(data, minsample){
	subpops <- rownames(data)
	passes <- names(which(table(subpops) >= minsample))
	filtered_data <- data[subpops %in% passes,]
	return(filtered_data)
}