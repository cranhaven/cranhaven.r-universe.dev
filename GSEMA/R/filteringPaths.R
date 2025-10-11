#' Fuction for filtering gene sets with low expression
#'
#' This function eliminates gene sets with low expression in both groups in
#' a study
#'
#' @param objectMApath A list of list. Each list contains two elements.
#' The first element is the Gene Set matrix (gene sets in rows and samples in
#' columns) and the second element is a vector of zeros and ones that represents
#' the state of the different samples of the Gene Sets matrix.
#' 0 represents one group (controls) and 1 represents the other group (cases).
#'
#' @param threshold A number that indicates the threshold to eliminate a gene
#' set. For a eliminate a gene set is necessary that the median for both groups
#' are less than the threshold
#' If  threshold = "sd" the threshold will be the standard deviation of the
#' gene set. The default value is 0.65.
#'
#' @param n_cores A number that indicates the number of cores to use in the
#' parallelization. The default value is 1.
#'
#'
#' @return The same objectMApath list but with the gene sets that do not meet
#' the threshold eliminated.
#'
#' @author Juan Antonio Villatoro Garcia,
#' \email{juanantoniovillatorogarcia@@gmail.com}
#'
#' @seealso \code{\link{createObjectMApath}}
#'
#' @examples
#'
#' data("simulatedData")
#' newObject <- filteringPaths(objectMApathSim, threshold = "sd")
#'
#' @export



filteringPaths <- function(objectMApath, threshold = 0.65, n_cores = 1){
    objectMApath <- mclapply(objectMApath, .filtering_pathways,
        threshold = threshold, mc.cores = n_cores)
    return(objectMApath)
}

#Function for filtering
.filtering_pathways <- function(study, threshold = 0.65){
    threshold_1 <- threshold
    exmatrix <- study[[1]]
    cond <- study[[2]]
    if(threshold == "sd"){
        threshold_1 <- sd(exmatrix)
        if(threshold_1 > 1){
            threshold_1 <- 1
        }
    }
    index <- c()
    for(path in seq_len(nrow(exmatrix))){
        m1 <- median(exmatrix[path,cond == 0])
        m2 <- median(exmatrix[path,cond == 1])
        if(abs(m1) < threshold_1 & abs(m2) < threshold_1){
            index <- c(index,path)
        }
    }
    if(!is.null(index)){
        exmatrix <- exmatrix[-index,, drop = FALSE]}
    study[[1]] <- exmatrix
    return(study)
}



