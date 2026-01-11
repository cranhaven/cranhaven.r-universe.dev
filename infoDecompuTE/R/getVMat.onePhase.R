##' Get the Variance Matrices for Single-Phase experiment
##' 
##' Construct the matrix for each variance components for the single-phase or
##' two-phase experiment.
##' 
##' 
##' @param Z.Phase1 a list of block design matrix from \code{makeBlkDesMat}
##' function from Phase 1 block structure.
##' @param design.df a data frame containing the experimental design. Requires
##' every column be a \code{\link{factor}}.
##' @param var.comp a vector of characters containing the variance components
##' of interest this allows the user to specify the variance components to be
##' shown on the ANOVA table. This also allows the user to specify artificial
##' stratum to facilitate decomposition. Default is \code{NA}, which uses every
##' random factor as the variance components with the first phase's variance
##' components in appear before the second phase's variance components.
##' @return A list of matrices.
##' @author Kevin Chang
##' @export
##' 
##' @examples
##' 
##'  design1 <- local({ 
##'     Ani = as.factor(LETTERS[c(1,2,3,4,
##'                               5,6,7,8)])
##'     Trt = as.factor(letters[c(1,1,1,1,
##'                               2,2,2,2)])
##'     data.frame(Ani, Trt, stringsAsFactors = TRUE )
##'   })
##' 
##'     blk.str = "Ani"
##'     
##' 		rT = terms(as.formula(paste("~", blk.str, sep = "")), keep.order = TRUE) 
##' 
##'     blkTerm = attr(rT,"term.labels")
##' 		Z = makeBlkDesMat(design1, rev(attr(rT,"term.labels")))
##' 
##'     V = getVMat.onePhase(Z, design1)
##'     
##' 
getVMat.onePhase <- function(Z.Phase1, design.df, var.comp = NA) {
    
    v.mat <- lapply(Z.Phase1, function(x) x %*% t(x))
    # Now construct variance matrices
    if (all(is.na(var.comp))) {
        return(v.mat)
        
    } else {
        
        if (names(v.mat)[1] == "e") {
            match.names <- match(var.comp, names(v.mat))
            
            if (any(is.na(match.names))) 
                match.names <- match.names[!is.na(match.names)]
            
            return(v.mat[c(1, match.names)])
        } else {
            match.names <- match(var.comp, names(v.mat))
            
            if (any(is.na(match.names))) 
                match.names <- match.names[!is.na(match.names)]
            
            return(v.mat[match.names])
        }
        
    }
    
} 
