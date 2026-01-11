##' Get the Variance Matrices for Two-Phase experiment
##' 
##' Construct the matrix for each variance components for the single-phase or
##' two-phase experiment.
##' 
##' 
##' @param Z.Phase1 a list of block design matrix from \code{makeBlkDesMat}
##' function from Phase 1 block structure.
##' @param Z.Phase2 a list of block design matrix from \code{makeBlkDesMat}
##' function from Phase 2 block structure.
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
##' @examples
##'      design2 <- local({ 
##'   Run = as.factor(rep(1:4, each = 4))
##'   Ani = as.factor(LETTERS[c(1,2,3,4,
##'                             5,6,7,8,
##'                             3,4,1,2,
##'                             7,8,5,6)])
##'   Tag = as.factor(c(114,115,116,117)[rep(1:4, 4)])
##'   Trt = as.factor(letters[c(1,2,1,2,
##'                             2,1,2,1,
##'                             1,2,1,2,
##'                             2,1,2,1)])
##'   data.frame(Run, Ani, Tag, Trt, stringsAsFactors = TRUE )
##' })
##' 
##'     blk.str1 = "Ani"
##'     blk.str2 = "Run"
##'    
##' 	rT1 = terms(as.formula(paste("~", blk.str1, sep = "")), keep.order = TRUE) 
##' 	#random terms phase 1
##' 	rT2 = terms(as.formula(paste("~", blk.str2, sep = "")), keep.order = TRUE) 
##' 	#random terms phase 2
##' 
##' 	blkTerm1 = attr(rT1,"term.labels")
##' 	blkTerm2 = attr(rT2,"term.labels")
##' 
##' 	Z1 = makeBlkDesMat(design2, rev(blkTerm1))
##' 	Z2 = makeBlkDesMat(design2, rev(blkTerm2))
##' 
##' 	V = getVMat.twoPhase(Z1, Z2, design2, var.comp = NA)
getVMat.twoPhase <- function(Z.Phase1, Z.Phase2, design.df, var.comp = NA) {
    #browser()
    v.mat <- lapply(c(Z.Phase1, Z.Phase2[-1]), function(x) x %*% t(x))
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
