##' Summarize an Theoretical Analysis of Variance Model of Two-Phase
##' Experiments
##' 
##' Computes the coefficients of the variance components for the expected mean
##' squares for two-phase experiments. The function accepts a data frame of the
##' experimental design with the structural formulae of the block and treatment
##' factors. Two tables containing the variance components of the random
##' effects and fixed effects are returned.
##' 
##' 
##' @param design.df a data frame containing the experimental design. Requires
##' every column be a \code{\link{factor}. Any punctuation or symbol such as
##' dots or parentheses should be avoid for the column names.}.
##' @param blk.str1 a single string of characters containing the structural
##' formula for the block factors of the first-phase experiment using the
##' Wilkinson-Rogers' syntax.
##' @param blk.str2 a single string of characters containing the structural
##' formula for the block factors of the second-phase experiment using the
##' Wilkinson-Rogers' syntax.
##' @param trt.str a single string of characters containing the structural
##' formula for the treatment factors using the Wilkinson-Rogers' syntax.
##' @param var.comp a vector of characters containing the variance components
##' of interest this allows the user to specify the variance components to be
##' shown on the ANOVA table. This also allows the user to specify artificial
##' stratum to facilitate decomposition. Default is \code{NA}, which uses every
##' random factor as the variance components with the first phase's variance
##' components in \code{random.terms1} appear before the second phase's
##' variance components in \code{random.terms2}.
##' @param blk.contr a list of first-phase block contrast vectors, this allows
##' the user to specify the contrasts for each block factor in the first phase
##' experiment. Note that if this argument is used, it is necessary to specify
##' the contrasts for every treatment factor with the same order as
##' \code{fixed.terms}. Default is \code{NA}, which uses the C matrix described
##' by John and Williams (1987).
##' @param trt.contr a list of treatment contrast vectors, this allows the user
##' to specify the contrasts for each treatment factor. Note that if this
##' argument is used, it is necessary to specify the contrasts for every
##' treatment factor with the same order as \code{fixed.terms}. Default is
##' \code{NA}, which uses the C matrix described by John and Williams (1987).
##' @param table.legend a logical allows the users to use the legend for the
##' variance components of the ANOVA table for a large design. Default is
##' \code{FALSE}, which uses the original names.
##' @param response a numeric vector contains the responses from the
##' experiment.
##' @param latex a logical allows the users to output the Latex script to Latex
##' table. Once the Latex script is generated, it requires the user to install
##' and load two Latex packages: \code{booktabs} and \code{bm} to compile the
##' Latex script.
##' @param fixed.names a vector of character allows the users to modify symbols
##' for the fixed effects for the Latex outputs.
##' @param decimal a logical allows users to display the coefficients as the
##' decimals. Default is \code{FALSE}, resulting in the use of function
##' \code{fractions}.
##' @param digits a integer indicating the number of decimal places. Default is
##' 2, resulting in 2 decimal places.
##' @param list.sep a logical allows users to present the efficiency factors
##' and coefficients of the fixed effects a list of separate matrices. Default
##' is \code{TRUE}.
##' @return The values returned depends on the value of the \code{table.legend}
##' argument. If \code{table.legend = FALSE}, this function will return a list
##' of two data frames. The first data frame contains the random effects and
##' the second data frame contains the fixed effects. If the
##' \code{table.legend} argument is \code{TRUE}, then it will return a list
##' containing two lists. The first list consists of a data frame of random
##' effects and a character string for the legend. The second list consists of
##' a data frame of fixed effects and a character string for the legend.  If
##' \code{response} argument is used, the random effect table will have one
##' extra column with of mean squares computed from the responses from the
##' experiment.
##' @author Kevin Chang
##' @seealso \code{\link{terms}} for more information on the structural
##' formula.
##' @references John J, Williams E (1987). \emph{Cyclic and computer generated
##' Designs}. Second edition. Chapman & Hall.
##' 
##' Nelder JA (1965b). "The Analysis of Randomized Experiments with Orthogonal
##' Block Structure. II. Treatment Structure and the General Analysis of
##' Variance." \emph{Proceedings of the Royal Society of London. Series A,
##' Mathematical and Physical Sciences}, 283(1393), 163-178.
##' 
##' Wilkinson GN, Rogers CE (1973). "Symbolic Description of Factorial Models
##' for Analysis of Variance." \emph{Applied Statistics}, 22(3), 392-399.
##' @keywords design
##' @examples
##' 
##' #Phase 2 experiment  
##' design2 <- local({ 
##'   Run = as.factor(rep(1:4, each = 4))
##'   Ani = as.factor(LETTERS[c(1,2,3,4,
##'                             5,6,7,8,
##'                             3,4,1,2,
##'                             7,8,5,6)])
##'   Sam = as.factor(as.numeric(duplicated(Ani)) + 1)
##'   Tag = as.factor(c(114,115,116,117)[rep(1:4, 4)])
##'   Trt = as.factor(c("healthy", "diseased")[c(1,2,1,2,
##'                             2,1,2,1,
##'                             1,2,1,2,
##'                             2,1,2,1)])
##'   data.frame(Run, Ani, Sam, Tag, Trt, stringsAsFactors = TRUE)
##' })
##' design2
##'                                   
##' summaryAovTwoPhase(design2, blk.str1 = "Ani", blk.str2 = "Run", 
##' trt.str = "Tag + Trt")                                            
##'    
##' #Add the sample into the Phase 1 block structure                                           
##' summaryAovTwoPhase(design2, blk.str1 = "Ani/Sam", blk.str2 = "Run", 
##' trt.str = "Tag + Trt")                                            
##' 
##' 
##' #Assuming there is crossing between the animals and samples 
##' summaryAovTwoPhase(design2, blk.str1 = "Ani*Sam", blk.str2 = "Run", 
##' trt.str = "Tag + Trt")                                            
##' 
##'  
##' #Set Artificial stratum 
##' design2$AniSet = as.factor(c(2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1))
##' design2
##' 
##' summaryAovTwoPhase(design2, blk.str1 =  "Ani/Sam", blk.str2 = "AniSet/Run", 
##' trt.str = "Tag + Trt", var.comp = c("Ani:Sam", "Ani", "Run"))                                    
##' 
##' #Define traetment contrasts                                   
##' TagA = rep(c(1,1,-1,-1),time = 4)                
##' TagB = rep(c(1,-1,1,-1),time = 4)                
##' TagC = TagA * TagB
##' Tag = list(TagA = TagA, TagB = TagB, TagC = TagC)
##' Tag
##' 
##' 
##' Trt = as.numeric(design2$Trt)-1.5
##' Trt
##' 
##' 
##' summaryAovTwoPhase(design2, blk.str1 =  "Ani/Sam", blk.str2 = "Run", 
##' trt.str = "Tag + Trt", 
##' trt.contr = list(Tag = list(TagA = TagA, TagB = TagB, TagC = TagC), Trt = Trt),
##' table.legend = TRUE)                                
##' 
##' #Compute MS 
##' set.seed(527)
##' summaryAovTwoPhase(design2, blk.str1 = "Ani/Sam", blk.str2 = "Run", 
##' trt.str = "Tag + Trt", response = rnorm(16))$ANOVA                                            
##' 
##' #Generate Latex scripts
##' summaryAovTwoPhase(design2, blk.str1 = "Ani/Sam", blk.str2 = "Run", 
##' trt.str = "Tag + Trt", latex = TRUE, fixed.names = c("\\gamma", "\\tau"))  
##' 
##' #Generate Latex scripts with MS
##' set.seed(527)
##' summaryAovTwoPhase(design2, blk.str1 = "Ani/Sam", blk.str2 = "Run", 
##' trt.str = "Tag + Trt", response = rnorm(16), latex = TRUE, 
##' fixed.names = c("\\gamma", "\\tau") )                               
##' 
##' 
##' @export summaryAovTwoPhase
summaryAovTwoPhase <- function(design.df, blk.str1, blk.str2, trt.str, var.comp = NA, 
    blk.contr = NA, trt.contr = NA, table.legend = FALSE, response = NA, latex = FALSE, 
    fixed.names = NA, decimal = FALSE, digits = 2, list.sep = TRUE) {
    
  design.df <- data.frame(sapply(design.df,
                                 function(x) gsub("[[:punct:]]", "", as.character(x))), 
                          stringsAsFactors = TRUE )
    
	#browser()
	newTerms = adjustMissingLevels(design.df, trt.str)
		
	design.df = newTerms$design.df
	trt.str = newTerms$str.for
	
	newTerms = adjustMissingLevels(design.df, blk.str1)
		
	design.df = newTerms$design.df
	blk.str1 = newTerms$str.for
	
    # Extract the fixed and random terms
    
    rT1 <- stats::terms(stats::as.formula(paste("~", blk.str1, sep = "")), keep.order = TRUE)  #random terms phase 1
    rT2 <- stats::terms(stats::as.formula(paste("~", blk.str2, sep = "")), keep.order = TRUE)  #random terms phase 2
    fT <- stats::terms(stats::as.formula(paste("~", trt.str, sep = "")), keep.order = TRUE)  #fixed terms
    
    
    # Preparing the block structures write('1.  Preparing the block structure.', '')
    
    blkTerm1 <- attr(rT1, "term.labels")
    blkTerm2 <- attr(rT2, "term.labels")
    
    # check for complete confounding and changing the names of the block factors
    # browser()
    if (any(grepl("\\:", blkTerm2))) {
        
        check.blkTerm2 <- unique(unlist(strsplit(blkTerm2, "\\:")))
    } else {
        check.blkTerm2 <- blkTerm2
    }
    
    if (any(grepl("\\:", blkTerm1))) {
        check.blkTerm1 <- unique(unlist(strsplit(blkTerm1, "\\:")))
    } else {
        check.blkTerm1 <- blkTerm1
    }
    
    # Check for Complete confounding.
    for (i in 1:length(check.blkTerm2)) {
        if (length(check.blkTerm1) == 1) {
            if (all(as.numeric(as.factor(design.df[, check.blkTerm1])) == as.numeric(as.factor(design.df[, 
                check.blkTerm2[i]])))) {
                cat("Note: Complete confounding between ", check.blkTerm1, " and ", 
                  check.blkTerm2[i], "!\n", sep = "")
                
                colnames(design.df)[which(colnames(design.df) == check.blkTerm1)] <- paste(colnames(design.df)[which(colnames(design.df) == 
                  check.blkTerm1)], "CCW", sep = "")
                
                blk.str1 <- paste(blkTerm1[which(blkTerm1 == check.blkTerm1)], "CCW", 
                  sep = "")
                
                if (!is.na(blk.contr)) {
                  names(blk.contr)[which(names(blk.contr) == check.blkTerm1)] <- paste(names(blk.contr)[which(names(blk.contr) == 
                    check.blkTerm1)], "CCW", sep = "")
                }
                check.blkTerm1 <- paste(check.blkTerm1, "CCW", sep = "")
                
            }
        } else {
            check.temp <- apply(design.df[, check.blkTerm1], 2, function(x) all(as.numeric(as.factor(x)) == 
                as.numeric(as.factor(design.df[, check.blkTerm2[i]]))))
            if (any(check.temp)) {
                cat("Note: Complete confounding between ", check.blkTerm1[which(check.temp)], 
                  " and ", check.blkTerm2[i], "!\n", sep = "")
                colnames(design.df)[which(colnames(design.df) == check.blkTerm1[which(check.temp)])] <- paste(colnames(design.df)[which(colnames(design.df) == 
                  check.blkTerm1[which(check.temp)])], "CCW", sep = "")
                
                blk.str1 <- gsub(check.blkTerm1[which(check.temp)], paste(check.blkTerm1[which(check.temp)], 
                  "CCW", sep = ""), blk.str1)
                if (!is.na(blk.contr)) {
                  names(blk.contr)[which(names(blk.contr) == check.blkTerm1[which(check.temp)])] <- paste(names(blk.contr)[which(names(blk.contr) == 
                    check.blkTerm1[which(check.temp)])], "CCW", sep = "")
                }
                check.blkTerm1[which(check.blkTerm1 == check.blkTerm1[which(check.temp)])] <- paste(check.blkTerm1[which(check.blkTerm1 == 
                  check.blkTerm1[which(check.temp)])], "CCW", sep = "")
            }
        }
    }
    
	rT1 <- stats::terms(stats::as.formula(paste("~", blk.str1, sep = "")), keep.order = TRUE)  
	#random terms phase 1
	blkTerm1 <- attr(rT1, "term.labels")
     
	
	#browser()
	
    Z1 <- makeBlkDesMat(design.df, rev(blkTerm1))
    Z2 <- makeBlkDesMat(design.df, rev(blkTerm2))
    
    # write('2. Defining the block structures of second Phase.', '')
    Pb <- makeOrthProjectors(Z2)
    
	if(length(blkTerm2) > 1)
		blkTerm2 = adjustEffectNames(effectsMatrix = attr(rT2, "factors"), effectNames = blkTerm2)

    
	
    if (names(Pb)[1] == "e") {
        names(Pb)[1] <- paste("Within", paste(unique(unlist(strsplit(names(Pb)[-1], 
            "[[:punct:]]+"))), collapse = "."))
        
        names(Pb)[-1] <- names(Z2)[-1] <-rev(blkTerm2)
		 
    } else {
        names(Pb) <- names(Z2) <- rev(blkTerm2)
    }
    
    # write('3. Defining the block structures of first phase within second Phase.', '')
    effectsMatrix <- attr(rT1, "factor")
    
	effectsMatrix[nrow(effectsMatrix), effectsMatrix[nrow(effectsMatrix),]==2] <- 1
	
	if(length(blkTerm1) > 1)
		blkTerm1 = adjustEffectNames(effectsMatrix = effectsMatrix, effectNames = blkTerm1)
	
	if (names(Z1)[1] == "e") {
		names(Z1)[-1] <- rev(blkTerm1)
		 
    } else {
        names(Z1) <- rev(blkTerm1)
    }
 	
	#browser()   
    T <- makeContrMat(design.df, effectNames = blkTerm1, effectsMatrix = effectsMatrix, 
        contr.vec = blk.contr)
    N <- makeOverDesMat(design.df, blkTerm1)
    

    
    #browser()
    
    res <- paste("Within", paste(unique(unlist(strsplit(names(T), "[[:punct:]]+"))), 
        collapse = "."))
    
    Pb1 <- lapply(Pb, function(z) infoDecompMat(z, T, N))
    
    # t.name = unique(unlist(strsplit(names(T), '[[:punct:]]')))
    t.name <- names(T)
    
    if (length(Pb) > 1) {
        pb1.names <- lapply((Pb1[-1]), names)
    } else {
        pb1.names <- lapply(Pb1, names)
    }
    
    # browser()
    for (i in 1:length(pb1.names)) {
        comp <- t.name %in% pb1.names[[i]]
        if (any(comp)) {
            break
        } else if (i == length(pb1.names)) {
            names(Pb1)[1] <- ""
        }
    }
    
    for (i in 1:length(Pb1)) {
        names(Pb1[[i]])[which(names(Pb1[[i]]) == "Residual")] <- res
    }
    
    # Preparing the treatment structures write('4.  Preparing the treatment structure.',
    # '')
    
   trtTerm <- attr(fT, "term.labels")
    effectsMatrix <- attr(fT, "factor")
     #browser()
    
	 effectsMatrix[nrow(effectsMatrix), effectsMatrix[nrow(effectsMatrix),]==2] <- 1
	 
	#browser()
	if(length(trtTerm) > 1)
		trtTerm = adjustEffectNames(effectsMatrix, trtTerm)
		
  	T <- makeContrMat(design.df = design.df, effectNames = trtTerm, effectsMatrix = effectsMatrix, 
  	                  contr.vec = trt.contr)
  	N <- makeOverDesMat(design.df = design.df, effectNames = trtTerm)
  	Replist <- getTrtRep(design.df, trtTerm)
	Rep <- Replist$Rep
  	trt.Sca <- Replist$Sca
  	
	
	#When there are treatment contrasts defined 
  	if (any(grepl("\\.", names(T)))) {
  	  colnames(Rep) <- trtTerm
  	  names(trt.Sca) <- trtTerm 
  	  Rep <- Rep[, sapply(strsplit(names(T), "\\."), function(x) x[1])]
  	  trt.Sca <- trt.Sca[sapply(strsplit(names(T), "\\."), function(x) x[1])]
  	} else {
  	  colnames(Rep) <- trtTerm
  	  names(trt.Sca) <- trtTerm      
	}
	 
    
	#browser()
	
    # Start calculating the VCs 2-phase experiment write('5. Start calculating the
    # variance components.', '') write('6. Pre- and post-multiply NTginvATN by block
    # projection matrices.', '')
    
	effFactors <- lapply(Pb1, function(y) lapply(y, function(z) getEffFactor(z, T, N, 
	                                                                         Rep, trt.Sca)))
	effFactors <- effFactors[sort(1:length(effFactors), decreasing = TRUE)]   
    
    
    v.mat <- getVMat.twoPhase(Z.Phase1 = Z1, Z.Phase2 = Z2, design.df = design.df, var.comp = var.comp)
    
    if (all(is.na(var.comp))) {
        names(v.mat)[-1] <- c(rev(blkTerm1), rev(blkTerm2))
    }
    
    
    ANOVA <- getCoefVC.twoPhase(Pb = effFactors, design.df = design.df, v.mat = v.mat, 
        response, table.legend, decimal = decimal, digits = digits)
    
    
    effFactors <- lapply(Pb1, function(y) lapply(y, function(z) getEffFactor(z, T, N, 
        Rep, trt.Sca)))
    effFactors <- effFactors[sort(1:length(effFactors), decreasing = TRUE)]
    
    
    if (latex) {
        EF <- getFixedEF.twoPhase(effFactors = effFactors, trt.Sca = trt.Sca, T = T, Rep = Rep, 
                                table.legend, decimal = decimal, digits = digits, list.sep = FALSE)
        return(toLatexTable(ANOVA = ANOVA, EF = EF, fixed.names = fixed.names))
    } else {
      EF <- getFixedEF.twoPhase(effFactors = effFactors, trt.Sca = trt.Sca, T = T, Rep = Rep, 
                                table.legend, decimal = decimal, digits = digits, list.sep = list.sep)
        return(list(ANOVA = ANOVA, Fixed = EF))
    }
    
    
} 
