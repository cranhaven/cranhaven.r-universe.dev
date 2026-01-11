##' Summarize an Theoretical Analysis of Variance Model of Single-Phase
##' Experiments
##' 
##' Computes the coefficients of the variance components for the expected mean
##' squares for single-phase experiments. The function accepts a data frame of
##' the experimental design with the structural formulae of the block and
##' treatment factors. Two tables containing the variance components of the
##' random effects and fixed effects are returned.
##' 
##' 
##' @param design.df a data frame containing the experimental design. Requires
##' every column be a \code{\link{factor}. Any punctuation or symbol such as
##' dots or parentheses should be avoid for the column names.}.
##' @param blk.str a single string of characters containing the structural
##' formula for the block factors using the Wilkinson-Rogers' syntax.
##' @param trt.str a single string of characters containing the structural
##' formula for the treatment factors using the Wilkinson-Rogers' syntax.
##' @param var.comp a vector of characters containing the variance components
##' of interest this allows the user to specify the variance components to be
##' shown on the ANOVA table. This also allows the user to specify artificial
##' stratum to facilitate decomposition. Default is \code{NA}, which uses every
##' random factor as the variance components from \code{random.terms}.
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
##' design1 <- local({ 
##'   Ani = as.factor(LETTERS[c(1,2,3,4,
##'                             5,6,7,8)])
##'   Trt = as.factor(letters[c(1,1,1,1,
##'                             2,2,2,2)])
##'   data.frame(Ani, Trt, stringsAsFactors = TRUE)
##' })
##' 
##' summaryAovOnePhase(design1, blk.str = "Ani", trt.str = "Trt") 
##' 
##' summaryAovOnePhase(design1, blk.str = "Ani", trt.str = "Trt", 
##' latex = TRUE, fixed.names = c("\\tau"))
##' 
##' 
##' @export summaryAovOnePhase
summaryAovOnePhase <- function(design.df, blk.str, trt.str, var.comp = NA, trt.contr = NA, table.legend = FALSE, 
                               response = NA, latex = FALSE, fixed.names = NA, decimal = FALSE, digits = 2, 
                               list.sep = TRUE) {
  
  design.df <- data.frame(sapply(design.df,
                                 function(x) gsub("[[:punct:]]", "", as.character(x))),
                          stringsAsFactors = TRUE )
  
  newTerms = adjustMissingLevels(design.df, trt.str)
  #browser()
  design.df = newTerms$design.df
  trt.str = newTerms$str.for
  ######################################################################################### Main methods starts here-> Extract the fixed and random terms
  
  rT <- stats::terms(stats::as.formula(paste("~", blk.str, sep = "")), keep.order = TRUE)  #random terms
  
  rT.terms <- attr(rT, "term.labels")
  
  fT <- stats::terms(stats::as.formula(paste("~", trt.str, sep = "")), keep.order = TRUE)  #fixed terms
  
  ######################################################################################### Preparing the block structures browser()
  Z <- makeBlkDesMat(design.df, rev(rT.terms))
  
  Pb <- makeOrthProjectors(Z)
  
  if (length(rT.terms) > 1) 
    rT.terms = adjustEffectNames(effectsMatrix = attr(rT, "factors"), effectNames = rT.terms)
  
  if (names(Pb)[1] == "e") {
    names(Pb)[1] <- paste("Within", paste(unique(unlist(strsplit(names(Pb)[-1], "[[:punct:]]"))), collapse = "."))
    names(Pb)[-1] <- rev(rT.terms)
    
  } else {
    names(Pb) <- rev(rT.terms)
    
  }
  names(Z)[-1] <- rev(rT.terms)
  
  ######################################################################################### Prepating the treatment structures
  
  trtTerm <- attr(fT, "term.labels")
  effectsMatrix <- attr(fT, "factor")
  # browser()
  
  effectsMatrix[nrow(effectsMatrix), effectsMatrix[nrow(effectsMatrix), ] == 2] <- 1
  
  if (length(trtTerm) > 1) 
    trtTerm = adjustEffectNames(effectsMatrix, trtTerm)
  
  T <- makeContrMat(design.df = design.df, effectNames = trtTerm, 
                    effectsMatrix = effectsMatrix, contr.vec = trt.contr)

  N <- makeOverDesMat(design.df = design.df, effectNames = trtTerm)
  Replist <- getTrtRep(design.df, trtTerm)
  Rep <- Replist$Rep
  trt.Sca <- Replist$Sca
  # browser()
  
  ################################################################################################
  # Needs to check allZero = apply(N, 2, function(x) all(x==0)) N = N[,!allZero] T = lapply(T, function(x)
  ################################################################################################
  # x[!allZero, !allZero]) Rep = Rep[!allZero, ]  
  ################################################################################################ 
  
  # When there are treatment contrasts defined
  if (any(grepl("\\.", names(T)))) {
    colnames(Rep) <- trtTerm
    names(trt.Sca) <- trtTerm
    Rep <- Rep[, sapply(strsplit(names(T), "\\."), function(x) x[1])]
    trt.Sca <- trt.Sca[sapply(strsplit(names(T), "\\."), function(x) x[1])]
  } else {
    
    colnames(Rep) <- trtTerm
    names(trt.Sca) <- trtTerm
  }
  
  ########################################################################################
  # Start calculating the VCs 1-phase experiment browser() pre- and post-multiply NTginvATN by block projection
  ########################################################################################
  # matrices
  
  effFactors <- lapply(Pb, function(z) getEffFactor(z, T, N, Rep, trt.Sca))
  effFactors <- effFactors[sort(1:length(effFactors), decreasing = TRUE)]
  
  # browser()
  v.mat <- getVMat.onePhase(Z.Phase1 = Z, design.df = design.df, var.comp = var.comp)
  
  if (all(is.na(var.comp))) {
    names(v.mat)[-1] <- rev(rT.terms)
  }
  # browser()
  
  ANOVA <- getCoefVC.onePhase(Pb = effFactors, design.df = design.df, v.mat = v.mat, 
                              response = response, table.legend = table.legend, 
                              decimal = decimal, digits = digits)
  
  ############################################################################################################## 
  # browser()
   
 
  if (latex) {
    Fixed <- getFixedEF.onePhase(effFactors = effFactors, trt.Sca = trt.Sca, T = T, Rep = Rep, 
                                 table.legend = table.legend, 
                                 decimal = decimal, digits = digits, list.sep = FALSE)
    return(toLatexTable(ANOVA = ANOVA, EF = Fixed, fixed.names = fixed.names))
  } else {
    Fixed <- getFixedEF.onePhase(effFactors = effFactors, trt.Sca = trt.Sca, T = T, Rep = Rep, 
                                 table.legend = table.legend, 
                                 decimal = decimal, digits = digits, list.sep = list.sep)
    return(list(ANOVA = ANOVA, Fixed = Fixed))
  }
} 


