#'Performing Gene Set Enrichment Meta-analysis
#'
#'It performs Gene Sets Enrichment meta-analysis by applying Effects size
#'combination methods
#'
#' @param objectMApath A list of list. Each list contains two elements.
#' The first element is the Gene Set matrix (gene sets in rows and samples in
#' columns) and the second element is a vector of zeros and ones that represents
#' the state of the different samples of the Gene Sets matrix.
#' 0 represents one group (controls) and 1 represents the other group (cases).
#'
#' @param effectS A list of two elements. The first element is a dataframe
#' with gene sets in rows and studies in columns.
#' Each component of the dataframe is the effect of a gene set in a study.
#' The second element of the list is also a dataframe
#' with the same structure, but in this case each component of the dataframe
#' represent the variance of the effect of a gene set in a study. This argument
#' should be only used in the case that objectMApath argument is null.
#'
#' @param measure A character string that indicates the type of effect size to
#' be calculated. The options are "limma", "SMD" and "MD". The default value
#' is "limma". See details for more information.
#'
#' @param WithinVarCorrect A logical value that indicates if the within variance
#' correction should be applied. The default value is TRUE. See details for more
#' information.
#'
#' @param typeMethod A character that indicates the method to be performed.
#' See "Details"for more information
#'
#' @param missAllow a number that indicates the maximum proportion of missing
#' values allowed in a sample. If the sample has more proportion of missing
#' values the sample will be eliminated. In the other case the missing values
#' will be imputed using the K-NN algorithm.
#'
#' @param numData The minimum number of datasets in which a gene
#' must be contained to be included in the emta-analysis. 
#' By default, the gene must be contained in all the datasets. 
#' If the number entered exceeds the number of studies, the total number of 
#' studies will be considered."
#'
#'
#' @details There are different ways to calculate the effect size of a gene set:
#'\enumerate{
#'  \item "MD": Raw Mean Difference (Borenstein, 2009)
#'  \item "SMD":  Standardized Mean Difference (Hedges, 1981)
#'  \item "limma": Standardized Mean Difference calculated from the
#'  t-statistics and degrees of freedom obtained by the limma package by
#'  applying the transformation of Rosenthal and Rosnow, 2008). Its calculation
#'  is similar to the one proposed by (Marot et al., 2009) but considering the
#'  transformation of (Rosenthal and Rosnow, 2008).
#'     }
#'
#' The correction of the variance of the effect size is based on
#' Lin L, Aloe AM (2021) in which the variance is calculated from the
#' different estimators.
#'
#' The meta-analysis methods that can be applied are:
#'\enumerate{
#'  \item "FEM": Fixed Effects model
#'  \item "REM": Random Effects model (Default).
#'     }
#'
#'
#'
#' @return A dataframe with the meta-analysis results. For more information
#' see the package vignette.
#'
#' @references
#'
#' Toro-Domínguez D., Villatoro-García J.A.,
#' Martorell-Marugán J., Román-Montoya Y., Alarcón-Riquelme M.E.,
#' Carmona-Sáez P. (2020).
#' A survey of gene expression meta-analysis: methods and applications,
#' Briefings in Bioinformatics, bbaa019,
#' \doi{10.1093/bib/bbaa019}
#'
#' Borenstein, M. (2009). Effect sizes for continuous data. In H. Cooper,
#' L. V. Hedges, & J. C. Valentine (Eds.),
#' The handbook of research synthesis and meta-analysis (2nd ed., pp. 221–235).
#' New York: Russell Sage Foundation.
#'
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect
#' size and related estimators. Journal of Educational Statistics, 6(2),
#' 107–128. \doi{10.2307/1164588}
#'
#' Lin L, Aloe AM (2021). Evaluation of various estimators for standardized mean
#' difference in meta-analysis. Stat Med. 2021 Jan 30;40(2):403-426.
#' \doi{10.1002/sim.8781}
#'
#' Marot, G., Foulley, J. L., Mayer, C. D., & Jaffrézic, F. (2009).
#' Moderated effect size and P-value combinations for microarray meta-analyses.
#' Bioinformatics. 2692-2699. \doi{10.1093/bioinformatics/btp444}
#' 
#' Rosenthal, R., & Rosnow, R. L. (2008). Essentials of behavioral research:
#' Methods and data analysis. McGraw-Hill.
#'
#' @author Juan Antonio Villatoro Garcia,
#' \email{juanantoniovillatorogarcia@@gmail.com}
#'
#'
#' @seealso \code{\link{calculateESpath}}
#'
#' @examples
#'
#' data("simulatedData")
#' results <- metaAnalysisESpath(objectMApath = objectMApathSim,
#'     measure = "limma", typeMethod = "REM")
#'
#' @export


metaAnalysisESpath<-function(objectMApath = NULL, effectS = NULL,
    measure = c("limma", "SMD", "MD"),
    WithinVarCorrect=TRUE,
    typeMethod=c("REM", "FEM"),
    missAllow=0.3, numData = length(objectMApath)){
    typeMethod <- match.arg(typeMethod)
    if(numData > length(objectMApath)){
        numData <- length(objectMApath)
    }
    if(typeMethod == "FEM" | typeMethod == "REM"){
        if(!is.null(objectMApath)){
            effectS <- calculateESpath(objectMApath, missAllow = missAllow,
                measure = measure, WithinVarCorrect = WithinVarCorrect)
        }
        else{
            if(is.null(effectS)){
                stop("You have to add an effectS argument is objectMA is null")}
            names(effectS) <- c("ES", "Var")
        }
        results <- .metaESpath(effectS, metaMethod = typeMethod,
            numData = numData)
    }
    return(results)
}

.metaESpath <- function(calESResults, metaMethod=c("REM","FEM"),
    numData = ncol(calESResults$ES)){
    metaMethod <- match.arg(metaMethod)
    K<-ncol(calESResults$ES)
    if(metaMethod == "REM"){
        message("Performing Random Effects Model")
        res <- .getREMpath(calESResults$ES, calESResults$Var)
        tempFDR <- res$FDR
        meta.res <- data.frame(matrix(0, ncol=9,
            nrow = length(rownames(calESResults$ES))))
        rownames(meta.res) <- rownames(calESResults$ES)
        meta.res[,2] <- res$mu.hat
        meta.res[,3] <- res$mu.var
        meta.res[,4] <- res$Qval
        meta.res[,5] <- res$tau2
        meta.res[,6] <- res$score
        meta.res[,7] <- res$pval
        meta.res[,8] <- tempFDR
        meta.res[,9] <- ncol(calESResults$ES)-rowSums(is.na(calESResults$ES))
        colnames(meta.res) <- c("Pathway", "Com.ES", "ES.var", "Qval",
            "tau2","Zval", "Pval", "FDR","numDatasets")
    }else{
        message("Performing Fixed Effects Model")
        res <- .getFEM(calESResults$ES,calESResults$Var)
        tempFDR <- res$FDR
        meta.res <- data.frame(matrix(0, ncol = 7,
            nrow = nrow(calESResults$ES)))
        rownames(meta.res) <- rownames(calESResults$ES)
        meta.res[,2] <- res$mu.hat
        meta.res[,3] <- res$mu.var
        meta.res[,4] <- res$zval
        meta.res[,5] <- res$pval
        meta.res[,6] <- tempFDR
        meta.res[,7] <- ncol(calESResults$ES)-rowSums(is.na(calESResults$ES))
        colnames(meta.res) <- c("Pathway", "Com.ES", "ES.var", "Zval", "Pval",
            "FDR", "numDatasets")
    }
    meta.res<- subset(meta.res,
        subset = meta.res[,"numDatasets"] > 1)
    meta.res<- subset(meta.res,
        subset = meta.res[,"numDatasets"] >= numData)
    meta.res <- as.data.frame(as.matrix(meta.res))
    meta.res[,1] <- rownames(meta.res)
    attr(meta.res,"metaMethod") <- metaMethod
    return(meta.res)
}

## Fixed Effects Model (FEM)
.getFEM <- function(em,vm){
    wt <- 1/vm
    mu.hat <- rowSums(wt*em, na.rm=TRUE)/rowSums(wt, na.rm=TRUE)
    mu.var <- 1/rowSums(wt, na.rm=TRUE)
    z.score <- mu.hat/sqrt(mu.var)
    z.p <- 2*(1-pnorm(abs(z.score)))
    qval <- p.adjust(z.p,method = "BH")
    res <- list(mu.hat = mu.hat,mu.var = mu.var,
        zval = z.score,pval = z.p, FDR = qval)
    return(res)
}


## RANDOM Effects Model (REM)
## Obtaining the Q that represents the total variance
.getQ <- function(em,vm){
    wt <- 1/vm
    temp1 <- wt * em
    mu.hat <- rowSums(temp1, na.rm=TRUE)/rowSums(wt, na.rm=TRUE)
    Q <- rowSums(wt * (em - mu.hat)^2, na.rm = TRUE)
    return(Q)
}

## Obtaining variance between studies
.getTau2 <- function(Q,vm,k){
    wt <- 1/vm
    s1 <- rowSums(wt, na.rm=TRUE)
    s2 <- rowSums(wt^2, na.rm=TRUE)
    temp <- (Q - (k - 1))/(s1 - (s2/s1))
    tau2 <- pmax(temp,0)
    return(tau2)
}


#REM model
.getREMpath <- function(em,vm){
    k <- ncol(em)
    Q.val <- .getQ(em,vm)
    tau2 <- .getTau2(Q.val,vm,k)
    temp.wt <- 1/(vm+tau2)
    mu.hat <- rowSums(temp.wt*em, na.rm=TRUE)/rowSums(temp.wt, na.rm = TRUE)
    mu.var <- rowSums((temp.wt^2)* (vm +tau2), na.rm=TRUE)/rowSums(temp.wt,
        na.rm = TRUE)^2
    Qpval <- pchisq(Q.val, df = k - 1, lower.tail = FALSE)
    score <- mu.hat/sqrt(mu.var)
    pval <- 2*(1-pnorm(abs(score)))
    qval <- p.adjust(pval,method="BH")
    res <- list(mu.hat = mu.hat, mu.var = mu.var, Qval = Q.val, Qpval = Qpval,
        tau2 = tau2, score = score, pval = pval, FDR = qval)
    return(res)
}
