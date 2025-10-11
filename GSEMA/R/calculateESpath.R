#' Calculation of Effects Sizes and their variance for the different Gene Sets
#' and studies
#'
#' This function uses different estimators to calculate the different effects
#' size and their variance for each gene and for each dataset
#'
#' @param objectMApath A list of list. Each list contains two elements.
#' The first element is the Gene Set matrix (gene sets in rows and samples in
#' columns) and the second element is a vector of zeros and ones that represents
#' the state of the different samples of the Gene Sets matrix.
#' 0 represents one group (controls) and 1 represents the other group (cases).
#'
#' @param measure A character string that indicates the type of effect size to
#' be calculated. The options are "limma", "SMD" and "MD". The default value
#' is "limma". See details for more information.
#'
#' @param WithinVarCorrect A logical value that indicates if the within variance
#' correction should be applied. The default value is TRUE. See details for more
#' information.
#'
#' @param missAllow a number that indicates the maximum proportion of missing
#' values allowed in a sample. If the sample has more proportion of missing
#' values the sample will be eliminated. In the other case the missing values
#' will be imputed using the K-NN algorithm.
#'
#' @details The different estimator methods that can be applied are:
#'\enumerate{
#'    \item "limma"
#'    \item "SMD"
#'    \item "MD"
#' }
#'
#' The \bold{"SMD"} (Standardized mean different) method calculates the effect
#' size using the Hedges'g estimator (Hedges, 1981).
#'
#' The \bold{"MD"} (raw mean different) calculates the effects size as the
#' difference between the means of the two groups (Borenstein, 2009).
#'
#' The \bold{"limma"} method used the limma package to calculate the effect size
#' and the variance of the effect size. The effect size is calculated from the
#' moderated Student's t computed by limma. From it, the estimator of Hedges'g
#' and its corresponding variance are obtained based on 
#' (Rosenthal, R., & Rosnow, R. L., 2008))
#' In this way, some of the false positives obtained by 
#' the "SMD" method are reduced.
#'
#' The \bold{WithinVarCorrect} parameter is a logical value that indicates if
#' the within variance correction should be applied. In the case of applying
#' the correction, the variance of the gene sets in each of the studies is
#' calculated based on the mean of the estimators and not on the estimator of
#' the study itself as described in formula (21) by (Lin L and Aloe AM 2021.)
#'
#'
#' @return A list formed by two elements:
#' \itemize{
#' \item{First element (ES) is a dataframe were columns are each of the studies
#' (datasets) and rows are the genes sets. Each element of the dataframe
#' represents the Effect Size.}
#' \item{Second element (Var) is a dataframe were columns are each of the
#' studies (datasets) and rows are the genes sets. Each element of the dataframe
#' represents the variance of the Effect size.}
#' }
#'
#' @references
#'
#' Borenstein, M. (2009). Effect sizes for continuous data. In H. Cooper,
#' L. V. Hedges, & J. C. Valentine (Eds.),
#' The handbook of research synthesis and meta-analysis (2nd ed., pp. 221–235).
#' New York: Russell Sage Foundation.
#'
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect
#' size and related estimators. Journal of Educational Statistics, 6(2),
#' 107–128. \doi{doi:10.2307/1164588}
#'
#' Lin L, Aloe AM (2021). Evaluation of various estimators for standardized mean
#' difference in meta-analysis. Stat Med. 2021 Jan 30;40(2):403-426.
#' \doi{10.1002/sim.8781}
#' 
#' Rosenthal, R., & Rosnow, R. L. (2008). Essentials of behavioral research:
#' Methods and data analysis. McGraw-Hill.
#'
#' @author Juan Antonio Villatoro Garcia,
#' \email{juanantoniovillatorogarcia@@gmail.com}
#'
#' @seealso \code{\link{createObjectMApath}}
#'
#' @examples
#'
#' data("simulatedData")
#' calculateESpath(objectMApath = objectMApathSim, measure = "limma")
#'
#' @export



#Function that combinaes all the functions
calculateESpath <- function(objectMApath, measure = c("limma", "SMD", "MD"),
    WithinVarCorrect = TRUE, missAllow = 0.3){
    measure <- match.arg(measure)
    objectMApath <- .metaImpute(objectMApath, missAllow=missAllow)
    K <- length(objectMApath)
    if(is.null(names(objectMApath))){
        names(objectMApath)<- paste("Study",seq_len(K),sep="")
    }
    Effect <- list(0)
    Variance <- list(0)
    for (i in seq_len(K)) {
        res <- .getESpath(objectMApath[i], measure = measure)
        colnames(res$ES) <- colnames(res$Var) <- names(objectMApath[i])
        Effect[[i]] <- res$ES
        Variance[[i]] <- res$Var
    }
    names(Effect) <- names(objectMApath)
    names(Variance) <- names(objectMApath)
    Total.Effect <- .matrixmerge(Effect)
    Total.Var <- .matrixmerge(Variance)
    result <- list(ES = Total.Effect, Var = Total.Var)
    if(WithinVarCorrect == TRUE){
        m_g <-.get_m_g(result)
        var_g_correct <- list(0)
        for (i in seq_len(K)) {
            var_g <- .getVar_biasCorrection(objectMApath[i], m_g)
            var_g_correct[[i]] <- var_g
        }
        names(var_g_correct) <- colnames(result$Var)
        Total.Var_correct <- .matrixmerge(var_g_correct)
        result$Var <- Total.Var_correct
    }
    return(result)
}


#Traditional form of calculate Effects size
.calculateES <- function(objectMA, missAllow = 0.3){
    objectMA <- .metaImpute(objectMA, missAllow=missAllow)
    K <- length(objectMA)
    if(is.null(names(objectMA))){
        names(objectMA)<- paste("Study",seq_len(K),sep="")
    }
    Effect <- list(0)
    Variance <- list(0)
    for (i in seq_len(K)) {
        res <- .getESpath(objectMA[i])
        colnames(res$ES) <- colnames(res$Var) <- names(objectMA[i])
        Effect[[i]] <- res$ES
        Variance[[i]] <- res$Var
    }
    names(Effect) <- names(objectMA)
    names(Variance) <- names(objectMA)
    Total.Effect <- .matrixmerge(Effect)
    Total.Var <- .matrixmerge(Variance)
    result <- list(ES = Total.Effect, Var = Total.Var)
    return(result)
}



#FUNCTION FOR MERGING MATRIX TAKING INTO ACCOUNT MISSING ROWS
.matrixmerge <- function(lista){
    t.lista <- lapply(lista, t)
    fused <- plyr::rbind.fill.matrix(t.lista)
    fused <- t(fused)
    colnames(fused) <- names(lista)
    return(fused)
}


#Function for delete samples with missing values
.deleteNa <- function(df) {
    no.miss <- colSums(is.na(df[[1]])) <= 0
    df[[1]] = df[[1]][,no.miss]
    df[[2]] = df[[2]][no.miss]
    return(df)
}
#FUNCTION FOR FILTERING SAMPLES WITH MORE THAN % MISSING VALUES
.metaImpute <- function(objectMA,missAllow){
    index.miss <- which(vapply(objectMA,
        FUN = function(y)any(is.na(y[[1]])),
        FUN.VALUE = TRUE))
    if(length(index.miss)>0){
        for(j in index.miss){
            k<-nrow(objectMA[[j]][[1]])
            rnum<-which(apply(objectMA[[j]][[1]],2,
                function(y) sum(is.na(y))/k)<missAllow)
            if(length(rnum)>1){
                objectMA[[j]][[1]][,rnum]<-impute.knn(
                    objectMA[[j]][[1]][,rnum],
                    k=10)$data}
            objectMA[[j]]<-.deleteNa(objectMA[[j]])
        }
    }
    return(objectMA)
}



#Functions for calculating effects size Response RAtio
.indCalESpath <-function(y,l, measure){

    if(measure == "MD"){
        #MD option
        l <- unclass(factor(l))
        m2 <- rowMeans(y[,l==1])
        m1 <- rowMeans(y[,l==2])
        sd2 <- apply(y[,l==1],1,sd)
        sd1 <- apply(y[,l==2],1,sd)
        n2 <- rep(sum(l==1), length(m1))
        n1 <- rep(sum(l==2), length(m1))
        result <- escalc(measure = "MD", m1i = m1, m2i = m2, sd1i = sd1,
            sd2i = sd2, n1i =n1, n2i = n2, vtype = "LS")
    }

    #Option SMD
    if(measure == "SMD"){
        l <- unclass(factor(l))
        m2 <- rowMeans(y[,l==1])
        m1 <- rowMeans(y[,l==2])
        sd2 <- apply(y[,l==1],1,sd)
        sd1 <- apply(y[,l==2],1,sd)
        n2 <- rep(sum(l==1), length(m1))
        n1 <- rep(sum(l==2), length(m1))
        result <- escalc(measure = "SMD", m1i = m1, m2i = m2, sd1i = sd1,
            sd2i = sd2, n1i =n1, n2i = n2, vtype = "LS2")
    }

    #Option t limma
    if(measure == "limma"){
        l <- unclass(factor(l))
        design <- matrix(data=0, ncol=2, nrow=ncol(y))
        rownames(design) <- colnames(y)
        colnames(design) <- c("CONTROL","CASE")
        for(i in seq_len(length(l))){
            if(l[[i]] == 2){
                design[i,1] = 0
                design[i,2] = 1
            }
            else{
                design[i,1] = 1
                design[i,2] = 0
            }
        }
        fit <- lmFit(y, design)
        contrast.matrix <- makeContrasts("CASE-CONTROL", levels = design)
        fit2 <- contrasts.fit(fit, contrast.matrix)
        fit2 <- eBayes(fit2)
        top.Table <- topTable(fit2, coef=1, adjust.method="BH",number=nrow(y),
            sort.by="none")
        ti <-  top.Table$t
        dfs <- fit2$df.total[1]
        n <- table(factor(l))
        #d <- (2*ti)/sqrt(fit2$df.total)
        d <- (sum(n)*ti)/(sqrt(fit2$df.total) * sqrt(sum(l==1)*sum(l==2)))
        ind <- diag(rep(1,length(n)))[l,]
        ym<-y%*%ind%*%diag(1/n)
        ntilde <- 1/sum(1/n)
        m <-sum(n)-2
        cm <- 1 - (3 / (4 * dfs -1))
        #cm <- 1 - (3 / (4 * m -1))
        dprime <- d*cm
        terme1 <- 1/ntilde
        vard <- (sum(l==1)^(-1)+sum(l==2)^(-1))+(d^2)/(2*(sum(l==1)+sum(l==2)))
        #vardprime <- sum(1/n)+dprime^2/(2*sum(n))
        vardprime = cm^2 * vard
        result <- cbind( dprime, vardprime)
    }
    colnames(result) <- c("ES", "VarES")
    rownames(result) <- rownames(y)
    return(result)
}


.getESpath <- function(x, measure){
    K <- length(x)
    ES.m <- Var.m <- N <- n <- NULL
    y <- x[[1]][[1]]
    l <- x[[1]][[2]]
    temp <- .indCalESpath(y,l, measure = measure)
    ES.m <- as.matrix(temp[,"ES"])
    Var.m <- as.matrix(temp[,"VarES"])
    N <- c(N,length(l))
    n <- c(n,table(l))
    rownames(ES.m) <- rownames(y)
    rownames(Var.m) <- rownames(y)
    res <- list(ES = ES.m,Var = Var.m)
    return(res)
}



#Functions for the Within Var correction
#Obtain the different mean of Hedges' g
.get_m_g <- function(allES){
    ESs <- allES$ES
    Vars <- allES$Var
    numerator_m_g <- rowSums(ESs/Vars, na.rm = TRUE)
    denominator_m_g <- rowSums(1/Vars, na.rm = TRUE)
    m_g <- numerator_m_g / denominator_m_g
    return(m_g)
}

#Obtain the variance with bias m_g correction
.indCalVar_biasCorrection <- function(y,l, m_g){
    l <- unclass(factor(l))
    n <- table(factor(l))
    var_g <- (1/sum(l==1)) + (1/sum(l==2)) + (m_g^2/(2*(sum(l==1) + sum(l==2))))
    return(var_g)
}

.getVar_biasCorrection <-function(x, m_g){
    K <- length(x)
    ES.m <- Var.m <- N <- n <- NULL
    y <- x[[1]][[1]]
    l <- x[[1]][[2]]
    var_g <- .indCalVar_biasCorrection(y,l,m_g)
    var_g <- as.matrix(var_g)
    names(var_g) <- rownames(y)
    return(var_g)
}



