insertMatBtoA <- function(A, B) {
    namesA <- rownames(A)
    namesB <- rownames(B)
    namesAinB <- namesA[namesA %in% namesB]
    namesA2 <- colnames(A)
    namesB2 <- colnames(B)
    namesAinB2 <- namesA2[namesA2 %in% namesB2]
    A[namesA %in% namesB, namesA2 %in% namesB2] <- B[match(namesAinB, 
        namesB), match(namesAinB2, namesB2)]
    return(A)
    
}
