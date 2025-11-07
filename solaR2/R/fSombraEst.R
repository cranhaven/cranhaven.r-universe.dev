fSombraEst<-function(angGen, distances, struct)
{
    stopifnot(is.list(struct),is.data.frame(distances))
    ## I prepare starting data
    dist <- with(struct, distances/L)
    Alpha <- angGen$Alpha
    Beta <- angGen$Beta
    AlS <- angGen$AlS
    AzS <- angGen$AzS
    cosTheta <- angGen$cosTheta
    h <- dist$H #It must be previously normalized
    if(is.null(h)) h <- 0
    d <- dist$D                   
    ## Calculations
    s <- cos(Beta)+cos(Alpha-AzS)*(sin(Beta)+h)/tan(AlS)
    FC <- sin(AlS)/sin(Beta+AlS)
    SombraCond <- (s-d>0)
    FS <- (s-d)*SombraCond*FC*(cosTheta>0)
    ## Result
    FS <- FS*(FS>0)
    FS[FS>1] <- 1
    return(FS)
}


