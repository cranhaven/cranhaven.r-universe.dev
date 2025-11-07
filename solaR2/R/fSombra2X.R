fSombra2X<-function(angGen,distances,struct)
{
    stopifnot(is.list(struct),is.data.frame(distances))
    ##I prepare starting data	
    P <- with(struct,distances/W)
    b <- with(struct,L/W)
    AzS <- angGen$AzS
    Beta <- angGen$Beta
    AlS <- angGen$AlS
    
    d1 <- abs(P$Lew*cos(AzS)-P$Lns*sin(AzS))
    d2 <- abs(P$Lew*sin(AzS)+P$Lns*cos(AzS))
    FC <- sin(AlS)/sin(Beta+AlS)
    s <- b*cos(Beta)+(b*sin(Beta)+P$H)/tan(AlS)
    FS1 <- 1-d1
    FS2 <- s-d2
    SombraCond <- (FS1>0)*(FS2>0)*(P$Lew*AzS>=0)
    SombraCond[is.na(SombraCond)] <- FALSE #NAs are of no use to me in a logical vector. I replace them with FALSE
    ## Result
    FS <- SombraCond*(FS1*FS2*FC)/b
    FS[FS>1] <- 1
    return(FS)
}	
