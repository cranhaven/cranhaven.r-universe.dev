fSombraHoriz<-function(angGen, distances, struct)
{
    stopifnot(is.list(struct), is.data.frame(distances))
    ## I prepare starting data 	
    d <- with(struct, distances/L)
    AzS <- angGen$AzS
    AlS <- angGen$AlS
    Beta <- angGen$Beta
    lew <- d$Lew #It must be previously normalized
    ## Calculations
    Beta0 <- atan(abs(sin(AzS)/tan(AlS)))
    FS <- 1-lew*cos(Beta0)/cos(Beta-Beta0)
    SombraCond <- (FS>0)
    ## Result
    FS <- FS*SombraCond
    FS[FS>1] <- 1
    return(FS)
}
