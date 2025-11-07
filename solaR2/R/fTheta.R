fTheta<-function(sol, beta, alpha = 0, modeTrk = 'fixed', betaLim = 90, 
                 BT = FALSE, struct, dist)
{
    stopifnot(modeTrk %in% c('two','horiz','fixed'))
    if (!missing(struct)) {stopifnot(is.list(struct))}
    if (!missing(dist)) {stopifnot(is.data.frame(dist))}
    
    betaLim <- d2r(betaLim)
    lat <- getLat(sol, 'rad')
    signLat <- ifelse(sign(lat) == 0, 1, sign(lat)) ##When lat = 0, sign(lat) = 0. I change it to sign(lat) = 1
    
    solI <- as.data.tableI(sol, complete = TRUE, day = TRUE)
    AlS <- solI$AlS
    AzS <- solI$AzS
    decl <- solI$decl
    w <- solI$w
    
    night <- solI$night

    Beta <- switch(modeTrk,
                   two = {Beta2x = pi/2-AlS	
                       Beta = Beta2x+(betaLim-Beta2x)*(Beta2x>betaLim)},
                   fixed = rep(d2r(beta), length(w)), 
                   horiz = {BetaHoriz0 = atan(abs(sin(AzS)/tan(AlS)))
                       if (BT){lew = dist$Lew/struct$L
                           Longitud = lew*cos(BetaHoriz0)
                           Cond = (Longitud>=1)
                           Longitud[Cond] = 1
                           ## When Cond==TRUE Length = 1
                           ## and therefore asin(Length) = pi/2,
                           ## so that BetaHoriz = BetaHoriz0
                           BetaHoriz = BetaHoriz0+asin(Longitud)-pi/2                                     
                       } else {
                           BetaHoriz = BetaHoriz0
                           rm(BetaHoriz0)}
                       Beta = ifelse(BetaHoriz>betaLim,betaLim,BetaHoriz)}
                   )
    is.na(Beta) <- night

    Alpha<-switch(modeTrk,
                 two = AzS,
                 fixed = rep(d2r(alpha), length(w)),
                 horiz=pi/2*sign(AzS))
    is.na(Alpha) <- night
    
    cosTheta<-switch(modeTrk,
                     two = cos(Beta-(pi/2-AlS)),
                     horiz = {
                         t1 = sin(decl)*sin(lat)*cos(Beta)      
                         t2 = cos(decl)*cos(w)*cos(lat)*cos(Beta)   
                         t3 = cos(decl)*abs(sin(w))*sin(Beta)   
                         cosTheta = t1+t2+t3
                         rm(t1,t2,t3)
                         cosTheta
                     },
                     fixed = {
                         t1 = sin(decl)*sin(lat)*cos(Beta)      
                         t2 = -signLat*sin(decl)*cos(lat)*sin(Beta)*cos(Alpha) 
                         t3 = cos(decl)*cos(w)*cos(lat)*cos(Beta)   
                         t4 = signLat*cos(decl)*cos(w)*sin(lat)*sin(Beta)*cos(Alpha) 
                         t5 = cos(decl)*sin(w)*sin(Alpha)*sin(Beta)   
                         cosTheta = t1+t2+t3+t4+t5
                         rm(t1,t2,t3,t4,t5)
                         cosTheta
                     }
                     )
    is.na(cosTheta) <- night
    cosTheta = cosTheta*(cosTheta>0) #when cosTheta<0, Theta is greater than 90º, and therefore the Sun is behind the panel.
    
    result <- data.table(Dates = indexI(sol),
                         Beta, Alpha, cosTheta)
    return(result)
}
