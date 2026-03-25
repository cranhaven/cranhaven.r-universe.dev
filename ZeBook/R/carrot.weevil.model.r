################################################################################
# "Working with dynamic models for agriculture"
# R script for pratical work
# Daniel Wallach (INRA), David Makowski (INRA), James W. Jones (U.of Florida),
# Francois Brun (ACTA)
# version : 2012-05-11
# Model described in the book, Appendix. Models used as illustrative examples: description and R code
################################ FUNCTIONS #####################################
#' @title Carrot weevil development model
#' @description \strong{Model description.} Simple model of developpement of carrot weevil.
#' @param tbase : base temperature
#' @param tteggs : duration of eggs stage in degre.day
#' @param ttlarvae : duration of larvae stage in degre.day
#' @param ttprepupae : duration of prepupae stage in degre.day
#' @param ttpupae : duration of pupae stage in degre.day
#' @param ttadultpreovi : duration of adult stage until egg laying in degre.day
#' @param weather : weather data.frame for one single year
#' @param sdate : date to begin simulation (day of year) (default 1)
#' @param ldate : date to end simulation (day of year) (default 360)
#' @return data.frame with daily state variable
#' @export
carrot.weevil.model = function (tbase=7.0,tteggs=130,ttlarvae=256,ttprepupae=114,ttpupae=130,ttadultpreovi=91,weather, sdate = 1, ldate =360){
    #Host: Carrot	DD
    #Developmental threshold Lower:7.0 degC
    #Eggs:	130.0 degC
    #Larvae:	256.0 degC
    #Prepupae:	114.0 degC
    #Pupae:	130.0 degC
    #Generation time (egg to adult):	630.0 degC
    # period of pre-oviposition of femal : 7 days (hypothesis at 20deg) = 7*(20-7) = 91degC
    ttgeneration = tteggs + ttlarvae + ttprepupae + ttpupae + ttadultpreovi
    TT <- rep(NA, ldate)
    STADE <- rep(NA, ldate)
    NGEN <- rep(NA, ldate)
    TT[sdate] = 0
    STADE[sdate] = 0
    NGEN[sdate] = 0
    for (day in sdate:(ldate - 1)) {
       dTT= max(0,(weather$TMIN[day]+weather$TMAX[day])/2 - tbase)
       print(dTT)
       TT[day+1] = TT[day] + dTT
    }

NGEN = TT %/% ttgeneration
TT = TT %% ttgeneration
STADE[TT>=0 & TT<(tteggs)] = "eggs"
STADE[TT>=tteggs & TT<(tteggs+ttlarvae)] = "larvae"
STADE[TT>=(tteggs+ttlarvae) & TT<(tteggs+ttlarvae+ttprepupae)] = "prepupae"
STADE[TT>=(tteggs+ttlarvae+ttprepupae) & TT<(tteggs+ttlarvae+ttprepupae+ttpupae)] = "pupae"
STADE[TT>=(tteggs+ttlarvae+ttprepupae+ttpupae) & TT<(tteggs+ttlarvae+ttprepupae+ttpupae+ttadultpreovi)] = "adultpreovi"

return(list(sim=data.frame(day = sdate:ldate, DACE = ((sdate:ldate)-sdate), TT = TT[sdate:ldate], STADE = STADE[sdate:ldate], NGEN = NGEN[sdate:ldate], stringsAsFactors=FALSE), param=c(tbase=tbase,tteggs=tteggs,ttlarvae=ttlarvae,ttprepupae=ttprepupae,ttpupae=ttpupae,ttadultpreovi=ttadultpreovi,ttgeneration=ttgeneration)))
}
################################################################################
#' @examples 
#' # not run in package test
#' # weather=subset(weather_FranceWest, WEYR==1997 & idsite==1)
#' # sim = carrot.weevil.model(tbase=7.0,tteggs=130,ttlarvae=256,ttprepupae=114,ttpupae=130,ttadultpreovi=91,weather, sdate = as.numeric(format(strptime("04-01", "%m-%d", tz=""), "%j")), ldate =360)$sim
#' # col_stade=c(eggs="blue",larvae="green", prepupae="orange" , pupae="purple", adultpreovi="red")
#' # sim$col=col_stade[sim$STADE]
#' # plot(sim$day,sim$NGEN, type="p",xlab="day of year",ylab="number of generation", col=col_stade[sim$STADE],lwd=3,pch=15)
#' # legend("topleft",  col= col_stade, legend=names(col_stade),pch=15)
#' # title("simulation of phenology of Carrot Weevil")
# end of file