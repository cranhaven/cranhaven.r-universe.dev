##################################################################################
# ZeBook 3rd edition. (c) F. Brun (ACTA), JW Jones (UF)
# Maize model, with harvest index and yield.
# from Muchow RC, Sinclair TR, and Bennett JM (1990). Temperature and Solar Radiation
# Effects on Potential Maize Yield across Locations AGRONOMY JOURNAL, VOL. 82, MARCH-APRIL 1990
##################################################################################
#' @title Expanded leaf area function for Muchow et al. (1990) Maize model
#' @description Compute fully expanded area by leaf number (A, cm2)
#' @param LN : Leaf number
#' @param AM : area of the largest leaf (cm2)
#' @param LNM : leaf number having the largest area (-)
#' @param a1 : coefficient of the statistical relation (default : -0.0344)
#' @param a2 : coefficient of the statistical relation (default : 0.000731)
#' @return vector of Expanded leaf area
#' @seealso \code{\link{maize.muchow.model}}, \code{\link{mm.LN.fct}}, \code{\link{mm.FAS.fct}}, \code{\link{maize.multisy}},
#' \code{\link{mm.HI.fct}},\code{\link{maize.muchow.graph}}
#' @export
#' @examples barplot(mm.A.fct(LN=1:20, AM=750, LNM=12),names.arg=1:20,
#' horiz=TRUE,xlab="leaf area (cm2)",ylab="leaf number")
mm.A.fct = function(LN, AM, LNM,a1=-0.0344,a2=0.000731){AM * exp(a1*(LN-LNM)^2 + a2*(LN-LNM)^3)}
##################################################################################
#' @title Leaf number function for Muchow et al. (1990) Maize model
#' @description Leaf number as a function of thermal time
#' @param TT1 : Thermal time from sowing (degC.day)
#' @param TTE : Thermal units from sowing to emergence/leaf growth (degC.day)
#' @param b1 : coefficient of the statistical relation (default : 2.5)
#' @param b2 : coefficient of the statistical relation (default : 0.00225)
#' @param TLN : total number of leaves initiated (-)
#' @return Leaf number
#' @seealso \code{\link{maize.muchow.model}}, \code{\link{mm.A.fct}}, \code{\link{mm.FAS.fct}}, \code{\link{maize.multisy}},
#' \code{\link{mm.HI.fct}},\code{\link{maize.muchow.graph}}
#' @export
#' @examples plot(1:1000,mm.LN.fct(1:1000,TTE=87))
mm.LN.fct = function(TT1,TTE,b1=2.5,b2=0.00225,TLN=20){pmin(round(b1*exp((TT1-TTE)*b2)), TLN)}
##################################################################################
#' @title Senescence function for Muchow et al. (1990) Maize model
#' @description Senesced fraction of total leaf area (FAS) increase with thermal units (TU) from emergence
#' @param TT : Thermal time (degC.day)
#' @param TTE : Thermal units from sowing to emergence/leaf growth (degC.day)
#' @param c1 : coefficient of the statistical relation (default : 0.00161)
#' @param c2 : coefficient of the statistical relation (default : 0.00328)
#' @return Senesced fraction of total leaf area
#' @seealso \code{\link{maize.muchow.model}}, \code{\link{mm.A.fct}}, \code{\link{mm.LN.fct}}, \code{\link{maize.multisy}},
#' \code{\link{mm.HI.fct}},\code{\link{maize.muchow.graph}}
#' @export
#' @examples plot(1:2500,mm.FAS.fct(1:2500,TTE=87))
mm.FAS.fct = function(TT,TTE,c1=0.00161,c2=0.00328){pmin(c1*exp(c2*(TT-TTE)),1)}
##################################################################################
#' @title Harvest index function for Muchow et al. (1990) Maize model
#' @description Compute the harvest index.
#' @param day : day of the year
#' @param daysilking : day of the year for silking (day)
#' @param HImax : maximum harvest index - genetic potential (-)
#' @param d1 : coefficient of the statistical relation (day-1, default : 0.015)
#' @param d2 : coefficient of the statistical relation (day, default : 3)
#' @return Harvest index
#' @seealso \code{\link{maize.muchow.model}}, \code{\link{mm.A.fct}}, \code{\link{mm.LN.fct}}, \code{\link{maize.multisy}},
#' \code{\link{mm.FAS.fct}},\code{\link{maize.muchow.graph}}
#' @export
#' @examples plot(1:350, mm.HI.fct(1:350, 200, 0.75), type="l")
mm.HI.fct = function(day,daysilking, HImax,d1=0.015,d2=3){pmin(HImax, pmax(0, d1*(day-(daysilking + d2))) )}
##################################################################################
#' @title Maize model, with harvest index and yield from Muchow et al. (1990)
#' @description Maize model, with harvest index and yield.
#' from Muchow RC, Sinclair TR, and Bennett JM (1990). Temperature and Solar Radiation
#' Effects on Potential Maize Yield across Locations AGRONOMY JOURNAL, VOL. 82, MARCH-APRIL 1990
#' @param Tbase1 : base temperature before silking(degC)
#' @param TTE : Thermal units from sowing to emergence/leaf growth (degC.day)
#' @param TTS : Thermal units from end of leaf growth to silking (degC.day)
#' @param Tbase2 : base temperature after silking (degC)
#' @param TTRUE : Thermal units from silking for RUE change (degC.day)
#' @param TTM : Thermal units from silking to physiological maturity (degC.day)
#' @param TLN : total number of leaves initiated (-)
#' @param AM : area of the largest leaf (cm2)
#' @param RUE1 : radiation use efficiency (g.MJ-1) from crop emergence until 500 thermal units (base 0 "C) after silking
#' @param RUE2 : radiation use efficiency (g.MJ-1) from 500 thermal units (base 0 "C) after silking
#' @param K : radiation extinction coefficient (-)
#' @param HImax : maximum harvest index - genetic potential (-)
#' @param Population : number of plant per square meter (-)
#' @param sdate : sowing date (day)
#' @param ldate : end of simulation (day)
#' @param weather : daily weather dataframe
#' @return data.frame with TT1, TT2, STADE, LN, LAI, B, HI, YIELD
#' @seealso \code{\link{mm.A.fct}}, \code{\link{mm.LN.fct}}, \code{\link{mm.FAS.fct}}, \code{\link{maize.multisy}},
#' \code{\link{mm.HI.fct}},\code{\link{maize.muchow.graph}}
#' @export
#' @examples
#' # not run in package test
#' # res = maize.muchow.model(weather=maize.weather(working.year=2010, working.site=1))
#' #res$FinalYield
maize.muchow.model=function(Tbase1=8,TTE=87,TTS=67,Tbase2=0,TTRUE=500,TTM=1150,TLN=20,
AM=596,RUE1=1.6,RUE2=1.2,K=0.4,HImax=0.5,Population= 7,sdate=100,ldate=365,
weather) {
  # LNM : leaf number having the largest area (-)
  LNM = round(3.53 + 0.46 * TLN)
  # table of individual leaf : leaf number (LN) and fully expanded area (A)
  tab_leaf = matrix(c(1:TLN, mm.A.fct(LN=1:TLN, AM=AM, LNM=LNM)),nrow = TLN, ncol=2, dimnames=list(NULL, c("LN","A")))

  #3 states variables, as 3 vectors initialized to NA
  # TT : temperature sum (degC.d). TT1: emergence to silking. TT2: silking to maturity
  TT1 <- rep(NA, ldate)
  TT2 <- rep(NA, ldate)
  # B : Biomass (g/m2)
  B <- rep(NA, ldate)
  # LAI : Leaf Area Index (m2 leaf/m2 soil)
  LAI <- rep(NA, ldate)
  LN <- rep(NA, ldate)
  STADE = rep(NA, ldate)
  HI = rep(NA, ldate)

  # Initialize state variable when sowing on day sdate
  TT1[sdate] <- 0
  TT2[sdate] <- 0
  B[sdate] <- 1
  LAI[sdate] <- 0
  LN[sdate] <- 0
  STADE[sdate] = "sown"
  HI[sdate] <- 0
  daysilking = NA
  daymaturity =NA

  # process rate and integration loop
  for (day in sdate:(ldate - 1)) {
    # Calculate the rates of state variables
    # Thermal time
    dTT1 = max((weather$Tmin[day] + weather$Tmax[day])/2 - Tbase1, 0)
    dTT2 = ifelse(is.na(daysilking), 0, max((weather$Tmin[day]+weather$Tmax[day])/2-Tbase2, 0))

    # Photosynthesis
    RUE = ifelse(TT2[day]<=TTRUE, RUE1, RUE2)
    dB = ifelse(STADE[day]=="maturity", 0,   weather$I[day]*RUE*(1 -exp(-K*LAI[day])) )

    # Update all of the state variables
    TT1[day + 1] <- TT1[day] + dTT1
    TT2[day + 1] <- TT2[day] + dTT2
    B[day + 1] <- B[day] + dB
    STADE[day+1] = STADE[day]

    # Compute auxilliary variables
    # Leaf number = (TT from emergence)
    LN[day+1] = ifelse(STADE[day]=="leaf grow",min(round(2.5*exp((TT1[day+1]-TTE)*0.00225)), TLN), LN[day])
    #PLA : Plant Leaf Area (cm2)
    PLA = sum(tab_leaf[tab_leaf[,"LN"]<=(LN[day+1]+2), "A"])
    LAI[day+1] = ifelse(STADE[day]=="sown", 0,Population * (PLA/(100*100)*(1-mm.FAS.fct(TT1[day+1],TTE=TTE))))

    # Phenology
    if(STADE[day]=="sown" & TT1[day+1]>= TT1[STADE=="sown" & !is.na(TT1)][1] +TTE){STADE[day+1]="leaf grow"}
    if(STADE[day]=="leaf grow" & LN[day+1]==TLN){STADE[day+1]="pre-silking"}
    if(STADE[day]=="pre-silking" & TT1[day+1]>= TT1[STADE=="pre-silking" & !is.na(TT1)][1] +TTS){STADE[day+1]="silking"}
    if(STADE[day]=="silking" & TT2[day+1]>= TTM){STADE[day+1]="maturity"}

    # Day of silking
    daysilking=(1:ldate)[STADE=="silking"&!is.na(STADE)][1]
    # Day of maturity
    daymaturity=(1:ldate)[STADE=="maturity"&!is.na(STADE)][1]
    #print(daysilking)
    # Harverst index
    HI[day+1] = mm.HI.fct(day+1, daysilking=daysilking , HImax=HImax)
    #print(HI)
  }
  # End loop

  # Yield
  YIELD =  HI * B

  if (!is.na(daymaturity)){
    ldate = daymaturity
    FinalYield = YIELD[daymaturity]
  }

  STADE=factor(STADE, levels=unique(STADE))

  return(list(sim=data.frame(day = sdate:ldate, TT1 = TT1[sdate:ldate],TT2 = TT2[sdate:ldate], STADE=STADE[sdate:ldate],
              LN = LN[sdate:ldate], LAI = LAI[sdate:ldate],
              B = B[sdate:ldate], HI = HI[sdate:ldate],YIELD = YIELD[sdate:ldate], stringsAsFactors=F),daysilking=daysilking, daymaturity=daymaturity,FinalYield=FinalYield,tab_leaf=tab_leaf))
}
#################################################################################
#' @title Plot dynamic output of Muchow Maize model.
#' @description Plot 6 graphs of main output variables of the Muchow Maize model.
#' @param res : list of result from maize.muchow.model
#' @seealso \code{\link{mm.A.fct}}, \code{\link{mm.LN.fct}}, \code{\link{mm.FAS.fct}}, \code{\link{maize.multisy}},
#' \code{\link{mm.HI.fct}},\code{\link{maize.muchow.model}}
#' @export
#' @examples
#' # not run in package test
#' # res = maize.muchow.model(weather=maize.weather(working.year=2010, working.site=1))
#' # maize.muchow.graph(res)
maize.muchow.graph=function(res){
  par(mfrow=c(2,3))
  barplot(res$tab_leaf[,2],names.arg=res$tab_leaf[,1],horiz=T,xlab="leaf area (cm2)",ylab="leaf number")
  plot(res$sim$day, res$sim$LN, type="l", xlab="day", ylab="Leaf number (-)",lwd=2)
  plot(res$sim$day, res$sim$LAI, type="l", xlab="day", ylab="Leaf Area Index (-)",lwd=2)
  plot(res$sim$day, res$sim$TT1, type="l", xlab="day", ylab="TT1 and TT2 (degC.day)",lwd=2)
  lines(res$sim$day, res$sim$TT2, lty=2,lwd=2)
  plot(res$sim$day, res$sim$B, type="l", xlab="day", ylab="Biomass and Yield (g.m-2)",lwd=2)
  lines(res$sim$day, res$sim$YIELD, lty=2,lwd=2)
  #plot(res$sim$day, res$sim$HI, type="l", xlab="day", ylab="Harvest Index (-)",lwd=2)
  plot(res$sim$day,rep(0,length(res$sim$day)),col=res$sim$STADE,pch=15,xlab="day",ylab="phenology",lwd=2,  yaxt='n',ylim=c(-2,10))
  legend("top", pch=15,ncol=2,col=unique(res$sim$STADE),legend=levels(res$sim$STADE),cex=0.75)
}
##################################################################################
#end of file
