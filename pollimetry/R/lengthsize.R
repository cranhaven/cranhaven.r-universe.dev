#' @name lengthsize
#' 
#' @title Converts body length to body size (body weight (mg)) for three pollinating insect groups (Diptera, Hymenoptera and Lepidoptera).
#' 
#' @description Calculates body size as dry  weight (mg) from existing allometries (See 'Details') using body length values (mm).  
#' 
#' @param BL A vector of pollinator body length (BL) measurements (mm).
#' 
#' @param Eq A vector specificing which predictive allometry to use. Acronyms denote first (and second author), publication date and taxon. DIP, HYM and LEP options are cases where authors modelled body size across all specimens within each order.
#' Options implemented are: DIP (10 equations): BN06D, G97D, GR84D, JS00DA, R77D, 
#'                          S80DCF, S80DCR, S80DMF, S93DA, W13D)<br />
#'                          Brachycera (2 equations): S93DB, Sabo02DB 
#'                          Nematocera (3 equations): JS00DN, S93DN, Sabo02DN; 
#'                          Asilidae (2 equations): Sabo02DA, S93DB; 
#'                          Bombyliidae (2 equations): S93DB, Sabo02DBB;
#'                          Cyclorrapha: S93DC
#'                          
#'                          HYM (12 equations): BN06H1, G97H, G97F, GR84H, JS00HA, 
#'                          R77H, S80HCF, S80HCR, S80HMF, S93HA, Sabo02H, W13H<br />
#'                          Formicidae (13 equations): BN06HF, GR84F, JS00HF, R77A,
#'                          S80FCF, S80FCR, S80FMF, S93HF, S93HH, S93HI, BN06H1, G97H, G97F<br />
#'                          Apidae: Sabo02HA; 
#'                          Braconidae: S93HB; 
#'                          Halictidae: S93HH; 
#'                          Ichneumonidae: S93HI; 
#'                          Pompilidae: S93HP; 
#'                          Vespidae: S93HV and Sabo02HV
#'                          
#'                          LEP(9 equations): BN06L,G97L,JS00L,R77L,S80LCF, S80LCR,
#'                          S80LMF,S93LA,W13L<br />
#'                          Noctuidea: S93LC, S93LN; 
#'                          Geometridae: S93LG; 
#'                          Microlepidoptera: S93LM; 
#'                          Arctiinae: S93LC.
#' 
#' @return A dataframe with pollinator body size (mg) is returned for each species from selected equation/s.
#' 
#' @examples
#' lengthsize(BL=c(10,5,2), Eq = c("S80DCR"))
#' @references Full reference list is available within Kendall et al. (2018) Pollinator size and its consequences: Predictive allometry for pollinating insects. <doi:10.1101/397604>
#' 
#' 
#' @export
lengthsize=function(BL, Eq = "DIP"){
  
if(!Eq %in% c("DIP","HYM","LEP", "S93DB","Sabo02DB", "Sabo02DA", "Sabo02DBB", "R77D",
              "S93DC", "BN06D", "G97D", "GR84D", "JS00DA",
              "S80DCF", "S80DCR", "S80DMF", "S93DA", "W13D", "JS00DN", 
              "S93DN", "Sabo02DN","DIP","Brachycera","Nematocera",
              "Asilidae","Bombyliidae")){
  stop("Equation should be one of 'DIP','HYM','LEP','R77D',
       'S93DB', 'Sabo02DB', 'Sabo02DA', 'Sabo02DBB', 
       'S93DC', 'BN06D', 'G97D', 'GR84D', 'JS00DA', 
       'S80DCF', 'S80DCR', 'S80DMF', 'S93DA', 'W13D', 'JS00DN', 
       'S93DN', 'Sabo02DN','All','Brachycera','Nematocera','Asilidae','Bombyliidae'")
} else {
  S93DB <- exp(-3.374+2.158*log(BL))
  Sabo02DB <- 0.006*(BL)^3.05
  Sabo02DA <- 0.38*(BL)^1.5
  Sabo02DBB <- 0.007*(BL)^3.337
  S93DC <- exp(-3.619+2.632*log(BL))
  BN06D <- exp(-0.041+0.010*(BL))
  G97D <- exp(-3.4294+2.5943*log(BL))
  GR84D <- exp(-3.653+2.546*log(BL))
  JS00DA <- exp(-2.462+1.881*log(BL))
  R77D <- exp(-3.293+2.366*log(BL))
  S80DCF <- exp(log(0.074)+1.64*log(BL))
  S80DCR <- exp(log(0.068)+1.59*log(BL))
  S80DMF <- exp(log(0.022)+2.42*log(BL))
  S93DA <- exp(-3.184+2.23*log(BL))
  W13D <- exp(-3.29+2.65*log(BL))
  JS00DN <- exp(-2.462+1.881*log(BL))
  S93DN <- exp(-3.675+2.212*log(BL))
  Sabo02DN <- 0.1*(BL)^1.57
  Sabo02HA <- 0.006*(BL)^3.407
  S93HB <- exp(-3.854+2.441*log(BL))
  BN06HF <- exp(log(0.001)+2.33*log(BL))
  GR84F <- exp(-3.997+log(BL)*2.489)
  JS00HF <- exp(-3.730+2.103*log(BL))
  R77A <- exp(-4.029+2.572*log(BL))
  S80FCF <- exp(log(0.012)+2.72*log(BL))
  S80FCR <- exp(log(0.021)+2.31*log(BL))
  S80FMF <- exp(log(0.034)+2.19*log(BL))
  S93HF <- exp(-4.727+2.919*log(BL))
  S93HH <- exp(-2.891+2.302*log(BL))
  S93HI <- exp(-4.149+2.464*log(BL))
  BN06H1 <- exp(-6.783+2.544*log(BL))
  G97H <- exp(-3.5917+2.6429*log(BL))
  G97F <- exp(-3.1415+2.3447*log(BL))
  GR84H <- exp(-2.86+(BL)*0.478)
  JS00HA <- exp(-3.556+2.193*log(BL))
  R77H <- exp(-3.871+2.407*log(BL))
  S80HCF <- exp(log(0.043)+2.07*log(BL))
  S80HCR <- exp(log(0.022)+2.29*log(BL))
  S80HMF <- exp(log(0.016)+2.55*log(BL))
  S93HA <- exp(-4.284+2.696*log(BL))
  Sabo02H <- 0.56*(BL)^1.56
  W13H <- exp(-4.3+3*log(BL))
  S93HP <- exp(-2.341+2.006*log(BL))
  S93HV <- exp(-3.54+2.782*log(BL))
  Sabo02HV <- 0.001*(BL)^3.723
  S93LG <- exp(-4.172+2.628*log(BL))
  S93LM <- exp(-4.913+2.918*log(BL))
  BN06L <- exp(log(0.001)+2.313*log(BL))
  G97L <- exp(-4.7915+2.8585*log(BL))
  JS00L <- exp(-3.268+2.243*log(BL))
  R77L <- exp(-4.037+2.903*log(BL))
  S80LCF <- exp(log(0.026)+2.5*log(BL))
  S80LCR <- exp(log(0.078)+1.32*log(BL))
  S80LMF <- exp(log(0.014)+2.55*log(BL))
  S93LA <- exp(-5.036+3.122*log(BL))
  W13L <- exp(-3.83+2.77*log(BL))
  S93LC <- exp(-3.755+2.658*log(BL))
  S93LN <- exp(-3.337+2.499*log(BL))
  
  
  if (Eq ==  "S93DB") out <- S93DB
  if (Eq == "Sabo02DB") out <- Sabo02DB
  if (Eq == "Sabo02DA") out <- Sabo02DA
  if (Eq == "Sabo02DBB") out <- Sabo02DBB
  if (Eq == "S93DC") out <- S93DC
  if (Eq == "BN06D") out <- BN06D
  if (Eq == "G97D") out <- G97D
  if (Eq == "GR84D") out <- GR84D
  if (Eq == "JS00DA") out <- JS00DA
  if (Eq == "R77D") out <- R77D
  if (Eq == "S80DCF") out <- S80DCF
  if (Eq == "S80DCR") out <- S80DCR
  if (Eq == "S80DMF") out <- S80DMF
  if (Eq == "S93DA") out <- S93DA
  if (Eq == "W13D") out <- W13D
  if (Eq == "JS00DN") out <- JS00DN
  if (Eq == "S93DN") out <- S93DN
  if (Eq == "Sabo02DN") out <- Sabo02DN
  if (Eq == "Brachycera") out <- cbind(S93DB, Sabo02DB)
  if (Eq == "Nematocera") out <- cbind(JS00DN, S93DN, Sabo02DN)
  if (Eq == "Asilidae") out <- cbind(Sabo02DA, S93DB)
  if (Eq == "Bombyliidae") out <-cbind(Sabo02DBB,S93DB)
  if (Eq == "DIP") out <- cbind(BN06D, G97D, GR84D, JS00DA, R77D, S80DCF, S80DCR, S80DMF, S93DA, W13D)
  
  if (Eq ==  "Sabo02HA") out <- Sabo02HA
  if (Eq == "S93HB") out <- S93HB
  if (Eq == "BN06HF") out <- BN06HF
  if (Eq == "GR84F") out <- GR84F
  if (Eq == "JS00HF") out <- JS00HF
  if (Eq == "R77A") out <- R77A
  if (Eq == "S80FCF") out <- S80FCF
  if (Eq == "S80FCR") out <- S80FCR
  if (Eq == "S80FMF") out <- S80FMF
  if (Eq == "S93HF") out <- S93HF
  if (Eq == "S93HH") out <- S93HH
  if (Eq == "S93HI") out <- S93HI
  if (Eq == "BN06H1") out <- BN06H1
  if (Eq == "G97H") out <- G97H
  if (Eq == "G97F") out <- G97F
  if (Eq == "GR84H") out <- GR84H
  if (Eq == "JS00HA") out <- JS00HA
  if (Eq == "R77H") out <- R77H
  if (Eq == "S80HCF") out <- S80HCF
  if (Eq == "S80HCR") out <- S80HCR
  if (Eq == "S80HMF") out <- S80HMF
  if (Eq == "S93HA") out <- S93HA
  if (Eq == "Sabo02H") out <- Sabo02H
  if (Eq == "W13H") out <- W13H
  if (Eq == "S93HP") out <- S93HP
  if (Eq == "S93HV") out <- S93HV
  if (Eq == "Sabo02HV") out <- Sabo02HV
  if (Eq == "Vespidae") out <- cbind(S93HV,Sabo02HV)
  if (Eq == "Formicidae") out <- cbind(BN06HF,GR84F,JS00HF,R77A,S80FCF,S80FCR,S80FMF,S93HF,S93HH,S93HI,BN06H1,G97H,G97F)
  if (Eq == "HYM") out = cbind(BN06H1,G97H, G97F,GR84H,JS00HA,R77H,S80HCF,S80HCR,S80HMF,S93HA,Sabo02H,W13H)
  
  if (Eq ==  "S93LG") out <- S93LG
  if (Eq == "S93LM") out <- S93LM
  if (Eq == "BN06L") out <- BN06L
  if (Eq == "G97L") out <- G97L
  if (Eq == "JS00L") out <- JS00L
  if (Eq == "R77L") out <- R77L
  if (Eq == "S80LCF") out <- S80LCF
  if (Eq == "S80LCR") out <- S80LCR
  if (Eq == "S80LMF") out <- S80LMF
  if (Eq == "S93LA") out <- S93LA
  if (Eq == "W13L") out <- W13L
  if (Eq == "S93LC") out <- S93LC
  if (Eq == "S93LN") out <- S93LN
  if (Eq == "LEP") out <- cbind(BN06L,G97L,JS00L,R77L,S80LCF, S80LCR,S80LMF,S93LA,W13L)
  if (Eq == "Noctuidea") out <- cbind(S93LC, S93LN)
  out
}
}  
