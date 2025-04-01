#' Wrapper Function for all Supported Soil Hydraulic Property Models
#' @description This function allows to select soil hydraulic property models.
#' @param p Vector of the model parameters, order is sensitve cf respective model documentation.
#' @param h Pressure heads [cm] for which the corresponding retention and conductivity values are calculated.
#' @param shpmodel 
#'  character
#' \tabular{lll}{
#'    \code{01110}\tab{unimodel van Genuchten-Mualem model, with the contraint of m = 1-1/n \insertCite{vanGenuchten.1980}{spsh}}\cr
#'    \code{01210}\tab{bimodel van Genuchten-Mualem model, with the contraint of m_i = 1-1/n_i \insertCite{Durner.1994}{spsh}}\cr
#'    \code{01310}\tab{trimodal van Genuchten-Mualem model, with the contraint of m_i = 1-1/n_i \insertCite{Durner.1994}{spsh}}\cr
#'    \code{02110}\tab{unimodel Kosugi 2 parametric-Mualem model \insertCite{Kosugi.1996}{spsh}}\cr
#'    \code{03110}\tab{unimodel Fredlund-Xing-Mualem model, with the contraint of m = 1-1/n \insertCite{Fredlund.1994}{spsh}}\cr
#' }
#' 
#' @param ivap.query 
#' \tabular{lll}{
#'    \code{NULL}\tab{no isothermal vapour conductivity will be calculated with \code{Kvap}}\cr
#'    \tab{Model type for isothermal vapour conductivity, see Details of function \link[spsh]{KvapFun} for model codes}\cr
#'    }
#' @return a \code{list} with calculated soil hydraulic properties at specified \code{h}:
#' \item{theta}{calculated volumetric moisture content}
#' \item{Se}{calculated saturation}
#' \item{Scap}{effective saturation (of the capillary part if \code{FM} is specified}
#' \item{cap}{specific water capacity function (of the capillary part if \code{FM} is specified}
#' \item{psd}{pore size distribution (of the capillary part if \code{FM} is specified}
#' \item{Kh}{total hydraulic conductivity}
#' if \code{FM} specified, additionally:
#'       
#' \item{thetacap}{calculated volumetric moisture content of the capillary part}
#' \item{thetanc}{calculated volumetric moisture content of the non-capillary part}
#' \item{Snc}{effective saturation of the non-capillary part}
#' \item{Kcap}{hydraulic conductivity of the capillary}
#' \item{Knc}{hydraulic conductivity of the non-capillary}
#' \item{Kvap}{isothermal vapour conductivity}
#' \item{Krcap}{relative hydraulic conductivity of the capillary}
#' \item{Krnc}{relative hydraulic conductivity of the non-capillary}
#' 
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @note the function is used to assign a new function variable with a function which calculates the soil hydraulic properties 
#' according to sepcified \code{shpmodel} and model specified by \code{ivap.query}
#' 
#' @references 
#' \insertRef{Durner.1994}{spsh}\cr
#' \insertRef{Fredlund.1994}{spsh}
#' \insertRef{Kosugi.1996}{spsh}
#' \insertRef{vanGenuchten.1980}{spsh}
#' \insertRef{Weber.2019}{spsh}\cr
#'
#' @examples
#' 
# load measured data
#'  data("shpdata1")
#'  retdata <- shpdata1$LFH1$wrc[!is.na(shpdata1$LFH1$wrc[,1]),]
#'  condata <- shpdata1$LFH1$hcc
#'  
#'  # assign auxiliary variables
#'  pF <- seq(-3, 6.8, length = 501)
#'  h <- 10^pF
#'  
#'  # assign list of parameters for the van Genuchten-Mualem model
#'  parL <- list("p" = c("thr"= 0.05, "ths" = 0.45, "alf1" = 0.01, "n" = 2, "Ks" = 100, "tau" = .5),
#'               "psel" = c(1, 1, 0, 1, 1, 1),
#'               "plo" = c(0.001 , 0.2, 0.001, 1.1, 1, -2),
#'               "pup" = c(0.3, 0.95, 1, 10, 1e4, 10))
#'  
#'  # calculate soil hydraulic property function values
#'  
#'  shyp.L <- shypFun(parL$p, h, shpmodel = "01110", ivap.query = NULL)
#'  wrc <- shyp.L$theta
#'  hcc <- log10(shyp.L$Kh)
#'  
#'  # # PLOT THE MEASURED WATER RETENTION CURVE
#'  ticksatmin <- -2
#'  tcllen <- 0.4
#'  ticksat <- seq(ticksatmin,5,1)
#'  pow <- ticksatmin:6
#'  
#'  par(mfrow = c(1,2), tcl = tcllen)
#'  plot(retdata, ylim = c(.0,.50), xlim = c(0, 6.8), ylab = "", xlab = "",
#'       col = "darkgrey", axes = FALSE, main = "Water Retention Curve", cex = 2)
#'  lines(log10(h), wrc, col = "darkblue", lwd = 2)
#'  legend("topright", c("observed", "simulated"),pch = c(1,NA),
#'         lty = c(NA,1), lwd = 2, bty = "n", cex = 1.3,col = c("darkgrey", "darkblue"))
#'  axis(1, at = pow, labels = parse(text = paste('10^',(pow), sep = "")), tcl = tcllen)
#'  axis(2, tcl = tcllen)
#'  axis(4, labels = NA)
#'  axis(3, labels = NA)
#'  mtext("pressure head |h| [cm]", 1, cex = 1.2, line = 2.8)
#'  mtext("vol. water content [ - ]", 2, cex = 1.2, line = 2.8)
#'  box()
#'  
#'  # PLOT THE MEASURED HYDRAULIC CONDUCTIVITY CURVE
#'  plot(condata, ylim = c(-8,2), xlim = c(0, 6.8), ylab = "", xlab = "", col = "darkgrey",
#'       axes = FALSE, main = "Hydraulic Conductivity Curve", cex = 2)
#'  lines(log10(h), hcc, col = "darkblue", lwd = 2)
#'  legend("topright", c("observed", "simulated"), pch = c(1,NA),
#'         lty = c(NA,1), lwd = 2, bty = "n", cex = 1.3, col = c("darkgrey","darkblue"))
#'  axis(1, at = pow, labels = parse(text = paste('10^',(pow), sep = "")), tcl = tcllen)
#'  axis(2)
#'  axis(4, labels = NA)
#'  axis(3, labels = NA)
#'  mtext("log10 K [cm/d]", 2, cex = 1.2, line = 2.8)
#'  mtext("pressure head |h| [cm]",1 , cex = 1.2, line = 2.8)
#'  box()
#'  par(mfrow = c(1,1))
#'  
#'  \dontrun{
#'  # HOW TO WRITE A MATER.IN FOR HYDRUS-1D
#'  
#'  mater_out <- cbind(shyp.L[['theta']], h, shyp.L[['Kh']], abs(shyp.L[['cap']]))
#'  
#'  materWriteFun <- function(mater_out.L, path = getwd(), sample) {
#'        
#'        # Function to write a Mater.in
#'        
#'        # ARGUMENTS
#'        
#'        # mater_outdata frame of 4 columns of calculated SHP values at h and length n. 
#'        # 1. Column: THETA
#'        # 2. Column: H(negative pressure heads)
#'        # 3. Column: K
#'        # 4. Column: C(positive)
#'        # path character specifying the path where the MATER.IN should be saved
#'        # sample optional chr for sample name: NULL = no name given
#'        
#'        n <- dim(mater_out)[1]
#'        sink(file.path(path, paste(sample, "MATER.IN", sep = "")))
#'        cat(c("iCap", "\n", "1", "\n", "NTab", "\n", n, "\n"))
#'        cat(c("\t","THETA", "\t\t\t","H","\t\t\t","K","\t\t\t","C"))
#'        cat("\n")
#'        
#'        write.table(format(mater_out, justify = "right"),
#'                    row.names = FALSE, col.names = FALSE, quote = FALSE)
#'        sink()
#'  }
#'  }
#' @export
#' 
#' 
shypFun <- function(p, h, shpmodel = "01110", ivap.query = NULL) {
      
      ### ASSIGN GENERAL 
      
      if(!is.na(grep("01110", shpmodel)[1]) == 1){
            
            # assign("shypFun", shypFun.01110)
            shypFun.int <- shypFun.01110
      }
      
      
      if(!is.na(grep("01210", shpmodel)[1]) == 1){
            
            shypFun.int <- shypFun.01210
      }
      
      if(!is.na(grep("01310", shpmodel)[1]) == 1){
            
            shypFun.int <-  shypFun.01310
      }
      
      
      if(!is.na(grep("02110", shpmodel)[1]) == 1){
            
            shypFun.int <-  shypFun.02110
      }
      
      if(!is.na(grep("03110", shpmodel)[1]) == 1){
            
            shypFun.int <-  shypFun.03110
      }
      
      #### Simple calculation of soil hydraulic property functions without 
      #### the Brunswick model.
      
      if(is.na(grep("FM", shpmodel)[1] == 1)){
            
            shypL <- shypFun.int(p, h) 
            kvap = KvapFun(p, por = p[2], retFun = NA, shypL$theta, model = ivap.query, pF = log10(h), output ="nolog10")
            
            return(list("theta" = shypL$theta,   
                        "Scap" = shypL$Se,
                        "cap" = shypL$cap, "psd" = shypL$psd,
                        "Kh" = shypL$Kh + kvap, "Kcap" = shypL$Kh, "Kvap" = kvap
            ))
            
            
            
            
            
      }
      
      #### Calculation if the soil hydraulic property functions are extended by the Brunswick model
      
      if(!is.na(grep("FM", shpmodel)[1] == 1)) {
            
            {
                  h.meas    <- abs(h)
                  h.support <- 10^seq(-3.11, 6.8, length = 302)
                  
                  h.calc  <- sort(c(h.support, h.meas))
                  
                  index.h <- match(h.meas, h.calc) 

                  thscnc  <- p[1] # [-]
                  thsc    <- p[2] # [-]
                  
                  # conductivity parameters
                  pcon  <- p[(length(p)-4):length(p)]
                  
                  Ksc   <- pcon[1]         # [cm d-1]
                  Ksnc  <- pcon[3]         # [cm d-1]
                  afilm <- pcon[4]         # Set to 1.5, as suggested by Tokunaga et al. (2009), WRR
                  
                  h0    <- abs(10^pcon[5]) # Set to 6.8 (pF = log10(10^6.8)) )
                  #
                  ### SATURATION FUNCTION
                  #
                  #
                  
                  ## CAPILLARY PART, based on equation (13) 
                  #
                  u <- shypFun.int(p, h.calc)$Se
                  u0 = u[length(h.calc)]
                  
                  scap = (u-u0)/(1-u0);
                  
                  ## NON CAPILLARY part, based on equations (4-7)
                  #
                  snc <- sncFun(h.calc, scap)
                  
                  #
                  ### SPECIFIC WATER CAPACITY FUNCTION (Not in the manuscript)
                  #
                  #
                  
                  # Capillary part (NUMERICALLY)
                  #
                  capcap <- thsc * (diff(scap)/diff(h.calc))
                  nh <- length(capcap)
                  capcap <- c(capcap, capcap[nh-1]/2)                               # vector length adjustment
                  
                  # Non-capillary part (NUMERICALLY)
                  #
                  capnc <- thscnc * (diff(snc)/diff(h.calc))
                  nh <- length(capnc)
                  capnc <- c(capnc, capnc[nh-1]/2) 
                  
                  # Summation of both parts
                  cap <- capcap + capnc
                  
                  # pore size density after Durner 1994, WRR (valid only of the capillary part)
                  poredis = log(10) * h.calc * capcap
                  
                  #
                  ### SOIL WATER RETENTION CURVE, based on equation 1
                  #
                  #
                  thetacap = thsc * scap
                  thetanc = thscnc * snc
                  theta = thetacap + thetanc
                  
                  #
                  ### HYDRAULIC CONDUCTIVITY CURVE
                  #
                  #
                  
                  ## Capillary conductivity, based on equations (10-11)
                  #
                  Kcap <- shypFun.int(p, h.calc)$Kh
                  ## non-capillary conductivity similar to Peters (2013)
                  #
                  krnc = h0^( -afilm * (1-snc));
                  
                  # total non capillary conductivity, second summand in equation (9)
                  Knc <- Ksnc * krnc;
                  
                  ## isothermal vapour conductivity acc. to Sposito (2006), with varying permeability models
                  # calculation of isothermal vapor conductivity based on model MQ61 (refer to paper, Table A-1)
                  # equations A-1 to A-4 and Table A-1 model "MQ61
                  kvap = KvapFun(p, por = thscnc + thsc, retFun = NA, theta, model = ivap.query, pF=log10(h.calc), output ="nolog10")
                  
                  ## total unsat. conductivity based on equation (8)
                  Kh <-  Kcap + Knc + kvap
                  
                  ### Return list of all variables
                  return(list("theta" = theta[index.h], "thetacap" = thetacap[index.h], "thetanc" = thetanc[index.h],  
                              "Scap" = scap[index.h], "Snc" = snc[index.h], 
                              "capcap"=capcap[index.h], "capnc" = capnc[index.h],
                              "cap" = cap[index.h], "psd" = poredis[index.h],
                              "Kh" = Kh[index.h], "Kcap" = Kcap[index.h], "Knc" = Knc[index.h], "Kvap" = kvap[index.h],
                              "krcap" = (Kcap/Ksc)[index.h], "krnc" = krnc[index.h]))
            }
      }
}




