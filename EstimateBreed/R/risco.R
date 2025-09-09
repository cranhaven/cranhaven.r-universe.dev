#'Risk of Disease Occurrence in Soybeans
#'@description
#'Calculation of the Risk of Disease Occurrence in Soybeans as a Function of
#'Variables meteorological variables (Engers et al., 2024).
#'@param DAY The column for the day of the month.
#'@param MONTH The column for the month of the year (numeric value).
#'@param AAT The average air temperature column (in degree Celsius).
#'@param RH The relative humidity column (in \%).
#'@param disease Define the soybean disease (Standard = 'rust').
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@param plot Plot a graph of the accumulation (Default is F (FALSE)).
#'@return Returns the parameters of the incidence probability of the selected
#'disease in the soybean crop, being: \cr
#' \cr
#' * RHrisk\cr
#'   Risk caused by relative humidity.\cr
#' \cr
#' * TEMPrisk\cr
#'   Risk caused by air temperature.\cr
#' \cr
#' * TOTALrisk\cr
#'   Product of the multiplication between RHrisk and TEMPrisk.\cr
#' \cr
#' * RELrisk\cr
#'   Relative risk obtained from the highest value of TOTALrisk.
#'@references
#'de Oliveira Engers, L.B., Radons, S.Z., Henck, A.U. et al.
#'Evaluation of a forecasting system to facilitate decision-making for the
#'chemical control of Asina soybean rust. Trop. plant pathol. 49, 539-546 (2024).
#'\doi{10.1007/s40858-024-00649-1}
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@examples
#'library(EstimateBreed)
#'
#'# Rust Risk Prediction
#'data("clima")
#'with(clima, risk(DY, MO, TMED, RH, disease = "rust"))
#'@export

risk <- function(DAY,MONTH,AAT,RH,disease="rust",verbose=FALSE,plot=FALSE){
  dados <- data.frame(DAY,MONTH,AAT,RH)
  if(disease=="rust"){
  alfa <- log(2)/log(2.30508474576)
  umidade <- aggregate(RH~DAY+MONTH,data=dados,FUN=function(x)sum(x>85))
  colnames(umidade) <- c("Day","Month","W")
  umidade <- as.data.frame(umidade)
  umidade$RHrisk <- 1/1+(2.72^(umidade$W-12))
  umidade$RHrisk <- ifelse(umidade$RHrisk > 100, 100, umidade$RHrisk)
  mediaUR <- aggregate(RHrisk~Month,data=umidade,FUN=mean)

  Temp_f <- subset(dados,RH>85)
  Temp <- aggregate(AAT~DAY+MONTH,data=Temp_f,FUN=mean)
  colnames(Temp) <- c("Day","Month","TMed")
  Temp <- as.data.frame(Temp)
  Temp$TEMPrisk <- (2*(Temp$TMed-8)*alfa*(22.75-8)*alfa-(Temp$TMed-8)*2*alfa)/(22.75-8)*2*alfa
  mediaTemp <- aggregate(TEMPrisk~Month,data=Temp,FUN=mean)
  riscofinal <- merge(mediaUR,mediaTemp)
  riscofinal <- as.data.frame(riscofinal)
  riscofinal$TOTALrisk <- riscofinal$RHrisk * riscofinal$TEMPrisk
  max_val <- max(riscofinal$TOTALrisk)
  min_val <- min(riscofinal$TOTALrisk)
  riscofinal$RELrisk <- (riscofinal$TOTALrisk-min_val)/(max_val-min_val)*100
  diarioGeral <- merge(umidade,Temp)
  diarioGeral$TOTALrisk <- diarioGeral$RHrisk * diarioGeral$TEMPrisk
  diarioGeral$TOTALrisk <- ifelse(diarioGeral$TOTALrisk > 100, 100, diarioGeral$TOTALrisk)
  if(verbose==TRUE){
    cat("\n--------------------------------------------------\n")
    cat("Risk of Asian Rust Occurrence")
    cat("\n--------------------------------------------------\n")
    print(riscofinal)
  }
  return(riscofinal)
    if(plot==TRUE){
      layout(matrix(c(1,3,2,3), 2, 2, byrow = TRUE))
      boxplot(Temp$riscoTEMP ~ Temp$Month, col=rgb(0.3,0.5,0.4,0.6),
              ylab="Temperature Risk (%)",xlab="Month",outline=FALSE)
      boxplot(umidade$riscoUR ~ umidade$Mponth, col=rgb(0.3,0.5,0.4,0.6),
              ylab="Relative Humidity Risk (%)",xlab="Month",outline=FALSE)
      boxplot(diarioGeral$TOTALrisk ~ diarioGeral$Month, col=rgb(0.3,0.5,0.4,0.6),
              ylab="Total Risk (%)",xlab="Month",outline=FALSE)
    }
  }#ELSEIF - ###ENVARG
  }
