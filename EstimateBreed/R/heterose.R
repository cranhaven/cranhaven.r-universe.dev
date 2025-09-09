#'Heterosis and Heterobeltiosis
#'@description
#'Calculation of heterosis and heterobeltiosis parameters of hybrids
#'@param GEN The column with the genotype name
#'@param GM The column with the average of the maternal parent
#'@param GP The column with the average of the paternal parent
#'@param PR The column with the average of the progeny
#'@param REP The column with the repetitions (if exists)
#'@param param Value to determine the parameter to be calculated. Default is 'all'.
#'To calculate heterosis only, use 'het'. To calculate only heterobeltiosis,
#'use 'hetb'.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@return Returns heterosis values based on the performance of the tested
#' parents and progenies. The standard error (SE) is also reported for each
#'  parameter.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@examples
#' library(EstimateBreed)
#'
#' data("maize")
#' #Extract heterosis and heterobeltiosis
#' general <- with(maize,het(GEN,GM,GP,PR,REP,param="all"))
#'
#' #Only extract heterosis
#' het <- with(maize,het(GEN,GM,GP,PR,REP,param = "het"))
#'
#' #Extract only heterobeltiosis
#' hetb <- with(maize,het(GEN,GM,GP,PR,REP,param = "hetb"))
#'@export

het <- function(GEN, GM, GP, PR, REP, param = "all",verbose=FALSE) {

  data <- data.frame(GEN, GM, GP, PR, REP)
  model1 <- aov(REP ~ GEN, data = data)
  MSe <- summary(model1)[[1]]["Residuals", "Mean Sq"]
  r <- length(unique(REP))
  data <- data %>%
    mutate(
      Heterosis = ((PR - ((GM + GP) / 2)) / ((GM + GP) / 2)) * 100,
      SE_Heterosis = sqrt((3 * MSe) / (2 * r))
    )
  Genitor <- pmax(GM, GP)
  data <- data %>%
    mutate(
      Heterobeltiosis = ((PR - Genitor) / Genitor) * 100,
      SE_Heterobeltiosis = sqrt((2 * MSe) / r)
    )

  if (param == "all") {
    return(data[, c("GEN", "Heterosis", "Heterobeltiosis")])
    if(verbose==TRUE){
      cat("Parameters\n")
      cat("SE_Heterosis:",paste(first(data$SE_Heterosis)),"\n")
      cat("SE_Heterobeltiosis:",paste(first(data$SE_Heterobeltiosis)),"\n")
      cat("-------------------------------------------\n")
      print(data[, c("GEN", "Heterosis", "Heterobeltiosis")])
    }
  } else if (param == "het") {
    return(data[, c("GEN", "Heterosis")])
    if(verbose==TRUE){
      cat("Parameters\n")
      cat("SE_Heterosis:",paste(first(data$SE_Heterosis)),"\n")
      cat("-------------------------------------------\n")
      print(data[, c("GEN", "Heterosis")])
    }
  } else if (param == "hetb") {
    return(data[, c("GEN", "Heterobeltiosis")])
    if(verbose==TRUE){
      cat("Parameters\n")
      cat("SE_Heterobeltiosis:",paste(first(data$SE_Heterobeltiosis)),"\n")
      cat("-------------------------------------------\n")
      print(first(data$SE_Heterobeltiosis))
      print(data[, c("GEN", "Heterobeltiosis")])
    }
  }
}
