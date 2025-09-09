#'Industrial quality of wheat
#'@description
#'Function for determining industrial quality indices of wheat genotypes,
#'described by Szareski et al. (2019).
#'@param GEN The column with the genotype name
#'@param NQ The column with the falling number
#'@param W The column with the gluten force (W)
#'@param PTN The column with the protein values
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@return Determines the industrial quality index for wheat crops, when
#'considering variables used to classify wheat cultivars.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Szareski, V. J., Carvalho, I. R., Kehl, K., Levien, A. M.,
#'Lautenchleger, F., Barbosa, M. H., ... & Aumonde, T. Z. (2019).
#'Genetic and phenotypic multi-character approach applied to multivariate
#'models for wheat industrial quality analysis.
#'Genetics and Molecular Research, 18(3), 1-14.
#'@examples
#'library(EstimateBreed)
#'
#'data("ptn")
#'with(ptn,is_qindustrial(Cult,NQ,W,PTN))
#'@export

is_qindustrial <- function(GEN, NQ, W, PTN, verbose=TRUE){

  genot <- as.factor(GEN)
  variav1 <- NQ
  variav2 <- W
  variav3 <- PTN

  sd_FN <- sd(variav1)
  sd_W <- sd(variav2)
  sd_PTN <- sd(variav3)
  desvios <- data.frame(sd_FN,sd_W,sd_PTN)

  final <- data.frame(sd_FN,sd_W,sd_PTN)
  indice <- ((variav1/sd_FN)*(variav2/sd_W)*(variav3/sd_PTN))

  dadosfinal <- data.frame(genot,indice)
  colnames(dadosfinal) <- c("GEN","Index")
  return(dadosfinal)
  if(verbose==TRUE){
    cat("\n-----------------------------\n")
    cat("Wheat Quality Index")
    cat("\n-----------------------------\n")
    print(dadosfinal)
    cat("\n-----------------------------\n")
    cat("Deviations")
    cat("\n-----------------------------\n")
    print(desvios)
  }
}

#'Peeling Index and Industrial Yield
#'@description
#'Calculating the Hulling Index and Industrial Yield of White Oats
#'@param GEN The column with the name of the genotypes.
#'@param NG2M The column with values for the number of grains larger than 2mm.
#'@param MG The column with grain mass values.
#'@param MC The column with karyopsis mass values.
#'@param RG The column with the grain yield values (kg per ha).
#'@param stat Logical argument. Use 'all' to keep all the observations or 'mean'
#'to extract the overall average.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@param ... General parameters of ggplot2 for utilization
#'@return Returns the peeling index and industrial yield considering the
#'standards desired by the industry.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@examples
#'library(EstimateBreed)
#'
#'data("aveia")
#'# Calculate the industrial yield without extracting the average
#'with(aveia, rend_ind(GEN,NG2M,MG,MC,RG))
#'
#'# Calculate the industrial yield by extracting the average per genotype
#'with(aveia, rend_ind(GEN,NG2M,MG,MC,RG,stat="mean"))
#'@export

rend_ind <- function(GEN,NG2M,MG,MC,RG,stat="all",verbose=FALSE,...){

  GEN <- as.factor(GEN)
  NG2M <- NG2M
  MG <- MG
  MC <- MC
  RG <- RG
  if(stat=="all"){
    ID <- MC/MG
    RI <- RG*(NG2M/100)*ID
    final <- data.frame(GEN,ID,RI)
    return(final)
    if(verbose==TRUE){
      cat("\n-----------------------------------------------------------------\n")
      cat("Peeling Index and Industrial Yield")
      cat("\n-----------------------------------------------------------------\n")
      print(final)
    }
  }else if (stat=="mean"){
    ID <- MC/MG
    RI <- RG*(NG2M/100)*ID
    dados <- data.frame(GEN,ID,RI)
    media_gen <- aggregate(cbind(ID, RI) ~ GEN,
                           data = dados,
                           FUN = mean,
                           na.rm = TRUE)
    return(media_gen)
    if(verbose==TRUE){
      cat("\n-----------------------------------------------------------------\n")
      cat("Peeling Index and Industrial Yield (Mean by Genotype)")
      cat("\n-----------------------------------------------------------------\n")
      print(media_gen)
    }
  }
}

#'Ear Indexes
#'@description
#'Estimating the viability index from the combination of two field variables.
#'@param GEN The column with the name of the genotypes
#'@param var1 The column containing the first variable
#'@param var2 The column containing the second variable
#'@param ylab The name of the chart's Y axis
#'@param xlab The name of the chart's X axis
#'@param stat Logical argument. Use 'all' to return the values obtained for all
#' observations or 'mean' to return the mean per genotype.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@param plot Logical argument. Plot a graphic if 'TRUE'.
#'@return Returns the index obtained between the reported variables. The higher
#'the index, the better the genotype.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Rigotti, E. J., Carvalho, I. R., Loro, M. V., Pradebon, L. C., Dalla Roza,
#'J. P., & Sangiovo, J. P. (2024). Seed and grain yield and quality of wheat
#'subjected to advanced harvest using a physiological ripening process.
#'Revista Engenharia na Agricultura - REVENG, 32, 54-64.
#'\doi{10.13083/reveng.v32i1.17394}
#'@examples
#'library(EstimateBreed)
#'
#'data("trigo")
#'#Ear viability index
#'index1 <- with(trigo,indviab(TEST,NGE,NEE))
#'
#'#Ear harvest index
#'index2 <- with(trigo,indviab(TEST,MGE,ME))
#'
#'#Spikelet deposition index in the ear
#'index3 <- with(trigo,indviab(TEST,NEE,CE))
#'@export

indviab <- function(GEN,var1,var2,ylab="Index",xlab="Genotype",stat="all",
                    verbose=FALSE,plot=FALSE){

  GEN <- as.factor(GEN)
  variav1 <- var1
  variav2 <- var2

  if(stat=="all"){
    indesp <- variav1/variav2
    mediaind <- mean(indesp)
    dados <- data.frame(GEN,indesp)
    colnames(dados) <- c("Genotype","Index")
    return(dados)
    if(verbose==TRUE){
      cat("\n-----------------------------------------------------------------\n")
      cat("General Viability Index")
      cat("\n-----------------------------------------------------------------\n")
      print(dados)
    }
    if(plot==TRUE){
      stop("The graph can only be plotted when stat='mean'",call. = FALSE)
    }
  }
  else if (stat=="mean"){
    indesp <- variav1/variav2
    dados <- data.frame(GEN,indesp)
    media_gen <- aggregate(indesp ~ GEN,
                           data = dados,
                           FUN = mean,
                           na.rm = TRUE)
    colnames(media_gen) <- c("Genotype","Index")
    return(media_gen)
    if(plot==TRUE){
      grafico <- ggplot(media_gen, aes(x=GEN, y=indesp)) +
        geom_bar(stat = "identity")+
        ylab(ylab)+xlab(xlab)+theme_classic()
      print(grafico)
    }
    if(verbose==TRUE){
      cat("\n-----------------------------------------------------------------\n")
      cat("Viability Index (Average per Genotype))")
      cat("\n-----------------------------------------------------------------\n")
      print(media_gen)
    }
  }}

#'Hectolitre weight of cereals
#'@description
#'Useful function for characterizing the hectolitre weight (HW) of experiments
#' with cereals.
#'@param GEN The column with the genotype name
#'@param HL Weight obtained on a 1qt lt scale, as determined by the
#'Rules for Seed Analysis (RAS), Ministry of Agriculture,
#'Livestock and Supply (2009).
#'@param crop Argument for selecting culture. Use 'trit' for wheat, 'oat' for
#'white oats, 'rye' for rye and 'barley' for barley
#'@param stat  Argument to select the function output type. Use 'all' to estimate
#' the HW for all replicates, or 'mean' to extract the mean for each genotype.
#'@return Returns the estimated value for the hectoliter weight considering the
#' selected cereal.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Brasil. Ministerio da Agricultura, Pecuaria e Abastecimento.
#'Secretaria de Defesa Agropecuaria. Regras para Analise de Sementes.
#' Brasilia: MAPA/ACS, 2009. 399 p. ISBN 978-85-99851-70-8.
#'@examples
#'library(EstimateBreed)
#'
#'GEN <- rep(paste("G", 1:5, sep=""), each = 3)
#'REP <- rep(1:3, times = 5)
#'MG <- c(78.5, 80.2, 79.1, 81.3, 82.0, 80.8, 76.9, 78.1, 77.5, 83.2,
#'84.1, 82.9, 77.4, 78.9, 79.3)
#'
#'data <- data.frame(GEN, REP, MG)
#'
#'trit <- with(data,hw(GEN,MG,crop="trit"))
#'
#'#Extract the average PH per genotype
#'trit <- with(data,hw(GEN,MG,crop="trit",stat="mean"))
#'@export

hw <- function(GEN, HL, crop="trit", stat="all") {

  dados <- data.frame(GEN, HL)

  if(crop == "trit") {
    dados <- dados %>%
      mutate(HW = -9.935757 + (HL * 0.451821))
  } else if (crop == "oat") {
    dados <- dados %>%
      mutate(HW = -3.512294 + (HL * 0.425507))
  } else if (crop == "rye") {
    dados <- dados %>%
      mutate(HW = -5.8241877 + HL * 0.4319325)
  } else if (crop == "barley") {
    dados <- dados %>%
      mutate(HW = -8.14433 + HL * 0.44523)
  }

  if(stat == "mean") {
    media <- aggregate(HW ~ GEN,
                       data = dados,
                       FUN = mean,
                       na.rm = TRUE)
    return(media)
  } else {
    dados <- dados %>%
      mutate(HW = format(HW, nsmall = 2))
    return(dados)
  }
}

#'Selection index for protein and grain yield
#'@description
#'Selection index for protein and grain yield (Pelegrin et al., 2017).
#'@param GEN The column with the name of the genotype
#'@param PTN The column with the crude protein values
#'@param RG The column with the grain yield values (in kg per ha)
#'@return Returns an industrial wheat quality index based solely on protein and
#'grain yield.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'de Pelegrin, A. J., Carvalho, I. R., Nunes, A. C. P., Demari, G. H., Szareski,
#'V. J., Barbosa, M. H., ... & da Maia, L. C. (2017).
#'Adaptability, stability and multivariate selection by mixed models.
#'American Journal of Plant Sciences, 8(13), 3324.
#'@examples
#'library(EstimateBreed)
#'
#'Gen <- c("G1", "G2", "G3", "G4", "G5")
#'PTN <- c(12.5, 14.2, 13.0, 11.8, 15.1)
#'RG <- c(3500, 4000, 3700, 3300, 4100)
#'
#'data <- data.frame(Gen,PTN,RG)
#'
#'iqptn <- with(data,is_ptnerg(Gen,PTN,RG))
#'@export

is_ptnerg <- function(GEN, PTN, RG, verbose=TRUE){

  genot <- as.factor(GEN)
  variav1 <- PTN
  variav2 <- RG

  sd_ptn <- sd(variav1)
  sd_rg <- sd(variav2)

  final <- data.frame(sd_ptn,sd_rg)
  index <- ((variav1/sd_ptn)*(variav2/sd_rg))

  dadosfinal <- data.frame(genot,index)
  return(dadosfinal)
  if(verbose==TRUE){
    cat("\n-----------------------------------------------------------------\n")
    cat("Genotype")
    cat("\n-----------------------------------------------------------------\n")
    print(dadosfinal)
  }
}
