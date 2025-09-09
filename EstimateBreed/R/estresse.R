#'Stress indices for genotype selection
#'@description
#'Selection indices for genotypes conducted under stress conditions cited
#'by Ghazvini et al. (2024).
#'@param GEN The column with the genotypes to be selected.
#'@param YS Productivity of the genotype without stress conditions.
#'@param YC Genotype productivity under stressful conditions.
#'@param index Index to be calculated (Standard 'ALL'). The indices to be used
#'are: 'STI' - Stress Tolerance Index, 'YI' - Yield Index, 'GMP' - Geometric Mean
#' Productivity, 'MP' - Mean Productivity, 'MH' - Harmonic Mean, 'SSI' - Stress
#' Stability Index, 'YSI' - Yield Stability Index, 'RSI' - Relative Stress Index.
#'@param bygen Returns the average of each genotype if 'TRUE'. Only in this way
#'it will be possible to plot graphs.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@param plot Plot graph if equal to 'TRUE' (Standard 'FALSE').
#'@param xlab Adjust the title of the x-axis in the graph.
#'@param ylab Adjust the title of the y-axis in the graph.
#'@param ... General ggplot2 parameters for graph customization.
#'@return Returns a table with the genotypes and the selected indices.
#'The higher the index value, the more resilient the genotype.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Ghazvini, H., Pour-Aboughadareh, A., Jasemi, S.S. et al.
#'A Framework for Selection of High-Yielding and Drought-tolerant
#'Genotypes of Barley: Applying Yield-Based Indices and Multi-index Selecion
#'Models. Journal of Crop Health 76, 601-616 (2024).
#'\doi{10.1007/s10343-024-00981-1}
#'@examples
#'library(EstimateBreed)
#'
#'data("aveia")
#'
#'#General
#'index <- with(aveia,stind(GEN,MC,MG,index = "ALL",bygen=TRUE))
#'
#'#Only the desired index
#'STI <- with(aveia,stind(GEN,MC,MG,index = "STI",bygen=TRUE))
#'@export

stind <- function(GEN,YS,YC,index="ALL",bygen=TRUE,verbose=FALSE,plot=FALSE,
                  xlab="Genotype",ylab="Values",...){
  GEN1 <- as.factor(GEN)
  YS1 <- YS
  YC1 <- YC
  verif <- data.frame(YS1,YC1)
  if (any(is.na(verif))) {
    warning("The data frame contains NA values!")
  }

  if(bygen==TRUE){
    media <- data.frame(GEN1,YS1,YC1)
    media_gen <- aggregate(cbind(YS1, YC1) ~ GEN1,
                           data = media,
                           FUN = mean,
                           na.rm = TRUE)
    GEN <- media_gen$GEN1;YS <- media_gen$YS;YC <- media_gen$YC
    if(index=="STI"){
      STI <- ((YS*YC)/(YC^2))
      STI <- data.frame(GEN,STI)
      return(STI)
      if(verbose==TRUE){
        cat("STI Index")
        cat("\n----------------------------\n")
        print(STI)
      }
      if(plot==TRUE){
        ggplot(STI, aes(x = GEN, y = STI, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "STI Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="YI"){
      YI <- YS/YC
      YI <- data.frame(GEN,YI)
      return(YI)
      if(verbose==TRUE){
        cat("YI Index")
        cat("\n----------------------------\n")
        print(YI)
      }
      if(plot==TRUE){
        ggplot(YI, aes(x = GEN, y = YI, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "YI Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="GMP"){
      GMP <- sqrt(YS*YC)
      GMP <- data.frame(GEN,GMP)
      return(GMP)
      if(verbose==TRUE){
        cat("GMP Index")
        cat("\n----------------------------\n")
        print(GMP)
      }
      if(plot==TRUE){
        ggplot(GMP, aes(x = GEN, y = GMP, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "GMP Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="MP"){
      MP <- (YS*YC)/2
      MP <- data.frame(GEN,MP)
      return(MP)
      if(verbose==TRUE){
        cat("MP Index")
        cat("\n----------------------------\n")
        print(MP)
      }
      if(plot==TRUE){
        ggplot(MP, aes(x = GEN, y = MP, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "MP Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="MH"){
      MH <- (2*(YC-YS))/(YC+YS)
      MH <- data.frame(GEN,MH)
      return(MH)
      if(verbose==TRUE){
        cat("MH Index")
        cat("\n----------------------------\n")
        print(MH)
      }
      if(plot==TRUE){
        ggplot(MH, aes(x = GEN, y = MH, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "MH Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="SSI"){
      xYC <- mean(media_gen$YC)
      xYS <- mean(media_gen$YS)
      SSI <- (1-(YC/YS))/(1-(xYC/xYS))
      SSI <- data.frame(GEN,SSI)
      return(SSI)
      if(verbose==TRUE){
        cat("SSI Index")
        cat("\n----------------------------\n")
        print(SSI)
      }
      if(plot==TRUE){
        ggplot(SSI, aes(x = GEN, y = SSI, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "SSI Index", x = xlab, y = ylab) +coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="YSI"){
      YSI <- YS/YC
      YSI <- data.frame(GEN,YSI)
      return(YSI)
      if(verbose==TRUE){
        cat("YSI Index")
        cat("\n----------------------------\n")
        print(YSI)
      }
      if(plot==TRUE){
        ggplot(YSI, aes(x = GEN, y = YSI, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "YSI Index", x = xlab, y = ylab)+coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="RSI"){
      xYC <- mean(media_gen$YC)
      xYS <- mean(media_gen$YS)
      RSI <- (YC/YS)/(xYC/xYS)
      RSI <- data.frame(GEN,RSI)
      return(RSI)
      if(verbose==TRUE){
        cat("RSI Index")
        cat("\n----------------------------\n")
        print(RSI)
      }
      if(plot==TRUE){
        ggplot(RSI, aes(x = GEN, y = RSI, fill = GEN)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "RSI Index", x = xlab, y = ylab)+coord_flip()+
          scale_fill_viridis_d()
      }
    }
    else if(index=="ALL"){
      STI <- ((YS*YC)/(YC^2))
      YI <- YS/YC
      GMP <- sqrt(YS*YC)
      MP <- (YS*YC)/2
      MH <- (2*(YC-YS))/(YC+YS)
      xYC <- mean(media_gen$YC)
      xYS <- mean(media_gen$YS)
      SSI <- (1-(YC/YS))/(1-(xYC/xYS))
      YSI <- YS/YC
      RSI <- (YC/YS)/(xYC/xYS)
      final <- data.frame(GEN,STI,YI,GMP,MP,MH,SSI,YSI,RSI)
      return(final)
      if(verbose==TRUE){
        cat("\n---------------------------------------------------------------------------------------------\n")
        cat("Stress Index")
        cat("\n---------------------------------------------------------------------------------------------\n")
        print(final)
      }
      if(plot==TRUE){
        final <- final %>%
          mutate(across(starts_with("GEN"), as.factor),
                 across(c(STI, YI, GMP, MP, MH, SSI, YSI, RSI), as.numeric))

        dados_long <- pivot_longer(final, cols = c(STI, YI, GMP, MP, MH, SSI, YSI, RSI),
                                   names_to = "indice", values_to = "valores")

        ggplot(dados_long, aes(x = GEN, y = valores, fill = GEN)) +
          geom_bar(stat = "identity") +
          facet_wrap(~ indice, ncol = 4) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none") +
          labs(title = "Index Values by Genotype", x = xlab, y = ylab) +
          scale_fill_viridis_d()
      }
    }
  }
}

#'Environmental Stress Index
#'@description
#'Determining the UTI (temperature and humidity index) from the air temperature
#'and relative humidity values over a given period of time
#'@param AAT The column with the average air temperature values
#'@param RH The column with the relative humidity values
#'@return Returns the stress condition based on the reported air temperature and
#' relative humidity values, being: Non-stressful condition (ITU>=70), Heat
#' stress condition (ITU between 71 and 78), Severe heat stress (ITU between 79
#' and 83), and Critical heat stress condition (ITU above 84).
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Tazzo, I. F., Tarouco, A. K., Allem Junior P. H. C., Bremm, C., Cardoso, L.
#'S., & Junges, A. H. (2024). Indice de Temperatura e Umidade (ITU) ao longo do
#'verao de 2021/2022 e estimativas dos impactos na bovinocultura de leite no Rio
#'Grande do Sul, Brasil. Ciencia Animal Brasileira, 2,5, e-77035P.
#'@export

itu <- function(AAT,RH){

  Tpo <- ((RH/100)^(1/8))*(112+(0.9*AAT))+(0.1*AAT)-112
  ITU <- AAT+((0.36*Tpo)+41.5)
  if(ITU>=70){
    print(ITU)
    cat("\n-----------------------------------------------------------------\n")
    cat("Non-stressful condition, range within thermal comfort")
    cat("\n-----------------------------------------------------------------\n")
  }
  else if (ITU>=71&ITU<=78){
    print(ITU)
    cat("\n-----------------------------------------------------------------\n")
    cat("Heat stress condition")
    cat("\n-----------------------------------------------------------------\n")
  }
  else if (ITU>=79&ITU<=83){
    print(ITU)
    cat("\n-----------------------------------------------------------------\n")
    cat("Severe heat stress (danger situation)")
    cat("\n-----------------------------------------------------------------\n")
  }
  else if (ITU>=84){
    print(ITU)
    cat("\n-----------------------------------------------------------------\n")
    cat("Critical heat stress condition (emergency situation)")
    cat("\n-----------------------------------------------------------------\n")
  }
}
