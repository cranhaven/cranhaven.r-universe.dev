#' General Selection Gain Function
#' @description
#' Computes selection gain using different selection methods
#' @param Var The column with the name of the variables of interest
#' @param h The column with the restricted heritability values
#' @param VF The column with the phenotypic variance values (optional)
#' @param P The column with the progeny values or selection pressure (optional)
#' @param DS The column with the selection differential values (optional)
#' @param Year The column with the year of selection (optional)
#' @param method The selection method: 'pressure', 'differential',
#' 'genitor_control", or 'year_weighted'
#' @param verbose Logical argument. Runs the code silently if FALSE.
#' @return A data frame with selection gain results
#' @author Willyan Junior Adorian Bandeira
#' @author Ivan Ricardo Carvalho
#' @author Murilo Vieira Loro
#' @author Leonardo Cesar Pradebon
#' @author Jose Antonio Gonzalez da Silva
#' @examples
#'library(EstimateBreed)
#'
#'SG(Var = c("A", "B", "C"), h = 0.5, VF = 1.2, P = "10", method = "pressure")
#'SG(Var = c("A", "B", "C"), h = 0.5, DS = 1.5, method = "differential")
#'SG(Var = c("A", "B", "C"), h = 0.5, VF = 1.2, P = "10", method = "genitor_control")
#'SG(Var = c("A", "B", "C"), h = 0.5, VF = 1.2, P = "10", Year = 5, method = "year_weighted")
#' @export

SG <- function(Var, h, VF = NULL, P = "1", DS = NULL, Year = NULL, method = "pressure",
               verbose=FALSE) {
  Var <- as.factor(Var)

  coeficientes <- c("1" = 2.7, "2" = 2.44, "3" = 2.27, "4" = 2.16, "5" = 2.08,
                    "10" = 1.76, "20" = 1.4, "40" = 0.97, "50" = 0.8, "60" = 0.64,
                    "70" = 0.5, "80" = 0.35, "90" = 0.2)

  if (!is.null(P) && !(P %in% names(coeficientes))) {
    stop("Invalid P value. Choose between: ", paste(names(coeficientes),
                                                    collapse = ", "))
  }

  GS <- switch(method,
               "pressure" = {
                 if (is.null(VF)) stop("VF is required for the 'pressure'
                                       method.")
                 coef <- coeficientes[P]
                 GS <- h * coef * sqrt(VF)
                 desc <- paste("Selection Gain with", P, "% pressure")
                 list(GS = GS, desc = desc, coef = coef)
               },
               "differential" = {
                 if (is.null(DS)) stop("DS is required for the 'differential'
                                       method.")
                 GS <- h * DS
                 desc <- "Selection Gain using Selection Differential"
                 list(GS = GS, desc = desc, coef = NA)
               },
               "genitor_control" = {
                 if (is.null(VF)) stop("VF is required for the 'genitor_control'
                                       method.")
                 coef <- coeficientes[P]
                 GS <- h * coef * 0.5 * sqrt(VF)
                 desc <- paste("Selection Gain with", P, "% pressure and Genitor
                               Control")
                 list(GS = GS, desc = desc, coef = coef)
               },
               "year_weighted" = {
                 if (is.null(VF) || is.null(Year)) stop("VF and Year are
                                                        required for the
                                                        'year_weighted' method.")
                 coef <- coeficientes[P]
                 GS <- (h * coef * sqrt(VF)) / Year
                 desc <- paste("Selection Gain with", P, "% pressure weighted
                               by Year")
                 list(GS = GS, desc = desc, coef = coef)
               },
               stop("Invalid method. Choose between 'pressure', 'differential',
                    'genitor_control' or 'year_weighted'.")
  )

  final_list <- list(
    Variable = Var,
    SG = GS$GS,
    H2 = h,
    PV = if (!is.null(VF)) VF else NULL,
    SP = if (method %in% c("pressure", "genitor_control", "year_weighted"))
      P else NULL,
    SD = if (method == "differential") DS else NULL,
    Year = if (method == "year_weighted") Year else NULL
  )

  final_list <- final_list[!sapply(final_list, is.null)]
  final <- suppressWarnings(data.frame(final_list, stringsAsFactors = FALSE,
                                       row.names = NULL))
  if(verbose==TRUE){
    cat("\n", strrep("-", 40), "\n", sep = "")
    cat(paste("Selection Gain using", method, "\n"))
    cat(strrep("-", 40), "\n", sep = "")
  }
  return(final)
}

####
#'Selection Differential (Mean and Deviations)
#'@description
#'Selection of Transgressive Genotypes - Selection Differential (SD)
#'@param Gen The column with the genotype name
#'@param Var The column with the values for the variable of interest
#'@param Control The column with the value of the variable 'X' for the controls
#'@param ylab The name of the Y axis.
#'@param xlab The name of the X axis.
#'@param plot Logical argument. Plots a graphic if 'TRUE'.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@return Returns the general parameters and the genotypes selected for each
#'treshold. Also plot a representative graph of the selected genotypes based on
#'the mean and standard deviations.
#'@examples
#'library(EstimateBreed)
#'
#'Gen <- paste0("G", 1:20)
#'Var <- round(rnorm(20, mean = 3.5, sd = 0.8), 2)
#'Control <- rep(3.8, 20)
#'
#'data <- data.frame(Gen,Var,Control)
#'
#'transg_sel <- with(data,transg(Gen,Var,Control,verbose=FALSE,plot=TRUE))
#'@export

transg <- function(Gen, Var, Control, verbose=FALSE, plot=FALSE,
                   ylab="Selection", xlab="Genotypes") {

  Gen <- as.factor(Gen)
  Var <- Var
  Testemunha <- Control
  Media <- mean(Var)
  DSg <- mean(Testemunha)
  Desvio <- sd(Var)
  DS1S <- Media + Desvio
  DS2S <- Media + (2 * Desvio)
  DS3S <- Media + (3 * Desvio)
  parametros <- list(
    "Overall Mean" = round(Media, 3),
    "Control Mean" = round(DSg, 3),
    "Standard Deviation" = round(Desvio, 3),
    "Mean + 1SD" = round(DS1S, 3),
    "Mean + 2SD" = round(DS2S, 3),
    "Mean + 3SD" = round(DS3S, 3)
  )
  dados <- data.frame(Gen, Var, Testemunha)
  above_mean <- as.character(dados$Gen[dados$Var>Media])
  above_cmean <- as.character(dados$Gen[dados$Var>DSg])
  above_DS1S <- as.character(dados$Gen[dados$Var>DS1S])
  above_DS2S <- as.character(dados$Gen[dados$Var>DS2S])
  above_DS3S <- as.character(dados$Gen[dados$Var>DS3S])

  x_min <- 1
  x_max <- length(levels(Gen))
  if(plot==TRUE){
    grafico <- ggplot(dados, aes(x = Gen, y = Var)) +
      geom_text(label = rownames(dados), nudge_x = 0, nudge_y = 0, color = "red",
                hjust = 3, size = 4) +
      ylab(ylab) + xlab(xlab) + theme_classic() +
      geom_segment(x = x_min, xend = x_max, y = Media, yend = Media, linetype = 1,
                   color = "darkred") +
      annotate("text", x = x_min, y = Media, label = "Overall Mean",
               color = "darkred",hjust = 0) +
      geom_segment(x = x_min, xend = x_max, y = DSg, yend = DSg, linetype = 1,
                   color = "purple") +
      annotate("text", x = x_min, y = DSg, label = "Control Mean",
               color = "purple",hjust = 0) +
      geom_segment(x = x_min, xend = x_max, y = DS1S, yend = DS1S, linetype = 3,
                   color = "blue") +
      annotate("text", x = x_min, y = DS1S, label = "DS1S", color = "blue",
               hjust = 0) +
      geom_segment(x = x_min, xend = x_max, y = DS2S, yend = DS2S, linetype = 4,
                   color = "darkgreen") +
      annotate("text", x = x_min, y = DS2S, label = "DS2S", color = "darkgreen",
               hjust = 0) +
      geom_segment(x = x_min, xend = x_max, y = DS3S, yend = DS3S, linetype = 5,
                   color = "darkorange") +
      annotate("text", x = x_min, y = DS3S, label = "DS3S", color = "darkorange",
               hjust = 0) +
      ggtitle("Selection of Transgressive Genotypes - Selection Differential (SD)")
    print(grafico)
  }
  if(verbose==TRUE){
    cat("\n---------------------------------------------------------------------\n")
    cat("Selection of Transgressive Genotypes - Selection Differential (SD)\n")
    cat("---------------------------------------------------------------------\n")
    cat("Parameters:\n")
    cat("---------------------------------------------------------------------\n")
    for (param in names(parametros)) {
      cat(sprintf("%-20s : %.3f\n", param, parametros[[param]]))
    }
    cat("\n---------------------------------------------------------------------\n")
    cat("Genotypes above each threshold:\n")
    cat("---------------------------------------------------------------------\n")

    print_wrapped_list <- function(label, values, width = 60) {
      if (length(values) == 0) {
        cat(sprintf("%-30s : None\n", label))
      } else {
        text <- paste(values, collapse = ", ")
        wrapped_text <- strwrap(text, width = width, prefix = "  ",
                                initial = paste(label, ": "))
        cat(paste0(wrapped_text, collapse = "\n"), "\n")
      }
    }
    print_wrapped_list("Genotypes above Control Mean", above_cmean)
    print_wrapped_list("Genotype above Overall Mean", above_mean)
    print_wrapped_list("Genotypes above Mean + 1SD", above_DS1S)
    print_wrapped_list("Genotypes above Mean + 2SD", above_DS2S)
    print_wrapped_list("Genotypes above Mean + 3SD", above_DS3S)
    cat("---------------------------------------------------------------------\n")
  }
}

###
#'Standard Segregation
#'@description
#'Didactic table of standard segregation by generation
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@return Create a didactic table of standard segregation, considering allogamous
#' and autogamous species and mutants. It shows the expected level of
#' heterozygosity, probable number of genes, environmental effect and Wright's
#' probabilistic coefficient.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@examples
#'library(EstimateBreed)
#'
#'default_seg(verbose=TRUE)
#'@export

default_seg <- function(verbose=TRUE){
  if(verbose==TRUE){
    AUTOGAM<-c("Parents","F1","F2","F3","F4","F5","F6","F7","F8","F9")
    HETEROZIGOSITY<-c(0,100,50,25,12.5,6.25,3.12,1.56,0.78,0.39)
    ALOGAM<-c("-","-","S0","S1","S2","S3","S4","S5","S6","S7")
    HOMOZIGOSITY<-c(100, 0,50,75,87.5,93.75,96.88,98.44,99.22, 99.61)
    MUTANTS<-c("-", "M0", "M1","M2", "M3","M4","M5","M6", "M7", "M8")
    SELECTION<-c("N","N","Quali","Quali",
                 "Quant","Quanti","Quanti","Quanti",
                 "Quanti","Quanti")
    N_GENES<-c("-","-","1 to 2",
               "1 to 2", "3 or more", "3 or more","3 or more","3 or more",
               "3 or more","3 or more")
    ENV_EF<-c("-","-","low",
              "low", "high", "high","high","high","high","high")
    COEF_F<-c(0.000,0.500,0.750,0.875,0.937,0.969,0.984,0.992,0.996,0.998)
    TABLESEG<-data.frame(AUTOGAM,ALOGAM,HETEROZIGOSITY,HOMOZIGOSITY,MUTANTS,
                         SELECTION,N_GENES, ENV_EF, COEF_F)
    return(TABLESEG)
  }
}

###
#'Inbreeding coefficient
#'@description
#'Function for calculating the inbreeding coefficient
#'@param var Column with the variable name
#'@param VG Column with genotypic variance
#'@param VF Column with phenotypic variance
#'@param generation Parameter to select the generation. Use 'all' to get the
#' parameters for all the generations or 'F3', 'F4', 'F5' and 'F6' for just
#'  one of the generations.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Falconer, D. S., & Mackay, T. F. C. (1996). Introduction to quantitative
#'genetics (4th ed.). Longman.
#'@return Returns the total, additive and dominance variance values based on
#'the variance components for a given variable.
#'@examples
#'library(EstimateBreed)
#'
#'var <- c("A","B","C","D","E")
#'VF <- c(2.5, 3.0, 2.8, 3.2, 2.7)
#'VG <- c(1.2, 1.5, 1.3, 1.6, 1.4)
#'data <- data.frame(var,VG,VF)
#'
#'#Calculating for all generations
#'inbr1 <- with(data,COI(var,VG,VF,generation = "all"))
#'
#'#Calculating for just one generation
#'inbr2 <- with(data,COI(var,VG,VF,generation = "F3"))
#'@export

COI <- function(var, VG, VF, generation = "all", verbose=FALSE) {
  var <- as.factor(var)
  factors <- list(
    F3 = c(1.5, 0.75, 1, 0.25, 0.5, 0.5),
    F4 = c(0.875, 0.438, 1.5, 0.188, 0.25, 0.25),
    F5 = c(1.875, 0.234, 1.75, 0.109, 0.125, 0.125),
    F6 = c(1.938, 0.121, 1.875, 0.059, 0.063, 0.063)
  )

  if (generation != "all") {
    if (!(generation %in% names(factors))) {
      stop("Error: Invalid generation. Choose between 'F3', 'F4', 'F5',
           'F6' or 'all'.")
    }
    factors <- factors[generation]
  }
  variance_list <- list()
  heritability_list <- list()

  for (gen in names(factors)) {
    f <- factors[[gen]]
    VA_Total <- VG / f[1]
    VD_Total <- VG / f[2]
    VA_Between <- VG / f[3]
    VD_Between <- VG / f[4]
    VA_Within <- VG / f[5]
    VD_Within <- VG / f[6]
    variances <- data.frame(
      Generation = gen, var, VA_Total, VD_Total, VA_Between, VD_Between,
      VA_Within, VD_Within
    )

    hVaT <- VA_Total / VF
    hVdT <- VD_Total / VF
    hVaE <- VA_Between / VF
    hVdE <- VD_Between / VF
    hVaD <- VA_Within / VF
    hVdD <- VD_Within / VF
    heritabilities <- data.frame(
      Generation = gen, var, hVaT, hVdT, hVaE, hVdE, hVaD, hVdD
    )

    variance_list[[gen]] <- variances
    heritability_list[[gen]] <- heritabilities
  }
  final_variances <- do.call(rbind, variance_list)
  final_heritabilities <- do.call(rbind, heritability_list)

  for (gen in names(factors)) {
    if(verbose==TRUE){
      cat("\n---------------------------------------------------------------------------------\n")
      cat(paste0(gen, " - Corrected Variances"))
      cat("\n---------------------------------------------------------------------------------\n")
      print(variance_list[[gen]])

      cat("\n---------------------------------------------------------------------------------\n")
      cat(paste0(gen, " - Heritability in the Narrow Sense"))
      cat("\n---------------------------------------------------------------------------------\n")
      print(heritability_list[[gen]])
    }
  }
  return(list(Variances = final_variances, Heritabilities = final_heritabilities))
}

###
#'Allelic and genotype-environment interactions
#'@description
#'Didactic function - Examples of allelic and gene interactions
#'@param type Type of allelic interaction. Use 'ad' for additivity, 'dom'
#'for complete dominance, 'domp' for partial dominance and 'sob' for
#'overdominance.
#'@param ge Type of GxE interaction. Use 'aus' for no interaction,
#''simple' for simple interaction and 'complex' for complex interaction.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@return Plot graphs representing allelic and genotype x environment
#'interactions.
#'@examples
#'library(EstimateBreed)
#'
#'didint (type="ad")
#'didint (type="dom")
#'didint (type="domp")
#'didint (type="sob")
#'
#'didint (ge="aus")
#'didint (ge="simple")
#'didint (ge="complex")
#'@export

didint <- function(type=NULL,ge=NULL){

  if(!is.null(type)){
  if(type=="ad"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 70, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)),
                        altura = rep(altura, each = 10))
    p <- ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5) +
      labs(title = "Additive Genetic Interaction",
           x = "Genotype", y = "Plant Height (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
    print(p)
  } else if(type=="dom"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 120, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)),
                        altura = rep(altura, each = 10))
    p <- ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5) +
      labs(title = "Genetic Interaction of Dominance",
           x = "Genotype", y = "Plant Height (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
    print(p)
  } else if(type=="domp"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 90, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)),
                        altura = rep(altura, each = 10))
    p <- ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5) +
      labs(title = "Partial Dominance Genetic Interaction",
           x = "Genotype", y = "Plant Height (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
    print(p)
  } else if(type=="sob"){
    genotipo <- c("AA", "Aa", "aa")
    efeito <- c(2, 1, 0)
    altura <- c(120, 160, 20)
    dados <- data.frame(genotipo = factor(rep(genotipo, each = 10)),
                        altura = rep(altura, each = 10))
    p <- ggplot(dados, aes(x = genotipo, y = altura, group = 1)) +
      geom_line(aes(color = genotipo), size = 1.2) +
      geom_point(aes(color = genotipo), size = 3) +
      geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
      labs(title = "Genetic Interaction of Overdominance",
           x = "Genotype", y = "Plant Height (cm)") +
      theme_minimal()+
      theme(legend.position = "none")
    print(p)
  }
  }
  if(!is.null(ge)){
    if(ge=="aus"){
      genotipo <- c("AA", "Aa")
      ambientes <- c("Env1", "Env2")
      efeito_ambiente_1 <- c(60, 70)
      efeito_ambiente_2 <- c(95, 105)
      dados <- data.frame(
        genotipo <- rep(genotipo, times = 2),
        ambiente <- rep(ambientes, each = 2),
        altura <- c(efeito_ambiente_1, efeito_ambiente_2)
      )
      p <- ggplot(dados, aes(x = ambiente, y = altura,
                        group = genotipo, color = genotipo)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
        labs(title = "No Genotype-Environment Interaction",
             x = "Environment", y = "Plant Height (cm)") +
        theme_minimal()
      print(p)
    } else if(ge=="simple"){
      genotipo <- c("AA", "Aa")
      ambientes <- c("Env1", "Env2")
      efeito_ambiente_1 <- c(60, 80)
      efeito_ambiente_2 <- c(85, 125)
      dados <- data.frame(
        genotipo <- rep(genotipo, times = 2),
        ambiente <- rep(ambientes, each = 2),
        altura <- c(efeito_ambiente_1, efeito_ambiente_2)
      )
      p <- ggplot(dados, aes(x = ambiente, y = altura,
                        group = genotipo, color = genotipo)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
        labs(title = "Simple Genotype-Environment Interaction",
             x = "Environment", y = "Plant Height (cm)") +
        theme_minimal()
      print(p)
    } else if(ge=="complex"){
      genotipo <- c("AA", "Aa")
      ambientes <- c("Env1", "Env2")
      efeito_ambiente_1 <- c(60, 100)
      efeito_ambiente_2 <- c(110, 60)
      dados <- data.frame(
        genotipo <- rep(genotipo, times = 2),
        ambiente <- rep(ambientes, each = 2),
        altura <- c(efeito_ambiente_1, efeito_ambiente_2)
      )
      p <- ggplot(dados, aes(x = ambiente, y = altura,
                        group = genotipo, color = genotipo)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = altura), vjust = -0.5, color = "black", size = 5)+
        labs(title = "Complex Genotype-Environment Interaction",
             x = "Environment", y = "Plant Height (cm)") +
        theme_minimal()
      print(p)
    }
  }
}

#'Genetic parameters for selection
#'@description
#'Function for determining selection parameters, based on an experiment
#'carried out on the rice crop. Intended for isolated evaluation of the performance
#'of lines within a given population.
#'@param .data The name of the object containing data.
#'@param GEN The column with the selected genotypes within the population.
#'@param REP The column with the repetitions (if any).
#'@param vars The column with the variable of interest.
#'@param K Selection pressure (Default 0.05).
#'@param check Logical argument. Checks the model's assumptions
#'statistical if the value is equal to TRUE.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@return A list containing the following components:
#'
#'\item{Environmental variance (sigmaE)}{The environmental variance (sigmaE) represents
#' the variability in phenotypic traits attributable to environmental factors.
#'  This variance is important for understanding how environmental conditions
#'   influence the observed phenotype.}
#'\item{Genotypic variance (sigmaG)}{The genotypic variance (sigmaG) reflects the
#'variability in phenotypic traits attributable to genetic differences between
#' individuals. It is crucial for assessing the genetic potential of a population
#'  for a specific trait.}
#'\item{Phenotypic variance (sigmaP)}{The phenotypic variance (sigmaP) is the total
#' observed variability in the phenotype, which is the sum of environmental and
#'  genotypic variances. This measure helps understand the overall range of
#'  variation observed in a given dataset.}
#'\item{Environmental coefficient of variance (ECV)}{The environmental
#'coefficient of variance (ECV) is the ratio of environmental variance to the
#'mean of the phenotypic value, expressed as a percentage. It gives an idea of
#'the magnitude of environmental variation relative to the mean value.}
#'\item{Genotypic coefficient of variance (GCV)}{The genotypic coefficient of
#'variance (GCV) is the ratio of genotypic variance to the mean of the phenotypic
#' value, also expressed as a percentage. It is used to estimate how much genetic
#'  variability can be exploited for improving desirable traits.}
#'\item{Phenotypic coefficient of variance (PCV)}{The phenotypic coefficient of
#'variance (PCV) is the ratio of phenotypic variance to the mean of the phenotypic
#' value, expressed as a percentage. It provides insight into the overall impact
#'  of both genetic and environmental factors on the observed variation.}
#'\item{Heritability (h2b)}{Heritability (h2b) is the proportion of phenotypic
#' variance attributable to genotypic variance. It indicates the potential for
#' selecting specific traits within a population.}
#'\item{Genetic advance (GA)}{Genetic advance (GA) represents the amount of
#'genetic progress that can be achieved in one generation by selecting the best
#' individuals for specific traits.}
#'\item{Genetic advance as percentage of the mean (GAM)}{Genetic advance as a
#' percentage of the mean (GAM) is a measure of how much genetic progress
#' represents relative to the population's mean. This value helps assess the
#' effectiveness of selection strategies.}
#'
#'@references
#'Yadav, S. P. S., Bhandari, S., Ghimire, N. P., Mehata, D. K., Majhi, S. K.,
#'Bhattarai, S., Shrestha, S., Yadav, B., Chaudhary, P., & Bhujel, S. (2024).
#'Genetic variability, character association, path coefficient, and diversity
#'analysis of rice (Oryza sativa L.) genotypes based on agro-morphological
#'traits. International Journal of Agronomy, 2024, Article ID 9946332.
#'\doi{10.1155/2024/9946332}
#'
#'@examples
#'library(EstimateBreed)
#'data("genot2")
#'
#'#Geting parameters without cheking model assumptions
#'parameters <- genpar(genot2,Gen,Rep,var =c("VAR1", "VAR2"))
#'parameters$anova
#'parameters$gp
#'
#'#Checking model assumptions
#'parameters <- genpar(genot2,Gen,Rep,var =c("VAR1", "VAR2"),check=TRUE)
#'parameters$anova
#'parameters$gp
#'@export

genpar <- function(.data, GEN, REP, vars, K = 0.05, check = FALSE, verbose = FALSE) {
  GEN <- enquo(GEN)
  REP <- enquo(REP)
  if (!is.character(vars)) {
    stop("'vars' must be a character vector containing variable names.")
  }
  data <- .data %>%
    select(!!GEN, !!REP, all_of(vars)) %>%
    mutate(across(c(!!GEN, !!REP), as.factor))
  colnames(data)[1:2] <- c("GEN", "REP")
  anova_results <- list()
  genpar_results <- list()
  for (var_name in vars) {
    cat("\n==================================================\n")
    cat("Analyzing Variable:", var_name, "\n")
    cat("==================================================\n")
    formula <- as.formula(paste(var_name, "~ GEN + REP"))
    model <- aov(formula, data = data)
    ANOVA_table <- summary(model)[[1]]
    MSg <- ANOVA_table["GEN", "Mean Sq"]
    MSe <- ANOVA_table["Residuals", "Mean Sq"]
    pvalue <- ANOVA_table["GEN", "Pr(>F)"]
    sigmaG <- (MSg - MSe) / length(unique(data$REP))
    sigmaE <- MSe
    sigmaP <- sigmaG + sigmaE
    mean_var <- mean(data[[var_name]], na.rm = TRUE)
    ECV <- (sqrt(sigmaE) / mean_var) * 100
    GCV <- (sqrt(sigmaG) / mean_var) * 100
    PCV <- (sqrt(sigmaP) / mean_var) * 100
    H2 <- sigmaG / sigmaP
    GA <- K * sqrt(sigmaG)
    GAM <- (GA / mean_var) * 100
    if (check == TRUE) {
      cat("\nPerforming assumption tests...\n")
      residuals_model <- residuals(model)
      shapiro_test <- shapiro.test(residuals_model)
      bartlett_test <- bartlett.test(data[[var_name]] ~ data$GEN)
      levene_test <- leveneTest(data[[var_name]] ~ data$GEN, data = data)
      bp_test <- bptest(model)
      cat("Shapiro-Wilk Normality Test: p-value =", round(shapiro_test$p.value, 5), "\n")
      if (shapiro_test$p.value < 0.05) cat("Normality assumption NOT met!\n")
      cat("Bartlett Homogeneity Test: p-value =", round(bartlett_test$p.value, 5), "\n")
      if (bartlett_test$p.value < 0.05) cat("Homogeneity of variances NOT met!\n")
      cat("Levene Homogeneity Test: p-value =", round(levene_test$`Pr(>F)`[1], 5), "\n")
      if (levene_test$`Pr(>F)`[1] < 0.05) cat("Homogeneity of variances NOT met!\n")
      cat("Breusch-Pagan Heteroscedasticity Test: p-value =", round(bp_test$p.value, 5), "\n")
      if (bp_test$p.value < 0.05) cat("Heteroscedasticity detected!\n")
      cat("\n--------------------------------------------------\n")
    }
    if (pvalue >= 0.05) {
      cat("\nGenotypic effect was NOT significant for variable:", var_name, "\n")
      anova_results[[var_name]] <- ANOVA_table
      genpar_results[[var_name]] <- NULL
    } else {
      cat("\nGenotypic effect was significant for variable:", var_name, "\n")
      result <- data.frame(
        Parameter = c("sigmaE", "sigmaG", "sigmaP", "ECV", "GCV", "PCV", "H2", "GA", "GAM"),
        Value = c(sigmaE, sigmaG, sigmaP, ECV, GCV, PCV, H2, GA, GAM)
      )
      colnames(result)[2] <- var_name
      anova_results[[var_name]] <- ANOVA_table
      genpar_results[[var_name]] <- result
    }
    if (verbose==TRUE) {
      cat("\n-- ANOVA Results for", var_name, ":\n")
      print(ANOVA_table)
      if (!is.null(genpar_results[[var_name]])) {
        cat("\n-- Genetic Parameter Estimates for", var_name, ":\n")
        print(genpar_results[[var_name]])
      }
    }
  }
  parameters <- list(
    anova = anova_results,
    gp = genpar_results
  )
  return(parameters)
}


#'Effective Population Size
#'
#'@description
#'Estimates the effective population size (\eqn{N_e}) adapted from Morais (1997).
#'The function provides two different calculation methods: 'classic' and 'alternative'.
#'
#'The classic method follows the equation:
#'
#'\deqn{N_e = \frac{\left(\sum SI\right)^2}{\sum \left(\frac{SI^2}{NE}\right)}}
#'
#'The alternative method is calculated as:
#'
#'\deqn{N_e = \frac{4 \sum SI}{2 + \sum \left(\frac{SI}{NE}\right)}}
#'
#'@param GEN The column with the name of the genotype (progeny).
#'@param SI The column with the number of individuals selected.
#'@param NE Number of individuals conducted during the selection period.
#'@param remove_na Logical argument. If 'TRUE', missing values will be removed.
#'@param method Character string specifying the calculation method. Options are
#'classic' (default) or 'alternative'. 'classic' uses the variance-based
#'method, while 'alternative' uses an adjusted method that accounts for
#'reproductive variation.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@return The result is the effective population size for any variable, based
#'on the number of individuals conducted and selected.
#'@references Morais, R. P. (1997). Effective population size and genetic
#'diversity in improved populations of self-pollinated plants (Doctoral
#'dissertation, University of Campinas).
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@examples
#'library(EstimateBreed)
#'
#'GEN <- c("Genotype1", "Genotype2", "Genotype3", "Genotype4", "Genotype5")
#'SI <- c(10, 15, 12, 18, 14)
#'NE <- c(100, 150, 120, 180, 140)
#'data <- data.frame(GEN,SI,NE)
#'
#'with(data, tamef(GEN, SI, NE, method = "classic"))
#'@export

tamef <- function(GEN, SI, NE, remove_na = TRUE, method = "classic",verbose=TRUE) {
  data <- data.frame(GEN, SI, NE)
  if (remove_na) {
    data <- na.omit(data)
  }
  if (any(data$SI <= 0, na.rm = TRUE)) stop("SI must contain only positive values.")
  if (any(data$NE <= 0, na.rm = TRUE)) stop("NE must contain only positive values.")
  if (method == "classic") {
    Sum_SI <- sum(data$SI, na.rm = TRUE)^2
    Pond_SI <- sum((data$SI^2) / data$NE, na.rm = TRUE)
    if(verbose==TRUE){
      cat("Method used:", method, "\n")
      cat("Sum_SI:", Sum_SI, "\n")
      cat("Pond_SI:", Pond_SI, "\n")
      Ne <- Sum_SI / Pond_SI
      cat("Effective Population Size: \n")
    }
    return(Ne)
  }
  if (method == "alternative") {
    Ne <- (4 * sum(data$SI, na.rm = TRUE)) / (2 + sum((data$SI / data$NE),
                                                      na.rm = TRUE))
    if(verbose==TRUE){
      cat("Method used:", method, "\n")
      cat("Effective Population Size: \n")
      print(Ne)
    }
    return(Ne)
  }
}
