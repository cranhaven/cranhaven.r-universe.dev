#'Auxiliary function for calculating ISGR
#'@description
#'This function receives a dataframe with temperature and precipitation data
#'and calculates the standard deviation of these parameters for each environment.
#'@param ENV Identification of each selection environment (to differentiate if
#'there is more than one cultivation cycle).
#'@param AAT Average air temperature (in degree Celsius) during the cycle in
#'each environment.
#'@param PREC Rainfall (in mm) during the cultivation cycle in each environment
#'@return A dataframe containing the identifier of the selection environment and
#'the standard deviations for temperature and precipitation.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@examples
#'library(EstimateBreed)
#'data("desvamb")
#'head(desvamb)
#'
#'#Use DPclim for the ISGR function to identify deviations correctly
#'DPclim <- with(desvamb,desv_clim(ENV,TMED,PREC))
#'@export

desv_clim <- function(ENV,AAT,PREC) {

  desvio <- data.frame(ENV,AAT,PREC)

  resultado <- desvio %>%
    group_by(.data$ENV) %>%
    summarise(
      STMED = sd(.data$AAT, na.rm = TRUE),
      TMEDR = mean(.data$AAT, na.rm = TRUE),
      SPREC = sd(.data$PREC, na.rm = TRUE),
      PRECIR = sum(.data$PREC, na.rm = TRUE)
    )
  return(resultado)
}

################################################################################
#'ISGR - Genetic Selection Index for Resilience
#'@description
#'Estimation of the selection index for environmental resilience
#'(Bandeira et al., 2024).
#'@param GEN Column referring to genotypes. Lines must have the prefix 'L' before
#' the number. Ex: L139.
#'@param ENV The column for the selection environment.
#'@param NG Number of grains of all genotypes evaluated
#'@param MG Grain mass of all genotypes evaluated
#'@param CICLO Number of days in the cycle to define rainfall
#'ideal (value of 3.5 mm per day). Can be changed manually in the 'req' argument.
#'@param req Average daily water demand for the soybean crop (standard 3.5 mm).
#'May change depending on the phenological stage.
#'@param stage Parameter to define the phenological stage the crop is in
#'Use 'veg' for vegetative and 'rep' for reproductive, if the
#'evaluations have only been carried out in a given period.
#'@return The ISGR - Genetic Selection Index for Resilience defines the ability
#' of genotypes to express their productivity components under the conditions of
#'  air temperature and rainfall offered by the environment. The lower the index,
#'   the more resilient the genotype.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Bandeira, W. J. A., Carvalho, I. R., Loro, M. V., da Silva, J. A. G.,
#'Dalla Roza, J. P., Scarton, V. D. B., Bruinsma, G. M. W., & Pradebon, L. C. (2024).
#'Identifying soybean progenies with high grain productivity and stress resilience
#'to abiotic stresses. Aust J Crop Sci, 18(12), 825-830.
#'@examples
#'library(EstimateBreed)
#'
#'#Obtain environmental deviations
#'data("desvamb")
#'head(desvamb)
#'
#'#Use DPclim for the ISGR function to identify deviations correctly
#'DPclim <- with(desvamb,desv_clim(ENV,TMED,PREC))
#'
#'#Calculate the ISGR
#'data("genot")
#'head(genot)
#'isgr_index <- with(genot, isgr(GEN,ENV,NG,MG,CICLO))
#'
#'#Define the water requirement per stage
#'isgr_index <- with(genot, isgr(GEN,ENV,NG,MG,CICLO,req=5,stage="rep"))
#'@export

isgr <- function(GEN, ENV, NG, MG, CICLO, req=3.5, stage=NULL) {

  GEN <- as.factor(GEN)
  ENV <- as.factor(ENV)
  NG <- as.numeric(NG)
  MG <- as.numeric(MG)
  CICLO <- as.numeric(CICLO)
  dados <- data.frame(GEN, ENV, NG, MG, CICLO)

  if (is.null(stage)) {
    req <- req
  } else if (stage == "veg") {
    req <- req
  } else if (stage == "rep") {
    req <- req
  }

  desvng <- sd(dados$NG, na.rm = TRUE)
  desvmg <- sd(dados$MG, na.rm = TRUE)

  dados <- dados %>%
    mutate(tipo = ifelse(grepl("^L", as.character(.data$GEN)), "Lin", "Test"))

  control <- dados %>%
    filter(.data$tipo == "Test")

  if (nrow(control) > 0) {
    NGT <- mean(control$NG, na.rm = TRUE)
    MGT <- mean(control$MG, na.rm = TRUE)
  } else {
    stop("No control found!")
  }

  SNG <- desvng
  SMG <- desvmg

  if (!exists("DPclim")) {
    stop("The standard deviations must be obtained from the desv_clim!")
  }

  PRECI <- dados %>%
    group_by(.data$ENV) %>%
    summarise(PREC = first(.data$CICLO) * req)

  prep1 <- dados %>%
    filter(.data$tipo == "Lin") %>%
    mutate(
      NGT = NGT,
      SNG = SNG,
      MGT = MGT,
      SMG = SMG
    ) %>%
    mutate(ENV = as.character(.data$ENV)) %>%
    as_tibble() %>%
    left_join(PRECI, by = "ENV") %>%
    rename(NGL = NG, MGL = MG) %>%
    left_join(DPclim %>% mutate(ENV = as.character(.data$ENV)), by = "ENV") %>%
    select(.data$GEN, .data$ENV, .data$NGL, .data$MGL,
           .data$NGT, .data$SNG, .data$MGT, .data$SMG,
           .data$PREC, .data$STMED, .data$TMEDR, .data$SPREC, .data$PRECIR)

  prep1 <- prep1 %>%
    mutate(
      ISGR = ((.data$NGT - .data$NGL) / .data$SNG) *
        ((.data$MGT - .data$MGL) / .data$SMG) *
        ((.data$PREC - .data$PRECIR) / .data$SPREC) *
        ((25 - .data$TMEDR) / .data$STMED)
    )

  final <- prep1 %>%
    select(Gen = .data$GEN, Env = .data$ENV, ISGR = .data$ISGR) %>%
    arrange(.data$ISGR)
  return(final)
}
