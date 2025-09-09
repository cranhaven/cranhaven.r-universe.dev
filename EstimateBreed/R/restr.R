#'Restriction of control variability
#'@description
#'Method for restricting the variability of control proposed by Carvalho et al.
#'(2023). It uses the restriction of the mean plus or minus one standard deviation.
#'standard deviation, which restricts variation by removing asymmetric values.
#'@param TEST The column with the name of the witness
#'@param REP The column with the replications
  #'@param Xi The column with the observed value for a given genotype.
#'@param scenario Scenario to be used for the calculation. Use 'original' to
#'do not restrict the witnesses by the mean plus or minus the standard deviations,
#' or 'restr' to apply the restriction.
#'@param zstat Logical argument. Applies Z-notation normalization if 'TRUE'.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@return Describes controls that were removed from the dataset to restrict
#'variability.
#'@references
#'Carvalho, I. R., Silva, J. A. G. da, Moura, N. B., Ferreira, L. L.,
#'Lautenchleger, F., & Souza, V. Q. de. (2023). Methods for estimation of
#'genetic parameters in soybeans: An alternative to adjust residual variability.
#'Acta Scientiarum. Agronomy, 45, e56156.
#'\doi{10.4025/actasciagron.v45i1.56156}
#'@examples
#'library(EstimateBreed)
#'
#'TEST <- rep(paste("T", 1:5, sep=""), each=3)
#'REP <- rep(1:3, times=5)
#'Xi <- rnorm(15, mean=10, sd=2)
#'
#'data <- data.frame(TEST,REP,Xi)
#'
#'#Apply the control variability constraint
#'Control <- with(data, restr(TEST,REP,Xi,scenario = "restr",zstat = FALSE))
#'
#'#Apply control variability restriction with normalization (Z statistic)
#'Control <- with(data, restr(TEST,REP,Xi,scenario = "restr",zstat = TRUE))
#'@export

restr <- function(TEST, REP, Xi, scenario = NULL, zstat = NULL,verbose=FALSE) {
  if (is.null(scenario)) {
    stop("Please inform if the restriction will be applied!")
  }

  if (scenario == "restr") {
    media <- mean(Xi)
    desvio <- sd(Xi)
    lim_1s <- c(media - desvio, media + desvio)
    dentro_limite <- Xi >= lim_1s[1] & Xi <= lim_1s[2]
    ream <- data.frame(TEST = TEST[dentro_limite], REP = REP[dentro_limite],
                       Xi = Xi[dentro_limite])
    removidos <- TEST[!dentro_limite]
    rep_removidos <- REP[!dentro_limite]
    gen_rep_removidos <- paste(removidos, rep_removidos, sep = "R")
    n_desvio <- sd(ream$Xi)
    n_media <- mean(ream$Xi)
    if(verbose==TRUE){
      cat("Removed Controls\n")
      print(gen_rep_removidos)
    }
  }

  if (is.null(zstat)) {
    stop("Please inform if standardization will be applied!")
  }

  if (zstat == FALSE) {
    if (scenario == "restr") {
      colnames(ream) <- c("GEN", "REP", "Xi")
      assign("Control", ream, envir = .estimatebreed_env)
      return(ream)
    } else if (scenario == "original") {
      datatest <- data.frame(TEST, REP, Xi)
      colnames(datatest) <- c("GEN", "REP", "Xi")
      assign("Control", datatest, envir = .estimatebreed_env)
      return(datatest)
    }
  }

  if (zstat == TRUE) {
    if (scenario == "restr") {
      reamost <- ream %>%
        mutate(znorm = (Xi - n_media) / n_desvio)
      colnames(reamost) <- c("GEN", "REP", "Xi", "znorm")
      assign("Control", reamost, envir = .estimatebreed_env)
      return(reamost)
    } else if (scenario == "original") {
      orig <- data.frame(TEST, REP, Xi)
      media <- mean(orig$Xi)
      desvio <- sd(orig$Xi)
      reamost_orig <- orig %>%
        mutate(znorm = (Xi - media) / desvio)
      colnames(reamost_orig) <- c("GEN", "REP", "Xi", "znorm")
      assign("Control", reamost_orig, envir = .estimatebreed_env)
      return(reamost_orig)
    }
  }
}
