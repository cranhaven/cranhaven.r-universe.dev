#' @import lme4
#' @import stats4
#' @import stats
#' @import utils
#' @export
MA_estimates <- function(data, sim0, sim1, family = binomial, link = "logit", nAGQ = 1, 
                        control = glmerControl(optimizer = "bobyqa")) {

    analyf0 <- data[order(data$simulation), ]
    
    # Using the function 'doseRecords' to make a random effects analysis.
    # this database will be used after in the random effects model based algorithm.

    Datasim <- doseRecords(analyf0)$doseRecords

    model <- glimem(Datasim, sim0 = sim0, sim1=sim1, family = family, nAGQ = nAGQ, control = control)

    otwo <- model$simData
    otwo <- otwo[order(otwo$simulation, otwo$dose), ]
    otwo <- otwo[, c("simulation", "dose", "Trial", "PredMu")]
    otwo <- otwo[order(otwo$simulation, otwo$dose, otwo$Trial), ]
  
    hatpij <- analyf0
  
    hatpij$ratio <- ifelse(hatpij$n == 0,
                            1,                  # if the condition is TRUE 
                            hatpij$x/hatpij$n   # else if the condition is FALSE
                            )
    otwof1 <- hatpij
    otwof1 <- otwof1[order(otwof1$simulation, otwof1$dose, otwof1$Trial), ]
  
    ptoxic1 <- merge(otwof1, otwo, by = c("simulation", "dose", "Trial"), all = TRUE)
  
  
    ptoxic1$rndpt <- ifelse(is.na(ptoxic1$PredMu) == FALSE,
                            ptoxic1$PredMu,
                            ptoxic1$ratio
                            )
  
    ptoxic1 <- ptoxic1[ , c("simulation", "dose", "Trial", "ratio", "PredMu", "rndpt")]
  
    ptoxic4 <- ptoxic1
  
    ptoxic4$PredMu <- ifelse(is.na(ptoxic4$PredMu) == TRUE,
                            "NA", # if PredMu is NA which means that no prediction for these values is available.
                            ptoxic4$PredMu
                            )
  
    ptoxic4 <- ptoxic4[order(ptoxic4$simulation, ptoxic4$Trial, ptoxic4$dose), ]
    
    rstart = NULL
    rend = NULL
    
    rstart = min(which(ptoxic4$simulation == sim0))
    rend = max(which(ptoxic4$simulation  == sim1))
    
    # res <- list(pred_RandomEffect = model$simData, estimates = ptoxic4[rstart:rend, ])
    # return(res)


    new("REMB", dataTox = data, sim0 = sim0, sim1 = sim1, family = family, link = link, estimates = ptoxic4[rstart:rend, ])



    ## OUTPUT --> A data.frame named as estimates is returned including the following outputs:
    ## 1) ratio: Defines the # of toxicities / # of subjects for each simulation, trial and dose. 
    ## 2) PredMu: is the predicted random effect center "mu"; if it's "NA" it means that there is no prediction for it.
    ## 3) rndpt: is the predicted toxicity probability using either "mu" if it is available or "ratio" if the corresponding "mu" is not available.
}
