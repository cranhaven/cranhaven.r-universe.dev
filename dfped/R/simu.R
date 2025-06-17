#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
simu <-
function(targetTox, targetEff, skeletonTox, skeletonEff, startingDose, nbSubjects, crmModel,
                 cohortSize, scenarioTox, scenarioEff, nbDesign, mu, sd = NULL, lesb, sigmaLI,
                 sigmaHI, adaptivePrior){
    
    sdSatoshi = sd
    # General parameters
    resultsSoFar <- list(data.frame(matrix(NA, ncol = 2*nbDesign + 2, 0)), data.frame(numeric(0)), 
                         data.frame(numeric(0)), data.frame(numeric(0)), c())
    kickoff <- FALSE
    
    givenDose <- c()					# Vector of given doses.
    tox <- c()							# Vector of simulated tox.
    eff <- c()							# Vector of simulated responses.
    currentDose <- startingDose			# Starting dose.
    
    nbPatientsj <- 0				
    
    # We start the simulation of patients.
    
    # Kickoff part: Until we have at least one efficacy and one toxicity,
    # We introduce new patients on a 3+3 based algorithm - cohorts of 3
    while (kickoff == FALSE & (nbPatientsj < nbSubjects)){
        givenDose <- c(givenDose, rep(currentDose, cohortSize))			# We simulate 3 patients that receive 3 times currentDose.
        nbPatientsj <- length(givenDose)
        
        # Update the number of current patients. 
        
        # Simulation of the responses for the included patients.
        tox <- c(tox, rbinom(cohortSize, 1, scenarioTox[currentDose]))		# Simulation of tox for the included patients.
        eff <- c(eff, rbinom(cohortSize, 1, scenarioEff[currentDose]))		# Simulation of the responses for the included patients.
        
        resultj <- crmAtj(crmModel, resultsSoFar, kickoff, nbPatientsj, tox, eff, givenDose,
                          skeletonTox, skeletonEff, cohortSize, targetTox, targetEff, mu, sdSatoshi,
                          lesb, sigmaLI, sigmaHI, adaptivePrior)
        
        
        # Update
        resultsSoFar <- resultj[[1]]		# Update the results with the patients that have been so far.
        kickoff <- resultj[[3]]				# Update the kickoff
        currentDose <- resultj[[2]]			# Update the currentDose
    }
    S1 <- TRUE								# We initialize the stopping rules as TRUE
    S2 <- TRUE
    
    # Enough hetergeneity in responses: we start the bayesian estimation
    while ((nbPatientsj < nbSubjects) & S1 & S2 ){
        givenDose <- c(givenDose, currentDose)								# We give the dose.
        nbPatientsj <- length(givenDose)									# We put the patient.
        tox <- c(tox, rbinom(1, 1, scenarioTox[currentDose]))				# We simulate the toxicity.
        eff <- c(eff, rbinom(1, 1, scenarioEff[currentDose]))				# We simulate the efficacite.
        resultj <- crmAtj(crmModel ,resultsSoFar, kickoff, nbPatientsj, tox, eff, givenDose,
                          skeletonTox, skeletonEff, cohortSize, targetTox, targetEff, mu, sdSatoshi,
                          lesb, sigmaLI, sigmaHI, adaptivePrior)
        
        # Update
        resultsSoFar <- resultj[[1]]										
        kickoff <- resultj[[3]]
        currentDose <- resultj[[2]]
        
        # Stopping Rules
        S1 <- resultj[[4]] < 0.90			# We continue until P(varphi_d1>targetTox) < 0.9.
        S2 <- resultj[[5]] < 0.90			# We continue until P(psi_dK < targetEff) < 0.9.
    }
    return(list(resultsSoFar, currentDose, givenDose))
}
