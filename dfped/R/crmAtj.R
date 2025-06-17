#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
crmAtj <-
function(crmModel, resultsSoFar, kickoff, nbPatientsj, tox, eff, givenDose,
                   skeletonTox, skeletonEff, cohortSize, targetTox, targetEff, mu,
                   sd = NULL, lesb, sigmaLI, sigmaHI, adaptivePrior){
    
    sdSatoshi = sd
    n <- length(givenDose)
    stoppingRuleTox <- 0
    stoppingRuleEff <- 0
    
    nbDesign <- length(skeletonTox[1, ])
    nbDoses <- length(skeletonTox[ ,1])
    
    # We test if the data kicked off already with either one toxicity or one efficacy
    # and if yes, we turn kickoff to TRUE.
    # Otherwise, nothing is changed except for the next given dose, which is incremented of 1.
    
    if(kickoff == FALSE){
        test <- kickoffControl(tox, givenDose[n], cohortSize, nbDoses)
        kickoff <- test[[1]]
        if (kickoff == FALSE){
            newDose <- test[[2]]	
        }
    }
    
    # If kickoff = TRUE, we estimate the parameters using the stan function fitDataj,
    # then we extrapolate the parameters from the samples with parametersFitj and
    # finally, we calculate the next dose (max of efficacy when tox< targetTox).
    
    if (kickoff == TRUE){
        if(adaptivePrior == TRUE){
            if(length(sdSatoshi) != 0){
                sigma <- sdSatoshi
            }else{
                if(priorChoice( tox, givenDose, skeletonTox, lesb)){
                    sigma <- sigmaHI
                }else{
                    sigma <- sigmaLI
                }	
            }
        }else{
            sigma <- sdSatoshi
        }
        
        # For each design, we compute the waic with the estimations of STAN.
        # After all designs have been run, we keep the one with the lower waic.
        
        waics <- 10000
        for(s in 1:nbDesign){
            fitj <- fitDataj(crmModel, nbPatientsj, nbDoses, tox, eff, givenDose, skeletonTox,
                             skeletonEff, mu, sigma, s)
            waicj <- waic(stanfit = fitj, s)
            if(waicj < waics){
                waics <- waicj
                paramj <- parametersFitj(fitj, nbDoses, 1, targetTox, targetEff, model = s)
            }
        }
        
        # We gather the estimated parameters that will be returned in resultSoFar
        
        r <- paramj[[2]][1][,1]
        q <- paramj[[3]][1][,1]
        if(TRUE %in% is.na(paramj[[4]][1][,1])){
            p <- (1-r)*q
        }else{
            p <- paramj[[4]][1][,1]
        }
        
        
        # Choice of the next dose.
        newDose <- doseChoice(r, q, p, targetTox, givenDose)
        
        resultsSoFar[[1]] <- rbind(resultsSoFar[[1]], paramj[[1]])
        resultsSoFar[[2]] <- rbind(resultsSoFar[[2]], t(paramj[[2]]))
        resultsSoFar[[3]] <- rbind(resultsSoFar[[3]], t(paramj[[3]]))
        resultsSoFar[[4]] <- rbind(resultsSoFar[[4]], t(data.frame(p)))
        resultsSoFar[[5]] <- c(resultsSoFar[[5]], sigma)
        stoppingRuleTox <- paramj[[5]][1]
        stoppingRuleEff <- paramj[[6]][1]
    }
    fin <- list(resultsSoFar, newDose, kickoff, stoppingRuleTox, stoppingRuleEff)
    return(fin)
}
