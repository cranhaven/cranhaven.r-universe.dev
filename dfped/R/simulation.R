#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
simulation <-
function(stanModel, scenarioTox, scenarioEff, nbSubjects, nbSimu, skeletonTox,
                       skeletonEff, targetTox, targetEff, cohortSize, startingDose,
                       sd = NULL, mu, adaptivePrior, saveName){
    
    sdSatoshi = sd
    
    if (file.exists(paste(saveName,".RData", sep = ""))==TRUE){
        stop("Error: This file already exists. Please change name.")
    }
    nbDesign <- length(skeletonTox[1, ])
    nbDoses <- length(scenarioTox)	
    dmax <- which(scenarioTox ==max(c(scenarioTox[1],scenarioTox[scenarioTox <= targetTox])))
    
    k <- 1
    
    lesb <- calcul.bi(skeletonTox[ ,1], mu, a = NULL, "power_log", targetTox)
    # print(lesb)
    sigmaLI <- sigmaLI(skeletonTox[ ,1], mu, a = NULL, "power_log", targetTox)
    sigmaHI <- sigmaHI(skeletonTox[ ,1], mu, a = NULL, "power_log", targetTox, 0.80)
    
    # Initialisation
    d <- c()							# Vector of the selected dose at the end of the trial. Length: nbSimu
    alpha <-  data.frame(numeric(0))
    beta <-  data.frame(numeric(0))
    ttt <- c()
    varphi <- data.frame(numeric(0))	# The toxicity
    psi <- data.frame(numeric(0))		# The efficacy
    p <- data.frame(numeric(0))
    nbSubjectsTreated <- c()
    
    # Simulations
    while(k <= nbSimu){
        t <- simu(targetTox, targetEff , skeletonTox, skeletonEff, startingDose, nbSubjects,
                  stanModel,cohortSize, scenarioTox, scenarioEff, nbDesign, mu,
                  sdSatoshi, lesb, sigmaLI, sigmaHI, adaptivePrior)
        d <- c(d, t[[2]])
        nbSubjectsTreated <- c(nbSubjectsTreated, length(t[[3]]))
        n <- length(t[[1]][[1]]$alpha1)
        value_alpha <- t[[1]][[1]][n,grep("alpha", colnames(t[[1]][[1]]))]
        alpha <- rbind(alpha, if(length(value_alpha)==0){rep(NA, 1)}else{value_alpha})
        value_beta <- t[[1]][[1]][n,grep("beta", colnames(t[[1]][[1]]))]
        beta <- rbind(beta, if(length(value_beta)==0){ rep(NA, 1)}else{value_beta})
        ttt <- c(ttt, sum(t[[3]] > dmax))
        nphi <-  length(t[[1]][[2]][,1])
        npsi <-  length(t[[1]][[3]][,1])
        np <- length(t[[1]][[4]][,1])
        varphi <- rbind(varphi, if(nphi == 0){ rep(NA,nbDoses)}else{t[[1]][[2]][nphi,]})
        psi <- rbind(psi, if(npsi == 0){ rep(NA, nbDoses)}else{t[[1]][[3]][npsi, ]})
        p <- rbind(p, if(np == 0){ rep(NA, nbDoses)}else{ t[[1]][[4]][np, ]})
        
        if(k == 1){
            colnames(alpha) <- paste("alpha", 1, sep = "")
            colnames(beta) <- paste("beta", 1, sep = "")
            colnames(varphi) <- paste("V", 1:nbDoses, sep = "")
            colnames(psi) <- paste("V", 1:nbDoses, sep = "")
            colnames(p) <- paste("V", 1:nbDoses, sep = "")
        }
        if((k - trunc(k/200)*200) == 0){
            saveSimu(saveName, d, alpha, beta, ttt, varphi, psi, p, nbSubjectsTreated)
        }
        k <- k+1
    }
    saveSimu(saveName, d, alpha, beta, ttt, varphi, psi, p, nbSubjectsTreated)
}
