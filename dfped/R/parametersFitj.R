#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
parametersFitj <-
function(fitj, nbDoses, nbDesign, targetTox, targetEff, model){
    S <- nbDesign
    res_stan <- extract(fitj, permuted = TRUE)
    probaVarphi <- c()
    probaPsi <- c()
    p <- c()
    param <- data.frame(matrix(NA, ncol = 2*S +1, nrow = 1))
    varAlpha <- c()
    varBeta <- c()
    
    interStopTox <- data.frame(table(res_stan$varphi[ ,1] > targetTox)/length(res_stan$varphi[ ,1]))
    stoppingRuleTox <- interStopTox[interStopTox[ ,1] == TRUE, 2]
    
    interStopEff <- data.frame(table(res_stan$psi[ ,nbDoses] < targetEff)/length(res_stan$psi[ ,nbDoses]))
    stoppingRuleEff <- interStopEff[interStopEff[ ,1] == TRUE, 2]
    
    if(length(stoppingRuleTox)==0){
        stoppingRuleTox <- 0
    }
    if(length(stoppingRuleEff)==0){
        stoppingRuleEff <- 0
    }
    if(S==1){
        param <- data.frame(alpha = mean(res_stan$alpha), beta = mean(res_stan$beta),
                            "model"= model)
    }else{
        for (s in 1:S){
            varAlpha <- c(varAlpha, paste('alpha', s, sep = ""))
            varBeta <- c(varBeta, paste('beta', s, sep = ""))
            param[1,s] <-  mean(res_stan$alpha[ ,s])
            param[1, s + S] <- mean(res_stan$beta[ ,s])
        }
        param[1, 2*S+2] <- NA
        colnames(param) <- c(varAlpha, varBeta, "model")
    }
    for (k in 1:nbDoses){
        probaVarphi <- c(probaVarphi, mean(res_stan$varphi[ ,k]))
        probaPsi <- c(probaPsi, mean(res_stan$psi[ ,k]))
        # In case where p is not estimated by the stan model
        if(is.null(res_stan$p[ ,k])){
            p <- c(p, NA)
        }else{	
            p <- c(p, mean(res_stan$p[ ,k]))
        }
    }
    probaVarphi <- data.frame(probaVarphi)
    probaPsi <- data.frame(probaPsi)
    p <- data.frame(p)
    fin <- list(param, probaVarphi, probaPsi, p, stoppingRuleTox, stoppingRuleEff)
    return(fin)
}
