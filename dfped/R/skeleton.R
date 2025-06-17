#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @import stats4
#' @export
skeleton <-
function(doseChildren, doseAdult, dataTox, dataAuc = NULL, Clad, Clch, nbSimu, graph = TRUE){
    dataframe_tox <- dataTox
    probaN <- function(intercept, pente, d, Cl){1/(1 + exp(-intercept - pente*(d/Cl)))}
    
    dose_child_corresp <- rep(NA, length(dataframe_tox$doses))
    for(i in 1:length(doseAdult)){
        dose_child_corresp[which(dataframe_tox$doses == doseAdult[i])] <- doseChildren[i]
    }
    
    dataframe_tox <- data.frame(dataframe_tox, dose_child_corresp)
    
    meta_Sarah <- metaPhase(dataframe_tox, doseAdult, nbSimu)
    res_sarah <- rep(NA, length(dataframe_tox$doses))
    
    for(i in 1:length(doseAdult)){
        res_sarah[which(dataframe_tox$doses == doseAdult[i])] <- meta_Sarah[[2]][i]
    }
    
    if (!is.null(dataAuc[1, ])){
        logitAUCThomas <- log(dataAuc[ ,2]/(1 - dataAuc[ ,2]))
        fitLogit <- lm(logitAUCThomas ~ dataAuc[ ,1])
        intercept <- fitLogit$coeff[1]
        pente <- fitLogit$coeff[2]
        gamma1 <-  probaN(intercept, pente, dataframe_tox$doses, Clad)
        
        lambda_metaAnalysis <- ((gamma1)^dataframe_tox$nbTox*(1 - gamma1)^(dataframe_tox$nbPatients - dataframe_tox$nbTox)) / ((res_sarah)^dataframe_tox$nbTox*(1-res_sarah)^(dataframe_tox$nbPatients - dataframe_tox$nbTox))
        weight_metaAnalysis <- lambda_metaAnalysis/(lambda_metaAnalysis + 1)
        mixtureEstimate_metaAnalysisSarah <- weight_metaAnalysis*gamma1 + (1 - weight_metaAnalysis)*res_sarah
        
        data_mixture <- data.frame(dataframe_tox, gamma1, res_sarah, mixtureEstimate_metaAnalysisSarah)
        estim_metaAnalysis <- aggregate(data_mixture$mixtureEstimate_metaAnalysisSarah, list(data_mixture$doses), mean)$x
        estimatesNP <- aggregate(data_mixture$res_sarah, list(dataframe_tox$doses), mean)$x
        estimatesP <- aggregate(data_mixture$gamma1, list(data_mixture$doses), mean)$x
        doses_children_from_ad <- aggregate(data_mixture$dose_child_corresp, list(data_mixture$doses), mean)$x
        
        logit_estimate_metaAnalysisSarah <- log(estim_metaAnalysis/(1 - estim_metaAnalysis))
        n_logit_estimate_metaAnalysisSarah <- length(logit_estimate_metaAnalysisSarah)
        
        AUC_ch_metaAnalysisSarah <- doseChildren[1:n_logit_estimate_metaAnalysisSarah]/Clch
        
        fitLogit_mixture_metaAnalysisSarah <- lm(logit_estimate_metaAnalysisSarah ~ AUC_ch_metaAnalysisSarah)
        
        slope <- fitLogit_mixture_metaAnalysisSarah$coeff[2]
        intercept <- fitLogit_mixture_metaAnalysisSarah$coeff[1]
        
        estimates <- round(1 / (1 + exp(-intercept - slope*(doseChildren/Clch))), digits = 2)
        nTot <- length(estimates)
        
        skeleton1_mat <- estimates
        skeleton2_mat <- c(estimates[2 : nTot], (estimates[nTot] + 1) / 2)
        skeleton3_mat <- c(estimates[1] / 2, estimates[1 : (nTot - 1)])
        
        # Plot the graph if user indicates graph == TRUE in the input arguments of simulation function. 
        
        if(graph == TRUE){
            plotEstimates(0, 300, slope, intercept, estim_metaAnalysis,
                          estimatesNP, estimatesP, doses_children_from_ad, Clch)
        }
        
        # return(data.frame(skeleton1_mat, skeleton2_mat, skeleton3_mat))
        return(list(skeleton1 = skeleton1_mat, skeleton2 = skeleton2_mat, skeleton3 = skeleton3_mat,
                    slope = slope, intercept = intercept, estimMetaAnalysis = estim_metaAnalysis, estimatesNP = estimatesNP,
                    estimatesP = estimatesP, dosesChildrenFromAd = doses_children_from_ad, Clch = Clch))
    }	
}
