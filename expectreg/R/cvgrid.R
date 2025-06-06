cvgrid <-
function(yy, B, quantile, DD, nb, constmat, types)
{
    las1 = seq(-2, 4, by = 0.75)
    glatterms = which(types != "parametric")
    lambdas_list <- list()
    penalty <- 1
    if(length(glatterms) >= 1) {
        for (i in 1:length(glatterms)){
            lambdas_list[[i]] <- las1
        }
        lambdas <- expand.grid(lambdas_list)
        score = rep(0, nrow(lambdas))
        
        lambdas = 10^lambdas
        
        penalty = rep(0, length(types))
        for (i in 1:nrow(lambdas)) {
            penalty[glatterms] = lambdas[i, ]
            aa <- asyregpen.lsfit(yy, B, quantile, abs(penalty), DD, nb, constmat)
            score_temp = aa$weight*(yy - B%*%aa$a)^2/(1-aa$diag.hat.ma)^2
            score[i] = mean(score_temp[which(is.finite(score_temp))],na.rm=TRUE) # laws Code
            
            #score[i] = mean(aa$weight * (yy - B %*% aa$a)^2/(1 - aa$diag.hat.ma)^2,na.rm = TRUE)
            
        }
        penalty[glatterms] = lambdas[which.min(score), ]
        
    }
    
    
    penalty
    
}
