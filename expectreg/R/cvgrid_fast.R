cvgrid_fast <-
function(yy,B,quantile,DD,nb,types,lala1)
{
    las1 = seq(-2, 4, by = 0.75)
    glatterms = which(types != "parametric")
    lambdas_list <- list()
    lambdas <- 1
    if(length(glatterms) >= 1) {
        for (i in 1:length(glatterms)){
            lambdas_list[[i]] <- las1
        }
        lambdas <- expand.grid(lambdas_list)
        score = rep(0, nrow(lambdas))
        
        lambdas = 10^lambdas
        
        penalty = rep(0, length(types))
        for(i in 1:nrow(lambdas)){
            penalty[glatterms] = lambdas[i,]
            lala_glatt <- penalty[glatterms]
            lala_orig <- lala1
            aa <- lltLS(y=as.numeric(yy), B=B, tau=quantile, lambdashort_glatt=abs(lala_glatt), lambdashort_orig=as.numeric(lala_orig), DD=DD, NB=as.integer(nb), glatterms = glatterms)
            score_temp = aa$weight*(yy - B%*%aa$a)^2/(1-aa$diag.hat.ma)^2
            score[i] = mean(score_temp[which(is.finite(score_temp))],na.rm=TRUE) # laws Code
            
            #score[i] =    mean(aa$weight*(yy - B%*%aa$a)^2)/(1-(1+sum(aa$diag.hat.ma))/length(aa$diag.hat.ma))^2 
        }
        penalty[glatterms] = lambdas[which.min(score),]
        lambdas[which.min(score),]
    }
    lambdas
    
}
