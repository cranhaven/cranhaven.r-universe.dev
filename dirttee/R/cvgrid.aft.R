cvgrid.aft <-
function(yy,delta,B,quantile,DD,nb,constmat,types)
{
	las1 = seq(-2, 4, by = .75)
	glatterms = which(types != "parametric")
#print(glatterms)
	#lambdas = matrix(las1,nrow=length(las1)*length(glatterms),ncol=1)
	#if(length(glatterms) > 1)
	#for(i in 2:length(glatterms))
    #  lambdas = cbind(lambdas,rep(las1,each=i,times=length(glatterms)-i+1))
     
     lambdas_list <- list()
    for(i in 1:length(glatterms)) {
        lambdas_list[[i]] <- las1
        }
    lambdas <- expand.grid(lambdas_list)
     
    score = rep(0, nrow(lambdas))
    
    lambdas = 10^lambdas
    
    penalty = rep(0,length(types))

    for(i in 1:nrow(lambdas))
    {
    	penalty[glatterms] = lambdas[i,]
    	aa <- asyregpen.aft(yy, delta, B, quantile, abs(penalty), DD, nb, constmat)

        score[i] =  mean(aa$weight*(yy - B%*%aa$a)^2/(1-aa$diag.hat.ma)^2,na.rm=TRUE)
    
    }
    
    penalty[glatterms] = lambdas[which.min(score),]

    penalty
    
}
