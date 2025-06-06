agcv <-
function(penalty,yy,B,quantile,DD,nb,constmat)
    # asymmetric cross validation
    # computes the acv score for the smoothing of the regression
    # score has to be minimized dependant on parameter "penalty"
    # therefore a grid search can be applied to this function
    # parameters:
    # penalty - smoothing parameter lambda
    # yy - vector of responses
    # B - basis for the approximation
    # p - quantile
    # DD - penalization matrix
{
    aa <- asyregpen.lsfit(yy, B, quantile, abs(penalty), DD, nb, constmat)
    
    
    #score = aa$weight*(yy - B%*%aa$a)^2/(1-aa$diag.hat.ma)^2
    #mean(score[which(is.finite(score))],na.rm=TRUE) # laws Code
    
    #  mean(aa$weight*((yy - B%*%aa$a)/(1-aa$diag.hat.ma))^2) 
    
    mean(aa$weight * (yy - B %*% aa$a)^2)/(1-(1+sum(aa$diag.hat.ma))/length(aa$diag.hat.ma))^2 
#   mean(aa$weight * (yy - B %*% aa$a)^2)/(1 - aa$diag.hat.ma)^2,na.rm = T)
}
