bicfun <-
function(penalty, yy, B, quantile, DD, nb, constmat)
{
    aa <- asyregpen.lsfit(yy, B, quantile, abs(penalty), DD, nb, constmat)
    
    score = log(sum(aa$weight * (yy - B %*% aa$a)^2/length(yy))) * length(yy) + log(length(yy)) * (sum(aa$diag.hat.ma))
    
    score
}
