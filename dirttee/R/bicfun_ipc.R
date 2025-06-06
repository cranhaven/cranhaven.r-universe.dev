bicfun_ipc <-
function(penalty, yy, B, quantile, DD, nb, constmat, KMweights, hat1 = FALSE)
{
    aa <- asyregpen_ipc(yy, B, quantile, abs(penalty), DD, nb, constmat, KMweights = KMweights, hat1 = hat1) #!
    
    score = log(sum(aa$weight * (yy - B %*% aa$a)^2/length(yy))) * length(yy) + log(length(yy)) * (sum(aa$diag.hat.ma))
    
    score
}
