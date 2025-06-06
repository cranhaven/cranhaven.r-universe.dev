acv_ipc <-
function (penalty, yy, B, quantile, DD, nb, constmat, KMweights, hat1 = FALSE) 
{
    aa <- asyregpen_ipc(yy, B, quantile, abs(penalty), DD, nb, constmat, KMweights = KMweights, hat1 = hat1) #!
    score = aa$weight * (yy - B %*% aa$a)^2/(1 - aa$diag.hat.ma)^2
    mean(score[which(is.finite(score))], na.rm = TRUE)
}
