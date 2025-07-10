loglikfunc <- function(Hmat, Klist, nu = 10000) {
    
    loglik1 <- function(x) {
        
        matchout <- match(rownames(x), rownames(Hmat))
        # outfunc<-LaplacesDemon::dwishart(x,nu,Hmat[matchout,matchout],log=TRUE)
        outfunc <- CholWishart::dWishart(x, nu, Hmat[matchout, matchout], 
            log = TRUE)
        
        return(outfunc)
    }
    listout <- lapply(Klist, loglik1)
    outfunc <- Reduce("+", listout)
    return(outfunc)
    
}
