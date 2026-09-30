rmaaddon <- function(params, affybatchtest) {
    
    if(!inherits(params, "rmatrain"))
      stop("Input parameter 'params' has to be of class 'rmatrain'.")
	
    rmadoc <- params$rmadoc
    sumdoc.rma <- params$sumdoc.rma
    nfeature <- params$nfeature
    
    # Perform RMA with addon quantile normalization:
    esetm.rma <- matrix(0,nrow=length(affybatchtest),ncol=nfeature)
    for(zz in 1:length(affybatchtest)){
      system.time(ab.add <- extractAffybatch(zz, affybatchtest))
      abg <- bg.correct(ab.add,'rma')
      abo.nrm.rma  <- normalizeqntadd(abg,rmadoc$mqnts)
      eset <- summarizeadd2(abo.nrm.rma,sumdoc.rma)
      esetm.rma[zz,] <- t(exprs(eset))
    }
    exp.test.add.rma <- esetm.rma     
    
    # Return the addon normalized test data:
    exp.test.add.rma
    
}
