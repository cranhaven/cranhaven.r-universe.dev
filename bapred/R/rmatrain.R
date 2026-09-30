rmatrain <- 
function(affybatchtrain){
    
    # Perform RMA:
    abg <- bg.correct(affybatchtrain,'rma')
    a.nrm.rma <- normalizeAffyBatchqntval(abg,'pmonly')
    
    # Store parameters for addon normalization:
    rmadoc <- Biobase::experimentData(a.nrm.rma)@preprocessing[['val']]
    summ.rma <- summarizeval2(a.nrm.rma)
    sumdoc.rma <- Biobase::experimentData(summ.rma)@preprocessing$val$probe.effects
    
    # Extract gene expressions:    
    exp.train.rma <- exprs(summ.rma)
    
	params <- list(xnorm=t(exp.train.rma), rmadoc=rmadoc, sumdoc.rma=sumdoc.rma, nfeature=nrow(exp.train.rma))
    
	class(params) <- "rmatrain"
   
    return(params)
    
  }
