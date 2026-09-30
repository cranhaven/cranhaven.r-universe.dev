summarizeadd2 <- 
function(abo,probe.effects){
    #############################################
    
    inds.all = indexProbes(abo,which="pm")
    if(dim(Biobase::exprs(abo))[2] != 1) stop("\n error: to many chips \n\n")
    #########if(sum( names(inds.all) == names(probe.effects)) != length(probe.effects)) stop("\n error:
    #########	names of the probe effects do not match the abo \n\n")
    if(sum( names(inds.all) == names(probe.effects)) != length(probe.effects)) {
      reorderind <- as.numeric(factor(names(probe.effects), levels=names(inds.all)))
      inds.all <- inds.all[reorderind]
      if(sum( names(inds.all) == names(probe.effects)) != length(probe.effects))
        stop("\n error: names of the probe effects do not match the abo \n\n")
    }
    
    inds = unlist(inds.all)
    
    ##=== subtract the probe effects 
    Biobase::exprs(abo)        =  log2(Biobase::exprs(abo))   # rma on log2 scale
    Biobase::exprs(abo)[inds,] =  Biobase::exprs(abo)[inds,]-unlist(probe.effects)
    ##=== calculate probe-set specific median as chip effect and residuals
    chipfu            =  function(x) median(Biobase::exprs(abo)[x,])
    resfu             =  function(x) Biobase::exprs(abo)[x,] - median(Biobase::exprs(abo)[x,])
    chip.effects      =  unlist(lapply(inds.all,chipfu))
    residuals         =  unlist(lapply(inds.all,resfu ))
    
    eset = abo
    Biobase::exprs(eset) = as.matrix(chip.effects)
    eset@experimentData@preprocessing$val= list(residuals=residuals)  
    return(eset)
}
