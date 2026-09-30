normalizeAffyBatchqntval <- 
function (abatch, type = c("separate", "pmonly", "mmonly", "together")) 
    {
      type <- match.arg(type)
      if ((type == "pmonly") | (type == "separate")) {
        pms  <- unlist(pmindex(abatch))
        noNA <- rowSums(is.na(affy::intensity(abatch)[pms, , drop = FALSE])) == 
          0
        pms <- pms[noNA]         
        # intensity(abatch)[pms, ] <- normalize.quantiles(intensity(abatch)[pms, 
        #     , drop = FALSE], copy = FALSE)
        tmp = normalizeqntval(affy::intensity(abatch)[pms,,drop=FALSE])
        affy::intensity(abatch)[pms, ] = tmp$x
        Biobase::description(abatch)@preprocessing = c(Biobase::description(abatch)@preprocessing,
                                              list(val=list(method="rma",mqnts=tmp$mqnts))) 
      }
      if ((type == "mmonly") | (type == "separate")) {
        mms <- unlist(mmindex(abatch))
        noNA <- rowSums(is.na(intensity(abatch)[mms, , drop = FALSE])) == 
          0
        mms <- mms[noNA]
        #intensity(abatch)[mms, ] <- normalize.quantiles(intensity(abatch)[mms, 
        #    , drop = FALSE], copy = FALSE)
        tmp = normalizeqntval(affy::intensity(abatch)[mms,,drop=FALSE])
        affy::intensity(abatch)[pms, ] = tmp$x
        Biobase::description(abatch)@preprocessing = c(Biobase::description(abatch)@preprocessing,
                                              list(val=list(method="rma",mqnts=tmp$mqnts))) 
      }
      if (type == "together") {
        pms <- unlist(indexProbes(abatch, "both"))
        #intensity(abatch)[pms, ] <- normalize.quantiles(intensity(abatch)[pms, 
        #    , drop = FALSE], copy = FALSE)
        tmp = normalizeqntval(affy::intensity(abatch)[mms,,drop=FALSE])
        affy::intensity(abatch)[pms, ] = tmp$x
        Biobase::description(abatch)@preprocessing = c(Biobase::description(abatch)@preprocessing,
                                              list(val=list(method="rma",mqnts=tmp$mqnts))) 
      }
      
      return(abatch)
}
