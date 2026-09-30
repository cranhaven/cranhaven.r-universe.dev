summarizeval2 <- 
function(abo){
      ###############################
      
      
      expr.set <- abo
      plmset <- affyPLM::rmaPLM(abo, background = FALSE, normalize = FALSE)
      Biobase::exprs(expr.set) = affyPLM::coefs(plmset)
      expr.set@experimentData@preprocessing$scale <- "log2"
      expr.set@experimentData@preprocessing$rmaPLM.settings <- plmset@model.description$modelsettings
      probe.effects  = affyPLM::coefs.probe(plmset) ##=== safe pars
      expr.set@experimentData@preprocessing$val$probe.effects = probe.effects
      x<-plmset
      pm.index <- unique(unlist(affy::indexProbes(x, "pm",row.names(affyPLM::coefs(x)))))
      rows <- x@nrow
      cols <- x@ncol
      pm.x.locs <- pm.index%%rows
      pm.x.locs[pm.x.locs == 0] <- rows
      pm.y.locs <- pm.index%/%rows + 1
      xycoor <- matrix(cbind(pm.x.locs,pm.y.locs),ncol=2)
      xycoor2 <- matrix(cbind(pm.x.locs,pm.y.locs+1),ncol=2)
      
      #type<-attributes(type)
      
      #if (is.element(type,c("weights"))){
      #  if (any(dim(x@weights[[1]]) ==0) & any(dim(x@weights[[2]]) ==0)){
      #    stop("Sorry this PLMset does not appear to have weights\n");
      #  }
      #  if (which == 0){
      #    which <- 1:max(dim(x@weights[[1]])[2], dim(x@weights[[2]])[2])
      #  }
      #}
      
      
      return(expr.set)
}
