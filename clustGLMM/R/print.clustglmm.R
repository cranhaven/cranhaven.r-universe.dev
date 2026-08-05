print.clustglmm <- function(x, which = "call", ...){
  stopifnot(length(which) >= 1,
            is.character(which),
            all(which %in% c("call", "clustering")))
  if(is.element("call", which)){
    cat(x$call)
  }
  
  if(is.element("clustering", which)){
    cat("\n## Clustering based on sampled indicators:\n")
    for(ch in x$chains){
      cat("-----------------------------------------------------------------------\n")
      cat(paste0("Chain ", ch, "\n"))
      cat("-----------------------------------------------------------------------\n")
      if(x$G[ch] > 1){
        cat(paste0("The most-frequent number of non-empty components: ", x$modeGplus[ch], "\n"))
        cat(paste0("Empty components: ", paste0(setdiff(1:x$G[ch], x$clusters[[ch]]), collapse = ", "), "\n"))
        cat(paste0("Non-empty components: ", paste0(x$clusters[[ch]], collapse = ", "), "\n"))
        print(summary(factor(x$clustering[,ch])))
        cat(paste0("Average certainty of assignment to a component: \n"))
        print(tapply(x$certainty[,ch], x$clustering[,ch], mean))
      }else{
        cat("This chain has G = 1.\n") 
        cat("No latent partition into clusters has been estimated.\n")
      }
    }
  }
}
