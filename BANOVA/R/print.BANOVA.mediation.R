print.BANOVA.mediation <- 
  function(x, ...){
    if (x$individual){
      cat('Indirect effect of', x$xvar,':\n')
      for (i in 1:length(x$individual_indirect)){
        table_name <- names(x$individual_indirect)[i]
        if (!is.null(table_name)){
          cat(gsub("_", " ", table_name),'\n')
        }
        print(noquote(x$individual_indirect[[i]]), row.names = F)
      }
    }else{
      cat('Indirect effect of', x$xvar,':\n')
      for (i in 1:length(x$indir_effects)){
        table_name <- names(x$indir_effects)[i]
        if (!is.null(table_name)){
          cat(gsub("_", " ", table_name),'\n')
        }
          print(noquote(x$indir_effects[[i]]), row.names = F)
          cat('effect size: ', x$effect_size[[i]], '\n')
      }
    }
  }
