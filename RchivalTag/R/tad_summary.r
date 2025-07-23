.tad_summary <- function(tad.df, vars=c(), bin_prefix='tad.'){
  
  if(nrow(tad.df) == 0) stop("empty data frame provided! please revise!")
  treat.vars <- names(tad.df)[grep(bin_prefix,names(tad.df))]
  tad.df$datetime <- c()
  op <- plyr::ddply(tad.df,c(vars),function(x){
    output <- sd.list <- list()
    for(y in treat.vars) {
      output[[y]] <- mean(x[[y]],na.rm=T)
      #       sd.list[[y]] <- sd(x[[y]],na.rm=T)    
    }
    c(unlist(output),nrec=nrow(x))
  })
  
  op2 <- plyr::ddply(tad.df,c(vars),function(x){
    output <- sd.list <- list()
    for(y in treat.vars) {
      sd.list[[y]] <- sd(x[[y]],na.rm=T)    
    }
    c(unlist(sd.list),nrec=nrow(x))
  })
  
  op3 <- plyr::ddply(tad.df,c(vars),function(x){
    output <- se.list <- list()
    for(y in treat.vars) {
      se.list[[y]] <- sd(x[[y]],na.rm=T)/sqrt(length(x[[y]]))   
    }
    c(unlist(se.list),nrec=nrow(x))
  })
  
  op$info <- 'mean'
  op2$info <- 'sd'
  op3$info <- 'se'
  op <- rbind(op,op2,op3)
  
  if(length(vars) == 0) op[,1] <- c()
  
  return(op)
}
