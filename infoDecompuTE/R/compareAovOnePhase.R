

compareAovOnePhase <- 
  function(design.df, design1.df, blk.str, trt.str, var.comp = NA, trt.contr = NA, 
           table.legend = FALSE, response = NA, latex = FALSE, 
          fixed.names = NA, decimal = FALSE, digits = 2, 
           list.sep = TRUE){
  
  
  
    noquote(
      cbind(Des1 = summaryAovOnePhase(design.df, blk.str = blk.str, trt.str = trt.str)$ANOVA[,1], 
            Des2 = summaryAovOnePhase(design1.df, blk.str = blk.str, trt.str = trt.str)$ANOVA[,1]))
  
}

compareAovTwoPhase <- 
  function(design.df, design1.df, blk.str1, blk.str2, trt.str, var.comp = NA, trt.contr = NA, 
           table.legend = FALSE, response = NA, latex = FALSE, 
           fixed.names = NA, decimal = FALSE, digits = 2, 
           list.sep = TRUE){
    
    noquote(cbind(
      Des1 = summaryAovTwoPhase(
        design.df,
        blk.str1 = blk.str1,
        blk.str2 = blk.str2,
        trt.str = trt.str
      )$ANOVA[, 1],
      Des2 = summaryAovTwoPhase(
        design1.df,
        blk.str1 = blk.str1,
        blk.str2 = blk.str2,
        trt.str = trt.str
      )$ANOVA[, 1]
    ))
    
  }
