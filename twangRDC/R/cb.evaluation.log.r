cb.evaluation.log <- function(n.keep=1) {
   
   callback <- function(env = parent.frame(), finalize = FALSE) {
      
      if ( env$iteration%%n.keep == 0){
         if (data.table::is.data.table(env$evaluation_log)){
            env$evaluation_log[ , (paste0("pscore",env$iteration)) := env$bst_evaluation ]
         }else{
            env$evaluation_log = data.table::data.table( env$bst_evaluation)
            names(env$evaluation_log) = paste0("pscore",env$iteration)
         }
      }
      if (finalize)
         return()
   }
   attr(callback, 'call') <- match.call()
   attr(callback, 'name') <- 'cb.evaluation.log'
   callback
}





