#' Balance tables for `twangRDC`
#'
#' `bal.table` calculates balance tables from a `ps.xgb` object
#' 
#' @param x A `ps.xgb` object.
#' @param type An optional character string requesting if balance should be summarized overall ("overall") or by strata ("strata"). Default: "overall".
#' @param n An integer specifying the number of rows to print in the balance table. Default: 10.
#' @param decreasing A logical value indicating if the balance table should be sorted in increasing or decreasing order.
#' @param which.sort An optional character string indicating if the balance table 
#'   should be sorted by the adjusted ("adj") or unadjusted ("unadj") absolute standardized differences. Default: "adj".
#' @param include.var A logical value indicating if the variable corresponding the the maximum absolute standardized difference 
#'   within strata should be included in the balance table. Only valid when `strata=TRUE`. Default: FALSE.
#'   
#' @return Returns a table.
#' 
#' @examples 
#' # See vignette for examples.
#'   
#' @export
bal.table = function(x , type="overall" , n=10 , decreasing=TRUE , which.sort="adj" , include.var=FALSE){
   if (type=="overall"){
      out = x$bal.tab
   }
   if (type=="strata"){
      keep = 1:min(ncol(x$es.max) , n)
      
      out = t(x$es.max[c(1,x$opt.iter/x$iters.per.step+1),,drop=F])
      
      if (include.var){
         temp = cbind(lapply(x$es , function(y) colnames(y)[which.max(abs(y))]))
         out = merge(x=out , y=temp , by=0)
         colnames(out) = c("Stratum","Unadjusted Maximum Standardized Difference","Adjusted Maximum Standardized Difference","Variable")
      }else{
         out <- cbind(rownames(out), data.frame(out, row.names=NULL))
         colnames(out) = c("Stratum","Unadjusted Maximum Standardized Difference","Adjusted Maximum Standardized Difference")
      }
      
      if (which.sort=="unadj"){
         out = out[order(abs(out[,"Unadjusted Maximum Standardized Difference"]) , decreasing=decreasing)[keep],,drop=F]
      }
      if (which.sort=="adj"){
         out = out[order(abs(out[,"Adjusted Maximum Standardized Difference"]) , decreasing=decreasing)[keep],,drop=F]
      }
   }
   return(out)
}