#' This method formats and outputs the final results to the R console
#'
#' This function defines the method for the "strata" class that has been created 
#' in the constructor function (strata.data() or strata.distr()) where all computed 
#' objects, via other relevant functions, are collated and passed as a list. The 
#' function extracts all individual objects from the "strata" class object and combines 
#' them into dataframes before writing the formatted outputs to the console. This 
#' is used for both cases, depending on either the data or a hypothetical distribution.
#' 
#' @param object A list: An object of class "strata".
#' @param ... Any data type: This argument can be any particular argument.
#' @export
#' 
#' @return \code{} returns the formatted output
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr MGM Khan <khan_mg@usp.ac.fj>
#'
#' @examples
#' \dontrun{
#' data <- rweibull(1000, shape=2, scale = 1.5)
#' res <- strata.data(data, h = 2, n=300)
#' summary(res)
#' }
#'
summary.strata <- function(object, ...)
   {
   #extract results from 'object' from "strata" class
   cost <- object$cost; distr <- object$distr; fit <- object$fit; 
   n <- object$n; N <- object$N; ch <- object$ch; maxval <- object$maxval; 
   initval <- object$initval; finval <- object$finval; dist <- object$dist; 
   h <- nrow(object$h)
   
   # writing the outputs to console
   cat("_____________________________________________\n")
   cat("Optimum Strata Boundaries for h =",h, "\n", sep = " ")
   cat("Data Range: [",(maxval*initval),", ",
       (maxval*finval),"]", " with d = ",
       (maxval*dist), "\n", sep = "")
   
   if(cost==TRUE)
   {
      cat("Stratum Costs: Ch = [",paste(ch, collapse=", "),"]", "\n", sep = "")
   }
   
   cat("Best-fit Frequency Distribution: ", paste(distr), "\n", sep = " ")
   cat("Parameter estimate(s): ", "\n")
   
   print(fit$estimate) #params of original data
   
   #Now lets get the actual results
   #extract results from 'object', convert to df
   tab2 <- data.frame(object$h, object$OSB, object$Wh, object$Vh, 
                     object$WhSh, object$nh, object$Nh, object$fh)
   colnames(tab2) <- c("Strata", "OSB", "Wh", "Vh", "WhSh", "nh", "Nh", "fh")
   
   #get totals as well
   tab3 <- data.frame("Total", "", object$WhTot, object$VhTot, object$WhShTot, 
                      object$nhTot, object$NhTot, object$fhTot)
   colnames(tab3) <- c("Strata", "OSB", "Wh", "Vh", "WhSh", "nh", "Nh", "fh") #for rbind to happen
   
   #rowbind the totals at the bottom
   tab <- rbind(tab2, tab3)
   #colnames(tab) <- c("Strata", "OSB", "Wh", "Vh", "WhSh", "nh", "Nh", "fh")

   cat("____________________________________________________\n")
   print(tab, row.names = FALSE)
   cat("____________________________________________________\n")
}
