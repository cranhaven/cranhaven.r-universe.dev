#' To re-allocate the stratum sample sizes (nh)
#'
#' This function re-calculates or re-allocate the stratum sample sizes 
#' (nh) after it has already been initially allocated via Neyman
#' allocation. This is applied to resolve the problem of oversampling 
#' in one or more of the strata.
#'
#' @param h A numeric: the no. of strata
#' @param x A vector: the osb that has been calculated
#' @param nh A vector: the stratum sample sizes that have been initially calculated
#' @param Nh A vector: the stratum population sizes that have been initially calculated
#' @param nume A numeric: the numerator total
#' @param my_env The environment my_env has various constants and outputs stored
#' from earlier opeartions through various other functions
#'
#' @return \code{} calculates and presents the new re-allocate stratum samples
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
realloc <- function(h, x, nh, Nh, nume, my_env){

Dh <- double(h)
deno.new <- 0
DhTot <- 0

for(i in 1:(length(x)-1))
   {
      #nh is less than or equal to stratum population totals
      if(nh[i] == Nh[i]){
         nh[i] <- nh[i] #no problem
      }
      #oversampling case, where nh is more than what's available in Nh
      else if(nh[i] > Nh[i]){
         Dh[i] <- nh[i]-Nh[i]# extra units, calculate difference first
         DhTot <- DhTot + Dh[i]
         nh[i] <- Nh[i] # assign all units in pop stratum to the sample stratum
      }
      else { #for the ok case: nh < Nh
         deno.new <- deno.new + nume[i]
      }
   }

   #re-alloc as per weighting of WhSh where pop units are available
   for(i in 1:(length(x)-1))
   {
      if(nh[i] < Nh[i]){
         nh[i] <- nh[i] + DhTot*(nume[i]/deno.new)#add to strata where space available
      }
   }
  my_env$nh <- nh #return samples
}
######################################################################################S