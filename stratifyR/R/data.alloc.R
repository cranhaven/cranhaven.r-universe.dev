#' To calculate the stratum sample sizes (nh) for a fixed sample size (n) 
#' directly based on the data
#'
#' This function is called towards the final stages of the stratification process
#' after OSB have been determined. It uses the boundaries to calculate the stratum 
#' sample allocations using Neyman allocation for all individual strata using the 
#' raw data.
#'
#' @param data A vector: provided as an input to the function
#' @param my_env The environment my_env has various constants and outputs stored
#' from earlier operations through various other functions
#'
#' @return \code{} calculates and stores quantities such as nh, Nh, Vh, etc.
#' in the my_env to be accessed and printed as outputs
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
data.alloc <- function(data, my_env)
{
  h <- my_env$h
  initval <- my_env$initval*my_env$maxval #in real data
  n <- my_env$n
  x <- c(initval, ((my_env$df)$x)*(my_env$maxval)) #take osb & append to initval

  ch <- my_env$ch #a vector of stratum sample costs
  
  distr <- as.character(my_env$obj["distr"]) #best-fit distr of scaled data

  var <- double(h)
  sigsq <- double(h)
  Wh <- double(h)
  nume <- double(h)
  nh <- double(h)
  Nh <- double(h)
  fh <- double(h)
  deno <- 0

  #get Wh, Sh, etc from data after OSB have been obtained
  data <- sort(data)

  for(i in 1:(length(x)-1))
  {
    Nh[i] = length(data[data >= x[i] & data <= x[i+1]])
    Wh[i] = Nh[i]/length(data)
    var[i] = try(var(data[data >= x[i] & data <= x[i+1]]), silent=TRUE)
    nume[i] <- (Wh[i]*sqrt(var[i]))*sqrt(ch[i])
    deno <- deno + nume[i]

    my_env$output <- data.frame("Wh" = round(Wh, digits=2),
                     "Vh" = round(var, digits=2), "WhSh" = round(nume, digits=3))
    my_env$Nh <- Nh
  }

  for(i in 1:(length(x)-1))
  {
    nh[i] <- n*nume[i]/deno #initial samples via Neyman allocation
  }

  realloc(h, x, nh, Nh, nume, my_env)
  nh <- my_env$nh #get re-allocated samples

  #check again
  for(i in 1:(length(x)-1)){
     if(nh[i] > Nh[i]){
        realloc(h, x, nh, Nh, nume, my_env)
        nh <- my_env$nh
     }
     else{ #<= case
      nh <- my_env$nh}
  }

  fh <- round((nh/Nh), digits=2) #stratum sampling fractions
  my_env$out <- data.frame("nh"=round(nh), "Nh"=Nh, "fh"=fh) #passed to data.res()

  #get some totals
  my_env$deno <- round(deno, digits=3)
  my_env$WhTot <- round(sum(Wh), digits=0)
  my_env$NhTot <- sum(Nh)
  my_env$nhTot <- round(sum(nh))
  my_env$VhTot <- round(sum(var), digits=2)
  my_env$fhTot <- round((my_env$nhTot/my_env$NhTot), digits=2)
}
###################################################################################