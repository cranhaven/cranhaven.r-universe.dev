#' Stratification of Univariate Survey Population Using the Data
#'
#' This function takes in the univariate population data
#' (argument \code{data}) and a fixed sample size (n)
#' to compute the optimum stratum boundaries (OSB) for a
#' given number of strata (L), optimum sample sizes (nh),
#' etc. directly from the data. The main idea used is from
#' Khan et al (2008) whereby the problem of stratification
#' is formulated into a Mathematical Programming Problem (MPP)
#' using the best-fit frequency distribution and its parameters
#' estimated from the data. This MPP is then solved for the
#' OSB using a Dynamic Programming (DP) solution procedure.
#'
#' @param data A vector of values of the survey variable y for
#' which the OSB are determined
#' @param h A numeric: denotes the number of strata to be created.
#' @param n A numeric: denotes a fixed total sample size.
#' @param cost A logical: has default cost=FALSE. If it is a stratum-cost problem,
#' cost=TRUE, with which, one must provide the Ch parameter.
#' @param ch A numeric: denotes a vector of stratum costs. When cost=FALSE, it
#' has a default of NULL.
#' @export
#'
#' @return \code{strata.data} returns Optimum Strata Boundaries (OSB),
#' stratum weights (Wh), stratum variances (Vh), Optimum Sample Sizes
#' (nh), stratum population sizes (Nh) and sampling fraction (fh).
#'
#' @return \code{strata.data} returns Optimum Strata Boundaries (OSB),
#' stratum weights (Wh), stratum variances (Vh), Optimum Sample Sizes
#' (nh), stratum population sizes (Nh) and sampling fraction (fh).
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr MGM Khan <khan_mg@usp.ac.fj>
#'
#' @seealso \code{strata.distr}
#'
#' @examples
#' \dontrun{
#' data <- rweibull(1000, shape=2, scale = 1.5)
#' hist(data)
#' obj <- strata.data(data, h = 2, n=300)
#' summary(obj)
#' #-------------------------------------------------------------
#' data(quakes)
#' mag <- quakes$mag
#' hist(mag) #to see the distribution
#' res <- strata.data(mag, h = 2, n=300) # a 2-strata solution
#' summary(res)
#' #-------------------------------------------------------------
#' data(faithful) #available data in R
#' eruptions = faithful$eruptions
#' res <- strata.data(eruptions, h = 2, n=20) # a 2-strata solution
#' summary(res)
#' #-------------------------------------------------------------
#' data(sugarcane)
#' Production <- sugarcane$Production
#' hist(Production)
#' res <- strata.data(Production, h = 2, n=1000)
#' summary(res)
#' #-------------------------------------------------------------
#' #The function be dynamically used to visualize the the strata boundaries,
#' #for 2 strata, over the density (or observations) of the "mag" variable
#' #from the quakes data (with purrr and ggplot2 packages loaded).
#' output <- quakes %>%
#'           pluck("mag") %>%
#'           strata.data(h = 2, n = 300)
#' quakes %>%
#'       ggplot(aes(x = mag)) +
#'       geom_density(fill = "blue", colour = "black", alpha = 0.3) +
#'       geom_vline(xintercept = output$OSB, linetype = "dotted", color = "red")
#' #-------------------------------------------------------------
#' }
#'
strata.data <- function(data, h, n, cost=FALSE, ch=NULL)
{
   #checking if args are correct
   if (missing(data))
      stop("'data' must be specified")
   if (missing(h))
      stop("'h' must be specified")
   if (missing(n))
      stop("'n' must be specified")
   if (n > length(data))
    stop("Your 'n' cannot be greater than 'N'")

   #create a new env and put params in it, this env is used to
   #store other variables as well that are used by other functions
   my_env <- new.env(parent = emptyenv())
   my_env$h <- h
   my_env$n <- n
   N <- length(data)
   my_env$N <- N

   #consider the cost problem
   my_env$cost <- cost #a scalar

   if(cost=="TRUE"){
      if(length(ch)!=h)
         stop("Please provide the 'ch' vector with correct size - it must match size of h")
      my_env$ch <- ch #a vector of size h
   }
   else {
      my_env$ch[1:h] <- 1 #stratum unit costs are 1
   }

   #if 0's exist, replace them with very small vals
   if(any(data == 0)){
      data[data == 0] <- eps <- 1e-5 #replace 0's with eps
   }
   else{data <- data}

   #compute max value from real data & store in my_env
   my_env$maxval <- max(data)
   #scale the data
   scaled_data <- data/(my_env$maxval)

   #compute values from scaled data, store in my_env
   my_env$initval <- min(scaled_data) #x0
   my_env$finval <- max(scaled_data) #xL
   my_env$dist <- max(scaled_data)-min(scaled_data) #d

   my_env$obj <- get.dist(scaled_data, my_env)  # data is scaled in get.dist()

   #initialize some constants in the new env
   my_env$z <- 100
   my_env$factor <- 4
   my_env$inc <- 0.001
   my_env$inc2 <- 0.00001
   my_env$points <- 1000
   my_env$stages <- h+1
   my_env$ylimits <- integer(h+1)
   my_env$p <- as.integer(my_env$dist*my_env$points)
   my_env$e <- as.integer(my_env$dist*(my_env$points)*(my_env$z)+1)

   #create the matrix to store results
   create.mat(my_env)
   dk2 <- my_env$dk2

   #access the constants to be used
   z <- my_env$z
   factor <- my_env$factor
   inc <- my_env$inc
   inc2 <- my_env$inc2
   points <- my_env$points
   ylimits <- my_env$ylimits
   p <- my_env$p
   e <- my_env$e

   #create osb for base case of k=1
   my_env$ObjFV <- data.optim(k=h, n=p, incf=inc, minYk=0, maxYk=p, isFirstRun=TRUE, my_env)

   #create vectors to store results
   d <- double(h)
   y <- double(h)
   x <- double(h)

   temp <- 0

   #3dp solutions
   for(i in h:1)
   {
      if(i == h)
      {
         d[i] <- my_env$dist
         y[i] <- my_env$dk2[i+1, p+1]
         x[i] <- my_env$initval + my_env$dist
      }
      else if(i == 1)
      {
         d[i] <- d[i+1] - y[i+1]
         y[i] <- d[i]
         x[i] <- y[i] + my_env$initval
      }
      else
      {
         d[i] <- d[i+1] - y[i+1]
         temp <- as.integer(d[i]*points)
         y[i] <- my_env$dk2[i+1, temp]
         x[i] <- x[i+1] - y[i+1]
      }
   }

   for(i in h:1)
   {
      my_env$ylimits[i+1] <- as.integer(y[i]*points*z)
   }

   #create osb for base case of k=2
   my_env$ObjFV <- data.optim(k=h, n=e-1, incf=inc2,
                     minYk=my_env$ylimits[h+1]-my_env$factor*my_env$z,
                     maxYk=my_env$ylimits[h+1]+my_env$factor*my_env$z,
                     isFirstRun=FALSE, my_env)  # for k>=2
   #6dp solutions
   for(i in h:1)
   {
      if(i == h)
      {
         d[i] <- my_env$dist
         y[i] <- my_env$dk2[i+1, e]
         x[i] <- my_env$initval + my_env$dist
      }
      else if(i == 1)
      {
         d[i] <- d[i+1] - y[i+1]
         y[i] <- d[i]
         x[i] <- y[i] + my_env$initval
      }
      else
      {
         d[i] <- d[i+1] - y[i+1]
         temp <- as.integer(d[i]*points*z)
         y[i] <- my_env$dk2[i+1, temp]
         x[i] <- x[i+1] - y[i+1]
      }
   }

   #now that boudaries exist, let's organize outputs
   my_env$df <- data.frame(h, d, y, x)

   #collate all objects in order to create "strata" class
   h <- data.frame("Strata" = 1:my_env$h)
   ObjFV <- my_env$ObjFV #this is for scaled data
   OSB <- round(((my_env$maxval)*(my_env$df)$x), digits=2) #convert from scaled osb to real osb
   distr <- as.character(my_env$obj["distr"])

   # handle case when triangle is actually rtriangle
   max <- my_env$obj[["params"]]["max"]
   mode <- my_env$obj[["params"]]["mode"]
   if(distr=="triangle"){
      if(round(max, digits=1) == round(mode, digits=1)){ #if max==mode
         distr <- "rtriangle"
      }
      else{
         distr <- distr
      }
   }

   #collate outputs from data.alloc()
   data.alloc(data, my_env)
   Output <- my_env$output  #is a df with Wh, Vh & WhSh
   out <- my_env$out #is a df with nh, Nh & fh

   #get the totals
   deno <- my_env$deno #total of WhSh
   WhTot <- my_env$WhTot #tot of stratum weights
   NhTot <- my_env$NhTot #tot pop units in strata h
   nhTot <- my_env$nhTot #tot sample units in strata h
   fhTot <- my_env$fhTot #stratum sampling fraction
   VhTot <- my_env$VhTot #tot stratum variances

   #fit into real data to get get parameters and best-fit distr
   if(distr == "pareto")
   {
      fit <- suppressWarnings(try(fitdist(data, distr="pareto",
                  start = list(shape = 1, scale = 1), lower=c(0, 0)), silent=TRUE))
   }
   else if(distr == ("triangle"))
   {
      eps = 1e-8; a <- min(data); b <- max(data); c <- mode.val(data);
      fit <- suppressWarnings(try(fitdist(data, distr = "triang", method="mle", lower=c(0,0),
                  start = list(min = a-eps, max = b+eps, mode = c)), silent=TRUE))
   }
   else if(distr == ("rtriangle"))
   {
      eps = 1e-8; a <- min(data); b <- max(data); c <- mode.val(data);
      fit <- suppressWarnings(try(fitdist(data, distr = "triang", method="mle", lower=c(0,0),
                  start = list(min = a-eps, max = b+eps, mode = c)), silent=TRUE))
   }
   else #gamma onwards
   {
      fit <- suppressWarnings(try(fitdist(data, distr = distr, method="mle",
                  lower = c(0,0)), silent=TRUE)) #re-fit into real data
   }

   #construct object lists and combine into single list
   out1 <- list("cost"=cost, "distr"=distr, "fit"=fit, "n"=n, "N"=N, "ch"=ch,
                "maxval"=my_env$maxval, "initval"=my_env$initval,
                "finval"=my_env$finval, "dist"=my_env$dist) #maxval for real data while other for scaled data
   out2 <- list(h=h, OSB=OSB, Wh=Output$Wh, Vh=Output$Vh,
                WhSh=Output$WhSh, nh=out$nh, Nh=out$Nh, fh=out$fh)
   out3 <- list("WhTot"=WhTot,"VhTot"=VhTot, "WhShTot"=deno,
                "nhTot"=nhTot,"NhTot"=NhTot, "fhTot"=fhTot) #first col strata, 2nd col empty

   out <- c(out1, out2, out3)
   class(out) <- "strata" # define class for results based on data
   return(out)
}
#########################################################################
#' Stratification of Univariate Survey Population Using the Data
#'
#' This function takes in the univariate population data
#' (argument \code{data}) and a fixed sample size (n)
#' to compute the optimum stratum boundaries (OSB) for a
#' given number of strata (L), optimum sample sizes (nh),
#' etc. directly from the data. The main idea used is from
#' Khan et al (2008) whereby the problem of stratification
#' is formulated into a Mathematical Programming Problem (MPP)
#' using the best-fit frequency distribution and its parameters
#' estimated from the data. This MPP is then solved for the
#' OSB using a Dynamic Programming (DP) solution procedure.
#'
#' @param data A vector of values of the survey variable y for
#' which the OSB are determined
#' @param h A numeric: denotes the number of strata to be created.
#' @param n A numeric: denotes a fixed total sample size.
#' @param cost A logical: has default cost=FALSE. If it is a stratum-cost problem,
#' cost=TRUE, with which, one must provide the Ch parameter.
#' @param ch A numeric: denotes a vector of stratum costs. When cost=FALSE, it
#' has a default of NULL.
#' @export
#'
#' @return \code{strata.data} returns Optimum Strata Boundaries (OSB),
#' stratum weights (Wh), stratum variances (Vh), Optimum Sample Sizes
#' (nh), stratum population sizes (Nh) and sampling fraction (fh).
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr MGM Khan <khan_mg@usp.ac.fj>
#'
#' @seealso \code{strata.distr}
#'
#' @examples
#' \dontrun{
#' data <- rweibull(1000, shape=2, scale = 1.5)
#' hist(data)
#' obj <- strata.data(data, h = 2, n=300)
#' summary(obj)
#' #-------------------------------------------------------------
#' data(anaemia)
#' Iron <- anaemia$Iron
#' res <- strata.data(Iron, h = 2, n=350)
#' summary(res)
#' #-------------------------------------------------------------
#' data(SHS) #Household Spending data from stratification package
#' weight <- SHS$WEIGHT
#' hist(weight); length(weight)
#' res <- strata.data(weight, h = 2, n=500)
#' summary(res)
#' #-------------------------------------------------------------
#' data(sugarcane)
#' Production <- sugarcane$Production
#' hist(Production)
#' res <- strata.data(Production, h = 2, n=1000)
#' summary(res)
#' #-------------------------------------------------------------
#' #The function be dynamically used to visualize the the strata boundaries,
#' #for 2 strata, over the density (or observations) of the "mag" variable
#' #from the quakes data (with purrr and ggplot2 packages loaded).
#' output <- quakes %>%
#'           pluck("mag") %>%
#'           strata.data(h = 2, n = 300)
#' quakes %>%
#'       ggplot(aes(x = mag)) +
#'       geom_density(fill = "blue", colour = "black", alpha = 0.3) +
#'       geom_vline(xintercept = output$OSB, linetype = "dotted", color = "red")
#' #-------------------------------------------------------------
#' }
#'
strata.data <- function(data, h, n, cost=FALSE, ch=NULL)
{
   #checking if args are correct
   if (missing(data))
      stop("'data' must be specified")
   if (missing(h))
      stop("'h' must be specified")
   if (missing(n))
      stop("'n' must be specified")
   if (n > length(data))
    stop("Your 'n' cannot be greater than 'N'")

   #create a new env and put params in it, this env is used to
   #store other variables as well that are used by other functions
   my_env <- new.env(parent = emptyenv())
   my_env$h <- h
   my_env$n <- n
   N <- length(data)
   my_env$N <- N

   #consider the cost problem
   my_env$cost <- cost #a scalar

   if(cost=="TRUE"){
      if(length(ch)!=h)
         stop("Please provide the 'ch' vector with correct size - it must match size of h")
      my_env$ch <- ch #a vector of size h
   }
   else {
      my_env$ch[1:h] <- 1 #stratum unit costs are 1
   }

   #if 0's exist, replace them with very small vals
   if(any(data == 0)){
      data[data == 0] <- eps <- 1e-5 #replace 0's with eps
   }
   else{data <- data}

   #compute max value from real data & store in my_env
   my_env$maxval <- max(data)
   #scale the data
   scaled_data <- data/(my_env$maxval)

   #compute values from scaled data, store in my_env
   my_env$initval <- min(scaled_data) #x0
   my_env$finval <- max(scaled_data) #xL
   my_env$dist <- max(scaled_data)-min(scaled_data) #d

   my_env$obj <- get.dist(scaled_data, my_env)  # data is scaled in get.dist()

   #initialize some constants in the new env
   my_env$z <- 100
   my_env$factor <- 4
   my_env$inc <- 0.001
   my_env$inc2 <- 0.00001
   my_env$points <- 1000
   my_env$stages <- h+1
   my_env$ylimits <- integer(h+1)
   my_env$p <- as.integer(my_env$dist*my_env$points)
   my_env$e <- as.integer(my_env$dist*(my_env$points)*(my_env$z)+1)

   #create the matrix to store results
   create.mat(my_env)
   dk2 <- my_env$dk2

   #access the constants to be used
   z <- my_env$z
   factor <- my_env$factor
   inc <- my_env$inc
   inc2 <- my_env$inc2
   points <- my_env$points
   ylimits <- my_env$ylimits
   p <- my_env$p
   e <- my_env$e

   #create osb for base case of k=1
   my_env$ObjFV <- data.optim(k=h, n=p, incf=inc, minYk=0, maxYk=p, isFirstRun=TRUE, my_env)

   #create vectors to store results
   d <- double(h)
   y <- double(h)
   x <- double(h)

   temp <- 0

   #3dp solutions
   for(i in h:1)
   {
      if(i == h)
      {
         d[i] <- my_env$dist
         y[i] <- my_env$dk2[i+1, p+1]
         x[i] <- my_env$initval + my_env$dist
      }
      else if(i == 1)
      {
         d[i] <- d[i+1] - y[i+1]
         y[i] <- d[i]
         x[i] <- y[i] + my_env$initval
      }
      else
      {
         d[i] <- d[i+1] - y[i+1]
         temp <- as.integer(d[i]*points)
         y[i] <- my_env$dk2[i+1, temp]
         x[i] <- x[i+1] - y[i+1]
      }
   }

   for(i in h:1)
   {
      my_env$ylimits[i+1] <- as.integer(y[i]*points*z)
   }

   #create osb for base case of k=2
   my_env$ObjFV <- data.optim(k=h, n=e-1, incf=inc2,
                     minYk=my_env$ylimits[h+1]-my_env$factor*my_env$z,
                     maxYk=my_env$ylimits[h+1]+my_env$factor*my_env$z,
                     isFirstRun=FALSE, my_env)  # for k>=2
   #6dp solutions
   for(i in h:1)
   {
      if(i == h)
      {
         d[i] <- my_env$dist
         y[i] <- my_env$dk2[i+1, e]
         x[i] <- my_env$initval + my_env$dist
      }
      else if(i == 1)
      {
         d[i] <- d[i+1] - y[i+1]
         y[i] <- d[i]
         x[i] <- y[i] + my_env$initval
      }
      else
      {
         d[i] <- d[i+1] - y[i+1]
         temp <- as.integer(d[i]*points*z)
         y[i] <- my_env$dk2[i+1, temp]
         x[i] <- x[i+1] - y[i+1]
      }
   }

   #now that boudaries exist, let's organize outputs
   my_env$df <- data.frame(h, d, y, x)

   #collate all objects in order to create "strata" class
   h <- data.frame("Strata" = 1:my_env$h)
   ObjFV <- my_env$ObjFV #this is for scaled data
   OSB <- round(((my_env$maxval)*(my_env$df)$x), digits=2) #convert from scaled osb to real osb
   distr <- as.character(my_env$obj["distr"])

   # handle case when triangle is actually rtriangle
   max <- my_env$obj[["params"]]["max"]
   mode <- my_env$obj[["params"]]["mode"]
   if(distr=="triangle"){
      if(round(max, digits=1) == round(mode, digits=1)){ #if max==mode
         distr <- "rtriangle"
      }
      else{
         distr <- distr
      }
   }

   #collate outputs from data.alloc()
   data.alloc(data, my_env)
   Output <- my_env$output  #is a df with Wh, Vh & WhSh
   out <- my_env$out #is a df with nh, Nh & fh

   #get the totals
   deno <- my_env$deno #total of WhSh
   WhTot <- my_env$WhTot #tot of stratum weights
   NhTot <- my_env$NhTot #tot pop units in strata h
   nhTot <- my_env$nhTot #tot sample units in strata h
   fhTot <- my_env$fhTot #stratum sampling fraction
   VhTot <- my_env$VhTot #tot stratum variances

   #fit into real data to get get parameters and best-fit distr
   if(distr == "pareto")
   {
      fit <- suppressWarnings(try(fitdist(data, distr="pareto",
                  start = list(shape = 1, scale = 1), lower=c(0, 0)), silent=TRUE))
   }
   else if(distr == ("triangle"))
   {
      eps = 1e-8; a <- min(data); b <- max(data); c <- mode.val(data);
      fit <- suppressWarnings(try(fitdist(data, distr = "triang", method="mle", lower=c(0,0),
                  start = list(min = a-eps, max = b+eps, mode = c)), silent=TRUE))
   }
   else if(distr == ("rtriangle"))
   {
      eps = 1e-8; a <- min(data); b <- max(data); c <- mode.val(data);
      fit <- suppressWarnings(try(fitdist(data, distr = "triang", method="mle", lower=c(0,0),
                  start = list(min = a-eps, max = b+eps, mode = c)), silent=TRUE))
   }
   else #gamma onwards
   {
      fit <- suppressWarnings(try(fitdist(data, distr = distr, method="mle",
                  lower = c(0,0)), silent=TRUE)) #re-fit into real data
   }

   #construct object lists and combine into single list
   out1 <- list("cost"=cost, "distr"=distr, "fit"=fit, "n"=n, "N"=N, "ch"=ch,
                "maxval"=my_env$maxval, "initval"=my_env$initval,
                "finval"=my_env$finval, "dist"=my_env$dist) #maxval for real data while other for scaled data
   out2 <- list(h=h, OSB=OSB, Wh=Output$Wh, Vh=Output$Vh,
                WhSh=Output$WhSh, nh=out$nh, Nh=out$Nh, fh=out$fh)
   out3 <- list("WhTot"=WhTot,"VhTot"=VhTot, "WhShTot"=deno,
                "nhTot"=nhTot,"NhTot"=NhTot, "fhTot"=fhTot) #first col strata, 2nd col empty

   out <- c(out1, out2, out3)
   class(out) <- "strata" # define class for results based on data
   return(out)
}
#########################################################################
