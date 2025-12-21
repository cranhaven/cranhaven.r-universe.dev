# This function is used in the infer_fixed_changepoints and the infer_unknown_changepoints functions.
# This function can be used to replace one position with a new one.
# The replacement can be either taken uniformly from the available positions or one of the neighbours is chosen uniformly;
# If one of the neighbours is already taken, choose uniformly from the available positions.

.equal_l <- function(p, l, N, depth){

   p_index <- sample(1:l, 1)

  # model 1 or model 2. If model 2 choose randomly from the available positions
  # if model 1, then increase or decrease by 1 the position
  m <- sample(c(1,2), 1)
  if(m == 2){
    q <- sample((depth + 3): (N-3), 1)
    # make sure we don't have duplicates
    while (q %in% p) {
      q <- sample((depth + 3): (N-3), 1)
    }
    p_new <- p
    p_new[p_index] <- q
  }


  # this is model 1, in which we explore closer points
  else{
    w <- sample(c(-1,1), 1)
    p_new <- p
    q <- (p[p_index] + w)

    if(p[p_index] == (depth+2)){
      if ((2*depth+3) %in% p){
        q <- sample((depth + 3): (N-3), 1)
        while (q %in% p) {
          q <- sample((depth + 3): (N-3), 1)
        }
      }

      else
        q <- (2*depth+3)
    }

    if(p[p_index] == (N-2)){
      if ((N-3) %in% p){
        q <- sample((depth + 3): (N-3), 1)
        while (q %in% p) {
          q <- sample((depth + 3): (N-3), 1)
        }
      }

      else
        q <- (N-3)
    }


    while (q %in% p) {
      q <- (q+w)
    }
    p_new[p_index] <- q
  }
  p_new <- sort(p_new)

  return(p_new)
}


.calculate_ctw_set <- function(input_data, depth, p_new, ctw_set, alphabet){

  N <- nchar(input_data)

  if(p_new[1] == 0 ){
    key <- paste(c("1", "_", toString(N)), collapse = '')

    ctw_set_new <- vector("list", 1)
    names(ctw_set_new) <- key

    ctw_set_new[[key]] <- CTW(input_data, depth, alphabet)
  }

  else{

    l_new <- length(p_new)

    key <- paste(c("1", "_", toString(p_new[1])), collapse = '')

    ctw_set_new <- vector("list", 1)
    names(ctw_set_new) <- key

    if(key %in% names(ctw_set))
      ctw_set_new[[key]] <- ctw_set[[key]]
    else
      ctw_set_new[[key]] <- CTW(substr(input_data,1,p_new[1]-1), depth, alphabet)


    if(l_new >= 2){
      for(j in (2:l_new)){

        key <- paste(c(toString(p_new[[j-1]]), "_", toString(p_new[j]-1)), collapse = '')
        if(key %in% names(ctw_set))
          ctw_set_new[[key]] <- ctw_set[[key]]
        else
          ctw_set_new[[key]] <- CTW(substring(input_data, p_new[[j-1]]-depth, p_new[j]-1), depth, alphabet)
      }
    }

    key <- paste(c(toString(p_new[[l_new]]), "_", toString(N)), collapse = '')
    if(key %in% names(ctw_set))
      ctw_set_new[[key]] <- ctw_set[[key]]
    else
      ctw_set_new[[key]] <- CTW(substring(input_data, p_new[[l_new]]-depth, N), depth, alphabet)
  }
  return(ctw_set_new)
}




#' @title Plot empirical conditional posterior of the number of change-points.
#' @description This function plots the conditional posterior distribution of the change-points locations given a specific number of change-points.
#' @param res the output obtained from the Metropolis-Hastings algorithms (either from infer_fixed_changepoints or infer_unknown_changepoints).
#' @param burn the proportion of the samples discarded as burn-in.
#' @param pm the desired range around the MAP location for each change-point location.
#' @param l condition on the number of change-points. If not initialised, the function expects as input the results obtained from the infer_fixed_changepoints function. 
#' @return plots of the empirical posterior distributions of the change-points given a specific number of change-points.
#' @export
#' @seealso \code{\link{infer_fixed_changepoints}}, \code{\link{infer_unknown_changepoints}}
#'
#' @examples
#' # Use as an example the el_nino dataset.
#' # Run the function with l_max = 3 change-points, a maximum depth of 5 and the [0, 1] alphabet.
#' # The sampler is run for 10000 iterations.
#' 
#' res_unknown <- infer_unknown_changepoints(el_nino, 3, 5, c("01"), 100, fileName = NULL)
#'
#' # Because l_max = 3 , there can be 0, 1, 2 or 3 changes.
#' # Let's see the posterior distribution on the number of changes
#' 
#'  plot_changepoint_posterior(res_unknown, 0.2)
#' 
#' # The MAP l is 2. Let's see the distribution of changes given l = 2.
#'
#' plot_individual_changepoint_posterior(res_unknown, 0.2, 20, 2)
#' 
#' # One can also see the distribution of changes given l = 1. 
#' 
#' plot_individual_changepoint_posterior(res_unknown, 0.2, 500, 1)
#' 
#' # This function can be also used with the infer_fixed_changepoints
#' # Assume l = 2.
#' 
#' res_fixed <- infer_fixed_changepoints(el_nino, 2, 5, c("01"), 100, fileName = NULL)
#' 
#' # The function is now called without l = 2 as the number of changes is fixed 
#' # (all sampled vectors have 2 values). 
#' 
#' plot_individual_changepoint_posterior(res_fixed, 0.2, 20)
plot_individual_changepoint_posterior <- function(res, burn, pm, l = NULL){
  graphics.off()
  desired_l <- l
  N <- length(res$positions)

  # get the positions of the changes
  if(!is.null(desired_l)){
    variable_l <- unlist(res$number_changes[floor(burn*N):N])
    map_l <- desired_l
  }
  else{
    variable_l <- integer(N-floor(burn*N)+1) + length(res$positions[[1]])
    map_l <- length(res$positions[[1]])
  }
  # discard the initial samples as burn-in
  positions <- res$positions[floor(burn*N):N]
  number_bins <-  max(variable_l) -min(variable_l)
  

  if(map_l != 0){
    # find the MAP positions for each change-point
    count <- 1
    if(!is.null(desired_l)){
      a <- table(variable_l)
      map_positions <- vector(mode = "list", length = as.integer(a[names(a) == desired_l]))
         }
    
    else
      map_positions <-positions
    
    for (i in (1:length(variable_l))){

      if(variable_l[i] == map_l){
        map_positions[count]<-positions[i]
        count<-(count+1)
      }
    }

    for (i in (1:map_l)){

      # for each change-point find the MAP position and draw a histogram around the MAP position
      pos <-sapply(map_positions, "[", i)
      max_pos <- as.integer(names(sort(table(pos), decreasing = T)[1]))
      pos<- pos[(pos>max_pos-pm)&(pos<max_pos+pm)]

      x <- graphics::hist(pos, breaks = 0.5*pm, plot = FALSE)
      # again, work with frequencies
      x$counts<-x$counts/sum(x$counts)
      
      if(Sys.getenv("RSTUDIO")!="1") # if the user uses RStudio or R
        dev.new()
      plot(x, xlab = "Location", ylab = "Frequency", col = "grey83", main = paste(c("Position ", i), collapse = ''))
    }
  }
}



#' @title Plot the empirical posterior distribution of the change-points.
#' @description This function plots the empirical posterior distribution of the change-points.
#' @param res the output obtained from the Metropolis-Hastings algorithms.
#' @param burn the proportion of the samples discarded as burn-in.
#' @return returns plot of the empirical posterior of the number of change-points (if the results from the infer_unknown_changepoints function were used).
#' @return returns plot of the empirical posterior of the change-points.
#' @export
#' @seealso \code{\link{infer_unknown_changepoints}}, \code{\link{infer_fixed_changepoints}}
#'
#' @examples
#' # Use as an example the el_nino dataset.
#' # Run the function with l_max = 3 change-points, a maximum depth of 5 and the [0, 1] alphabet.
#' # The sampler is run for 100 iterations
#' 
#' res_unknown <- infer_unknown_changepoints(el_nino, 3, 5, c("01"), 100, fileName = NULL)
#'
#' # Plot the posterior distribution of the locations and the posterior of the number of change-points.
#' 
#' plot_changepoint_posterior(res_unknown, 0.2)
#' 
#' # This function can be also used with the infer_fixed_changepoints.
#' # Assume l = 2.
#' 
#' res_fixed <- infer_fixed_changepoints(el_nino, 2, 5, c("01"), 100, fileName = NULL)
#' 
#' # Now, the function will only output the posterior distribution of the change-points 
#' # (the number is fixed).
#' 
#' plot_changepoint_posterior(res_fixed, 0.2)
plot_changepoint_posterior <- function(res, burn){
  graphics.off()
  N <- length(unlist(res$positions))
  res_burn <- unlist(res$positions)[floor(burn*N):N]

  # work with frequency
  x <- graphics::hist(res_burn, breaks = N, plot = FALSE)
  x$counts<-x$counts/sum(x$counts)
  plot(x, main = "", xlab = "Location", ylab = "MCMC log-posterior", col = "grey83")
  
  if("number_changes" %in% names(res)){
    # plot the posterior over the number of change-points
    N <- length(res$number_changes)
    variable_l <- unlist(res$number_changes[floor(burn*N):N])
    
    x <- graphics::hist(variable_l, plot = FALSE)
    
    # work with frequencies
    x$counts<-x$counts/sum(x$counts)
    if(Sys.getenv("RSTUDIO")!="1") # if the user uses RStudio or R
      dev.new()
    plot(x, xlab = "Location", ylab = "Frequency", col = "grey83", main = "Number of change-points")
  }
}




#' @title Calculates the exact posterior for a sequence with a single change-point.
#' @description This function calculates the exact posterior for a sequence with a single change-point.
#' @param input_data the sequence to be analysed.
#' @param depth maximum memory length.
#' @param alphabet symbols appearing in the sequence.
#' @return empirical posterior of the change-points locations.
#' @export
#' @seealso  \code{\link{infer_unknown_changepoints}}
#'
#' @examples
#' # Use the first 300 samples of the simian_40 dataset.
#' # Run the function with 1 change-point, a maximum depth of 2 and the ["a", "c", "g", "t"] alphabet.
#' 
#' res <- calculate_exact_changepoint_posterior(substr(simian_40, 1, 300), 2, c("acgt"))
calculate_exact_changepoint_posterior <- function(input_data, depth, alphabet){

  N <- nchar(input_data)

  out <- vector(mode = "list", length = (N-depth))

  # apply the CTW for the entire sequence (there are always two segments)
  for(j in ((depth+1):(N))){

    x<- (CTW(substring(input_data, 1, j), depth, alphabet) + CTW(substring(input_data, j+1-depth, N), depth, alphabet))
    x <- (x + log(j-depth+1) + log(N-j+1))
    out[j - depth] <- x
  }
  out <- c(integer(depth), out)
  return(out)
}
