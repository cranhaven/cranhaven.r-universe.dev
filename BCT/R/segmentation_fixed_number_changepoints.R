#' @title Inferring the change-points locations when the number of change-points is fixed.
#' @description This function implements the Metropolis-Hastings sampling algorithm for inferring the locations of the change-points.
#' @param input_data the sequence to be analysed.
#' @param l number of change-points.
#' @param depth maximum memory length.
#' @param alphabet symbols appearing in the sequence.
#' @param iters number of iterations; for more information see \href{https://arxiv.org/pdf/2203.04341.pdf}{Lungu et al. (2022)}.
#' @param fileName file path for storing the results.
#' @return return a list object which includes:
#' \item{positions}{the sampled locations of the change-points.}
#' \item{acceptance_prob}{the empirical acceptance ratio.}
#' @export
#' @seealso \code{\link{infer_unknown_changepoints}}
#' @examples
#' # Use as an example the three_changes dataset.
#' # Run the function with 3 change-points, a maximum depth of 5 and the [0,1,2] alphabet.
#' # The sampler is run for 100 iterations
#' output <- infer_fixed_changepoints(three_changes, 3, 5, c("012"), 100, fileName = NULL)
#' 
#' # If the fileName is not set to NULL, 
#' # the output file will contain on each line the sampled locations of the change-points.
infer_fixed_changepoints <- function(input_data, l, depth, alphabet, iters, fileName = NULL){
  #writes the results in a file
  if(!is.null(fileName))
    fileConn<-file( paste(c(fileName, "_", depth, "fixed", ".txt"), collapse = "" ), "w" )
  N <- nchar(input_data)
  acc <- 0

  #sample the initial position vector
  p <- sample((depth + 1): (N-1), l, replace = FALSE)
  p <- sort(p)

  #p_out contains the final output
  p_out <- vector(mode = "list", length = iters)

  #at each iteration the CTW results is stored for next function pass
  ctw_set <- vector("list", 1)

  key <-paste(c("1", "_", toString(p[1])), collapse = '')
  names(ctw_set) <- key
  #calculates the CTW of the first segment
  ctw_set[key] <- CTW(substring(input_data, 1, p[1]), depth, alphabet)

  # calculates the CTW of the remaining segments
  if(l>=2){
    for(j in (2:l)){
      key <- paste(c(toString(p[j-1]+1), "_", toString(p[j])), collapse = '')
      ctw_set[[key]] <- CTW(substring(input_data, p[j-1]+1-depth, p[j]), depth, alphabet)}
  }

  # calculates the CTW of the final segment
  key <- paste(c(toString(p[l]+1), "_", toString(N)), collapse = '')
  ctw_set[[key]] <- CTW(substring(input_data, p[l]+1-depth, N), depth, alphabet)

  # the MH-algorithm
  for(iter in 1:iters){

    if(iter%%500 == 0)
      print(iter)

    # sample a changepoint position
    p_index <- sample(1:l, 1)

    # decide to whether sample uniformly or sample from the neighbours
    m <- sample(c(1,2), 1)

    # if m = 1 sample uniformly
    if(m == 1){
      q <- sample((depth + 2): (N-2), 1)
      while (q %in% p) {
        q <- sample((depth + 2): (N-2), 1)
      }

      p_new <- p
      p_new[p_index] <- q
    }

    else{
      p_new <- p

      # if the first element is depth+2, then go to depth+3 (if available) or uniformly select from the available positions
      if(p_new[p_index] == depth+2){
        if((depth+3) %in% p_new){
          q <- sample((depth + 4): (N-2), 1)
          while (q %in% p_new)
            q <- sample((depth + 4): (N-2), 1)

          p_new[p_index] = q
        }
        else
          p_new[p_index] = depth+3
      }

      # if the last element is N-1, then go to N-2 (if available) or uniformly select from the available positions
      else{
        if(p_new[p_index] == N-1){
          if((N-2) %in% p_new){
            q <- sample((depth + 2): (N-3), 1)
            while (q %in% p_new)
              q <- sample((depth + 2): (N-3), 1)

            p_new[p_index] = q
          }
          else
            p_new[p_index] = N-2
        }

        #if not, just select one of its neighbours (if one neighbour occupied, then propose uniformly a new position)
        else{
          w <- sample(c(-1,1), 1)

          q <- (p_new[p_index] + w)

          while (q %in% p_new)
            q <- sample((depth + 2): (N-1), 1)

          p_new[p_index] <- q
        }
      }
    }

    # make sure that the new vector is sorted
    p_new <- sort(p_new)
    # calculate the CTW for the entire sequence
    ctw_set_new <- .calculate_ctw_set(input_data, depth, p_new, ctw_set, alphabet)

    ppl_old <- sum(unlist(ctw_set))
    ppl_new <- sum(unlist(ctw_set_new))

    log_ratio <- (ppl_new - ppl_old)

    #calculate the log-ratio between the new proposal and the old proposal
    log_ratio <- (log_ratio + log(p_new[1] - depth-2, base = exp(1)) - log(p[1] - depth-2, base = exp(1)))

    if(l>=2){
      for (i in 2:l)
        log_ratio <- (log_ratio + log(p_new[i] - p_new[i-1]-1, base = exp(1))- log(p[i] - p[i-1]-1, base = exp(1)))
    }
    log_ratio <- (log_ratio + log(N - p_new[l]-1, base = exp(1)) - log(N - p[l]-1, base = exp(1)))

    log_alpha <- min(0, log_ratio)

    log_u <- log(stats::runif(1), base = exp(1))

    if(log_u<log_alpha){
      acc <- (acc+1)
      p <- p_new
      ctw_set <- ctw_set_new
    }

    p_out[[iter]] <- p
    if(!is.null(fileName))
      write(p, fileConn, append = TRUE)
  }
  print(acc/iters)
  if(!is.null(fileName))
    close(fileConn)
  return(list("positions" = p_out, "acceptance_prob" = acc/iters))
}
