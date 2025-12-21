#' @title Inferring the number of change-points and their locations.
#' @description This function implements the Metropolis-Hastings sampling algorithm for inferring the number of change-points and their locations.
#' @param input_data the sequence to be analysed.
#' @param l_max maximum number of change-points.
#' @param depth maximum memory length.
#' @param alphabet symbols appearing in the sequence.
#' @param iters number of iterations; for more information see \href{https://arxiv.org/pdf/2203.04341.pdf}{Lungu et al. (2022)}.
#' @param fileName file path for storing the results.
#' @return return a list object which includes:
#' \item{number_changes}{sampled number of change-points.}
#' \item{positions}{sampled locations of the change-points.}
#' \item{acceptance_prob}{the empirical acceptance ratio.}
#' @export
#' @seealso \code{\link{infer_fixed_changepoints}}
#' @examples
#' # Use as an example the three_changes dataset.
#' # Run the function with 5 change-points, a maximum depth of 5 and the [0,1,2] alphabet.
#' # The sampler is run for 100 iterations
#' output <- infer_unknown_changepoints(three_changes, 5, 5, c("012"), 100, fileName = NULL)
#' 
#' # If the fileName is not set to NULL, 
#' # the output file will contain on each line the sampled number of change-points 
#' # and the associated sampled locations of the change-points.
infer_unknown_changepoints <- function(input_data, l_max, depth, alphabet, iters, fileName=NULL){

  #writes the results in a file
  if(!is.null(fileName))
    fileConn<-file(paste(c(fileName, "_", l_max, "_", depth, ".txt"), collapse = "" ), "w" )

  N <- nchar(input_data)
  acc <- 0

  #sample the initial number of changes and their positions
  l <- sample(0:l_max, 1)

  #p_out contains the positions of the samples
  p_out <- vector(mode = "list", length = iters)

  #l_out contains the number of changes
  l_out <- vector(mode = "list", length = iters)

  ctw_set <- vector("list", 1)

  # if l = 0 just calculate the CTW for the entire sequence
  if(l == 0){
    names(ctw_set) <- paste(c("1", "_", toString(N)), collapse = '')
    ctw_set[[paste(c("1", "_", toString(N)), collapse = '')]] <- CTW(input_data, depth, alphabet)
    p <- 0
  }

  else{

    # initial sample of the positions
    p <- sample((depth + 3): (N-2), l, replace = FALSE)
    p <- sort(p)

    # at each iteration the CTW results is stored for next function pass
    names(ctw_set) <- paste(c("1", "_", toString(p[1])), collapse = '')
    ctw_set[[paste(c("1", "_", toString(p[1])), collapse = '')]] <- CTW(substring(input_data, 1, p[1]-1), depth, alphabet)

    # calculates the CTW of the remaining segments
    if(l>=2){
      for(j in (2:l)){
        key <- paste(c(toString(p[j-1]), "_", toString(p[j]-1)), collapse = '')
        ctw_set[[key]] <- CTW(substring(input_data, p[j-1]-depth, p[j]-1), depth, alphabet)}

    }
    # calculates the CTW of the final segment
    key <- paste(c(toString(p[l]), "_", toString(N)), collapse = '')
    ctw_set[[key]] <- CTW(substring(input_data, p[l]-depth, N), depth, alphabet)
  }

  from_m <-0
  counter <- 0

  # the MH-algorithm
  for(iter in 1:iters){


    if(iter%%500 == 0)
      print(iter)

    # sample a new number of change-points and their locations based on the number
    # if l = 0, l_new = 1
    if(l == 0){
      l_new <- 1
      p_new <- sample((depth+2):(N-1), 1)

    }

    # if l = l_max decide to whether sample l_max or l_max-1 with equal probabilities
    else{
      if(l == l_max){
        l_new <- sample(c(l_max,l_max-1), 1)

        # if l=l_max just replace randomly one of the positions (according to the algorithm described in the paper; either by one of its neighbours or one of the uniformly selected positions)
        if(l_new == l_max)
          p_new <- .equal_l(p, l, N, depth)

        # if l=l_max-1, delete one of the positions
        else{
          if(l == 1)
            p_new <- 0
          else{
            g <- sample(1:l, 1)
            p_new <- p[-g]
          }
        }
      }

      # if l != l_max or 0
      else{
        l_new <- sample(c(l-1,l,l+1), 1)
        if(l_new == l)
          # replace one of the positions with a new one
          p_new <- .equal_l(p, l, N, depth)

          # drop randomly one position
        if(l_new == (l-1)){
          if(l_new == 0)
            p_new <- 0
          else{
            g <- sample(1:l, 1)
            p_new <- p[-g]
          }
        }

        # add randomly a new position
        if(l_new == (l+1)){
          g <- sample((depth+3):(N-2),1)
          while(g %in% p)
            g <- sample((depth+3):(N-2),1)
          p_new <- c(p, g)
        }
      }
    }
    p_new <- sort(p_new)

    # calculate the CTW for the entire sequence
    ctw_set_new <- .calculate_ctw_set(input_data, depth, p_new, ctw_set, alphabet)

    ppl_old <- sum(unlist(ctw_set))
    ppl_new <- sum(unlist(ctw_set_new))

    # calculate the log-ratio between the new proposal and the old proposal
    log_ratio <- (ppl_new - ppl_old)

    # add the needed extra terms
    if(l_new == (l-1)){
      if(l == 1)
        log_ratio <- (log_ratio+log((N-depth-3)*(N-depth-4)/2/(N-depth-2), base = exp(1)))
      else{
        if(l == l_max)
          log_ratio <- (log_ratio+log((N-depth-2*l_max-1)*(N-depth-2*l_max-2)/3/(N-depth-l_max-1)/(2*l_max+1), base = exp(1)) )

        else
          log_ratio <- (log_ratio+log((N-depth-2*l-1)*(N-depth-2*l-2)/2/(2*l+1)/(N-depth-l-1), base = exp(1)) )
      }
    }


    if(l_new == (l+1)){
      if(l == 0)
        log_ratio <- (log_ratio+log(2*(N-depth-2)/(N-depth-3)/(N-depth-2), base = exp(1)) )
      else{
        if(l == (l_max-1))
          log_ratio <- (log_ratio+log(3*(N-depth-l_max-2)*(2*l_max+1)/(N-depth-2*l_max-1)/(N-depth-2*l_max-2), base = exp(1)) )
        else
          log_ratio <- (log_ratio+log((2*l+2)*(2*l+3)*(N-depth-l-2)/(N-depth-2*l-4)/(N-depth-2*l-3)/(l+1), base = exp(1)) )
      }
    }

    if((l_new > 0)&(l>0)){
      log_ratio <- (log_ratio + log(p_new[1]- depth - 2 ,base = exp(1)) - log(p[1] - depth - 2 ,base = exp(1)) )

      if(l_new>=2){
        for (i in 2:l_new)
          log_ratio <- (log_ratio + log(p_new[i] - p_new[i-1]-1 ,base = exp(1)))
      }
      if(l>=2){
        for (i in 2:l)
          log_ratio <- (log_ratio - log(p[i] - p[i-1]-1 ,base = exp(1)))
      }
      log_ratio <- (log_ratio + log(N - p_new[l_new]-1 ,base = exp(1)) - log(N - p[l]-1 ,base = exp(1)) )
    }

    if(l == 0)
      log_ratio <- (log_ratio + log(p_new[1] - 2 - depth ,base = exp(1)) + log(N-p_new[1] ,base = exp(1)) - log(N-depth-2) )

    if(l_new == 0)
      log_ratio <- (log_ratio - log(p[1] - 1-depth ,base = exp(1)) - log(N-p[1], base = exp(1)) + log(N-depth-2))


    log_alpha <- min(0, log_ratio)

    log_u <- log(stats::runif(1), base = exp(1))

    if(log_u<log_alpha){
      acc <- (acc+1)
      p <- p_new
      l <- l_new
      ctw_set <- ctw_set_new
    }
    p_out[[iter]] <- p
    l_out[[iter]] <- l

    counter<-0

    if(!is.null(fileName))
      write(paste(c(l, p), collapse = ";"), fileConn, append = TRUE)


  }
  if(!is.null(fileName))
    close(fileConn)
  return(list("number_changes" = l_out,"positions" = p_out, "acceptance_prob" = acc/iters))
}

