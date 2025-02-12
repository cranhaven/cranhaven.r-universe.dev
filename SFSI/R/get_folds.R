#====================================================================
#  Create a matrix with random folds partitions for a provided sample size
#     n:      Sample size
#     k: Number of folds to be included in the partition
#     nCV:    Number of partitions to create
#     seed:   Vector of integers for randomization. If not NULL
#             a number of partitions equal to the length(seed)
 #            will be created
#====================================================================
get_folds <- function(n, k = 5L, nCV = 1L, seed = NULL)
{
  if(n == k){
    randomize <- FALSE
    if(nCV > 1L){
      nCV <- 1
      message(" Only nCV = 1 data partition is created when 'n' is equal to 'k'")
    }
  }else{
    randomize <- TRUE
    if(is.null(seed)){   # Seeds for randomization
      seed <- round(seq(1E3, .Machine$integer.max/1000, length=nCV))
    }else{
      if(nCV != length(seed)){
        stop("'seeds' should be a vector of length equal to 'nCV'")
      }
    }
  }
  if(n < k){
    stop("'n' should be an integer greater or equal to 'k'")
  }

  folds <- matrix(NA, nrow=n, ncol=nCV)
  colnames(folds) <- paste0("CV",1:nCV)
  ff <- as.integer(rep(seq(k),ceiling(n/k))[1:n])
  stopifnot(all(seq(k) %in% ff))  # Internal checkpoint

  for(j in 1:nCV)
  { # Creating folds
    if(randomize){
      set.seed(seed[j])
      folds[,j] <- sample(ff)
    }else{
      folds[,j] <- ff
    }
  }

  return(folds)
}
