# Window method for class mcmcOutput

window.mcmcOutput <- function(x, start=1, end=NULL, thin=1, ...)  {
  nChains <- attr(x, "nChains")
  if(is.null(nChains) || nrow(x) %% nChains != 0)
    stop("nChains attribute of x is missing or invalid.")
  draws.per.chain <- nrow(x) / nChains
  if(start > draws.per.chain)
    start <- 1
  if(is.null(end) || end > draws.per.chain || end <= start)
    end <- draws.per.chain
  if(start < 1 || end < 1 || thin < 1)
    stop("Arguments start, end, and thin must be integers > 1")
  retain <- seq.int(from=start-1+thin, to=end, by=thin)
  npar <- ncol(x)
  x_new <- unclass(x)
  dim(x_new) <- c(draws.per.chain, nChains, npar)
  x_new <- x_new[retain, , ]
  mostattributes(x_new) <- attributes(x)
  attr(x_new, "mcpar") <- NULL  # no longer valid
  dim(x_new) <- c(length(retain)*nChains, npar)
  colnames(x_new) <- colnames(x)
  return(x_new)
}


