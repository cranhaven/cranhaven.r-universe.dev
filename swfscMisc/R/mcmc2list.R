#' @title Convert mcmc.list posterior to list
#' @description Convert `mcmc.list` posterior to named list of vectors or arrays.
#'
#' @param x object of class \link[coda]{mcmc.list}.
#' @param pars vector of parameter names to extract.
#' @param collapse.chains return array with dimension for each chain?
#'
#' @note If \code{collapse.chains = TRUE}, the last dimension of arrays will always 
#'   be samples from the posterior. If \code{collapse.chains = FALSE}, the last 
#'   dimension of arrays will be individual chains, and the one prior to that 
#'   will be samples from the posterior for each chain.
#' 
#' @seealso
#'   \link[base]{aperm} to transpose the array if necessary.   
#'   \link[base]{as.data.frame.table} to convert arrays to data.frames.
#'   
#' @export
#' 
mcmc2list <- function(x, pars, collapse.chains = TRUE) {
  if(!coda::is.mcmc.list(x)) stop("'x' must be of class 'mcmc.list'")
  mcmc.varnames <- coda::varnames(x)
  
  p <- sapply(pars, function(p) {
    p1 <- paste0("^", p, "$")
    p2 <- paste0("^", p, "\\[")
    grep(paste0(p1, "|", p2), mcmc.varnames, value = TRUE)
  }, simplify = FALSE)
  p <- p[sapply(p, length) > 0]
  
  post.comb <- runjags::combine.mcmc(x, collapse.chains = F)
  if(!is.list(post.comb)) post.comb <- list(post.comb)
  
  sapply(names(p), function(x) {
    dim.nums <- gsub(x, "", p[[x]])
    dim.nums <- gsub("\\[|\\]", "", dim.nums)
    dim.nums <- do.call(rbind, strsplit(dim.nums, ","))
    dim.nums <- apply(dim.nums, 2, as.numeric)
    
    mat <- lapply(post.comb, function(chain) {
      chain <- chain[, p[[x]]]
      if(length(dim.nums) == 0) {
        stats::setNames(as.vector(chain), rownames(chain))
      } else {
        dims <- apply(dim.nums, 2, max)
        arr <- array(as.vector(t(chain)), dim = c(dims, nrow(chain)))
        dimnames(arr)[length(dims) + 1] <- list(rownames(chain))
        arr
      }
    })
    
    if(length(dim.nums) == 0) {
      if(collapse.chains) unlist(mat) else do.call(cbind, mat) 
    } else {
      iter.dim <- ncol(dim.nums) + if(collapse.chains) 1 else 2
      do.call(abind::abind, c(mat, list(along = iter.dim)))
    }
  }, simplify = FALSE)
}


