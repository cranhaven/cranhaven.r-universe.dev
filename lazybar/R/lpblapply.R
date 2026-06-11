# lapply <- function(X, FUN, ...){
#   FUN <- match.fun(FUN)
#   pb <- lazyProgressBar(length(X))
#   out <- vector("list", length(X))
#   for(i in seq_along(X)){
#     out[[i]] <- lapply(X[i], FUN, ...)
#     pb$tick()$print()
#   }
#   names(out) <- names(X)
#   return(out)
#
#
