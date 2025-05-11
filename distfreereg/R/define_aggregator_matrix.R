# This function defines a matrix that sums over grouped observations. Grouping
# is determined by using columns specified in "ordering" to find repeated
# covariate values. The aggregator matrix has one column per repeated value, and
# the column contains "1"s in the rows corresponding to that repeated value in
# the sorted covariate matrix. Note that "ordering" should be a list of column
# names or numbers; "natural" is not accepted here.
define_aggregator_matrix <- function(X, ordering){
  stopifnot(is.list(ordering))
  ordering <- unlist(ordering)
  if(!is.numeric(ordering)){
    stopifnot(!is.null(colnames(X)))
    ordering <- which(colnames(X) %in% ordering)
  }
  
  X <- cbind(X, 1)
  colnames(X) <- paste0("V", seq_len(ncol(X)))
  ordering <- colnames(X)[ordering]
  
  form <- reformulate(termlabels = ordering, response = paste0("V", ncol(X)))
  agg <- aggregate(form, data = X, FUN = "sum")
  
  cols <- paste0("agg[[\"", ordering, "\"]]", collapse = ", ")
  times <- agg[,ncol(agg)][eval(str2expression(paste0("order(", cols, ")")))]
  return(diag(1/sqrt(times))[rep(1:nrow(agg), times = times),])
}
