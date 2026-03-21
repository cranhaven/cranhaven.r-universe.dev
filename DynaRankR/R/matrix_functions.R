#' Convert data from edgelist to interaction matrix
#' 
#' This function converts data in edgelist format to an interaction matrix.
#' 
#' @param edgelist A two column matrix or dataframe with the identities of winners in the first column and losers in the second column.
#' @param identities A list of contestant identities. 
#'        This list dictates the order in which contestants are arranged in the resulting matrix. 
#' 
#' @return Produces an interaction matrix with winners in the rows and losers in the columns. 
#'         Contestants are arranged according to the order specified by identities.  
#' 
#' @examples edges <- C.crocuta.female$interactions[C.crocuta.female$interactions$period == 1989,1:2]
#' ids <- C.crocuta.female$contestants[C.crocuta.female$contestants$period == 1989,'id']
#' edgelist_to_matrix(edgelist = edges, identities = ids)
#' 
#' 
#' @export
#' 
edgelist_to_matrix <- function(edgelist, identities){
  if(ncol(as.matrix(edgelist)) != 2){stop('edgelist is not a two-column matrix or dataframe')}
  if(any(!c(edgelist[,1], edgelist[,2]) %in% identities)){stop('Not all winners and losers contained in identities')}
  mat <- matrix(data = 0, nrow = length(identities), ncol = length(identities), dimnames = list(identities, identities))
  for(row in 1:length(edgelist[,1])){
    mat[edgelist[row,1], edgelist[row,2]] <- mat[edgelist[row,1], edgelist[row,2]] + 1
  }
  return(mat)
}

#' Convert data from interaction matrix to edgelist
#' 
#' This function converts data in an interaction matrix to edgelist format.
#' 
#' @param mat Interaction matrix containing outcomes of interactions. Dimension names
#'        are interpreted as individual identities. 
#' @return A two-column dataframe with winners in the first column and losers in the second column.
#' 
#' @examples 
#' edges <- C.crocuta.female$interactions[C.crocuta.female$interactions$period == 1989,1:2]
#' ids <- C.crocuta.female$contestants[C.crocuta.female$contestants$period == 1989,'id']
#' mat <- edgelist_to_matrix(edgelist = edges, identities = ids)
#' get_edgelist(mat) 
#'
#' @export

get_edgelist <- function(mat){
  edgelist <- data.frame(winners = rep(NA, sum(mat)), losers = rep(NA, sum(mat)))
  current_row = 1
  for(cell in 1:length(mat)){
    if(mat[cell] <= 0){next}
    for(intx in 1:mat[cell]){
      edgelist[current_row,] <- dimnames(mat)[[1]][arrayInd(cell, dim(mat))]
      current_row <- current_row+1
    }
  }
  return(edgelist)
}

binarize_mat <- function(mat){
  return(
    ifelse(mat > t(mat), 1, ifelse(mat < t(mat), -1, 0))
  )
}

make_full_matrix <- function(order){
  full_matrix <- matrix(0, nrow = length(order), ncol = length(order), dimnames = list(order, order))
  full_matrix[upper.tri(full_matrix)] <- 1
  return(full_matrix)
}

##List inconsistencies
list_inconsistencies <- function(mat){
  if(nrow(mat) < 2){
    return(c())
  }
  index_is <- c()
  for(r in 2:length(mat[1,])){
    for(c in 1:(r-1)){
      if(mat[r,c] > mat[c,r]){
        index_is <- c(index_is, r, c)
      }
    }  
  }
  return(index_is)
}

##Move an individual in a matrix
moverowcol <- function(X, s, d){
  s <- which(dimnames(X)[[1]] == s)
  d <- which(dimnames(X)[[1]] == d)
  new.order <- dimnames(X)[[1]]
  new.order <- unlist(append(new.order[-s], new.order[s], after = d-1))
  return(X[new.order, new.order])
}

#Correlation between two orders
order_corr <- function(derived, original){
  order_corr_df <- data.frame(ID = row.names(original), OrigRank = seq(1:length(row.names(original))))
  order_corr_df$NewRank <- data.frame(NewRank = seq(1:length(order_corr_df[,1])), row.names = row.names(derived))[row.names(original),]
  return(stats::cor(x = order_corr_df$OrigRank, y = order_corr_df$NewRank, method = 'spearman'))
}

