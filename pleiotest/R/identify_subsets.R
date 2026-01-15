#' @title Internal function to identify sub-sets of data and return a list with IDs.
#' @description This function is used internally in pleioR.
#' @param trait character indicating traits.
#' @param id character indicating IDs.
#' @return list with an ID matrix and ID subsets.
#' @author Original code by Fernando Aguate.
#' @export
identify_subsets <- function(trait, id){
  trait <- as.character(trait)
  id <- as.character(id)

  if (length(trait) != length(id))
    stop('Vectors trait and id should be of the same length.')
  trait_id <- split(trait, id)
  matrix_0 <- matrix(0,
              nrow = length(unique(id)),
              ncol = length(unique(trait)),
              dimnames = list('row.names' = names(trait_id), 'col.names' = unique(trait)))

  for (i in seq_along(trait_id))
    matrix_0[i, trait_id[[i]]] <- 1

  matrix_1 <- matrix_0[!duplicated(matrix_0),, drop = F]
  temp_1 <- apply(matrix_0, 1, function(x)
    paste(x, collapse = ''))
  temp_2 <- apply(matrix_1, 1, function(x)
    paste(x, collapse = ''))
  temp_3 <- table(temp_1)[match(temp_2, names(table(temp_1)))]
  matrix_2 <- cbind(matrix_1, temp_3)

  colnames(matrix_2) <- c(unique(trait), 'n')
  rownames(matrix_2) <- NULL
  if(nrow(matrix_2) > 1){
    id_matrix <- matrix_2[do.call(order, as.list(as.data.frame(matrix_2[, -ncol(matrix_2)]))),]
  } else {
    id_matrix <- matrix_2
  }
  id_subsets <- split(names(temp_1), temp_1)

  return(list(id_matrix, id_subsets))
}



