#' Title
#'
#' @param set TBD
#' @param dimension_name_1 TBD
#' @param dimension_name_2 TBD
#'
#' @return TBD
#' @export
move_nodes_to_unit_circle<-function(set, dimension_name_1, dimension_name_2) {
  # get node position on the specified two dimensions
  dimension_names = c(dimension_name_1,dimension_name_2)
  node_position = set$rotation$nodes[,..dimension_names]
  # compute the length of each node position vector on the two dimensional plane
  length_list = sqrt(node_position[,1]^2+node_position[,2]^2)
  # compute the re-scaling coefficient for each non-zero node vector
  non_zero_lengths = which(length_list!=0)
  length_list[non_zero_lengths] = max(length_list)/length_list[non_zero_lengths]
  # move nodes to the circle
  for(i in non_zero_lengths)
  {
    set$rotation$nodes[[dimension_name_1]][i]=as.numeric(set$rotation$nodes[[dimension_name_1]][i]*length_list[i])
    set$rotation$nodes[[dimension_name_2]][i]= as.numeric(set$rotation$nodes[[dimension_name_2]][i]*length_list[i])
  }
  # compute the node weights so that the centroids could be computed
  codeNames = set$rotation$codes
  row_counts = set$connection.counts
  node_weights = data.frame(matrix(0,nrow=nrow(row_counts),ncol=length(codeNames)))

  for(i in 1:(length(codeNames)-1))
  {
    for(j in (i+1):length(codeNames))
    {
      connection_name = paste0(codeNames[i]," & ",codeNames[j])
      x = row_counts[,..connection_name]/2
      node_weights[,i]=node_weights[,i]+x
      node_weights[,j]=node_weights[,j]+x
    }
  }
  rs = rowSums(node_weights)
  rs_1 = which(rs!=0)
  node_weights[rs_1,]=node_weights[rs_1,]/rs[rs_1]
  # finally, recompute centroids
  centroids = as.matrix(node_weights)%*%as.matrix(set$rotation$nodes)
  for(j in 1:ncol(centroids))
  {
    set$model$centroids[,j+1] = centroids[,j]
  }
  return(set)
}


#' Title
#'
#' @param set TBD
#' @param dimension_name_1 TBD
#' @param dimension_name_2 TBD
#'
#' @return TBD
#' @export
move_nodes_to_unit_circle_with_equal_space <- function(set,dimension_name_1,dimension_name_2) {
  # get node position on the specified two dimensions
  dimension_names = c(dimension_name_1,dimension_name_2)
  node_position = set$rotation$nodes[,..dimension_names]
  # compute the length of each node position vector on the two dimensional plane
  length_list = sqrt(node_position[,1]^2+node_position[,2]^2)
  # find non-zero node positions
  non_zero_lengths = which(length_list!=0)
  node_position_non_zero = node_position[non_zero_lengths,]
  # divide the angle
  rotation_angle = 2*pi/nrow(node_position_non_zero)
  # order the nodes along the circle
  node_position_non_zero$id = c(1:nrow(node_position_non_zero))
  node_position_non_zero_upper = node_position_non_zero[which(node_position_non_zero[[dimension_name_2]]>=0),]
  node_position_non_zero_lower = node_position_non_zero[which(node_position_non_zero[[dimension_name_2]]<0),]
  node_position_non_zero_upper = node_position_non_zero_upper[order(node_position_non_zero_upper[[dimension_name_1]],decreasing = TRUE),]
  node_position_non_zero_lower = node_position_non_zero_lower[order(node_position_non_zero_lower[[dimension_name_1]],decreasing = FALSE),]
  node_position_non_zero_sorted = rbind(node_position_non_zero_upper,node_position_non_zero_lower)
  # find which has the max length
  max_i = which(length_list[non_zero_lengths]==max(length_list))[1]
  first_i = which(node_position_non_zero_sorted$id==max_i)[1]
  # find the coordinates of the fixed node
  x1=node_position_non_zero_sorted[[dimension_name_1]][first_i]
  y1=node_position_non_zero_sorted[[dimension_name_2]][first_i]
  # rotate the ordered nodes
  for(i in 1:nrow(node_position_non_zero_sorted))
  {
    ind = (first_i+i-1)%%nrow(node_position_non_zero_sorted)
    if(ind==0)
    {
      ind = nrow(node_position_non_zero_sorted)
    }
    angle = (i-1)*rotation_angle
    x2 = x1*cos(angle)-y1*sin(angle)
    y2 = x1*sin(angle)+y1*cos(angle)
    node_position_non_zero_sorted[[dimension_name_1]][ind]=x2
    node_position_non_zero_sorted[[dimension_name_2]][ind]=y2
  }
  # match the order of the original data
  node_position_non_zero_sorted = node_position_non_zero_sorted[order(node_position_non_zero_sorted$id,decreasing = FALSE),]
  node_position[non_zero_lengths,]=node_position_non_zero_sorted[,..dimension_names]
  set$rotation$nodes[,dimension_names]=node_position

  # compute the node weights so that the centroids could be computed
  codeNames = set$rotation$codes
  row_counts = set$connection.counts
  node_weights = data.frame(matrix(0,nrow=nrow(row_counts),ncol=length(codeNames)))

  for(i in 1:(length(codeNames)-1))
  {
    for(j in (i+1):length(codeNames))
    {
      connection_name = paste0(codeNames[i]," & ",codeNames[j])
      x = row_counts[,..connection_name]/2
      node_weights[,i]=node_weights[,i]+x
      node_weights[,j]=node_weights[,j]+x
    }
  }
  rs = rowSums(node_weights)
  rs_1 = which(rs!=0)
  node_weights[rs_1,]=node_weights[rs_1,]/rs[rs_1]
  # finally, recompute centroids
  centroids = as.matrix(node_weights)%*%as.matrix(set$rotation$nodes)
  for(j in 1:ncol(centroids))
  {
    set$model$centroids[,j+1] = centroids[,j]
  }
  return(set)
}
