#' insert_nodes: Part of DCEM package.
#'
#' Implements the node insertion into the heaps.
#'
#' @param heap_list (list): The nested list containing the heaps. Each entry in the
#' list is a list maintained in max-heap structure.
#'
#' @param heap_assn (numeric): The vector representing the heap assignments.
#'
#' @param data_probs (string): A vector containing the probability for data.
#'
#' @param leaves_ind (numeric): A vector containing the indices of leaves in heap.
#'
#' @param num_clusters (numeric): The number of clusters. Default: \strong{2}
#'
#' @return
#'         A nested list. Each entry in the list is a list maintained
#'         in the max-heap structure.
#'
#' @usage
#' insert_nodes(heap_list, heap_assn, data_probs, leaves_ind, num_clusters)
#'
#' @references
#' Parichit Sharma, Hasan Kurban, Mehmet Dalkilic DCEM: An R package for clustering big data via
#' data-centric modification of Expectation Maximization, SoftwareX, 17, 100944 URL
#' https://doi.org/10.1016/j.softx.2021.100944
#'
#' @export

insert_nodes <- function(heap_list, heap_assn, data_probs, leaves_ind, num_clusters){

  # Loop over each heap
  for (clus in 1:num_clusters){

    ind = which(heap_assn == clus)

    if (length(ind)!=0){

      # Get the probability and index
      # of the data
      prob = data_probs[ind]
      node = leaves_ind[ind]

      # Perform insertion into max-heap
      for (j in 1:length(ind)){

        l <- length(heap_list[[clus]])
        heap_list[[clus]][[l+1]] <- c(prob[j], node[j])
        i <- l+1

        while(ceiling(i/2)>0){

          if ( heap_list[[clus]][[ceiling(i/2)]][1] < heap_list[[clus]][[i]][1]){
            temp <- heap_list[[clus]][[i]]
            heap_list[[clus]][[i]] <- heap_list[[clus]][[ceiling(i/2)]]
            heap_list[[clus]][[ceiling(i/2)]] <- temp
            i <- ceiling(i/2);
          }
          else{
            break
          }
        }
      }
    }

  }

  return(heap_list)
}

#' separate_data: Part of DCEM package.
#'
#' Separate leaf nodes from the heaps.
#'
#' @param heap_list (list): The nested list containing the heaps. Each entry in the
#' list is a list maintained in max-heap structure.
#'
#' @param num_clusters (numeric): The number of clusters. Default: \strong{2}
#'
#' @return
#'         A nested list where,
#'
#'         First entry is the list of heaps with leaves removed.
#'
#'         Second entry is the list of leaves.
#'
#' @usage
#' separate_data(heap_list, num_clusters)
#'
#' @references
#' Parichit Sharma, Hasan Kurban, Mehmet Dalkilic DCEM: An R package for clustering big data via
#' data-centric modification of Expectation Maximization, SoftwareX, 17, 100944 URL
#' https://doi.org/10.1016/j.softx.2021.100944
#'
#' @export

separate_data <- function(heap_list, num_clusters){

  leaf_list <- c()

  # Loop over the heaps and
  # seperate the leaves from the
  # non-leaf nodes to seperate
  # expressive and non-expressive
  # data
  for (clus in 1:num_clusters){

    if(is.null(length(heap_list[[clus]])) | length(heap_list[[clus]]) == 1 | length(heap_list[[clus]]) == 0){
      heap_list[[clus]] <- heap_list[[clus]][1]
    }

    if (length(heap_list[[clus]]) == 2){
      vals <- heap_list[[clus]][[2]][2]
      leaf_list <- append(leaf_list, vals)
      heap_list[[clus]] <- heap_list[[clus]][1]
    }

    else {
      leaf_start <- ceiling(length(heap_list[[clus]])/2)
      leaf_end <- length(heap_list[[clus]])
      vals <- as.vector(unlist(lapply(heap_list[[clus]][leaf_start:leaf_end], `[[`, 2)))
      leaf_list <- append(leaf_list, vals)
      heap_list[[clus]] <- heap_list[[clus]][1:leaf_start-1]
    }
  }

  separated_data <- list(heap_list, leaf_list)
  return(separated_data)
}
