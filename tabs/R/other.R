#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stringi stri_detect_regex
#' @importFrom dplyr tibble as_tibble full_join 
#' @importFrom ape Ntip Nnode
#' 
NULL
#' 
#' @title add.alpha
#' 
#' @description add transparency level to color#' 
#' 
#' @param col color
#' @param alpha alpha level
#'
#' @noRd
#' 
#' @keywords internal
#' 
add.alpha <- function(col, alpha=1){
  # example: add.alpha('red', 1)
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, grDevices::col2rgb)/255, 2,
        function(x)
          grDevices::rgb(x[1], x[2], x[3], alpha=alpha))
}


#' @title pbar
#' 
#' @description progress bar 
#' 
#' @param min Minimum value of the progress bar
#' @param max Maximum value of the progress bar
#' @param style Progress bar style (also available style = 1 and style = 2)
#' @param width Progress bar width. Defaults to getOption("width") 
#' @param char Character used to create the bar
#' 
#'
#' 
#' @return progress bar 
#'
#' @noRd
#' 
#' @keywords internal
#'
#' 
pbar <- function(min=0,max=10,style=3,width=50,char="="){
  # Create a connection to suppress output
  temp_conn <- file(tempfile(), open = "w")
  sink(temp_conn, type = "output")  # Redirect stdout
  
  pb <- txtProgressBar(min = min,     
                       max = max, 
                       style = style,   
                       width = width,  
                       char = char)   
  sink()  # Restore stdout
  close(temp_conn)  # Close the temporary connection
  
  return(pb)
}


#' @title update_pbar
#' 
#' @description update progress bar 
#' 
#' @param pb progress bar
#' @param value value to use (iterator)
#' 
#'
#' 
#' @return progress bar 
#'
#' @noRd
#' @keywords internal
#'
#' 
update_pbar <- function(pb, value) {
  # Capture progress bar output as a string
  progress_string <- capture.output(setTxtProgressBar(pb, value))
  # Print as a message (avoids console clutter)
  message("\r", progress_string, appendLF = FALSE)
}



#' @title contains_accent_or_weird_symbol
#' 
#' @description convert weird symbols 
#' 
#' @param word a word
#' 
#'
#' 
#' @return vector
#'
#' @noRd
#' 
#' @keywords internal
#'
#' 
contains_accent_or_weird_symbol <- function(word) {
  # Define the pattern for accents and weird symbols
  pattern <- "[^\\p{ASCII}]"
  # Check if the word contains any character matching the pattern
  return(stringi::stri_detect_regex(word, pattern))
}

#' @title add_class
#' 
#' @description add class obtained from tidytree
#' 
#' @param x object
#' @param name value
#'
#' 
#' @return class
#'
#' 
#' @keywords internal
#' @noRd
#' 
add_class <- function(x, name){
  xx <- setdiff(name, class(x))
  if (length(xx)>0){
    class(x) <- base::union(xx, class(x))
  }
  return (x)
}


#' @title as_tibble.phylo
#' 
#' @description add class obtained from tidytree
#' 
#' @param x phylo 
#'
#' 
#' @return tibble
#'
#' @keywords internal
#' @noRd
#' 
as_tibble.phylo <- function(x, ...) {
  phylo <- x
  ntip <- ape::Ntip(phylo)
  N <- ape::Nnode(phylo, internal.only=FALSE)
  
  tip.label <- phylo[["tip.label"]]
  edge <- phylo[["edge"]]
  colnames(edge) <- c("parent", "node")
  res <- dplyr::as_tibble(edge)
  if (!is.null(phylo$edge.length))
    res$branch.length <- phylo$edge.length
  
  label <- rep(NA, N)
  label[1:ntip] <- tip.label
  if ( !is.null(phylo$node.label) ) {
    label[(ntip+1):N] <- phylo$node.label
  }
  ## isTip <- rep(FALSE, N)
  ## isTip[1:ntip] <- TRUE
  
  label.df <- dplyr::tibble(node=1:N, label=label) #, isTip = isTip)
  res <- dplyr::full_join(res, label.df, by='node')
  
  idx <- is.na(res$parent)
  res$parent[idx] <- res$node[idx]
  
  if (!is.null(phylo$edge.length) && !is.null(phylo$root.edge))
    res$branch.length[res$parent == res$node] = phylo$root.edge
  
  res <- res[order(res$node),]
  aa <- names(attributes(phylo))
  group <- aa[ ! aa %in% c("names", "class", "order", "reroot", "node_map")]
  if (length(group) > 0) {
    for (group_ in group) {
      ## groupOTU & groupClade
      group_info <- attr(phylo, group_)
      if (length(group_info) == nrow(res)) {
        res[[group_]] <- group_info
      }
    }
  }
  #class(res) <- c("tbl_tree", class(res))
  res <- add_class(res, 'tbl_tree')
  return(res)
}
