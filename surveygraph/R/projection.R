#' Outputs the survey projection onto the agent or symbolic layer
#'
#' @description
#' `make_projection()` outputs the agent or symbolic network corresponding
#' to a survey, i.e. the row or column projection.
#' 
#' @return
#' A data frame corresponding to the edge list of the specified network. It
#' contains three columns named 
#' 
#'   - `u`, the first node adjacent to the edge
#' 
#'   - `v`, the second node adjacent to the edge, and 
#' 
#'   - `weight`, the similarity between nodes `u` and `v`
#' 
#' @param data A data frame corresponding to a survey
#' @param layer A string flag specifying which layer to project
#'   
#'   - `"agent"` produces the network corresponding to the agents, which we assume
#'   to be rows in `data`
#' 
#'   - `"symbolic"` produces the network corresponding to the symbols, or items,
#'   which we assume to be columns in `data`
#' 
#' @param threshold_method A string flag specifying how edges are selected in
#' the network representation.
#' 
#'   - `"raw_similarity"` means we remove all edges whose weight, meaning node
#'   similarity, is below a specified threshold.
#' 
#'   - `"target_lcc"` finds the value of the threshold that results in the network
#'   whose largest connected component is as close as possible to a specified
#'   value. In general a range of thresholds will satisfy this condition, and we
#'   choose the upper limit of this range. 
#' 
#'   - `"target_ad"` finds the value of the threshold that results in the network
#'   whose average degree is as close as possible to a specified value.
#' 
#' @param method_value A utility variable that we interpret according to the
#'   `threshold_method` chosen.
#' 
#'   - If `threshold_method = "raw_similarity"`, then `method_value` is
#'   interpreted as the similarity threshold, and thus is in the range `[-1, 1]`.
#'   A value of -1 means no edges are removed, and a value of 1 means all edges
#'   are removed.
#' 
#'   - If `threshold_method = "target_lcc"`, then `method_value` is interpreted as
#'   the desired fractional size of the largest connected component, in the range
#'   `[0, 1]`. E.g., when set to 0, no nodes are connected, and if set to 1, the
#'   network is as sparse as possible while remaining fully connected.
#' 
#'   - If `threshold_method = "target_ad"`, then `method_value` is interpreted as
#'   the desired average degree. We assume that `method_value` is normalised to
#'   the range `[0, 1]` When `method_value = 0`, then no nodes are connected, and
#'   if `method_value = 1`, the network is complete, meaning it contains every
#'   possible edge.
#' @param centre If `FALSE`, we shift edge weights by 1 from `[-1, 1]` to `[0, 2]`. 
#'   Defaults to TRUE.
#' @param similarity_metric This currently has just one allowed value, namely the
#'   Manhattan distance, which is the default.
#' 
#' @export
#' @examples
#' S <- make_synthetic_data(20, 5)
make_projection <- function(data, 
                            layer, 
                            threshold_method = NULL, 
                            method_value = NULL, 
                            centre = NULL, 
                            similarity_metric = NULL){

  # check that layer is either agent or symbolic
  if(layer != "agent" && layer != "symbolic")
    message("layer needs to be either agent or symbolic")

  # check the value of centre
  if(is.null(centre)){
    if(layer == "agent"){
      centre <- as.integer(1)
    }else if(layer == "symbolic"){
      centre <- as.integer(0)
    }
  }else if(centre == TRUE){
    centre <- as.integer(1)
  }else if(centre == FALSE){
    centre <- as.integer(0)
  }

  # check the value of similarity_matric
  if(is.null(similarity_metric)){
    if(layer == "agent"){
      similarity_metric <- as.integer(0)
    }else if(layer == "symbolic"){
      similarity_metric <- as.integer(0)
    }
  }else if(similarity_metric == "manhattan"){
    similarity_metric <- as.integer(0)
  }else{
    message("overriding similarity metric to manhattan distance") 
    similarity_metric <- as.integer(0)
  }

  if(layer == "agent"){
    if(!is.null(threshold_method) && !is.null(method_value)){
      if(threshold_method == "target_lcc"){
        edgelist <- .Call("rmake_proj_agent_lcc", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else if(threshold_method == "target_ad"){
        edgelist <- .Call("rmake_proj_agent_ad", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else if(threshold_method == "raw_similarity"){
        edgelist <- .Call("rmake_proj_agent_similar", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else{
        message("threshold_method must be target_lcc, target_ad, or raw_similarity")
        message("defaulting to target_lcc with method_value = 1")
        edgelist <- .Call("rmake_proj_agent_lcc", data, 0.95, centre, similarity_metric)
        return(edgelist)
      }
    }else{
      edgelist <- .Call("rmake_proj_agent_lcc", data, 0.97, centre, similarity_metric)
      return(edgelist)
    }
  }

  if(layer == "symbolic"){
    if(!is.null(threshold_method) && !is.null(method_value)){
      if(threshold_method == "target_lcc"){
        edgelist <- .Call("rmake_proj_symbolic_lcc", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else if(threshold_method == "target_ad"){
        edgelist <- .Call("rmake_proj_symbolic_ad", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else if(threshold_method == "raw_similarity"){
        edgelist <- .Call("rmake_proj_symbolic_similar", data, method_value, centre, similarity_metric)
        return(edgelist)
      }else{
        message("threshold method must be one of [...]")
        message("defaulting to target_ad with method_value = 1")
        edgelist <- .Call("rmake_proj_symbolic_ad", data, 1, centre, similarity_metric)
        return(edgelist)
      }
    }else{
      edgelist <- .Call("rmake_proj_agent_lcc", data, 0.97, centre, similarity_metric)
      return(edgelist)
    }
  }
}
