#' Illustrates how network properties vary with the similarity threshold
#'
#' @description
#' `make_threshold_profile()` outputs properties of the agent or symbolic network
#' as a function of similarity threshold.
#' 
#' @return
#' A data frame containing properties of the `agent` or `symbolic` network as a
#' function of the similarity threshold. In particular, it contains three columns
#' named
#' 
#'   - `threshold`, the value of the similarity threshold
#' 
#'   - `ad`, the average degree resulting from `threshold`, and 
#' 
#'   - `lcc`, the size of the largest connected component resulting from
#'   `threshold`
#' 
#' @param data A data frame corresponding to the attitudes held by agents with
#'   respect to a number of items
#' @param layer A string flag specifying the type of network to be extracted,
#'   
#'   - `"agent"` produces the network corresponding to the agents, which we assume
#'   to be rows in `data`
#' 
#'   - `"symbolic"` produces the network corresponding to the symbols, or items,
#'   which we assume to be columns in `data`
#' 
#' @details
#' Note that this routine is expensive on large graphs. We study networks over the
#' full range of similarity thresholds `[-1, 1]`, and as a result, produce
#' networks that are complete at the lower limit of that range. Note that by default we
#' will subsample the provided survey with the C++ implementation in order to
#' avoid memory issues. We could then allow a flag that turns off the subsampling
#' step, at the user's peril.
#' @export
#' @examples
#' S <- make_synthetic_data(20, 5)
make_threshold_profile <- function(data, layer){
  if(layer == "agent"){
    tdata <- .Call("rmake_threshold_profile_agent", data)
    return(tdata)
  }else if(layer == "symbolic"){
    tdata <- .Call("rmake_threshold_profile_symbolic", data)
    return(tdata)
  }else{
    message("layer must be either agent or symbolic")
  }
}
