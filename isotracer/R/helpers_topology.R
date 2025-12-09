### * None of the functions in this file is exported

### * make_topology()

#' Build a network topology
#'
#' @section Link format:
#' 
#' Links are defined by a string describing connections between groups of
#' compartments and their directions using \code{->} and \code{<-}. The groups
#' being connected can be a single compartment or several compartments. Several
#' compartments can be separated either by commas or by spaces. Connection
#' descriptions can be chained in a single string.
#'
#' Some valid links are for example \code{"NH4, NO3 -> epi"} or
#' \code{"NH4, NO3 -> epi -> lepto, tricor"}.
#'
#' See the Examples section for more details.
#' 
#' @param links Vector of strings defining the connections between
#'     compartments. Alternatively, can be a data frame with two columns
#'     describing the source and destination of each link, in which case the
#'     arguments "from" and "to" must be provided.
#' @param from Optional, string containing the column name for sources if
#'     "links" is a data frame
#' @param to Optional, string containing the column name for destinations if
#'     "links" is a data frame
#' @param split Optional, vector of strings containing the names of the
#'     compartments which comprise an active and a refractory portions
#'
#' @return A matrix describing the topology of the network (with class
#'     topology). A coefficient (i,j) is 1 if material can flow from
#'     compartment j (column) into compartment i (row), and 0 otherwise.
#'
#' @examples
#' topo <- isotracer:::make_topology(links = "NH4, NO3 -> epi -> pseph, tricor")
#' topo
#'
#' # A larger foodweb
#' links <- c("NH4, NO3 -> seston, epi, CBOM, FBOM",
#'            "seston -> lepto", "epi -> petro, pseph",
#'            "CBOM, FBOM -> eudan", "CBOM -> phyllo",
#'            "FBOM -> tricor -> arg, euthy")
#' topo2 <- isotracer:::make_topology(links = links, split = "epi")
#' topo2
#'
#' # Using a data frame to specify the links
#' links <- data.frame(source = c("NH4", "NO3", "epi"),
#'                     consumer = c("epi", "epi", "petro"))
#' topo3 <- isotracer:::make_topology(links, from = "source", to = "consumer")
#' topo3
#' 
#' @keywords internal
#' @noRd

make_topology <- function(links, from = NULL, to = NULL, split = NULL) {
    # Links are provided as strings
    if (is.null(from) & is.null(to)) {
        out <- build_uptake_mask(links = links)
        out <- structure(out, class = c("topology", class(out)))
        attr(out, "split") = split
        return(out)
    }
    # Links are provided as a data frame
    # Build the links strings from the input data frame
    links <- paste(links[[from]], links[[to]], sep = " -> ")
    return(make_topology(links, split = split))
}

### * topo_get_upsilon_names()

#' @keywords internal
#' @noRd

topo_get_upsilon_names <- function(topo) {
    comps <- colnames(topo)
    out <- rep(NA, ncol(topo)^2)
    k <- 1
    for (i in seq_len(nrow(topo))) {
        for (j in seq_len(ncol(topo))) {
            if (topo[i,j] == 1) {
                out[k] <- paste("upsilon", comps[j], "to", comps[i], sep = "_")
                k = k + 1
            }
        }
    }
    if (k == 1) {
        return(vector())
    }
    return(out[1:(k-1)])
}

### * topo_get_lambda_names()

#' @keywords internal
#' @noRd

topo_get_lambda_names <- function(topo) {
    comps <- colnames(topo)
    out <- rep(NA, ncol(topo))
    k <- 1
    for (j in seq_len(ncol(topo))) {
        out[k] <- paste("lambda", comps[j], sep = "_")
        k = k + 1
    }
    if (k == 1) {
        return(vector())
    }
    return(out[1:(k-1)])
}

### * topo_get_portionAct_names()

#' @keywords internal
#' @noRd

topo_get_portionAct_names <- function(topo) {
    split <- attr(topo, "split")
    if (is.null(split)) {
        return(NULL)
    }
    params <- paste0("portion.act_", split)
    return(params)
}

