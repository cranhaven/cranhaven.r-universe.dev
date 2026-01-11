#' The Enron data set as extracted from \code{igraph} using the script in data-raw
#'
#' A data set of counts of emails between email addresses
#' This is a non-symmetric network.
#' Nodes represent email address.
#' The edge-state ij between two email addresses i and j is the number of emails sent from i to j
#' The Groups vector is the node label from the igraph attribute "notes"
#'
#' @format A list containing
#' \describe{
#'   \item{Edges}{an edges object with each edge-state representing the number of emails between two email addresses}
#'   \item{Groups}{A vector giving a group name to which the email address belong. The order matches the edges such that Edges[i,j] is the edge-state between the nodes i and nodes j who are members of Groups[i] and Groups[j] respectively}
#' }
#' @source \url{https://cran.r-project.org/package=igraphdata}
"Enron"

#' The Macaque data set as extracted from \code{igraph} using the script in data-raw
#' @format An \code{edges} object of activation counts between brain regions in a Macaque
#' @seealso igraph
"Macaque"

#' The Stack-Overflow data set as extracted from \code{igraph} using the script in data-raw
#' Extracted on 27/8/2019 from Kaggle (login required) using:
#' \code{library(rvest)}
#' \code{read_html("https://www.kaggle.com/stackoverflow/stack-overflow-tag-network/downloads/stack_network_links.csv/1")}
#' @format An \code{edges} object of activation counts between brain regions in a Macaque
#' @source \url{https://www.kaggle.com/stackoverflow/stack-overflow-tag-network/}
#' @seealso igraph
"StackOverflow"
