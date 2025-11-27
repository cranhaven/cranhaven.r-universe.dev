#' Spectral clustering for \code{\link[graph]{graphNEL}} objects
#'
#' @param g \code{\link[graph]{graphNEL}} object
#' @param Cn_min minimum cluster size
#' @param tol tolerance
#' @param names are we dealing with alphaNumeric (1) or numeric (!1) ids
#' @param fix_neig whether to fix neighbouring nodes found in same community
#' 
#' @return \code{data.frame} with node names and membership information
#' @export
#' @seealso \code{\link{spectral_igraph_membership}}
#'
#' @examples
#' library(graph)
#' V = letters[1:12]
#' g2 = randomEGraph(V, edges=20)
#' mem.df = spectral_graphNEL(g2)
#' head(mem.df)
spectral_graphNEL <- function(g, Cn_min = 1L, tol = 0.00001, names = 1L,
                              fix_neig = 0L){
  if(!inherits(g,'graphNEL')){
    stop('Graph should be "graphNEL" object.')
  }
  led <- graph::edges(g)
l <- lapply(names(led),function(.x)data.frame(V1=rep(.x,length(led[[.x]])),
                                              V2=led[[.x]]))
df <- do.call(rbind,l)
load_data(df = df)
status <- spectral(fix_neig=1)
spec   <- membership(detach_graph=1)
spec.df <- data.frame(names=spec$ID,membership=spec$K)
return(spec.df)
}