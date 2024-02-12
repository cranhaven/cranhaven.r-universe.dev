print.vertex.sort <- function(x, ...) {
  # Print all details of a vertex.sort object
  # Args:
  #   x: a vertex.sort object

  if ( class(x)!="vertex.sort" )
    return("Error: first argument should be a vertex.sort object")

  cat("Call:\n")
  print(x$call)
  cat("\ngraph:\n")
  print(x$graph)
  cat("\nTop layer:\n")
  print(x$top.actors)
  cat("\nCore layer:\n")
  print(x$core.actors)
  cat("\nBottom layer:\n")
  print(x$bottom.actors)
  cat("\nExcluded actors:\n")
  if (length(x$excluded.actors)!=0) print(x$excluded.actors) else cat(" No actors were excluded\n")
  cat("\nDisconnected actors:\n")
  if (length(x$disconnected.actors)!=0) print(x$disconnected.actors) else cat(" No disconnected actors\n")
  cat("\nDisconnected targets:\n")
  if (length(x$disconnected.targets)!=0) print(x$disconnected.targets) else cat(" No disconnected targets\n")
  cat("\nLevels:\n")
  print(x$nodes.in.levels)
}
