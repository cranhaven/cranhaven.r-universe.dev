dnpr <- function(vgraph, viteration_no, vparallel=FALSE, vcpus=1) {
  # Calls the Create_rand_g_without_preserving_degrees function. Parallel running mode can be enabled.
  # The ultimate goal is to randomize a network without preserving its node in- and out-degrees
  # Args:
  #   vgraph: a graph object representing the network to randomize
  #   viteration_no: integer representing the number of random networks that we need to generate
  #   vparallel: a boolean variable. If it is equal to True, the funciton will run in parallel mode
  #   vcpus: an integer indicating the number of cpus to use in case parallel mode is on.
  # Returns:
  #   rand_vgraph: a list containing the randomized networks in the form of graph objects

  if ( class(vgraph)!="igraph" )
    stop("Error: first argument should be an igraph graph object")
  if ( class(viteration_no)!="numeric" )
    stop("Error: second argument should be an integer")
  if ( class(vparallel)!="logical" )
    stop("Error: third argument should be a logical")
  if ( class(vcpus)!="numeric" )
    stop("Error: fourth argument should be an integer")

  if (vparallel==T & vcpus==1)
    warning("If you would like to run your code in parallel mode, the number of cpus should be bigger than 1")

  nodes_no    <- vcount(vgraph)
  edges_no    <- ecount(vgraph)
  swaping_no  <- edges_no*10
  actors_ids  <- which(degree(vgraph,V(vgraph)$id,"out")>0)
  log_name    <- "dnpr_log.txt"

  rand_vgraph <- NULL

  do.call("sfInit", list(parallel=vparallel, cpus=vcpus, slaveOutfile="slaveOutfile.txt"))

  igraph   <- as.environment("package:igraph")
  snowfall <- as.environment("package:snowfall")
  sfLibrary(snowfall)
  sfLibrary(igraph)

  sfExport("vgraph", "actors_ids", "edges_no", "nodes_no", "swaping_no", "viteration_no", "log_name")

  rand_vgraph <- do.call(list, sfClusterApplyLB(1:viteration_no, Create_rand_g_without_preserving_degrees))

  sfStop()
  unlink(c(log_name, "slaveOutfile.txt"))

  return(rand_vgraph)
}
