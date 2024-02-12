odpr <- function(vgraph, viteration_no, vparallel=FALSE, vcpus=1) {
  # Calls the Create_rand_g_with_preserving_out_degrees function. Parallel running mode can be enabled.
  # The ultimate goal is to randomize a network by preserving its node out-degrees
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


  edges_no    <- ecount(vgraph)
  swaping_no  <- edges_no*10
  edges       <- get.edgelist(vgraph)
  from        <- edges[,1]
  to          <- edges[,2]
  log_name    <- "odpr_log.txt"

  rand_vgraph <- NULL

  do.call("sfInit", list(parallel=vparallel, cpus=vcpus, slaveOutfile="slaveOutfile.txt"))

  igraph   <- as.environment("package:igraph")
  snowfall <- as.environment("package:snowfall")
  sfLibrary(snowfall)
  sfLibrary(igraph)

  Sub_switch_edges = get0("Sub_switch_edges")
  sfExport("Sub_switch_edges","vgraph", "from", "to", "edges_no", "swaping_no", "viteration_no", "log_name")

  rand_vgraph <- do.call(list, sfClusterApplyLB(1:viteration_no, Create_rand_g_with_preserving_out_degrees))

  sfStop()
  unlink(c(log_name, "slaveOutfile.txt"))

  return(rand_vgraph)
}
