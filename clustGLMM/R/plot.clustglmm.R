plot.clustglmm <- function(x, what="ng", which="traceplots", ng_trace_chain_split=TRUE, ...){

  if(missing(what)){
    adepts <- c("ng", "Gplus", "w", "deviance", "pUig_int")
    availables <- intersect(adepts, rownames(x$settings[[x$chains[1]]][x$settings[[x$chains[1]]]$save, ]))
    what <- availables[1]
  }
  if(missing(which)){
    if(what == "pUig_int"){
      which="clusters_ECDF"
    }
  }
  
  args <- names(list(...))
  
  if((what == "ng") & (which == "traceplots") & ng_trace_chain_split){
    plot_ng_trace_chain_split(x, ...)
  }else{
    if(any(is.element("dimspec", args), is.element("gspec", args))){
      # User attempts to specify some specific subset of parameters
      # Plotting of subset larger than 1 is not supported (too flexible)
      switch(which,
             "ACF" = plot_ACF(x, what, ...),
             "ECDF" = plot_ECDF(x, what, ...),
             "kerneldensity" = plot_kerneldensity(x, what, ...),
             "traceplots" = plot_traceplots(x, what, ...),
             "clusters_ECDF" = plot_clusters(x, doKern = FALSE, doECDF = TRUE, what, ...),
             "clusters_kerneldensity" = plot_clusters(x, what, doKern = TRUE, doECDF = FALSE, ...)
      )
      # If not a single scalar value, 
      # an error/warning is printed with an instruction to be more specific.
      # This is performed by get_scalar_samples(). 
    }else{
      # No specification of what should be plotted
      switch(which,
             "ACF" = plot_ACF_param(x, what, ...),
             "ECDF" = plot_ECDF_param(x, what, ...),
             "kerneldensity" = plot_kerneldensity_param(x, what, ...),
             "traceplots" = plot_traceplots_param(x, what, ...),
             "clusters_ECDF" = plot_clusters_ECDF_param(x, what, ...),
             "clusters_kerneldensity" = plot_clusters_kerneldensity_param(x, what, ...)
             )
    }
  }
}