ks.test.compare <-
  function(x, ..., stat = NULL){
    stopifnot(!is.null(names(x[["observed_stats"]])))
    if(is.null(stat)) stat <- names(x[["observed_stats"]])[[1]]
    strict_match(stat, choices = names(x[["observed_stats"]]))
    stopifnot(!is.null(x[["observed_stats"]][[stat]]), !is.null(x[["mcsim_stats"]][[stat]]))
    output <- ks.test(x[["observed_stats"]][[stat]], x[["mcsim_stats"]][[stat]], ...)
    output[["data.name"]] <- paste0(deparse1(substitute(x)), "[[\"observed_stats\"]][[",
                                    deparse1(substitute(stat)), "]]", " and ",
                                    deparse1(substitute(x)), "[[\"mcsim_stats\"]][[",
                                    deparse1(substitute(stat)), "]]")
    return(output)
  }
