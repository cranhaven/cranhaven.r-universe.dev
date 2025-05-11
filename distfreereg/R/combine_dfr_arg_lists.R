combine_dfr_arg_lists <-
  function(...){
    # This function combines lists of possible distfreereg() arguments into a
    # single list, removing elements with duplicated names by calling
    # combine_lists(). In particular, this function extracts any override
    # elements, combines those separately, and adds the resulting list to the
    # final returned list.
    comb_lists <- list(...)
    override_lists <- lapply(comb_lists, function(x) x[["override"]])
    override_lists <-
      as.list(unlist(override_lists[which(sapply(override_lists,
                                                 function(x) !is.null(unlist(x))))],
                     recursive = FALSE))
    override <- combine_lists(override_lists)
    
    comb_list <- combine_lists(...)
    comb_list[["override"]] <- NULL

    output <- comb_list
    if(length(override) > 0) output <- c(output, list(override = override))
    return(output)
  }
