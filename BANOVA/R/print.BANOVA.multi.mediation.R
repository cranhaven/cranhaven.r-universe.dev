print.BANOVA.multi.mediation <- 
  function(x, ...){
    print.effects <- function(list_with_effects, mediators = NULL, additional_title = NULL, 
                              list_with_effect_sizes = NULL){
      print.list.elements <- function(list_with_effects, list_with_effect_sizes){
        if (!inherits(list_with_effects, "list")){
          temp <- list_with_effects
          list_with_effects <- list(temp)
        } 
        for (i in 1:length(list_with_effects)){
          print(noquote(list_with_effects[[i]]), row.names = F, right=T)
          if (!is.null(list_with_effect_sizes)){
            cat("effect size:", list_with_effect_sizes[[i]])
            cat("\n")
          } else {
            cat("\n")
          }
        }
      }
      if (!is.null(mediators)){
        counter <- 1
        for (mediator in mediators){
          if (!is.null(additional_title)) cat(additional_title[counter])  
          print.list.elements(list_with_effects[[mediator]], list_with_effect_sizes[[mediator]])
          counter <- counter + 1
        }
      } else {
        if (!is.null(additional_title)) cat(additional_title)  
        print.list.elements(list_with_effects, list_with_effect_sizes)
      }
    }
    num_dashes <- 100
    xvar <- x$xvar
    mediators <- x$mediators
    num_mediators <- length(mediators)
    if (x$individual){
      save_options <- getOption("max.print")
      options(max.print=nrow(x$individual_indirect[[1]])*(num_mediators+10))
      #Report individual indirect effects of the causal variable and effect sizes
      cat(paste(strrep("-", num_dashes), '\n'))
      cat(paste0("Individual indirect effects of the causal variable", xvar, "on the outcome variables\n"))
      print.effects(x$individual_indirect, mediators,
                    paste(rep("\nIndirect effects of", num_mediators), rep(xvar, num_mediators), 
                          rep("via", num_mediators), mediators, rep("\n", num_mediators), sep = " "))
      
      #Report total individual indirect effects of the causal variable
      cat(paste(strrep("-", num_dashes), '\n'))
      cat(paste("Total individual indirect effects of the causal variable", xvar, 
                "on the outcome variables\n\n"))
      print.effects(x$total_indir_effects)
  
      options(max.print=save_options)
    }else{
      #Report direct effects of the causal variable on the outcome
      cat(paste(strrep("-", num_dashes), '\n'))
      cat(paste("Direct effects of the causal variable", xvar, "on the outcome variable\n\n"))
      print.effects(x$dir_effects)
      
      #Report direct effects of the mediator variables on the outcome
      mediator_names <- paste(mediators,  collapse = " and ")
      cat(paste(strrep("-", num_dashes), '\n'))
      cat(paste("Direct effects of mediators", mediator_names, "on the outcome variable\n"))
      print.effects(x$m1_effects, mediators, paste(rep("\nDirect effects of", num_mediators), 
                                                   mediators, rep("\n", num_mediators), sep = " "))
    
      #Report direct effects of the causal variable on mediator variables
      cat(paste(strrep("-", num_dashes), '\n'))
      cat(paste("Direct effects of the causal variable", xvar, "on the mediator variables\n"))
      print.effects(x$m2_effects, mediators, paste(rep("\nDirect effects of", num_mediators), 
                                                   rep(xvar, num_mediators), rep("via", num_mediators), 
                                                   mediators, rep("\n", num_mediators), sep = " "))
      
      #Report indirect effects of the causal variable and effect sizes
      cat(paste(strrep("-", num_dashes), '\n'))
      cat(paste0("Indirect effects of the causal variable", xvar, "on the outcome variables\n"))
      print.effects(x$indir_effects, mediators, list_with_effect_sizes = x$effect_sizes,
                    paste(rep("\nIndirect effects of", num_mediators), rep(xvar, num_mediators), 
                          rep("via", num_mediators), mediators, rep("\n", num_mediators), sep = " "))
      
      #Report total indirect effects of the causal variable
      cat(paste(strrep("-", num_dashes), '\n'))
      cat(paste("Total indirect effects of the causal variable", xvar, 
                "on the outcome variables\n\n"))
      print.effects(x$total_indir_effects)
      
    }
  }