mappingFR <- function(data, var = NULL, colID = NULL,
                      type = c("static", "interactive"),
                      typeStatic = c("tmap", "choro.cart", "typo", "bar"),
                      unit = c("region"),
                      year = c("2021", "2020", "2019"),
                      matchWith = c("name", "code"), 
                      dir = NULL,
                      add_text = NULL, subset = NULL,
                      facets = NULL, aggregation_fun = sum, aggregation_unit = NULL,
                      options = mapping.options())
  
{
  check.unit.names = options$check.unit.names
  use_cache = options$use_cache
  use_internet = options$use_internet
  type <- match.arg(type, choices = eval(formals(mappingFR)$type))
  typeStatic <- match.arg(typeStatic, choices = eval(formals(mappingFR)$typeStatic))

  
  if(!is.null(var))
  {
    if(is.numeric(var))
    {
      var <- colnames(data)[var]
      
    }else{
      if(!any(sapply(var, function(x) x == colnames(data))))
      {
        stop("Column names var does not exist.", call. = FALSE)
      }
    }
  }
  
  
  if(!inherits(data, "FR"))
  {

    data <- data.frame(data, check.names = FALSE)
    
    unit <- match.arg(unit, choices = eval(formals(mappingFR)$unit))
    year <- match.arg(year, choices = eval(formals(mappingFR)$year))
    matchWith <- match.arg(matchWith, choices = eval(formals(mappingFR)$matchWith))
    
    data <- FR(data = data, colID = colID, unit = unit, matchWith = matchWith, subset = subset,
               year = year, check.unit.names = check.unit.names, use_cache = use_cache, use_internet = use_internet, crs = options$crs,
               aggregation_fun = aggregation_fun, aggregation_unit = aggregation_unit, aggregation_var = var, dir = dir)
    
    colID <- attributes(data)$colID
    
  }else
  {
    
    unit <- attributes(data)$unit
    year <- attributes(data)$year
    colID <- attributes(data)$colID
    
    if(!is.null(subset))
    {
      if(!inherits(subset, "formula"))
      {
        stop("subset must be a formula.")
      }
      data <- subset_formula(data = data, formula = subset)
    }
    
    
    if(!is.null(aggregation_unit))
    {
      
      if(is.null(aggregation_fun))
      {
        stop("aggregation_fun must be provided to aggregate data")
      }
      
      
      if(any(aggregation_unit%in%c("region")))
      {
        unit <- aggregation_unit
        nm <- getNamesFR(year = year, unit = unit, all_levels = TRUE)
        if(attributes(data)$unit == aggregation_unit)
        {
          colnames(nm)[which(colnames(nm) == aggregation_unit)] <- attributes(data)$colID
          aggregation_unit <- attributes(data)$colID
        }
        
        
        if(is.null(facets)| isTRUE(facets == aggregation_unit))
        {
          data <- aggregate(x = data[,var],
                            by = list(var = data[, aggregation_unit, drop = TRUE]), FUN = aggregation_fun)
          
          colnames(data)[1] <- aggregation_unit
          facets_join <- NULL
          
        }else{
          
          if(!any(facets %in% colnames(data)))
          {
            stop("facets name does not exit.")
          }
          
          dt <- lapply(levels(factor(data[,facets, drop = TRUE])), function(x) aggregate(x = data[data[,facets, drop = TRUE]==x, var],
                                                                                         by = list(var = data[data[,facets, drop = TRUE]==x,aggregation_unit, drop = TRUE],
                                                                                                   var2 = data[data[,facets, drop = TRUE]==x,facets, drop = TRUE]),
                                                                                         FUN = aggregation_fun))
          
          data <- do.call("rbind", dt)
          class(data) <- c(class(data),"FR")
          
          colnames(data)[1] <- aggregation_unit
          
          if(any(facets %in% colnames(nm)))
          {
            facets <- paste(facets,"_facets", sep = "")
          }
          
          colnames(data)[2] <- facets
          facets_join <- facets
          
        }
        
        data[,aggregation_unit] <- stri_trans_tolower(data[,aggregation_unit, drop = TRUE])
        nm[,aggregation_unit] <- as.character(stri_trans_tolower(nm[,aggregation_unit, drop = TRUE]))
        data[,aggregation_unit] = as.character(data[,aggregation_unit, drop = TRUE])
        data <- suppressWarnings(left_join(data, nm,c(aggregation_unit)))
        
        
      }else{
        
        if(is.null(facets)| isTRUE(facets == aggregation_unit))
        {
          data <- aggregate(x = data[,var],
                            by = list(var = data[, aggregation_unit, drop = TRUE]), FUN = aggregation_fun)
          colnames(data)[1] <- aggregation_unit
          
        }else{
          dt <- lapply(levels(factor(data[,facets, drop = TRUE])), function(x) aggregate(x = data[data[,facets, drop = TRUE]==x, var],
                                                                                         by = list(var = data[data[,facets, drop = TRUE]==x,aggregation_unit, drop = TRUE],
                                                                                                   var2 = data[data[,facets, drop = TRUE]==x,facets, drop = TRUE]),
                                                                                         FUN = aggregation_fun))
          
          data <- do.call("rbind", dt)
          
          colnames(data)[1] <- aggregation_unit
          colnames(data)[2] <- facets
        }
        
      }
      
      colID <- aggregation_unit
      
    }

  
  }
  
  
  
  
  
  if(type == "static")
  {
    
    if(typeStatic == "tmap")
    {
      
      mapping_tmap(data, var = var,
                   facets = facets, add_text = add_text,
                   options = options)
      
    }else if(typeStatic == "choro.cart")
    {
      mapping_choro(data = data, var = var, options = options)
      
    }else if(typeStatic == "typo")
    {
      
      mapping_typo(data = data, var = var, options = options)
      
    }else if(typeStatic == "bar")
    {
      
      mapping_bar(data = data, var = var, options = options)
      
    }
    
  }else if(type == "interactive"){
    if(!is.null(facets))
    {
      
      
      if(isFALSE(options$facets.free.scale))
      {
        plot_interactive_choro_facetes(data = data,
                                       var = var,
                                       colID = colID,
                                       facets = facets,
                                       options = options)
      }else{
        plot_interactive_choro_facetes_freeScale(data = data,
                                                 var = var,
                                                 colID = colID,
                                                 facets = facets,
                                                 options = options)
      }
      
    }else{
      
      plot_interactive_choro(data = data,
                             var = var,
                             colID = colID,
                             options = options)
    }
    
    
  }
  
  
}



