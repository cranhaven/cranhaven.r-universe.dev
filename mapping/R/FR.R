
loadCoordFR <- function(unit = c("region"),
                        year = c("2021", "2020", "2019"),
                        unit_subset = NULL, matchWith = NULL, dir = NULL,
                        use_cache = TRUE, use_internet = TRUE, crs = NULL)
{
  
  year <- match.arg(year, choices = eval(formals(loadCoordFR)$year))
  unit <- match.arg(unit, choices = eval(formals(loadCoordFR)$unit))

    
  if(is.null(crs))
  {
    crs <- mapping.options()$crs
  }
  
  
  file <- paste("fr_",year,"_",unit,".geojson", sep = "")
  nn <- paste(unit,year,sep = "_")
  
  download <- TRUE
  
  if(use_cache)
  {
    fl <- list.files(tempdir())
    ck <- startsWith(file, fl)
    download <- !any(ck)
    
  }
  
  internet <- has_internet()
  
  if(download)
  {
    
    if(!is.null(dir))
    {
      
      if(substr(dir, str_length(dir),str_length(dir)) == "/")
      {
        coord <- get(load(paste(dir, "fr_",year,"_",unit,".RData", sep = "")))
        
      }else{
        coord <- get(load(paste(dir, "/fr_",year,"_",unit,".RData", sep = "")))
      }
      
    }else{
      if(internet | use_internet)
      {
        url <- paste("https://raw.githubusercontent.com/mappinguniverse/geospatial/master/FR/GeoJSON/", file, sep = "")
        response <- FALSE
        
        if(!response)
        {
          
          temp <- tempfile(pattern = nn, fileext = ".geojson")
          download.file(url, paste(tempdir(),file,sep = "/"))
          coord <- read_sf(dsn = paste(tempdir(),file,sep = "/"), as_tibble = FALSE)
          
        }
      }else{
        coord <-  get(paste("fr_","2021","_",unit, sep = ""), pos = "package:mapping")
      }
    }
  }
  else{
    
    coord <- read_sf(dsn = paste(tempdir(),file,sep = "/"), as_tibble = FALSE)
    
  }
  
  
  
  
  coord <- st_transform(coord, crs = crs)
  coord <- st_make_valid(coord)
  class(coord) <- c(class(coord),"FR")
  attributes(coord)$unit <- unit
  attributes(coord)$colID <- unit
  
  if(!is.null(unit_subset))
  {
    if(is.null(matchWith))
    {
      matchWith <- unit
    }
    
    unit_subset <- stri_trans_tolower(unit_subset)
    unt <- stri_trans_tolower(coord[,matchWith, drop = TRUE])
    coord <- coord[ unt %in% unit_subset, ]
  }
  
  return(coord)
}


FR <- function(data, colID = NULL,
               unit = c("region"),
               year = c("2021", "2020", "2019"),
               matchWith = c("name", "code"),
               subset = NULL, add = NULL, new_var_names = NULL,
               aggregation_fun = sum, aggregation_unit = NULL, aggregation_var = NULL,
               facets = NULL, check.unit.names = TRUE, dir = NULL,
               use_cache = TRUE, print = FALSE, use_internet = TRUE, crs = NULL)
{
  

  matchWith <- match.arg(matchWith, choices = eval(formals(FR)$matchWith))
  
  if(inherits(data, "FR"))
  {
    
    colID <- attributes(data)$colID
    year <- attributes(data)$year
    unit <- attributes(data)$unit
    colName <- colnames(data)[colID]
    coord <- loadCoordFR(unit = unit, year = year, use_cache = use_cache, crs = crs)
    out <- data
    
  }else{
    
    unit <- match.arg(unit, choices = eval(formals(FR)$unit))
    year <- match.arg(year, choices = eval(formals(FR)$year))
    
    data <- data.frame(data, check.names = FALSE)
    
    if(is.null(crs))
    {
      crs <- mapping.options()$crs
    }
    
    
    if(matchWith == "name")
    {
      matchWith <- unit
      
    }
    
    
    
    if(is.null(colID))
    {
      colID <- 1
      
    }else{
      
      if(is.character(colID))
      {
        
        colID <- which(names(data) == colID)
        if(length(colID)==0)
        {
          stop("colID name does not exist.", call. = FALSE)
        }
      }
    }
    
    
    
    coord <- loadCoordFR(unit = unit, year = year, use_cache = use_cache, crs = crs, dir = dir)
    coord[[matchWith]] <- stri_trans_tolower(coord[[matchWith]])
    data[,colID] <- stri_trans_tolower(data[,colID])
    
    
    if(check.unit.names)
    {
      Fr <- coord[[matchWith]]
      dd <- setdiff(x = data[,colID, drop = TRUE], y = Fr)
      if(!length(dd) == 0 & isTRUE(print))
      {
        warning(paste("No match found for variables: ", paste(dd, sep = ", ")) , call. = FALSE)
      }
    }
    
    
    if(!is.null(add))
    {
      
      if(!inherits(add, "formula"))
      {
        stop("add must be a formula.")
      }
      
      data <- add.formula(data = data, formula = add, names = new_var_names)
    }
    
    
    colName <- colnames(data)[colID]
    wh_colName <- colnames(coord) == matchWith
    
    wh_ck <- colnames(coord) %in% colnames(data)
    
    if( sum(wh_ck) >= 1)
    {
      colnames(coord)[wh_ck] <- paste(colnames(coord)[wh_ck], "_load", sep = "")
    }
    
    
    colnames(coord)[wh_colName] <- colName
    
    
    out <- suppressMessages(left_join(coord, data, by = colName, keep = FALSE))
    
  }
  
  
  
  if(!is.null(subset))
  {
    if(!inherits(subset, "formula"))
    {
      stop("subset must be a formula.")
    }
    out <- subset_formula(data = out, formula = subset)
  }
  
  if(!is.null(aggregation_unit))
  {
    if(is.null(aggregation_fun))
    {
      stop("aggregation_fun must be provided to aggregate data")
    }
    
    if(is.null(aggregation_var))
    {
      stop("aggregation_var must be provided to aggregate data")
    }else{
      if(is.numeric(aggregation_var))
      {
        aggregation_var <- colnames(data)[aggregation_var]
      }else{
        if(!any(sapply(aggregation_var, function(x) x == colnames(out))))
        {
          stop("Column names aggregation_var does not exist.", call. = FALSE)
        }
      }
    }
    
    
    year <- year
    if(any(aggregation_unit%in%c("region")))
    {
      nm <- getNamesFR(year = year, unit = aggregation_unit, all_levels = TRUE)
      
      if(unit == aggregation_unit)
      {
        colnames(nm)[which(colnames(nm) == aggregation_unit)] <- colName
        aggregation_unit <- colName
      }else{
        unit <- aggregation_unit
      }
      
      
      if(is.null(facets)| isTRUE(facets == aggregation_var))
      {
        
        out <- aggregate(x = out[,aggregation_var],
                         by = list(var = out[, aggregation_unit, drop = TRUE]), FUN = aggregation_fun)
        colnames(out)[1] <- aggregation_unit
        facets_join <- NULL

      }else{
        
        if(!any(facets %in% colnames(out)))
        {
          stop("facets name does not exit")
        }
        
        dt <- lapply(levels(factor(out[,facets, drop = TRUE])), function(x) aggregate(x = out[out[,facets, drop = TRUE]==x, aggregation_var],
                                                                                      by = list(var = out[out[,facets, drop = TRUE]==x,aggregation_unit, drop = TRUE],
                                                                                                var2 = out[out[,facets, drop = TRUE]==x,facets, drop = TRUE]),
                                                                                      FUN = aggregation_fun))
        
        out <- do.call("rbind", dt)
        class(out) <- c(class(out),"FR")
        
        colnames(out)[1] <- aggregation_unit
        
        if(any(facets %in% colnames(nm)))
        {
          facets <- paste(facets,"_facets", sep = "")
        }
        
        colnames(out)[2] <- facets
        facets_join <- facets
        
      }
      out[,aggregation_unit] <- stri_trans_tolower(out[,aggregation_unit, drop = TRUE])
      nm[,aggregation_unit] <- as.character(stri_trans_tolower(nm[,aggregation_unit, drop = TRUE]))
      out[,aggregation_unit] = as.character(out[,aggregation_unit, drop = TRUE])
      out <- suppressWarnings(left_join(out, nm,c(aggregation_unit)))
      
      
    }else{
      
      if(is.null(facets)| isTRUE(facets == aggregation_var))
      {
        
        out <- aggregate(x = out[,aggregation_var],
                         by = list(var = out[, aggregation_unit, drop = TRUE]), FUN = aggregation_fun)
        colnames(out)[1] <- aggregation_unit
      }else{
        
        out <- lapply(levels(factor(out[,facets, drop = TRUE])), function(x) aggregate(x = out[out[,facets, drop = TRUE]==x, aggregation_var],
                                                                                       by = list(var = out[out[,facets, drop = TRUE]==x,aggregation_unit, drop = TRUE],
                                                                                                 var2 = out[out[,facets, drop = TRUE]==x,facets, drop = TRUE]),
                                                                                       FUN = aggregation_fun))
        
        out <- do.call("rbind", dt)
        
        colnames(out)[1] <- aggregation_unit
        colnames(out)[2] <- facets
        
      }
      
    }
    
    colID <- aggregation_unit
    colName <- aggregation_unit
    
  }else{
    
    
    out <- suppressMessages(left_join(coord, out[,-ncol(out), drop = TRUE]))
    
  }
  
  
  class(out) <- c(class(out),"FR")
  attributes(out)$unit <- unit
  attributes(out)$year <- year
  attributes(out)$colID <- colName
  
  return(out)
}

