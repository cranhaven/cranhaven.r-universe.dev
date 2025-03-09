
loadCoordEU <- function(unit = c("nuts0","nuts1", "nuts2", "nuts3", "urau"),
                        year = c("2021","2016", "2013", "2010", "2006", "2003"),
                        scale = c("20", "60"),
                        unit_subset = NULL, matchWith = NULL, dir = NULL,
                        use_cache = TRUE, use_internet = TRUE, crs = NULL)
{

  year <- match.arg(year, choices = eval(formals(loadCoordEU)$year))
  unit <- match.arg(unit, choices = eval(formals(loadCoordEU)$unit))
  scale <- match.arg(scale, choices = eval(formals(loadCoordEU)$scale))

  if(is.null(crs))
  {
    crs <- mapping.options()$crs
  }

  if(unit == "urau")
  {
    scale = "100"
    year = "2018"
  }

  file <- paste("eu_",year,"_",unit, "_",scale,"m",".geojson", sep = "")
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
        coord <- get(load(paste(dir, "eu_",year,"_",unit, "_",scale,"m",".RData", sep = "")))

      }else{
        coord <- get(load(paste(dir, "/eu_",year,"_",unit, "_",scale,"m",".RData", sep = "")))
      }

    }else{
      if(internet | use_internet)
      {
        if(unit == "urau")
        {
          url <- paste("https://raw.githubusercontent.com/mappinguniverse/geospatial/master/EU/urau/", file, sep = "")
        }else{
          url <- paste("https://raw.githubusercontent.com/mappinguniverse/geospatial/master/EU/nuts/GeoJSON/", file, sep = "")
        }
        response <- FALSE

        if(!response)
        {

          temp <- tempfile(pattern = nn, fileext = ".geojson")
          download.file(url, paste(tempdir(),file,sep = "/"))
          coord <- read_sf(dsn = paste(tempdir(),file,sep = "/"), as_tibble = FALSE)

        }
        }else{
          coord <-  get(paste("eu_","2016","_",unit, "_","60m", sep = ""), pos = "package:mapping")
        }
}
      }
    else{

      coord <- read_sf(dsn = paste(tempdir(),file,sep = "/"), as_tibble = FALSE)

    }




  coord <- st_transform(coord, crs = crs)
  coord <- st_make_valid(coord)
  class(coord) <- c(class(coord),"EU")
  attributes(coord)$unit <- unit
  attributes(coord)$colID <- unit

  if(!is.null(unit_subset))
  {
    if(is.null(matchWith))
    {
      matchWith <- unit
    }

    unit_subset <- tolower(unit_subset)
    unt <- tolower(coord[,matchWith, drop = TRUE])
    coord <- coord[ unt %in% unit_subset, ]
  }

  return(coord)
}

checkNamesEU <- function(id,
                         unit = c("nuts0","nuts1", "nuts2", "nuts3", "urau"),
                         year = c("2021","2016", "2013", "2010", "2006", "2003"),
                         matchWith = c("nuts", "id", "iso2", "iso3", "country_code"),
                         scale = c("20", "60"), return_logical = FALSE, print = TRUE, use_internet = TRUE)
{

  unit <- match.arg(unit, choices = eval(formals(checkNamesEU)$unit))
  year <- match.arg(year, choices = eval(formals(checkNamesEU)$year))
  matchWith <- match.arg(matchWith, choices = eval(formals(checkNamesEU)$matchWith))
  scale <- match.arg(scale, choices = eval(formals(checkNamesEU)$scale))

  if(unit == "urau")
  {
    scale = "100"
    year = "2018"
  }

  if(matchWith == "nuts")
  {
    matchWith <- unit
  }

  coord <- loadCoordEU(unit = unit, year = year, scale = scale)
  Eu <- coord[[matchWith]]

  if(!is.null(ncol(id)))
  {
    warning("x must be vector. The first column is considered")
    id <- id[,1]
  }

  id <- tolower(id)
  Eu <- tolower(Eu)

  nomatch <- setdiff(x = id, y = Eu)

  ## NON FUNZIONE. SISTEMARE!!!!!!!!!!!!!!!1

  # if(any(nomatch))
  # {
  #   warning(paste(x[nomatch], ", ") , call. = FALSE)
  # }

  if(!length(nomatch) == 0 & isTRUE(print))
  {
    warning(paste("No match found for variables: ", paste(nomatch, sep = ", ")) , call. = FALSE)
  }

  if(isTRUE(return_logical))
  {
    nomatch <- id %in% Eu
  }


  return(nomatch)

}


EU <- function(data, colID = NULL,
               unit = c("nuts0","nuts1", "nuts2", "nuts3", "urau"),
               year = c("2021","2016", "2013", "2010", "2006", "2003"),
               matchWith = c("nuts", "id", "iso2", "iso3", "country_code"),
               scale = c("20", "60"), show_eu = TRUE, subset = NULL, add = NULL, new_var_names = NULL,
               aggregation_fun = sum, aggregation_unit = NULL, aggregation_var = NULL,
               facets = NULL, check.unit.names = TRUE, dir = NULL,
               use_cache = TRUE, print = FALSE, use_internet = TRUE, crs = NULL)
{

  scale <- match.arg(scale, choices = eval(formals(EU)$scale))
  matchWith <- match.arg(matchWith, choices = eval(formals(EU)$matchWith))

  if(inherits(data, "EU"))
  {

    colID <- attributes(data)$colID
    year <- attributes(data)$year
    unit <- attributes(data)$unit
    colName <- colnames(data)[colID]
    coord <- loadCoordEU(unit = unit, year = year, scale = scale, use_cache = use_cache, crs = crs)
    out <- data

  }else{

    unit <- match.arg(unit, choices = eval(formals(EU)$unit))
    year <- match.arg(year, choices = eval(formals(EU)$year))

    data <- data.frame(data, check.names = FALSE)

    if(is.null(crs))
    {
      crs <- mapping.options()$crs
    }

    if(matchWith == "nuts")
    {
      matchWith <- unit
    }


    if(unit == "urau")
    {
      scale = "100"
      year = "2018"
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



    coord <- loadCoordEU(unit = unit, year = year, scale = scale, use_cache = use_cache, crs = crs, dir = dir)
    coord[[matchWith]] <- tolower(coord[[matchWith]])
    data[,colID] <- tolower(data[,colID])


    if(check.unit.names)
    {
      Eu <- coord[[matchWith]]
      dd <- setdiff(x = data[,colID, drop = TRUE], y = Eu)
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
      if(any(aggregation_unit%in%c("nuts0","nuts1", "nuts2", "nuts3", "urau")))
      {
        nm <- getNamesEU(year = year, unit = aggregation_unit, all_levels = TRUE)

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
          class(out) <- c(class(out),"EU")

          colnames(out)[1] <- aggregation_unit

          if(any(facets %in% colnames(nm)))
          {
            facets <- paste(facets,"_facets", sep = "")
          }

          colnames(out)[2] <- facets
          facets_join <- facets

        }
        out[,aggregation_unit] <- tolower(out[,aggregation_unit, drop = TRUE])
        nm[,aggregation_unit] <- as.character(tolower(nm[,aggregation_unit, drop = TRUE]))
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
      if(isFALSE(show_eu))
      {

        out <- out[out[,colName, drop = TRUE] %in% data[,colName],]

      }else{

        out <- suppressMessages(left_join(coord, out[,-ncol(out), drop = TRUE]))
      }
    }


  class(out) <- c(class(out),"EU")
  attributes(out)$unit <- unit
  attributes(out)$year <- year
  attributes(out)$colID <- colName

  return(out)
}

