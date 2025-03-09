loadCoordWR <- function(unit = c("country", "nato", "ocde", "continent", "region", "subregion", "region_wb", "type_income", "type_economy"),
                        res = c("low", "hi"),
                        unit_subset = NULL, matchWith = NULL, dir = NULL,
                        use_cache = TRUE, use_internet = TRUE, crs = NULL)
{

  res <- match.arg(res, choices = eval(formals(loadCoordWR)$res))
  unit <- match.arg(unit, choices = eval(formals(loadCoordWR)$unit))

  if(is.null(crs))
  {
    crs <- mapping.options()$crs
  }


  file <- paste("world_",unit,"_",res,".geojson", sep = "")
  nn <- paste("world_",unit,"_", res,sep = "")


  download <- TRUE
  if(use_cache)
  {
    fl <- list.files(tempdir())
    ck <- startsWith(file, fl)
    download <- !any(ck)

  }

if(download){

  if(!is.null(dir))
  {

    if(substr(dir, str_length(dir),str_length(dir)) == "/")
    {
      coord <- get(load(paste(dir, "world_",unit,"_",res,".RData", sep = "")))

    }else{
      coord <- get(load(paste(dir, "/world_",unit,"_",res,".RData", sep = "")))
    }

  }else
  {
    internet <- has_internet()

    if(internet | use_internet)
    {
      url <- paste("https://raw.githubusercontent.com/mappinguniverse/geospatial/master/world/GeoJSON/", file, sep = "")
      response <- FALSE

      if(!response)
      {

        temp <- tempfile(pattern = nn, fileext = ".geojson")
        download.file(url, paste(tempdir(),file,sep = "/"))
        coord <- read_sf(dsn = paste(tempdir(),file,sep = "/"), as_tibble = FALSE)

      }
      }else{
        coord <-  get("world_low", pos = "package:mapping")
      }
}
    }else{
    coord <- read_sf(dsn = paste(tempdir(),file,sep = "/"), as_tibble = FALSE)

    }


  coord <- st_transform(coord, crs = crs)
  coord <- st_make_valid(coord)
  class(coord) <- c(class(coord),"WR")
  attributes(coord)$unit <- unit
  attributes(coord)$colID <- unit

  if(!is.null(unit_subset))
  {

    if(unit != "country" | unit != "nato" | unit != "ocde")
    {
      matchWith <- unit
    }else
    {
      matchWith <- unit
    }

    unit_subset <- tolower(unit_subset)
    unt <- tolower(coord[,matchWith, drop = TRUE])
    coord <- coord[ unt %in% unit_subset, ]
  }

  return(coord)
}

checkNamesWR <- function(id,
                        unit = c("country", "nato", "ocde", "continent", "region", "subregion", "region_wb", "type_income", "type_economy"),
                        matchWith = c("country", "iso2", "iso3", "iso3_eh", "iso3_numeric", "iso3_un", "iso2_wb", "iso3_wb", "name_formal", "name_wb"),
                        res = c("low", "hi"), return_logical = FALSE, print = TRUE, use_internet = TRUE)
{


  matchWith <- match.arg(matchWith, choices = eval(formals(checkNamesWR)$matchWith))
  res <- match.arg(res, choices = eval(formals(checkNamesWR)$res))
  unit <- match.arg(unit, choices = eval(formals(checkNamesWR)$unit))

  coord <- loadCoordWR(unit = unit, res = res, use_internet = use_internet)

  if(unit != "country" | unit != "nato" | unit != "ocde")
  {
    matchWith <- unit
  }

  Wr <- coord[[matchWith]]

  if(!is.null(ncol(id)))
  {
    warning("id must be vector. The first column is considered")
    id <- id[,1]
  }

  id <- tolower(id)
  Wr <- tolower(id)

  nomatch <- setdiff(x = id, y = Wr)

  ## NON FUNZIONA SISTEMARE!!!!!!!!!!!!!!!1

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
    nomatch <- id %in% Wr
  }

  return(nomatch)

}


WR <- function(data, colID = NULL,
               unit = c("country", "nato", "ocde", "continent", "region", "subregion", "region_wb", "type_income", "type_economy"),
               matchWith = c("country", "iso2", "iso3", "iso3_eh", "iso3_numeric", "iso3_un", "iso2_wb", "iso3_wb", "name_formal", "name_wb"),
               res = c("low", "hi"), show_wr = TRUE, subset = NULL, add = NULL, new_var_names = NULL,
               aggregation_fun = sum, aggregation_unit = NULL, aggregation_var = NULL,
               facets = NULL, check.unit.names = TRUE, dir = NULL, use_cache = TRUE, print = FALSE, use_internet = TRUE, crs = NULL)
{

  res <- match.arg(res, choices = eval(formals(WR)$res))
  matchWith <- match.arg(matchWith, choices = eval(formals(WR)$matchWith))

  if(inherits(data, "WR"))
  {

    colID <- attributes(data)$colID
    year <- attributes(data)$year
    unit <- attributes(data)$unit
    colName <- colnames(data)[colID]
    coord <- loadCoordWR(unit = unit, res = res, use_cache = use_cache, use_internet = use_internet, crs = crs)
    out <- data

  }else{
    unit <- match.arg(unit, choices = eval(formals(WR)$unit))


    data <- data.frame(data, check.names = FALSE)


    if(is.null(crs))
    {
      crs <- mapping.options()$crs
    }


    if(is.null(colID))
    {
      colID <- 1

    }else{

      if(is.character(colID))
      {

        colID <- which(names(data) == colID)

      }
    }


    coord <- loadCoordWR(unit = unit, res = res, use_cache = use_cache, use_internet = use_internet, crs = crs, dir = dir)
    coord[[matchWith]] <- tolower(coord[[matchWith]])
    data[,colID] <- tolower(data[,colID])

    if(check.unit.names)
    {
      wr <- coord[[matchWith]]
      dd <- setdiff(x = data[,colID, drop = TRUE], y = wr)
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


    out <- suppressMessages(left_join(coord, data, by = (colName)))

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


      #year <- attributes(data)$year
      if(any(aggregation_unit %in% c("country", "nato", "ocde", "continent",
                                     "region", "subregion", "region_wb", "type_income",
                                     "type_economy")))
      {
        nm <- getNamesWR()
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
          class(out) <- c(class(out),"WR")

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
        out <- suppressWarnings(left_join(out, nm,c(aggregation_unit, facets_join)))


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

      if(isFALSE(show_wr))
      {
        
        out <- out[out[,colName, drop = TRUE] %in% data[,colName],]

      }else{

        out <- suppressMessages(left_join(coord, out[,-ncol(out), drop = TRUE]))
      }
    }


  class(out) <- c(class(out),"WR")
  attributes(out)$unit <- unit
  attributes(out)$colID <- colName

  return(out)
}


