
loadCoordUS <- function(unit = c("country","region", "division","state",
                                 "county", "district", "district_county","urban_area"),
                        year = c("2018"),  scale = c("20", "50", "500"),
                        unit_subset = NULL, matchWith = NULL, dir = NULL,
                        use_cache = TRUE, use_internet = TRUE, crs = NULL)
{

  year <- match.arg(year, choices = eval(formals(loadCoordUS)$year))
  unit <- match.arg(unit, choices = eval(formals(loadCoordUS)$unit))
  scale <- match.arg(scale, choices = eval(formals(loadCoordUS)$scale))


  if(is.null(crs))
  {
    crs <- mapping.options()$crs
  }

  if(scale == 500 & unit == "country")
  {
    scale = 50
  }

  if(unit == "district_county")
  {
    scale = 500
  }


  if(scale =="500")
  {
    scale <- paste(scale,"k", sep = "")
  }else{
    scale <- paste(scale,"m", sep = "")

  }


  if(unit == "country")
  {
    file <- paste("us_",year,"_",scale,".geojson", sep = "")

  }else if(unit == "urban_area")
  {
    file <- paste("us_",year,"_",unit,".geojson", sep = "")

  }else{
    file <- paste("us_",year,"_",unit, "_",scale,".geojson", sep = "")

  }

  nn <- paste(unit,year,sep = "_")

  ck <- TRUE

  download <- TRUE

  if(use_cache)
  {
    fl <- list.files(tempdir())
    ck <- startsWith(file, fl)
    download <- !any(ck)

  }


if(download)
  {

  if(!is.null(dir))
  {


    if(substr(dir, str_length(dir),str_length(dir)) == "/")
    {
      coord <- get(load(paste(dir, "us_",year,"_",unit, "_",scale,".RData", sep = "")))

    }else{
      coord <- get(load(paste(dir, "/us_",year,"_",unit, "_",scale,".RData", sep = "")))
    }


  }else{
    internet <- has_internet()

    if(internet | use_internet)
    {
      url <- paste("https://raw.githubusercontent.com/mappinguniverse/geospatial/master/US/GeoJSON/", file, sep = "")
      response <- FALSE

      if(!response)
      {

        temp <- tempfile(pattern = nn, fileext = ".geojson")
        download.file(url, paste(tempdir(),file,sep = "/"))
        coord <- read_sf(dsn = paste(tempdir(),file,sep = "/"), as_tibble = FALSE)

      }
      }else{
        coord <-  get(paste("us_","2018","_",unit, "_","20m",".geojson", sep = ""), pos = "package:mapping")
      }
}
    }else{
    coord <- read_sf(dsn = paste(tempdir(),file,sep = "/"), as_tibble = FALSE)
  }


  coord <- st_transform(coord, crs = crs)
  coord <- st_make_valid(coord)
  class(coord) <- c(class(coord),"US")
  attributes(coord)$unit <- unit
  attributes(coord)$colID <- ifelse(unit == "district_county", "district", unit)

  if(!is.null(unit_subset))
  {
    if(unit == "state")
    {

      if(is.null(matchWith))
      {
        matchWith <- unit

      }else if(matchWith != "name")
      {
        matchWith <- paste(unit, "_", matchWith, sep = "")

      }

    }else{
      matchWith <- unit
    }

    unit_subset <- tolower(unit_subset)
    unt <- tolower(coord[,matchWith, drop = TRUE])
    coord <- coord[ unt %in% unit_subset, ]
  }

  return(coord)
}

checkNamesUS <- function(id,
                         unit = c("country","region", "division",
                                  "state", "county", "district", "district_county",
                                  "urban_area"),
                         year = c("2018"),
                         matchWith = c("name", "id", "number"),
                         scale = c("20", "50", "500"), return_logical = FALSE, print = TRUE, use_internet = TRUE)
{

  unit <- match.arg(unit, choices = eval(formals(checkNamesUS)$unit))
  year <- match.arg(year, choices = eval(formals(checkNamesUS)$year))
  matchWith <- match.arg(matchWith, choices = eval(formals(checkNamesUS)$matchWith))
  scale <- match.arg(scale, choices = eval(formals(checkNamesUS)$scale))


  if(unit == "state")
  {
    if(matchWith != "name")
    {
      matchWith <- paste(unit, "_", matchWith, sep = "")

    }else{
      matchWith <- unit
    }

  }else{
    matchWith <- unit
  }

  coord <- loadCoordUS(unit = unit, year = year, scale = scale, use_internet = use_internet)
  Us <- coord[[matchWith]]

  if(!is.null(ncol(id)))
  {
    warning("x must be vector. The first column is considered")
    id <- id[,1]
  }

  id <- tolower(id)
  Us <- tolower(Us)

  nomatch <- setdiff(x = id, y = Us)

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
    nomatch <- id %in% Us
  }


  return(nomatch)

}

US <- function(data, colID = NULL,
               unit = c("country","region", "division","state", "county",
                        "district", "district_county","urban_area"),
               year = c("2018"),
               matchWith = c("name", "id", "number"),
               scale = c("20", "50", "500"), show_us = TRUE,
               subset = NULL, add = NULL, new_var_names = NULL, aggregation_fun = sum, aggregation_unit = NULL, aggregation_var = NULL,
               facets = NULL, check.unit.names = TRUE, dir = NULL, use_cache = TRUE,
               print = FALSE, use_internet = TRUE, crs = NULL)
{

  scale <- match.arg(scale, choices = eval(formals(US)$scale))
  matchWith <- match.arg(matchWith, choices = eval(formals(US)$matchWith))


  if(inherits(data, "US"))
  {

    colID <- attributes(data)$colID
    year <- attributes(data)$year
    unit <- attributes(data)$unit
    colName <- colnames(data)[colID]
    coord <- loadCoordUS(unit = unit, year = year, scale = scale, use_cache = use_cache, use_internet = use_internet, crs = crs, dir = dir)
    out <- data

  }else{
    unit <- match.arg(unit, choices = eval(formals(US)$unit))
    year <- match.arg(year, choices = eval(formals(US)$year))
    scale <- match.arg(scale, choices = eval(formals(US)$scale))
    matchWith <- match.arg(matchWith, choices = eval(formals(US)$matchWith))

    data <- data.frame(data, check.names = FALSE)

    if(is.null(crs))
    {
      crs <- mapping.options()$crs
    }

    check_county <- unit == "county"

    if(unit == "state")
    {
      if(matchWith != "name")
      {
        matchWith <- paste(unit, "_", matchWith, sep = "")

      }else{
        matchWith <- unit
      }

    }else{
      matchWith <- unit
    }

    if(is.null(colID))
    {

      if(check_county)
      {
        colID <- c(1,2)

      }else{

        colID <- 1

      }


    }else if(is.character(colID)){

      if(check_county)
      {

        if(length(colID) != 2)
        {
          stop("coldID must be of length 2.", call. = FALSE)

        }else{

          colID <- match(colID,colnames(data) )

        }

      }else{
        colID <- which(colnames(data) == colID)
        if(length(colID)==0)
        {
          stop("colID name does not exist.", call. = FALSE)
        }
      }

    }



    coord <- loadCoordUS(unit = unit, year = year, scale = scale, use_cache = use_cache, use_internet = use_internet, crs = crs)
    coord[[matchWith]] <- tolower(coord[[matchWith]])
    data[,colID] <- tolower(data[,colID])


    if(check.unit.names)
    {
      Us <- coord[[matchWith]]

      if(check.unit.names)
      {
        if(check_county)
        {
          data[,colID[2]] <- tolower(data[,colID[2]])
          dd <- setdiff(x = data[,colID[2], drop = TRUE], y = Us)

        }else{

          dd <- setdiff(x = data[,colID, drop = TRUE], y = Us)

        }

        if(!length(dd) == 0 & isTRUE(print))
        {
          warning(paste("No match found for variables: ", paste(dd, sep = ", ")) , call. = FALSE)
        }

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

        if(any(aggregation_unit%in%c("country","region", "division","state", "county",
                                     "district", "district_county","urban_area")))
        {
          nm <- getNamesUS(year = year, unit = aggregation_unit, all_levels = TRUE)

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
            class(out) <- c(class(out),"IT")

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
        if(isFALSE(show_us))
        {
          out <- out[out[,colName, drop = TRUE] %in% data[,colName],]

        }else{

          out <- suppressMessages(left_join(coord, out[,-ncol(out), drop = TRUE]))
        }
      }


  class(out) <- c(class(out),"US")
  attributes(out)$unit <- unit
  attributes(out)$year <- year
  attributes(out)$colID <- colName

  return(out)
}

