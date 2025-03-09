
loadCoordIT <- function(unit = c("none","ripartizione", "regione", "provincia","comune"),
                        year = c("2021", "2020", "2019", "2018", "2017"),
                        unit_subset = NULL, matchWith = NULL,
                        dir = NULL,
                        use_cache = TRUE, use_internet = TRUE, crs = NULL)
{

  year <- match.arg(year, choices = eval(formals(loadCoordIT)$year))
  unit <- match.arg(unit, choices = eval(formals(loadCoordIT)$unit))


  if(is.null(crs))
  {
    crs <- mapping.options()$crs
  }

  if(unit == "none")
  {
    file <- paste("it_coord",".geojson", sep = "")
    nn <- "IT_coord"

  }else{
    file <- paste("it_",year,"_",unit,".geojson", sep = "")
    nn <- paste(unit,year,sep = "_")
  }

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
      coord <- get(load(paste(dir,"it_","2020","_",unit, ".RData",sep = "")))

    }else{
      coord <- get(load(paste(dir,"/it_","2020","_",unit, ".RData",sep = "")))
    }

  }else{


  internet <- has_internet()

  if(internet | use_internet)
  {
    url <- paste("https://raw.githubusercontent.com/mappinguniverse/geospatial/master/IT/GeoJSON/", file, sep = "")
    response <- FALSE

    if(!response)
    {

      temp <- tempfile(pattern = nn, fileext = ".geojson")
      download.file(url, paste(tempdir(),file,sep = "/"))
      coord <- read_sf(dsn = paste(tempdir(),file,sep = "/"), as_tibble = FALSE)
    }
    }else{
      coord <-  get(paste("it_","2020","_", unit, sep = ""), pos = "package:mapping")
    }
}
  }else{

  coord <- read_sf(dsn = paste(tempdir(),file,sep = "/"), as_tibble = FALSE)

    }




  coord <- st_transform(coord, crs = crs)
  coord <- st_make_valid(coord)
  class(coord) <- c(class(coord),"IT")
  attributes(coord)$unit <- unit
  attributes(coord)$colID <- unit

  if(!is.null(unit_subset))
  {
    if(!is.null(matchWith))
    {
      if(matchWith == "name")
      {
        matchWith <- unit

      }else if(matchWith == "number")
      {
        matchWith <- paste("code", unit, sep = "_")

      }else if(matchWith == "code")
      {
        matchWith <- "code"
      }
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

checkNamesIT <- function(id,
                         unit = c("ripartizione", "regione", "provincia", "comune"),
                         year = c("2021", "2020", "2019", "2018", "2017"),
                         matchWith = c("name", "code", "number"),
                         return_logical = FALSE, print = TRUE, use_internet = TRUE)
{

  unit <- match.arg(unit, choices = eval(formals(checkNamesIT)$unit))
  year <- match.arg(year, choices = eval(formals(checkNamesIT)$year))
  matchWith <- match.arg(matchWith, choices = eval(formals(checkNamesIT)$matchWith))

  if(matchWith == "name")
  {
    matchWith <- unit

  }else if(matchWith == "number")
  {
    matchWith <- paste("code", unit, sep = "_")

  }else if(matchWith == "code")
  {
    matchWith <- "code"
  }

  coord <- loadCoordIT(unit = unit, year = year, use_internet = use_internet)
  Ita <- coord[[matchWith]]

  if(!is.null(ncol(id)))
  {
    warning("id must be vector. The first column is considered")
    id <- id[,1]
  }

  id <- tolower(id)
  Ita <- tolower(Ita)

  nomatch <- setdiff(x = id, y = Ita)

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
    nomatch <- id %in% Ita
  }

  return(nomatch)

}


IT <- function(data, colID = NULL,
               unit = c("none","ripartizione", "regione", "provincia", "comune"),
               year = c("2021", "2020","2019", "2018", "2017"),
               matchWith = c("name", "code","number"), show_it = TRUE,
               subset = NULL, add = NULL, new_var_names = NULL, aggregation_fun = sum, aggregation_unit = NULL, aggregation_var = NULL,
               facets = NULL, check.unit.names = TRUE, dir = NULL, use_cache = TRUE, print = FALSE, use_internet = TRUE, crs = NULL)
{


  matchWith <- match.arg(matchWith, choices = eval(formals(IT)$matchWith))
  unit <- match.arg(unit, choices = eval(formals(IT)$unit))




  if(inherits(data, "IT") & length(attributes(data)$unit == "none") == 0)
  {
    out <- data
    colName <- NULL
  }else if(!inherits(data, "IT") & unit == "none"){

    coord <- loadCoordIT(unit = unit, year = year, use_cache = use_cache, use_internet = use_internet, crs = crs)
    out <- coord
    colName <- NULL

  }else{

  if(inherits(data, "IT"))
  {


    colID <- attributes(data)$colID
    year <- attributes(data)$year
    unit <- attributes(data)$unit
    colName <- colnames(data)[colID]
    coord <- loadCoordIT(unit = unit, year = year, dir = dir, use_cache = use_cache, use_internet = use_internet, crs = crs)
    out <- data

  }else{

    unit <- match.arg(unit, choices = eval(formals(IT)$unit))
    year <- match.arg(year, choices = eval(formals(IT)$year))
    matchWith <- match.arg(matchWith, choices = eval(formals(IT)$matchWith))


    data <- data.frame(data, check.names = FALSE)

    if(is.null(crs))
    {
      crs <- mapping.options()$crs
    }

    if(matchWith == "name")
    {
      matchWith <- unit

    }else if(matchWith == "number")
    {
      matchWith <- paste("code", unit, sep = "_")

    }else if(matchWith == "code")
    {
      matchWith <- "code"
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

    coord <- loadCoordIT(unit = unit, year = year, use_cache = use_cache, use_internet = use_internet, crs = crs)

    if(!(unit == "none"))
    {

      coord[[matchWith]] <- tolower(coord[[matchWith]])
      data[,colID] <- tolower(data[,colID])

      if(check.unit.names)
      {
        Ita <- coord[[matchWith]]
        dd <- setdiff(x = data[,colID], y = Ita)
        if(!length(dd) == 0 & isTRUE(print))
        {
          warning(paste("No match found for vraibles: ", paste(dd, sep = ", ")) , call. = FALSE)
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

      out <- suppressMessages(left_join(coord, data, by = colName,keep = FALSE))

    }
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


      if(!any(aggregation_unit %in% colnames(out)))
      {

        stop("aggregation_unit name does not exit")

      }

      if(any(aggregation_unit%in% c("none","ripartizione", "regione", "provincia", "comune")))
      {
        nm <- getNamesIT(year = year, unit = aggregation_unit, all_levels = TRUE)

        if(unit == aggregation_unit)
        {
          colnames(nm)[which(colnames(nm) == aggregation_unit)] <- colName
          aggregation_unit <- colName
        }else{
          unit <- aggregation_unit
        }

        if(is.null(facets) | isTRUE(facets == aggregation_var))
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

        if(is.null(facets) | isTRUE(facets == aggregation_var))
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
      if(isFALSE(show_it))
      {

        out <- out[out[,colName, drop = TRUE] %in% data[,colName],]

      }else{

        out <- suppressMessages(left_join(coord, out[,-ncol(out), drop = TRUE]))
      }
    }
  }


  class(out) <- c( class(out),"IT")
  attributes(out)$unit <- unit
  attributes(out)$year <- year
  attributes(out)$colID <- colName
  return(out)
}

