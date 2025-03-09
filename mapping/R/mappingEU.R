mappingEU <- function(data, var = NULL, colID = NULL,
                      type = c("static", "interactive"),
                      typeStatic = c("tmap", "choro.cart", "typo", "bar"),
                      unit = c("nuts0","nuts1", "nuts2", "nuts3", "urau"),
                      year = c("2021","2016", "2013", "2010", "2006", "2003"),
                      matchWith = c("nuts", "id", "iso2", "iso3", "country_code"),
                      scale = c("20", "60"), dir = NULL,
                      show_eu = TRUE, add_text = NULL, subset = NULL,
                      facets = NULL, aggregation_fun = sum, aggregation_unit = NULL,
                      options = mapping.options())

{


  check.unit.names = options$check.unit.names
  use_cache = options$use_cache
  use_internet = options$use_internet
  type <- match.arg(type, choices = eval(formals(mappingEU)$type))
  typeStatic <- match.arg(typeStatic, choices = eval(formals(mappingEU)$typeStatic))


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

  if(!inherits(data, "EU"))
  {


    data <- data.frame(data, check.names = FALSE)


    unit <- match.arg(unit, choices = eval(formals(mappingEU)$unit))
    year <- match.arg(year, choices = eval(formals(mappingEU)$year))
    scale <- match.arg(scale, choices = eval(formals(mappingEU)$scale))
    matchWith <- match.arg(matchWith, choices = eval(formals(mappingEU)$matchWith))

    if(unit == "urau")
    {
      scale = "100"
      year = "2018"
    }


    data <- EU(data = data, colID = colID, unit = unit, year = year, subset = subset, show_eu = show_eu,
               matchWith = matchWith, scale = scale, check.unit.names = check.unit.names,
               use_cache = use_cache, use_internet = use_internet, crs = options$crs,
               aggregation_fun = aggregation_fun, aggregation_unit = aggregation_unit, aggregation_var = var)

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


      if(any(aggregation_unit%in%c("nuts0","nuts1", "nuts2", "nuts3", "urau")))
      {
        unit <- aggregation_unit
        nm <- getNamesEU(year = year, unit = unit, all_levels = TRUE)

        if(attributes(data)$unit == aggregation_unit)
        {
          colnames(nm)[which(colnames(nm) == aggregation_unit)] <- attributes(data)$colID
          aggregation_unit <- attributes(data)$colID
        }

        if(is.null(facets) | isTRUE(facets == aggregation_unit))
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
          class(data) <- c(class(data),"EU")

          colnames(data)[1] <- aggregation_unit

          if(any(facets %in% colnames(nm)))
          {
            facets <- paste(facets,"_facets", sep = "")
          }

          colnames(data)[2] <- facets
          facets_join <- facets

        }

        data[,aggregation_unit] <- tolower(data[,aggregation_unit, drop = TRUE])
        nm[,aggregation_unit] <- as.character(tolower(nm[,aggregation_unit, drop = TRUE]))
        data[,aggregation_unit] = as.character(data[,aggregation_unit, drop = TRUE])
        data <- suppressWarnings(left_join(data, nm,c(aggregation_unit)))


      }else{





        if(is.null(facets) | isTRUE(facets == aggregation_unit))
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

