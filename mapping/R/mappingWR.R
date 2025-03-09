
mappingWR <- function(data, var = NULL, colID = NULL,
                      type = c("static", "interactive"),
                      typeStatic = c("tmap", "choro.cart", "typo", "bar"),
                      unit = c("country", "nato", "ocde",
                               "continent", "region", "subregion", "region_wb",
                               "type_income", "type_economy"),
                      matchWith = c("country", "iso2", "iso3", "iso3_eh", "iso3_numeric", "iso3_un", "iso2_wb", "iso3_wb", "name_formal", "name_wb"),
                      res = c("low", "hi"), dir = NULL,
                      show_wr = TRUE, add_text = NULL, subset = NULL,
                      facets = NULL, aggregation_fun = sum, aggregation_unit = NULL,
                      options = mapping.options(legend.position = c("left","bottom")))
{

  check.unit.names = options$check.unit.names
  use_cache = options$use_cache
  use_internet = options$use_internet
  type <- match.arg(type, choices = eval(formals(mappingWR)$type))
  typeStatic <- match.arg(typeStatic, choices = eval(formals(mappingWR)$typeStatic))


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


  if(!inherits(data, "WR"))
  {

    data <- data.frame(data, check.names = FALSE)

    res <- match.arg(res, choices = eval(formals(mappingWR)$res))
    matchWith <- match.arg(matchWith, choices = eval(formals(mappingWR)$matchWith))

    data <- WR(data = data, colID = colID, subset = subset, show_wr = show_wr,
               matchWith = matchWith, res = res, check.unit.names = check.unit.names,
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


    if(!is.null(aggregation_unit) )
    {

      if(is.null(aggregation_fun))
      {
        stop("aggregation_fun must be provided to aggregate data")
      }

      if(any(aggregation_unit %in% c("country", "nato", "ocde",
                                 "continent", "region", "subregion", "region_wb",
                                 "type_income", "type_economy")))
      {
      unit <- aggregation_unit
      nm <- getNamesWR()

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
        class(data) <- c(class(data),"WR")

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


