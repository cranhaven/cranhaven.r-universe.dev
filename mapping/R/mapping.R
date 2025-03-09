mapping <- function(data = NULL, var = NULL,
                    colID = NULL,
                    type = c("static", "interactive"),
                    typeStatic = c("tmap", "choro.cart", "typo", "bar"),
                    add_text = NULL, subset = NULL, facets = NULL, aggregation_fun = sum,
                    aggregation_unit = NULL, options = mapping.options(), ...)
{


  use_cache = options$use_cache
  use_internet = options$use_internet
  type <- match.arg(type, choices = eval(formals(mapping)$type))
  typeStatic <- match.arg(typeStatic, choices = eval(formals(mapping)$typeStatic))




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



  if(!inherits(data, c("sf", "IT", "EU", "WR","US")))
  {
    stop("data must be an object of class `sf`, `IT`, `EU`, `WR`, or `US`",call. = FALSE)
  }



  if(!is.null(aggregation_unit))
  {

    if(is.null(aggregation_fun))
    {
      stop("aggregation_fun must be provided to aggregate data")
    }


    data <- aggregate(x = data[,var],
                      by = list(var = data[, aggregation_unit, drop = TRUE]), FUN = aggregation_fun)
    colnames(data)[1] <- aggregation_unit
    colID <- aggregation_unit


  }

  if(is.null(colID))
  {
    colID = colnames(data)[1]

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

