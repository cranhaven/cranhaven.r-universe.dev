
## subsecting a data.frame using formula

subset_formula <- function(data, formula = ~I())
{
  

  if(!inherits(data, "data.frame"))
  {
    data <- data.frame(data)
  }
  ll <- unlist(model.frame(data = data, formula = formula))
  return(data[ll,])
}

add.formula <- function(data, formula = ~I(), names = NULL)
{


  if(!inherits(data, "data.frame"))
  {
    data <- data.frame(data)
  }

  ll <- model.frame(data = data, formula = formula,drop.unused.levels = FALSE, na.action=NULL)

  if(!is.null(names))
  {
    colnames(ll) <- names
  }
  ll <- data.frame(data, ll)

  class(ll) <- class(data)
  return(ll)
}


mapPalette <- function(type,
                       nclass = NULL)
{

  if(missing(type) | is.null(type))
  {
    return(NULL)

  }else if(is.na(type)){

    return(NULL)

  }else{

    if(is.null(nclass))
    {
      nclass = 20
    }

    if(type == "viridis" | type == "magma" | type == "plasma" | type == "inferno" | type == "cividis" )
    {
      viridis(n = nclass, option = type )
    }else if(type %in% carto.pal.info())

    {
      carto.pal(pal1 = type, n1 = nclass)

    }else{
      get_brewer_pal(palette = type, plot = FALSE, n = nclass )
    }
  }

}


saveObj <- function(obj, name,
                    as = c("RData", "csv", "json", "geojson", "shp"),
                    ...)
{

  split_name <- strsplit(basename(name), split="\\.", )


  if(length(split_name[[1]]) == 2)
  {
    as <- split_name[[1]][[2]]

  }else{

    name <- paste(name, as, sep = ".")

  }



  if(as == "RData")
  {

    save(obj, file = name)

  }else if(as == "csv")
  {

    write.csv(x = obj, file = name, ...)

  }else if(as == "json")
  {

    jsn <- toJSON(x = obj, pretty = TRUE, ...)
    write(jsn, name)

  }else if(as == "geojson")
  {

    geojson_write(input = obj,
                  geometry = "polygon", file = name, ...)

  }else if(as == "shp")
  {

    st_write(obj, name, driver="ESRI Shapefile", ...)


  }else
  {
    stop("A valid format must be provided")
  }


}



