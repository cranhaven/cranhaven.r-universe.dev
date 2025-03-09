getNamesIT <- function(year = c("2021","2020","2019","2018", "2017"),
                       unit = c("ripartizione", "regione", "provincia", "comune"),
                       all_levels = TRUE)
{

  year <- match.arg(year, choices = eval(formals(getNamesIT)$year))
  unit <- match.arg(unit, choices = eval(formals(getNamesIT)$unit))

  dt <-  get("namesIT", pos = "package:mapping")
  dt <- dt[[year]]

  if(unit == "provincia")
  {
    if(isFALSE(all_levels))
    {
      out <- unique(dt[,unit, drop = TRUE])

    }else{
      out <- (dt[c("ripartizione","regione", "provincia", "code", "code_ripartizione", "code_regione", "code_provincia" )])
      out <- distinct(out)
    }


  } else if(unit == "ripartizione")
  {
    if(isFALSE(all_levels))
    {
      out <- unique(dt[,unit, drop = TRUE])
    }else{
      out <- (dt[c("ripartizione","code_ripartizione")])
      out <- distinct(out)
    }


  }else if(unit == "regione")
  {

    if(isFALSE(all_levels))
    {
      out <- unique(dt[,unit, drop = TRUE])
    }else{
      out <- (dt[c("ripartizione","regione", "code_ripartizione", "code_regione")])
      out <- distinct(out)
    }



  }else if(unit == "comune")
  {
    if(isFALSE(all_levels))
    {
      out <- dt[,unit, drop = TRUE]
    }else{
      out <- dt
    }
  }

  return(out)
}


getNamesEU <- function(year = c("2021","2016", "2013", "2010", "2006", "2003"),
                       unit = c("nuts0", "nuts1", "nuts2","nuts3"), id = FALSE,
                       all_levels = TRUE)
{
  year <- match.arg(year, choices = eval(formals(getNamesEU)$year))
  unit <- match.arg(unit, choices = eval(formals(getNamesEU)$unit))

  dt <-  get("namesEU", pos = "package:mapping")
  dt <- dt[[year]]
  if(isTRUE(id))
  {
    unit <- paste(unit, "_id",sep = "")
  }

  if(unit == "nuts3" | unit == "nuts3_id")
  {
    if(isFALSE(all_levels))
    {
      out <- dt[,unit, drop = TRUE]

    }else{
      out <- dt
    }

  } else if(unit == "nuts2" | unit == "nuts2_id")
  {
    if(isFALSE(all_levels))
    {
      out <- unique(dt[,unit, drop = TRUE])
    }else{

      rm <- !colnames(dt)%in%c("id","nuts3", "nuts3_id")

      out <- (dt[,rm])
      out <- distinct(out)
    }


  }else if(unit == "nuts1" | unit == "nuts1_id")
  {

    if(isFALSE(all_levels))
    {
      out <- unique(dt[,unit, drop = TRUE])
    }else{
      rm <- !colnames(dt)%in%c("id","nuts3", "nuts3_id","nuts2","nuts2_id")
      out <- (dt[,rm])
      out <- distinct(out)
    }



  }else if(unit == "nuts0" | unit == "nuts0_id")
  {
    if(isFALSE(all_levels))
    {
      out <- unique(dt[,unit, drop = TRUE])
    }else{
      rm <- !colnames(dt)%in%c("id","nuts3", "nuts3_id","nuts2","nuts2_id", "nuts1","nuts1_id")
      out <- (dt[,rm])
      out <- distinct(out)
    }
  }

  return(out)
}


getNamesWR <- function(unit = c("all","country", "name_formal", "name_wb", "iso2",
                                "iso3", "iso3_eh", "iso3_numeric", "iso3_un",
                                "iso2_wb", "iso3_wb"))
{

  unit <- match.arg(unit, choices = eval(formals(getNamesWR)$unit))

  dt <-  get("namesWR", pos = "package:mapping")

  if(length(unit) != 1)
  {
    unit = "all"
  }

  if(unit == "all")
  {
    out <- dt
  }else{
    out <- dt[, colnames(dt)%in%unit]
  }

  return(out)
}




getNamesUS <- function(year = "2018",
                       unit = c("region", "division","state", "county",
                                "district", "district_county",
                                "urban_area"),
                       id = FALSE,
                       all_levels = TRUE)
{
  year <- match.arg(year, choices = eval(formals(getNamesUS)$year))
  unit <- match.arg(unit, choices = eval(formals(getNamesUS)$unit))

  dt <-  get("namesUS", pos = "package:mapping")
  dt <- dt[[year]][[unit]]

  if(isTRUE(id) & unit == "state")
  {
    unit <- paste(unit, "_id",sep = "")
  }

  out <- dt

  if(unit == "state" | unit == "county" | unit == "division")
  {
    if(isFALSE(all_levels))
    {
      out <- unique(as.vector(dt[,unit, drop = TRUE]))
    }
  }else if(unit == "region")
  {
    out <- data.frame(region = out)
  }




  return(out)
}



getNamesUK <- function(year = c("2020", "2019"),
                       unit = c("country", "county"),
                       all_levels = TRUE)
{
  
  year <- match.arg(year, choices = eval(formals(getNamesUK)$year))
  unit <- match.arg(unit, choices = eval(formals(getNamesUK)$unit))
  
  dt <-  get("namesUK", pos = "package:mapping")
  dt <- dt[[year]]
  
  if(unit == "country")
  {
    if(isFALSE(all_levels))
    {
      out <- unique(dt[,unit, drop = TRUE])
      
    }else{
      out <- (dt[c("country","code_country" )])
      out <- distinct(out)
    }
  
}else if(unit == "county")
{
  if(isFALSE(all_levels))
  {
    out <- dt[,unit, drop = TRUE]
  }else{
    out <- dt
  }
}

return(out)
}


getNamesDE <- function(unit = c("state","district", "municipal", "municipality"),
                       all_levels = TRUE)
{
  
  unit <- match.arg(unit, choices = eval(formals(getNamesDE)$unit))
  
  dt <-  get("namesDE", pos = "package:mapping")

  if(unit == "state")
  {
    if(isFALSE(all_levels))
    {
      out <- (dt[[unit]])
      
    }else{
      out <- (dt[[unit]][c("state","code_state", "code" )])
    }
    
  }else if(unit == "district")
  {
    if(isFALSE(all_levels))
    {
      out <- dt[[unit]]
    }else{
      out <- (dt[[unit]][c("state","code_district", "code" )])
    }
  }else if(unit == "municipal")
  {
    if(isFALSE(all_levels))
    {
      out <- dt[[unit]]
    }else{
      out <- (dt[[unit]][c("state","code_municipal", "code" )])
    }
  }else if(unit == "municipality")
  {
    if(isFALSE(all_levels))
    {
      out <- dt[[unit]]
    }else{
      out <- (dt[[unit]][c("state","code_municipality", "code" )])
    }
  }
  
  return(out)
}


getNamesFR <- function(year = c("2021","2020", "2019"),
                       unit = c("region"),
                       all_levels = TRUE)
{
  
  year <- match.arg(year, choices = eval(formals(getNamesFR)$year))
  unit <- match.arg(unit, choices = eval(formals(getNamesFR)$unit))
  
  dt <-  get("namesFR", pos = "package:mapping")
  dt <- dt[[year]]
  
  if(unit == "region")
  {
    if(isFALSE(all_levels))
    {
      out <- unique(dt[,unit, drop = TRUE])
      
    }else{
      out <- dt
    }
    
  }
  
  return(out)
}
