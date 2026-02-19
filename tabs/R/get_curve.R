#' @importFrom terra crs rast
#' 
NULL 
#' 
#' @title Get and prepare curve 
#' 
#' @author Johannes De Groeve
#' @description load a curve of interest
#'
#' @param curve SpatRaster. Curve value, vector, grid or list of grids indicating the relative altitude of a biogeographic system per time period compared to the present. A typical example is a sea level curve indicating the relative sea level position above or below sea level compared to the present. 
#' @param verbose Boolean. FALSE: No messages are printed. TRUE: Standard verbose mode. 2: Very verbose mode, displaying detailed information. 
#'
#' @return A SpatRaster or vector with curve values in a suitable format for the reconstruct function. 
#' 
#' @seealso \href{https://uva_ibed_piac.gitlab.io/tabs/articles/Ba-tabs-curve.html}{curve}
#' 
#' 
#' 
#' @export
#'
#' @examples
#' curve <- get_curve('lambeck')
#' curve <- get_curve('cutler')
#' curve <- get_curve('IPCC')
#' curve <- get_curve('funza')
#'
#' curve <- get_curve()
#' curve <- get_curve(10)
#' curve <- get_curve(c(0,100,200,300,400,500))
#' 
#' # period definition curve 
#' cur <- c(0,100,200,300,400,500) # altitudes
#' names(cur) <- c(0,1,2,3,4,5) # periods 
#' curve <- get_curve(cur)
#' 
#' # add source attribute
#' cur <- c(0,100,200,300,400,500) # altitudes
#' names(cur) <- c(0,1,2,3,4,5) # periods 
#' attr(cur, 'source') <- 'new curve' # curve source 
#' curve <- get_curve(cur) 
#' 
#' # custom-curve from data frame
#' cur <- data.frame(period=0:10, 
#'                   altitude=seq(0,-20,-2), 
#'                   source='custom')
#' curve <- get_curve(cur)
#' 
get_curve <- function(curve=NULL, verbose=FALSE){
  
  if(is.null(curve)){ # present day
    curve_data <- data.frame(year_before_after_present=0,sea_level_m=0,source='present')
  } else { 
    
    # check if the curve is the sample dataset 
    if(class(curve)[1] == 'SpatRaster'){
      sample <- sporades()$curve
      # DIR <- system.file("extdata", "sporades.gpkg", package = "tabs")
      # r_names <- names(terra::rast(DIR))
      # sample <- terra::rast(DIR,r_names[3:length(r_names)])
      if(all.equal(curve,sample)[1] == TRUE){
        curve_data <- curve
        attr(curve_data, 'source') <- 'st_curve (sample)'
      } else {
        curve_data <- curve
      }
    } else {
    
    if(tolower(curve[1]) %in% tolower(c('Funza','Lambeck','Cutler','Bintanja','IPCC'))){ # if curve stored  in r package
      if(grepl('lambeck', curve,ignore.case = T)){curve_data <- tabs::lambeck} # curve == 'Lambeck'
      if(grepl('cutler', curve,ignore.case = T)){curve_data <- tabs::cutler} # curve == 'Cutler'
      if(grepl('bintanja', curve,ignore.case = T)){curve_data <- tabs::bintanja} # curve == 'Bintanja'
      if(grepl('ipcc', curve,ignore.case = T)){curve_data <- tabs::IPCC_global_mean} # curve == 'IPCC'
      if(grepl('funza', curve,ignore.case = T)){curve_data <- tabs::funza} # curve == 'IPCC'
      
    } else { # if curve is not stored in r package, read from locally 
      if(grepl('.csv',curve[1])){
        curve_data <- read.csv(curve, stringsAsFactors = FALSE)
      } else {
        if(is.numeric(curve)){
            if(as.numeric(verbose) > 1) message('set default periods')
            curve_data <- curve
            # if there are no names provided (time periods) they are automatically generated
            if(is.null(names(curve))){
            names(curve_data) <- 0:(length(curve)-1)
            } 
            if(is.null(attr(curve_data,'source'))){attr(curve_data,'source') <- 'custom'}
            sea_level_m <- curve_data
        } else { # spatio-temporal curve 
          if(is.data.frame(curve)){
            curve_data <- curve
            # message('test')
          } else {
            
            if(curve == 'st_curve'){ # if the curve is the st_curve load the path 
              curve <- paste0(options()$tabs.datasetPath,'/curve/RSL_mosaic')
            }
            
            if(is.character(curve)){ # if the curve is character (a path already) the previous step is not needed 
            rastlist <- list.files(path = curve, full.names =TRUE, pattern='.ASC$|.tif$')
            if(length(rastlist)==0) {stop('curve name or path does not exist')}
            
            curve_data <- rast(rastlist)  
            
            if(curve == paste0(options()$tabs.datasetPath,'/curve/RSL_mosaic')){
              attr(curve_data,'source') <- 'st_curve'
              }
            
            if(crs(curve_data) == ''){
              terra::crs(curve_data) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
            }
            names(curve_data) <- gsub('-0','0',-as.numeric(gsub('\\.','',gsub('[A-z]','',names(curve_data)))) * 100)
            } 
            
        }
      }
      
    }
    
    }
    }
  }
  
  if(is.data.frame(curve_data)){
    
    if(is.null(curve_data$sea_level_m)){
      # check if there is a column which might be the sea level
      words <- 'sealevel|sea_level|rsl|rslm|ufl|upper_forest_line|upperforestline|upperforestlimit|upper_forest_limit|altitude|elevation|elev|alt|height|level|masl|mbsl|curve|value|valore|livellodelmare|zeespiegelstand'
      column <- colnames(curve_data)[grepl(words,tolower(colnames(curve_data)))]
      column_index <- grep(words,tolower(colnames(curve_data)))
      if(length(column) == 1){
        #unlist(regmatches(time_column, gregexpr('kyear|myr|myear|year|kyr|yr|time|period', time_column, perl=TRUE)))
        sea_level_m <- curve_data[,grepl(words,tolower(colnames(curve_data)))]
        message('Column ',column_index, ' named ', column, ' is assumed to be the curve value')
      } 
      if(length(column) == 0){
        sea_level_m <- curve_data[,2]
        warning('no column with the curve value found, it is assumed the second column is the curve altitude expressed in meters')
      }
      if(length(column) > 2){
        stop('multiple columns are matched as curve value, cannot determine the correct curve column')
      }
    } else {
      sea_level_m <- curve_data$sea_level_m
    }
    
    if(is.null(curve_data$year_before_after_present)){
      
      # check if there is a column which might be the period
      time_column <- colnames(curve_data)[grepl('kyr|kyear|myr|myear|year|yr|time|period',tolower(colnames(curve_data)))]
      time_column_index <- grep('kyr|kyear|myr|myear|year|yr|time|period',tolower(colnames(curve_data)))
      if(length(time_column) == 1){
        #unlist(regmatches(time_column, gregexpr('kyear|myr|myear|year|kyr|yr|time|period', time_column, perl=TRUE)))
        names(sea_level_m) <- curve_data[,grepl('kyear|myr|myear|year|kyr|yr|time|period',tolower(colnames(curve_data)))]
        message('Column ',time_column_index, ' named ', time_column, ' is assumed to be the time period')
      } 
      if(length(time_column) == 0){
        names(sea_level_m) <- curve_data[,1]
        warning('no column with temporal year_before_after_present, it is assumed the first column is the temporal information expressed in years')
      }

      if(length(time_column) > 2){
        stop('multiple columns are matched as the time period, cannot determine the time period')
      }
    } else {
      names(sea_level_m) <- curve_data$year_before_after_present  
    }

    
    if(is.null(curve_data$source)){
      if(grepl('.csv',curve[1])){
        attr(sea_level_m,'source') <- gsub('.csv','',basename(curve))
      } else {
        warning("no column named source, the source name of the curve cannot be derived, please add 'source' column, or the default 'custom' is used")
        attr(sea_level_m,'source') <- 'custom'
      }
    } else {
      attr(sea_level_m,'source') <- curve_data$source[1]
    }    
    
    if(any(as.numeric(names(sea_level_m)) %% 1 != 0) & max(as.numeric(names(sea_level_m)))!=0){
      warning('time should be represented by integer values')
    }
    
    curve_data <- sea_level_m
  }
  
  # names(curve_data) <- ifelse(grepl('-',names(curve_data)),
  #                             paste0('BP',sprintf("%07d", abs(as.numeric(names(curve_data))))),
  #                             paste0('AP',sprintf("%07d", abs(as.numeric(names(curve_data))))))

  
  return(curve_data)
}



#' #' load a sea curve from the database
#' #'
#' #' @title interpolate_curve
#' #' @author Johannes De Groeve
#' #' @description interpolate curve 
#' #'
#' #' @param curve the name of the curve ('Lambeck','Cutler','Bintanja') or a local file with the required data
#' #' @param time_steps
#' #' 
#' #' @return a vector 
#' #'
#' #' @export
#' #' 
#' #' @keyword internal
#' #'
#' #' @examples
#' #' \dontrun{
#' #' interpolate_curve(curve,time_steps)
#' #' }
#' #' 
#' interpolate_curve <- function(curve, time_steps) {
#'   
#'   #curve <- get_curve(curve)
#'   
#'   if(!is.null(time_steps[1])){
#'   years_before_after_present <- as.numeric(names(curve))
#'   curve_data <-  as.vector(names(curve))   
#'     if(length(time_steps) == 1){
#'     message('time_steps is an interval')
#'     # Initialize vectors to store interpolated results
#'     min <- max(years_before_after_present)
#'     max <- min(years_before_after_present)
#'     time_steps <- seq(min,max,time_steps)
#'   }
#'   
#'   interpolated <- numeric(length(time_steps))
#'   
#'   # Perform interpolation for each time step
#'   for (i in seq_along(time_steps)) {
#'     time_step <- time_steps[i]
#'     
#'     # Check if time_step is within the range of the dataset
#'     if (time_step < min(years_before_after_present) || time_step > max(years_before_after_present)) {
#'       stop(paste("time_step", time_step, "is out of range of the dataset"))
#'     }
#'     
#'     # Linear interpolation for Elevation
#'     interpolated[i] <- approx(years_before_after_present, curve_data, xout = time_step)$y
#'     
#'   }
#'   
#'   names(interpolated) <- time_steps
#'   
#'   } else {
#'   interpolated <- curve 
#'   }
#'   # Return a data frame with the interpolated results
#'   return(interpolated)
#' }




#' #' Import a sea curve in the database
#' #' @title import_curve
#' #' @author Johannes De Groeve
#' #' @description Import a sea curve in the database
#' #' @param df A data frame with the curve data to be imported (should include 4 columns: year_before_after_present, sea_level_m, curve_name, publication_id)
#' #' @param global Boolean (TRUE/FALSE) to specify whether the curve is global.
#' #' @param boundary The extent to which the curve is valid provided by a vector of coordinates c(xmin,xmax,ymin,ymax). The default is set to c(-180,180,-90,90).
#' #' @param public Boolean (TRUE/FALSE) whether the curve can be shared publically through the database?
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' 
#' #' \dontrun{
#' #' lambeck <- readRDS('/Users/jedgroev/Downloads/1_IBED/IBED_TCE/PIAC_ISLAND_GEOGRAPHY/PIAC workflow/input/GLOBAL/time_level_Lambeck.rds')
#' #' # check the curve
#' #' head(lambeck) # check if all columns are there including a column for the official curve name, check if the units are correct.
#' #' # add column for the curve name
#' #' lambeck$curve_name <- 'Lambeck'
#' #' # rename the columns to those names used in the database
#' #' colnames(lambeck) <- c('year_before_after_present','sea_level_m','curve_name')
#' #' # make sure that units are correct (years before/after present; meters below/above sea level)
#' #' lambeck$year_before_after_present <- lambeck$year_before_after_present * 1000
#' #' lambeck$publication_id <- '10.1073/pnas.1411762111'
#' #' # inspect another time
#' #' head(lambeck)
#' #'
#' #' # now you can import the curve
#' #' import_curve(df=lambeck)
#' #' }
#' #' 
#' import_curve <- function(df, global = TRUE, boundary = c(-180,180,-90,90), public=TRUE){
#'   # official curve name
#'   # extent = is it a local or a global curve?
#'   # boundary = the xmin,xmax,ymin,ymax for which the curve is valid.
#'   # public = is the sea curve public or private resource? TRUE/FALSE
#'   credentials <- get_credentials()
#'   # import curves in the database in R
#'   con <- DBI::dbConnect("PostgreSQL", dbname = credentials$database, host = 'localhost', user = credentials$user, password = credentials$password)
#'   curves <- rpostgis::pgGetGeom(con,query='select curve_name,curve_id, st_envelope(geom) geom, publication_id from main.curves')
#'   
#'   if(is.character(all.equal(colnames(df), c('year_before_after_present','sea_level_m','curve_name','publication_id'))) == FALSE){ # open - check if the three columns for the data to be imported match
#'     df$curve_name <- trimws(df$curve_name, which = "both")
#'     df$publication_id <- trimws(df$publication_id, which = "both")
#'     
#'     #  if(length(unique(df$curve_name)) == 1){
#'     if(grepl("^[[:upper:]]+$", substr(df$curve_name[1],1,1)) |
#'        grepl("^[[:lower:]]+$", substr(df$curve_name[1],2,nchar(df$curve_name)))){ # open - is the official name starting with a uppercase and the rest is lowercase?
#'       
#'       
#'       if(nrow(curves[which(curves$curve_name == df$curve_name[1]),]) == 0){ # open - is the official curve name not yet in the database?
#'         
#'         if(global){ # open - global curve?
#'           if( is.character(all.equal(boundary,c(-180,180,-90,90))) == FALSE){ # open - global curve boundaries?
#'             e <- as(raster::extent(boundary),'SpatialPolygons')
#'             sp::proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#'             e$global <- TRUE
#'             # add curve name
#'             e$curve_name <- df$curve_name[1]
#'             e$publication_id <- df$publication_id[1]
#'             
#'             if(public){
#'               e$public <- TRUE
#'             } else {
#'               e$public <- FALSE
#'             }
#'             # import the curve
#'             rpostgis::pgInsert(con, c('main','curves'),e, partial.match=TRUE)
#'           } else {
#'             message('curve is defined as global (global=TRUE), but the boundary is different from the default (boundary = c(-180,180,-90,90))')
#'           } # close - global curve boundaries?
#'         } else {
#'           if( is.character(all.equal(boundary,c(-180,180,-90,90)))== FALSE){ # open - local curve boundaries?
#'             message('default global boundary (boundary = c(-180,180,-90,90)) is used while curve is not global (global=FALSE)')
#'           } else {
#'             e <- as(raster::extent(boundary),'SpatialPolygons')
#'             sp::proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#'             e$global <- FALSE
#'             # add curve name
#'             e$curve_name <- df$curve_name[1]
#'             e$publication_id <- df$publication_id[1]
#'             
#'             if(public){
#'               e$public <- TRUE
#'             } else {
#'               e$public <- FALSE
#'             }
#'             # import the curve
#'             rpostgis::pgInsert(con, c('main','curves'),e, partial.match=TRUE)
#'           } # close - local curve boundaries
#'         } # close - global curve?
#'         
#'       } else {
#'         message(paste0('curve ', df$curve_name[1],' is already in the database'))
#'       }  # close - is the official curve name in the database?
#'       
#'       curve_data <- DBI::dbGetQuery(con, paste0('SELECT DISTINCT curve_name FROM main.curve_data JOIN main.curves USING (curve_id) WHERE curve_name = \'',df$curve_name[1],'\''))
#'       curve_name_indb <- as.vector(unlist(DBI::dbGetQuery(con, paste0('SELECT curve_name FROM main.curves WHERE curve_name = \'',df$curve_name[1],'\''))))
#'       
#'       if(is.character(all.equal(curve_name_indb,df$curve_name[1])) == FALSE){ # open - curve name in database - only if that is the case this part can be ran
#'         if(is.character(all.equal(curve_data$curve_name,df$curve_name[1])) == TRUE) {# open - check if curve_data is in the database?
#'           curve_id <- DBI::dbGetQuery(con, paste0('SELECT curve_id FROM main.curves WHERE curve_name = \'',df$curve_name[1],'\''))
#'           
#'           to_import <- df
#'           to_import$curve_id <- as.vector(unlist(curve_id))
#'           to_import <- to_import[,c('year_before_after_present','sea_level_m','curve_id')]
#'           DBI::dbWriteTable(con, c('main','curve_data'),to_import,append=TRUE, row.names=FALSE)
#'           message(paste0('curve data of ', df$curve_name[1], ' imported in the database.'))
#'           
#'           if(credentials$user %in% c('postgres','db_manager')){
#'             if(public){
#'               write.csv(df,paste0('~/Downloads/2_GITHUB/PIAC/Data/',df[1,'curve_name'],'.csv'))
#'             }
#'           }
#'         } else {
#'           message(paste0('curve data of ', df$curve_name[1], ' already in the database.'))
#'         } # close - check if curve_data is in the database?
#'       } # close - curve name in database - only if that is the case this part can be ran
#'     } else {
#'       message('Double-check the spelling of the curve is correct: First letter should be upper case, the rest lower case (e.g., Lambeck)')
#'     } # close - is the official name starting with a uppercase letter?
#'     
#'   } else {
#'     message("column names do not correspond! Double-check column names and units of the data. Update the column names as follows: colnames(seacurve) <- c('year_before_after_present','sea_level_m','curve_name')")
#'     
#'   } # close - check if the three columns for the data to be imported match
#'   dbDisconnect(con)
#' }

