#' Extract region from a SpatialGridDataFrame
#'
#' This method is intended for use as the main helper function for 
#' extractStData.
#'
#' @return a modified SpatialGridDataFrame, sgdf, with the climatology for each
#'  location accessible via attr(sgdf@data@values, 'scaled:center') if anomalies
#'  were computed
#'  
#' @importFrom raster brick mask aggregate crop scale extract
#' 
#' @param sgdf SpatialGridDataFrame containing data to extract
#' @param type whether to return the raw data, anomalies (data minus temporal 
#' average at each location), standardized anomalies (anomalies divided by
#' temporal standard deviation at each location), or spatially standardized
#' data (data minus overall spatial average divided by spatial std. dev.; each
#' year gets its own spatial standardization )
#' @param extent raster::extent object featuring region to extract, or a 
#' SpatialPolygonsXXX object used for extracting areal data
#' @param aggfact if provided, will spatially average the data
#' @param mask if an sgdf is provided, the data will be masked before
#'  extraction, aggregation, and anomaly computation
#' @param aspect TRUE to return the aspect of the surface at each location 
#'   instead of the value of the surface itself
#' @param slope TRUE to return the slope of the surface at each location instead
#'  of the value of the surface itself
#' @param aspect.categories if aspect==TRUE, this specifies the number of 
#'   discrete categories to divide aspect numbers (0-360) into.  NULL if the
#'   original scale (0-360) should be kept. By design, the aspect categories
#'   will be centered on north in the first category.
#'   

extractRegion = function(sgdf, extent, 
                         type='response', 
                         aggfact=NULL, mask=NULL, 
                         aspect=F, aspect.categories=NULL,
                         slope=F) {
  
  # convert sgdf to a raster object
  sgdf = brick(sgdf)
  
  # mask data
  if(!is.null(mask))
    sgdf = mask(sgdf, brick(mask))

  # extract data
  if(inherits(extent,'Extent')) {
    
    # extract and aggregate the data
    if(!is.null(aggfact) && aggfact > 1)
      sgdf = aggregate(sgdf, fact=aggfact)
    
    # if(aspect) {
    #   for(i in 1:sgdf@data@nlayers) {
    #     # compute aspects
    #     sgdf[[i]] = aspect(sgdf[[i]], latlon = T)
    #     # classify aspects
    #     if(!is.null(aspect.categories)) {
    #       sgdf[[i]]@data@values = as.numeric(
    #         cut((sgdf[[i]]@data@values + 180/aspect.categories) %% 360, 
    #             seq(0, 360, length.out = aspect.categories+1))
    #       )
    #     }
    #   }
    # } else if(slope) {
    #   for(i in 1:sgdf@data@nlayers) {
    #     # compute slopes
    #     sgdf[[i]] = slope(sgdf[[i]], latlon = T)
    #   }
    # }
    
    # crop data
    sgdf = raster::crop(sgdf, extent)
    
    # extract data to compute anomalies
    dat = sgdf@data@values
    
  } else if(startsWith(class(extent), 'SpatialPolygons')) {
    
    # extract average value from each region
    sgdf = raster::extract(x = sgdf, y = extent, fun = mean, sp = TRUE,
                           na.rm = TRUE)
    
    # only retain the extracted data
    sgdf@data = sgdf@data[,-(1:ncol(extent@data))]
    
    # extract data to compute anomalies
    dat = sgdf@data
    
  }
  
  #
  # compute anomalies
  #
  
  # compute anomalies
  match.opts = c('response', 'std.anomaly', 'anomaly', 'spatial.anomaly', 
                 'spatial.std.anomaly')
  type = match.opts[pmatch(type, match.opts)]
  if( type=='std.anomaly' ) {
    dat = t(scale(t(dat), center=T, scale=T))
  }
  else if( type=='anomaly') {
    dat = t(scale(t(dat), center=T, scale=F))
  }
  else if( type=='spatial.anomaly') {
    dat = scale(dat, center=T, scale=F)
  }
  else if( type=='spatial.std.anomaly') {
    dat = scale(dat, center=T, scale=T)
  }
  
  # reassemble data
  if(inherits(extent,'Extent')) { 
    # restore matrix structure to data
    if(!inherits(dat,'matrix')) {
      dat = matrix(dat, ncol=1)
    }
    sgdf@data@values = dat
  } else if(startsWith(class(extent), 'SpatialPolygons')) {
    sgdf@data = data.frame(dat)
  }
  
  sgdf
}