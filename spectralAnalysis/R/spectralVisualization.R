# Plotly renewed 3dPlot 
# 
# Author: Adriaan Blommaert
###############################################################################



#' help function to make plotly axis title
#' 
#' @keywords internal 
plotlyAxis                       <-  function( title ) {
  listSettings                   <-  list( title = title )
  
  ## conditional settings
  isWavelength                <-  grep( "wave" , x = title , ignore.case = TRUE  )
  if( length( isWavelength) > 1 ) {
    listSettings$autorange       <-  "reversed"       
  }
  return( listSettings )
}


### Plotting methods 

#' Plotting methods for 'spectralAnalysis'
#' 
#' @param x the object to be plotted
#' @param y not used, for consitency with plot method
#' @param type character choice of plot type between:
#'   * \code{"3D"} surface plot (default)
#'   * \code{"spectralAxis"} spectral axis in legend, time on the axis
#' 	 * \code{"wavelength"} same behaviour as with \code{type} is "spectralAxis", kept for historical reasons
#'   * \code{"time"} time in legend, spectral values on the axis
#'   *  \code{"contour"} contour plot of spectra time versus spectral values
#' @param timeUnit time unit specification in the form of a character string
#' @param ylab Specification y-axis label. By default this is "Wavelength", but can be different for e.g. mass spectra
#' @param colors colorblind friendly palettes are used from the de \code{\link[viridis]{viridis_pal}} choice between \code{"A"} (magna),
#'  \code{"B"} (inferno),  \code{"C"} (plasma) and \code{"D"} the default viridis color palette
#' @param nColors number of colours to use, only relevant if \code{type} is "contour", defaults to 200
#' @param limits 2-element numeric vector specifying the range of values onto which the color scale should be mapped (currently only relevant if \code{type} is "contour")
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom magrittr  %>%
#' @importFrom viridis viridis_pal
#' @examples 
#' 
#' ### visualization 
#'   \donttest{
#'   data = getSpectraInTimeExample()
#'   plot( x =  data , type = "3D" , timeUnit = "hours" , timePointsAlt = FALSE )
#'   plot( x =  data[  , r(500, 350) ] , type = "3D" ,
#'       timeUnit = "hours" , timePointsAlt = TRUE , colors = "B"  )
#'   plot( x =  data[ e( 1 , 2 , 3) , , timeUnit = "hours" ] ,
#'       type = "time" , timeUnit = "hours" , timePointsAlt = FALSE ) 
#'   plot( x =  data[ , e( seq( 200 , 400 , 50 ) ) ] , 
#'      type = "spectralAxis" , timeUnit = "minutes" , timePointsAlt = TRUE , colors = "A" )
#' 
#'   plot( x = data , type = "contour" , nColors = 200 , 
#'     colors = "C" , timeUnit = "seconds", timePointsAlt = TRUE)
#' }
#' 
#' @importFrom graphics image
#' @rdname SpectraInTime-class                  
#' @export
setMethod( "plot" , signature =  c( x = "SpectraInTime" , y = "missing" ) , function( x , y , type = "3D" , timeUnit = "hours"  , 
        timePointsAlt = FALSE , ylab = "Wavelength", colors = "D" , nColors = 200  , limits = NULL) {
      ##  check inputs
#      cat("plot method is used" , "\n" )
      allowedPlotTypes               <-  c( "3D" , "spectralAxis" , "wavelength", "time" , "contour" )
      plotTypesFormat                <-  paste0( "'" , paste( allowedPlotTypes , collapse = "', '") , "'" )
      checkPlotTypeAllowed           <-  type %in% allowedPlotTypes
      if( ! checkPlotTypeAllowed ) {
        stop( "Choose plotType out of: " , plotTypesFormat )
      }
      
      ##  call plot subtype
      arguments                         <-  list( x = x , y = NULL , timeUnit = timeUnit , timePointsAlt = timePointsAlt , ylab = ylab, colorPalette = colors  )
      if( type  == "3D" ){  
#        cat("3D plot chosen")
        plotlyPlot                      <-  do.call( plot.spectrum3D , args = arguments  ) 
      }
      if( type %in% c("spectralAxis", "wavelength") ){   
        plotlyPlot                      <-  do.call( plot.spectralAxis , args = arguments  ) 
      }
      if( type == "time" ) {
        plotlyPlot                      <-  do.call( plot.times , args = arguments  ) 
      }
	  if( type == "contour" ){
		  plotContourSpectra( spectra = x, timeUnit = timeUnit, timePointsAlt = timePointsAlt , ylab = ylab,  colorPalette = colors , nColors = nColors , limits = limits)
		  return(invisible(NULL))			  
	  }
      return( plotlyPlot )
    }
)


plot.spectrum3D           <-  function( x , y , timeUnit , timePointsAlt , ylab, colorPalette ) {
  spectra                 <-  getSpectra( x )
  timePoints              <-  getTimePoints( x , timeUnit = timeUnit  , timePointsAlt = timePointsAlt  )
  spectralAxis              <-  getSpectralAxis( x )
  nColors                 <-  100 # large number plot works fine              
  
  p                       <-  plot_ly( z = spectra , y = timePoints , x = spectralAxis , colors = viridis_pal(option = colorPalette )( nColors ) ) %>% add_surface()
  plot                    <-  p %>% layout( scene = list( 
          xaxis = plotlyAxis( paste0("x = " , ylab ) ) ,
          zaxis = plotlyAxis( "z = Response" ) ,
          yaxis = plotlyAxis( paste0("y = Time in " , timeUnit )  )
      )
  ) 
  return( plot )
}



plot.spectralAxis          <-  function( x , y ,  timeUnit  , timePointsAlt , ylab, colorPalette  ) {
  spectraFlat             <-  as.data.frame( x ,  timeUnit = timeUnit , timePointsAlt = timePointsAlt ) 
  spectraFlat$Wavelength  <-  paste0( ylab, " = ",  spectraFlat$spectralAxis )
  nColors                 <-  length( getSpectralAxis( x ) )
  p                      <-  plot_ly(spectraFlat ,  x = ~timePoints , y = ~response  , type = "scatter" ,
      color = ~Wavelength  ,  symbol =  ~Wavelength  , 
      mode = 'lines+markers' ,
      colors = viridis_pal(option = colorPalette )( nColors ) )
  plot                   <- p    %>% layout( 
      xaxis = plotlyAxis( paste0("Time in " , timeUnit ) ) ,
      yaxis = plotlyAxis( "Response" )
  ) 
  return( plot )
  
}



plot.times                <-  function( x , y ,  timeUnit  , timePointsAlt , ylab,  colorPalette  ) {
	spectraFlat             <-  try( as.data.frame( x ,  timeUnit = timeUnit , timePointsAlt = timePointsAlt ) , silent = TRUE)
	
	spectraFlat$Time        <-  paste0( "Time = ",  spectraFlat$timePoints , timeUnit  )
	nColors                 <-  length( getTimePoints( x ) )
	if(ylab == "m/z"){ # Different plotting in case of mass spec data
		spectraFlat$id <- seq_len(nrow(spectraFlat))
		spectraFlatList <- replicate(2, spectraFlat, simplify = F)
		spectraFlatList[[2]]$response <- 0
		spectraFlat2 <- group2NA(dplyr::bind_rows(spectraFlatList), "id", "Time")
		plotMode <- "markers"
		p                       <-  plot_ly(colors = viridis_pal(option = colorPalette )( nColors )) %>% layout( 
				xaxis =  plotlyAxis( ylab ) ,
				yaxis =  plotlyAxis( "Response" )) %>% 
		add_markers(data = spectraFlat, x = ~spectralAxis, y = ~response, color = ~Time)
		plot 					<- add_paths(p, data = spectraFlat2, x = ~spectralAxis, y = ~response, showlegend = FALSE,  color = ~Time)
	} else{ 
		plotMode <- "lines+markers"
		p                       <-  plot_ly(spectraFlat ,  x = ~spectralAxis , y = ~response  , type = "scatter" ,
				color = ~Time , symbol = ~Time , mode = plotMode , colors = viridis_pal(option = colorPalette )( nColors )
		) 
		plot                    <- p    %>% layout( 
				xaxis =  plotlyAxis( ylab ) ,
				yaxis =  plotlyAxis( "Response" )
		) 
	}
	return( plot )

}




### plotting multiple spectra in a list 



plotListOfSpectra        <-  function( dataList , y = "not important" ,   times = 0  , timeUnit = "seconds",  timePointsAlt = FALSE, ylab = "Wavelength", colors = "D" ){
  ## checks 
  # matching spectra
  nExperiments          <-  length( dataList )
  nTimes                <-  length( times )
  spectralAxis        <-  lapply( dataList , getSpectralAxis ) 
  spectralVals           <-  spectralAxis[[ 1 ]]
  identicalWavelengths  <-  sapply( spectralAxis , function( ax ) { all(  ( ax == spectralVals ))}) 
  if( ! all( identicalWavelengths ) ){stop( "unequal spectral axis over experiments" )}
  
  nWavelengths          <-  length( spectralVals )
  ## subset data
  experimentNames       <-  lapply( dataList , getExperimentName )
  dataSubset            <-  lapply( dataList , FUN= function( spec ){
      specSelect        <-  spec[ e( times ) , , timeUnit = timeUnit , timePointsAlt = timePointsAlt ] 
      specData          <-  getSpectra( specSelect )
      specData
    } 
  ) 
  
  dataToPlotList        <-  mapply( FUN = function( experiment , dataSet ){
      response          <-  as.vector( t( dataSet ) )
      Time              <-  paste0( "time = " , rep( times , rep( nWavelengths , nTimes ) ) ," " ,timeUnit )
      plotDataExp       <-  data.frame( spectralAxis = unname( spectralVals ) , response = response , experiment = experiment , Time = Time )      
    } ,
    experiment  = experimentNames , dataSet = dataSubset , SIMPLIFY = FALSE  )
  
  dataToPlot            <-  data.frame( rbindlist( dataToPlotList  ) )
  
  
#     ##construct plotly plot  # PLOTLY does not support this double grouping, make ggplot instead
#     colorPalette            <-  colors
#     nColors                 <-  nExperiments
#     p                       <-  plot_ly( dataToPlot ,  x = ~wavelength , y = ~response  , type = "scatter" ,
#       color = ~Time , symbol = ~experiment , mode = 'lines+markers' ,
#       colors = viridis_pal(option = colorPalette )( nColors ) 
#     ) 
#     plot                    <- p    %>% layout( 
#       xaxis =  plotlyAxis( "Wavelength" ) ,
#       yaxis =  plotlyAxis( "Response" )
#     ) 
  
  with( data = dataToPlot  , {
      experimentPlot      <-  ggplot( data = dataToPlot , aes( x = spectralAxis , y = response , group = interaction( Time , experiment )  ) ) +
        geom_line( aes( color = experiment , linetype = Time ) ) +
        scale_color_viridis( option = colors , discrete = TRUE  ) +
		xlab(ylab)
      return( experimentPlot )
    }
  )
}





#' @rdname SpectraInTime-class                  
#' @export 
#' @param ... additional argument, for plotting a list of spectra one can use:
#' \itemize{
#'  \item \code{times} numeric vector of time points to plot
#'  \item \code{timeUnit} time unit for \code{times} default to "seconds"
#'  \item \code{timePointsAlt} logical value indicating whether alternative time axis should be used, defaults to \code{FALSE}
#' }
#' @importFrom data.table rbindlist
#' @import ggplot2 
#' @importFrom viridis scale_color_viridis
#' 
#' @examples
#'  ## plotting a list of spectra
#'     
#'  listOfSpectra     <-  getListOfSpectraExample()
#'  plot( listOfSpectra , times = 1 , timeUnit = "hours" )
#'  plot( listOfSpectra , times = 1 , timeUnit = "hours" , timePointsAlt = TRUE  )
#'  plot( listOfSpectra , times = 1:3 , timeUnit = "hours" , colors = "B" ) 
#' \donttest{
#'  plot( listOfSpectra , timeUnit = "hours" , colors = "C" , type = "contour" )
#'}
#' @method plot list
#' @importFrom graphics par
#' @name plot
plot.list               <-  function( x  , ... , type = "line" , ncol = 2  ){	
  # make sure user settings are not modified by the function
  oldpar <- par(no.readonly = TRUE)    
  on.exit(par(oldpar))          
  
  arguments             <-  list( ... )
  argsExtended          <-  c(  arguments , list( dataList = x ) )
  
  
  ## check class
  firstClass            <-  class( x[[1]] )
  allSpectralClass      <-  checkIdenticalClass( x , firstClass )
  if( !allSpectralClass ){
    stop( "plotting method not defined or objects of different class" )
  }
  
  ### chose plot type 
  if( type == "contour" ){
	  nExperiments      <-  length( x )
	  nRow              <-  ceiling( nExperiments / ncol )
	  par( mfrow = c(nRow , ncol ) )
	  plotSilent        <-  lapply( x , function( spec ){
		argsExtended          <-  c(  arguments , list( x = spec ) , type = "contour" )
		do.call( plot , args = argsExtended )
			  } ) 
	  par( mfrow = c(1 , 1 ) )	  
  } else if (type == "line" ) {
	  resultPlot    <-  do.call( plotListOfSpectra , args = argsExtended )
	  return( resultPlot )	  
  } else {
	  stop( "ploty type not defined for list of 'SpectraInTime' choose from 'line' or 'contour' ")
  }
}




plotContourSpectra         <-  function( spectra, colorPalette , nColors , timeUnit ,  timePointsAlt, ylab, limits){
	 timePoints            <-  getTimePoints( spectra , timeUnit = timeUnit, timePointsAlt = timePointsAlt ) 
	 spectralAxis           <-  getSpectralAxis(  spectra )
	 pureSpectra           <-  getSpectra( spectra )
	 experimentName        <-  getExperimentName( spectra ) 
	 orderTime             <-  order( timePoints )
	 orderSpectralAxis       <-  order( spectralAxis )
	 spectralRange <- range(pureSpectra)
	 if(is.null(limits)){ 
		 limits <- spectralRange
	 } else{
		 pureSpectra[pureSpectra < limits[1]] <- limits[1]
		 pureSpectra[pureSpectra > limits[2]] <- limits[2]
	 }
	 # colorPalette = "D" ; nColors = 20
	 spectralDF <- data.frame(time = rep(timePoints[orderTime], length(spectralAxis[orderSpectralAxis])), spectralAxis = as.vector(t(replicate(length(timePoints),spectralAxis))), spectra = as.vector(pureSpectra[orderTime , orderSpectralAxis]))
	
	 p <- ggplot2::ggplot(data = spectralDF, ggplot2::aes(x = time, y = spectralAxis, z = spectra)) + 
			 ggplot2::stat_summary_2d(bins = c(min(length(unique(spectralDF$time))-1, 100), min(length(unique(spectralDF$spectralAxis))-1, 100)), fun = quantile, fun.args = list(probs = 0.99)) + 
			 ggplot2::scale_fill_gradientn(colours = viridis_pal(option = colorPalette )( nColors ), limits = limits) +
			 ggplot2::xlab(paste0("time in " , timeUnit)) +
	         ggplot2::ylab(ylab) +
			 ggplot2::ggtitle(experimentName)
	 
	 print(p)
	 
	
#	 filled.contour(  x = timePoints[orderTime], y = spectralAxis[orderSpectralAxis] , z = pureSpectra[orderTime , orderSpectralAxis] , col = viridis_pal(option = colorPalette )( nColors ), 
#			 xlab = paste0("time in " , timeUnit) , ylab = ylab , main = experimentName , nlevels = nColors+1)
}


