## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.pos = 'H',
  comment = "#>"
)

## ----message=F, warning =F , echo = FALSE-------------------------------------
library(webshot)
library(plotly) 

## ----echo = TRUE, message = FALSE, results = TRUE-----------------------------
library( spectralAnalysis )
spectralEx                 <-  getSpectraInTimeExample( )
str( spectralEx)

## ----echo = TRUE, message = FALSE, results = TRUE-----------------------------
dim( spectralEx)
getExperimentName(spectralEx)
getExtraInfo( spectralEx )
getStartTime( spectralEx )
getTimePoints( spectralEx )
getTimePoints( spectralEx , timePointsAlt = TRUE , timeUnit = "seconds" ) 
getTimePoints( spectralEx , timeUnit = "minutes" )
getTimePoints( spectralEx , timeUnit = "hours" )
getUnits( spectralEx )
getTimePoints( spectralEx , timePointsAlt = TRUE ) 
spectra       <-  getSpectra( spectralEx )
dim( spectra )

## ----echo = TRUE, message = FALSE, results = TRUE-----------------------------
setTimePointsAlt( spectralEx  )  <-  getTimePoints( spectralEx )  -  200 

## ----echo = TRUE, message = FALSE, results = TRUE, error = TRUE---------------
setTimePointsAlt( spectralEx  )  <-  getTimePoints( spectralEx ) * 5 

## ----eval = FALSE-------------------------------------------------------------
#  ?SpectraInTime

## ----echo = TRUE, message = FALSE, results = TRUE , eval = FALSE--------------
#  allSPCFiles     <- loadAllSPCFiles(directoryFiles)

## ----echo = TRUE, message = FALSE, results = TRUE-----------------------------
print( r( 2.5 , 10.8) )
print( r( c(1 , 3 , 2 , 6 , 9 , 3 ) ) )

print( e( 1, 3 ,5 ,6 ,7 ,8 ) )

## ----echo = TRUE, message = FALSE, results = TRUE-----------------------------
# range subsetting 
spectralEx                      <-   getSpectraInTimeExample()
spectraSubset                   <-  spectralEx[ r( 1000 , 30000 ) , r(130 , 135 ) ]
getTimePoints( spectraSubset )
getSpectralAxis(  spectraSubset )
spectraTimeSubset               <-  spectralEx[ r( 1000 , 30000 ) ,  ]
spectraWavelengthSubset         <-  spectralEx[  ,  r(130 , 135 ) ]

# other types of subsetting 
# logical
spectraSubsetLogical   <-  spectralEx[ getTimePoints( spectralEx ) > 300  ,
    getSpectralAxis( spectralEx ) <= 500 ]
subsetLogTimeAlt       <-  spectralEx[ 
    getTimePoints( spectralEx , timePointsAlt = TRUE ) > 0 ,
    getSpectralAxis( spectralEx ) <= 500 ]

# integer

subsetInteger                   <-  spectralEx[ c( 1, 5, 10)  , c( 4 , 4 , 4 , 8 , 16) ] 

# closest element matching 

spectraSubsetElem               <-  spectralEx[ e( 1.234 , 3.579 ) ,
    e( 200.001 , 466.96  ) , timeUnit = "hours" ]
getTimePoints( spectraSubsetElem , timeUnit = "hours" )
getSpectralAxis( spectraSubsetElem  )

## ----echo = TRUE, message = FALSE, results = TRUE , eval = TRUE---------------
summarySpec       <-  summary( spectralEx )
str( summarySpec )

## ----plotBaseTime, fig.cap = "time view plot",  echo = TRUE, message = FALSE, results = TRUE , eval = TRUE----
data = getSpectraInTimeExample()
library( plotly )
plot( x =  data[ e( 1 , 2 , 3) , , timeUnit = "hours" ] , type = "time" , timeUnit = "hours" , timePointsAlt = FALSE ) 

## ----plotBaseWavelenght, fig.cap = "Wavelength view plot" ,  echo = TRUE, message = FALSE, results = TRUE , eval = TRUE----
plot( x =  data[ , e( seq( 200 , 400 , 50 ) ) ] , type = "spectralAxis" , timeUnit = "minutes" , timePointsAlt = TRUE ) 

## ----plotBaseContour, fig.cap = "Contour plot" ,  echo = TRUE, message = FALSE, results = TRUE , eval = TRUE----
plot( x = data , type = "contour" , nColors = 200 , colors = "C" , timeUnit = "seconds", timePointsAlt = TRUE ) 

## ----plotBase3D, fig.cap = c("3D plot" , "time view plot" , "wavelength view plot" , "contour plot"),  echo = TRUE, message = FALSE, results = TRUE , eval = FALSE----
#  plot( x =  data , type = "3D" , timeUnit = "hours" , timePointsAlt = FALSE )

## ----plotListLine, fig.cap = "Line plot for list of SpectraInTime"------------
listOfSpectra     <-  getListOfSpectraExample()
plot( listOfSpectra , times = 1:3 , timeUnit = "hours" , colors = "A" ) 

## ----plotListContour, fig.cap = "Contour plot for list of SpectraInTime"------
plot( listOfSpectra , timeUnit = "hours" , colors = "C" , type = "contour" )

## ----echo = TRUE, message = FALSE, results = TRUE-----------------------------
processTimes        <-  getProcessTimesExample() 
processTimes

## ----echo = TRUE, message = FALSE, results = TRUE-----------------------------
processTimesFrame   <-  getProcessTimesFrameExample()
processTimesFrame

## ----echo = TRUE, message = FALSE, results = TRUE-----------------------------
spectra             <-  getSpectraInTimeExample()
listOfSpectra       <-  getListOfSpectraExample()
pathProcessTimes    <-  getPathProcessTimesExample()
   
ex1    <-  timeAlign( x = spectra , y = processTimes ,
    cutCooling = TRUE , cutBeforeMinTemp = TRUE )
ex2    <-  timeAlign( x = listOfSpectra , y = processTimesFrame ,
    cutCooling = TRUE , cutBeforeMinTemp = TRUE )
ex3    <-  timeAlign( x = listOfSpectra , y = pathProcessTimes ,
    cutCooling = TRUE , cutBeforeMinTemp = TRUE  , timeFormat =  "%Y-%m-%d %H:%M:%OS" )

## ----echo = TRUE--------------------------------------------------------------
exampleData1       <-  readSpectra( system.file( "exampleData/exampleExperiment1.txt" ,
        package = "spectralAnalysis") ) 

## ----plotReal, fig.cap = "Contour plot 2 real example experiments"------------
plot( exampleData1, type = "contour" )

## -----------------------------------------------------------------------------
dim( exampleData1 )
wavelengthRange         <-  r ( 800 ,  1625 )
spectralDataSelect      <-  exampleData1[  r( 0 , 5 ) , wavelengthRange , timeUnit = "hours" ]

## ----plot4, fig.cap =  "Selected spectra of an example experiment", fig.align = "center", fig.pos = "h" , echo = FALSE----
plot( spectralDataSelect , type =  "contour" ) 

## ----plot5, fig.cap = "Raw spectra example experiment", fig.align = "center", fig.pos = "h"----
timesToShow           <-  e( 0.5 , 5 )
plot( spectralDataSelect[ timesToShow , , timeUnit = "hours"] , type = "time" ) 

## ----plot6, fig.cap = "Baseline corrected spectra  example experiment", fig.align = "center", fig.pos = "h"----
spectralDataBaseline        <-  baselineCorrect( spectralDataSelect ,
    method = 'modpolyfit', degree = 4 )
plot( spectralDataBaseline[ timesToShow , , timeUnit = "hours"] , type = "time" ) 

## ----plot7, fig.cap = "Smoothed spectra example experiment", fig.align = "center", fig.pos = "h"----
spectralDataSmooth       <-  smooth( spectralDataBaseline , window = 5 )
plot( spectralDataSmooth[ timesToShow , , timeUnit = "hours"] , type = "time" ) 

## ----plot8, fig.cap = "Derivative spectra example experiment", fig.align = "center", fig.pos = "h"----
spectralDataDerivative   <-  smooth(  spectralDataBaseline , derivative = 1  )
plot( spectralDataDerivative[ timesToShow , , timeUnit = "hours"] , type = "time" ) 

## ----plot9, fig.cap = "Integral normalized spectra example experiment", fig.align = "center", fig.pos = "h"----
spectralDataNormalized   <-  normalize( spectralDataSmooth , method = "integration"   )
plot( spectralDataNormalized[ timesToShow , , timeUnit = "hours"] , type = "time" ) 

## ----eval = FALSE-------------------------------------------------------------
#  ?scatterCorrrect

## -----------------------------------------------------------------------------
allSpectraDataProcessed    <-  lapply( listOfSpectra , preprocess , with = spectralDataSmooth  )

## ----results = FALSE----------------------------------------------------------
spectralExSelect  <-  spectralDataSmooth[ r( 0 , 5 ) , , timeUnit = "hours" ] 
nmfResult    <-  spectralNMF( spectralExSelect , rank = 3 , subsamplingFactor = 5 )
nmfObject    <-  getDimensionReduction( nmfResult , type = "NMF")$NMF

## ----plotNMF, fig.cap =  "NMF-trends on example experiments"------------------

nmfTrends    <-  t( NMF::coef( nmfObject ) )
matplot( nmfTrends , type = "l" , x = getTimePoints( spectralExSelect , timeUnit = "hours"  ) , xlab = "time in hours"  )

## ----eval = FALSE-------------------------------------------------------------
#  ?spectralNMF

