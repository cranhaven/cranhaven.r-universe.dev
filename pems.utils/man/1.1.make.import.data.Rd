\name{1.1.make.import.data}
\alias{1.1.make.import.data}
\alias{pems.objects}
\alias{is.pems}
\alias{isPEMS}
\alias{pems}
\alias{makePEMS}
\alias{rebuildPEMS}
\alias{pems.element}
\alias{as.pems}
\alias{as.pems.default}
\alias{as.pems.data.frame}
\alias{makePEMSElement}
\alias{import2PEMS}
\alias{importTAB2PEMS}
\alias{importCSV2PEMS}
\alias{importOBS2PEMS}
\alias{importOB12PEMS}
\alias{importParSYNC2PEMS}
\alias{importCAGE2PEMS}
\alias{importRoyalTek2PEMS}
\alias{importKML2PEMS}
\alias{importSEMTECH2PEMS}

\title{
  making and importing data
}

\description{
  Various pems.utils functions to make and import data as 
  pems objects.
}

\usage{

#making pems objects
 
is.pems(x, full.test = TRUE, ...)

pems(x, units = NULL, constants = NULL, history = NULL,  
          ...)

pems.element(x, name = NULL, units = NULL,   
          ...)

#associated 

isPEMS(...)
makePEMS(...)
makePEMSElement(...)
rebuildPEMS(x, ...)

\method{as.pems}{data.frame}(x, ...)

#importing data as pems objects

#general

import2PEMS(file.name = file.choose(), ..., 
          file.reader = read.delim, 
          output="pems")

importTAB2PEMS(..., file.reader = read.delim)

importCSV2PEMS(..., file.reader = read.csv)

#Horiba OBS

importOBS2PEMS(file.name = file.choose(), 
          pems = "Horiba OBS", 
          constants = NULL, history = NULL, 
          analytes = c("co", "co2", "nox", "hc"),  
          fuel = c("petrol", "diesel", "gasoline"), ...)

importOB12PEMS(file.name = file.choose(), 
          pems = "Horiba OBS", 
          constants = NULL, history = NULL, 
          analytes = c("co", "co2", "nox", "hc"),  
          fuel = c("petrol", "diesel", "gasoline"), ...)

#3DATX parSYNC

importParSYNC2PEMS(file.name = file.choose(), 
          reset.signals = TRUE, 
          history = NULL, constants = NULL, 
          pm.analyzer = "parSYNC", ... )

#3DATX CAGE
importCAGE2PEMS(..., calibrator = "CAGE")

#Sensors Inc SEMTECH

importSEMTECH2PEMS(file.name = file.choose(), 
          history = NULL, constants = NULL, 
          pems = "SEMTECH", ...)


#RoyalTek GPS

importRoyalTek2PEMS(file.name = file.choose(), 
          file.type = c("special", "txt", "nmea"),
          vbox = "RoyalTEk", history = NULL, 
          constants = NULL, ...) 


#KML files

importKML2PEMS(file.name = file.choose(), history = NULL, 
          constants = NULL, source = "Unknown", ...) 

}


\arguments{

  \item{x}{
  (A required object) For \code{is.pems}, any object to 
  be tested as a \code{pems} object. 
  For \code{pems}, an object to be used as the starting 
  point to make a \code{pems} object, so typically a 
  \code{data.frame} or another \code{pems} object. For 
  \code{pems.element}, an object to be used as the starting 
  point to make a \code{pems.element}. 
  For \code{rebuildPEMS}, a \code{pems} object to be 
  rebuilt.
} 

  \item{full.test}{
  (Logical) For \code{is.pems}, should the full pems 
  test be applied and the \code{pems} structure 
  confirmed?  
} 

  \item{\dots}{
  (Optional) Other arguments, handling varies. For 
  \code{is.pems} these are ignored. For \code{pems} 
  these are added to the pems object unmodified. For 
  \code{import...} functions, these are passed on and 
  added to the \code{constants} component of the 
  \code{pems} object. Note: This different handling 
  is experimental and may be subject to change in future. 
}

  \item{units, constants, history}{
  (Default pems arguments) These are arguments that are routinely generated for 
  \code{pems} objects. \code{units} holds unit ids for unit management, \code{constants} holds 
  constants that should used specifically with data in the \code{pems} object, and \code{history} 
  holds the \code{pems} object modification history.   
}

  \item{name}{
  (Default pems.element argument) \code{name} (and \code{units}) are arguments that are routinely 
  generated for \code{pems.element} objects.    
}

  \item{file.name}{
  (file connection, etc.) For \code{import...} functions, the file/source to be imported. Note: the 
  default, \code{file.name = file.choose()}, automatically opens a file browser if this argument is 
  not supplied.   
}

%  \item{time.stamp, local.time, time.format}{
%   Relatively crude \code{import...} functions are useful for importing data from the clipboard or simple 
%   file types. However, these sometimes need careful handling of time data. If supplied, \code{time.stamp} 
%   and \code{local.time} are used as indices or ids (column numbers or names) for data series that the 
%   user would like to use as the data time stamp and local time records, respectively. If supplied, 
%   \code{time.format} sets the format in which the \code{time.stamp} should be imported if present/idenified.    
%}

  \item{file.type, file.reader}{
  Data reader parameters for some \code{import...} functions. \code{file.type} is the type of file 
  to be imported. Note: Some \code{import...} functions can handle more than one file type, and 
  \code{file.type = "[option]"} should be used to identify these. (Note: \code{file.type} options 
  are typically file type identifiers, such as the file extensions, and a default \code{'special'}, 
  which leaves the choice to the function. This way this option can typically be ignored unless, e.g. 
  the function does not recognise the file type but the user knows it and wants to force the method.) 
  \code{file.reader} identifies the R method/function that should be used to read data from the supplied 
  file. For example, for \code{importTAB2PEMS} and \code{importCSV2PEMS}, by default, these are the 
  standard \code{R} \code{read...} functions \code{\link{read.delim}} and \code{\link{read.csv}}, 
  respectively.   
}

  \item{output}{
  Where included in formal arguments, an option to control function output.
}

  \item{pems, vbox, pm.analyzer, calibrator, source}{
  (Character vectors) For some \code{import...} functions, data source descriptions may be automatically 
  added to the \code{pems} object. \code{pems} and \code{vbox} are two examples, but others, such as 
  vehicle and fuel descritpions can also be added in a similar fashion. Note: These are for user-reference, 
  so can say whatever you want.
}

  \item{analytes}{
  (Character vector) For \code{import...} functions, the names of any pems elements to be tagged as 
  analyte concentrations. Note: If the PEMS unit reports concentrations rather than emissions it is 
  often useful to identify these at import to avoid confusion, and to simplify later handling. So, if 
  encountered, analyte names are prefixed with the term \code{'conc.'}.    
}

  \item{fuel}{
  Some \code{import...} functions that handle exhaust monitoring system data may assume fuel types when 
  calibrating inputs or calculating constants. In such cases the \code{fuel} argument is also included 
  to identify which fuel was used. 
}

  \item{reset.signals}{
   (Logical or Character vector) For \code{importParSYNC2PEMS}, should any raw signal be reset? The 
   default (\code{TRUE}) reverses the sign of opacity and ionization signals.      
}

}

\details{

  \code{is.pems} tests if an object is/is not a \code{pems} object.

  \code{pems} makes a \code{pems} object using supplied data and information. 

  \code{pems.element} makes a \code{pems.element} object using supplied data and information.

  \code{as.pems...} functions attempt to convert a supplied object into a \code{pems} object. 
  Currently, there is only a \code{data.frame} method and (by \code{default}) a \code{pems} 
  method.

  \code{isPEMS}, \code{makePEMS} and \code{makePEMSElement} are historical code, retained for 
  backward compatibility.

  \code{rebuildPEMS} rebuilds \code{pems} object as a different build version. 

  Crude \code{import...} functions import simple file structures, and are useful for getting 
  data quickly into \code{R:pems.utils}. \code{importTAB2PEMS} imports tab delimited files and 
  clipboard content. \code{importCSV2PEMS} imports comma delimited files. Both assume a simple 
  file structure (i.e. data series in columns with names as headers), but require some time data 
  management by the user. Note: These are wrappers for \code{import2PEMS}. 

  Other \code{import...} import specific file types.

  \code{importOBS2PEMS} imports standard Horiba OBS files and converts them to \code{pems} 
  objects. See Notes below. 

  \code{importOB12PEMS} imports \code{.OB1} files and converts them to \code{pems} objects. \code{OB1} 
  files are generated by a Visual Basic PEMS data handler used during the RETEMM project. Notes below. 

  \code{importParSYNC2PEMS} imports standard parSYNC files and converts them to \code{pems} 
  objects. See Notes below. 

  \code{importCAGE2PEMS} imports standard CAGE files and converts them to \code{pems} objects. 
  See \code{importParSYNC2PEMS} Notes below.

  \code{importSEMTECH2PEMS} imports Sensors Inc. SEMTECH ECOSTAR files and converts them to \code{pems} 
  objects. See Notes below. 

  \code{importRoyalTek2PEMS} imports \code{.txt} and \code{.nmea} format Royal Tek GPS files and 
  converts them to \code{pems} objects. See Notes below. 

  \code{importKML2PEMS} imports \code{.kml} format KML files and 
  converts them to \code{pems} objects. See Notes below. 
 
}


\value{

  \code{is.pems} return a logical, \code{TRUE} if the supplied object is \code{pems} class, 
  otherwise \code{FALSE}. If the argument \code{full.test = TRUE} is also supplied, additional 
  information about the object is returned as \code{comment(output)}. 

  \code{pems} and \code{pems.element} functions return \code{pems} and \code{pems.element} 
  objects, respectively, made using the supplied file and any additional information also 
  supplied in the same call.

  \code{rebuildPEMS} rebuilts the supplied \code{pems} object. The default rebuilds as the lastest 
  build structure.

  \code{import...} functions return a \code{pems} object, made using the supplied file and any 
  additional information also supplied in the same call. 

}

\references{
  References in preparation.
}
\author{
  Karl Ropkins
}
\note{

  \code{isPEMS}, \code{makePEMS} and \code{makePEMSElement} were earlier functions that performed 
  the same functions as \code{is.pems}, \code{pems} and \code{pems.elements}, respectively. The 
  the current versions of these functions are wrappers for their replacements. 
     
  With the crude \code{import...} functions (\code{import2PEMS}, \code{importTAB2PEMS}, 
  \code{importCSV2PEMS}) modifications are minimal. Unless any additional changes are requested 
  in the \code{import...(...)} call, the data is simply read in as a \code{data.frame} and 
  converted to a \code{pems} object.  

  With \code{importOBS2PEMS}, OBS data is also modified as follows: data series names are simplified and 
  converted to lower case to simplify use in R; the data series \code{time.stamp} and \code{local.time} 
  are added (generated using the file time stamp, the row counter and the log.rate constant); 
  data series \code{latitude} and \code{longitude} are resigned according to set N/S and E/W values, 
  if these are present/valid; \code{latitude} and \code{longitude} units are also reset to \code{'d.degLat'} 
  and \code{'d.degLon'}. Any data series names in \code{analytes} is renamed \code{'conc.[analyte name]'}. 
  If not supplied in the \code{importOBS2PEMS}, typical OBS constants are currently assumed. Several of these 
  are based on emission source fuel. Defaults for these are generated according to \code{fuel} (default 
  \code{'petrol'}).

  With \code{importOB12PEMS}, handling is similar to that with \code{importOBS2PEMS}. 

  With \code{importParSYNC2PEMS}, the parSYNC data modifications are as follows: parSYNC Date and Time 
  data-series are merged and converted into \code{*POSIX} as \code{time.stamp}; \code{local.time} is calculated 
  from this; the parSYNC data series Timestamp is retained as \code{parsync.timestamp}; by default units are 
  removed from data-series names, but retained as \code{units(pems)}; (again by default) all names are 
  converted to lower case. The default option \code{reset.signal = TRUE} reverses the voltage reading 
  of opacity and ionization time-series (x = - x), if present. Alternatively, imports can be identified 
  specifically by name, e.g. \code{reset.signal = "opacity"} to just reset opacity. Typical OBS constants 
  are currently assumed, if not supplied. Several of these are based on emission source fuel. The default 
  assumption is that the fuel is \code{'petrol'} unless \code{fuel} has been assigned.  

  With \code{importCAGE2PEMS}, handling is similar to that with \code{importParSYNC2PEMS}. 

  With \code{importSEMTECH2PEMS}, SEMTECH data is imported and modified as follows: data series names are simplified and 
  converted to lower case to simplify use in R; the data series \code{time.stamp} and \code{local.time} 
  are added (generated using the file time stamp). Defaults constants are assigned according to \code{fuel} (default 
  \code{'petrol'}). This function in in-development and has so far only been used with files from two 
  sources, so handle with care, and \code{time.format} has varied so may need resetting for some files.

  With \code{importRoyalTek2PEMS}, the Royal Tek data modifications are currently being documented.  

  With \code{importKML2PEMS}, the function attempts to import and time.stamp, latitude, longtiude and altitude data 
  in the suppled KML file. This function in in-development and has so far only been used with KML files from one 
  source, so handle with care.  
 
}

%\section{Warning }{
%  Currently, \code{makePEMS} and \code{import...} functions handle extra arguments differently. (See 
%  Arguments above for details.) This may be subject to change.
%}

\seealso{
  See \code{\link{ref.unit.conversions}} and \code{\link{convertUnits}} for general unit handling; 
  \code{\link{merge.pems}} for pems data merging and alignment. 
}
\examples{

###########
##example 1 
###########

#make little pems

data <- data.frame(speed=1:10, emissions=1:10)
units <- c("m/s", "g/s")
pems <- pems(x = data, units=units, example="my record") 
 
pems                       #the pems object
summary(pems)              #summary of held data
pems$speed                 #the speed pems.element

#import data file as pems using import... functions

#For example, to import CSV file as pems object 
\dontrun{
pems <- importCSV2PEMS() 
}

}

\keyword{ methods }
