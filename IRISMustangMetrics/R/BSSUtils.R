##
##    Utility functions for communicating with the Backend Storage Service (BSS)
##    of IRIS DMC project MUSTANG.
##
##    Copyright (C) 2012  Mazama Science, Inc.
##    by Jonathan Callahan, jonathan@mazamascience.com
##
##    This program is free software; you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation; either version 2 of the License, or
##    (at your option) any later version.
##
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with this program; if not, write to the Free Software
##    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

utils::globalVariables(c('snclq','value','everything'))

################################################################################
# Method to save a list of Metric objects to disk.
#
# Internet access methods come from the RCurl package which is loaded by the
# IRISSeismic package which is required by the siesmicMetrics package.
################################################################################

saveMetricList <- function(metricList,
                           id=Sys.getpid(), rdata=FALSE) {
  
  name <- paste0("metricList_",id)      
  
  if (rdata) {
    
    # Save the data as .RData file
    filename <- paste(name,"RData",sep=".")
    result <- try( saveReturn <- save(metricList,file=filename),
                   silent=TRUE)
    if (inherits(result,"try-error")) {
      stop(geterrmessage())      
    }
    
  } else {
    
    # Save the data as .xml file
    filename <- paste(name,"xml",sep=".")
    
    # Choose the appropriate data-to-XML conversion
    if (length(metricList) == 1 && inherits(metricList[[1]],"character")) { # For latency which comes preformatted as XML
      xml <- metricList[[1]]
    } else if (inherits(metricList[[1]],"GeneralValueMetric")) {
      xml <- metricList2Xml(metricList)
    } else if (inherits(metricList[[1]],"SingleValueMetric")) {
      xml <- metricList2Xml(metricList)    
    } else if (inherits(metricList[[1]],"SpectrumMetric")) {
      xml <- spectrumMetric2Xml(metricList)
    } else if (length(metricList) == 1 && inherits(metricList[[1]],"MultipleTimeValueMetric")) {
      xml <- timesMetric2Xml(metricList[[1]])
    } else {
      stop(paste("saveMetricList: metric of class '",class(metricList[[1]]),"' is not recognized.",sep=""))
    }
    
    result <- try( saveReturn <- write(file=filename,xml,append=TRUE), silent=TRUE)
    if (inherits(result,"try-error")) {
      stop(geterrmessage())      
    }
    
  }
  
  return(invisible(filename))
  
}

################################################################################
# Methods to parse and simplifiy incoming error messages
################################################################################

convertBssErrors <- function(err_msg) {
  
  # Look for lines like this:
  #
  # <message>...</message>
  
  # TODO:  Use XML parsing to get BSS error messages
  # NOTE:  It seems that the long java dumps are cut off when returned by geterrmessage() so we don't have complete XML
  # NOTE:  That's why we're using simple string matching right now.
  lines <- unlist(stringr::str_split(err_msg,"\n"))
  bssErrorMessages <- lines[stringr::str_detect(lines,"<message>")]
  err_msg <- bssErrorMessages
  err_msg <- stringr::str_trim(stringr::str_replace(stringr::str_replace(err_msg,"</message>",""),"<message>",""))
  return(err_msg)
}


################################################################################
# getBssMetricList method returns a list of SingleValueMetric objects
#
# This method extends the IrisClient object from package "IRISSeismic" so that it 
# can access BSS webservices that are internal to project MUSTANG.
#
# See documentation on Duncan Temple Lang's PDF package for details on the 
# XML parsing functions used in this method.
################################################################################

if (!isGeneric("getBssMetricList")) {
  setGeneric("getBssMetricList", function(obj, network, station, location, channel,
                                          starttime, endtime, metricName, url) {
    standardGeneric("getBssMetricList")
  })
}

getBssMetricList.IrisClient <- function(obj, network, station, location, channel,
                                        starttime, endtime, metricName, url) {
  
  # TODO:  Talk with Andrew C about harmonizing BSS with webservice wildcarding syntax
  # Convert webservice style wildcarding with "*" to BSS equivalent "".
  network <- stringr::str_replace(network, "\\*", "")
  station <- stringr::str_replace(station, "\\*", "")
  location <- stringr::str_replace(location, "\\*", "")
  channel <- stringr::str_replace(channel, "\\*", "")
  
  # Obtain the XML and parse it
  result <- try( measurementsXml <- getMetricsXml(obj,network,station,location,channel,starttime,endtime,metricName,url),
                 silent=TRUE)
  
  # Handle error response
  if (inherits(result,"try-error")) {  
    err_msg <- geterrmessage()
    if (stringr::str_detect(err_msg,regex("Not Found",ignore_case=TRUE))) {
      stop(paste("getBssMetricList.IrisClient: URL Not Found:",url))
    } else {
      stop(paste("getBssMetricList.IrisClient:",err_msg),url)
    } 
    
  }  
  
  # No errors so proceed
  
  doc <- XML::xmlRoot(XML::xmlTreeParse(measurementsXml))
  
  if (length(XML::xmlChildren(doc)) == 0) {
    
    # Empty result -- no measurements found
    metricList <- list()
    
  } else {
    
    # TODO:  Add support for multiple error messages
    # Check for errors
    bssErrors <- XML::xmlElementsByTagName(doc,"error")
    if (length(bssErrors) > 0) {
      msg1 <- XML::xmlElementsByTagName(bssErrors[[1]])[[1]]
      err_msg <- paste("getBssMetricList:",XML::xmlValue(msg1))
      stop(err_msg)
    }
    
    if (metricName == "up_down_times") {
      
      # An example returning a single MultipleTimeValueMetric
      #
      # http://service.iris.edu//mustang/measurements/1/query?metric=up_down_times&target=IU.XMAS.10.BHE.B&startafter=2012-07-00T00:00:00@endbefore=2012-07-00T00:00:00
      # <measurements>
      #  <up_down_times net="IU" sta="XMAS" loc="10" chan="BHE" qual="M" target="IU.XMAS.10.BHE.B" start="2012-07-01T00:00:00.000" end="2012-07-02T00:00:00.000">
      #   <t value="2012-07-01T00:00:00.000"/>
      #   <t value="2012-07-01T22:48:36.000"/>
      #   <t value="2012-07-01T22:58:43.000"/>
      #   <t value="2012-07-01T23:23:31.000"/>
      #   <t value="2012-07-01T23:33:39.000"/>
      #   <t value="2012-07-01T23:59:59.000"/>
      #  </up_down_times>
      # </measurements>
      
      # TODO:  support BSS response with more than one MultipleTimeValueMetric
      if (length(XML::xmlChildren(doc)) > 1) {
        err_msg <- paste("getBssMetricList:  more than one MultipleTimeValueMetric in BSS response")
        stop(err_msg)
      }
      
      # Extract measurements attributes as a character matrix with one column for each measurement
      # and rows named: "value"  "net"    "sta"    "loc"    "chan"   "qual"   "target" "start"  "end"
      # NOTE:  The following attributeMatrix should only have a single row
      attributeMatrix <- XML::xmlSApply(doc, XML::xmlAttrs)
      
      # Pre-allocate a list of the proper length
      metricList <- vector("list", ncol(attributeMatrix))
      
      # Convert information into MultipleTimeValueMetrics
      # NOTE:  Should only be one metric
      for (i in seq(ncol(attributeMatrix))) {
        snclq <- attributeMatrix["target",i]
        starttime <- as.POSIXct(attributeMatrix["start",i], format="%Y-%m-%dT%H:%M:%OS", tz="GMT")
        endtime <- as.POSIXct(attributeMatrix["end",i], format="%Y-%m-%dT%H:%M:%OS", tz="GMT")
        metricName <- colnames(attributeMatrix)[i]
        
        # NOTE:  <t value="..."/> elements only have a single attribute so we don't have to acces it with ["value"]
        metricNodes <- XML::xmlChildren(doc)
        timeStrings <- as.character(XML::xmlSApply(metricNodes[[1]],XML::xmlAttrs))
        upDownTimes <- as.POSIXct(timeStrings, format="%Y-%m-%dT%H:%M:%OS", tz="GMT") 
        
        metricList[[i]] <- new("MultipleTimeValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName="up_down_times", values=upDownTimes)        
      }
      
      
    } else {
      
      # An example returning multiple SingleValueMetrics
      #
      # http://service.iris.edu/mustang/measurements/1/query?net=IU&sta=ANMO&loc=00&chan=BH?&timewindow=2012-07-12T00:00:00,2012-07-13T00:00:00&metric=percent_availability
      # <measurements>
      #  <percent_availability value="100.00" net="IU" sta="ANMO" loc="00" chan="BH1" qual="M" target="IU.ANMO.00.BH1.B" start="2012-07-12T00:00:00.000" end="2012-07-13T00:00:00.000"/>
      #  <percent_availability value="100.00" net="IU" sta="ANMO" loc="00" chan="BH2" qual="M" target="IU.ANMO.00.BH2.B" start="2012-07-12T00:00:00.000" end="2012-07-13T00:00:00.000"/>
      #  <percent_availability value="100.00" net="IU" sta="ANMO" loc="00" chan="BHZ" qual="M" target="IU.ANMO.00.BHZ.B" start="2012-07-12T00:00:00.000" end="2012-07-13T00:00:00.000"/>
      #  ...
      # </measurements>
      
      # Extract measurements attributes as a character matrix with one column for each measurement
      # and rows named: "value"  "net"    "sta"    "loc"    "chan"   "qual"   "target" "start"  "end"
      attributeMatrix <- XML::xmlSApply(doc, XML::xmlAttrs)
      
      # Pre-allocate a list of the proper length
      metricList <- vector("list", ncol(attributeMatrix))
      
      # Convert information into SingleValueMetrics
      for (i in seq(ncol(attributeMatrix))) {
        snclq <- attributeMatrix["target",i]
        starttime <- as.POSIXct(attributeMatrix["start",i], format="%Y-%m-%dT%H:%M:%OS", tz="GMT")
        endtime <- as.POSIXct(attributeMatrix["end",i], format="%Y-%m-%dT%H:%M:%OS", tz="GMT")
        metricName <- colnames(attributeMatrix)[i]
        value <- as.numeric(attributeMatrix["value",i])
        metricList[[i]] <- new("SingleValueMetric", snclq=snclq, starttime=starttime, endtime=endtime, metricName=metricName, value=value)
      }
      
    }
    
  }
  
  return(metricList)
}

# All arguments specified
setMethod("getBssMetricList", signature(obj="IrisClient", 
                                        network="character", location="character", station="character", 
                                        channel="character",starttime="POSIXct", endtime="POSIXct", 
                                        metricName="character", url="character"), 
          function(obj, network, station, location, channel, starttime, endtime, metricName, url) 
            getBssMetricList.IrisClient(obj, network, station, location, channel, starttime, endtime, 
                                        metricName, url))
# url="missing"
setMethod("getBssMetricList", signature(obj="IrisClient", 
                                        network="character", location="character", station="character", 
                                        channel="character",starttime="POSIXct", endtime="POSIXct", 
                                        metricName="character", url="missing"), 
          function(obj, network, station, location, channel, starttime, endtime, metricName, url) 
            getBssMetricList.IrisClient(obj, network, station, location, channel,
                                        starttime, endtime, metricName,
                                        url="http://service.iris.edu/mustang/measurements/1/query?"))


################################################################################
# getMetricsXml method returns measurementsXml from the BSS metrics webservice:
#
# http://service.iris.edu/mustang/measurements
#
################################################################################

if (!isGeneric("getMetricsXml")) {
  setGeneric("getMetricsXml", function(obj, network, station, location, channel,
                                       starttime, endtime, metricName, url) {
    standardGeneric("getMetricsXml")
  })
}

getMetricsXml.IrisClient <- function(obj, network, station, location, channel,
                                     starttime, endtime, metricName, url) {
  
  # Assemble URL
  if (network != "") {
    url <- paste(url,"net=",network,sep="")    
  }
  if (station != "") {
    url <- paste(url,"&sta=",station,sep="")
  }
  if (location != "") {
    # NOTE:  Unlike other web services, the measurements service expects "" rather than "--" for the BLANK location designator.
    location <- stringr::str_replace(location,"--","")
    url <- paste(url,"&loc=",location,sep="")
  }
  if (channel != "") {
    url <- paste(url,"&cha=",channel,sep="")
  }
  # TODO:  What sort of starttime/endtime checking should we have?
  url <- paste(url,"&timewindow=",format(starttime,"%Y-%m-%dT%H:%M:%OS", tz="GMT"),",",
               format(endtime,"%Y-%m-%dT%H:%M:%OS", tz="GMT"),sep="") # webservice requires "T" format
  
  url <- paste(url,"&metric=",metricName,sep="")
  
  if (obj@debug) {
    write(paste("URL =",url), stdout())
  }
  
  # Get data from BSS measurements webservice
  # NOTE:  RCurl::getURLContent returns a binary objected based on the "resulting HTTP  header's Content-Type field."
  # NOTE:  Use RCurl::getURL to return data as character.
  result <- try( measurementsXml <- RCurl::getURL(url, useragent=obj@useragent),
                 silent=TRUE)
  
  # Handle error response
  if (inherits(result,"try-error")) {  
    err_msg <- geterrmessage()
    if (stringr::str_detect(err_msg,regex("Not Found",ignore_case=TRUE))) {
      stop(paste("getMetricsXml.IrisClient: URL Not Found:",url))
    } else if (stringr::str_detect(err_msg,regex("couldn't connect to host",ignore_case=TRUE))) {
      stop(paste("getMetricsXml.IrisClient: Couldn't connect to host"),url)
    } else {
      stop(paste("getMetricsXml.IrisClient:",err_msg),url)
    } 
    
  }
  
  # No errors so proceed
  
  return(measurementsXml)
}

# All arguments specified
setMethod("getMetricsXml", signature(obj="IrisClient", 
                                     network="character", location="character", station="character", 
                                     channel="character",starttime="POSIXct", endtime="POSIXct", 
                                     metricName="character", url="character"), 
          function(obj, network, station, location, channel, starttime, endtime, metricName, url) 
            getMetricsXml.IrisClient(obj, network, station, location, channel,
                                     starttime, endtime, metricName, url))
# url="missing"
setMethod("getMetricsXml", signature(obj="IrisClient", 
                                     network="character", location="character", station="character", 
                                     channel="character",starttime="POSIXct", endtime="POSIXct", 
                                     metricName="character", url="missing"), 
          function(obj, network, station, location, channel, starttime, endtime, metricName, url) 
            getMetricsXml.IrisClient(obj, network, station, location, channel,
                                     starttime, endtime, metricName,
                                     url="http://service.iris.edu/mustang/measurements/1/query?"))



################################################################################
# getLatencyValuesXml method returns XML from the LatencyValues servlet:
#
# http://www.iris.edu/mustang/latency/LatencyValues?net=IU&stn=ANMO&loc=*&chn=BHZ
#
################################################################################

if (!isGeneric("getLatencyValuesXml")) {
  setGeneric("getLatencyValuesXml", function(obj, network, station, location, channel, url) {
    standardGeneric("getLatencyValuesXml")
  })
}

getLatencyValuesXml.IrisClient <- function(obj, network, station, location, channel, url) {
  
  # Assemble URL
  if (network != "") {
    url <- paste(url,"net=",network,sep="")    
  }
  if (station != "") {
    url <- paste(url,"&sta=",station,sep="")
  }
  if (location != "") {
    # NOTE:  Unlike other web services, the measurements service expects "" rather than "--" for the BLANK location designator.
    location <- stringr::str_replace(location,"--","")
    url <- paste(url,"&loc=",location,sep="")
  }
  if (channel != "") {
    url <- paste(url,"&cha=",channel,sep="")
  }
  
  if (obj@debug) {
    write(paste("URL =",url), stdout())
  }
  
  # Get data from LatencyValues servlet
  # NOTE:  RCurl::getURLContent returns a binary objected based on the "resulting HTTP  header's Content-Type field."
  # NOTE:  Use RCurl::getURL to return data as character.
  result <- try( latencyValuesXml <- RCurl::getURL(url, useragent=obj@useragent),
                 silent=TRUE)
  
  # Handle error response
  if (inherits(result,"try-error")) {  
    err_msg <- geterrmessage()
    if (stringr::str_detect(err_msg,regex("Not Found",ignore_case=TRUE))) {
      stop(paste("getLatencyValuesXml.IrisClient: URL Not Found:",url))
    } else if (stringr::str_detect(err_msg,regex("couldn't connect to host",ignore_case=TRUE))) {
      stop(paste("getLatencyValuesXml.IrisClient: Couldn't connect to host"),url)
    } else {
      stop(paste("getLatencyValuesXml.IrisClient:",err_msg),url)
    } 
    
  }
  
  # No errors so proceed
  
  return(latencyValuesXml)
}

# All arguments specified
setMethod("getLatencyValuesXml", signature(obj="IrisClient", 
                                           network="character", location="character", station="character", 
                                           channel="character", url="character"), 
          function(obj, network, station, location, channel, url) 
            getLatencyValuesXml.IrisClient(obj, network, station, location, channel, url))
# url="missing"
setMethod("getLatencyValuesXml", signature(obj="IrisClient", 
                                           network="character", location="character", station="character", 
                                           channel="character", url="missing"), 
          function(obj, network, station, location, channel, url) 
            getLatencyValuesXml.IrisClient(obj, network, station, location, channel,
                                           url="http://www.iris.edu/mustang/latency/LatencyValues?"))


################################################################################
# Simpler access to measurements from MUSTANG
################################################################################

# createBssUrl -----------------------------------------------------------------
#
# Create the URL needed to access data from the MUSTANG BSS
#
# Example of getting single valued measurements output from the ''measurements' service:
#
# http://service.iris.edu/mustang/measurements/1/query?net=IU&sta=ANMO&loc=10&cha=BHZ&timewindow=2013-06-01T00:00:00,
#       2013-06-02T00:00:00%20&format=text&metric=sample_mean,sample_min

if (!isGeneric("createBssUrl")) {
  setGeneric("createBssUrl", function(obj, network, station, location, channel, starttime, endtime, metricName, ... ) {
    standardGeneric("createBssUrl")
  })
}

createBssUrl.IrisClient <- function(obj, network, station, location, channel,
                                    starttime, endtime, metricName, constraint="", url=NULL) {

  if (is.null(url)){
     url <- "http://service.iris.edu/mustang/measurements/1/query?"
  }
  
  # Assemble URL
  if (network != "") {
    url <- paste(url,"net=",network,sep="")    
  }
  if (station != "") {
    url <- paste(url,"&sta=",station,sep="")
  }  
  if (location != "") {
    # NOTE:  Unlike other web services, the measurements service expects "" rather than "--" for the BLANK location designator.
    location <- stringr::str_replace(location,"--","")
    url <- paste(url,"&loc=",location,sep="")
  }
  if (channel != "") {
    url <- paste(url,"&cha=",channel,sep="")
  }
  # REC -- allow a new constraint term called 'intersects' which means we don't use the timewindow term
  # intersection means that metric start is less than specified end of time window and
  # metric end is greater than start of time window.
  if (constraint == "intersects") {
    # &start_lt=2012-08-08T00:00:00&end_gt=2012-08-02T00:00:00  
    url <- paste(url,"&start_lt=",
                 format(endtime,"%Y-%m-%dT%H:%M:%OS", tz="GMT"),"&end_gt=",
                 format(starttime,"%Y-%m-%dT%H:%M:%OS", tz="GMT"),sep="") # webservice requires "T" format
  } else {
    url <- paste(url,"&timewindow=",
                 format(starttime,"%Y-%m-%dT%H:%M:%OS", tz="GMT"),",",
                 format(endtime,"%Y-%m-%dT%H:%M:%OS", tz="GMT"),sep="") # webservice requires "T" format
  }
  
  url <- paste(url,"&format=text",sep="")
  # metricName can be a comma-separated list of metric names
  url <- paste(url,"&metric=",metricName,sep="")
  
  # Add any "constraint" argument verbatim
  if (constraint != "" && constraint != "intersects") {
    url <- paste(url,"&",constraint,sep="")    
  }

  url <- paste(url,"&nodata=404",sep="")

  if (obj@debug) {
    write(paste("URL =",url), stdout())
  }
  
  return(url)  
  
}

# All arguments specified
setMethod("createBssUrl", signature(obj="IrisClient", 
                                    network="character", location="character", station="character", 
                                    channel="character",starttime="POSIXct", endtime="POSIXct", 
                                    metricName="character"), 
          function(obj, network, station, location, channel, starttime, endtime, metricName, ...) 
            createBssUrl.IrisClient(obj, network, station, location, channel, starttime, endtime, 
                                    metricName, ...))



# getSingleValueMetrics ---------------------------------------------------
#
# NOTE:  metricName can be a comma-separated list of metric names.
#
# Example of getting single valued measurements output from the ''measurements' service:
#
# Single value metrics have output that is limited to "value","target","start","end","lddate". Metrics
# that have other row names returned will not work correctly with this function.
#
# http://service.iris.edu/mustang/measurements/1/query?net=IU&sta=ANMO&loc=10&cha=BHZ&timewindow=2013-06-01T00:00:00,
#       2013-06-02T00:00:00%20&format=text&metric=sample_mean,sample_min
#
# "Sample Mean Metric"
# "value","target","start","end","lddate"
# "-7021.92","IU.ANMO.10.BHZ.B","2013/06/01 00:00:00","2013/06/02 00:00:00","1970/01/01 00:00:00"
# "-7021.92","IU.ANMO.10.BHZ.M","2013/06/01 00:00:00","2013/06/02 00:00:00","2013/09/11 04:28:08.394736"
# 
# "Sample Min Metric"
# "value","target","start","end","lddate"
# "-19622.0","IU.ANMO.10.BHZ.B","2013/06/01 00:00:00","2013/06/02 00:00:00","1970/01/01 00:00:00"
# "-19622.0","IU.ANMO.10.BHZ.M","2013/06/01 00:00:00","2013/06/02 00:00:00","2013/09/11 04:28:08.394736"

if (!isGeneric("getSingleValueMetrics")) {
  setGeneric("getSingleValueMetrics", function(obj, network, station, location, channel, starttime, endtime, metricName, constraint, url) {
    standardGeneric("getSingleValueMetrics")
  })
}

getSingleValueMetrics.IrisClient <- function(obj, network, station, location, channel,
                                             starttime, endtime, metricName, constraint, url) {
  
  # Sanity check
  if (length(metricName) > 1) {
    stop(paste('metricName should be a single string with comma separated metric names, not a vector of metric names.'))
  }
  
  # Create the BSS URL
  url <- createBssUrl(obj, network, station, location, channel, starttime, endtime, metricName, constraint, url)  
  
  # Get data from the measurements webservice
  # NOTE:  RCurl::getURLContent returns a binary objected based on the "resulting HTTP  header's Content-Type field."
  # NOTE:  Use RCurl::getURL to return data as character.
  h <-  RCurl::basicTextGatherer()
  result <- try( measurementsText <- RCurl::getURL(url, useragent=obj@useragent,
                 .opts = list(headerfunction = h$update,followlocation = TRUE, low.speed.time=300, low.speed.limit=1, connecttimeout=300)),
                 silent=TRUE)
  
  # Handle error response
  if (inherits(result,"try-error")) {
       err_msg <- gsub("Error in function \\(type, msg, asError = TRUE\\)  :","",geterrmessage())
       err_msg <- gsub("\n","",err_msg)
       stop(paste("getMustangMetrics.IrisClient:",stringr::str_trim(err_msg)))
  }

  result <- try(header <- RCurl::parseHTTPHeader(h$value()))
  if (inherits(result,"try-error")) {
       err_msg <- gsub("Error in function \\(type, msg, asError = TRUE\\)  :","",geterrmessage())
       err_msg <- gsub("\n","",err_msg)
       stop(paste("getMustangMetrics.IrisClient:",stringr::str_trim(err_msg)))
  }

  if (header["status"] != "200" && header["status"] != "204"  && header["status"] != "404" ) {
    err_msg <- measurementsText
    if (stringr::str_detect(err_msg, regex("Not Found",ignore_case=TRUE))) {
        stop(paste("getSingleValueMetrics.IrisClient: URL Not Found"))
    } else if (stringr::str_detect(err_msg, regex("couldn't connect to host",ignore_case=TRUE))) {
        stop(paste("getSingleValueMetrics.IrisClient: Couldn't connect to host"))
    } else if (stringr::str_detect(err_msg, regex("cannot open the connection",ignore_case=TRUE))) {
        stop(paste("getSingleValueMetrics.IrisClient: Cannot open connection"))
    } else if (stringr::str_detect(err_msg, regex("operation too slow",ignore_case=TRUE))) {
        stop(paste("getSingleValueMetrics.IrisClient: The query timed out"))
    } else {
        stop(paste("getSingleValueMetrics.IrisClient: Unexpected http status code",header["status"],",",header["statusMessage"]))
    }
  }

  lines <- unlist(stringr::str_split(measurementsText,"\\n"))

  # Handle error messages coming directly from the BSS
  if (any(stringr::str_detect(tolower(lines),"no targets were found"))) {
      stop(paste("getSingleValueMetrics.IrisClient:", "No targets were found"))
  } else if (any(stringr::str_detect(tolower(lines),"404 page not found"))) {
      stop(paste("getSingleValueMetrics.IrisClient: URL not found"))
  } else if (any(stringr::str_detect(tolower(lines),"404 not found"))) {
      stop(paste("getSingleValueMetrics.IrisClient: URL not found"))
  } else if (any(stringr::str_detect(tolower(lines),"No data found"))) {
      stop(paste("getSingleValueMetrics.IrisClient: No data found"))
  } else if (any(stringr::str_detect(tolower(lines),"timeoutexception"))) {
      stop(paste("getSingleValueMetrics.IrisClient:","The query timed out."))
  } else if (any(stringr::str_detect(tolower(lines),"slow"))) {
      stop(paste("getSingleValueMetrics.IrisClient:","The query timed out."))
  } else if (any(stringr::str_detect(lines,"<h2>MUSTANG Error</h2>"))) {
      next_index <- which(stringr::str_detect(lines,"<h2>MUSTANG Error</h2>"))+1
      err_msg <- gsub("</p>|<p>|&#039;","",lines[next_index])
      stop(paste("getSingleValueMetrics.IrisClient:",stringr::str_trim(err_msg)))
  } else if (any(stringr::str_detect(tolower(lines),"error report"))) {
      pattern_match <- regmatches(lines, regexec("description.*?</u>",lines))[[1]][1]
      if (!is.na(pattern_match)){
          err_msg <-  gsub("description</b> <u>","",pattern_match)
          err_msg <-  gsub("</u>","",err_msg)
          stop(paste("getSingleValueMetrics.IrisClient:",stringr::str_trim(err_msg)))
      } else {
          stop(paste("getSingleValueMetrics.IrisClient:","Unexpected error"))
      }
  } 

    
  # No errors so proceed
  
  # Dataframes will be returned in a list
  dataframeList <- list()
  
  # NOTE:  The metrics will not necessarily be returned in the order requested.
  # TODO:  Find alternate solution to hardcoded metric names.
  # Metric names returned with format=text do not match the requested metric names
  # NOTE:  Copied names from http://service.iris.edu/mustang/measurements/1
  # NOTE:  and did 1 minute of vim munging to generate this list.
  convertName <- list("Amplifier Saturation Metric"="amplifier_saturation",
                      "Calibration Signal Metric"="calibration_signal",
                      "Clock Locked Metric"="clock_locked",
                      "Data Latency Metric"="data_latency",
                      "Dc Offset Metric"="dc_offset",
                      "DC Offset Times Metric"="dc_offset_times",
                      "Digital Filter Charging Metric"="digital_filter_charging",
                      "Digitizer Clipping Metric"="digitizer_clipping",
                      "Event Begin Metric"="event_begin",
                      "Event End Metric"="event_end",
                      "Event In Progress Metric"="event_in_progress",
                      "Feed Latency Metric"="feed_latency",
                      "Glitches Metric"="glitches",
                      "Max Gap Metric"="max_gap",
                      "Max LTA/STA Metric"="max_ltasta",
                      "Max Overlap Metric"="max_overlap",
                      "Max STA/LTA Metric"="max_stalta",
                      "Missing Padded Data Metric"="missing_padded_data",
                      "Num Gaps Metric"="num_gaps",
                      "Num Overlaps Metric"="num_overlaps",
                      "Num Spikes Metric"="num_spikes",
                      "Percent Availability Metric"="percent_availability",
                      "PSD Metric"="psd",
                      "Sample Max Metric"="sample_max",
                      "Sample Mean Metric"="sample_mean",
                      "Sample Median Metric"="sample_median",
                      "Sample Min Metric"="sample_min",
                      "Sample RMS"="sample_rms",
                      "Sample SNR"="sample_snr",
                      "Spikes Metric"="spikes",
                      "Station Completeness Metric"="station_completeness",
                      "Station Up Down Times Metric"="station_up_down_times",
                      "Suspect Time Tag Metric"="suspect_time_tag",
                      "Telemetry Sync Error Metric"="telemetry_sync_error",
                      "Timing Correction Metric"="timing_correction",
                      "Timing Quality Metric"="timing_quality",
                      "Total Latency Metric"="total_latency",
                      "Up Down Times Metric"="up_down_times")
  
  # Break the text into chunks separated by "\n\n".
  # NOTE:  stringr::str_split uses extended regular expressions and '\' needs to be escaped
  chunks <- unlist(stringr::str_split(measurementsText,"\\n\\n"))
  
  # length of chunks represents the number of metrics represented
  for (i in seq(length(chunks))) {

    lines <- unlist(stringr::str_split(chunks[i],"\\n"))
    if (is.na(lines[2]) || !(stringr::str_detect(lines[2],"lddate"))) {
       stop(paste("getSingleValueMetrics.IrisClient:","Unexpected query return."))
    }

    # Create a dataframe from the text
    DF <- utils::read.csv(skip=1, header=TRUE, stringsAsFactors=FALSE, text=chunks[i])
    
    # Get metric name from first line of chunk
    lines <- unlist(stringr::str_split(chunks[i],"\\n"))
    bssName <- stringr::str_replace_all(lines[1],'"','')
    measurementName <- convertName[[bssName]]
    
    # REC
    # if there is no measurementName found in the convertName list, let's take the metric
    # name that is the header line for this chunk and convert it to the BSS metric name.
    if (length(measurementName) == 0) {
      # split text components on space
      splitNameList <- strsplit(bssName,"[[:blank:]]")
      # convert text to lower case
      toLowerNameList <- unlist(lapply(splitNameList,tolower))
      # remove the last term ('metric')
      shorterName <- toLowerNameList[-(length(toLowerNameList))]
      # join text with underscores
      measurementName <- paste(shorterName,collapse="_")
    }
    
    # Sanity check
    if ( is.null(DF) || (nrow(DF) == 0) ) {
      message(paste0('No measurements found for ',measurementName,'.'))
      next
    }
    
    # Convert from BSS DF names to 'seismic' package standard naming
    names <- names(DF)
    for (i in seq(length(names))) {
      if (names[i] == 'value') {
        names[i] <- measurementName
      } else if (names[i] == 'target') {
        names[i] <- "snclq"
      } else if (names[i] == 'start') {
        names[i] <- "starttime"
      } else if (names[i] == 'end') {
        names[i] <- "endtime"
      } else if (names[i] == 'lddate') {
        names[i] <- "loadtime"
      }
    }
    names(DF) <- names
    
    # Convert time strings
    DF$starttime <- as.POSIXct(DF$starttime, "%Y/%m/%d %H:%M:%OS", tz="GMT")
    DF$endtime <- as.POSIXct(DF$endtime, "%Y/%m/%d %H:%M:%OS", tz="GMT")    
    DF$loadtime <- as.POSIXct(DF$loadtime, "%Y/%m/%d %H:%M:%OS", tz="GMT")
    
    # NOTE:  The database was originally populated with a version of this package
    # NOTE:  that always assigned quality to be 'B'. Later versions obtained the
    # NOTE:  quality from the miniSEED packet (typically 'M').  Because of this
    # NOTE:  it is possible to have duplicate entries that only differ in the Q
    # NOTE:  part of their snclq.  To avoid double counting, we need to remove 
    # NOTE:  duplicates when the only difference is 'B'/'M'.
    
    # Reorder rows to be in temporal order by reverse loadtime -- most recent first
    DF <- DF[order(DF$loadtime,decreasing=TRUE),]
    
    # Create a uniqueId that does not use quality by first removing the Q part of N.S.L.C.Q
    sncl <- stringr::str_replace(DF$snclq,"\\.[A-Z]$","")
    uniqueId <- paste(sncl,format(DF$starttime,"%Y%m%d"),format(DF$endtime,"%Y%m%d"),sep='.')
    
    # Remove any rows which share a duplicate uniqueId (older loadtime, i.e. quality='B')
    DF <- DF[!duplicated(uniqueId),]
    
    # Reorder rows to be in temporal order by starttime
    DF <- DF[order(DF$starttime),]
    measurementName_DF <- paste(measurementName,"DF",sep="_")
    dataframeList[[measurementName_DF]] <- DF
  }
  
  if(length(dataframeList)==0){
     return(NULL)
  }

  # Convert dataframeList into a 'tidy' dataframe appropriate for use with ggplot2
  
  # This function will be applied to each dataframe in DFList
  # Adds a "metricName" column and "value" column
  addMetricName <- function(df) {
    metric <- names(df)[[1]]
    df$metricName <- metric
    n <- names(df)
    n[[1]] <- "value"
    names(df) <- n
    return(df)
  }
  
  # Apply function to our list and bind the rows together to make one big dataframe
  # NOTE:  http://www.r-bloggers.com/the-rbinding-race-for-vs-do-call-vs-rbind-fill/
  TidyDF <- do.call("rbind", lapply(dataframeList, addMetricName))
  
  # Move last column to first 
  TidyDF <- TidyDF[,c(6,1,2,3,4,5)]
  
  # Remove ugly rownames
  row.names(TidyDF) <- NULL
  
  return(TidyDF)
  
}

# All arguments specified
setMethod("getSingleValueMetrics", signature(obj="IrisClient", 
                                             network="character", location="character", station="character", 
                                             channel="character",starttime="POSIXct", endtime="POSIXct", 
                                             metricName="character", constraint="character", url="character"), 
          function(obj, network, station, location, channel, starttime, endtime, metricName, constraint, url) 
            getSingleValueMetrics.IrisClient(obj, network, station, location, channel, starttime, endtime, 
                                             metricName, constraint, url))
# url="missing"
setMethod("getSingleValueMetrics", signature(obj="IrisClient", 
                                             network="character", location="character", station="character", 
                                             channel="character",starttime="POSIXct", endtime="POSIXct", 
                                             metricName="character", constraint="character", url="missing"), 
          function(obj, network, station, location, channel, starttime, endtime, metricName, constraint, url) 
            getSingleValueMetrics.IrisClient(obj, network, station, location, channel,
                                             starttime, endtime, metricName, constraint,
                                             "http://service.iris.edu/mustang/measurements/1/query?"))

# constraint="missing"
setMethod("getSingleValueMetrics", signature(obj="IrisClient", 
                                             network="character", location="character", station="character", 
                                             channel="character",starttime="POSIXct", endtime="POSIXct", 
                                             metricName="character", constraint="missing", url="character"), 
          function(obj, network, station, location, channel, starttime, endtime, metricName, constraint, url) 
            getSingleValueMetrics.IrisClient(obj, network, station, location, channel,
                                             starttime, endtime, metricName, "", url))


# constraint="missing", url="missing"
setMethod("getSingleValueMetrics", signature(obj="IrisClient", 
                                             network="character", location="character", station="character", 
                                             channel="character",starttime="POSIXct", endtime="POSIXct", 
                                             metricName="character", constraint="missing", url="missing"), 
          function(obj, network, station, location, channel, starttime, endtime, metricName, constraint, url) 
            getSingleValueMetrics.IrisClient(obj, network, station, location, channel,
                                             starttime, endtime, metricName, "",
                                             "http://service.iris.edu/mustang/measurements/1/query?"))

# getGeneralValueMetrics ---------------------------------------------------
#
# NOTE:  metricName can be a comma-separated list of metric names.
#
# Example of getting general valued measurements output from the ''measurements' service:
#
# http://service.iris.edu/mustang/measurements/1/query?net=IU&sta=ANMO&location=00&
#       channel=BH[12Z]&timewindow=2016-08-29T00:00:00,2016-08-30T00:00:00&
#       format=text&metric=sample_mean,orientation_check
#
#"Sample Mean Metric"
#"value","target","start","end","lddate"
#"-515529","IU.ANMO.00.BHZ.M","2016/08/29 00:00:00","2016/08/30 00:00:00","2016/08/31 18:52:54.283171"
#"-737990","IU.ANMO.00.BH1.M","2016/08/29 00:00:00","2016/08/30 00:00:00","2016/08/31 18:51:19.142419"
#"-557756","IU.ANMO.00.BH2.M","2016/08/29 00:00:00","2016/08/30 00:00:00","2016/08/31 18:52:07.134140"

#"Orientation Check Metric"
#"azimuth_R","backAzimuth","azimuth_Y_obs","azimuth_X_obs","azimuth_Y_meta","azimuth_X_meta","max_Czr","max_C_zr","magnitude","target","start","end","lddate"
#"73.0000","90.6756","17.6756","107.676","18.0000","108.000","0.979605","0.747436","7.10","IU.ANMO.00.BHZ.M","2016/08/29 05:10:48","2016/08/29 05:21:08","2016/08/31 23:07:13.379066"

if (!isGeneric("getGeneralValueMetrics")) {
  setGeneric("getGeneralValueMetrics", function(obj, network, station, location, channel, starttime, endtime, metricName, ...) {
    standardGeneric("getGeneralValueMetrics")
  })
}

getGeneralValueMetrics.IrisClient <- function(obj, network, station, location, channel,
                                             starttime, endtime, metricName, constraint="", url=NULL) {

  # Sanity check
  if (length(metricName) > 1) {
    stop(paste('metricName should be a single string with comma separated metric names, not a vector of metric names.'))
  }

  # NOTE:  The metrics will not necessarily be returned in the order requested.
  # TODO:  Find alternate solution to hardcoded metric names.
  # Metric names returned with format=text do not match the requested metric names
  convertName <- list("Amplifier Saturation Metric"="amplifier_saturation",
                      "Calibration Signal Metric"="calibration_signal",
                      "Clock Locked Metric"="clock_locked",
                      "Cross Talk Metric"="cross_talk",
                      "Data Latency Metric"="data_latency",
                      "Dc Offset Metric"="dc_offset",
                      "Digital Filter Charging Metric"="digital_filter_charging",
                      "Digitizer Clipping Metric"="digitizer_clipping",
                      "Event Begin Metric"="event_begin",
                      "Event End Metric"="event_end",
                      "Event In Progress Metric"="event_in_progress",
                      "Feed Latency Metric"="feed_latency",
                      "Glitches Metric"="glitches",
                      "Max Gap Metric"="max_gap",
                      "Max LTA/STA Metric"="max_ltasta",
                      "Max Overlap Metric"="max_overlap",
                      "Max STA/LTA Metric"="max_stalta",
                      "Metric Error Metric"="metric_error",
                      "Missing Padded Data Metric"="missing_padded_data",
                      "Num Gaps Metric"="num_gaps",
                      "Num Overlaps Metric"="num_overlaps",
                      "Num Spikes Metric"="num_spikes",
                      "Percent Availability Metric"="percent_availability",
                      "PSD Metric"="psd",
                      "Sample Max Metric"="sample_max",
                      "Sample Mean Metric"="sample_mean",
                      "Sample Median Metric"="sample_median",
                      "Sample Min Metric"="sample_min",
                      "Sample Rate Channel"="sample_rate_channel",
                      "Sample Rate Resp"="sample_rate_resp",
                      "Sample RMS"="sample_rms",
                      "Sample SNR"="sample_snr",
                      "Spikes Metric"="spikes",
                      "Station Completeness Metric"="station_completeness",
                      "Station Up Down Times Metric"="station_up_down_times",
                      "Suspect Time Tag Metric"="suspect_time_tag",
                      "Telemetry Sync Error Metric"="telemetry_sync_error",
                      "Timing Quality Metric"="timing_quality",
                      "Total Latency Metric"="total_latency",
                      "Ts Channel Continuity"="ts_channel_continuity",
                      "Ts Channel Gap List"="ts_channel_gap_list",
                      "Ts Channel Up Time Metric"="ts_channel_up_time",
                      "Ts Gap Length Metric"="ts_gap_length",
                      "Ts Gap Length Total Metric"="ts_gap_length_total",
                      "Ts Max Gap Metric"="ts_max_gap",
                      "Ts Max Gap Total Metric"="ts_max_gap_total",
                      "Ts Num Gaps Metric"="ts_num_gaps",
                      "Ts Num Gaps Total Metric"="ts_num_gaps_total",
                      "Ts Percent Availability Metric"="ts_percent_availability",
                      "Ts Percent Availability Total Metric"="ts_percent_availability_total")


  # Create the BSS URL
  url2 <- createBssUrl(obj, network, station, location, channel, starttime, endtime, metricName, constraint, url) 

  if (is.null(url)) {
    url <- paste0(strsplit(url2,"\\?")[[1]][1],"?")
  }

  # Get data from the measurements webservice
  # NOTE:  Use RCurl::getURL to return data as character.
  h <-  RCurl::basicTextGatherer()
  result <- try( measurementsText <- RCurl::getURL(url2, useragent=obj@useragent,
                 .opts = list(headerfunction = h$update,followlocation = TRUE, low.speed.time=300, low.speed.limit=1, connecttimeout=300)),
                 silent=TRUE)

  if (inherits(result,"try-error")) {
       err_msg <- gsub("Error in function \\(type, msg, asError = TRUE\\)  :","",geterrmessage())
       err_msg <- gsub("\n","",err_msg)
       stop(paste("getMustangMetrics.IrisClient:",stringr::str_trim(err_msg)))
  }

  result <- try(header <- RCurl::parseHTTPHeader(h$value()))
  if (inherits(result,"try-error")) {
       err_msg <- gsub("Error in function \\(type, msg, asError = TRUE\\)  :","",geterrmessage())
       err_msg <- gsub("\n","",err_msg)
       stop(paste("getMustangMetrics.IrisClient:",stringr::str_trim(err_msg)))
  }

  if (header["status"] != "200" && header["status"] != "204" && header["status"] != "404" ) {
    err_msg <- measurementsText
    if (stringr::str_detect(err_msg, regex("Not Found",ignore_case=TRUE))) {
        stop(paste("getMustangMetrics.IrisClient: URL Not Found"))
    } else if (stringr::str_detect(err_msg, regex("couldn't connect to host",ignore_case=TRUE))) {
        stop(paste("getMustangMetrics.IrisClient: Couldn't connect to host"))
    } else if (stringr::str_detect(err_msg, regex("cannot open the connection",ignore_case=TRUE))) {
        stop(paste("getMustangMetrics.IrisClient: Cannot open connection"))
    } else if (stringr::str_detect(err_msg, regex("operation too slow",ignore_case=TRUE))) {
        stop(paste("getMustangMetrics.IrisClient: The query timed out"))
    } else {
        stop(paste("getMustangMetrics.IrisClient: Unexpected http status code",header["status"],",",header["statusMessage"]))
    }
  }

  lines <- unlist(stringr::str_split(measurementsText,"\\n"))

    # Handle error messages coming directly from the BSS
    if (any(stringr::str_detect(tolower(lines),"no targets were found"))) {
        stop(paste("getMustangMetrics.IrisClient:", "No targets were found"))
    } else if (any(stringr::str_detect(tolower(lines),"404 page not found"))) {
        stop(paste("getMustangMetrics.IrisClient: URL not found"))
    } else if (any(stringr::str_detect(tolower(lines),"404 not found"))) {
        stop(paste("getMustangMetrics.IrisClient: URL not found"))
    } else if (any(stringr::str_detect(tolower(lines),"No data found"))) {
        stop(paste("getMustangMetrics.IrisClient: No data found"))
    } else if (any(stringr::str_detect(tolower(lines),"timeoutexception"))) {
        stop(paste("getMustangMetrics.IrisClient:","The query timed out."))
    } else if (any(stringr::str_detect(tolower(lines),"slow"))) {
        stop(paste("getMustangMetrics.IrisClient:","The query timed out."))
    } else if (any(stringr::str_detect(lines,"<h2>MUSTANG Error</h2>"))) {
        next_index <- which(stringr::str_detect(lines,"<h2>MUSTANG Error</h2>"))+1
        err_msg <- gsub("</p>|<p>|&#039;","",lines[next_index])
        stop(paste("getMustangMetrics.IrisClient:",stringr::str_trim(err_msg)))
    } else if (any(stringr::str_detect(tolower(lines),"error report"))) {
        pattern_match <- regmatches(lines, regexec("description.*?</u>",lines))[[1]][1]
        if (!is.na(pattern_match)){
            err_msg <-  gsub("description</b> <u>","",pattern_match)
            err_msg <-  gsub("</u>","",err_msg)
            stop(paste("getMustangMetrics.IrisClient:",stringr::str_trim(err_msg)))
        } else {
            stop(paste("getMustangMetrics.IrisClient:","Unexpected error"))
        }
    } 

  # Dataframes will be returned in a list
  dataframeList <- list()


  # Break the text into chunks separated by "\n\n".
  # NOTE:  stringr::str_split uses extended regular expressions and '\' needs to be escaped

  chunks <- unlist(stringr::str_split(measurementsText,"\\n\\n"))

  # length of chunks represents the number of metrics represented
  for (i in seq(length(chunks))) {

    lines <- unlist(stringr::str_split(chunks[i],"\\n"))
    if (is.na(lines[2]) || !(stringr::str_detect(lines[2],"lddate"))) {
       stop(paste("getMustangMetrics.IrisClient:","Unexpected query return."))
    }   

    # Create a dataframe from the text
    result <- try( DF <- utils::read.csv(skip=1, header=TRUE, stringsAsFactors=FALSE, text=chunks[i]),silent=TRUE)
 
    if (inherits(result,"try-error")) {
        err_msg <- geterrmessage()
        stop(paste("getMustangMetrics.IrisClient: read.csv", err_msg))
    }


    # Get metric name from first line of chunk
    lines <- unlist(stringr::str_split(chunks[i],"\\n"))
    bssName <- stringr::str_replace_all(lines[1],'"','')
    measurementName <- convertName[[bssName]]

    # REC
    # if there is no measurementName found in the convertName list, let's take the metric
    # name that is the header line for this chunk and convert it to the BSS metric name.
    if (length(measurementName) == 0) {
      # split text components on space
      splitNameList <- strsplit(bssName,"[[:blank:]]")
      # convert text to lower case
      toLowerNameList <- unlist(lapply(splitNameList,tolower))
      # remove the last term ('metric')
      shorterName <- toLowerNameList[-(length(toLowerNameList))]
      # join text with underscores
      measurementName <- paste(shorterName,collapse="_")
    }

    # Convert from BSS DF names to 'seismic' package standard naming
    names <- names(DF)
    for (i in seq(length(names))) {
      if (names[i] == 'target') {
        names[i] <- "snclq"
      } else if (names[i] == 'start') {
        names[i] <- "starttime"
      } else if (names[i] == 'end') {
        names[i] <- "endtime"
      } else if (names[i] == 'lddate') {
        names[i] <- "loadtime"
      }
    }
    names(DF) <- names

    if ( is.null(DF) || (nrow(DF) == 0) ) {
      message(paste0('No measurements found for ',measurementName,'.'))
      next
    } 

    # Convert time strings
    result <- try( DF$starttime <- as.POSIXct(DF$starttime, "%Y/%m/%d %H:%M:%OS", tz="GMT"), silent=TRUE)
    if (inherits(result,"try-error")) {
        stop(paste("getMustangMetrics.IrisClient: convert starttime",DF$starttime[1], "to as.POSIXct failed"))
    }

    result <- try(DF$endtime <- as.POSIXct(DF$endtime, "%Y/%m/%d %H:%M:%OS", tz="GMT"), silent=TRUE)
    if (inherits(result,"try-error")) {
        stop(paste("getMustangMetrics.IrisClient: convert endtime",DF$endtime[1], "to as.POSIXct failed"))
    }

    result <- try(DF$loadtime <- as.POSIXct(DF$loadtime, "%Y/%m/%d %H:%M:%OS", tz="GMT"), silent=TRUE)
    if (inherits(result,"try-error")) {
        stop(paste("getMustangMetrics.IrisClient: convert loadtime", DF$loadtime[1], "to as.POSIXct failed"))
    }

    # NOTE:  The database was originally populated with a version of this package
    # NOTE:  that always assigned quality to be 'B'. Later versions obtained the
    # NOTE:  quality from the miniSEED packet (typically 'M').  Because of this
    # NOTE:  it is possible to have duplicate entries that only differ in the Q
    # NOTE:  part of their snclq.  To avoid double counting, we need to remove
    # NOTE:  duplicates when the only difference is 'B'/'M'.

    # Reorder rows to be in temporal order by reverse loadtime -- most recent first
    DF <- DF[order(DF$loadtime,decreasing=TRUE),]

    # Create a uniqueId that does not use quality by first removing the Q part of N.S.L.C.Q
    sncl <- stringr::str_replace(DF$snclq,"\\.[A-Z]$","")
    uniqueId <- paste(sncl,format(DF$starttime,"%Y%m%d%H%M"),format(DF$endtime,"%Y%m%d%H%M"),sep='.')

    # Remove any rows which share a duplicate uniqueId (older loadtime, i.e. quality='B')
    DF <- DF[!duplicated(uniqueId),]

    # add row with measurementName
    metricName <- rep(measurementName, nrow(DF))
    DF <- cbind(metricName,DF, stringsAsFactors=FALSE)

    # Reorder rows to be in temporal order by starttime
    DF <- DF[order(DF$starttime),]
    measurementName_DF <- paste(measurementName,"DF",sep="_")
    dataframeList[[measurementName_DF]] <- DF

  }

  if(length(dataframeList)==0){  # return NULL if no results returned for any metric
     return(NULL)
  } else {
     # Combine dataframeList into one dataframe
     BigDF <- suppressMessages(Reduce(dplyr::full_join,dataframeList))
     BigDF <- BigDF[order(BigDF$metricName,BigDF$starttime),]
     if ("value" %in% names(BigDF)) {
           BigDF <- BigDF %>% dplyr::select(metricName, snclq, value, starttime, endtime, everything())
     } else {
           BigDF <- BigDF %>% dplyr::select(metricName, snclq, starttime, endtime, everything())
     }
     return(BigDF)
  }
}

# All arguments specified
setMethod("getGeneralValueMetrics", signature(obj="IrisClient",
                                             network="character", location="character", station="character",
                                             channel="character",starttime="POSIXct", endtime="POSIXct",
                                             metricName="character"),
          function(obj, network, station, location, channel, starttime, endtime, metricName, ...)
            getGeneralValueMetrics.IrisClient(obj, network, station, location, channel, starttime, endtime,
                                             metricName, ...))


getMustangMetrics <- getGeneralValueMetrics

# getPsdMetrics -----------------------------------------------------------
#
# Example of getting PSD output from the 'measurements' service
#
# http://service.iris.edu/mustang/noise-psd/1/query?net=IU&sta=ANMO&cha=BHZ&loc=00&quality=M&starttime=2013-05-01T00:00:00&endtime=2013-05-01T01:00:00&format=xml
#
#<PsdRoot>
#<Created>2016-09-23T20:50:36.661Z</Created>
#<RequestedDateRange>
#<Start>2013-05-01T00:00:00.000Z</Start>
#<End>2013-05-01T01:00:00.000Z</End>
#</RequestedDateRange>
#<AnalyzedDateRange>
#<Start>2013-05-01T00:00:00.000Z</Start>
#<End>2013-05-01T02:00:00.000Z</End>
#</AnalyzedDateRange>
#<Psds>
#<Psd target="IU.ANMO.00.BHZ.M" start="2013-05-01T00:00:00.000Z" end="2013-05-01T01:00:00.000Z">
#<value freq="0.0052556" power="-174.1390637788908"/>
#<value freq="0.00573128" power="-174.01815949029867"/>
#<value freq="0.00625" power="-174.86542709483024"/>
#<value freq="0.00681567" power="-174.69764082822218"/>
# ... ... through all frequencies
# ... repeated for each hour long chunk

if (!isGeneric("getPsdMetrics")) {
  setGeneric("getPsdMetrics", function(obj, network, station, location, channel, starttime, endtime, url) {
    standardGeneric("getPsdMetrics")
  })
}

getPsdMetrics.IrisClient <- function(obj, network, station, location, channel, starttime, endtime, url) {
  
  if (location == "") {
    location <- "--"
  }

  # Create the BSS URL
  url <- paste0(url,"net=",network,"&sta=",station,"&loc=",location,"&cha=",channel,"&starttime=",starttime,"&endtime=",endtime,"&quality=M&format=xml")
  
  # Read the data directly from the URL 
  result <- try( psdXml <- RCurl::getURL(url, useragent=obj@useragent), silent=TRUE)
  
  # Handle error response
  if (inherits(result,"try-error")) {
    err_msg <- geterrmessage()
    if (stringr::str_detect(err_msg,regex("Not Found",ignore_case=TRUE))) {
      stop(paste("getPsdMetrics.IrisClient: URL Not Found:",url))
    } else if (stringr::str_detect(err_msg,regex("couldn't connect to host",ignore_case=TRUE))) {
      stop(paste("getPsdMetrics.IrisClient: Couldn't connect to host"),url)
    } else {
      stop(paste("getPsdMetrics.IrisClient:",err_msg, url))
    } 
    
  }

  if (stringr::str_detect(psdXml,regex("Not Found",ignore_case=TRUE))) {
      stop(paste("getPsdMetrics.IrisClient: URL Not Found:",url))
  } else if (stringr::str_detect(psdXml,regex("couldn't connect to host",ignore_case=TRUE))) {
      stop(paste("getPsdMetrics.IrisClient: Couldn't connect to host"),url)
  } 
  
  # No errors so proceed
  docP <- XML::xmlTreeParse(psdXml, useInternalNodes=TRUE)
  nodesP <- XML::getNodeSet(docP,"//Psd")
  
  DFList <- lapply(nodesP, function(x) {
                         freq <- sapply(XML::getNodeSet(x,"value"), function(z) as.numeric(XML::xmlGetAttr(z,"freq")))
                         power <- sapply(XML::getNodeSet(x,"value"), function(z) as.numeric(XML::xmlGetAttr(z,"power")))
                         target <- rep(XML::xmlGetAttr(x,"target"),length(freq))
                         start <- rep(as.POSIXct(XML::xmlGetAttr(x,"start"),format="%Y-%m-%dT%H:%M:%OSZ"),length(freq))
                         end <- rep(as.POSIXct(XML::xmlGetAttr(x,"end"),format="%Y-%m-%dT%H:%M:%OSZ"),length(freq))
                         dplyr::data_frame(target, start, end, freq, power)
                      })
  DF <- do.call(rbind,DFList)
  return(DF)
}

# All arguments specified
setMethod("getPsdMetrics", signature(obj="IrisClient", 
                                     network="character", location="character", station="character", 
                                     channel="character", starttime="POSIXct", endtime="POSIXct", 
                                     url="character"), 
          function(obj, network, station, location, channel, starttime, endtime, url) 
            getPsdMetrics.IrisClient(obj, network, station, location, channel, starttime, endtime, url))
# url="missing"
setMethod("getPsdMetrics", signature(obj="IrisClient", 
                                     network="character", location="character", station="character", 
                                     channel="character", starttime="POSIXct", endtime="POSIXct", 
                                     url="missing"), 
          function(obj, network, station, location, channel, starttime, endtime, url) 
            getPsdMetrics.IrisClient(obj, network, station, location, channel, starttime, endtime,
                                     url="http://service.iris.edu/mustang/noise-psd/1/query?"))


