#' @include internalHelpers.R
NULL

#' read or save a \code{\link{SpectraInTime-class}} from or to a  \code{.txt} file 
#' 
#' @param object  object to save
#' @param directory directory to save object
#' @param precision number of significant digits controlling precission
#' @return the path to which the file is saved 
#' @note experiment name is used to save the experiment
#' @note default time formats are assumed to convert to \code{\link{SpectraInTime-class}}
#' @note some data precession is lost because of internal conversion to JSON format 
#' @name saveSpectra
#' @aliases readSpectra save read
#' @importFrom jsonlite  toJSON read_json
#' @examples 
#'   \dontshow{ directory       <-  tempdir() } 
#'  spectra         <-  getSpectraInTimeExample()
#'  saveSpectra(  spectra , directory )
#'  experimentName  <-  getExperimentName( spectra )
#'  file            <-  file.path( directory , paste0( experimentName , ".txt")  )
#'  spectraRead     <-  readSpectra( file )
#'  \dontshow{ unlink( directory ) }
#' @author Adriaan Blommaert
#' @export
saveSpectra                           <-  function( object , directory , precision = 32 ) {
  objectAsList                      <-  convertS4ToList( object )
  experimentName                    <-  getExperimentName( object )
  fileName                          <-  paste0( experimentName , ".txt" )
  filePath                          <-  file.path( directory , fileName )
  testJSON                          <-  toJSON( objectAsList , force=TRUE, auto_unbox=TRUE, pretty=FALSE , digits = I(precision) )
  cat( testJSON  , file = filePath )   
  return( filePath )
}



#' @rdname saveSpectra
#' @return \code{\link{SpectraInTime-class}}
#' @param file to be read 
#' @export
readSpectra                            <-  function( file ){
  objectAsList                         <-  read_json(path = file , simplifyVector = TRUE )
#  str( objectAsList )
  objectAsListFormat                   <-  objectAsList
  startTime                            <-  objectAsList$startTime
  startTimeFormat                      <-  as.POSIXct( startTime ) 
  objectAsListFormat[[ "startTime" ]]  <-  startTimeFormat
  names(objectAsListFormat)[3] 		   <- "spectralAxis"
  objectAsSpectraInTime                <-  convertListToS4( objectAsListFormat , "SpectraInTime" );
  return( objectAsSpectraInTime )
} 

#### try as XML
#
#library( xml2 )
#
##' save a \code{\link{SpectraInTime-class}}  as a \code{.txt} file 
##' 
##' @param object  object to save
##' @param directory directory to save object
##' @return the path to which the file is saved 
##' @note experiment name is used to save the experiment
##' @note default time formats are assumed to convert to \code{\link{SpectraInTime-class}}
##' @note some data precession is lost because of internal conversion to JSON format 
##' @name saveSpectra
##' @aliases readSpectra save read
##' @importFrom jsonlite  toJSON read_json
##' @examples 
#saveSpectraXML                           <-  function( object , directory ) {
#  objectAsList                      <-  convertS4ToList( object )
#  experimentName                    <-  getExperimentName( object )
#  fileName                          <-  paste0( experimentName , ".txt" )
#  filePath                          <-  file.path( directory , fileName )
#  xmlDoc                            <-  as_xml_document( list( SpectraInTime  = objectAsList ) )
##  testJSON                          <-  toJSON( objectAsList , force=TRUE, auto_unbox=TRUE, pretty=FALSE )
##  cat( testJSON  , file = filePath ) 
#
#  
#  return( filePath )
#}
#
#
#
##' @rdname saveSpectra
##' @param file to be read 
#readSpectraXML                         <-  function( file ){
#  objectAsList                         <-  read_json(path = file , simplifyVector = TRUE )
#  str( objectAsList )
#  objectAsListFormat                   <-  objectAsList
#  startTime                            <-  objectAsList$startTime
#  startTimeFormat                      <-  as.POSIXct( startTime ) 
#  objectAsListFormat[[ "startTime" ]]  <-  startTimeFormat
#  objectAsSpectraInTime                <-  convertListToS4( objectAsListFormat , "SpectraInTime" );
#  return( objectAsSpectraInTime )
#} 
#
#
#
#### notes
#
## as.xml2 stops R on small example
## test own example  ( and build up )
#
#
#object     <-  getSpectraInTimeExample()[1:2 , 1:3]
#str( object )
#
#as_xml_document( list(x = list( )) )
#
#as_xml_document(list(foo = list(
#            bar = structure(list(), id = "a"),
#            bar = structure(list(), id = "b"))))
#
#
#as_xml_document( list( foo = list(
#            bar = structure(list(), id = "a"),
#            bar = structure(list(), id = "b")
#     )
#  )
#)
#
#as_xml_document( list( foo = list(
#            bar = structure(list(), id = "a"),
#            bar = structure(list(), id = "b")
#        )
#    )
#)
#
### try convert matrix as xml
#
#as_xml_document( list( foo = list(
#            bar = structure(vector, id = c(1,2,3)),
#            bar = structure(list(), id = "b")
#        )
#    )
#)
#
#
#as_xml_document( list( foo = list(
#            bar = structure(vector, id = "a"),
#            bar = structure(list(), id = "b")
#        )
#    )
#)
#
#
#### try it with XML package
#
#
#library(XML)    
#top = newXMLNode("a")
#newXMLNode("b", attrs=c(x=1, y='abc'), parent=top)
#newXMLNode("c", "With some text", parent=top)
#
#
### with numberic data 
#vec  = c(1,2,3 , 2^(1/2) ) 
#mat  = diag(5)
#
#
#
#library( spectralAnalysis )
#object                      <-  getSpectraInTimeExample()
#slots                       <-  slotNames( object )
#experimentName              <-  getExperimentName( object )
#top                         <-  newXMLNode("newXMLNode")
#newXMLNode( "timePoints" , slot( object ,"timePoints") , parent=top )
#newXMLNode("c", "With some text", parent=top)
#
#
#
#x = rnorm(3)
#z = xmlTree("r:data", namespaces = c(r = "http://www.r-project.org"))
#z$addNode("numeric", attrs = c("r:length" = length(x)), close = FALSE)
#lapply(x, function(v) z$addNode("el", x))
#z$closeNode()
## should give   <r:data><numeric r:length="3"/></r:data>
#
#
### try it on numeric data 
#
#
#
#    
#    ##' Convert List to XML
#    ##'
#    ##' Can convert list or other object to an xml object using xmlNode
#    ##' @title List to XML
#    ##' @param item 
#    ##' @param tag xml tag
#    ##' @return xmlNode
#    ##' @export
#    ##' @author David LeBauer, Carl Davidson, Rob Kooper
#    listToXml <- function(item, tag) {
#  # just a textnode, or empty node with attributes
#  if(typeof(item) != 'list') {
#    if (length(item) > 1) {
#      xml <- xmlNode(tag)
#      for (name in names(item)) {
#        xmlAttrs(xml)[[name]] <- item[[name]]
#      }
#      return(xml)
#    } else {
#      return(xmlNode(tag, item))
#    }
#  }
#  
#  # create the node
#  if (identical(names(item), c("text", ".attrs"))) {
#    # special case a node with text and attributes
#    xml <- xmlNode(tag, item[['text']])
#  } else {
#    # node with child nodes
#    xml <- xmlNode(tag)
#    for(i in 1:length(item)) {
#      if (names(item)[i] != ".attrs") {
#        xml <- append.xmlNode(xml, listToXml(item[[i]], names(item)[i]))
#      }
#    }    
#  }
#}
#
#listToXml( list( a=1 , b = 2 ) , tag = top)
#
### xml data in R is not wel supported ( reading is well supported )
#library( jsonlite )
#str <- base64_enc(serialize(iris, NULL))
#out <- unserialize(base64_dec(str))
#
#identical( iris , out )
#stopifnot(identical(out, iris))
#
