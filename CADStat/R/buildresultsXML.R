## An error reporting function -- convert error message to xml output
localJGRError = function( errMes, location, geterr=TRUE )
{
  if( geterr ){
    Error = data.frame( Error=c( errMes, geterrmessage() ) )
  } else {
    Error = data.frame( Error=errMes )
  }
  buildresultsXML( object=list(Error), location=location )
  stop( call.=FALSE )
}

buildresultsXML <- function(object=NULL,title=" ",text=NULL,location=stop('Save location required'),...)
{
  # 'require' outdated, new standard uses requireNamespace()
  #require(XML)
  # Better practice remove XML from import and add sparingly XML:: within this function
  # Need to come back and refine <--------------------------- ultimately remove XML from full import
  #if (requireNamespace("XML", quietly = TRUE)) {
    #XML::
    #require(RSvgDevice)
    result.nodes <- xmlNode("article")
    result.nodes <- append.xmlNode(result.nodes,xmlNode("title",title))
    if(!is.null(text))
      result.nodes <- append.xmlNode(result.nodes,xmlNode("para",text))
    if(is.list(object)){
      for(i in 1:length(object)){
        if(!is.null(object[[i]])){
          result.nodes <- append.xmlNode(result.nodes,docbook(object[[i]],...))
        }
      }
    }
    svg.file <- list.files(location,pattern=".svg")
    if(length(svg.file)>0){
      for(i in 1:length(svg.file)){
        svg.nodes <- xmlRoot(xmlTreeParse(file=svg.file[i]))
        result.nodes <- append.xmlNode(result.nodes,svg.nodes)
      }
    }
    png.file <- list.files(location,pattern=".png")
    if(length(png.file)>0){
      for(i in 1:length(png.file)){
        png.nodes <- xmlNode("code",attrs=c(language="text/html"),xmlNode("img",attrs=c(src=png.file[i])))
        if(is.na(charmatch("CADStat",png.file[i])))
          png.nodes <- append.xmlNode(png.nodes,xmlNode("h2",paste("Figure ",i,".  ",gsub(".png","",png.file[i]),".",sep="")))
        result.nodes <- append.xmlNode(result.nodes,png.nodes)
      }
    }
    saveXML(result.nodes,
            file=file.path(location,"results.xml"),
            prefix = '<?xml version="1.0"?>\n<?xml-stylesheet type="text/xsl" href="../../../doc/stylesheets/caddis.xsl"?>\n')
  #}

}


## Generate a result folder with random name
genResultSpace = function(){
  ## make sure results folder exists
#  resultSpace = file.path(.libPaths()[1],"CADStat","workspace","results")
  
  # To conform to CRAN guidelines, this function
  # generates a space for storing results in an R session's
  # temporary directory.
  
  CADSTAT_TEMP_DIR = file.path(tempdir(), "CADStat")
  
  if ( !dir.exists( CADSTAT_TEMP_DIR ) ){
    dir.create( CADSTAT_TEMP_DIR )
  }
  
  resultSpace = file.path(CADSTAT_TEMP_DIR,"results")
    if( !dir.exists( resultSpace )){
    dir.create( resultSpace )
  }
  ## test to make sure folder doesn't currently exist
  while( dir.exists( (resFolder=file.path(resultSpace,
                         paste("result",ceiling(runif(1)*100000000),sep=""))) ) ){}
  dir.create(resFolder)
  return( resFolder )
}

## Delete all results folders
#' @export
cleanCADStatWorkspace = function(){
  CADSTAT_TEMP_DIR = file.path(tempdir(), "CADStat")
  resultSpace = file.path(CADSTAT_TEMP_DIR,"results")  
  unlink(file.path(resultSpace,"result*"),recursive=TRUE)
}
