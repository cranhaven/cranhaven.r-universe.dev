#' @export
datamerge.JGR <- function( x, y, by11="None", by12="None", by13="None",
  by21="None", by22="None", by23="None", all.x=TRUE, all.y=TRUE,
  suffixes=c(".1",".2"),rName="mergedData",
  sort=FALSE,writeout=FALSE,outName="mergedData.txt", browserResults=FALSE)
{
  ## Get place to store results
  if (browserResults) resultLocation = genResultSpace()

  by1 = c(by11,by12,by13)
  nby1 = by1 %in% c("","None")
  by2 = c(by21,by22,by23)
  nby2 = by2 %in% c("","None")
  if( any( xor(nby1,nby2) ) )
    { 
      if (browserResults){
          localJGRError( paste( "Uneven variable selection:",
                    "\nFor each variable name chosen in Dataset 1, there must be",
                    "\na corresponding variable chosen for Dataset 2." ),
                    resultLocation, geterr=FALSE )
    }  
  }
  if( all( nby1 ) ){
    bys = FALSE
  } else {
    bys = TRUE
    by.x = by1[!nby1]
    by.y = by2[!nby2]
  }
  if( suffixes[1]=="" ){ suffixes[1]=".1" }
  if( suffixes[2]=="" ){ suffixes[2]=".2" }

  if( bys ){
    mergedData = try( merge( x, y, by.x=by.x, by.y=by.y, all.x=all.x, all.y=all.y,
      sort=sort, suffixes=suffixes ) )
  } else {
    mergedData = try( merge( x, y, all.x=all.x, all.y=all.y,
      sort=sort, suffixes=suffixes ) )
  }
  if( inherits( mergedData, "try-error" ) ){
    if (browserResults){
        localJGRError( "Failure merging data sets.", resultLocation )
      }
    }
  
  if( rName=="" ){ rName="mergedData" }
  pos <- 1
  envir = as.environment(pos)
  assign( rName, mergedData, envir = envir)

  if( writeout ){
    tmp = try( write.table( mergedData, outName, sep="\t", row.names=FALSE ) )
    if( inherits( tmp, "try-error" ) ){

      if (browserResults){
          localJGRError( "Error writing out merged data.", resultLocation )
        }
    } else {
        cat(paste("Merged data has been successfully written to ",outName, sep="","."))
    }
  }
  

  return(invisible())
}

