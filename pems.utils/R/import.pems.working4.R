##########################
##########################
#various pems imports
##########################
##########################


#additional (1)

#work in progress


#############################
#############################
##importKML2PEMS
#############################
#############################

#quick importer for KML data
#started 07-03-2016
#kr v0.0.1 (07-03-2016)


#?? functions (see note)
###################
#importKML2PEMS
#

#currently very crude because kml may be very variable


###################
#to do
###################
#tidy
#add distance and speed
#see addSpeed2KML in working in pems.tsc


###################
#notes
###################
#

###################
#importKML2PEMS
###################


importKML2PEMS <- function (file.name = file.choose(), history = NULL, 
    constants = NULL, source = "Unknown", ...) 
{

    #importer for KML for an outknown source
    #version one and we know there are lots of versions
    #so test = TRUE, and reject if does not recognise it

#example > head(ans, 20)
# [1] "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"                                                                                                                                           
# [2] "<kml xmlns=\"http://www.opengis.net/kml/2.2\" xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns:kml=\"http://www.opengis.net/kml/2.2\" xmlns:atom=\"http://www.w3.org/2005/Atom\">"
# [3] "  <Document>"                                                                                                                                                                         
# [4] "    <description>Van run for battery</description>"                                                                                                                                   
# [5] "    <TimeSpan>"                                                                                                                                                                       
# [6] "      <begin>2016-02-05T11:58:14.000Z</begin>"                                                                                                                                        
# [7] "      <end>2016-02-05T13:33:30.000Z</end>"                                                                                                                                            
# [8] "    </TimeSpan>"                                                                                                                                                                      
# [9] "    <Placemark>"                                                                                                                                                                      
#[10] "      <name>Motorbiking</name>"                                                                                                                                                       
#[11] "      <Style>"                                                                                                                                                                        
#[12] "        <LineStyle>"                                                                                                                                                                  
#[13] "          <color>ffff0000</color>"                                                                                                                                                    
#[14] "          <width>5</width>"                                                                                                                                                           
#[15] "        </LineStyle>"                                                                                                                                                                 
#[16] "      </Style>"                                                                                                                                                                       
#[17] "      <LineString>"                                                                                                                                                                   
#[18] "        <tessellate>1</tessellate>"                                                                                                                                                   
#[19] "        <coordinates>-1.8313721418380737,53.9264297485351562,109.0000000000000000"                                                                                                    
#[20] "-1.8313690423965454,53.9264373779296875,110.0000000000000000"     
#> tail(ans)
#[1] "-1.8314907550811768,53.9267654418945312,142.0000000000000000"
#[2] "</coordinates>"                                              
#[3] "      </LineString>"                                         
#[4] "    </Placemark>"                                            
#[5] "  </Document>"                                               
#[6] "</kml>"                                                      
 
    this.call <- match.call()
    fun.name <- "importKML2PEMS"
    extra.args <- list(...)

    to.lower <- if ("to.lower" %in% names(extra.args)) 
        extra.args$to.lower
    else TRUE
    extra.args <- extra.args[!names(extra.args) %in% "to.lower"]

    test <- if ("test" %in% names(extra.args)) 
        extra.args$test else TRUE
    extra.args <- extra.args[!names(extra.args) %in% "test"]

#this is currently only for KML files
#kmz would need unzipping first

#    extra.args <- listUpdate(list(header = TRUE), extra.args)
#    extra.args$file <- file.name
#see readLines help 
#it does not want any extra args it does not know
#formals check/update? 

    ans <- readLines(file.name)

    if(test){
        if(length(grep("</kml>", ans))<1) 
             stop("Sure this is KML? test=FALSE if so.")
        temp <- FALSE

        #tests for known structures
        if(length(grep("<tessellate>1</tessellate>", ans))>0)
              temp <- TRUE
#possible alternative to tessellate
# [8] "    </TimeSpan>"                                                                                                                                                                      
# [9] "    <Placemark>" 

        if(!temp)
             stop("Not a KML format I know. test=FALSE to try to import anway...")
    }

    #get begin and end
    begin <- ans[grep("<begin>", ans)][1]
    begin <- gsub(" ", "", begin)
    begin <- gsub("<begin>", "", begin)
    begin <- gsub("Z</begin>", "", begin)
    begin <- as.POSIXct(strptime(begin, format = "%Y-%m-%dT%H:%M:%OS"))

    end <- ans[grep("<end>", ans)][1]
    end <- gsub(" ", "", end)
    end <- gsub("<end>", "", end)
    end <- gsub("Z</end>", "", end)
    end <- as.POSIXct(strptime(end, format = "%Y-%m-%dT%H:%M:%OS"))

#for later
#because it is tesselate
#return(seq(begin, end, length.out=10))

    ##kml documentation
    #https://developers.google.com/kml/documentation/kml_tut#placemarks
    #says this is lat, lon, alt

    #get coord range

    temp <- grep("<coordinates>", ans)
    ans <- ans[temp[length(temp)]:length(ans)]
    temp <- grep("</coordinates>", ans)
    ans <- ans[1:temp[length(temp)]]
    ans <- gsub(" ", "", ans)
    ans <- gsub("<coordinates>", "", ans)
    ans <- gsub("</coordinates>", "", ans)

    ans <- unlist(strsplit(ans, ","))

    lon <- as.numeric(ans[seq(1, length(ans), by=3)])
    lat <- as.numeric(ans[seq(2, length(ans), by=3)])
    alt <- as.numeric(ans[seq(3, length(ans), by=3)])

    temp <- min(c(length(lat), length(lon), length(alt)), na.rm=TRUE) 
    lat <- lat[1:temp]
    lon <- lon[1:temp]
    alt <- alt[1:temp]
    ans <- data.frame(lat=lat, lon=lon, alt=alt)
    temp <- seq(begin, end, length.out=temp)
    temp <- data.frame(time.stamp = temp, 
                       local.time = as.numeric(temp - temp[1]))
    ans <- cbind(temp, ans)

    #units
    units <- c("Y-M-D H:M:S", "s", "d.degLat", "d.degLon", "m")

    #output
    if (to.lower) 
        names(ans) <- tolower(names(ans))
    output <- makePEMS(x = ans, units = units, constants = constants, 
        history = history, source = source, ...)
    class(output) <- "not.pems"
    output$history[length(output$history)] <- this.call
    class(output) <- "pems"
    return(output)
}



