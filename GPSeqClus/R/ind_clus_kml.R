#' @title Plot individual cluster .kml
#'
#' @description Uses results from 'GPSeq_clus" to plot individual cluster .kmls
#'
#' @param AID Desired AID from sequential cluster output
#' @param cn Desired cluster number
#' @param locs Location dataframe output from GPSeq_clus()
#' @param cs Cluster summary output from GPSeq_clus()
#' @param centroid_calc 'mean' (default) or 'median' centroid plot
#' @param overwrite TRUE (default) labels output as "ind.kml" that overwrites with each run within tempdir(). FALSE saves outputs as "AID_cn"
#' @param dir File path when saving output
#'
#' @return Opens the cluster locations and centroid .kml for assessment.
#' @export
#'
#'
ind_clus_kml<- function(AID, cn, locs, cs, centroid_calc= "mean", overwrite= TRUE, dir= NULL){
  store_dir<-getwd()
  on.exit(setwd(store_dir))
  if(AID %in% cs$AID == FALSE){stop(paste("AID", AID, "not found", sep=" "))}
  if(length(cn)!=1){stop("'ind_clus_kml()' only accepts individual clusters")}
  if(cn %in% cs[which(cs$AID == AID), "clus_ID"] == FALSE){stop(paste("Cluster", cn, "does not exist for", AID, sep=" "))}
  if((!is.na(centroid_calc) && !is.null(centroid_calc) && (centroid_calc == "mean" | centroid_calc == "median"))==FALSE){stop("'centroid_calc' argument must = 'median' or 'mean'")}
  if((!is.na(overwrite) && !is.null(overwrite) && (overwrite == TRUE | overwrite == FALSE))==FALSE){stop("'overwrite' argument must = 1/T/TRUE or 0/F/FALSE")}
  ind_clus<-locs[which(locs$AID == AID & locs$clus_ID == cn),]
  ind_clus<-ind_clus[which(!is.na(ind_clus$Lat)),]
  ind_clus$AID2<-""
  ind_clus<-ind_clus[,c("AID2", "AID", "clus_ID", "TelemDate", "Long", "Lat")]
  clus_g_c<-cs[which(cs$AID == AID & cs$clus_ID==cn),]
  if(centroid_calc=="median"){
    spgeo<-sp::SpatialPointsDataFrame(matrix(c(clus_g_c$g_med_Long, clus_g_c$g_med_Lat), ncol=2), clus_g_c, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  } else {
    spgeo<-sp::SpatialPointsDataFrame(matrix(c(clus_g_c$g_c_Long, clus_g_c$g_c_Lat), ncol=2), clus_g_c, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  }
  aa<-ind_clus[1,]
  aa$AID2<-"Centroid"
  aa$TelemDate<-NA
  aa$Lat<-spgeo$coords.x2
  aa$Long<-spgeo$coords.x1
  ind_clus<-rbind(ind_clus, aa)
  if(overwrite==TRUE){
    f_name<-"ind"
    setwd(tempdir())
  } else {
    f_name<-paste(AID, "_", cn, sep="")
    setwd(dir)
  }
  ind_clus<- sp::SpatialPointsDataFrame(matrix(c(ind_clus$Long, ind_clus$Lat), ncol=2), ind_clus, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  #set the name of the field you want in the gps
  ind_clus@data$name<-paste(ind_clus$AID[1], ind_clus$clus_ID, ind_clus$AID2, ind_clus@data$TelemDate, sep=" ")
  sf <- sf::st_as_sf(ind_clus)
  sf$name[sf$AID2=="Centroid"]=paste(ind_clus$AID[1], ind_clus$clus_ID[1], "Centroid", sep=" ")
  #write basic kml
  sf::st_write(obj= sf["name"], driver="KML", dsn= if(overwrite==TRUE){paste0(tempdir(),"/", "ind.kml")}else{paste0(dir,"/",f_name, ".kml")}, delete_layer=TRUE)
  #read back file to edit kml properties
  ed<-readLines(if(overwrite==TRUE){paste0(tempdir(),"/", "ind.kml")}else{paste0(dir,"/",f_name, ".kml")}) #opens for script-based editing
  #first edit in the color and scale layers
  ed  <- gsub(pattern = paste0('<Folder><name>', f_name, '</name>'),
              replacement = paste0('<Folder><name>', f_name, '</name>
         <Style id="all">
         <LabelStyle>
         <scale>0.6</scale>
         </LabelStyle>
         <IconStyle>
         <color>#ff1c1ae4</color>
         <scale>0.45</scale>
         <Icon>
         <href>http://maps.google.com/mapfiles/kml/pal2/icon18.png</href>
         </Icon>
         </IconStyle>
         <BalloonStyle>
         <text>$[description]</text>
         </BalloonStyle>
         </Style>
         <Style id="centroid">
         <LabelStyle>
         <scale>0.6</scale>
         </LabelStyle>
         <IconStyle>
         <color>#ff999999</color>
         <scale>0.45</scale>
         <Icon>
         <href>http://maps.google.com/mapfiles/kml/pal2/icon18.png</href>
         </Icon>
         </IconStyle>
         <BalloonStyle>
         <text>$[description]</text>
         </BalloonStyle>
         </Style>'), x = ed)
  for(g in 1:nrow(sf)){ #now loop through for timestamps
    if(g == nrow(sf)){
      ed  <- gsub(pattern = paste0("Centroid", '</name'), replacement = paste0('Centroid','</name>', '<styleUrl>#centroid</styleUrl>'), x = ed)
    } else {
      ed  <- gsub(pattern = paste0(as.character(sf$TelemDate[g]), '</name>'), replacement = paste0(as.character(sf$TelemDate[g]),'</name>', '<styleUrl>#all</styleUrl>',
                                                                                               '<TimeStamp><when>',
                                                                                               gsub(" ", "", gsub(as.character(sf$TelemDate[g]), pattern = "(.{10})(.*)", replacement = "\\1T\\2"), fixed = TRUE),
                                                                                               'Z</when></TimeStamp>'), x = ed)
    }
  }
  #replace basic kml with edited version
  writeLines(ed, con=if(overwrite==TRUE){paste0(tempdir(),"/", "ind.kml")}else {paste0(dir,"/",f_name, ".kml")})
  #auto open
  shell(if(overwrite==TRUE){paste0(tempdir(),"/", "ind.kml")}else{paste0(dir,"/",f_name, ".kml")}, wait=FALSE)
  #clear objects
  rm(spgeo, sf, g, ed, aa, ind_clus, f_name, clus_g_c)
}

