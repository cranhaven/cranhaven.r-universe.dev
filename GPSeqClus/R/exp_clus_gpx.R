#' @title Export cluster .gpx file
#'
#' @description Uses results from 'GPSeq_clus" to export .gpx file from specified AID and vector of desired cluster numbers
#' for navigation during field site investigations.
#'
#' @param AID Desired AID from sequential cluster output
#' @param cn Numeric vector of desired cluster numbers to include in .gpx output, default is "all"
#' @param locs Location dataframe output from GPSeq_clus()
#' @param cs Cluster summary output from GPSeq_clus()
#' @param centroid_calc 'mean' (default) or 'median' centroid plot
#' @param dir File path to save output
#'
#' @return .gpx file
#' @export
#'
#' @examples
#' \donttest{
#' exp_clus_gpx(AID = "ML1605M", cn = 4,
#'              locs = GPSeq_clus(dat = ML_ex_dat[1:50,], search_radius_m = 200, window_days = 6,
#'                     clus_min_locs = 3, show_plots = c(FALSE, "mean"))[[1]],
#'              cs = GPSeq_clus(dat = ML_ex_dat[1:50,], search_radius_m = 200, window_days = 6,
#'                   clus_min_locs = 3, show_plots = c(FALSE, "mean"))[[2]],
#'              dir= tempdir()
#' )
#' }
#'
exp_clus_gpx<-function(AID, cn= "all", locs, cs, centroid_calc="mean", dir= NULL){
  if(AID %in% cs$AID == FALSE){stop(paste("AID", AID, "not found", sep=" "))}
  if(cn[1] == "all"){cn<-cs[which(cs$AID == AID),"clus_ID"]}
  clus_sub<-cs[which(cs$AID == AID & cs$clus_ID %in% cn),]
  if(length(cn[-which(cn %in% cs$clus_ID)])>0){stop(paste("Cluster(s)", paste(cn[-which(cn %in% cs$clus_ID)], collapse=","), "do not exist in cluster summary", sep=" "))}
  out_all<- locs[which(locs$AID == AID),]
  sites<-NA
  for(p in 1:nrow(clus_sub)){
    ind_clus<-out_all[which(out_all$clus_ID == clus_sub$clus_ID[p]),]
    ind_clus<-ind_clus[which(!is.na(ind_clus$Lat)),]
    ind_clus$AID2<-""
    ind_clus<-ind_clus[,c("AID2", "AID", "clus_ID", "TelemDate", "Long", "Lat")]
    clus_g_c<-clus_sub[which(clus_sub$clus_ID == cn[p]),]
    if(centroid_calc=="median"){
      spgeo<-sp::SpatialPointsDataFrame(matrix(c(clus_g_c$g_med_Long, clus_g_c$g_med_Lat), ncol=2), clus_g_c, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
    } else {
      spgeo<-sp::SpatialPointsDataFrame(matrix(c(clus_g_c$g_c_Long, clus_g_c$g_c_Lat), ncol=2), clus_g_c, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
    }
    aa<-ind_clus[1,]
    aa$AID2<-"Centroid"
    aa$TelemDate<-ind_clus$TelemDate[nrow(ind_clus)]+25200 #this adds a generic time to the end of the cluster
    aa$Lat<-spgeo$coords.x2
    aa$Long<-spgeo$coords.x1
    ind_clus<-rbind(ind_clus, aa)
    ind_clus[which(ind_clus$AID2 == "Centroid"), "TelemDate"]<- NA
    sites<-rbind(sites, ind_clus)
  }
  sites<-sites[-1,]
  sites<-sites[order(sites$AID2,sites$TelemDate),] #sort
  #write gpx
  ind_gpx<- sp::SpatialPointsDataFrame(matrix(c(sites$Long, sites$Lat), ncol=2), sites, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  #set the name of the field you want in the gps
  ind_gpx@data$name<-paste(out_all$AID[1], sites$clus_ID, sites$AID2, ind_gpx@data$TelemDate, sep=" ")
  #sf object to write gpx
  sf <- sf::st_as_sf(ind_gpx)
  sf::st_write(obj= sf["name"], driver="GPX", dsn= paste0(dir,"/",out_all$AID[1],".gpx"), delete_layer=TRUE)
}
