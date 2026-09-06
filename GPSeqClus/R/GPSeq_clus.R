#' @title Sequential cluster algorithm of location data
#'
#' @description Applies sequential clustering algorithm to location data based on user-defined parameters
#' and appends results to the dataframe. Provides a summary dataframe with attributes for each cluster
#' commonly used as covariates in subsequent modeling efforts. Plots interactive cluster maps.
#'
#' @param dat Any dataframe including single or multiple animal location datasets that includes:
#' \describe{
#'   \item{$AID}{Animal identification for each location}
#'   \item{$TelemDate}{Location timestamps as POSIXct format "YYYY-MM-DD HH:MM:SS" with single "tzone" attribute}
#'   \item{$Long}{Longitude values as decimal degrees (-180 to +180) including NAs for failed fixes}
#'   \item{$Lat}{Latitude values as decimal degrees (-90 to +90) including NAs for failed fixes}
#' }
#' @param search_radius_m Search radius (meters) from cluster centroid when building clusters.
#' @param window_days Temporal window (days) to search for new locations from the most recent location in a cluster
#' @param clus_min_locs Minimum number of locations required to form a cluster. Default is 2.
#' @param centroid_calc Method for recalculating centroids when actively building clusters - e.g., "median" or "mean" (default). Not to be confused with
#'                      plotting the "mean" or "median" centroid once a cluster has been built.
#' @param show_plots Vector of TRUE/FALSE for plotting followed by plotting argument for the "median" or "mean" centroid - e.g., c(TRUE, "mean") (default)
#' @param scale_plot_clus When plotting, scale cluster markers based on number of locations (TRUE/FALSE).
#' @param store_plots When plotting, also assign map outputs to global environment (TRUE/FALSE).
#' @param season_breaks_jul Ascending numeric vector of julian days (0-365) used to classify by season/parturition/hunting seasons etc.
#'                          e.g., c(121, 274, 305) result may be: 1 Nov - 30 Apr (winter = 0), 1 May - 31 Aug (summer = 1), 1 Oct - 31 Oct (hunting season = 2)
#' @param daylight_hrs Manually set start and stop hours (0-24) to classify day and night locations. - e.g. c(6,18) would classify 6AM - 6PM as daylight hrs.
#'                     NA (default) uses 'suncalc' package to convert cluster location and time to be classified based on specific specific sunrise and sunset times.
#' @param prbar Show progress bars (TRUE/FALSE).
#'
#' @return Returns a list containing two dataframes. The first contains the original location dataframe with "clus_ID" column assigning each row a cluster ID if applicable.
#'         The second dataframe in the list contains a summary of sequential clusters and common cluster attributes (descriptions below) for subsequent modeling.
#'         If 'show_plots' argument is active, returns interactive maps of locations and clusters by animal.
#'
#' \describe{
#'   \item{AID}{Animal identification}
#'   \item{clus_ID}{Sequential cluster ID number}
#'   \item{clus_start}{Timestamp of first location in cluster}
#'   \item{clus_end}{Timestamp of last location in cluster}
#'   \item{clus_status}{"Closed" if the time window (window_days) has expired for the cluster according to users Sys.time() output.
#'                      These clusters are therefore solidified and should not change if appending new location data.
#'                      "Open" if the time window remains open at the time the function was run. "Open" clusters have the ability
#'                      to shift sequence, combine with other clusters, emerge as a new cluster, etc. This attribute becomes
#'                      relevant when appending new satellite data to the location dataframe, and may serve as an index of whether
#'                      an animal continues to actively visit the cluster site within the time window.}
#'   \item{g_c_Long}{Geometic centroid longitude value calculated using the mean}
#'   \item{g_c_Lat}{Geometic centroid latitude value calculated using the mean}
#'   \item{g_med_Long}{Geometic centroid longitude value calculated using the median}
#'   \item{g_med_Lat}{Geometic centroid latitude value calculated using the median}
#'   \item{clus_dur_hr}{Hours from the first to last locations of the cluster}
#'   \item{n_clus_locs}{Number of locations within the cluster}
#'   \item{visits}{Number of visits/revisits to the cluster based on the number of times locations fall outside the search radius and return
#'                 to add locations to the cluster}
#'   \item{fix_succ_clus_dur}{Fix rate success during the duration of the cluster}
#'   \item{adj_clus_locs}{Adjusted number of cluster locations accounting for missed fixes (number cluster locations / fix success of cluster duration)}
#'   \item{fid}{Fidelity to the cluster during cluster duration (number locations on cluster - number locations off cluster)}
#'   \item{max_foray}{Maximum location distance (meters) from centroid during cluster duration for all locations}
#'   \item{clus_radius}{Maximum location distance (meters) from centroid during cluster duration for cluster-attributed locations}
#'   \item{avg_clus_dist}{Mean distance from all cluster locations to centroid}
#'   \item{n_24_per}{Number of unique 24 hr periods during the cluster duration that hold at least one cluster location}
#'   \item{bin_24hr}{Binary output for cluster duration (0 == less or equal to 24hr, 1 == greater than 24hr)}
#'   \item{season}{Nominal attribute for user defined seasons based on 'season_breaks_jul' argument}
#'   \item{night_pts}{Number of night cluster locations based on 'daylight_hrs' argument}
#'   \item{night_prop}{Proportion of night cluster locations}
#' }
#'
#' @importFrom sp SpatialPointsDataFrame CRS
#' @importFrom sf st_as_sf st_write
#' @importFrom geosphere distHaversine distm
#' @importFrom suncalc getSunlightTimes
#' @importFrom stats median na.omit
#' @import plyr
#' @importFrom purrr keep walk
#' @import leaflet
#' @importFrom leaflet.extras addSearchFeatures searchFeaturesOptions
#' @importFrom tcltk setTkProgressBar tkProgressBar
#' @importFrom utils globalVariables setTxtProgressBar txtProgressBar
#' @importFrom htmlwidgets onRender
#'
#' @export
#'
#' @examples
#' GPSeq_clus(dat = ML_ex_dat[1:50,], search_radius_m = 200, window_days = 6,
#'            clus_min_locs = 3, show_plots = c(FALSE, "mean"))
#'
#' \donttest{
#' GPSeq_clus(dat = ML_ex_dat, search_radius_m = 50, window_days = 2.5, clus_min_locs = 12,
#'            centroid_calc = "median", show_plots = c(TRUE, "median"), scale_plot_clus = FALSE,
#'            season_breaks_jul = c(120, 240, 300), daylight_hrs = c(8, 16), prbar=FALSE)
#' }
#'
GPSeq_clus<-function(dat, search_radius_m, window_days, clus_min_locs=2, centroid_calc="mean", show_plots=c(TRUE, "mean"), scale_plot_clus=TRUE, store_plots=FALSE, season_breaks_jul=NA, daylight_hrs=NA, prbar=TRUE){
  #ensure data is properly set up below
  if(is.data.frame(dat)==FALSE){stop("GPSeq_clus requires input as dataframe.")}
  if(("AID" %in% colnames(dat))==FALSE){stop("No 'AID' column found.")}
  if(("TelemDate" %in% colnames(dat))==FALSE){stop("No 'TelemDate' column found.")}
  if(("Long" %in% colnames(dat))==FALSE){stop("No 'Long' column found.")}
  if(("Lat" %in% colnames(dat))==FALSE){stop("No 'Lat' column found.")}
  if(inherits(dat$TelemDate, 'POSIXct')==FALSE){stop("'TelemDate' must be POSIXct.")}
  #ensure arguments are valid below
  if(is.na(centroid_calc) | is.null(centroid_calc)){stop("'centroid_calc' argument must = 'median' or 'mean'")}
  if((!is.na(centroid_calc) && !is.null(centroid_calc) && (centroid_calc == "mean" | centroid_calc == "median"))==FALSE){stop("'centroid_calc' argument must = 'median' or 'mean'")}
  if(is.na(search_radius_m) | is.null(search_radius_m)){stop("invalid 'search_radius_m'")}
  if((!is.na(search_radius_m) && !is.null(search_radius_m) && (search_radius_m>=0))==FALSE){warning("No clusters identified when 'search_radius_m' <= 0.")}
  if(clus_min_locs<2){warning("Clusters must have at least 2 locations. 'clus_min_locs' argument < 2 returns default of 2.")}
  if(!is.na(daylight_hrs[1])){
    if((length(daylight_hrs)!=2)==TRUE){stop("Invalid 'daylight_hrs' argument. Must be a vector of 2 numbers representing daylight start and stop times in hours.")}
    if(is.numeric(daylight_hrs)==FALSE){stop("Invalid 'daylight_hrs' argument. Must be a vector of 2 numbers representing daylight start and stop times in hours.")}
    if(any(daylight_hrs<0 | daylight_hrs>23)==TRUE){stop("Invalid 'daylight_hrs' argument. Arguments must be 0-23.")}
  }
  if(all(is.na(show_plots))){stop("Invalid 'show_plots' argument. Must be a vector of 2 arguments consisting of c('TRUE/FALSE', 'mean/median').")}
  if(length(show_plots)!=2){stop("Invalid 'show_plots' argument. Must be a vector of 2 arguments consisting of c('TRUE/FALSE', 'mean/median').")}
  #if(!is.logical(show_plots[1])){stop("Invalid 'show_plots' argument. Must be a vector of 2 arguments consisting of c('TRUE/FALSE', 'mean/median').")}
  if((show_plots[2]=='mean' | show_plots[2]=='median')==FALSE){stop("Invalid 'show_plots' argument. Must be a vector of 2 arguments consisting of c('TRUE/FALSE', 'mean/median').")}
  if(!is.na(season_breaks_jul[1])){
    if(any(is.numeric(season_breaks_jul))==FALSE){stop("Invalid 'season_breaks_jul' argument. Must be acending vector of at least 2 numeric values.")}
    if(length(season_breaks_jul)<2){stop("Invalid 'season_breaks_jul' argument. Must be acending vector of at least 2 numeric values.")}
    if(any(season_breaks_jul<0 | season_breaks_jul>365)==TRUE){stop("Invalid 'season_breaks_jul' argument. Arguments must be 0-365.")}
    if((all(diff(season_breaks_jul)>0))==FALSE){stop("Invalid 'season_breaks_jul' argument. Arguments must be in acending order.")}
  }
  dat<-dat[order(dat$AID,dat$TelemDate),] #begin by ordering the data
  #set up location data output with cluster number attribute
  dat2<-dat[1,]
  dat2$clus_ID<-NA
  dat2<-dat2[-1,]
  uni_AID<-as.character(unique(dat$AID))            #loop through AIDs
  if(prbar==TRUE){
    message("TOTAL PROGRESS")
    pb <- utils::txtProgressBar(min=0, max=length(uni_AID), style=3)}  #initiate base progress bar
  for(zz in 1:length(uni_AID)) {                        #start loop
    out_all<-subset(dat, AID == uni_AID[zz])                                    #get rows per AID
    if(length(which(is.na(out_all$Lat)))==0){warning(paste(uni_AID[zz], "shows no missed locations. Ensure 'failed' fix attempts are included for accurate cluster attributes."))}
    #subset successful fixes for algorithm
    out<-out_all[which(!is.na(out_all$Lat)),]                                   #subset only usable locations
    if(nrow(out)>0){
      ##############################################
      ###########Construct Primary Cluster Algorithm
      ##############################################
      #add/prep columns
      out$index<-seq(1:nrow(out))
      out$ClusID<-0
      out<-moveMe(out, "ClusID", "first")
      out<-moveMe(out, "index", "first")
      #first internal loop with windows progress bar and label by animal and process
      c<-1                                                                    #seed ticker for sequential cluster numbers
      if(prbar==TRUE){pb2 <- tcltk::tkProgressBar(min = 0, max = nrow(out), width = 500)}
      for(j in 1:nrow(out)){                                                  #for each sequential location
        if(out$ClusID[j] == 0){                                              #IF THE LOCATION HAS NOT BEEN ASSIGNED A CLUSTER
          cent<-c(out[j,"Long"], out[j,"Lat"])                                   #LL reference point location as cluster centroid
          t<- out[which(out$TelemDate > out[j,"TelemDate"] & out$TelemDate <= out[j,"TelemDate"] + as.difftime(window_days, units= "days")),] #create time window
          AR_Clus<- unique(t[which(t$ClusID >0),"ClusID"])                      #list of clusters IDs already formed within the time window
          for(m in 1:length(AR_Clus)){  #this small loop updates locations within the time window that already are clustered the centroid of thier clusters
            b<-out[which(out$ClusID == AR_Clus[m]),]                            #pull all the locations from that cluster
            if(nrow(t)>0){
              if(centroid_calc == "median"){
                t[which(t$ClusID == AR_Clus[m]),"Long"]<-stats::median(b$Long)             #recalc gc Long   -- median --
                t[which(t$ClusID == AR_Clus[m]),"Lat"]<-stats::median(b$Lat)               #recalc gc Lat
              } else {
                t[which(t$ClusID == AR_Clus[m]),"Long"]<-mean(b$Long)             #recalc gc Long  -- mean --
                t[which(t$ClusID == AR_Clus[m]),"Lat"]<-mean(b$Lat)               #recalc gc Lat
              }
            }
          }
          if(nrow(t)>0){
            t$centLong<-cent[1]                                                   #LL attribute all point distances from cent
            t$centLat<-cent[2]
            t$dist<-geosphere::distm(cbind(t$centLong,t$centLat), cbind(t$Long,t$Lat), fun = geosphere::distHaversine)[1,]
          }
          if(length(t[which(t$dis<=search_radius_m),"index"])>0){                           #if a point meets distance criteria
            if(out$ClusID[min(t[which(t$dist<search_radius_m),"index"])]==0){                #and if that candidate also has not been assigned
              out$ClusID[j]<-c                                                  #assign the reference point a new clusID
              out$ClusID[min(t[which(t$dist<search_radius_m),"index"])]<-c                   #also assign the candidate point the same ID to create the new cluster
              c<-c+1                                                            #then hit the ticker
            } else {                                                            #OR, if the candidate is already part of a cluster
              out$ClusID[j]<-out$ClusID[min(t[which(t$dist<search_radius_m),"index"])]       #just assign the point the candidate that cluster ID
            }
          }
        } else { #IF THE LOCATION HAS ALREADY BEEN ASSIGNED A CLUSTER - compare cluster centroid as reference point
          if(centroid_calc == "median"){
            cent<- c(stats::median(out[which(out$ClusID == out$ClusID[j]),"Long"]),stats::median(out[which(out$ClusID == out$ClusID[j]),"Lat"]))#recalc centroid --median--
          } else {
            cent<- c(mean(out[which(out$ClusID == out$ClusID[j]),"Long"]),mean(out[which(out$ClusID == out$ClusID[j]),"Lat"]))#recalc centroid --mean--
          }
          t<- out[which(out$TelemDate > out[j,"TelemDate"] & out$TelemDate <= out[j,"TelemDate"] + as.difftime(window_days, units= "days")),] #create time window
          AR_Clus<- unique(t[which(t$ClusID >0),"ClusID"])                      #same loop as before to update candidate cluster centers
          for(m in 1:length(AR_Clus)){
            b<-out[which(out$ClusID == AR_Clus[m]),]
            if(nrow(t)>0){
              if(centroid_calc == "median"){
                t[which(t$ClusID == AR_Clus[m]),"Long"]<-stats::median(b$Long)             #recalc gc Long   -- median --
                t[which(t$ClusID == AR_Clus[m]),"Lat"]<-stats::median(b$Lat)               #recalc gc Lat
              } else {
                t[which(t$ClusID == AR_Clus[m]),"Long"]<-mean(b$Long)             #recalc gc Long  -- mean --
                t[which(t$ClusID == AR_Clus[m]),"Lat"]<-mean(b$Lat)               #recalc gc Lat
              }
            }
          }
          if(nrow(t)>0){
            t$centLong<-cent[1]                                                   #LL
            t$centLat<-cent[2]
            t$dist<-geosphere::distm(cbind(t$centLong,t$centLat), cbind(t$Long,t$Lat), fun = geosphere::distHaversine)[1,]
          }
          if(length(t[which(t$dist<=search_radius_m),"index"])>0){                           #if a location meets distance criteria
            if(out$ClusID[min(t[which(t$dist<search_radius_m),"index"])]==0){                #and if that candidate also has not been assigned
              out$ClusID[min(t[which(t$dist<search_radius_m),"index"])]<- out$ClusID[j]      #assign candidate the reference cluster ID
            } else{ #if the candidate is also part of a cluster - we need to merge the clusters into one
              #the next line looks at all locations from both clusters for the minimum Telemdate between them for that cluster ID
              merge_num<-out[which(out$TelemDate == min(c(out[which(out$ClusID == out$ClusID[j]),"TelemDate"],out[which(out$ClusID == out$ClusID[min(t[which(t$dist<search_radius_m),"index"])]),"TelemDate"]))), "ClusID"]
              #then we attribute both clusters that ID
              out[which(out$ClusID == out$ClusID[j]),"ClusID"]<-merge_num
              out[which(out$ClusID == out$ClusID[min(t[which(t$dist<search_radius_m),"index"])]),"ClusID"]<-merge_num
              rm(merge_num)
            }
          }
        }
        if(prbar==TRUE){tcltk::setTkProgressBar(pb2, j, title=paste("Animal", uni_AID[zz], "...building clusters...", round(j/nrow(out)*100, 0), "% completed"))}                                                       #update progress bar
      }
      if(prbar==TRUE){
        close(pb2)
        rm(pb2,b,c,m,j,t,AR_Clus,cent)
      } else {
        rm(b,c,m,j,t,AR_Clus,cent)
      }

      ###################fin primary cluster algroythm
      see<-rle(out$ClusID)                                          #run the rle function, to get bouts of identical values (cluster IDs)
      bout_end<- cumsum(rle(out$ClusID)$lengths)                    #record the start row
      bout_start<- as.integer(c(1, bout_end +1))                    #and end row of each bout
      bout_start<- bout_start[-length(bout_start)]                  #remove the last start bout
      bout_start<-out$TelemDate[bout_start]                         #convert bout starts to timestamps
      bout_end<-out$TelemDate[bout_end]                             #convert bout ends to timestamps
      bout_duration<-round(difftime(bout_end, bout_start, units= "hours"),1)  #calculate bout durations
      bouts<-data.frame(bout_start,bout_end, bout_duration, see[["lengths"]], see[["values"]]) #merge into a new bouts dataframe
      names(bouts)[names(bouts) == "see...values..."]<- "ClusID"            #rename column for Cluster ID
      names(bouts)[names(bouts) == "see...lengths..."]<- "consec_locs"      #rename column for consective locations
      rm(bout_start, bout_end, see)                                         #clear variables
      #we now have a dataframe that shows temporal bouts and number of locations in sequence
      #but these include non-cluster bouts and jump back and forth among cluster IDs
      bouts2<-subset(bouts, ClusID != 0)                            #so remove non-cluster bouts and put in new bout2 df
      bouts2<-bouts2[order(bouts2$ClusID, bouts2$bout_start),]      #sort by cluster ID and bout start
      #summarize clusters
      clus_summary<-plyr::ddply(bouts2, "ClusID", summarize,
                                clus_start= min(bout_start), clus_end= max(bout_end),
                                clus_dur_hr=round(difftime(max(bout_end), min(bout_start), units="hours"),1),
                                n_clus_locs= sum(consec_locs))
      #eliminate clusters that dont meet the required minimum number of locations
      clus_summary<-clus_summary[which(clus_summary$n_clus_locs >= clus_min_locs),]
      if(nrow(clus_summary)==0){
        warning(paste("Zero clusters identified for", uni_AID[zz], "given user-entered parameters."))
        out_all$clus_ID<-NA
      } else {
        #sort and relabel new clusters in sequence
        clus_summary<-clus_summary[order(clus_summary$clus_start),] #sort
        clus_summary$clus_ID3<-seq(1:nrow(clus_summary))
        clus_summary<-moveMe(clus_summary, "clus_ID3" , "first")
        #now reattribute cluster/revisit in bouts2
        bouts2$clus_ID3<-NA
        bouts2$clus_ID3<-clus_summary[match(bouts2$ClusID, clus_summary$ClusID), "clus_ID3"]  #match bouts with summary cluster numbers
        bouts2<-moveMe(bouts2, "clus_ID3" , "first")
        bouts2<-bouts2[order(bouts2$clus_ID3, bouts2$bout_start),] #sort by cluster ID and bout start
        clus_summary$ClusID<-NULL
        bouts2$ClusID<-NULL
        bouts2<-bouts2[which(!is.na(bouts2$clus_ID3)),]  #remove bouts associated with clusters that dont meet minimum point criteria
        #Calculate number of visits/revisits to each cluster and add as a cluster summary attribute
        see<-rle(bouts2$clus_ID3)         #run rle again to get the number of revisits
        clus_summary$visits<-see$lengths  #add number of visits to the cluster summary
        #now reattribute cluster IDs to the whole dataset including missed locations
        out_all$clus_ID3<-NA
        for(k in 1:nrow(bouts2)){
          out_all[which(out_all$TelemDate >= bouts2$bout_start[k] & out_all$TelemDate <= bouts2$bout_end[k]), "clus_ID3"]<- bouts2$clus_ID3[k]
        }
        rm(k, bouts, bouts2, bout_duration, see, out)
        #relabel and clean up columns
        names(clus_summary)[names(clus_summary) == "clus_ID3"]<- "clus_ID"
        names(out_all)[names(out_all) == "clus_ID3"]<- "clus_ID"
        clus_summary$AID<-out_all$AID[1]
        clus_summary<-moveMe(clus_summary, "AID", "first")
        #############fin with cluster identification###################################
        ###################Build Cluster Attributes for modeling######################
        #add which clusters are currently open during this system run time
        clus_summary$clus_status<-"Closed"
        clus_summary<-moveMe(clus_summary, "clus_status", "after", "clus_end")
        clus_summary[which(clus_summary$clus_end >= Sys.time() - as.difftime(window_days, units= "days")), "clus_status"]<- "Open"
        #calculate geometric center of clusters from cluster points and attribute to clus_summary    -- compare median vs mean location
        xxx<-plyr::ddply(out_all, "clus_ID", summarize,
                         g_c_Long= mean(Long, na.rm=TRUE), g_c_Lat= mean(Lat, na.rm=TRUE), g_med_Long= stats::median(Long, na.rm=TRUE), g_med_Lat= stats::median(Lat, na.rm=TRUE))
        xxx<-stats::na.omit(xxx)  ### use na.omit() to account for cases where only one cluster is formed by all locations
        clus_summary<-cbind(clus_summary, xxx[,2:5]) #bind to cluster summary
        clus_summary<-moveMe(clus_summary,c("g_c_Long", "g_c_Lat", "g_med_Long", "g_med_Lat"), "after", "clus_status")
        rm(xxx)
        #add some additional cluster attributes
        clus_summary$fix_succ_clus_dur<-NA      # (fix success of clus duration)
        clus_summary$adj_clus_locs<-NA          # (number clus pts / fix success of cluster duration)
        clus_summary$fid<-NA                    # (number clus pts - points away)- Fidelity
        clus_summary$max_foray<-NA              # max distance from center cluster during cluster duration
        clus_summary$clus_radius<-NA            # max dist within cluster locs -cluster radius
        clus_summary$avg_clus_dist<-NA          # avg. dist from cluster locs to centroid
        clus_summary$n_24_per<-NA               # number of 24 hr periods with cluster points over cluster duration
        clus_summary$bin_24hr<-NA               # cluster points span less than or greater than one 24 hr period
        clus_summary$season<-NA                 # seasons sequence based on julian breaks
        clus_summary$night_pts<-NA              # number of night cluster locs
        clus_summary$night_prop<-NA             # number night locs (1800-0600)/total cluster locs (night=0, day=1)
        if(!is.na(season_breaks_jul[1])){clus_summary$season<-0}  #if the season argument exists, set all to reference season == 0 before loop
        #loop to calculate attributes
        if(prbar==TRUE){pb3 <- tcltk::tkProgressBar(min = 0, max = nrow(clus_summary), width = 500)}
        for(i in 1:nrow(clus_summary)){
          ggg<-out_all[which(out_all$TelemDate >= clus_summary$clus_start[i] & out_all$TelemDate <= clus_summary$clus_end[i]),] #subset cluster fix attempts
          clus_summary$fix_succ_clus_dur[i]<-round(nrow(ggg[which(!is.na(ggg$Lat)),])/nrow(ggg),2)                              #prop fix success during cluster duration
          clus_summary$adj_clus_locs[i]<- round(clus_summary$n_clus_locs[i] / clus_summary$fix_succ_clus_dur[i],1)              #adjusted cluster locations
          fff<-ggg[which(!is.na(ggg$Lat)),]                                                                                     #subset cluster locations
          fff<-fff[which(fff$clus_ID != clus_summary$clus_ID[i] | is.na(fff$clus_ID)),]                                         #locations off cluster
          clus_summary$fid[i]<- clus_summary$n_clus_locs[i] - nrow(fff)                                                         #locs on - locs off = Fidelity
          ggg$centLong<-clus_summary[i, "g_c_Long"]                                                                             #LL distances from centroid
          ggg$centLat<-clus_summary[i, "g_c_Lat"]
          ggg$dist<-geosphere::distm(cbind(ggg$centLong,ggg$centLat), cbind(ggg$Long,ggg$Lat), fun = geosphere::distHaversine)[1,]
          ggg$days<-difftime(ggg$TelemDate, ggg$TelemDate[1], units="days")                                                     #sets up 24 hr period covariates
          clus_summary$max_foray[i]<-round(max(ggg$dist, na.rm=T))                                                              #max foray distance from cluster
          clus_summary$clus_radius[i]<-round(max(ggg$dist[which(ggg$clus_ID == clus_summary$clus_ID[i])],na.rm=T))              #max dist of cluster locations = cluster radius
          clus_summary$avg_clus_dist[i]<- round(mean(ggg$dist[which(ggg$clus_ID == clus_summary$clus_ID[i])],na.rm=T))          #mean distance of cluster locations from center
          aa<-ggg[which(ggg$clus_ID == clus_summary$clus_ID[i]),]                                                               #subset cluster locs
          clus_summary$n_24_per[i]<-length(unique(floor(aa$days)))                                                              #number of unique 24hr periods with at least one location at cluster
          if(clus_summary$n_24_per[i]>1){clus_summary$bin_24hr[i]<-1} else {clus_summary$bin_24hr[i]<-0}                        #binary whether cluster was >24 hrs
          #set up seasons
          if(!is.na(season_breaks_jul[1])){                                     #if season breaks were entered as an argument
            for(p in 2:length(season_breaks_jul)){                              #from the second break to the number of breaks entered
              if(julian_conv(ggg$TelemDate[1])>= season_breaks_jul[p-1] & julian_conv(ggg$TelemDate[1])< season_breaks_jul[p]){ #apply dichotomous/nominal season
                clus_summary$season[i]<-p-1
              }
            }
            rm(p)
          }
          #day/night locations for each cluster
          aa<-aa[which(!is.na(aa$Lat)),]
          if(!is.na(daylight_hrs[1])){
            #option to set own breaks for day v night locations
            aa$hour<-NA
            aa$hour<-as.numeric(format(aa$TelemDate, format='%H'))
            clus_summary$night_pts[i]<-nrow(aa[which(aa$hour<=daylight_hrs[1] | aa$hour>=daylight_hrs[2]),])
            clus_summary$night_prop[i]<-round(clus_summary$night_pts[i] / clus_summary$n_clus_locs[i],2)
          } else{
            ttt<-aa[,c("TelemDate", "Long", "Lat")]
            #since the format %Z occasionally returns multiple zones, we will use the attr() call
            #ttt$date<-as.Date(aa$TelemDate, tz=(unique(base::format(out_all$TelemDate, format="%Z"))))
            ttt$date<-as.Date(aa$TelemDate, tz=attr(dat$TelemDate,"tzone"))
            names(ttt)[names(ttt) == "Lat"]<- "lat"
            names(ttt)[names(ttt) == "Long"]<- "lon"
            #dd<-getSunlightTimes(data=ttt ,tz=(unique(base::format(out_all$TelemDate, format="%Z")))) #get times
            dd<-suncalc::getSunlightTimes(data=ttt ,tz=attr(dat$TelemDate,"tzone"))
            dd$TelemDate<-aa$TelemDate
            #determine night locations
            #aa$night<-dd %>% rowwise() %>% mutate(match = ifelse(between(TelemDate, sunrise, sunset), 1, 0))# %>% select(match) #night=0, day=1
            aa$night<-ifelse(((dd$TelemDate>=dd$sunrise) & (dd$TelemDate<=dd$sunset)),1,0)
            clus_summary$night_pts[i]<-length(which(aa$night == 0))
            clus_summary$night_prop[i]<-round(clus_summary$night_pts[i] / clus_summary$n_clus_locs[i],2)
            rm(dd, ttt)
          }
          if(prbar==TRUE){tcltk::setTkProgressBar(pb3, i, title=paste("Animal",uni_AID[zz], "...building cluster covariates...", round(i/nrow(clus_summary)*100, 0), "% completed"))}
        }
        if(prbar==TRUE){
          close(pb3)
          rm(pb3, ggg, fff, aa, i)
        } else {
          rm(ggg, fff, aa, i)
        }
        ####END cluster prep####
      }
      #####add leaflet plotting component
      if(show_plots[1]==T){
        if(nrow(clus_summary)>0){
          if(show_plots[2] == "median"){
            clus_plot<-clus_summary
            clus_plot$Lat<-clus_plot$g_med_Lat
            clus_plot$Long<-clus_plot$g_med_Long
          } else {
            clus_plot<-clus_summary
            clus_plot$Lat<-clus_plot$g_c_Lat
            clus_plot$Long<-clus_plot$g_c_Long
          }
          clus_plot$Popup<-NA
          clus_plot$Popup<-paste(clus_plot$AID[1], paste("Clus#", as.character(clus_plot$clus_ID),sep=""), "centroid",sep=" ")
        }
        #prep individual locations
        out2<-out_all
        out2<-out2[which(!is.na(out2$Lat)),]
        out2$Label<-NA
        out2$Label<-paste(out2$AID, as.character(out2$TelemDate),"cn", out2$clus_ID, sep=" ") #Create hover over layer
        out2$type<-"location"
        if(nrow(clus_summary)>0){
          out2[which(out2$clus_ID %in% clus_summary[,"clus_ID"]),"type"]<-"cluster location"
          pal2<- leaflet::colorFactor(
            palette= c('darkred', 'black'),
            domain = out2$type)
        } else{
          pal2<- leaflet::colorFactor(
            palette= c('black'),
            domain = out2$type)
        }
        a<-out2 %>%
          leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::addCircleMarkers(lng=~Long, lat=~Lat, radius=.5, opacity=100, label=~Label,color=~pal2(type), group="Locations") %>%
          leaflet::addPolylines(data=out2,lng=~Long, lat=~Lat, weight=2, color="black", group="Locations") %>%
          leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap)  #choose base layer
        if(nrow(clus_summary)>0){
          if(scale_plot_clus==TRUE){
            a<- leaflet::addCircleMarkers(map=a,data=clus_plot, lng=~Long, lat=~Lat, radius=~n_clus_locs, color="yellow",opacity=100,popup=~Popup,group="Clusters")
          } else {
            a<- leaflet::addCircleMarkers(map=a,data=clus_plot, lng=~Long, lat=~Lat, radius=1, color="yellow",opacity=100,popup=~Popup,group="Clusters")
          }
          a<-leaflet::addMarkers(map=a, data=clus_plot, lng=~Long, lat=~Lat, group="searchClusters", popup =~Popup,
                                 icon = leaflet::makeIcon(iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",iconWidth = 1, iconHeight = 1))
          a<-leaflet.extras::addSearchFeatures(map=a, targetGroups = "searchClusters", options = leaflet.extras::searchFeaturesOptions(propertyName = 'popup', openPopup=T, zoom=15, hideMarkerOnCollapse=T))
        }
        #Set up ESRI provided tiles
        esri <- providers %>%
          purrr::keep(~ grepl('^Esri',.))
        #remove a bunch of worthless esri layers
        esri[[11]]<-NULL
        esri[[9]]<-NULL
        esri[[8]]<-NULL
        esri[[7]]<-NULL
        esri[[6]]<-NULL
        esri[[2]]<-NULL
        #reorder list so desired list is on top of legend and as primary basemap
        esri <- esri[c("Esri.DeLorme", "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.NatGeoWorldMap", "Esri")]
        esri %>%
          purrr::walk(function(x) a <<- a %>% leaflet::addProviderTiles(x,group=x))
        a<-a %>%
          leaflet::addLayersControl(
            baseGroups = names(esri),
            options = leaflet::layersControlOptions(collapsed = TRUE),
            overlayGroups = c("Clusters","Locations"))%>%
          leaflet::addLegend(pal = pal2, values = out2$type, group = "Locations", opacity=100, position = "bottomleft")
        a <- a %>% addTitle(text=out2$AID[1], color= "black", fontSize= "18px", leftPosition = 50, topPosition=2)
        print(a)
        if(nrow(clus_summary)>0){rm(clus_plot)}
        if(store_plots==T){
          assign(paste0("Map_", print(as.character(out2$AID[1]))), a)
        }
        rm(a, esri, out2, pal2)
      }
      if(!exists("t_summ") & (ncol(clus_summary)==23)){
        t_summ<-clus_summary[1,]
        t_summ<-t_summ[-1,]
      }
      if(ncol(clus_summary)==23){
        t_summ<-rbind(t_summ, clus_summary)
      }
    } else {
      warning(paste(uni_AID[zz], "has no 'successful' fixes."))
      out_all$clus_ID<-NA
      if(!exists("dat2")){
        dat2<-out_all[1,]
        dat2<-out_all[-1,]
      }
    }
    dat2<-rbind(dat2,out_all)   #append dat2 with out_all data
    if(prbar==TRUE){utils::setTxtProgressBar(pb, zz)}                         #update the base progress bar % for each animal
  }
  if(prbar==TRUE){close(pb)}                                          #when finished close the base progress bar
  dat<-dat2  #write the updated location output back to dat
  rm(dat2)
  clus_summary<-t_summ  #write the cluster summary info back to clus_summary
  if(prbar==TRUE){rm(pb)}
  rm(zz, uni_AID, out_all, t_summ)
  return(list(dat, clus_summary))
}
