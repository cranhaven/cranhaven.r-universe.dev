# Part of the SMITIDstruct R package.
# Copyright (C) 2018 Jean-François Rey <jean-francois.rey@inra.fr>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,i
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#

#' loadHost
#' @description load host object from a file
#' @param file a file containing hosts data
#' @return a list of Host object (HostSet)
#' include Class-Host.R
#' @export
loadHost <- function(file="host.txt") {
  
  if( !file.exists(file) ) {
    stop("can not find ", file)
  }
  
  dfhost <- utils::read.table(file)
  #print(dfhost)
  
  host <- apply(dfhost, 1, function(x) { new("Host", ID = as.character(x[1]) )}) 
  names(host) <- as.character(dfhost[,1])
  class(host) <- "HostSet"
  
  return(host)
}

#' createHost
#' @description create a list of Host class object
#' @param list_host a character vector of host ID
#' @return a HostSet of host object with there ID
#' @examples
#' lh <- seq(1,30,1)
#' lhost <- createHost(lh)
#'
#' @export
createHost <- function(list_host) {
  
  lhost <- lapply(list_host, function(x) { new("Host", ID = as.character(x) )}) 
  names(lhost) <- as.character(list_host[])
  class(lhost) <- "HostSet"
  
  return(lhost)
}

#' addHost
#' @description add an Host to a HostSet
#' @param lhost a hostSet Object
#' @param id a character of host ID
#' @return a HostSet of host object with there ID
#' @examples
#' lhost <- list()
#' lhost <- addHost(lhost,"42")
#'
#' @export
addHost <- function(lhost, id) {
    
    if( !is.null(lhost[[as.character(id)]]) ) { return(lhost)}
    
    host <- new("Host", ID = as.character(id))
    lhost[[as.character(id)]] <- host
    
    return(lhost)
}

#' loadTree
#' @description load sources and offsprings from file
#' @param lhost a HostSet
#' @param file a file containing tree data
#' @param source column name for source ID
#' @param receptor column name for receptor ID
#' @param tinf column name for infection Time
#' @param weight column name of infection weight
#' @return the lhost param update with sources and offsprings
#' @examples
#' path = system.file("extdata", "data-simul/", package="SMITIDstruct")
#' lhost <- list()
#' class(lhost) <- "hostSet"
#' lhost <- loadTree(lhost,paste(path,"/tree.txt",sep=''))
#' @export
loadTree <- function(lhost = list(), file="tree.txt", source = "ID-source", receptor = "ID-receptor", tinf = "Tinf", weight = "Weight") {
  
  tree <- utils::read.table(file,header=TRUE, check.names=FALSE)
  
  lhost <- loadTreeDF(lhost,tree,source,receptor,tinf,weight)
  return(lhost)
  # apply(tree, 1, function(x) {
  #   if( is.numeric(as.numeric(x[tinf])) ) time <- as.numeric(x[tinf])
  #   else time <- as.numeric(as.POSIXct(x[tinf]))
  #   
  #   lhost <<- addHost(lhost, x[receptor])
  #   lhost <<- addHost(lhost, x[source])
  #   
  #   indice <- which(names(lhost) == x[receptor])
  #   if(length(indice) > 0) { lhost[[indice]]@sources[nrow(lhost[[indice]]@sources) +1,] <<- c("time" = time, 'id' = as.character(x[source])) }
  #   
  #   indice <- which(names(lhost) == x[source])
  #   if(length(indice) > 0) { lhost[[indice]]@offsprings[nrow(lhost[[indice]]@offsprings) +1,] <<- c("time" = time, 'id' = as.character(x[receptor])) }
  # })
  # return(lhost)
}

#' loadTreeDF
#' @description load sources and offsprings from a data.frame
#' @param lhost a HostSet
#' @param df a data.frame containing tree data
#' @param source column name for source ID
#' @param receptor column name for receptor ID
#' @param tinf column name for infection Time
#' @param weight infection links probability
#' @return the lhost param update with sources and offsprings
# @examples
#' @export
loadTreeDF <- function(lhost = list(), df=data.frame(), source = "ID-source", receptor = "ID-receptor", tinf = "Tinf", weight = "Weight") {
  
  tree <- df
  
  apply(tree, 1, function(x) {
    
    if( is.StringDate(x[tinf]) ) time <- getTimestamp(x[tinf])
    else time <- as.numeric(x[tinf])

    ##if( !is.na(as.numeric(x[tinf])) && is.numeric(as.numeric(x[tinf])) ) time <- as.numeric(x[tinf])
    ##else time <- getTimestamp(x[tinf])

    if( !is.na(as.double(x[weight])) && as.double(x[weight]) >= 0.0 && as.double(x[weight]) <= 1.0 ) prob <- as.double(x[weight])
    else prob <- 1.0
    
    lhost <<- addHost(lhost, x[receptor])
    lhost <<- addHost(lhost, x[source])
    
    indice <- which(names(lhost) == x[receptor])
    if(length(indice) > 0) { lhost[[indice]]@sources[nrow(lhost[[indice]]@sources) +1,] <<- c("time" = time, 'id' = as.character(x[source]), "prob" = prob) }
    
    indice <- which(names(lhost) == x[source])
    if(length(indice) > 0) { lhost[[indice]]@offsprings[nrow(lhost[[indice]]@offsprings) +1,] <<- c("time" = time, 'id' = as.character(x[receptor]), "prob" = prob) }
  })
  return(lhost)
}

# setMethod(f="addSource", signature=c("Host", "character", "numeric"), function(host,id_source, time) {
#   if( is.numeric(as.numeric(time)) ) {time <- as.numeric(time)}
#   host@sources[length(host@sources)+1,] <- c("time" = time, "id"= id_source)
#   host
# })
# 
# addOffspring <- function(host,id_offspring, time) {
#   if( is.numeric(as.numeric(time)) ) {time <- as.numeric(time)}
#   host@offsprings[length(host@offsprings)+1,] <- c(time,id_offspring)
#   host
# }

#' addViralObs
#' @description load Viral pop observation in Host object
#' @param lhost a HostSet
#' @param lvpop a ViralPopSet
#' @return lhost update with viral population observed
#' @export
addViralObs <- function(lhost, lvpop) {
  
  lapply(1:(length(lvpop)), function(x) {
    indice <- which(names(lhost) == lvpop[[x]]@ID)
    if(length(indice) == 0 ) { warning("Can't find host with ID ",lvpop[[x]]@ID) }
    else { lhost[[indice]]@ID_V_POP[nrow(lhost[[indice]]@ID_V_POP)+1,] <<- c("time" = lvpop[[x]]@time, "id" = x) }
    
  })
  return(lhost)
}

#' loadCoords
#' @description Load Hosts states
#' @param lhost a HostSet
#' @param dfCoords a data.frame with host ID, time and longitude latitude values
#' @param id colname for host ID
#' @return lhost updated
#' @examples
#' path = system.file("extdata", "data-simul/", package="SMITIDstruct")
#' lhost <- list()
#' lhost <- loadTree(lhost,paste(path,"/tree.txt",sep=''))
#' coords <- read.table(file=paste(path,"/hosts_coords.txt",sep=''), header=TRUE, check.names=FALSE)
#' lhost <- loadCoords(lhost,coords)
#' @export
loadCoords <- function(lhost, dfCoords, id="ID") {
        apply(dfCoords,1, FUN=function(l){
            #if( !is.na(as.numeric(l["time"])) && is.numeric(as.numeric(l["time"])) ) time <- as.numeric(l["time"])
            if( is.timestamp(l["time"]) || is.juliendate(l["time"]) ) time <- as.numeric(l["time"])
            else time <- getTimestamp(l["time"])
            lhost[[as.character(l[id])]]@coordinates <<- rbind(lhost[[as.character(l[id])]]@coordinates, st_sf(geom=st_sfc(st_point(c(as.numeric(l["longitude"]), as.numeric(l["latitude"]), time), dim = "XYM"))))
        })
    return(lhost)
}

#' simulateStates
#' @description simulate states from sources infection
#' @param lhost a HostSet
#' @return lhost update with states from sources time ~
#' @export
simulateStates <- function(lhost) {
  
  lapply(1:length(lhost), function(x) {
    
    lhost[[x]]@states[1,] <<- c("time"=0,"value" = "dob")
    for(s in lhost[[x]]@sources$time) {
      lhost[[x]]@states[nrow(lhost[[x]]@states)+1,] <<- c("time"=s[1],"value" = "Inf")
    }
  })
  return(lhost)
}

#' loadStates
#' @description Load Hosts states
#' @param lhost a HostSet
#' @param dfStates a data.frame with host ID and states in columns and time as value
#' @param id colname for host ID
#' @param colStates colnames of States columns
#' @return lhost updated
#' @examples
#' path = system.file("extdata", "data-simul/", package="SMITIDstruct")
#' lhost <- list()
#' class(lhost) <- "hostSet"
#' lhost <- loadTree(lhost,paste(path,"/tree.txt",sep='')) 
#' obs <- read.table(paste(path,"/obs.txt",sep=''),header=TRUE, check.names=FALSE)
#' obs.states <- c(colnames(obs[-grep("ID|Tobs.*",colnames(obs))]))
#' lhost <- loadStates(lhost, obs, colStates=obs.states)
#' @export
loadStates <- function(lhost, dfStates, id="ID", colStates) {
    
    #lsID = unique(dfStates[,id])
    
    for(s in colStates) {
        apply(dfStates,1, FUN=function(l){
            if(!is.na(l[s])) {
                if( is.StringDate(l[s]) ) time <- getTimestamp(l[s])
                else time <- as.numeric(l[s])
                lhost[[as.character(l[id])]]@states <<- rbind(lhost[[as.character(l[id])]]@states, data.frame(time=as.character(time),value=as.character(s), stringsAsFactors = FALSE))
            }
        })
    }
    
    return(lhost)
}

#' setStates
#' @description set hosts states from a data.frame
#' @param lhost a HostSet
#' @param dfStates a data.frame with host ID and states and time in columns
#' @param colStates vector of the columns name, id, time and states
#' @return the HostSet updated
setStates <- function(lhost, dfStates, colStates=c( id="ID",time="time", states="value")) {
    
    #lsID = unique(dfStates$colSates$id)
    
    #for( hid in lsID) {
        #if( !is.na(as.numeric(dfStates$colStates$id[,colStates$time])) && is.numeric(as.numeric(dfStates$colStates$id[,colStates$time])) ) time <- as.numeric(dfStates$colStates$id[,colStates$time])
        #else time <- getTimestamp(dfStates$colStates$id[,colStates$time])
        # TODO modify TIME value in host states for timestamp
        warning("setStates is deprecated")
        #lhost[[as.character(hid)]]@status <- rbind(lhost[[as.character(hid)]]@status, dfStates$colStates$id[,c(colStates$time, colStates$states)])
    #}
    
    return(lhost)
    
}



#' getStates
#' @description get Host(s) states
#' @param lhost a HostSet
#' @param id a vector of host id (default NA : all lhost)
#' @return a data.frame
#' @export
getStates <- function(lhost, id=NA) {
    if(is.na(id)) ids = names(lhost)
    else ids = id
    
    states <- data.frame(matrix(nrow=0,ncol=3))
    names(states) <- c("ID","time","status")
    
    for(idh in ids) {
        df <- lhost[[idh]]@states
        df <- cbind(rep(idh,nrow(df)), df)
        names(df) <- c("ID", "time", "status")
        states <- rbind(states,df)
    }
    
    return(states)
}

#' loadCovs
#' @description Load Hosts covariates
#' @param lhost a HostSet
#' @param dfCovs a data.frame with host ID in rows and covariates in columns
#' @param id colname for host ID
#' @param colCovs colnames of covariates columns
#' @return lhost updated with covariates
#' @export
loadCovs <- function(lhost, dfCovs, id="ID", colCovs) {
    
    lsID = unique(dfCovs[,id])
    
    for(s in colCovs) {
        apply(dfCovs,1, FUN=function(l){ if(!is.na(l[s])) {lhost[[l[id]]]@covariates <<- rbind(lhost[[l[id]]]@covariates, data.frame(time=NA, name=as.character(s),value=as.character(l[s]), stringsAsFactors = FALSE))}})
    }
    
    return(lhost)
}

#' getCov
#' @description get Host(s) covariates
#' @param lhost a HostSet
#' @param id a vector of host id (default NA : all lhost)
#' @return a data.frame
#' @export
getCov <- function(lhost, id=NA) {
    if(is.na(id)[1]) ids = names(lhost)
    else ids = id
    
    covs <- data.frame(matrix(nrow=0,ncol=3))
    names(covs) <- c("ID","time","name","value")
    
    for(idh in ids) {
        df <- lhost[[idh]]@covariates
        df <- cbind(rep(idh,nrow(df)), df)
        names(df) <- c("ID", "time", "name", "value")
        states <- rbind(states,df)
    }
    
    return(states)
    
}

#' getTransmissionTree 
#' @description get a transmission tree as a data.frame
#' @param lhost a hostSet
#' @param id a vector of hosts ids (default NA : all host)
#' @return a data.frame as source|target|time in columns
#' @examples
#' path = system.file("extdata", "data-simul/", package="SMITIDstruct")
#' lhost <- list()
#' lhost <- loadTree(lhost,paste(path,"/tree.txt",sep=''))
#' print(getTransmissionTree(lhost))
#' @export
getTransmissionTree <- function(lhost, id=NA) {
    if(is.na(id)[1]) ids = names(lhost)
    else ids = id
    
    tt <- data.frame(matrix(nrow=0,ncol=4))
    names(tt) <- c("source","target","time","weight")
    
    for(idh in ids) {
        
        dfs <- lhost[[idh]]@sources
        dfs <- cbind(target = rep(idh, nrow(dfs)), dfs)
        names(dfs) <- c("target", "time", "source", "weight")
        tt <- rbind(tt, as.data.frame(dfs[ ,names(tt)]))
        
        dfs <- lhost[[idh]]@offsprings
        dfs <- cbind(source=rep(idh, nrow(dfs)), dfs)
        names(dfs) <- c("source", "time", "target", "weight")
        tt <- rbind(tt, as.data.frame(dfs[ ,names(tt)]))
    }
    
    return(unique(tt))
    
}

#' getTimeLine
#' @description get the time line of an host
#' @param lhost a hostSet
#' @param id a host ID
#' @return a data.frame 
#' @export
getTimeLine <- function(lhost, id) {
    
    tl <- data.frame("level"=character(),"label"=character(),"ID"=character(), "prob"=character(), "timestart"=character(),"timeend"=character(), stringsAsFactors = FALSE)
    
    # top
    if( nrow(lhost[[id]]@sources) > 0 ) apply(lhost[[id]]@sources, MARGIN=1, FUN=function(l) {tl <<- rbind(tl, data.frame(level="top", label="Infected By", ID=l["id"], prob=l["prob"], timestart=l["time"], timeend=NA,stringsAsFactors = FALSE))})
    
    # middle
    if( nrow(lhost[[id]]@covariates) > 0 ) apply(lhost[[id]]@covariates, MARGIN=1, FUN=function(l) { if(!is.na(l["time"])) tl <<- rbind(tl, data.frame(level="middle", label=l["name"], ID=l["value"], prob="", timestart=l["time"], timeend=NA,stringsAsFactors = FALSE))})
    
    timeorder <- order(lhost[[id]]@states$time)
    for( s in seq_len(length(timeorder)) ) {
        if( s+1 <= length(timeorder)) timeend = lhost[[id]]@states[timeorder[s+1],"time"]
        else timeend = "Inf"
        tl <- rbind(tl, data.frame(level="middle", label=lhost[[id]]@states[timeorder[s],"value"], ID=id, prob="", timestart=lhost[[id]]@states[timeorder[s],"time"], timeend=as.character(timeend),stringsAsFactors = FALSE))
    }
    
    #Obs
    if( nrow(lhost[[id]]@ID_V_POP) > 0 ){ apply(lhost[[id]]@ID_V_POP, MARGIN=1, FUN=function(l) {tl <<- rbind(tl, data.frame(level="middle", label="Obs", ID=l["id"], prob="", timestart=l["time"], timeend=NA,stringsAsFactors = FALSE))}) }
    
    # bottom
    if( nrow(lhost[[id]]@offsprings) > 0 ){ apply(lhost[[id]]@offsprings, MARGIN=1, FUN=function(l) {tl <<- rbind(tl, data.frame(level="bottom", label="Offspring", ID=l["id"], prob=l["prob"], timestart=l["time"], timeend=NA,stringsAsFactors = FALSE))}) }
    
    return(tl)
}

#' getDiversity.pDistance
#' @description get pairwise distance of an host over viral population observated
#' @param host an Host object
#' @param lvpop a ViralPopSet object
#' @return a data.frame with col as time of observation and p_distance
#' @export
getDiversity.pDistance <- function(host, lvpop) {
    if(is.null(host) || is.null(lvpop) || length(host) == 0 || length(lvpop) == 0) return(data.frame())
    df <- data.frame(time = character(), p_distance=numeric, stringsAsFactors = FALSE)
    df <- rbind(df,data.frame(time = as.numeric(host@ID_V_POP$time), p_distance = sapply(host@ID_V_POP$id, FUN = function(x){ return( diversity.pDistance(lvpop[[as.numeric(x)]]))}),stringsAsFactors = FALSE))
    return(df)
    
}

#' getDiversity.sfs
#' @description get Allele Frequency Spectrum or Site Frequency spectra for observated viral pop of an host
#' @param host an Host object
#' @param lvpop an ViralPopSet object
#' @return a list indexed by time that contains allele.time and count
#' @export
getDiversity.sfs <- function(host, lvpop) {
    if(is.null(host) || is.null(lvpop) || length(host) == 0 || length(lvpop) == 0) return(list())

    sfs <- as.list(sapply(host@ID_V_POP$id, FUN = function(x){ return(diversity.sfs(lvpop[[as.numeric(x)]]))}, simplify=FALSE))
    names(sfs) <- host@ID_V_POP$time
    
    return(sfs)
}
