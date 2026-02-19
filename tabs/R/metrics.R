#' @importFrom utils tail
#' @importFrom ape rtree 
#' @importFrom dplyr slice_max lag
#'
#' @title Get area in meter
#' 
#' @author Johannes De Groeve
#' @description Get the area based on an object of class tabs or recvect object 
#'
#' @param filename character. Object of class tabs, recvect (SpatVector) or path where outputs of the reconstruct-function were exported. Data exported in the following formats can be read by get_area: directory tree, .qs2 and .rds.
#' @param verbose boolean, print messages
#'
#' @return tabs object 
#'
#' @export
#' 
#' @inherit reconstruct examples
#' 
get_area <- function(filename,verbose=FALSE){
  
  if(is.character(filename)){ # if character, calculate area from an already exported object 
    reconstruction <- import(filename)
    paleov <- reconstruction$recvect
  } else {
    if(inherits(filename,'SpatVectorCollection')){
      paleov <- filename
    } else {
      # if reconstruct
      paleov <- filename$recvect  
    }
    }

paleov_df <- lapply(paleov, data.frame)
paleov_all <- do.call('rbind.data.frame',paleov_df)
unique_rec <- unique(paleov_all[,c('recid','recname','iso')])
isos <- unique(unique_rec$iso)
paleov_all <- split(paleov_all,f=paleov_all$iso)
unique_rec <- split(unique_rec, f=unique_rec$iso)

if(as.numeric(verbose) == 1) {message('3. prepare recarea')}
area_l <- list()
for(j in 1:length(unique_rec)){
    if(as.numeric(verbose) == 1) message('3.', j, '.', ' polygons with iso: ',isos[j])
    if(as.numeric(verbose) == 1) {pb <- pbar(max=length(unique_rec))} # pbar for each iso 
    unique_recid <- unique_rec[[j]]$recid
    unique_period <- unique(paleov_all[[j]]$period)
    #if('area' %in% metrics) {
    area <- matrix(nrow=length(unique_recid),
                   ncol=length(unique_period))
    colnames(area) <- unique_period
    recname_split <- lapply(paleov_all[[j]]$recnames, function(x) jsonlite::fromJSON(x)$id)
    for(i in 1:length(unique_recid)){
      recname_exists <- as.vector(unlist(lapply(recname_split,function(x) sort(grepl(paste0('^',unique_recid[i],'$'),x),decreasing=TRUE)[1])))
      paleov_sub <- paleov_all[[j]][recname_exists,]
      paleov_sub$len <- nchar(paleov_sub$recnames)
      paleov_sub <- paleov_sub[order(paleov_sub$period,paleov_sub$len),]
      paleov_sub <- paleov_sub %>% 
        group_by(.data$period) %>% 
        mutate(id=row_number()) %>% 
        arrange(.data$period,.data$len) %>% 
        filter(.data$id==1) # subset(id==1)
      
      area[i,] <- merge(data.frame(period=unique_period),paleov_sub, by='period',all=TRUE)$area
      
      if(as.numeric(verbose) > 1) message('compute area curve ',unique_rec[[j]][i,'recname'])
      if(as.numeric(verbose) == 1) {update_pbar(pb, i)}
    }
    area <- cbind(unique_rec[[j]],area)
    rownames(area) <- NULL
    area_l[[j]] <- area 
  #}
    if(as.numeric(verbose) == 1){ close(pb) }
}

area <- do.call('rbind.data.frame',area_l)
return(area)
}


#' @title get_tree
#' 
#' @author Johannes De Groeve
#' @description get the tree based on a tabs object or recvect object 
#'
#' @param filename path where files or qs2/rds was exported, reconstruct object of class tabs or recvect object 
#' @param verbose boolean, print messages
#' @param name recname, default NULL will create a tree for every merged polygon
#'
#' @return tbl_tree object 
#' 
#' @note This function is experimental and buggy and may change in future versions.
#' 
#' @noRd
#' @keywords internal
#'
get_tree <- function(filename=NULL,name=NULL,verbose=FALSE){
  
  if(is.character(filename)){ # if character, calculate area from an already exported object 
    reconstruction <- import(filename)
    paleov <- reconstruction$recvect
  } else {
    if(inherits(filename,'SpatVectorCollection')){
      paleov <- filename
    } else {
      # if reconstruct
      paleov <- filename$recvect  
    }
  }
  
  # merge into a single multipolygon
  paleov <- terra::vect(paleov)
  
  # period with maximum size (this is the reference period from where to start the reconstruction)
  size <- paleov %>% 
    as.data.frame() %>%
    dplyr::group_by(.data$period) %>% 
    dplyr::summarize(area=sum(.data$area,na.rm=TRUE)) %>% 
    dplyr::slice_max(order_by=.data$area) %>% 
    dplyr::select("period") %>% 
    unlist() %>% 
    as.vector()
  
  # select recnames and recname only for the maximum extent period
  json <- as.data.frame(paleov)[which(paleov$period == size),] %>%
    dplyr::select("recnames","recname") 
  # if you are only interested in a tree for a specific island 
  if(!is.null(name)){
    data("regions", package = "tabs", envir = environment())
    # use the ascii style to query the island dataset
    name <- lapply(name, function(x) {
        if(length(unique(grepl(paste0('^',stringi::stri_trans_general(str =x, id = "Latin-ASCII"),'$'),regions$name_ascii,ignore.case =TRUE))) > 1){
        names <- regions[grepl(paste0('^',stringi::stri_trans_general(str =x, id = "Latin-ASCII"),'$'),regions$name_ascii,ignore.case =TRUE),'name']
        json <- json[grepl(names,json$recnames,ignore.case =TRUE),]
        } else {
        json <- json[grepl(x,json$recnames,ignore.case =TRUE),]
        }
        }
        )
    json <- unique(do.call(rbind.data.frame,name))
    #json <- json[grepl(name,json$recnames,ignore.case =TRUE),]
  }
  # identify the unique recid and recname per time period within the maximum extent period 
  unique_recid <- lapply(json$recnames, function(x) jsonlite::fromJSON(x)$id)
  names(unique_recid) <- json$recname # add the name of the (paleo) polygon for that period
  unique_recname <- lapply(json$recnames, function(x) jsonlite::fromJSON(x)$name)
  names(unique_recname) <- json$recname
  
  # create an object for the sequence 
  sequence <- unique_recid
  # create an object for the tree 
  trees <- unique_recid
  for(i in 1:length(unique_recid)){ # loop through all the IDs existing in the periods with largest extent 
    names(unique_recid[[i]]) <- unique_recname[[i]] # add the reclabs of the polygons to the id 
    # identify the start and end period of reconstructed polygon labels (recname/recid)
    sequence_l <- lapply(1:length(unique_recid[[i]]), function(x) {
      # all polygons in which a certain recid is present in the JSON (e.g. Korakas; 235088)
      paleov[grepl(unique_recid[[i]][x],paleov$recnames),c('period','recid','recname')] %>% 
        as.data.frame() %>%
        dplyr::arrange(dplyr::desc(.data$period)) %>% 
        dplyr::group_by(.data$recid,.data$recname) %>%
        # define minimum and maximum period before merging into larger polygon
        dplyr::mutate(min=min(as.numeric(gsub('[A-Z]','',.data$period))), 
                      max=max(as.numeric(gsub('[A-Z]','',.data$period))),
                      length=max(as.numeric(gsub('[A-Z]','',.data$period)))-min(as.numeric(gsub('[A-Z]','',.data$period))),
                      group=names(unique_recid)[i], # interconnected polygon group 
                      recid_before_merge=unique_recid[[i]][x], # current label before merging in larger polygon
                      recname_before_merge=names(unique_recid[[i]][x])) %>% # current label before merging in larger polygon
        dplyr::select("group","recid","recid_before_merge","recname","recname_before_merge","min","max","length") %>%
        unique() %>% 
        dplyr::ungroup()
    })
    
    # add the labels to each element in the list  
    names(sequence_l) <- unlist(unique_recname[i])
    # merge, add extra columns for tree-format 
    sequence_l <- do.call(rbind.data.frame,sequence_l) %>% 
      # group by min max and recid and arrange by group/max/min
      dplyr::group_by(.data$min,.data$max,.data$recid) %>%
      dplyr::arrange(.data$group,dplyr::desc(.data$max),dplyr::desc(.data$min)) %>%
      # dplyr::ungroup() %>%
      # dplyr::group_by(group,recid,recname) %>%
      # labeling columns (recname, recid)
      dplyr::mutate(
        # labels of external node polygons
        recid_label=ifelse(.data$recid_before_merge==.data$recid,.data$recid_before_merge, NA),
        recname_label=ifelse(.data$recname_before_merge==.data$recname,.data$recname_before_merge, NA), 
        # merged name to create an ordered list of nodes  
        recid_min_max=paste0(.data$recid,.data$min,.data$max)#, 
        #node=NA,
        #parentA=NA
      ) %>% 
      dplyr::arrange(.data$recid_label) %>%
      #sequence_l <- sequence_l[order(sequence_l$recid_label),] # r
      #sequence_l <- sequence_l %>% 
      dplyr::ungroup() %>%
      # add node ids ordered based on U
      dplyr::mutate(node=as.integer(factor(.data$recid_min_max, levels = unique(.data$recid_min_max)))) %>%
      # order by group, max, min
      dplyr::arrange(.data$group,dplyr::desc(.data$max),dplyr::desc(.data$min)) %>% 
      dplyr::ungroup() %>% 
      # group and mutate by recid to define the parentA using the lag (previous node within the group)
      dplyr::group_by(.data$group,.data$recid,.data$recname) %>% 
      dplyr::mutate(parentA=dplyr::lag(.data$node),
                    n=dplyr::n()) %>% 
      # if there parent is an NA and the group has the same name as the recname, the node and parent are the same
      dplyr::mutate(parentA=ifelse(.data$group==.data$recname & is.na(.data$parentA),.data$node,.data$parentA)) %>%
      dplyr::ungroup() %>% 
      # define groups based on the recid using the order group / max / min 
      dplyr::mutate(groups=as.integer(factor(.data$recid, levels = unique(.data$recid))))
    
    # Define an extended compact function
    compact <- function(x) {
      Filter(function(y) !is.null(y[1]) && !is.na(y[1]) && y[1] != FALSE && y[1] != 0, x)
    }
    
    g <- split(sequence_l, sequence_l$groups) # split based on this ordered list
    # fill the missing parent labels
    if(length(g) > 1){
      n <- unlist(lapply(g,function(x) x$n[1] > 1))

      for(j in length(g):2){
        if(as.numeric(verbose) > 1) { message(j) }
        m <- g[n]
        parent <- g[[j]][is.na(g[[j]]$parentA),]
        r <- compact(lapply(m, function(x) {
          r <- x[x$recid_before_merge %in% parent$recid,]
          if(nrow(r)==0){r <- NULL}
          return(r)}))
        m <- m[names(r)][names(r) != names(g)[j] ]
        m <- tail(m,1)
        #if(length(m) != 0){
        g[[j]][is.na(g[[j]]$parentA),]$parentA <- dplyr::left_join(parent, m[[1]], by = c("recid" = "recid_before_merge"))$node.y
        #} else {
        #g <- g  
        #}
      }
    }
    
    # merge again and remove those rows which have multiple min/max/recids 
    sequence_l<- do.call(rbind.data.frame, g) %>% 
      dplyr::group_by(.data$min,.data$max,.data$recid) %>%
      dplyr::slice_head(n=1) %>% # something is wrong here cause some branches are removed for i = 1
      dplyr::arrange(.data$group,dplyr::desc(.data$max),dplyr::desc(.data$min)) %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(.data$group,.data$recid,.data$recname) %>% 
      dplyr::mutate(branch.lengthA=ifelse(is.na(dplyr::lag(.data$length)), .data$length,.data$length - dplyr::lag(.data$length))) %>%
      dplyr::ungroup() 
    
    # merge with tree structure and format correctly 
    
    # check whether a tree can be created (> 1 polygon)
    true_tree <- nrow(sequence_l) > 1
    
    # create the structure for the tree 
    if(true_tree) { # if there is more than one polygon / element in the tree
      tree <- ape::rtree(length(unique_recid[[i]]))
    } else { # else, create a fictional second branch
      tree <- ape::rtree(length(unique_recid[[i]])+1)
    }
    tree_format <- as_tibble.phylo(tree)
    sequence_l <- dplyr::left_join(tree_format,sequence_l, by='node')
    if(!true_tree){
      sequence_l <- sequence_l %>% 
        mutate(branch.lengthA=ifelse(.data$node==2,0, .data$branch.lengthA),parentA=3)
    }
    
    # New column names as a named vector
    new_names <- c('parentA','branch.lengthA','labelA','label','parent','branch.length')
    # Old column names to be replaced
    old_names <- c('parent','branch.length','label','recname_label','parentA','branch.lengthA')
    # Replace the old names with new names
    colnames(sequence_l)[match(old_names, colnames(sequence_l))] <- new_names
    # remove branches with NA's but only in true trees
    if(true_tree){
      sequence_l <- sequence_l[!is.na(sequence_l$branch.length),]
    }
    # reorder the columns 
    sequence_l <- sequence_l %>% 
      dplyr::select("parent","node","branch.length","label", dplyr::everything())
    # drop unimportant columns 
    sequence_l$parentA <- NULL; sequence_l$labelA <- NULL; sequence_l$branch.lengthA <- NULL
    #trees[[i]] <- tidytree::as.phylo(sequence_l)
    sequence[[i]] <- sequence_l
  }
  #if(length(sequence) == 1){unlist(sequence)}
  return(sequence)
}



# 
# library(tidytree)
# 
# # computing area based on tabs object, recvect object or an exported dataset
# 
# # load data samples
# sporades <- sporades()
# topo <- sporades$topo
# correction <- sporades$correction
# curve <- sporades$curve
# labs <- sporades$labs
# 
# dir <- tempdir() # export to temporary directory
# 
# rec <- reconstruct(topo=topo,
#                    region=labs,
#                    curve=curve,
#                    correction=correction,
#                    reclabs='name',
#                    filename=paste0(dir,'/sporades.qs2'),
#                    overwrite=TRUE)
# 
# tree <- get_tree(rec)
# tree <- get_tree(rec$recvect)
# tree <- get_tree(paste0(dir,'/sporades.qs2'))
# 
# # lapply(tree,function(x) plot(tidytree::as.phylo(x)))
# 
# # create tree for a single polygon (keeps all relations)
# tree <- get_tree(rec,name='Nisi Alonnisos')
# # plot(tidytree::as.phylo(tree[[1]]))
# 
# tree <- get_tree(paste0(dir,'/sporades.qs2'),
#                  name=c('Nisi Alonnisos','Nisida Praso'))
# # lapply(tree,function(x) plot(tidytree::as.phylo(x)))


# 
# # LIST ISLANDNAMES
# # reference islands intersecting a paleopolygon (present day existing islands)
# # paleo islands intersecting a paleopolygon
# list_islands <- function(reconstruction, reference){
#   if(reference){
#     pv <- lapply(reconstruction[[1]]$islandnstr, function(x) )
#   } else {
#     pv <- reconstruction[[1]][[i]]$paleoislandstr
#   }
# }
# 
# # islands = vector of islands to calculate metrics for
# metrics <- function(reconstruction, filename='~', islands=NULL, metrics=c('distance','distance from continent','time since isolation')){
# 
#   # INPUT DATA
#   if(is.character(reconstruction)){ # if reconstruction is loaded from computer
#     paleov <- rast(list.files(pattern='VEC',path=reconstruction))
#   } else { # reconstruct
#     paleov <- reconstruction[[1]]
#   }
# 
#   # SELECT ISLANDS OF INTEREST
#   if(is.null(islands)){
#     # bla bla
#   }
#   # DISTANCE BETWEEN ISLANDS - CAN BE DONE IN PARALLEL
#   if ('distance' %in% metrics){
#     tdir <-tempfile("METRICS_")
#     for(i in 1:length(paleov)){
#       pv <- paleov[[i]]
#       pv <- project(pv, '+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
#       d <- round(as.matrix(terra::distance(pv,symmetrical=FALSE)))
#       colnames(d) <- pv$paleoname
#       rownames(d) <- pv$paleoname
#       write.csv(as.data.frame(d),paste0(tdir, '/D_ISL',vecn,'.csv'))
#     }
#     zip(zipfile=paste0(filename, "_reconstruction_dist.zip"),
#         files=list.files(tdir,full.names =TRUE),
#         flags = '-r9Xj')
#   }
#   # DISTANCE FROM CONTINENT - CAN BE DONE IN PARALLEL
# 
# 
# 
# 
# }
# 
# 
