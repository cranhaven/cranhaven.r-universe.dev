#' Subset beam.stats Result
#'
#' Filter the beam.stats object from compute_beam_stats with various filtering criteria. Default is to filter to top 50 sets with smallest q-value. At least one filtering criteria must be specified. Can also use intersection or union of multiple criteria.
#'
#' @param beam.result A beam.stats object from compute_beam_stats
#' @param beam.set.pvals A list containing BEAMR set p-values from compute_set_pvalues; required if p.limit or q.limit are specified.
#' @param beam.feat.pvals A list containing feature-level p-values from compute_feature_pvalues; required if p.feat.limit or q.feat.limit are specified.
#' @param mtx.rows A list of vectors of feature names corresponding to row.id in set.data. List names correspond to mtx.id in set.data. If specified, filter to all sets containing at least one of these features.
#' @param set.ids A character vector of set.ids. If specified, filter to these sets.
#' @param endpts A character vector of endpoint names. If specified, filter to sets that correspond to these endpoints.
#' @param omics A character vector of omics names. If specified, fitler to sets that correspond to these omics.
#' @param p.limit A numeric value. If specified, determine mtx.rows that are below this threshold if p<1 or top p sets if p>1.
#' @param q.limit A numeric value. If specified, determine mtx.rows that are below this threshold if q <1 or top q sets if q>1.
#' @param p.feat.limit A numeric value. If specified, determine mtx.rows that are below this threshold if p.feat<1 or top p.feat sets if p.feat>1 (feature p-values).
#' @param q.feat.limit A numeric value. If specified, determine mtx.rows that are below this threshold if q.feat<1 or top q.feat sets if q.feat>1.
#' @param intersect A logical value. Default is TRUE. If TRUE, use intersection of all specified criteria. If FALSE use union of all specified criteria.
#' @param recalc A logical value. Default is FALSE. If TRUE, recalculate p-values. If FALSE use original set p-values..
#'
#' @returns A list with filtered beam.stats object, updated beam.set.pvals, and filtered beam.feat.pvals.
#' @importFrom dplyr inner_join
#' @importFrom dplyr full_join
#' @importFrom purrr reduce
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' data(beam_stats)
#' test.pvals <- compute_set_pvalues(beam.stats=beam_stats)
#' test.feat.pvals <- compute_feature_pvalues(beam.stats=beam_stats)
#' filt.beam.stats <- subset_beam_result(beam_stats, test.pvals, test.feat.pvals,
#'                                       endpts=c("EFS","OS"), q.limit=10, intersect=TRUE,
#'                                       recalc=FALSE)
subset_beam_result <- function(beam.result, beam.set.pvals=NULL, beam.feat.pvals=NULL,
                               mtx.rows=NULL, set.ids=NULL, endpts=NULL, omics=NULL,
                               p.limit=NULL, q.limit=NULL, p.feat.limit=NULL,
                               q.feat.limit=NULL, intersect=TRUE, recalc=FALSE)
{
  if(all(is.null(c(mtx.rows, set.ids, endpts, omics, p.limit, q.limit, p.feat.limit, q.feat.limit)))){
    q.limit=50
    message("You must specify a filtering criteria! Subsetting for default q.limit=50.")
  }
  res.mtx.row <- NA
  res.set.id <- NA
  res.endpts <- NA
  res.omics <- NA
  res.p <- NA
  res.q <- NA
  res.feat.p <- NA
  res.feat.q <- NA
  if(!is.null(mtx.rows)){
    res.mtx.row <- filter_mtx_rows(beam.stats=beam.result, mtx.rows.list=mtx.rows)
  }
  if(!is.null(set.ids)){
    res.set.id <- filter_setids(beam.stats=beam.result, set.ids=set.ids)
  }
  if(!is.null(endpts)){
    res.endpts <- filter_endpts(beam.stats=beam.result, endpts=endpts)
  }
  if(!is.null(omics)){
    res.omics <- filter_omics(beam.stats=beam.result, omics=omics)
  }
  if(!is.null(p.limit)){
    res.p <- filter_pval(beam.stats=beam.result, beam.pvalues=beam.set.pvals, p.limit=p.limit)
  }
  if(!is.null(q.limit)){
    res.q <- filter_qval(beam.stats=beam.result, beam.pvalues=beam.set.pvals, q.limit=q.limit)
  }
  if(!is.null(p.feat.limit)){
    if(is.null(beam.feat.pvals))
      stop("You must specify beam.feat.pvals to filter with p.feat.limit!")
    res.feat.p <- filter_feat_pval(beam.stats=beam.result, feat.pvalues=beam.feat.pvals, p.feat.limit=p.feat.limit)
  }
  if(!is.null(q.feat.limit)){
    if(is.null(beam.feat.pvals))
      stop("You must specify beam.feat.pvals to filter with q.feat.limit!")
    res.feat.q <- filter_feat_qval(beam.stats=beam.result, feat.pvalues=beam.feat.pvals, q.feat.limit=q.feat.limit)
  }
  res.list <- list(res.mtx.row, res.set.id, res.endpts, res.omics, res.p, res.q, res.feat.p, res.feat.q)
  n.res <- length(which(!is.na(res.list)))
  if(n.res==1){
    beam.stat.res <- res.list[[which(!is.na(res.list))]]
    if(recalc){
      beam.set.pvals.res <- compute_set_pvalues(beam.stat.res)
    }
    else{
      beam.set.pvals.res <- beam.set.pvals
      beam.set.pvals.res$set.pvals <- beam.set.pvals$set.pvals[which(beam.set.pvals$set.pvals$set.id %in% beam.stat.res$beam.data$set.data$set.id),]
      beam.set.pvals.res$set.mtch <- beam.set.pvals$set.mtch[which(beam.set.pvals$set.mtch$set.id %in% beam.stat.res$beam.data$set.data$set.id),]
    }
    beam.feat.pvals.res.test <- try(compute_feature_pvalues(beam.stat.res), silent=TRUE)
    if(inherits(beam.feat.pvals.res.test, "try-error")){
      beam.feat.pvals.res <- NULL
    }
    else{
      beam.feat.pvals.res <- compute_feature_pvalues(beam.stat.res)
    }
    res.list <- list(beam.stat.res, beam.set.pvals.res, beam.feat.pvals.res)
    names(res.list) <- c("beam.stats", "beam.set.pvals", "beam.feat.pvals")
    return(res.list)
  }
  else{
    res.ids <- which(!is.na(res.list))
    res.list.filt <- res.list[res.ids]
    set.list.filt <- list()
    for(m in 1:length(res.list.filt)){
      res.temp <- res.list.filt[[m]]
      set.temp <- res.temp$beam.data$set.data
      set.list.filt[[m]] <- set.temp
    }
    if(intersect){
      set.int <- set.list.filt %>% reduce(inner_join) # this is missing endpoint information
      beam.stat.res <- filter_set_data_rows(beam.stats=beam.result, set.data.rows=set.int)
      # Modify beam.specs to keep only things in res.list.filt
      beam.specs.old <- beam.stat.res$beam.specs
      filt.specs.name <- c()
      for(j in 1:length(res.list.filt)){
        beam.specs.temp <- res.list.filt[[j]]$beam.specs
        filt.specs.name <- c(filt.specs.name, beam.specs.temp$name)
      }
      # Intersect - only keep beam.specs in all res.list.filt
      dup.filt.specs.names <- filt.specs.name[which(duplicated(filt.specs.name))]
      beam.specs.new <- beam.specs.old[which(beam.specs.old$name %in% dup.filt.specs.names),]
      beam.stat.res$beam.specs <- beam.specs.new
      # Filter the actual beam.stats to keep only those in beam.specs.new
      beam.stat.res$beam.stats <- beam.stat.res$beam.stats[which(names(beam.stat.res$beam.stats) %in% beam.specs.new$name)]
      if(recalc){
        beam.set.pvals.res <- compute_set_pvalues(beam.stat.res)
      }
      else{
        beam.set.pvals.res <- beam.set.pvals
        beam.set.pvals.res$set.pvals <- beam.set.pvals$set.pvals[which(beam.set.pvals$set.pvals$set.id %in% beam.stat.res$beam.data$set.data$set.id),]
        beam.set.pvals.res$set.mtch <- beam.set.pvals$set.mtch[which(beam.set.pvals$set.mtch$set.id %in% beam.stat.res$beam.data$set.data$set.id),]
      }
      beam.feat.pvals.res.test <- try(compute_feature_pvalues(beam.stat.res), silent=TRUE)
      if(inherits(beam.feat.pvals.res.test, "try-error")){
        beam.feat.pvals.res <- NULL
      }
      else{
        beam.feat.pvals.res <- compute_feature_pvalues(beam.stat.res)
      }
      res.list <- list(beam.stat.res, beam.set.pvals.res, beam.feat.pvals.res)
      names(res.list) <- c("beam.stats", "beam.set.pvals", "beam.feat.pvals")
      return(res.list)
    }
    else{
      set.uni <- set.list.filt %>% reduce(full_join)
      beam.stat.res <- filter_set_data_rows(beam.stats=beam.result, set.data.rows=set.uni)
      # Modify beam.specs to keep only things in res.list.filt
      beam.specs.old <- beam.stat.res$beam.specs
      filt.specs.name <- c()
      for(j in 1:length(res.list.filt)){
        beam.specs.temp <- res.list.filt[[j]]$beam.specs
        filt.specs.name <- c(filt.specs.name, beam.specs.temp$name)
      }
      beam.specs.new <- beam.specs.old[which(beam.specs.old$name %in% unique(filt.specs.name)),]
      beam.stat.res$beam.specs <- beam.specs.new
      beam.stat.res$beam.stats <- beam.stat.res$beam.stats[which(names(beam.stat.res$beam.stats) %in% beam.specs.new$name)]
      if(recalc){
        beam.set.pvals.res <- compute_set_pvalues(beam.stat.res)
      }
      else{
        beam.set.pvals.res <- beam.set.pvals
        beam.set.pvals.res$set.pvals <- beam.set.pvals$set.pvals[which(beam.set.pvals$set.pvals$set.id %in% beam.stat.res$beam.data$set.data$set.id),]
        beam.set.pvals.res$set.mtch <- beam.set.pvals$set.mtch[which(beam.set.pvals$set.mtch$set.id %in% beam.stat.res$beam.data$set.data$set.id),]
      }
      beam.feat.pvals.res.test <- try(compute_feature_pvalues(beam.stat.res), silent=TRUE)
      if(inherits(beam.feat.pvals.res.test, "try-error")){
        beam.feat.pvals.res <- NULL
      }
      else{
        beam.feat.pvals.res <- compute_feature_pvalues(beam.stat.res)
      }
      res.list <- list(beam.stat.res, beam.set.pvals.res, beam.feat.pvals.res)
      names(res.list) <- c("beam.stats", "beam.set.pvals", "beam.feat.pvals")
      return(res.list)
    }
  }


  # return list with
  #   filtered beam.stats
  #   filtered beam.data
  #   filtered set.pvals
  #   filtered feature.pvals
}

#######################################
# Filter beam.stats by q.feat.limit
filter_feat_qval <- function(beam.stats, feat.pvalues, q.feat.limit){
  if(q.feat.limit < 0){
    stop(paste0("Your q.feat.limit, ", q.feat.limit, ", is not valid. It must be greater than 0."))
  }
  # Check the limit - need to check within the feat.pvalues list
  # Filter feat.pvalues
  # Generate mtx.rows
  # Get filtered beam.stats using filter_mtx_rows
  set.data <- beam.stats$beam.data$set.data
  feat.pvals.df <- data.frame()
  for(k in 1:length(feat.pvalues)){
    feat.pvals <- feat.pvalues[[k]]
    feat.pvals$analysis.name <- names(feat.pvalues)[k]
    feat.pvals.df <- rbind.data.frame(feat.pvals.df, feat.pvals)
  }
  if(q.feat.limit < 1 & q.feat.limit > 0){
    if(q.feat.limit < min(feat.pvals.df$q))
      stop(paste0("Your q.feat.limit, ", q.feat.limit, ", is smaller than the minimum feature-level q-value, ", min(feat.pvals.df$q), ". Please choose a larger value."))
    feat.pvals.filt <- feat.pvals.df[which(feat.pvals.df$q <= q.feat.limit),]
    row.id.filt <- feat.pvals.filt$id
    mtx.rows <- set.data[which(set.data$row.id %in% row.id.filt),,drop=FALSE]
    res <- filter_set_data_rows(beam.stats=beam.stats, set.data.rows=mtx.rows)
  }
  if(q.feat.limit >= 1){
    feat.pvals.ord <- feat.pvals.df[order(feat.pvals.df$q, feat.pvals.df$p, abs(feat.pvals.df$beta)),]
    n.p <- nrow(feat.pvals.ord)
    if(q.feat.limit >= n.p){
      stop(paste0("Your q.feat.limit, ", q.feat.limit, ", is larger than or equal to the number of feature q-values, ", n.p, ". No filtering needed."))
    }
    feat.pvals.ord.filt <- feat.pvals.ord[1:q.feat.limit,]
    row.id.filt <- feat.pvals.ord.filt$id
    mtx.rows <- set.data[which(set.data$row.id %in% row.id.filt), ,drop=FALSE]
    res <- filter_set_data_rows(beam.stats=beam.stats, set.data.rows=mtx.rows)
  }
  return(res)
}

#######################################
# Filter beam.stats by p.feat.limit
filter_feat_pval <- function(beam.stats, feat.pvalues, p.feat.limit){
  if(p.feat.limit < 0){
    stop(paste0("Your p.feat.limit, ", p.feat.limit, ", is not valid. It must be greater than 0."))
  }
  # Check the limit - need to check within the feat.pvalues list
  # Filter feat.pvalues
  # Generate mtx.rows
  # Get filtered beam.stats using filter_mtx_rows
  set.data <- beam.stats$beam.data$set.data
  feat.pvals.df <- data.frame()
  for(k in 1:length(feat.pvalues)){
    feat.pvals <- feat.pvalues[[k]]
    feat.pvals$analysis.name <- names(feat.pvalues)[k]
    feat.pvals.df <- rbind.data.frame(feat.pvals.df, feat.pvals)
  }
  if(p.feat.limit < 1 & p.feat.limit > 0){
    if(p.feat.limit < min(feat.pvals.df$p))
      stop(paste0("Your p.feat.limit, ", p.feat.limit, ", is smaller than the minimum feature-level p-value, ", min(feat.pvals.df$p), ". Please choose a larger value."))
    feat.pvals.filt <- feat.pvals.df[which(feat.pvals.df$p <= p.feat.limit),]
    row.id.filt <- feat.pvals.filt$id
    mtx.rows <- set.data[which(set.data$row.id %in% row.id.filt), ,drop=FALSE]
    res <- filter_set_data_rows(beam.stats=beam.stats, set.data.rows=mtx.rows)
  }
  if(p.feat.limit >= 1){
    feat.pvals.ord <- feat.pvals.df[order(feat.pvals.df$p, abs(feat.pvals.df$beta)),]
    n.p <- nrow(feat.pvals.ord)
    if(p.feat.limit >= n.p){
      stop(paste0("Your p.feat.limit, ", p.feat.limit, ", is larger than or equal to the number of feature p-values, ", n.p, ". No filtering needed."))
    }
    feat.pvals.ord.filt <- feat.pvals.ord[1:p.feat.limit,]
    row.id.filt <- feat.pvals.ord.filt$id
    mtx.rows <- set.data[which(set.data$row.id %in% row.id.filt), ,drop=FALSE]
    res <- filter_set_data_rows(beam.stats=beam.stats, set.data.rows=mtx.rows)
  }
  return(res)
}

#######################################
# Filter beam.stats by q.limit
filter_qval <- function(beam.stats, beam.pvalues, q.limit){
  set.pvals <- beam.pvalues$set.pvals
  set.data <- beam.stats$beam.data$set.data
  if(q.limit < 1 & q.limit > 0){
    if(q.limit < min(set.pvals$q.set))
      stop(paste0("Your q.limit, ", q.limit, ", is smaller than the minimum set q-value, ", min(set.pvals$q.set), ". Please choose a larger value."))
    set.pvals.filt <- set.pvals[which(set.pvals$q.set <= q.limit),]
    set.id.filt <- set.pvals.filt$set.id
    mtx.rows <- set.data[which(set.data$set.id %in% set.id.filt),,drop=FALSE]
    res <- filter_set_data_rows(beam.stats=beam.stats, set.data.rows=mtx.rows)
  }
  if(q.limit >= 1){
    set.pvals.ord <- set.pvals[order(set.pvals$q.set, set.pvals$p.set, set.pvals$distance.ratio),]
    n.p <- nrow(set.pvals.ord)
    if(q.limit >= n.p){
      stop(paste0("Your q.limit, ", q.limit, ", is larger than or equal to the number of set q-values, ", n.p, ". No filtering needed."))
    }
    set.pvals.ord.filt <- set.pvals.ord[1:q.limit,]
    set.id.filt <- set.pvals.ord.filt$set.id
    mtx.rows <- set.data[which(set.data$set.id %in% set.id.filt),,drop=FALSE]
    res <- filter_set_data_rows(beam.stats=beam.stats, set.data.rows=mtx.rows)
  }
  if(q.limit < 0)
    stop(paste0("Your q.limit, ", q.limit, ", is not valid. It must be greater than 0."))
  return(res)
}

#######################################
# Filter beam.stats by p.limit
filter_pval <- function(beam.stats, beam.pvalues, p.limit){
  set.pvals <- beam.pvalues$set.pvals
  set.data <- beam.stats$beam.data$set.data
  if(p.limit < 1 & p.limit > 0){
    if(p.limit < min(set.pvals$p.set))
      stop(paste0("Your p.limit, ", p.limit, ", is smaller than the minimum set p-value, ", min(set.pvals$p.set), ". Please choose a larger value."))
    set.pvals.filt <- set.pvals[which(set.pvals$p.set <= p.limit),]
    set.id.filt <- set.pvals.filt$set.id
    mtx.rows <- set.data[which(set.data$set.id %in% set.id.filt),,drop=FALSE]
    res <- filter_set_data_rows(beam.stats=beam.stats, set.data.rows=mtx.rows)
  }
  if(p.limit >= 1){
    set.pvals.ord <- set.pvals[order(set.pvals$p.set, set.pvals$distance.ratio),]
    n.p <- nrow(set.pvals.ord)
    if(p.limit >= n.p){
      stop(paste0("Your p.limit, ", p.limit, ", is larger than or equal to the number of set p-values, ", n.p, ". No filtering needed."))
    }
    set.pvals.ord.filt <- set.pvals.ord[1:p.limit,]
    set.id.filt <- set.pvals.ord.filt$set.id
    mtx.rows <- set.data[which(set.data$set.id %in% set.id.filt),,drop=FALSE]
    res <- filter_set_data_rows(beam.stats=beam.stats, set.data.rows=mtx.rows)
  }
  if(p.limit < 0)
    stop(paste0("Your p.limit, ", p.limit, ", is not valid. It must be greater than 0."))
  return(res)
}

#######################################
# Filter beam.stats by omics
filter_omics <- function(beam.stats, omics){
  # Check that omics are in the beam.stats data
  mtx <- beam.stats$beam.specs$mtx
  om.mtch <- which(mtx %in% omics)
  if(length(om.mtch)==0)
    stop(paste0("Omic ", omics, " not found in beam.stats. \n  "))
  if(any(!(omics %in% mtx)))
    warning(paste0("Omic ", omics[which(!(omics %in% mtx))], " not found in beam.stats. \n  "))
  # Filter set.data then call filter_mtx_rows
  mtx.rows <- beam.stats$beam.data$set.data[which(beam.stats$beam.data$set.data$mtx.id %in% omics),,drop=FALSE]
  res <- filter_set_data_rows(beam.stats=beam.stats, set.data.rows=mtx.rows)
}

#######################################
# Filter beam.stats by endpts
filter_endpts <- function(beam.stats, endpts){
  # check if endpts are in the beam.stats data
  name <- beam.stats$beam.specs$name
  eps.list <- strsplit(name, "\\.")
  eps.df <- do.call(rbind.data.frame, eps.list)
  colnames(eps.df) <- c("Omic", "Endpoint")
  ep.mtch <- which(eps.df$Endpoint %in% endpts)
  if(length(ep.mtch)==0)
    stop(paste0("Endpoint ", endpts, " not found in beam.stats. \n  "))
  if(any(!(endpts %in% eps.df$Endpoint)))
    warning(paste0("Endpoint ", endpts[which(!(endpts %in% eps.df$Endpoint))], " not found in beam.stats. \n  "))
  # Filter beam.specs
  beam.specs.filt <- beam.stats$beam.specs[ep.mtch,]
  beam.stats.filt <- beam.stats$beam.stats[which(names(beam.stats$beam.stats) %in% beam.specs.filt$name)]
  res <- list(beam.stats.filt, beam.specs.filt, beam.stats$beam.data)
  names(res) <- c("beam.stats", "beam.specs", "beam.data")
  class(res) <- "beam.stats"
  return(res)
}

######################################
# Filter beam.stats by set.ids

filter_setids <- function(beam.stats, set.ids){
  # Check if set.ids are in the beam.stats data
  set.mtch=which(beam.stats$beam.data$set.data$set.id %in% set.ids) # extract rows corresponding to the set.id
  if (length(set.mtch)==0)
    stop(paste0("set.ids ", set.ids," not found.")) # stop if set.id not found
  if(length(set.mtch) < length(set.ids))
    message(paste0("set.ids ", set.ids[-which(set.ids %in% beam.stats$beam.data$set.data$set.id)]), " not found and will be excluded.")
  # Convert to mtx.rows
  set.mtx.rows <- beam.stats$beam.data$set.data[set.mtch,,drop=FALSE]
  res <- filter_set_data_rows(beam.stats=beam.stats, set.data.rows=set.mtx.rows)
  return(res)
}

#####################################
# Filter beam.stats by mtx.rows
# mtx.rows = list of vectors of feature names, list names correspond to mtx.id names in beam.data$set.data
filter_mtx_rows <- function(beam.stats,mtx.rows.list){
  # convert mtx.rows to data.frame
  mtx.rows <- list_to_df(mtx.rows.list)
  beam.stats.l <- beam.stats$beam.stats
  beam.data <- beam.stats$beam.data
  beam.specs <- beam.stats$beam.specs
  set.data <- beam.data$set.data
  set.data$mtx.row <- paste(set.data$mtx.id, set.data$row.id, sep=",")
  mtx.rows$mtx.row <- paste(mtx.rows$mtx.id, mtx.rows$row.id, sep=",")
  # Test to make sure mtx.rows are in beam.stats
  set.mtch<-which(set.data$mtx.row %in% mtx.rows$mtx.row) # extract rows corresponding to the set.id
  if(length(set.mtch)==0)
    stop(paste0("Not all mtx.rows present in set.data \n", mtx.rows)) # stop if mtx.rows not found
  mtch.set <- beam.data$set.data[set.mtch,,drop=FALSE] # extract relevant set.data with set.id, mtx.id, row.id columns

  # Filter beam.stats, beam.specs, and beam.data
  #n.set.rows<-length(mtch.set$set.id) # number of rows in set (number of omics types available?)
  un.mtx <- unique(mtch.set$mtx.id) # number of unique mtx.id's
  beam.specs.filt <- beam.specs[which(beam.specs$mtx %in% un.mtx),,drop=FALSE]
  # Initialize beam.stats.filt
  beam.stats.filt <- vector("list",length(beam.stats.l))
  names(beam.stats.filt) <- names(beam.stats.l)
  # Initialize filtered beam.data objects
  mtx.data.filt <- vector("list", length(beam.data$mtx.data))
  names(mtx.data.filt) <- names(beam.data$mtx.data)
  mtx.anns.filt <- vector("list", length(beam.data$mtx.anns))
  names(mtx.anns.filt) <- names(beam.data$mtx.data)
  for(i in 1:length(un.mtx)){
    name.filt <- beam.specs[which(un.mtx[i]==beam.specs$mtx),c("name")]
    row.ids <- mtx.rows[which(mtx.rows$mtx.id==un.mtx[i]),c("row.id")]
    # filter beam.stats
    for(j in 1:length(name.filt)){
      beam.stats.filt[[name.filt[j]]] <- as.data.frame(beam.stats.l[[name.filt[j]]][which(rownames(beam.stats.l[[name.filt[j]]])%in%row.ids),,drop=FALSE])
    }
    beam.stats.filt <- beam.stats.filt[which(names(beam.stats.filt) %in% beam.specs.filt$name)]
    # filter beam.data
    mtx.name <- un.mtx[i]
    # filter beam.data$mtx.data
    mtx.data.filt[[mtx.name]] <- beam.data$mtx.data[[mtx.name]][which(rownames(beam.data$mtx.data[[mtx.name]]) %in% row.ids),,drop=FALSE]
    # filter beam.data$mtx.anns
    mtx.anns.filt[[mtx.name]] <- beam.data$mtx.anns[[mtx.name]][which(beam.data$mtx.anns[[mtx.name]]$id %in% row.ids),,drop=FALSE]
  }
  beam.data.filt <- beam.data
  beam.data.filt$mtx.data <- mtx.data.filt
  beam.data.filt$mtx.anns <- mtx.anns.filt
  beam.data.filt$set.data <- mtch.set
  beam.data.filt$anns.mtch <- match_anns(mtx.anns.filt, mtx.data.filt)


  # Package results as beam.stats object
  res <- list(beam.stats=beam.stats.filt,
              beam.specs=beam.specs.filt,
              beam.data=beam.data.filt)
  class(res) <- "beam.stats"

  return(res)
}

##################################################
# Convert list entry to set.data type data.frame
list_to_df <- function(mtx.list){
  list.names <- names(mtx.list)
  df.temp <- data.frame()
  for(i in 1:length(mtx.list)){
    df.entry <- cbind.data.frame(rep(list.names[[i]], times=length(mtx.list[[i]])), mtx.list[[i]])
    df.temp <- rbind.data.frame(df.temp, df.entry)
  }
  colnames(df.temp) <- c("mtx.id", "row.id")
  res <- df.temp
  return(res)
}

#####################################
# Filter beam.stats by set.data row: set.id, mtx.id, row.id
#set.data.rows <- beam.stats$beam.data$set.data[1:5,]
filter_set_data_rows <- function(beam.stats,set.data.rows){
  beam.stats.l <- beam.stats$beam.stats
  beam.data <- beam.stats$beam.data
  beam.specs <- beam.stats$beam.specs
  set.data <- beam.data$set.data
  set.data$set.row <- paste(set.data$set.id, set.data$mtx.id, set.data$row.id, sep=",")
  set.data.rows$set.row <- paste(set.data.rows$set.id, set.data.rows$mtx.id, set.data.rows$row.id, sep=",")
  # Test to make sure mtx.rows are in beam.stats
  set.mtch<-which(set.data$set.row %in% set.data.rows$set.row) # extract rows corresponding to the set.id
  if(length(set.mtch)==0)
    stop(paste0("Not all mtx.rows present in set.data \n", set.data.rows)) # stop if mtx.rows not found
  mtch.set <- beam.data$set.data[set.mtch,,drop=FALSE] # extract relevant set.data with set.id, mtx.id, row.id columns

  # Filter beam.stats, beam.specs, and beam.data
  #n.set.rows<-length(mtch.set$set.id) # number of rows in set (number of omics types available?)
  un.mtx <- unique(mtch.set$mtx.id) # number of unique mtx.id's
  beam.specs.filt <- beam.specs[which(beam.specs$mtx %in% un.mtx),,drop=FALSE]
  # Initialize beam.stats.filt
  beam.stats.filt <- vector("list",length(beam.stats.l))
  names(beam.stats.filt) <- names(beam.stats.l)
  # Initialize filtered beam.data objects
  mtx.data.filt <- vector("list", length(beam.data$mtx.data))
  names(mtx.data.filt) <- names(beam.data$mtx.data)
  mtx.anns.filt <- vector("list", length(beam.data$mtx.anns))
  names(mtx.anns.filt) <- names(beam.data$mtx.data)
  for(i in 1:length(un.mtx)){
    name.filt <- beam.specs[which(un.mtx[i]==beam.specs$mtx),c("name")]
    row.ids <- set.data.rows[which(set.data.rows$mtx.id==un.mtx[i]),c("row.id")]
    # filter beam.stats
    for(j in 1:length(name.filt)){
      beam.stats.filt[[name.filt[j]]] <- as.data.frame(beam.stats.l[[name.filt[j]]][which(rownames(beam.stats.l[[name.filt[j]]])%in%row.ids),,drop=FALSE])
    }
    beam.stats.filt <- beam.stats.filt[which(names(beam.stats.filt) %in% beam.specs.filt$name)]
    # filter beam.data
    mtx.name <- un.mtx[i]
    # filter beam.data$mtx.data
    mtx.data.filt[[mtx.name]] <- beam.data$mtx.data[[mtx.name]][which(rownames(beam.data$mtx.data[[mtx.name]]) %in% row.ids),,drop=FALSE]
    # filter beam.data$mtx.anns
    mtx.anns.filt[[mtx.name]] <- beam.data$mtx.anns[[mtx.name]][which(beam.data$mtx.anns[[mtx.name]]$id %in% row.ids),,drop=FALSE]
  }
  beam.data.filt <- beam.data
  beam.data.filt$mtx.data <- mtx.data.filt
  beam.data.filt$mtx.anns <- mtx.anns.filt
  beam.data.filt$set.data <- mtch.set
  beam.data.filt$anns.mtch <- match_anns(mtx.anns.filt, mtx.data.filt)


  # Package results as beam.stats object
  res <- list(beam.stats=beam.stats.filt,
              beam.specs=beam.specs.filt,
              beam.data=beam.data.filt)
  class(res) <- "beam.stats"

  return(res)
}



#########################################
# Match mtx.data with mtx.anns

match_anns <- function(mtx.anns, mtx.data){
  if(any(sapply(mtx.data, is.null))){
    mtx.data2 <- mtx.data[-which(sapply(mtx.data, is.null))]
    mtx.anns2 <- mtx.anns[-which(sapply(mtx.anns, is.null))]
  }
  else{
    mtx.data2 <- mtx.data
    mtx.anns2 <- mtx.anns
  }
  mtx.names <- names(mtx.data2)
  n.mtx <- length(mtx.data2)
  anns.mtch=NULL
  if (!is.null(mtx.anns2))
  {
    message(paste0("  Working on mtx.anns: ",date()))
    anns.mtch=cbind.data.frame(mtx.data=mtx.names,
                               mtx.anns="",
                               id.clm=NA,
                               nrow.mtx=NA,
                               nrow.ann=NA,
                               nrow.map=NA)

    n.anns=length(mtx.anns2)
    if (is.null(names(mtx.anns2)))
      names(mtx.anns2)=paste0("mtx.ann",1:n.anns)

    anns.names=names(mtx.anns2)
    for (i in 1:n.mtx)
    {
      message(paste0("  Matching matrix ",i," with annotations: ",date()))
      mtx.rows=rownames(mtx.data2[[i]])
      anns.mtch[i,"nrow.mtx"]=length(mtx.rows)
      n.mtch=rep(NA,n.anns)
      for (j in 1:n.anns)
      {
        best.mtch=get_id_index(mtx.anns2[[j]],mtx.rows,warn=FALSE)
        n.mtch[j]=sum(!is.na(best.mtch))
      }
      if (any(n.mtch)>0)
      {
        best.ann=which.max(n.mtch)
        anns.mtch[i,"mtx.anns"]=anns.names[best.ann]
        anns.mtch[i,"id.clm"]=find_id_clm(mtx.anns2[[best.ann]],mtx.rows)
        anns.mtch[i,"nrow.ann"]=nrow(mtx.anns2[[best.ann]])
        anns.mtch[i,"nrow.map"]=max(n.mtch)
      }
    }
  }
  return(anns.mtch)
}

