add_after_index <- function(index, vec, newrec){
  if(index == 0){
    return(c(newrec, vec))
  }else if(index == length(vec)){
    return(c(vec, newrec))
  }else{
    return(c(vec[1:index], newrec, vec[(index+1):length(vec)]))
  }
}

#' @importFrom rlang .data
add_new_ids_mri <- function(new.ids, working.ranks, contestants, period, periods, ranks){
  new.ids <- contestants[contestants$id %in% new.ids &
                          contestants$period == period,]
  if('convention2' %in% names(new.ids)){
    if(!is.numeric(new.ids$convention2))
      stop('convention2 not numeric')
    new.ids <- new.ids[order(new.ids$convention2, decreasing = TRUE),]
  }
  for(nid in new.ids$id){
    mom <- new.ids[new.ids$id == nid,'convention1']
    if(!mom %in% contestants$id){
      stop(paste0('can\'t add \'', nid, '\' because mom never appears in hierarchy'))
    }
    
    if(mom %in%  working.ranks){
      mom.index <- which(working.ranks == mom)
      working.ranks <- add_after_index(index = mom.index, vec = working.ranks,
                                       newrec = nid)
    }else{
      period.index <- which(periods == period)-1
      mom.index <- NULL
      while(!length(mom.index) & period.index > 0){
        prev.period.rank <- dplyr::filter(ranks, .data$period == periods[period.index])
        ##If mom was highest ranked, add to front and move to next id
        if(prev.period.rank$id[1] == mom){
          mom.index <- 0
        }else{
          ##Otherwise, locate id ranked above mom and add new individual below her
          mom.index <- which(prev.period.rank$id == mom)-1
        }
        period.index <- period.index - 1
      }
      if(length(mom.index)){
        working.ranks <- add_after_index(mom.index, working.ranks, nid)
      }else{
        stop(paste0('Could not place ', nid, '. Mom not in ranks'))
      }
    }
  }
  return(working.ranks)
}

#' @importFrom rlang .data
add_new_ids_tenure <- function(new.ids, working.ranks, contestants, period){
  new.ids <- contestants[contestants$id %in% new.ids &
                           contestants$period == period,]
  
  if('convention2' %in% names(new.ids)){
    new.ids <- new.ids[order(new.ids$convention1, new.ids$convention2),]
  }else{
    new.ids <- new.ids[order(new.ids$convention1),]
  }
  
  if(any(!sapply(new.ids[1,startsWith(names(new.ids), 'convention')], class) %in% c('Date', 'numeric'))){
    stop('Conventions must be dates or numeric')
  }
  
  conts.this.period <- contestants[contestants$period == period,]
  
  if('convention2' %in% names(conts.this.period)){
    conts.ordered <- dplyr::arrange(conts.this.period, .data$convention1, dplyr::desc(.data$convention2))
  }else{
    conts.ordered <- dplyr::arrange(conts.this.period, .data$convention1)
  }
  for(nid in new.ids$id){
    working.ranks <- add_after_index(which(nid == conts.ordered$id)-1, vec = working.ranks,newrec = nid)
  }
  return(working.ranks)
}

#' @importFrom rlang .data
add_new_ids_phys_attr <- function(new.ids, working.ranks, contestants, period){
  new.ids <- contestants[contestants$id %in% new.ids &
                           contestants$period == period,]
  
  if('convention2' %in% names(new.ids)){
    new.ids <- new.ids[order(new.ids$convention1, new.ids$convention2, decreasing = TRUE),]
  }else{
    new.ids <- new.ids[order(new.ids$convention1, decreasing = TRUE),]
  }
  
  if(any(!sapply(new.ids[1,startsWith(names(new.ids), 'convention')], class) %in% c('Date', 'numeric'))){
    stop('Conventions must be numeric')
  }
  
  conts.this.period <- contestants[contestants$period == period,]
  
  if('convention2' %in% names(conts.this.period)){
    conts.ordered <- dplyr::arrange(conts.this.period, dplyr::desc(.data$convention1), dplyr::desc(.data$convention2))
  }else{
    conts.ordered <- dplyr::arrange(conts.this.period, dplyr::desc(.data$convention1))
  }
  for(nid in new.ids$id){
    working.ranks <- add_after_index(which(nid == conts.ordered$id)-1, vec = working.ranks,newrec = nid)
  }
  return(working.ranks)
}

add_new_ids_age <- add_new_ids_tenure
