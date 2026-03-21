#' Calculate hierarchy dynamics 
#' 
#' Calculates hierarchy dynamics from an inferred longitudinal hierarchy,
#' returning the provided longitudinal hierarchy with added columns. 
#' The function calculates hierarchy dynamics for each individual in each period
#' (excluding the first period). Dynamics can be calculated in rank units or 
#' score units by specifying the \strong{type} parameter. Rank dynamics are returned as 
#' the total dynamics (delta), active dynamics (delta.active), and passive
#' dynamics (delta.passive). Score dynamics are calculated as delta. 
#' See Strauss & Holekamp (in revision) for more details.  
#' 
#' @param ranks A dataframe such as the output of the ranking functions provided
#'              in the DynaRankR package. Should include at least the following 
#'              columns:
#'                \describe{
#'                 \item{period}{Study period.}
#'                 \item{id}{Identity of contestant.}
#'                 \item{rank}{Rank of id in period. Only required if calculating
#'                             rank dynamics.}
#'                 \item{score}{Score of id in period. Only required if calculating
#'                             score dynamics.}
#'                }
#' 
#' @param type A character string, either 'score' or 'rank'. Determines
#'             whether rank dynamics or score dynamics are calculated. 
#'
#' @return Returns the supplied dataframe with new column(s) for hierarchy dynamics. 
#'         New individuals receive NA for all dynamics because they can not have undergone any changes.
#' 
#' @examples female.ranks <- informed_elo(contestants = C.crocuta.female$contestants, convention = 'mri',
#' initial.ranks = C.crocuta.female$initial.ranks,
#' interactions = C.crocuta.female$interactions)
#' 
#' female.ranks.score <- get_dynamics(ranks = female.ranks, type = 'score')
#' female.ranks.rank <- get_dynamics(ranks = female.ranks, type = 'rank')
#' 
#' @references Strauss ED & Holekamp KE (in revision). Journal of Animal Ecology.
#' 
#' @export


get_dynamics <- function(ranks, type = c('rank','score')){
  if(length(type) != 1){
    type = 'rank'
    warning('hierarchy dynamics type not specified. Using type = rank')
  }
  
  ##Error checks
  if(type == 'rank'){
    ranks$delta <- NA
    ranks$delta.active <- NA
    ranks$delta.passive <- NA
  }else if(type == 'score'){
    ranks$delta <- NA
  }else{stop(paste0('Hierarchy dynamics type \'', type, '\' not supported'))}
  ##Check for required column names
  if(!'period' %in% names(ranks)){
    stop('ranks must contain \'period\' column')
  }
  if(!'id' %in% names(ranks)){
    stop('ranks must contain \'id\' column')
  }
  if(!type %in% names(ranks)){
    stop(paste0('ranks must contain \'', type, '\' column'))
  }
  
  periods <- unique(ranks$period)
  
  for(current.period in periods[-1]){
    current.ranks <- ranks[ranks$period == current.period,]
    prev.ranks <- ranks[ranks$period == periods[(which(periods == current.period)-1)],]
    if(type == 'rank'){
      ranks[ranks$period == current.period,]$delta <- 
        sapply(current.ranks$id,
               FUN = get_delta_rank,
               order = current.ranks$id,
               prev.order = prev.ranks$id)
      
      ranks[ranks$period == current.period,]$delta.active <- 
        sapply(current.ranks$id,
               FUN = get_delta_active_rank,
               order = current.ranks$id,
               prev.order = prev.ranks$id)
      
      ranks[ranks$period == current.period,]$delta.passive <- 
        ranks[ranks$period == current.period,]$delta - 
        ranks[ranks$period == current.period,]$delta.active
      
    }else if(type == 'score'){
      ranks[ranks$period == current.period,]$delta <- 
        sapply(current.ranks$id,
               FUN = get_delta_score,
               current.ranks = current.ranks,
               prev.ranks = prev.ranks)
    }
  }
  return(ranks)
}

get_delta_rank <- function(id, order, prev.order){
  if(!(id %in% order & id %in% prev.order)){
    return(NA)
  }
  return(which(prev.order == id) - which(order == id))
}

get_delta_active_rank <- function(id, order, prev.order){
  if(!(id %in% order & id %in% prev.order)){
    return(NA)
  }
  order <- intersect(order, prev.order)
  prev.order <- intersect(prev.order, order)
  
  return(which(prev.order == id) - which(order == id))
}

get_delta_score <- function(id, current.ranks, prev.ranks){
  if(!(id %in% current.ranks$id & id %in% prev.ranks$id)){
    return(NA)
  }
  return(current.ranks[current.ranks$id == id,'score'] - prev.ranks[prev.ranks$id == id,'score'])
}
