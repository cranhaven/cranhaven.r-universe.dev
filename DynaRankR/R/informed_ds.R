#' David's Score method informed by prior information
#'
#' Use David's Score method to infer a dominance hierarchy over multiple study periods.
#' New contestants are added according to the convention specified by the user.
#' Scores are calculated using Dij and are normalized.  
#' Full description of the addition of new individuals is described
#' in Strauss & Holekamp (in revision). To run the original David's Score procedure,
#' use convention flag 'none'. 
#' 
#'
#'@param contestants A dataframe with the identities of the contestants for 
#'                    each study period along with the relevant data for 
#'                    adding them to the hierarchy. There should be one row per
#'                    contestant per study period.
#'                    Periods should appear in chronological order.
#'                    The dataframe should contain the following columns: 
#'                    \describe{
#'                      \item{period}{Study period.}
#'                      \item{id}{Identity of contestant.}
#'                      \item{convention1}{The primary convention by which new
#'                      individuals are added to the hierarchy. Interpretation
#'                      of this column varies depending on the value of the 
#'                      \strong{convention} argument. If \strong{convention} = none, 
#'                      this column is optional.}
#'                      \item{convention2}{Optional. The secondary data for 
#'                      resolving ties in convention1. Interpretation
#'                      of this column varies depending on the value of the 
#'                      \strong{convention} argument.}
#' }
#' @param convention A flag determining how new individuals are added to the
#'                   hierarchy. The value of this flag influences how the convention1
#'                   and convention2 columns of the \strong{contestants} argument are interpreted.
#'                   Currently this function supports five options:
#'                   \describe{
#'                    \item{none}{The standard David's Score procedure (using Dij) is run. 
#'                    Individuals are not added according to prior information 
#'                    and scores are calculated independently for each period.}
#'                    \item{mri}{New contestants are added to the hierarchy
#'                    according to maternal rank inheritance with youngest
#'                    ascendancy. \strong{convention1} should be a vector of 
#'                    mother identities for each contestant. \strong{convention2}
#'                    should be an optional vector of intra-litter ranks (lower 
#'                    numbers = higher rank) for resolving the order of 
#'                    contestants from the same mother
#'                    joining the hierarchy in the same study period.}
#'                    \item{tenure}{New contestants are added to the hierarchy
#'                    according their tenure in the group. \strong{convention1} should be a vector of 
#'                    dates on which each contestant joined the group. \strong{convention2} should be an
#'                    optional vector of numerical data for resolving ties
#'                    in convention1 (e.g., body size). Higher values are 
#'                    considered higher rank.}
#'                    \item{age}{New contestants are added to the hierarchy
#'                    according their age (older = higher rank).
#'                    \strong{convention1} should be a vector of birthdates or 
#'                    numerical age classes. \strong{convention2} should be an
#'                    optional vector of numerical data for resolving ties
#'                    in convention1 (e.g., body size). Higher values are 
#'                    considered higher rank.}
#'                    \item{phys_attr}{New contestants are added to the hierarchy
#'                    according to some physical attribute (larger value = higher rank). 
#'                    \strong{convention1} should be a vector of numerical attribute
#'                    measurements. \strong{convention2} should be an
#'                    optional vector of numerical data for resolving ties
#'                    in convention1. Higher values are 
#'                    considered higher rank.}
#'                   }
#' 
#' @param initial.ranks The initial ordering of individuals for the first study
#'        period. Required if using maternal rank inheritance as the convention.
#'        For other conventions, if initial.ranks is not specified,
#'         the order determined by convention1 is used to create the initial order. 
#' 
#' @param interactions A dataframe of interaction data with the following columns:
#'         \describe{
#'          \item{winner}{Identities of winners.}
#'          \item{loser}{Identities of losers.}
#'          \item{period}{Study period in which interactions occurred.}}
#' 
#' @return Produces a dataframe with the following columns: 
#'          \describe{
#'          \item{period}{Study period.}
#'          \item{id}{Identity of contestant.}
#'          \item{score}{David's Score of contestant.}
#'          \item{rank}{Ordinal rank of contestant in study period. Lower numbers
#'          equal higher rank.}
#'          \item{stan.rank}{Rank of contestant standardized for group size.
#'          Values range from 1 (highest rank) to -1 (lowest rank).}
#'          \item{old.order}{Identity of contestants arranged in the previous order (the order they
#'          were in before updating the order based on observations from the current
#'          study period).}}
#' 
#' @importFrom dplyr "%>%"
#' @importFrom rlang .data
#' 
#' @references Strauss ED & Holekamp KE (in revision). Journal of Animal Ecology.
#'   
#'             de Vries H, Stevens JMG, Vervaecke H (2006). Animal Behavior.
#'
#'@examples 
#' ##Informed ds
#' female.ranks <- informed_ds(contestants = C.crocuta.female$contestants, convention = 'mri',
#' initial.ranks = C.crocuta.female$initial.ranks,
#' interactions = C.crocuta.female$interactions)
#' 
#' ##Standard ds
#' female.ranks <- informed_ds(contestants = C.crocuta.female$contestants, convention = 'none',
#' interactions = C.crocuta.female$interactions)                          
#'
#'@export

informed_ds <- function(contestants, convention,
                        initial.ranks = NULL, interactions){
  periods <- unique(contestants$period)
  
  
  if(convention == 'mri'){
    if(is.null(initial.ranks)){
      stop('initial.ranks must be provided if convention = mri')
    }
    missing.moms <- unique(contestants$convention1[which(!contestants$convention1 %in% contestants$id &
                            !contestants$id %in% initial.ranks)])
    if(length(missing.moms)){
      stop('some moms not included in contestants. Missing moms: ', paste(missing.moms, collapse = ', '))
    }
  }
  
  if(is.null(initial.ranks) & convention %in% c('tenure', 'age')){
    if('convention2' %in% names(contestants)){
      initial.ranks <- dplyr::filter(contestants, .data$period == periods[1]) %>% 
        dplyr::arrange(.data$convention1, dplyr::desc(.data$convention2)) %>%
        dplyr::pull(.data$id)
    }else{
      initial.ranks <- dplyr::filter(contestants, .data$period == periods[1]) %>% 
        dplyr::arrange(.data$convention1) %>% 
        dplyr::pull(.data$id)
    }
  }else if(is.null(initial.ranks) & convention %in% c('phys_attr')){
    if('convention2' %in% names(contestants)){
      initial.ranks <- dplyr::filter(contestants, .data$period == periods[1]) %>% 
        dplyr::arrange(dplyr::desc(.data$convention1), dplyr::desc(.data$convention2)) %>%
        dplyr::pull(.data$id)
    }else{
      initial.ranks <- dplyr::filter(contestants, .data$period == periods[1]) %>% 
        dplyr::arrange(dplyr::desc(.data$convention1)) %>% 
        dplyr::pull(.data$id)
    }
  }
  
  if(!convention %in% c('mri','tenure','age','phys_attr','none'))
    stop('convention not recognized. Must be one of: \'mri\', \'tenure\', \'age\', \'phys_attr\', \'none\'')
  
  if(convention != 'none' & any(!c('period', 'id', 'convention1') %in% names(contestants))){
    stop('contestants dataframe missing \'period\' or \'id\' column')
  }else if(any(!c('period', 'id') %in% names(contestants))){
    stop('contestants dataframe missing \'period\', \'id\', or \'convention1\' column')
  }
  
  if(any(!c('winner', 'loser', 'period') %in% names(interactions)))
    stop('interactions dataframe missing \'winner\', \'loser\', or \'period\' column')
  
  ##initialize ranks 
  ranks <- contestants
  ranks$id <- NA
  ranks$rank <- NA
  ranks$old.order <- NA
  ranks$score <- NA
  ranks <- dplyr::select(ranks, .data$period, .data$id, .data$score, .data$rank, .data$old.order)
  
  working.ranks <- initial.ranks
  if(convention == 'none'){
    working.ranks <- dplyr::filter(contestants, .data$period == periods[1])$id
  }
  
  working.ranks <- working.ranks[working.ranks %in% dplyr::filter(contestants, .data$period == periods[1])$id]
    
  
  ##Prep for first period
  current.Dij <- matrix(data = 0, nrow = length(working.ranks), ncol = length(working.ranks),
                        dimnames = list(working.ranks, working.ranks))
  current.Dij[upper.tri(current.Dij)] <- 1
  current.Dij <- ds_single(current.Dij)
  
  
  
  for(current.period in periods){
    
    ##For long run, print status
    if(length(periods) >= 500){
      decile <- floor(length(periods)/10)
      if(which(periods == current.period) %% decile == 0){
        cat(paste0('\nWorking on period ', current.period,' (', which(periods == current.period), ' of ', length(periods), ' periods)'))
      }
    }
  
    new.ids <- dplyr::filter(contestants, .data$period == current.period, 
                      !.data$id %in% rownames(current.Dij))$id
    
    ## Add new ids according to convention
    if(length(new.ids)){
      working.ranks <- switch(convention,
                              mri = add_new_ids_mri(new.ids, working.ranks, contestants, current.period, periods, ranks),
                              tenure = add_new_ids_tenure(new.ids, working.ranks, contestants, current.period),
                              age = add_new_ids_age(new.ids, working.ranks, contestants, current.period),
                              phys_attr = add_new_ids_phys_attr(new.ids, working.ranks, contestants, current.period),
                              none = c(working.ranks, new.ids))
      new.ids <- NULL
    }
    
    ## Remove dead or emigrated individuals
    dead <- which(!working.ranks %in% dplyr::filter(contestants, .data$period == current.period)$id)
    if(length(dead)){working.ranks <- working.ranks[-dead]}
    
    dead <- which(!rownames(current.Dij) %in% dplyr::filter(contestants, .data$period == current.period)$id)
    if(length(dead)){current.Dij <- current.Dij[-dead,-dead]}
    
    initial.ranks <- working.ranks
    
    ## filter interactions to only those in this period and with these contestants
    intx.matrix <- interactions %>%
      dplyr::filter(.data$period %in% current.period,
             .data$winner %in% working.ranks,
             .data$loser %in% working.ranks) %>%
      dplyr::select(names(interactions)[c(1,2)]) %>%
      edgelist_to_matrix(identities = working.ranks)
    
    ids.for.current.Dij <- working.ranks[which(!working.ranks %in% colnames(current.Dij))]
    
    if(length(ids.for.current.Dij)){
      
      mat.temp <- matrix(data = 0, nrow = length(working.ranks), ncol = length(working.ranks),
                         dimnames = list(working.ranks, working.ranks))
      mat.temp[rownames(current.Dij), colnames(current.Dij)] <- current.Dij
      ###New individuals get 0.75 for wins down hierarchy, 0.25 for wins up hierarchy
      mat.temp[lower.tri(mat.temp) & 
                 colnames(mat.temp) %in% ids.for.current.Dij] <- 0.25
      mat.temp[upper.tri(mat.temp) & colnames(mat.temp) %in% ids.for.current.Dij] <- 0.75
      ###New individuals get 0.25 for losses down hierarchy, 0.75 for losses up hierarchy
      mat.temp[t(lower.tri(mat.temp) & 
                 colnames(mat.temp) %in% ids.for.current.Dij)] <- 0.75
      mat.temp[t(upper.tri(mat.temp) & 
                   colnames(mat.temp) %in% ids.for.current.Dij)] <- 0.25
      
      current.Dij <- mat.temp
    }
    if(convention == 'none'){
      current.Dij <- ds_single(obs = intx.matrix)
    }else{
      current.Dij <- informed_ds_single(obs = intx.matrix,
                                        prior = current.Dij)
    }
    
    current.scores <- data.frame(id = working.ranks,
                             normDS = calc_ds(current.Dij),
                             stringsAsFactors = FALSE)
    current.scores <- dplyr::arrange(current.scores, dplyr::desc(.data$normDS))
    
    ## save to ranks object
    ranks[ranks$period == current.period,'old.order'] <- initial.ranks
    ranks[ranks$period == current.period,'id'] <- current.scores$id
    ranks[ranks$period == current.period,'rank'] <- 1:length(current.scores$id)
    ranks[ranks$period == current.period,'score'] <- current.scores$normDS
  }
  
  ranks <- ranks %>% 
    dplyr::group_by(.data$period) %>% 
    dplyr::mutate(stan.rank = -2*(.data$rank-1)/(max(.data$rank)-1) + 1) %>% 
    dplyr::select(.data$period, .data$id, .data$score, .data$rank, .data$stan.rank, .data$old.order) %>% 
    as.data.frame()
  
  return(ranks)
}




informed_ds_single <- function(obs, prior){
  size <- obs + t(obs)
  Pij <- ifelse(size == 0, 0, obs/size)
  return(
    ifelse(size == 0,
           prior,
           Pij - ((Pij - prior)*(1/(size+1))))
  )
}

ds_single <- function(obs){
  size <- obs + t(obs)
  Pij <- ifelse(size == 0, 0, obs/size)
  return(
    ifelse(size == 0,
           0,
           Pij - ((Pij - 0.5)*(1/(size+1))))
  )
}

calc_ds <- function(Dij.mat){
  nids <- nrow(Dij.mat)
  w <- apply(X = Dij.mat, MARGIN = 1, FUN = sum)
  l <- apply(X = Dij.mat, MARGIN = 2, FUN = sum)
  w2 <- apply(X = Dij.mat * matrix(data = w, nrow = nids, ncol = nids, byrow = TRUE), MARGIN = 1, FUN = sum)
  l2 <- apply(X = Dij.mat * matrix(data = l, nrow = nids, ncol = nids, byrow = FALSE), MARGIN = 2, FUN = sum)
  
  return((w + w2 - l - l2 + nids*(nids-1)/2)/nids)
}

