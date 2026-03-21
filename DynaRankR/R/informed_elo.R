#' Elo-rating method informed by prior information
#'
#' Use Elo-rating method to infer a dominance hierarchy over multiple study periods.
#' New contestants are added according to the convention specified by the user. 
#' Full description of the addition of new individuals is described
#' in Strauss & Holekamp (in revision). To run the original Elo-rating procedure,
#' use convention flag 'none'. 
#' 
#' @param contestants A dataframe with the identities of the contestants for 
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
#'                    \item{none}{The standard Elo-rating procedure is run. 
#'                    Individuals joining the hierarchy receive a score equal to 
#'                    the mean of other group members.}
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
#' @param K Parameter influencing the magnitude of score changes after each outcome.
#' 
#' @param lambda Parameter influencing the shape of the logistic function
#'               linking the difference in score between winner and loser 
#'               to the expected probability of each contestant winning.
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
#'          \item{score}{Elo-rating score of contestant.}
#'          \item{rank}{Ordinal rank of contestant in study period. Lower numbers
#'          equal higher rank.}
#'          \item{stan.rank}{Rank of contestant standardized for group size.
#'          Values range from 1 (highest rank) to -1 (lowest rank).}
#'          \item{old.order}{Identity of contestants arranged in the previous order (the order they
#'          were in before updating the order based on observations from current
#'          study period).}}
#' 
#' @importFrom dplyr "%>%"
#' @importFrom rlang .data
#' 
#' @references Strauss ED & Holekamp KE (in revision). Journal of Animal Ecology.
#'   
#'             Albers PCH & de Vries H (2000). Animal Behavior. 
#'
#'@examples 
#' ##Informed elo
#' female.ranks <- informed_elo(contestants = C.crocuta.female$contestants, convention = 'mri',
#' initial.ranks = C.crocuta.female$initial.ranks,
#' interactions = C.crocuta.female$interactions)
#' 
#' ##Standard elo
#' female.ranks <- informed_elo(contestants = C.crocuta.female$contestants, convention = 'none',
#' interactions = C.crocuta.female$interactions)
#'
#'@export
#'
informed_elo <- function(contestants, convention, K = 200, lambda = 100, initial.ranks = NULL, interactions){
  
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
  
  if(is.null(initial.ranks) & convention %in% c('age', 'tenure')){
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
  
  if(is.null(initial.ranks)){
    initial.ranks <- dplyr::filter(contestants, .data$period == periods[1])$id
  }
  if(convention == 'none'){
    current.scores <- data.frame(data.frame(id = initial.ranks, score = 0 ,stringsAsFactors = FALSE))
  }else{
    current.scores <- data.frame(id = initial.ranks, score = seq(from = K*(length(initial.ranks)-1), 
                                                                 to = 0,
                                                                 by = -K),stringsAsFactors = FALSE)
  }
  
  for(current.period in periods){
    
    ##For long run, print status
    if(length(periods) >= 500){
      decile <- floor(length(periods)/10)
      if(which(periods == current.period) %% decile == 0){
        cat(paste0('\nWorking on period ', current.period,' (', which(periods == current.period), ' of ', length(periods), ' periods)'))
      }
    }
    
    intx <- interactions %>%
      dplyr::filter(.data$period == current.period) %>%
      dplyr::select(names(interactions)[c(1,2)])
    
    new.ids <- dplyr::filter(contestants, .data$period == current.period, 
                      !.data$id %in% current.scores$id)$id
    
    if(length(new.ids)){
      current.scores <-rbind(current.scores, 
                             switch(convention,
                                    mri = add_new_ids_mri_elo(new.ids, current.scores, contestants, current.period, periods, ranks),
                                    tenure = add_new_ids_tenure_elo(new.ids, current.scores, contestants, current.period),
                                    age = add_new_ids_age_elo(new.ids, current.scores, contestants, current.period),
                                    phys_attr = add_new_ids_phys_attr_elo(new.ids, current.scores, contestants, current.period),
                                    none = add_new_ids_noconv_elo(new.ids, current.scores)))
      new.ids <- NULL
    }
    current.scores$score <- as.numeric(current.scores$score)
    ## Remove dead or emigrated individuals
    dead <- which(!current.scores$id %in% dplyr::filter(contestants, .data$period == current.period)$id)
    if(length(dead)){current.scores <- current.scores[-dead,]}
    
    ###Check to ensure all individuals in intx for current period are included
    ###in contestants for current period.
    intx.no.contestants <- c(intx$winner[!intx$winner %in% current.scores$id],
                             intx$loser[!intx$loser %in% current.scores$id])
    if(length(intx.no.contestants)){
      intx <- dplyr::filter(intx, !.data$winner %in% intx.no.contestants, 
                     !.data$loser %in% intx.no.contestants)
    }
      
      for(i in 1:nrow(intx)){
        initial.ranks <- current.scores$id
        winner <- intx[i,]$winner
        loser <- intx[i,]$loser
        E_winner = 1/(1 + exp((current.scores[current.scores$id == winner,'score'] - 
                                current.scores[current.scores$id == loser,'score'])/-lambda))
        E_loser = 1/(1 + exp((current.scores[current.scores$id == loser,'score'] - 
                               current.scores[current.scores$id == winner,'score'])/-lambda))
        
        current.scores[current.scores$id == winner,'score'] <- 
          current.scores[current.scores$id == winner,'score'] + K*(1-E_winner)
        
        current.scores[current.scores$id == loser,'score'] <- 
          current.scores[current.scores$id == loser,'score'] + K*(0-E_loser)
      }
    current.scores <- dplyr::arrange(current.scores, dplyr::desc(.data$score))
    
    ## save to ranks object
    ranks[ranks$period == current.period,'old.order'] <- initial.ranks
    ranks[ranks$period == current.period,'id'] <- current.scores$id
    ranks[ranks$period == current.period,'rank'] <- 1:length(current.scores$id)
    ranks[ranks$period == current.period,'score'] <- current.scores$score
  }
  
  ranks <- ranks %>% 
    dplyr::group_by(.data$period) %>% 
    dplyr::mutate(stan.rank = -2*(.data$rank-1)/(max(.data$rank)-1) + 1) %>% 
    dplyr::select(.data$period, .data$id, .data$score, .data$rank, .data$stan.rank, .data$old.order) %>% 
    as.data.frame()
  
  return(ranks)
}
