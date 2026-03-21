#' Plot individual ranks or scores
#' 
#' Plot ranks of individuals in a single social group over multiple study periods.
#' 
#' @param ranks A dataframe of ranks. There should be one row per contestant per
#' study period. Must include at least the following columns:
#'    \describe{
#'      \item{period}{Study periods. They should appear in chronological order.}
#'      \item{id}{The identity of each contestant. Each contestant should appear
#'      once per study period.}
#'      \item{rank}{The rank of each contestant in each study period. This can be
#'      absolute rank or standardized rank. Only required if \strong{type}
#'      is 'rank'.}
#'      \item{score}{The score of each contestant. Only required if \strong{type}
#'      is 'score'.}
#'    }
#'  
#' @param type A character string, either 'score', 'rank', or 'stan.rank'. Determines
#'             whether scores, ranks, or standardized ranks are plotted. 
#' 
#' @examples female.ranks <- informed_elo(contestants = C.crocuta.female$contestants, convention = 'mri',
#' initial.ranks = C.crocuta.female$initial.ranks,
#' interactions = C.crocuta.female$interactions)
#' 
#' plot_ranks(female.ranks, type = 'rank')
#' plot_ranks(female.ranks, type = 'score')
#' plot_ranks(female.ranks, type = 'stan.rank')
#' 
#' @importFrom graphics plot axis lines
#' 
#' @export

plot_ranks <- function(ranks, type = c('rank', 'stan.rank', 'score')){
  
  if(length(type) > 1){
    type <- 'rank'
    warning('Defaulting to type = rank')
  }
  
  if(type == 'stan.rank'){
    ylimit <- c(-1,1)
    ylabel <- 'Standardize Rank'
    if(!'stan.rank' %in% names(ranks)){
      stop('supplied ranks don\'t contain \'stan.rank\' column')
    }
    ranks$rank <- ranks$stan.rank
  }else if(type == 'rank'){
    ylimit <- c(max(ranks$rank), 0)
    ylabel <- 'Rank'
  }else if(type == 'score'){
    ylimit <- c(min(ranks$score), max(ranks$score))
    ylabel <- 'Score'
    if(!'score' %in% names(ranks)){
      stop('supplied ranks don\'t contain \'score\' column')
    }
    ranks$rank <- ranks$score
  }else{stop('Only type \'rank\', \'stan.rank\', and \'score\' supported.')}
  
  if(is.factor(ranks$period)){
    ranks$period <- as.character(ranks$period)
    ranks$x <- as.numeric(factor(ranks$period, levels = unique(ranks$period)))
    plot(x = ranks$x, y = ranks$rank, type = 'n', ylim = ylimit,
         xlab = 'Study period', ylab = ylabel, xaxt = 'n')
    axis(1, at = ranks$x, labels = ranks$period)
    for(id in unique(ranks$id)){
      lines(x = ranks[ranks$id == id,'x'], y = ranks[ranks$id == id,'rank'])
    }
  }else if(is.character(ranks$period)){
    ranks$x <- as.numeric(factor(ranks$period, levels = unique(ranks$period)))
    plot(x = ranks$x, y = ranks$rank, type = 'n', ylim = ylimit,
         xlab = 'Study period', ylab = ylabel, xaxt = 'n')
    axis(1, at = ranks$x, labels = ranks$period)
    for(id in unique(ranks$id)){
      lines(x = ranks[ranks$id == id,'x'], y = ranks[ranks$id == id,'rank'])
    }
  }else{
    plot(x = ranks$period, y = ranks$rank, type = 'n', ylim = ylimit,
         xlab = 'Study period', ylab = ylabel)
    for(id in unique(ranks$id)){
      lines(x = ranks[ranks$id == id,'period'], y = ranks[ranks$id == id,'rank'])
    }
  }
}

