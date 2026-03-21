makePraisBibby <- function(dat, rank_x, rank_y){
  # Calculates Prais-Bibby Index for a dataset using rank_x and rank_y
  # dat - data.frame
  # rank_x - string
  # rank_y - string
  condition <- dat[[rank_x]] == dat[[rank_y]]
  numerator <- nrow(subset(dat, condition))
  denominator <- nrow(dat)
  value <- numerator/denominator
  return(1 - value)
}

makeAverageMovement <- function(dat, rank_x, rank_y){
  # Calculates Average Movement Index for a dataset using rank1 and rank2
  # dat - data.frame
  # rank1 - string
  # rank2 - string
  movement <- abs(dat[[rank_x]] - dat[[rank_y]])
  return(mean(movement))
}

makeOriginSpecific <- function(dat, rank1, rank2, where, variety){
  # Computes one of several different origin specific indices depending on location and variety
  # dat - data.frame
  # rank1 - string
  # rank2 - string
  # where - string - the location of the index; 'top' and 'bottom' are accepted
  # variety - string - does the index measure any movement or only far movement;
  # 'total' and 'far' are accepted
  # bounds - sequence - indicates the bounds for the rank
  n1 <- max(dat[[rank1]])
  n2 <- max(dat[[rank2]])
  if (where == 'top'){
    if (variety == 'total'){
      num <- nrow(subset(dat, dat[[rank1]] == n1 & dat[[rank2]] < n2))
      den <- nrow(subset(dat, dat[[rank1]] == n1))
      value <- num / den
      return(value)
    } else if (variety == 'far') {
      num <- nrow(subset(dat, dat[[rank1]] == n1 & dat[[rank2]] < n2 - 1))
      den <- nrow(subset(dat, dat[[rank1]] == n1))
      value <- num / den
      return(value)
    } else stop("Not a valid variety. Use total or far.")
  }
  else if (where == 'bottom'){
    if (variety == 'total'){
      num <- nrow(subset(dat, dat[[rank1]] == 1 & dat[[rank2]] > 1))
      den <- nrow(subset(dat, dat[[rank1]] == 1))
      value <- num / den
      return(value)
    } else if (variety == 'far') {
      num <- nrow(subset(dat, dat[[rank1]] == 1 & dat[[rank2]] > 2))
      den <- nrow(subset(dat, dat[[rank1]] == 1))
      value <- num / den
      return(value)
    } else stop("Not a valid variety. Use total or far.")
  } else stop("Not a valid where argument. Use top or bottom.")
}

makeWGM <- function(dat, rank1, rank2){
  # Calculates the Weighted Group Mobility Index for a dataset using rank1 and rank2
  # dat - data.frame
  # rank1 - string
  # rank2 - string
  ranks <- unique(dat[[rank1]])
  n <- length(ranks)
  r_num <- c()
  r_den <- c()
  q <- c()
  for (i in 1:n){
    r_num[i] <- nrow(subset(dat, dat[[rank1]] == ranks[i] & dat[[rank2]] != ranks[i])) / nrow(subset(dat, dat[[rank1]] == ranks[i]))
    r_den[i] <- nrow(subset(dat, dat[[rank2]] != ranks[i])) / nrow(dat)
    q[i] <- r_num[i] / r_den[i]
  }
  value <- (1/n) * sum(q)
  return(value)
}

makeIndex <- function(dat, rank_x, rank_y, index){
  if (index == 'prais_bibby') {
    value <- makePraisBibby(dat, rank_x = rank_x, rank_y = rank_y)
    return(list('prais_bibby' = value))
  }
  else if (index == 'average_movement') {
    value <- makeAverageMovement(dat, rank_x, rank_y)
    return(list('average_movement' = value))
  }
  else if (index == 'wgm') {
    value <- makeWGM(dat, rank_x, rank_y)
    return(list('wgm' = value))
  }
  else if (index == 'origin_specific') {
    total_top <- makeOriginSpecific(dat, rank_x, rank_y, 'top', 'total')
    far_top <- makeOriginSpecific(dat, rank_x, rank_y, 'top', 'far')
    total_bottom <- makeOriginSpecific(dat, rank_x, rank_y, 'bottom', 'total')
    far_bottom <- makeOriginSpecific(dat, rank_x, rank_y, 'bottom', 'far')
    value <- list('os_total_top' = total_top,
                  'os_far_top' = far_top,
                  'os_total_bottom' = total_bottom,
                  'os_far_bottom' = far_bottom)
    return(value)
  }
  else (stop('Not a supported index! See the mobilityIndexR::getMobilityIndices documentation.'))
}

makeMobilityIndices <- function(dat, col_x, col_y, type, indices, num_ranks, exclude_value, bounds, strict, rerank_exclude_value){
  df_rank_x <- makeRanks(dat = dat, col_in = col_x, col_out = 'rank_x', type = type,
                         num_ranks = num_ranks, exclude_value = exclude_value,
                         mixed_col = col_x, bounds = bounds, strict = strict)
  df_rank_y <- makeRanks(dat = dat, col_in = col_y, col_out = 'rank_y', type = type,
                         num_ranks = num_ranks, exclude_value = exclude_value,
                         mixed_col = col_x, bounds = bounds, strict = strict)
  df <- merge(df_rank_x$data, df_rank_y$data, by = 'id')
  output <- list()
  if (!missing(exclude_value)){
    df <- modifyExcludeValueRank(dat = df, col_x = col_x, col_y = col_y, rank_x = 'rank_x', rank_y = 'rank_y',
                                 rerank_exclude_value = rerank_exclude_value, bounds_x = df_rank_x$bounds,
                                 bounds_y = df_rank_y$bounds, exclude_value = exclude_value)
  }
  if (indices == 'all') {
    indices <- c('prais_bibby', 'average_movement', 'wgm', 'origin_specific')
  }
  for (index in indices){
    if (!(index %in% c('prais_bibby', 'average_movement', 'wgm', 'origin_specific'))){
      stop(paste('Index', index, 'not supported.'))
    }
    value <- makeIndex(dat = df, rank_x = 'rank_x', rank_y = 'rank_y', index = index)
    output <- c(output, value)
  }
  return(output)
}

makeBootstrapSamples <- function(dat, col_x, col_y, type, indices, num_ranks, exclude_value, bounds, strict, rerank_exclude_value, bootstrap_iter){
  stopifnot(is.numeric(bootstrap_iter))
  stopifnot(bootstrap_iter > 1)
  output <- data.frame(makeMobilityIndices(dat = dat, col_x = col_x, col_y = col_y, type = type, indices = indices, num_ranks = num_ranks, exclude_value = exclude_value, bounds = bounds, strict = strict, rerank_exclude_value = rerank_exclude_value))
  for(i in 2:bootstrap_iter){
    temp_index <- sample(x = nrow(dat), size = nrow(dat), replace = TRUE)
    temp_dat <- dat[temp_index, ]
    stopifnot(nrow(dat) == nrow(temp_dat))
    temp_dat[['id']] <- 1:nrow(temp_dat)
    temp_results <- makeMobilityIndices(dat = temp_dat, col_x = col_x, col_y = col_y, type = type, indices = indices, num_ranks = num_ranks, exclude_value = exclude_value, bounds = bounds, strict = strict, rerank_exclude_value = rerank_exclude_value)
    output[i,] <- temp_results
  }
  return(output)
}

#' @title Calculates Mobility Indices at Two Points in Time
#'
#' @description Calculates mobility indices from two columns in dataset. Supports Prais-Bibby,
#' Absolute Movement, Origin Specific, and Weighted Group Mobility indices and relative, mixed,
#' and absolute types of rankings in the calculation these indices.
#'
#' @import stats
#'
#' @param dat a dataframe with an "id" column
#' @param col_x a character string denoting the first column to be used in the index calculation
#' @param col_y a character string denoting the second column to be used in the index calculation
#' @param type a character string indicating the type of ranking;
#' accepts 'relative', 'mixed', and 'absolute'
#' @param indices a vector of character strings indicating which mobility indices are desired;
#' currently support 'prais_bibby', 'average_movement', 'wgm', and 'origin_specific'.
#' The default value is 'all'.
#' @param num_ranks an integer specifying the number of ranks for a relative or mixed ranking
#' @param exclude_value a single numeric value that is excluded in calculating the transition matrix;
#' see the rerank_exclude_value parameter to specify how the exclude value is handled
#' @param bounds a sequence of numeric bounds for defining absolute ranks
#' @param rerank_exclude_value a character string indicating how the exclude value is handled when present; accepts
#' 'as_new_rank', 'as_existing_rank', and 'exclude'
#' @param strict logical. If TRUE, indices are calculated from the given values. If FALSE,
#' indices are calculated by jittering the values to ensure uniqueness of bounds of ranks.
#' Only used with relative and mixed types. The default value is TRUE.
#' @param intervals logical. If TRUE, will calculate bootstrapped confidence intervals
#' using the percentile method. The default value is FALSE.
#' @param interval_pct a number between zero and one indicating the size of the bootstrapped intervals.
#' The default value is 0.95.
#' @param bootstrap_iter the number of bootstrap iterations used to estimate intervals.
#' The default value is 100.
#'
#' @return Returns a named vector containing the desired index values
#' @export
#'
#' @examples
#' data(incomeMobility)
#' getMobilityIndices(dat = incomeMobility,
#'                    col_x = 't0',
#'                    col_y = 't2',
#'                    type = 'relative',
#'                    num_ranks = 5)
getMobilityIndices <- function(dat, col_x, col_y, type, indices = 'all', num_ranks, exclude_value, bounds, rerank_exclude_value, strict = TRUE, intervals = FALSE, interval_pct = 0.95, bootstrap_iter = 100){
  output <- makeMobilityIndices(dat = dat, col_x = col_x, col_y = col_y, type = type, indices = indices, num_ranks = num_ranks, exclude_value = exclude_value, bounds = bounds, strict = strict, rerank_exclude_value = rerank_exclude_value)
  if (intervals == TRUE){
    stopifnot(is.numeric(interval_pct))
    stopifnot(interval_pct < 1 & interval_pct > 0)
    bootstrap_samples <- makeBootstrapSamples(dat = dat, col_x = col_x, col_y = col_y, type = type, indices = indices, num_ranks = num_ranks, exclude_value = exclude_value, bounds = bounds, strict = strict, rerank_exclude_value = rerank_exclude_value, bootstrap_iter = bootstrap_iter)
    for (index in colnames(bootstrap_samples)){
      lower_bound <- (1 - interval_pct)/2
      upper_bound <- (1 + interval_pct)/2
      value <- list('lower' = quantile(x = bootstrap_samples[[index]], probs = lower_bound),
                    'upper' = quantile(x = bootstrap_samples[[index]], probs = upper_bound),
                    'interval_bounds' = c(lower_bound, upper_bound))
      list_name <- paste0(index, '_intervals')
      output_list <- list()
      output_list[[list_name]] <- value
      output <- c(output, output_list)
    }
  }
  output <- output[order(names(output))]
  return(output)
}


