


#' OAHC S4 Class
#'
#'  @include heuristic_model_search_algorithm.R
#'
#' @slot score numeric
# TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @slot cluster list
# TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#'
# @return
# @export
#'
#'
#'
setClass("OAHC",
         representation(score = "numeric",
                        cluster = "list"),
         contains = "Heuristic.model.search.algorithm"
)

setMethod(
  f = "initialize",
  signature = "OAHC",
  definition = function(.Object,
                        score,
                        cluster) {
    cat("~~~ OAHC: initializator ~~~ \n")
    # Assignment of the slots
    .Object@score <- score
    .Object@cluster <- cluster
    return(.Object)
    # return of the object
  }
)


#'   OAHC Constructor
#'
#'   This function calculates the best stage configuration of a hyperstage
#'   associated with a specific variable of time-slice t_0 or t_k, k>=1, using
#'   the oahc algorithm (oahc - Optimised Agglomerative Hierarchical Clustering)
#'
#' @param level  numeric   - level under optimisation
#' @param prior.distribution  (list of matrices) - see function
#' \code{prior.distribution}
#' @param contingency.table (list of matrices) - see function ContingencyTable
#' @param tree  an object 'Event.tree'
#'
#' @return a OAHC S4 object
# @export
#'
#' @seealso SingleScore,  PairwiseScore, SingleReorder, NaReorder, KeepLexOrder
# It is required the following functions:
# SingleScore - This function calculates the score associated with a particular level.
# PairwiseScore - This function calculates the score asscoiated with a pair of merged stages.
# SingleReorder - This function reorders a vector that had an element changed.
# NaReorder - This function reorders a vector that had an element changed to NA.
# KeepLexOrder - This function keep a lexicographical order of a vector that have had one element changed.
OAHC <- function(level, prior.distribution, contingency.table, tree) {
  if (tree@num.situation[level] == 1)
    return(new(
      "OAHC",
      score = SingleScore(tree@num.situation[level],
                           prior.distribution[[level]],
                           contingency.table[[level]]),
      cluster = list(1)
    ))

  vec.score <-
    sapply(1:tree@num.situation[level], function(x)
      SingleScore(x, prior.distribution[[level]], contingency.table[[level]]))

  # To calculate the score asscociated with each stage of the finest partition.
  cluster <- lapply(1:tree@num.situation[level], function(x)
    x)

  # To create a list to track the clusters of situation that constitute stages.
  pair.score <- sapply(1:(tree@num.situation[level] - 1), function(x)
    sapply((x + 1):tree@num.situation[level], function(y)
      PairwiseScore(x, y, vec.score, prior.distribution[[level]],
                     contingency.table[[level]])))

  # List of vectors. Each vector records the pairwise score for stages (i,j),
  # where i<j according to lexicographical order.
  order.pair.score <-
    sapply(1:(tree@num.situation[level] - 1), function(x)
      order(pair.score[[x]], decreasing = TRUE))

  #List of vectors. Each vector orders its corresponding vector in pair.score.
  max.score <- sapply(1:(tree@num.situation[level] - 1), function(x)
    pair.score[[x]][order.pair.score[[x]][1]])

  #Vector that records that maximum score of each component of pair.score.
  order.max.score <- order(max.score, decreasing = TRUE)

  #Vector that records the order of the vector max.score.
  if (level > tree@num.variable)
    variable <- level - tree@num.variable
  else
    variable <- level
  aux.na <- rep(NA, tree@num.category[variable])
  count <- 0

  #To track the total number of stages merged
  count.na <- rep(0, (tree@num.situation[level] - 1))
  while ((max.score[order.max.score[1]] > 0) &&
         (count < (tree@num.situation[level] - 1))) {
    aux.1 <- order.max.score[1]
        #order.max.score[1] identifies the first stage to be merged.
    aux.2 <-
      order.max.score[1] + order.pair.score[[order.max.score[1]]][1]
    #aux identifies the second stage to be merged
    cluster[[aux.1]] <- c(cluster[[aux.1]], cluster[[aux.2]])
    cluster[[aux.2]] <- NA

    vec.score[c(aux.1, aux.2)] <- c(max.score[aux.1] +
                                      sum(vec.score[c(aux.1, aux.2)]), NA)

    #To update the score w.r.t. this new stage configuration
    prior.distribution[[level]][aux.1, ] <-
      prior.distribution[[level]][aux.1, ] + prior.distribution[[level]][aux.2,]

    #To update the prior.distribution tables
    prior.distribution[[level]][aux.2, ] <- aux.na
    contingency.table[[level]][aux.1, ] <-
      contingency.table[[level]][aux.1, ] + contingency.table[[level]][aux.2, ]

    #To update the contingency.table tables
    contingency.table[[level]][aux.2, ] <- aux.na
    pair.score[[aux.1]] <-
      sapply((aux.1 + 1):tree@num.situation[level], function(x)
        PairwiseScore(aux.1, x, vec.score, prior.distribution[[level]],
                       contingency.table[[level]]))

    #To update the score w.r.t. the first merged stage
    order.pair.score[[aux.1]] <-
      order(pair.score[[aux.1]], decreasing = TRUE)
    count.na[aux.1] <- count.na[aux.1] + 1
    max.score[aux.1] <-
      pair.score[[aux.1]][order.pair.score[[aux.1]][1]]
    if (is.na(max.score[aux.1])) {
      ref <- which(order.max.score == aux.1)
      if (ref != tree@num.situation[level]) {
        order.max.score[ref:(tree@num.situation[level] - 2)] <-
          order.max.score[(ref + 1):(tree@num.situation[level] - 1)]
      }
      order.max.score[tree@num.situation[level] - 1] <- NA
      pair.score[[aux.1]] <- NA
      count.na[aux.1] <- NA
      order.pair.score[aux.1] <- NA
      count <- count + 1
    } else {
      # TODO(Collazo) the next lines are presenting NA in the vector.
      # it is necessary to check why NA was inserted and avoid this
      # only removing NA by hands doesn't solve (I tried.. using the following
      # technic: vec <- vec[!is.na(vec)]     .....   :(


      #  problematic lines - findInterval
      ref <- tree@num.situation[level] -
        findInterval(max.score[aux.1], max.score[order.max.score[
          (tree@num.situation[level] - count - 1):2]]) - count - 1
      if (ref != 1) {
        order.max.score[1:(ref - 1)] <- order.max.score[2:ref]
        order.max.score[ref] <- aux.1
      }
    }

    if (aux.2 < tree@num.situation[level]) {
      # To update the score w.r.t. the second merged stage

      if (!is.na(count.na[aux.2])) {
        pair.score[[aux.2]] <- NA
        count.na[aux.2] <- NA
        order.pair.score[[aux.2]] <- NA
        max.score[aux.2] <- NA
        ref <- which(order.max.score == aux.2)
        if (ref < tree@num.situation[level] - 1) {
          order.max.score[ref:(tree@num.situation[level] - 2)] <-
            order.max.score[(ref + 1):(tree@num.situation[level] - 1)]
        }
        order.max.score[tree@num.situation[level] - 1] <- NA
        count <- count + 1
      }
    }

    if (aux.1 > 1) {
      # To update stages that precede the first merged stage in terms of
      # lexicographical order
      aux.a <- aux.1
      aux.b <- aux.2
      for (i in 1:(aux.1 - 1)) {
        aux.a <- aux.a - 1
        aux.b <- aux.b - 1
        if (!is.na(count.na[i])) {
          pair.score[[i]][aux.a] <-
            PairwiseScore(i, aux.1, vec.score, prior.distribution[[level]],
                           contingency.table[[level]])
          #To update the score w.r.t. the first stage
          flag <- order.pair.score[[i]][1]
          order.pair.score[[i]] <-
            SingleReorder(aux.a, order.pair.score[[i]],
                           pair.score[[i]], count.na[i])
          if (pair.score[[i]][aux.a] >=
              pair.score[[i]][order.pair.score[[i]][1]] ||
              flag == aux.a) {
            # The first stage is the best score, or the first stage was
            # but it is not the best score now.
            max.score[i] <-
              pair.score[[i]][order.pair.score[[i]][1]]
            order.max.score <-
              SingleReorder(i, order.max.score, max.score, count)
          }

          pair.score[[i]][aux.b] <-
            NA  #To update the score w.r.t. the second stage.
          flag.1 <- order.pair.score[[i]][1]
          order.pair.score[[i]] <-
            NaReorder(aux.b, order.pair.score[[i]])
          count.na[i] <- count.na[i] + 1
          if (flag == aux.b || flag.1 == aux.b) {
            # The second stage was but it is not the best score now.
            max.score[i] <-
              pair.score[[i]][order.pair.score[[i]][1]]
            order.max.score <-
              SingleReorder(i, order.max.score, max.score, count)
          }
        }
      }
    }

    if (aux.2 - aux.1 > 1) {
      # To update the score w.r.t. the second stage.
      aux.b <- aux.2 - aux.1
      for (i in (aux.1 + 1):(aux.2 - 1)) {
        aux.b <- aux.b - 1
        if (!is.na(count.na[i])) {
          pair.score[[i]][aux.b] <- NA
          flag <- order.pair.score[[i]][1]
          order.pair.score[[i]] <-
            NaReorder(aux.b, order.pair.score[[i]])
          count.na[i] <- count.na[i] + 1
          if (count.na[i] == tree@num.situation[level] - i) {
            pair.score[[i]] <- NA
            count.na[i] <- NA
            order.pair.score[[i]] <- NA
            max.score[i] <- NA
            count <- count + 1
            ref <- which(order.max.score == i)
            if (ref != tree@num.situation[level]) {
              order.max.score[ref:(tree@num.situation[level] - 2)] <-
                order.max.score[(ref + 1):(tree@num.situation[level] - 1)]
            }
            order.max.score[tree@num.situation[level] - 1] <- NA

          } else {
            if (flag == aux.b) {
              # The second stage was but it is not the best score now.
              max.score[i] <-
                pair.score[[i]][order.pair.score[[i]][1]]
              order.max.score <-
                SingleReorder(i, order.max.score, max.score, count)
            }
          }
        }
      }
    }
  }
  temp <-
    new("OAHC",
        score = sum(vec.score, na.rm = TRUE),
        cluster = cluster)
  return(temp)
}


# TODO(Collazo) documentar
# SingleReorder
#
# This function reorders a vector that had an element changed.
#
# @param x TODO(Colazzo) Ampliar com tipo de dado e significado semantico
# @param order.score TODO(Colazzo) Ampliar com tipo de dado e significado semantico
# @param score TODO(Colazzo) Ampliar com tipo de dado e significado semantico
# @param na.count TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#
SingleReorder <- function(x, order.score, score, na.count) {
  const <- length(score) - na.count
  if (const == 1)
    return(order.score)
  flag <- which(order.score == x)

  if (flag == 1) {
    order.score <- order.score[2:const]
  } else if (flag == const) {
    order.score <- order.score[1:(const - 1)]
  } else {
    order.score <- order.score[c(1:(flag - 1), (flag + 1):const)]
  }


  ref <- const - findInterval(score[x], score[rev(order.score)])

  if (ref != const) {
    order.score[(ref + 1):(const + 1)] <- order.score[ref:const]
    order.score[ref] <- x
    order.score <- KeepLexOrder(ref, order.score, score)
  } else {
    order.score[ref] <- x
  }
  order.score <- c(order.score, rep(NA, na.count))
  return(order.score)
}



#' KeepLexOrder
#'
#' This function keep a lexicographical order of a vector
#  that have had one element
#'
#' @param ref numeric
# TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @param order.vector vector
# TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @param score.vector vector
# TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#'
#'
KeepLexOrder <- function(ref, order.vector, score.vector) {
  aux <- 0
  stop <- FALSE
  while (stop == FALSE) {
    if (score.vector[[order.vector[ref]]] ==
        score.vector[[order.vector[ref + 1]]] &&
        order.vector[ref] > order.vector[ref + 1]) {
      aux <- order.vector[ref + 1]
      order.vector[ref + 1] <- order.vector[ref]
      order.vector[ref] <- aux
      ref <- ref + 1
    } else {
      stop <- TRUE
    }
  }
  return(order.vector)
}

# TODO(Collazo) - documentar
# PairwiseScore
#
# This function calculates the score asscoiated with a pair of merged stages.
#
# @param stage.1 TODO(Colazzo) Ampliar com tipo de dado e significado semantico
# @param stage.2 TODO(Colazzo) Ampliar com tipo de dado e significado semantico
# @param vec.score TODO(Colazzo) Ampliar com tipo de dado e significado semantico
# @param prior.distribution TODO(Colazzo) Ampliar com tipo de dado e significado semantico
# @param contingency.table TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#
#
PairwiseScore <- function(stage.1, stage.2, vec.score, prior.distribution,
                           contingency.table) {
  if (is.na(vec.score[stage.1]) || is.na(vec.score[stage.2]))
    return(NA)
  alpha <- prior.distribution[stage.1,] + prior.distribution[stage.2,]
  N <- contingency.table[stage.1,] + contingency.table[stage.2,]
  score <-
    sum(lgamma(alpha + N)) - sum(lgamma(alpha)) + lgamma(sum(alpha)) -
    lgamma(sum(alpha + N)) - vec.score[stage.1] - vec.score[stage.2]
  return(score)
}




# TODO(Collazo) Documentar
# SingleScore
#
# This function calculates the score associated with a particular level.
#
# @param stage TODO(Colazzo) Ampliar com tipo de dado e significado semantico
# @param prior.distribution TODO(Colazzo) Ampliar com tipo de dado e significado semantico
# @param contingency.table TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#
SingleScore <- function(stage, prior.distribution, contingency.table) {
  alpha <- prior.distribution[stage,]
  N <- contingency.table[stage,]
  score <- sum(lgamma(alpha + N)) - sum(lgamma(alpha)) +
    lgamma(sum(alpha)) - lgamma(sum(alpha + N))
  return(score)
}

# TODO(COllazo) documentar
# NaReorder
#
# This function reorders a vector that had an element changed to NA.
#
# @param x TODO(Colazzo) Ampliar com tipo de dado e significado semantico
# @param order.score TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#
NaReorder <- function(x, order.score) {
  flag <- which(order.score == x)
  const <- length(order.score)
  if (flag == 1) {
    order.score <- c(order.score[2:const], NA)
  } else if (flag == const) {
    order.score[const] <- NA
  } else {
    order.score <- order.score[c(1:(flag - 1), (flag + 1):const, NA)]
  }
  return(order.score)
}
