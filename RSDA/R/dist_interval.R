#' Compute the distance between two rows
#' @keywords internal
sym.Interval.distance <- function(sym.data, variable, w1, w2, gamma = 0.5, method = "Minkowski",
                                  normalize = TRUE) {
  if (method == "Gowda.Diday" | method == "Ichino" | method == "Minkowski" | method ==
    "Hausdorff") {
    result <- sym.data
    if (result$sym.var.types[variable] == "$I") {
      lenght <- result$sym.var.length[variable]
      ini <- result$sym.var.starts[variable]
      var <- result$meta[, ini:(ini + lenght - 1)]
      Intersect <- abs(max(var[w1, 1], var[w2, 1]) - min(var[w1, 2], var[
        w2,
        2
      ]))
      Union <- abs(min(var[w1, 1], var[w2, 1]) - max(var[w1, 2], var[w2, 2]))
      k <- abs(max(var[w1, 2], var[w2, 2]) - min(var[w1, 1], var[w2, 1]))
      total.lenght <- max(var[, 2]) - min(var[, 1])
      if (method == "Gowda.Diday") {
        D1 <- abs(abs((var[w1, 2] - var[w1, 1])) - abs((var[w2, 2] - var[
          w2,
          1
        ]))) / k
        if (var[w1, 1] > var[w2, 2] | var[w1, 2] < var[w2, 1]) {
          D2 <- (abs(var[w1, 2] - var[w1, 1]) + abs(var[w2, 2] - var[w2, 1])) / k
        } else {
          D2 <- (abs(var[w1, 2] - var[w1, 1]) + abs(var[w2, 2] - var[w2, 1]) -
            (2 * Intersect)) / k
        }
        D3 <- abs(var[w1, 1] - var[w2, 1]) / total.lenght
        Distance <- D1 + D2 + D3
      }
      if (method == "Ichino" | method == "Minkowski") {
        if (gamma > 0.5) {
          gamma <- 0.5
        }
        if (gamma < 0) {
          gamma <- 0
        }
        if (var[w1, 1] > var[w2, 2] | var[w1, 2] < var[w2, 1]) {
          Distance <- Union + gamma * (-abs(var[w1, 2] - var[w1, 1]) - abs(var[
            w2,
            2
          ] - var[w2, 1]))
        } else {
          Distance <- Union - Intersect + gamma * (2 * Intersect - abs(var[
            w1,
            2
          ] - var[w1, 1]) - abs(var[w2, 2] - var[w2, 1]))
        }
      }
      if (method == "Hausdorff") {
        Distance <- max(abs(var[w1, 1] - var[w2, 1]), abs(var[w1, 2] - var[
          w2,
          2
        ]))
      }
      if (normalize == TRUE) {
        Distance <- Distance / total.lenght
      }
    } else {
      Distance <- NA
    }
    return(Distance)
  }
  return("Invalid method")
}


#' Distance for Symbolic Interval Variables.
#' @name sym.dist.interval
#' @description This function computes and returns the distance matrix by using the specified
#' distance measure to compute distance between symbolic interval variables.
#'
#' @param sym.data A symbolic object
#' @param variables Numeric vector with the number of the variables to use.
#' @param gamma gamma value for the methods ichino and minkowski.
#' @param method Method to use (Gowda.Diday, Ichino, Minkowski, Hausdorff)
#' @param normalize A logical value indicating whether normalize the data in the ichino or hausdorff method.
#' @param SpanNormalize A logical value indicating whether
#' @param q q value for the hausdorff method.
#' @param euclidea A logical value indicating whether use the euclidean distance.
#' @param pond A numeric vector
#'
#' @return An object of class 'dist'
#' @export
#' @importFrom stats as.dist
sym.dist.interval <- function(sym.data, gamma = 0.5, method = "Minkowski", normalize = TRUE,
                              SpanNormalize = FALSE, q = 1, euclidea = TRUE, pond = rep(1, length(variables))) {
  sym.data <- to.v2(sym.data)
  variables <- (1:(sym.data$M))
  if (sum(pond) != length(variables) & sum(pond) > 1) {
    pond <- rep(1 / length(variables), length(variables))
  }

  for (med in 1:length(method)) {
    if (method[med] == "Gowda.Diday" | method[med] == "Ichino" | method[med] ==
      "Minkowski" | method[med] == "Hausdorff") {
      result <- sym.data
      h <- 1
      Dissimilarity.matrix <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))

      if (method[med] == "Minkowski") {
        for (var in variables) {
          Matrix <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
          Matrix_pond <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
          for (j in 1:nrow(result$data)) {
            for (i in j:nrow(result$data)) {
              Matrix[i, j] <- sym.Interval.distance(
                sym.data, var, i, j, gamma,
                method[med], normalize
              )
            }
          }
          if (q == 1) {
            Matrix_pond <- Matrix_pond + (Matrix * pond[h])
          } else {
            Matrix_pond <- Matrix_pond + (Matrix)^q
          }
          Dissimilarity.matrix <- Dissimilarity.matrix + Matrix_pond
          h <- h + 1
        }
        Dissimilarity.matrix <- as.dist(Dissimilarity.matrix^(1 / q))
      }

      if (method[med] == "Gowda.Diday" | method[med] == "Ichino") {
        for (var in variables) {
          Matrix <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
          Matrix_pond <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
          for (j in 1:nrow(result$data)) {
            for (i in j:nrow(result$data)) {
              Matrix[i, j] <- sym.Interval.distance(
                sym.data, var, i, j, gamma,
                method[med], normalize
              )
            }
          }
          Matrix_pond <- Matrix_pond + (Matrix * pond[h])
          Dissimilarity.matrix <- Dissimilarity.matrix + Matrix_pond
          h <- h + 1
        }
        Dissimilarity.matrix <- as.dist(Dissimilarity.matrix)
      }

      if (method[med] == "Hausdorff") {
        if (normalize == TRUE) {
          SpanNormalize <- FALSE
        }
        Dissimilarity.matrix <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
        for (var in variables) {
          Matrix <- matrix(c(0), ncol = nrow(result$data), nrow = nrow(result$data))
          for (j in 1:nrow(result$data)) {
            for (i in j:nrow(result$data)) {
              Matrix[i, j] <- sym.Interval.distance(
                sym.data, var, i, j, gamma,
                method[med], normalize
              )
            }
          }
          H <- ((1 / (2 * nrow(result$data)^2)) * sum(Matrix^2))^(1 / 2)
          if (SpanNormalize == TRUE) {
            Matrix <- Matrix / H
          }
          if (euclidea == TRUE) {
            Dissimilarity.matrix <- Dissimilarity.matrix + Matrix^2
          } else {
            Dissimilarity.matrix <- Dissimilarity.matrix + Matrix
          }
        }
        if (euclidea == TRUE) {
          Dissimilarity.matrix <- as.dist(Dissimilarity.matrix)^(1 / 2)
        } else {
          Dissimilarity.matrix <- as.dist(Dissimilarity.matrix)
        }
      }
    } else {
      return("Invalid method")
    }

    if (med == 1) {
      res <- list(Dissimilarity.matrix)
      names <- matrix(c(method[med]))
    } else {
      names <- rbind(names, method[med])
      res[[length(res) + 1]] <- Dissimilarity.matrix
    }
    names(res) <- names
  }
  res <- res[[1]]
  attr(res, "Labels") <- result$sym.obj.names
  return(res)
}
