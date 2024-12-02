
Ldist <- function(x, y, w, p) {
  ldist <- ((w ^ p) %*% (abs(x - y) ^ p)) ^ (1 / p)
  return(ldist)
}

#' Technique for Order of Preference by Similarity to Ideal Solution (TOPSIS)
#'
#' @description This function implements Technique for Order of Preference by Similarity to Ideal Solution (TOPSIS).
#' This approach is based on the principle that the best solutions must be near to a positive ideal
#' solution \eqn{(I+)} and far from a negative ideal solution \eqn{(I-)} in the
#' criteria space. The weighted distance measure used to detect these similarities
#' allows the user to possibly assign different importance to the criteria considered.
#' The distance measure used is:
#' \deqn{L_p(a,b) = \left[ \sum_{j=1}^{m}(w_j)^p(|a-b|)^p\right] ^(1/p)}{%
#' L_p(a,b) = [ \sum{j=1}^{m}(w_j^p * |a-b|^p]^{(1/p)}}
#' The metric on the basis of which solution ranking occurs is:
#'
#' \deqn{S(x) = \frac{L_p(x,I-)}{(L_p(x,I+) + L_p(x,I-)}}{%
#' S(x) = L_p(x,I-) / (L_p(x,I+) + L_p(x,I-))}
#'
#'
#' @param out A list as the \code{megaAR} list returned by \code{\link[multiDoE]{runTPLS}}.
#' @param w A vector of weights. It must sum to 1. The default wights are uniform.
#' @param p A coefficient. It determines the type of distance used. The default value is 2.
#'
#' @return The function returns a list containing the following items:
#' \itemize{
#' \item{\code{ranking}: A dataframe containing the ranking values of S(x) and the
#' ordered indexes according to the TOPSIS approach (from the best to the worst).}
#' \item{\code{bestScore}: The scores of the best solution.}
#' \item{\code{bestSol}: The best solution.}
#' }
#'
#' @references
#' M. Méndez, M. Frutos, F. Miguel and R. Aguasca-Colomo. TOPSIS Decision on
#' Approximate Pareto Fronts by Using Evolutionary Algorithms: Application to an
#' Engineering Design Problem. Mathematics, 2020.
#' \url{https://www.mdpi.com/2227-7390/8/11/2072}
#'
#' @export
#'
topsisOpt <- function(out, w = NULL, p = 2) {
  if (is.null(w)){
    w <- rep(1/out$megaAR$dim, out$megaAR$dim)
  }

  paretoFront <- out$megaAR

  if (sum(w) != 1) {
    stop("Weight vector invalid.")

  } else {
    matrice <- paretoFront$scores

    # 1. normalized score matrix
    rw <- dim(matrice)[1]
    cl <- dim(matrice)[2]
    normPF <- matrix(NA, rw, cl)
    normPF <- apply(matrice, 2, function(x) sapply(x, function(y) y/sum(x)))

    # 2. ideal and nadir point
    ideal <- apply(normPF, 2, min)
    nadir <- apply(normPF, 2, max)

    # 3. calculation of distances
    ideal_dist <- apply(normPF, 1, function(x) Ldist(x, ideal, w, p))
    nadir_dist <- apply(normPF, 1, function(x) Ldist(x, nadir, w, p))

    # 4. ranking
    S <- vector(length = rw)
    for (i in 1:rw) {
      S[i] <- nadir_dist[i] / (ideal_dist[i] + nadir_dist[i])
    }
    ranking <- data.frame("paretoFront_pos" = c(1:rw), "S(x)" = S)
    ranking2 <- ranking[order(ranking$S.x., decreasing = T),]
    bestScore <- paretoFront$scores[ranking2[1, 1],]
    bestSol <- paretoFront$solutions[[ranking2[1, 1]]]
  }
  return(list("ranking" = ranking2, "bestScore" = bestScore, "bestSol" = bestSol))
}

#
# paretoFront <- tpls$megaAR
# prova <- topsisOpt(paretoFront, w = c(1/5, 1/5, 3/5), p = 2)
# esse <- ranking[,2]
# plot_ly(x=paretoFront$scores[,1],
#         y=paretoFront$scores[,2],
#         z=paretoFront$scores[,3],
#         type="scatter3d", mode="markers", color = esse)

