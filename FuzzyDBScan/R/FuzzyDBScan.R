# FuzzyDBScan-------------------------------------------------------------------

#' Fuzzy DBScan
#'
#' @description
#' This object implements fuzzy DBScan with both, fuzzy cores and fuzzy borders.
#' Additionally, it provides a predict function.
#'
#' @details
#' A method to initialize and run the algorithm and a function
#' to predict new data.
#' The package is build upon the paper
#' "Fuzzy Extensions of the DBScan algorithm" from Ienco and Bordogna.
#' The predict function assigns new data based on the same criteria as the
#' algorithm itself.
#' However, the prediction function freezes the algorithm to preserve the
#' trained cluster structure and treats each new prediction object individually.
#' Note, that border points are included to the cluster.
#'
#' @references
#' Ienco, Dino, and Gloria Bordogna.
#' Fuzzy extensions of the DBScan clustering algorithm.
#' Soft Computing 22.5 (2018): 1719-1730.
#'
#' @examples
#' # load factoextra for data and ggplot for plotting
#' library(factoextra)
#' dta = multishapes[, 1:2]
#' eps = c(0, 0.2)
#' pts = c(3, 15)
#' # train DBScan based on data, ep and pts
#' cl = FuzzyDBScan$new(dta, eps, pts)
#' # Plot DBScan for x and y
#' library(ggplot2)
#' cl$plot("x", "y")
#' # produce test data
#' x <- seq(min(dta$x), max(dta$x), length.out = 50)
#' y <- seq(min(dta$y), max(dta$y), length.out = 50)
#' p_dta = expand.grid(x = x, y = y)
#' # predict on test data and plot results
#' p = cl$predict(p_dta, FALSE)
#' ggplot(p, aes(x = p_dta[, 1], y = p_dta[, 2], colour = as.factor(cluster))) +
#'   geom_point(alpha = p$dense)
#' @import R6
#' @import checkmate
#' @import data.table
#' @importFrom dbscan frNN
#' @import ggplot2
#' @name Fuzzy_DBScan
#' @export
FuzzyDBScan = R6Class("FuzzyDBScan",
                      public = list(
                        #' @description Create a FuzzyDBScan object. Apply the
                        #'  fuzzy DBScan algorithm given the data `dta`, the
                        #'  range of the radius `eps` and the range of the
                        #'  Points `pts`.
                        #' @param dta [data.frame] | [matrix]\cr
                        #'  The data to be clustered by the algorithm. Allowed
                        #'  are only [numeric] columns.
                        #' @param eps [numeric]\cr
                        #'  The size (radius) of the epsilon neighborhood.
                        #'  If  the radius contains 2 numbers, the fuzzy cores
                        #'  are calculated between the minimum and the maximum
                        #'  radius.
                        #'  If epsilon is a single number, the algorithm
                        #'  looses the fuzzy core property. If the length of
                        #'  `pts` is also 1L, the algorithm equals to non-fuzzy
                        #'  DBScan.
                        #' @param pts [numeric]\cr
                        #' number of maximum and minimum points required in the
                        #' `eps`  neighborhood for core points (excluding the
                        #' point itself). If the length of the argument is 1,
                        #' the algorithm looses its fuzzy border property. If
                        #' the length of `eps` is also 1L, the algorithm equals
                        #' to non-fuzzy DBScan.
                        initialize = function(dta, eps, pts){
                          if(is.matrix(dta)) dta = data.table(dta)
                          assert_data_frame(dta, types = "numeric",
                                            min.rows = 2L, min.cols = 1L)
                          assert_numeric(eps, lower = 0L, any.missing = FALSE,
                                         min.len = 1L, max.len = 2L)
                          assert_integerish(pts, lower = 1L, upper = nrow(dta),
                                            any.missing = FALSE, min.len = 1L,
                                            max.len = 2L)
                          self$dta = dta
                          self$eps = eps
                          self$pts = pts
                          private$currentPoint = 0L
                          private$border_n = list()
                          self$clusters = rep(0L, times = nrow(dta))
                          self$dense = rep(0L, times = nrow(dta))
                          self$point_def = rep(NA, times = nrow(dta))
                          for(i in 1L:nrow(self$dta)) {
                            if(self$clusters[i] != 0L) next
                            neighbors = private$get_neighbors(dta, dta[i, ])
                            private$neighbors_dist = neighbors$dist
                            if(sum(private$neighbors_dist) < min(pts)) {
                              self$clusters[i] = -1L
                              self$dense[i] = 1L
                            } else {
                              private$neighbors_id = neighbors$id
                              private$currentPoint = private$currentPoint + 1L
                              private$expand(i)
                            }
                          }
                          self$point_def[self$clusters == -1L] = "Noise"
                          self$point_def[private$border_id] = "Border Point"
                          b_densities = lapply(private$border_n, private$get_border_density)
                          lapply(b_densities, function(x) self$dense[x[1]] = x[2])
                          self$results = private$return_probs(self$clusters, self$dense)
                        },
                        
                        #' @description Predict new data with the initialized
                        #' algorithm.
                        #' @param new_data [data.frame] | [matrix]\cr
                        #'  The data to be predicted by the algorithm. Allowed
                        #'  are only [numeric] columns which should match to
                        #'  `self$dta`.
                        #' @param cmatrix [logical]\cr
                        #'  Indicating whether the assigned cluster should be
                        #'  returned in form of a matrix where each column
                        #'  indicates for the probability of the new data to
                        #'  belong to a respective cluster. The object will have
                        #'  the same shape as the `results` field. If set to
                        #'  `FALSE` the shape of the returned assigned clusters
                        #'  is a two-column [data.table] with one column
                        #'  indicating the assigned cluster and the second
                        #'  column indicating the respective probability of
                        #'  the new data.
                        predict = function(new_data, cmatrix = TRUE){
                          if(is.matrix(self$dta)) self$dta = data.table(self$dta)
                          assert_data_frame(self$dta, ncols = ncol(self$dta),
                                            types = c("numeric", "integerish"),
                                            any.missing = FALSE, min.rows = 1L)
                          assert_logical(cmatrix, any.missing = FALSE, len = 1L)
                          result = apply(new_data, 1, private$predict_observation)
                          cluster_df = rbindlist(result)
                          if(!cmatrix) return(cluster_df)
                          private$return_probs(cluster_df$cluster, cluster_df$dense)
                        },
                        
                        #' @description Plot clusters and soft labels on two
                        #'  features.
                        #' @param x [character]\cr
                        #' Feature to plot on the x-axis.
                        #' @param y [character]\cr
                        #' Feature to plot on the y-axis.
                        plot= function(x, y){
                          assert_character(x, any.missing = FALSE, len = 1L)
                          assert_character(y, any.missing = FALSE, len = 1L)
                          cluster_num = as.factor(self$clusters)
                          ggplot(data = self$dta, aes(!!!list(x = sym(x), y = sym(y)), colour = cluster_num, alpha = self$dense)) +
                            geom_point()
                        },
                        
                        #' @field dta [data.frame] | [matrix]\cr
                        #'  The data to be clustered by the algorithm. Allowed
                        #'  are only [numeric] columns.
                        dta = NULL,
                        
                        #' @field eps [numeric]\cr
                        #'  The size (radius) of the epsilon neighborhood.
                        #'  If  the radius contains 2 numbers, the fuzzy cores
                        #'  are calculated between the minimum and the maximum
                        #'  radius.
                        #'  If epsilon is a single number, the algorithm
                        #'  looses the fuzzy core property. If the length of
                        #'  `pts` is also 1L, the algorithm equals to non-fuzzy
                        #'  DBScan.
                        eps = NULL,
                        
                        #' @field pts [numeric]\cr
                        #'  number of maximum and minimum points required in the
                        #'  `eps`  neighborhood for core points (excluding the
                        #'  point itself). If the length of the argument is 1,
                        #'  the algorithm looses its fuzzy border property. If
                        #'  the length of `eps` is also 1L, the algorithm equals
                        #'  to non-fuzzy DBScan.
                        pts = NULL,
                        
                        #' @field clusters [factor]\cr
                        #'  Contains the assigned clusters per observation in
                        #'  the same order as in `dta`.
                        clusters = NULL,
                        
                        #' @field dense [numeric]\cr
                        #'  Contains the assigned density estimates per
                        #'  observation in the same order as in `dta`.
                        dense = NULL,
                        
                        #' @field point_def [character]\cr
                        #'  Contains the assigned definition estimates per
                        #'  observation in the same order as in `dta`. Possible
                        #'  are "Core Point", "Border Point" and "Noise".
                        point_def = NULL,
                        
                        #' @field results [data.table]\cr
                        #'  A table where each column indicates for the
                        #'  probability of the new data to belong to a
                        #'  respective cluster.
                        results = NULL
                      ),
                      private = list(
                        expand = function(point){
                          self$clusters[point] = private$currentPoint
                          self$dense[point] = private$get_density(private$neighbors_dist)
                          self$point_def[point] = "Core Point"
                          j = 1L
                          while(j > 0L) {
                            nextPoint = private$neighbors_id[j]
                            if(self$clusters[nextPoint] %in% c(-1L, 0L)){
                              self$clusters[nextPoint] = private$currentPoint
                              nextNeighbors = private$get_neighbors(self$dta, self$dta[nextPoint, ])
                              if(sum(nextNeighbors$dist) >= min(self$pts)) {
                                self$dense[nextPoint] = private$get_density(nextNeighbors$dist)
                                self$point_def[nextPoint] = "Core Point"
                                new_neighbours = setdiff(nextNeighbors$id, private$neighbors_id)
                                private$neighbors_id = c(private$neighbors_id,
                                                         nextNeighbors$id)
                              } else {
                                private$border_n = append(private$border_n,
                                                          list(c("id" = nextPoint, nextNeighbors$id))
                                )
                                private$border_id = c(private$border_id, nextPoint)
                              }
                            }
                            j = ifelse(length(private$neighbors_id) <= j, 0L, j + 1L)
                          }
                        },
                        get_neighbors = function(dta, query){
                          n_list = frNN(dta, max(self$eps), query)
                          dist = (max(self$eps) - n_list$dist[[1]][-1]) / (max(self$eps) - min(self$eps))
                          dist[dist > 1L] = 1
                          list("id" = n_list$id[[1L]][-1L],
                               "dist" = dist
                          )
                        },
                        get_density = function(n_dist){
                          if(sum(n_dist) > max(self$pts)) return(1)
                          if(length(self$pts) == 1L) return(sum(n_dist) / self$pts)
                          (sum(n_dist) - min(self$pts)) / (max(self$pts) - min(self$pts))
                        },
                        get_border_density = function(b_vec){
                          fcores = setdiff(b_vec, private$border_id)
                          c("id" = b_vec["id"],
                            "dense" = min(self$dense[fcores]))
                        },
                        return_probs = function(clusters, dense){
                          ids = sort(unique(self$clusters))
                          prob_per_cl = lapply(ids, function(x){
                            vec = rep(0, length(clusters))
                            vec[clusters == x] = dense[clusters == x]
                            vec
                          })
                          prob_df = do.call(cbind.data.frame, prob_per_cl)
                          colnames(prob_df) = ids
                          prob_df[, 1] = 1 - apply(prob_df[, -1, drop=FALSE], 1, sum)
                          data.table(prob_df)
                        },
                        predict_observation = function(x) {
                          x = matrix(x, nrow = 1)
                          neigh = private$get_neighbors(self$dta, query = x)
                          n_clusters = self$clusters[neigh$id]
                          # case 1: point is noise if there is no core point around
                          if(sum(neigh$dist) < min(self$pts)) {
                            if(!any(self$point_def[neigh$id] == "Core Point")){
                              return(data.table("cluster" = -1L, "dense" = 1L))
                            }
                          }
                          # Delete noise around pt
                          if(any(n_clusters == -1L)) {
                            neigh$id = neigh$id[n_clusters != -1]
                            neigh$dist = neigh$dist[n_clusters != -1]
                            n_clusters = n_clusters[n_clusters != -1]
                          }
                          #Should be length 1
                          cluster = unique(self$clusters[neigh$id])
                          if(length(cluster) > 1L){
                            # Point in neighborhood are assigned to multiple clusters assigned.
                            # Closest pt cluster assignment is used as reference.
                            cluster = n_clusters[which.min(neigh$dist)]
                            neigh$id = neigh$id[n_clusters == cluster]
                            neigh$dist = neigh$dist[n_clusters == cluster]
                          }
                          # case 2: new core pt
                          if(sum(neigh$dist) >= min(self$pts)){
                            return(
                              data.table("cluster" = cluster,
                                         "dense" = private$get_density(neigh$dist)))
                          }
                          # Border Point: neighbordist < minpts but core point around
                          # Density is the minimal dense of a point around
                          idx = which.min(self$dense[neigh$id])
                          id = neigh$id[idx]
                          data.table("cluster" = self$clusters[id],
                                     "dense" = self$dense[id])
                        },
                        currentPoint = NULL,
                        neighbors_id = NULL,
                        neighbors_dist = NULL,
                        border_id = NULL,
                        border_n = NULL
                      )
)
