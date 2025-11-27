#' @title Plot probability curves
#' @description Plot the probability curve \code{P(Dij, mu, lamda)} for removing links from the initial complete network
#' @param Dij A vector of Euclidean distances between node \code{i} and \code{j}
#' @param mu The concave-to-convex transition point of the probability curves \code{P(Dij, mu, lamda) = [1 + exp(-lamda(Dij - mu))]^-1}, where \code{Dij} is the Euclidean distance between node \code{i} and \code{j}
#' @param lamda The steepness of the probability curves
#'
#' @export
#' @return Return a plot with probability curves
#' @examples
#' # plot the probabilities for removing network links between node i and j with
#' # Euclidean distances Dij
#'
#' dis <- seq(.05, 10, length.out = 20)
#' m <- c(.1, 2, 5, 10)
#' l <- c(.0001, .15, .35, .75, 1.25, 5, 30)
#' ahn_prob(dis, m, l)
#'
ahn_prob <- function(Dij = seq(.05, 10, length.out = 30), mu = c(.1, 2, 5, 10), lamda = c(.0001, .15, .35, .75, 1.25, 5, 30)){
  df <- data.frame()
  for(u in 1:length(Dij)){
    for(v in 1:length(mu)){
      for(w in 1:length(lamda)){
        t <- data.frame(Dij = Dij[u],
                        mu = paste('mu = ', as.character(mu[v]), sep = ''),
                        lamda = as.character(lamda[w]),
                        Prob = 1/(1 + exp(-lamda[w]*(Dij[u] - mu[v]))))
        df <- rbind(df, t)
      }
    }
  }
  return(ggplot(data = df, mapping = aes(x = df$Dij, y = df$Prob, color = lamda)) +
           geom_line() +
           geom_point(size = 0.5) +
           facet_wrap(facets =  vars(mu)) +
           ylab('Probability of removing the link between node i and j') +
           xlab('Euclidean distance between node i and j') +
           theme(panel.border = element_rect(fill = NA, color = "black", size = 0.5),
                 panel.grid.major = element_line(color = "black", size = .015),
                 panel.grid.minor = element_line(color = "black", size = .015),
                 panel.background = element_rect(fill = "transparent",colour = NA),
                 plot.background = element_rect(fill = "transparent",colour = NA),
                 axis.title.x = element_text(color = "black", size = 10),
                 axis.title.y = element_text(color = "black", size = 10),
                 axis.text.x = element_text(color = "black", size = 10),
                 axis.text.y = element_text(color = "black", size = 10),
                 axis.ticks = element_line(color = "black", size = .2)))
}
