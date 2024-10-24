#' @name plot.sprunstest
#' @rdname plot.sprunstest
#'
#' @title Plot the empirical distribution of runs
#'
#' @details Plot the histogram with the empirical distribution
#' of the runs
#' @param x  A object of class \emph{sprunstest}.
#' @param ... further arguments passed to or from other methods.
#' @return No return value, called for side effects
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paez@@gmail.com} \cr
#'   Manuel Ruiz  \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#'
#' @seealso
#' \code{\link{sp.runs.test}}.
#'
#'
#' @examples
#' # Example 1: Fastfood example. sf (points)
#' data("FastFood.sf")
#' x <- sf::st_coordinates(sf::st_centroid(FastFood.sf))
#' listw <- spdep::knearneigh(x, k = 2)
#' formula <- ~ Type
#' srq <- sp.runs.test(formula = formula, data = FastFood.sf, listw = listw, nsim = 299)
#' plot(srq)
#'
#' # Example 2: Spain example (poligons with 0 neinghbourhood)
#' data("provinces_spain")
#' sf::sf_use_s2(FALSE)
#' listw <- spdep::poly2nb(as(provinces_spain,"Spatial"), queen = FALSE)
#' provinces_spain$Older <- cut(provinces_spain$Older, breaks = c(-Inf,19,22.5,Inf))
#' levels(provinces_spain$Older) = c("low","middle","high")
#' formula <- ~ Older
#' srq <- sp.runs.test(formula = formula, data = provinces_spain, listw = listw, nsim = 299)
#' plot(srq)
#' provinces_spain$Mal2Fml <- factor(provinces_spain$Mal2Fml > 100)
#' levels(provinces_spain$Mal2Fml) = c("men","woman")
#' formula <- ~ Mal2Fml
#' srq <- sp.runs.test(formula = formula, data = provinces_spain, listw = listw, nsim = 299)
#' plot(srq)

#' @export
#'

plot.sprunstest <- function(x, ...){
srq <- x
runs <- srq$MaxNeig # max(as.numeric(levels(as.data.frame(srq$dnr)$Var1)))
fff <- matrix(0, ncol = 2, nrow = runs)
fff[,1] <- 1:runs
fff[,2] <- srq$dnr[,2]
fff <- as.data.frame(fff)
if (is.null(srq$nsim)){
    g1 <- ggplot(data = fff, aes(x = fff$V1, y = fff$V2)) +
    geom_bar(stat="identity", color = "black", fill = "steelblue") +
    labs(x = "Number of runs", y = "Frequency") +
    theme_bw()
    suppressWarnings(print(g1))
}  else {
aa <- matrix(0, ncol = runs, nrow = srq$nsim)
for (i in 1:srq$nsim){
aa[i,] <- as.data.frame(table(factor(srq$SRLP[,i], levels = c(1:runs))))$Freq
}
hh <- matrix(0, ncol = 2, nrow = runs)
for (i in 1:runs){
hh[i,] <- quantile(aa[,i],c(0.05,.95))
}
fff$min <- hh[,1]
fff$max <- hh[,2]
fff$mean <- colMeans(aa)

g1 <- ggplot(data = fff, aes(x = fff$V1, y = fff$V2)) +
  geom_bar(stat="identity",color = "black",
                    fill = "steelblue") +
  labs(x = "Number of runs", y = "Frequency") +
  geom_errorbar(data = fff, aes(x =  fff$V1, ymin = min, ymax = max), width = 0.3,
                colour = "red", alpha = 0.9, size = 1.1) +
  geom_point(data=fff, aes(x = fff$V1, y = mean), size = 3, shape = 21, fill = "white") +
  theme_bw()
suppressWarnings(print(g1))
  }
}





