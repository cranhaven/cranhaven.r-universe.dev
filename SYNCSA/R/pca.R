#' @title Principal Components Analysis (PCA) with NA (missing data)
#'
#' @description The function use the option "pairwise.complete.obs" (in function \code{\link{cor}}) for
#' calculate the correlation. The correlation between each pair of variables is computed
#' using all complete pairs of observations on those variables.
#'
#' @encoding UTF-8
#' @importFrom graphics plot abline text points arrows
#' @aliases pca print.pcasyncsa
#' @param data A data frame  or matrix with individuals in rows and variables in columns.
#' @param x A object of class pcasyncsa.
#' @param show Draw "variables" or "individuals".
#' @param axis Axis for draw, must have length equal to two (Default axis = c(1, 2)).
#' @param xlab Text for x label (Default xlab = axis[1]).
#' @param ylab Text for y label (Default ylab = axis[2]).
#' @param arrows Logical argument (TRUE or FALSE) to specify if arrows are showed for variables (Default arrows = TRUE).
#' @param text Logical argument (TRUE or FALSE) to specify if text are showed for individuals (Default text = TRUE).
#' @param points Logical argument (TRUE or FALSE) to specify if points are showed for individuals (Default points = FALSE).
#' @param ... Parameters for \code{\link{plot}} function.
#' @return \item{decomposition}{list with the results of decomposition of correlation matrix.}
#' \item{eigenvalues}{Data frame containing all the eigenvalues, the
#' percentage of inertia and the cumulative percentage of inertia.}
#' \item{individuals}{Coordinates for the individuals.}
#' \item{variables}{Correlation between original variables and axes.}
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{syncsa}}
#' \code{\link{syncsa}}
#' @keywords SYNCSA
#' @examples
#' data(ADRS)
#' traits<-ADRS$traits
#' # Some NA
#' traits[c(1,5),1]<-NA
#' traits[3,2]<-NA
#' traits
#' res<-pca(traits)
#' res
#' plot(res, show = "variables", arrows = TRUE)
#' plot(res, show = "individuals", axis = c(1, 2), text = TRUE)
#' plot(res, show = "individuals", text = FALSE, points = TRUE)
#' @export
pca <- function(data)
{
  res <- list(call = match.call())
  colnames(data) <- colnames(data, do.NULL = FALSE, prefix = "Var.")
  rownames(data) <- rownames(data, do.NULL = FALSE, prefix = "Ind.")
  COR <- cor(data, use = "pairwise.complete.obs")
  data.norm <- as.matrix(cent.norm(data, na.rm = TRUE))
  data.NA <- apply(data.norm, 2, is.na)
  data.norm[data.NA] <- 0
  RES_eig <- eigen(COR)
  res$decomposition <- RES_eig
  eig <- RES_eig$values[RES_eig$values>=0]
  res.eig <- data.frame(Eigenvalues = eig, Perc.Inertia = eig/sum(eig), Cum.Inertia = cumsum(eig/sum(eig)))
  rownames(res.eig) <- paste("Axis.", 1:length(eig), sep = "")
  res$eigenvalues <- res.eig
  ind.coord <- data.norm%*%RES_eig$vectors[, RES_eig$values>=0, drop = FALSE]
  colnames(ind.coord) <- paste("Axis.", 1:ncol(ind.coord), sep = "")
  # var.coord <- cor(data, ind.coord, use = "pairwise.complete.obs")
  var.coord <- RES_eig$vectors[, RES_eig$values>=0, drop = FALSE]%*%diag(sqrt(eig))
  colnames(var.coord) <- paste("Axis.", 1:ncol(var.coord), sep = "")
  rownames(var.coord) <- colnames(data)
  res$individuals <- ind.coord
  res$variables <- var.coord
  class(res) <- "pcasyncsa"
  return(res)
}
