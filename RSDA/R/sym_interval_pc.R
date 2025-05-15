#' Compute a symbolic interval principal components curves
#' @name sym.interval.pc
#' @aliases sym.interval.pc
#' @author Jorge Arce.
#' @description Compute a symbolic interval principal components curves
#' @usage sym.interval.pc(sym.data, method = c('vertex', 'centers'), maxit, plot, scale, center)
#' @param sym.data Shoud be a symbolic data table read with the function read.sym.table(...)
#' @param method It should be 'vertex' or 'centers'.
#' @param maxit Maximum number of iterations.
#' @param plot TRUE to plot immediately, FALSE if you do not want to plot.
#' @param scale TRUE to standardize the data.
#' @param center TRUE to center the data.
#'
#' @return
#' prin.curve: This a symbolic data table with the interval principal components. As
#' this is a symbolic data table we can apply over this table any other symbolic data
#' analysis method (symbolic propagation).
#'
#' cor.ps: This is the interval correlations between the original interval
#' variables and the interval principal components, it can be use to plot the symbolic
#' circle of correlations.
#' @references
#' Arce J. and Rodriguez O. (2015) 'Principal Curves and Surfaces
#' to Interval Valued Variables'. The 5th Workshop on Symbolic
#' Data Analysis, SDA2015, Orleans, France, November.
#'
#' Hastie,T. (1984).
#' Principal Curves and Surface. Ph.D Thesis Stanford University.
#'
#' Hastie,T. & Weingessel,A. (2014).
#' princurve - Fits a Principal Curve in Arbitrary Dimension.R package version 1.1--12
#' http://cran.r-project.org/web/packages/princurve/index.html.
#'
#' Hastie,T. & Stuetzle, W. (1989). Principal Curves.
#' Journal of the American Statistical Association, Vol. 84-406, 502--516.
#'
#' Hastie, T., Tibshirani, R. & Friedman, J. (2008).
#' The Elements of Statistical Learning; Data Mining, Inference and Prediction. Springer, New York.
#' @seealso sym.interval.pca
#' @examples
#' \dontrun{
#' data(oils)
#' res.vertex.ps <- sym.interval.pc(oils, "vertex", 150, FALSE, FALSE, TRUE)
#' class(res.vertex.ps$sym.prin.curve) <- c("sym.data.table")
#' sym.scatterplot(res.vertex.ps$sym.prin.curve[, 1], res.vertex.ps$sym.prin.curve[, 2],
#'   labels = TRUE, col = "red", main = "PSC Oils Data"
#' )
#'
#' data(facedata)
#' res.vertex.ps <- sym.interval.pc(facedata, "vertex", 150, FALSE, FALSE, TRUE)
#' class(res.vertex.ps$sym.prin.curve) <- c("sym.data.table")
#' sym.scatterplot(res.vertex.ps$sym.prin.curve[, 1], res.vertex.ps$sym.prin.curve[, 2],
#'   labels = TRUE, col = "red", main = "PSC Face Data"
#' )
#' }
#' @keywords Principal Curve
#' @export
#' @importFrom princurve principal.curve
#'
sym.interval.pc <- function(sym.data, method = c("vertex", "centers"), maxit, plot,
                            scale, center) {
  idn <- all(sym.data$sym.var.types == sym.data$sym.var.types[1])

  if (idn == FALSE) {
    stop("All variables have to be of the same type")
  }
  method <- match.arg(method)

  if ((sym.data$sym.var.types[1] != "$C") && (sym.data$sym.var.types[1] != "$I")) {
    stop("Variables have to be continuos or Interval")
  } else if (sym.data$sym.var.types[1] == "$C") {
    res <- princurve::principal_curve(sym.data$data, maxit = maxit)
  } else if (sym.data$sym.var.types[1] == "$I") {
    vertex <- vertex.interval(sym.data)
    individuals <- scale(as.matrix(vertex$vertex), scale = scale, center = center)


    if (method == "centers") {
      centers <- centers.interval(sym.data)

      res <- princurve::principal_curve(as.matrix(centers), maxit = maxit)

      n <- dim(individuals)

      projection.matrix <- matrix(data = NA, nrow = n[1], ncol = n[2])

      distance.vector <- rep(NA, n[1])

      lambda <- rep(NA, n[1])

      orthogonal.projection <- rep(NA, n[1])

      for (i in 1:n[1]) {
        neig <- neighbors.vertex(as.matrix(individuals[i, ]), res$s, 2)
        v <- -neig$neighbors[1, ] + neig$neighbors[2, ]
        vp <- -neig$neighbors[1, ] + individuals[i, ]
        proy <- sum(v * vp) / (norm.vect(v)^2) * v
        proy.point <- neig$neighbors[1, ] + proy
        projection.matrix[i, ] <- proy.point
        orthogonal.projection[i] <- sum((vp - proy) * v)
        distance.vector[i] <- norm.vect(vp - proy)

        lambda1 <- res$lambda[neig$order[1:2]]
        if (lambda1[1] <= lambda1[2]) {
          lambda[i] <- -lambda1[1] + norm.vect(proy)
        } else {
          lambda[i] <- lambda1[1] - norm.vect(proy)
        }
      }

      res.var.ind <- variance.princ.curve(data = individuals, curve = projection.matrix)
      res.var.mid <- variance.princ.curve(data = as.matrix(centers), curve = res$s)
      res.var <- list(res.var.ind = res.var.ind, res.var.mid = res.var.mid)
      colnames(projection.matrix) <- sym.data$sym.var.names
      res.limits <- sym.interval.pc.limits(
        sym.data = sym.data, prin.curve = projection.matrix,
        num.vertex = vertex$num.vertex, lambda = lambda, res.var$res.var.mid$var.order
      )

      num.vars <- sym.data$M
      variables <- rep("X", num.vars)
      for (i in 1:num.vars) {
        variables[res.var.ind$var.order[i]] <- paste0("prin_surface_", as.character(i))
      }

      colnames(projection.matrix) <- variables
      projection.matrix <- projection.matrix[, res.var.ind$var.order]
      correl <- cor(x = projection.matrix, y = vertex$vertex)
    } else if (method == "vertex") {
      res <- princurve::principal_curve(individuals, maxit = maxit)
      res.var <- variance.princ.curve(data = individuals, curve = res$s)

      res.limits <- sym.interval.pc.limits(
        sym.data = sym.data, prin.curve = res$s,
        num.vertex = vertex$num.vertex, lambda = res$lambda, res.var$var.order
      )
      num.vars <- sym.data$M
      variables <- rep("X", num.vars)
      for (i in 1:num.vars) {
        variables[res.var$var.order[i]] <- paste0("prin_surface_", as.character(i))
      }

      colnames(res$s) <- variables
      res$s <- res$s[, res.var$var.order]
      correl <- cor(x = res$s, y = vertex$vertex)
    }


    return(list(
      prin.curve = res, sym.prin.curve = res.limits, var.curve = res.var,
      cor.ps = correl
    ))
  }

  return(TRUE)
}
