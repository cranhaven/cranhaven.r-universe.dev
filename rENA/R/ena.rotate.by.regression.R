###
#' @title ENA Rotate by regression
#'
#' @description This function allows user to provide a regression formula for rotation on x and optionally on y.
#'    If regression formula for y is not provide, svd is applied to the residual data deflated by x to get y coordinates.
#'
#' @param enaset An \code{\link{ENAset}}
#' @param params list of parameters, may include:
#'     x_var: Regression formula for x direction, such as "lm(formula=V ~ Condition + GameHalf + Condition : GameHalf)".
#'     y_var: Regression formula for y direction (optional).
#'     points: A unit by connection weight matrix for rotation. If not provided, points in enaset are used.
#'     fullNames: If true, all independent variable names are included in the x and y names.
#'        Otherwise, only first variable name is used.
#'
#' @export
#' @return \code{\link{ENARotationSet}}
ena.rotate.by.hena.regression = function(enaset, params) {
  #x, y = NULL, points = NULL, fullNames = F){

  # check arguments
  if ( !is.list(params) || is.null(params$x_var) ) {
    stop("params must be provided as a list() and provide `x_var`")
  }
  x = params$x_var;
  y = params$y_var;
  points = params$points;
  fullNames = params$fullNames;
  if(is.null(fullNames)) {
    fullNames = F;
  }
  #get points

  if(!is.null(points)){
    p <- points
  }
  else  if (is.null(enaset$points.normed.centered)) {
    p <- as.matrix(enaset$model$points.for.projection)
  }
  else {
    p <- as.matrix(enaset$points.normed.centered)
  }

  #get variables

  # attach(enaset$meta.data,warn.conflicts = F)

  #regress to get v1 using x

  V = p
  # v1 = eval(parse(text = x))$coefficients[2,]
  v1 = with(enaset$meta.data, {
    eval(parse(text = x))$coefficients[2,]
  })

  # make v1  a unit vector

  norm_v1 = sqrt(sum(v1 * v1))
  if (norm_v1 != 0) {
    v1 = v1/norm_v1
  }

  # name v1 vector

  if(is.na(all.vars(parse(text = x)[[1]][["formula"]])[2])){
    xName = names(v1)[1]
  } else {
    if(fullNames){
      xName = parse(text = x)[[1]][["formula"]][[3]]
    } else {
      xName = all.vars(parse(text = x)[[1]][["formula"]])[2]
    }
  }

  # Save v1

  R = matrix(c(v1), ncol = 1)
  colnames(R) = c(paste0(xName,"_reg"))

  #deflate matrix by x dimension

  A = as.matrix(p)
  defA = as.matrix(A) - as.matrix(A) %*% v1 %*% t(v1)

  #if y formula is given, regress by y formula

  if (!is.null(y)){

    # regress to get v2 vector using formula y

    V = defA;
    v2 = eval(parse(text = y))$coefficients[2,]

    #make v2 a unit vector

    norm_v2 = sqrt(sum(v2 * v2))
    if (norm_v2 != 0) {
      v2 = v2/norm_v2
    }

    #name v2 vector

    if(is.na(all.vars(parse(text = y)[[1]][["formula"]])[2])){
      yName = names(v2)[1]
    } else {
      if(fullNames){
        yName = parse(text = y)[[1]][["formula"]][[3]]
      } else {
        yName = all.vars(parse(text = y)[[1]][["formula"]])[2]
      }
    }

    # save both v1 and v2

    R = cbind(v1, v2)
    colnames(R) = c(paste0(xName,"_reg"), paste0(yName,"_reg"))

    #deflat by v2

    defA = as.matrix(defA) - as.matrix(defA) %*% v2 %*% t(v2)
  }

  # get svd for deflated points

  svd_result = prcomp(defA, retx=FALSE, scale=FALSE, center=FALSE, tol=0)
  svd_v = svd_result$rotation

  # Merge rotation vectors
  vcount = ncol(R);
  colNamesR = colnames(R)
  combined = cbind(R, svd_v[, 1:(ncol(svd_v) - vcount)]);
  colnames(combined) = c(
    colNamesR,
    paste0("SVD", ((vcount + 1):ncol(combined)))
  );

  #create rotation set
  rotation_set <- ENARotationSet$new(
    node.positions = NULL,
    rotation = combined,
    codes = enaset$rotation$codes,
    eigenvalues = NULL
  )
  return(rotation_set)
}
