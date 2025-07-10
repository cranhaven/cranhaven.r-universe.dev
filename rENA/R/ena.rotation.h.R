#' @title hENA rotation for ENA
#'
#' @description hENA rotation function.
#'
#' @param enaset ena set
#' @param params list of parameters
#'
#' @return ena set
#' @export
ena.rotation.h <- function(
  enaset,
  params
) {
  # check arguments
    if ( !is.list(params) || is.null(params$x_var) ) {
      stop("params must be provided as a list() and provide `x_var`")
    }
    x_var = params$x_var;
    y_var = params$y_var;
    control_vars = params$control_vars;
    centering = ifelse(!is.null(params$centering), params$centering, TRUE);
    include_xy = ifelse(!is.null(params$include_xy), params$include_xy, FALSE);
    formula = params$formula;

  # get centered data
    if (!is.null(enaset$model$points.for.projection)) {
      data = data.table::copy(enaset$model$points.for.projection)
    }
    else {
      data = data.table::copy(enaset$points.normed.centered)
    }

  # Prep
    value_vars = colnames(as.matrix(data))
    data.table::set(x = data, j = value_vars, value = data[, lapply(.SD, function(x) x - mean(x)), .SDcols = value_vars])

  # dummy code x_var
    if (!is.numeric(data[[x_var]])) {
      x_var_f = paste0(x_var,"_f")
      data[[x_var_f]] = data.table::rleidv(x = data, cols = x_var) - 1
      x_var = x_var_f;
    }

  # dummy code y_var
    if (!is.null(y_var) && !is.numeric(data[[y_var]])) {
      y_var_f = paste0(y_var,"_f")
      data[[y_var_f]] = data.table::rleidv(x = data, cols = y_var) - 1
      y_var = y_var_f;
    }
    both_vars = c(x_var, y_var)

  # centering x_var and y_var
    if ( centering ) {
      data[, c(both_vars) := lapply(.SD, function(x) x - mean(x)), .SDcols = c(both_vars)]
    }

  # prepare regression formula
    f = paste(c(both_vars, control_vars), collapse = " + ")

    if ( include_xy ) {
      xy_var = paste(both_vars, collapse = "_");
      data[[xy_var]] = data[[x_var]] * data[[y_var]];
      f = paste(c(f, xy_var), collapse = " + ");
    }

    if (!is.null(formula)) {
      f = formula;
    }

  # run regression models and get slope variables
    v = matrix(sapply(value_vars, function(v) {
      formula = as.formula(paste0("data$`", v, "` ~ ", f));
      lm(formula, data = data)$coefficients[seq_along(both_vars) + 1];
    }), ncol = length(both_vars), byrow = TRUE)

  # Prep deflation
    R = NULL;
    '..value_vars' = NULL;
    A = as.matrix(data[, ..value_vars]);

  # Normalize x rotation vector
    v1 = v[, 1, drop = FALSE];
    norm_v1 = sqrt(sum( v1 * v1 ));
    if (norm_v1 != 0) {
      v1 = v1 / norm_v1;
      R = v1;
    }
    defA = as.matrix(A) - as.matrix(A) %*% v1 %*% t(v1);

  # Normalize y rotation vector, if applicable
    v2 = NULL;
    if (!is.null(y_var)) {
      v2 = v[, 2]
      v2 = as.numeric(v2) - as.numeric(t(v2) %*% v1) * v1;
      norm_v2 = sqrt(sum( v2 * v2 ));

      if (norm_v2 != 0) {
        v2 = v2 / norm_v2;
        if( is.null(R) ) {
          R = matrix(c(v2), ncol = 1)
        }
        else {
          R = matrix(c(R, v2), ncol = 2)
        }
      }

      defA = defA - defA %*% v2 %*% t(v2);
    }

  # get svd for deflated points
    # svd_result = svd(defA)
    # svd_v = svd_result$v;
    svd_result = prcomp(defA, retx=FALSE, scale=FALSE, center=FALSE, tol=0)
    svd_v = svd_result$rotation

  # Merge rotation vectors
    vcount = ncol(R);
    combined = cbind(R, svd_v[, 1:(ncol(svd_v) - vcount)]);

    colnames(combined) = c(
      paste(c("x","y")[seq_len(vcount)], both_vars[seq_len(vcount)], sep = "_"),
      paste0("SVD", ((vcount + 1):ncol(combined)))
    );

  # put into ENARotationSet
    # browser()
    rotation_set <- ENARotationSet$new(
      node.positions = NULL,
      rotation = combined,
      codes = enaset$rotation$codes,
      eigenvalues = svd_result$sdev ^ 2
    )

  # Done
    return(rotation_set)
}
