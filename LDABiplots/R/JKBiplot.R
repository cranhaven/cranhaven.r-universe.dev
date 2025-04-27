#' @title JKBiplot
#' @description This function performs the representation of JK Biplot (Gabriel,1971).
#' @usage JKBiplot (X, Transform.Data = 'scale')
#' @param X array_like; \cr
#'     A data frame which provides the data to be analyzed. All the variables must be numeric.
#' @param Transform.Data character; \cr
#'     A value indicating whether the columns of X (variables) should be centered or scaled. The options are: "center" if center is TRUE, centering is done by subtracting the column means (omitting NA) of x from their corresponding columns, and if center is FALSE, centering is not done. "scale" the value of scale determines how column scaling is performed (after centering). If scale is a numeric-alike vector with length equal to the number of columns of x, then each column of x is divided by the corresponding value from scale. If scale is TRUE then scaling is done by dividing the (centered) columns of x by their standard deviations if center is TRUE, and the root mean square otherwise. If scale is FALSE, no scaling is done. To scale by standard deviations without centering, use scale(x,center=FALSE,scale=apply(x,2,sd,na.rm=TRUE)),"center_scale" center=TRUE and scale=TRUE,"none" neither center nor scale is done. The default value is "scale".
#' @details Algorithm used to construct the JK Biplot. The Biplot is obtained as result of the configuration of markers for individuals and markers for variables in a reference system defined by the factorial axes resulting from the Decomposition in Singular Values (DVS).
#' @return \code{JKBiplot} returns a list containing the following components:
#' \item{eigenvalues}{  array_like; \cr
#'           vector with the eigenvalues.
#'           }
#' \item{explvar}{  array_like; \cr
#'           an vector containing the proportion of variance explained by the first 1, 2,.,k principal components obtained.
#'           }
#' \item{loadings}{  array_like; \cr
#'           the loadings of the principal components.
#'           }
#' \item{coord_ind}{  array_like; \cr
#'           matrix with the coordinates of individuals.
#'           }
#' \item{coord_var}{  array_like; \cr
#'           matrix with the coordinates of variables.
#'           }
#' @references
#' \itemize{
#'  \item Gabriel, K. R. (1971). The Biplot graphic display of matrices with applications to principal components analysis. Biometrika, 58(3), 453-467.
#' }
#' @import stats
#' @examples
#'  JKBiplot(mtcars)
#' @export
JKBiplot <- function(X, Transform.Data = 'scale'){
  # List of objects that the function returns
  jkb <-
    list(
      eigenvalues = NULL,
      explvar = NULL,
      loadings = NULL,
      coord_ind = NULL,
      coord_var = NULL
    )
  # Sample's tags
  ind_tag <- rownames(X)
  # Variable's tags
  vec_tag <- colnames(X)
  #### 1. Transform data ####
  if (Transform.Data == 'center') {
    X <-
      scale(
        as.matrix(X),
        center = TRUE,
        scale = FALSE
      )
  }
  if (Transform.Data == 'scale') {
    X <-
      scale(
        as.matrix(X),
        center = FALSE,
        scale = apply(X,2,sd,na.rm=T)
      )
  }
  if (Transform.Data == 'center_scale') {
    X <-
      scale(
        as.matrix(X),
        center = TRUE,
        scale = TRUE
      )
  }
  if (Transform.Data == 'none') {
    X <- as.matrix(X)
  }
  #### 2. SVD decomposition ####
  svd <- svd(X)
  U <- svd$u
  d <- svd$d
  D <- diag(d)
  V <- svd$v
  #### 3. Components calculated ####
  PCs <- vector()
  for (i in 1:dim(V)[2]){
    npc <- vector()
    npc <- paste("Dim", i)
    PCs <- cbind(PCs, npc)
  }
  ##### 4. Output ####
  #### >Eigenvalues ####
  jkb$eigenvalues <- eigen(cor(X))$values
  names(jkb$eigenvalues) <- PCs
  #### > Explained variance ####
  jkb$explvar <-
    round(
      jkb$eigenvalues / sum(jkb$eigenvalues),
      digits = 4
    ) * 100
  names(jkb$explvar) <- PCs
  #### >Loagings ####
  jkb$loadings <- V
  row.names(jkb$loadings) <- vec_tag
  colnames(jkb$loadings) <- PCs
  #### >Row coordinates ####
  jkb$coord_ind <- U %*% D
  row.names(jkb$coord_ind) <- ind_tag
  colnames(jkb$coord_ind) <- PCs
  #### >Column coordinates ####
  jkb$coord_var <- V
  row.names(jkb$coord_var) <- vec_tag
  colnames(jkb$coord_var) <- PCs
  d1 = (max(jkb$coord_var[, 1]) - min(jkb$coord_var[,1]))/(max(jkb$coord_ind[, 1]) -  min(jkb$coord_ind[, 1]))
  d2 = (max(jkb$coord_var[, 2]) - min(jkb$coord_var[,2]))/(max(jkb$coord_ind[, 2]) -  min(jkb$coord_ind[, 2]))
  d = max(d1, d2)
  jkb$coord_var <- jkb$coord_var/d
  jkb
}