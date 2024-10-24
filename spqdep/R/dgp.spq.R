#' @title Generation of qualitative process with spatial structure
#' @description The purpose of the function \code{dgp.spq} is to generate a random dataset
#' with the dimensions and spatial structure decided by the user. This function may be
#' useful in pure simulation experiments or with the aim of showing specific properties
#' and characteristics of a spatial qualitative dataset.
#' @usage dgp.spq(listw = listw, p = p,  rho = rho, control = list())
#' @param listw A \code{listw} object of the class nb, knn, listw o matrix created
#' for example by
#'   \code{\link[spdep]{nb2listw}} from \pkg{spatialreg} package; if
#'   \code{\link[spdep]{nb2listw}} not given, set to
#'   the same spatial weights as the \code{listw} argument. It can
#'   also be a spatial weighting matrix of order \emph{(NxN)} instead of
#'   a \code{listw} object. Default = \code{NULL}.
#' @param rho the level of spatial dependence (values between -1 y 1)
#' @param p a vector with the percentage of elements of each categories.
#' The lengths must be the number of categories.
#' The sum of the elements of vector must be 1.
#' @param control List of additional control arguments. See control argument section.
#' @return a factor of length N (\code{listw} is a matrix of order \emph{(NxN)}) with
#' levels the first capital letters: "A", "B", ....
#'
#' The description of the DGP is available in Páez et al. 2010 (pag 291) and in details section.
#' @section Control arguments:
#' \describe{
#' \item{seedinit}{seed to generate the data sets}
#' }
#'
#' @details
#' In order to obtain categorical random variables with controlled degrees
#'  of spatial dependence, we have designed a two- stage data generating process:\cr
#'  \cr
#'  Firstly, we simulate autocorrelated data using the following model:
#'  \deqn{Y = (I - \rho W)^{-1} \epsilon}
#'  where \eqn{\epsilon = N(0,1)} I is the \eqn{N \times N} identity matrix, \eqn{\rho} is a parameter
#'   of spatial dependence, and \emph{W} is a connectivity matrix that
#'   determines the set of spatial relationships among points.\cr
#'   \cr
#'   In the second step of the data generation process, the continuous spatially
#'   autocorrelated variable Y is used to define a discrete spatial process
#'   as follows. Let \eqn{b_{ij}} be defined by:\cr
#'   \deqn{p(Y \leq b_{ij})= {i \over j} \ \ \ with \ \ \ i<j}
#'   Let \eqn{A =\{a_1,a_2,...,a_k\}} and define the discrete spatial process as:
#'   \deqn{X_s=a_1 \ \ \ if \ \ \ Y_s \leq b_{1k}}
#'   \deqn{X_s=a_i \ \ \ if \ \ \ b_{i-1k} < Y_s \leq b_{ik}}
#'   \deqn{X_s=a_k \ \ \ if \ \ \ Y_s > b_{k-1k}}
#'
#' @seealso
#' \code{\link{Q.test}}, \code{\link{Q.map.test}}, \code{\link{sp.runs.test}}, \code{\link{scan.test}}
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Ruiz M, López FA, A Páez. (2010). \emph{Testing for spatial association of qualitative
#'     data using symbolic dynamics}. Journal of Geographical Systems. 12 (3) 281-309
#'   }
#' @export
#' @examples
#' #
#' N <- 100
#' cx <- runif(N)
#' cy <- runif(N)
#' coor <- cbind(cx,cy)
#' p <- c(1/6,3/6,2/6)
#' rho = 0.5
#' listw <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(cbind(cx,cy), k = 4)))
#' xf <- dgp.spq(list = listw, p = p, rho = rho)
#'
#' data(provinces_spain)
#' listw <- spdep::poly2nb(provinces_spain, queen = FALSE)
#' p <- c(1/6,3/6,2/6)
#' rho = 0.9
#' xf <- dgp.spq(p = p, listw = listw, rho = rho)
#' provinces_spain$xf <- xf
#' plot(provinces_spain["xf"])

dgp.spq <- function(listw = listw, p = p,  rho = rho, control = list()) {

  ################################################
  # Controls
  con <- list(seedinit = NULL)
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  if (length(noNms <- namc[!namc %in% nmsC]))
    warning("unknown names in control: ", paste(noNms, collapse = ", "))
  seedinit <- con$seedinit

  if (sum(p)!=1)  stop("The sum of p must be equal to 1")

if (inherits(listw, "knn")){
  listw <- nb2listw(knn2nb(listw)) }
if (inherits(listw, "listw")){
  listw <- listw2mat(listw) }
if (inherits(listw, "nb")){
  listw <- nb2mat(listw, style = "W", zero.policy = TRUE)}
if (inherits(listw, "matrix")) {
  listw <- listw/matrix(rowSums(listw),
                        ncol = dim(listw)[1],
                        nrow =dim(listw)[1])
  listw[is.na(listw)]<-0
  }
set.seed(seedinit)
n <- dim(listw)[1]
listw <- as(listw,"dgCMatrix")
k = 1
y <- Matrix::solve(Matrix::Diagonal(n) - rho*listw) %*%
        matrix(rnorm(n*k,mean = 0, sd = 1), n, k)
# y <- Matrix::solve(diag(n)-rho*listw)%*%rnorm(n,1)
y <- as.matrix(y)
Y <- cut(y, quantile(y,c(0, cumsum(p))),
         include.lowest = TRUE)
levels(Y) <- as.character(1:length(p))
levels(Y) <- LETTERS[1:length(p)]

return(Y)
}
