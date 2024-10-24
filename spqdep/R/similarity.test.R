#'
#' @title Compute the similarity test.
#'
#' @description This function compute the nonparametric test for spatial independence
#' using symbolic analysis for categorical/qualitative spatial process.
#'
#' @param data an (optional) data frame or a sf object containing the variable to testing for.
#' @param formula a symbolic description of the factor (optional).
#' @param fx a factor (optional).
#' @param nsim Number of permutations.
#' @param listw a listw object
#' @param alternative a character string specifying the type of cluster, must be one
#' of "High" (default), "Both" or "Low".
#' @param distr A string. Distribution of the test "asymptotic" (default) or "bootstrap"
#' @param control List of additional control arguments.
#' @usage similarity.test(formula = NULL, data = NULL, fx = NULL, listw = listw,
#' alternative = "two.sided", distr = "asymptotic", nsim = NULL, control = list())
#'
#' @return A object of the \emph{htest}
#'   \tabular{ll}{
#'     \code{data.name} \tab a character string giving the names of the data.\cr
#'     \code{statistic} \tab Value of the similarity test \cr
#'     \code{N} \tab total number of observations.\cr
#'     \code{Zmlc} \tab Elements in the Most Likelihood Cluster.\cr
#'     \code{alternative} \tab a character string describing the alternative hypothesis. \cr
#'     \code{p.value} \tab p-value of the similarity test \cr
#'     \code{similiarity.mc} \tab values of the similarity test in each permutation. \cr
#'     }
#' @section Control arguments:
#'   \tabular{ll}{
#'     \code{seedinit} \tab Numerical value for the seed (only for boot version). Default value seedinit=123 \cr
#'       }
#' @details
#' This testing approach for spatial independence that extends some of the properties
#'  of the join count statistic. The premise of the tests is similar to the join count
#'  statistic in that they use the concept of similarity between neighbouring spatial
#'  entities (Dacey 1968; Cliff and Ord 1973, 1981). However, it differs by taking
#'  into consideration the number of joins belonging locally to each spatial unit,
#'  rather than the total number of joins in the entire spatial system. The approach
#'  proposed here is applicable to spatial and network contiguity structures, and
#'  the number of neighbors belonging to observations need not be constant.\cr
#'
#'  We define an equivalence relation \eqn{\sim} in the set of locations S. An equivalence
#'  relation satisfies the following properties:\cr \cr
#'  Reflexive: \eqn{s_i \sim s_i} for all \eqn{s_i \in S},\cr
#'  Symmetric: If \eqn{s_i \sim s_j}, then \eqn{s_j \sim s_i} for all \eqn{s_i,\ s_j \in S} and\cr
#'  Transitive: If \eqn{s_i \sim s_j} and \eqn{s_j \sim s_k}, then \eqn{s_i \sim s_k}
#'  for all \eqn{s_i, \  s_j, \ s_k \in S}\cr \cr
#'  We call the relation \eqn{\sim} a similarity relation. Then, the null hypothesis that
#'  we are interested in is
#'  \deqn{H_0: \{X_s\}_{s \in S} \ \ is\ \ iid}
#'  Assume that, under the null hypothesis, \eqn{P(s_i \sim s_{ji}) = p_i} for all
#'  \eqn{s_{ji} \in N_{s_i}}. \cr
#'  Define\cr
#'  \deqn{I_{ij}=1 \ \ if \ \ s_i \sim s_{ji} \ \ ; 0 \ \ otherwise}\cr
#'  Then, for a fixed degree d and for all location si with degree d, the variable d\cr
#'  \deqn{\Lambda_{(d,i)}=\sum_{j=1}^d I_{ij}}
#'  gives the number of nearest neighbours to si that are similar to si.
#'  Therefore, under the null hypothesis, \eqn{H_0}, \eqn{\Lambda(d,i)} follows a binomial
#'  distribution \eqn{B(d, p_i)}.
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Farber, S., Marin, M. R., & Paez, A. (2015).
#'     Testing for spatial independence using similarity relations.
#'       \emph{Geographical Analysis}. 47(2), 97-120.
#'   }
#' @seealso
#' \code{\link{sp.runs.test}}, \code{\link{dgp.spq}}, \code{\link{Q.test}}, , \code{\link{scan.test}}
#' @export
#'
#' @examples
#'
#' # Case 1:
#' N <- 100
#' cx <- runif(N)
#' cy <- runif(N)
#' listw <- spdep::knearneigh(cbind(cx,cy), k = 3)
#' p <- c(1/4,1/4,1/4,1/4)
#' rho <- 0.5
#' fx <- dgp.spq(p = p, listw = listw, rho = rho)
#' W <- (spdep::nb2mat(spdep::knn2nb(listw)) >0)*1
#' similarity <- similarity.test(fx = fx, data = FastFood.sf, listw = listw)
#' print(similarity)
#'
#' # Case 2: test with formula, a sf object (points) and knn
#' data("FastFood.sf")
#' coor <- sf::st_coordinates(sf::st_centroid(FastFood.sf))
#' listw <- spdep::knearneigh(coor, k = 4)
#' formula <- ~ Type
#' similarity <- similarity.test(formula = formula, data = FastFood.sf, listw = listw)
#' print(similarity)
#'
#' # Case 3:
#' data(provinces_spain)
#' listw <- spdep::poly2nb(as(provinces_spain,"Spatial"), queen = FALSE)
#' provinces_spain$Mal2Fml <- factor(provinces_spain$Mal2Fml > 100)
#' levels(provinces_spain$Mal2Fml) = c("men","woman")
#' formula <- ~ Mal2Fml
#' similarity <- similarity.test(formula = formula, data = provinces_spain, listw = listw)
#' print(similarity)


similarity.test <-  function(formula = NULL, data = NULL, fx = NULL, listw = listw,
                             alternative = "two.sided", distr = "asymptotic", nsim = NULL,
                          control = list()){

# chequeo
  alternative <- match.arg(alternative, c("greater", "less", "two.sided"))
  distr <- match.arg(distr, c("asymptotic", "bootstrap"))

  # Solo admite matrices knn, nb o matrix
  if (!inherits(listw, "nb")){
    if (!inherits(listw, "matrix")){
      if (!inherits(listw, "knn")){
        stop ("listw must be is an object of the class: knn, nb or matrix")
      }
    }
  }
  # Solo admite matrices knn, nb o matrix
  if (inherits(listw, "knn")){
    W <- (nb2mat(knn2nb(listw),
                        zero.policy = TRUE) > 0)*1
    }
  if (inherits(listw, "nb")){
    W <- (nb2mat(listw, zero.policy = TRUE) >0)*1
  }
# Selecciona los argumentos. Bien con (formula + data) o bien incluye la variable (fx)
if (!is.null(formula) && !is.null(data)) {
  if (inherits(data, "Spatial")) data <- as(data, "sf")
  mfx <- get_all_vars(formula, data)[,1]
  data.name <- names(get_all_vars(formula,data))
} else if (!is.null(fx)) {
  mfx <- fx
  if (is.null(names(fx))) data.name <- "fx"
} else stop("data wrong")

if (!is.factor(mfx))
  stop(paste(deparse(substitute(fx)), "is not a factor"))

  # Controls
  con <- list(seedinit = 123)
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  if (length(noNms <- namc[!namc %in% nmsC]))
    warning("unknown names in control: ", paste(noNms, collapse = ", "))
  seedinit <- con$seedinit


## Calculo test
N <- length(mfx)
q <- length(levels(mfx))
m <- rep(0,q)
m <- table(mfx)
S0=sum(W)
S1=2*S0;

sw <- rowSums(W)^2
S2 <- 2*sum(sw)
SS <- (matrix(mfx,ncol = N,nrow = N)==t(matrix(mfx,ncol = N,nrow = N)))*W
est <- sum(SS)/N
p0 <- t(m)%*%(m-1)/(N*(N-1))
varij <- p0*(1-p0)
covijk <- (m*(m-1))%*%(m-2)/(N*(N-1)*(N-2))-p0^2
num <- 0
for (c in 1:q){
  m2 <- m[-c]
  num <- num + m[c]*(m[c]-1)* ( (m[c]-2)*(m[c]-3) + t(m2)%*%(m2-1))
}
covijks <-  num/(N*(N-1)*(N-2)*(N-3))-p0^2

# MEAN OF THE ESTATISTIC
meanest <- S0/N*p0

# VARIANCE OF THE ESTATISTIC
varest <- 1/N^2*(S1*varij + (S2-2*S1)*covijk + (S0^2-S2+S1)*covijks)

#####################################
## Similarity mc
#####################################
if (distr == "bootstrap"){
  if (!is.null(seedinit)) set.seed(seedinit)
  similarity.mc <- rep(0,nsim)
  for (f in 1:nsim){
    fxp <- mfx[sample(N)]
    SS <- (matrix(fxp, ncol = N, nrow = N)==t(matrix(fxp, ncol = N, nrow = N)))*W
    estp <- sum(SS)/N
    similarity.mc[f] <- (estp-meanest)/varest^0.5
  }

  SSnormal <- (est-meanest)/varest^0.5
  names(SSnormal) <- "Similarity-test"

  if (alternative == "two.sided")
    p.value <- sum(abs(similarity.mc) > est)/(nsim+1)
  else if (alternative == "greater")
    p.value <- sum(similarity.mc > est)/(nsim+1)
  else p.value <- sum(similarity.mc < est)/(nsim+1)
  if (!is.finite(SSnormal) || p.value < 0 || p.value > 1)
    warning("Out-of-range p-value: reconsider test arguments")

}
if (distr == "asymptotic"){
# NORMAL APPROXIMATION OF THE ESTATISTIC
SSnormal <- (est-meanest)/varest^0.5
names(SSnormal) <- "Similarity-test"
similarity.mc <- NULL
if (alternative == "two.sided")
  p.value <- 2 * pnorm(abs(SSnormal),
                              lower.tail = FALSE)
else if (alternative == "greater")
  p.value <- pnorm(SSnormal, lower.tail = FALSE)
else p.value <- pnorm(SSnormal )
if (!is.finite(SSnormal) || p.value < 0 || p.value > 1)
  warning("Out-of-range p-value: reconsider test arguments")
}

srq <- list(method = paste0("Similarity test of spatial dependence for qualitative data. Distribution: ", distr),
            statistic = SSnormal, p.value = p.value, N = N, alternative = alternative, distr = distr,
            similarity.mc = similarity.mc,
                    data.name = data.name#, estimate = vec
            )
class(srq) <- c("htest")
srq
}
