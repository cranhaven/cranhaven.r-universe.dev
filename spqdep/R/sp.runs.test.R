#'
#' @title Compute the global spatial runs test.
#'
#' @description This function compute the global spatial runs test for spatial independence of a
#' categorical spatial data set.
#' @param data an (optional) data frame or a sf object containing the variable to testing for.
#' @param listw A neighbourhood list (type knn or nb) or a W matrix that indicates the order of the elements in each \eqn{m_i-environment}
#' (for example of inverse distance). To calculate the number of runs in each \eqn{m_i-environment}, an order must
#' be established, for example from the nearest neighbour to the furthest one.
#' @param fx a factor (optional).
#' @param formula a symbolic description of the factor (optional).
#' @param alternative a character string specifying the alternative hypothesis, must be one
#' of "two.sided" (default), "greater" or "less".
#' @param distr A string. Distribution of the test "asymptotic" (default) or "bootstrap".
#' @param nsim Number of permutations to obtain pseudo-value and confidence intervals (CI).
#' Default value is NULL to don`t get CI of number of runs.
#' @param control List of additional control arguments.
#' @usage sp.runs.test(formula = NULL, data = NULL, fx = NULL,
#' listw = listw, alternative = "two.sided" ,
#' distr = "asymptotic", nsim = NULL,control = list())
#'
#' @details The order of the neighbourhoods (\eqn{m_i-environments}) is critical to obtain the test. \cr
#' To obtain the number of runs observed in each \eqn{m_i-environment}, each element must be associated
#' with a set of neighbours ordered by proximity.
#' Three kinds of lists can be included to identify \eqn{m_i-environments}:\cr
#'
#' \itemize{
#'     \item  \code{knn}: Objects of the class knn that consider the neighbours in order of proximity.
#'     \item  \code{nb}: If the neighbours are obtained from an sf object, the code internally
#'     will call the function \code{\link{nb2nb_order}} it will order them in order
#'     of proximity of the centroids.
#'     \item  \code{matrix}: If a object of matrix class based in the inverse of the distance in introduced
#'     as argument, the function \code{\link{nb2nb_order}} will also be called internally
#'     to transform the object the class matrix to a matrix of the class nb with ordered neighbours.
#'     }
#'
#' Two alternative sets of arguments can be included in this function to compute the spatial runs test:
#'
#'   \tabular{ll}{
#'     \code{Option 1} \tab A factor (fx) and a list of neighborhood (\code{listw}) of the class knn. \cr
#'     \code{Option 2} \tab A sf object (data) and formula to specify the factor. A list of neighbourhood (listw) \cr
#'     }
#'
#' @section Definition of spatial run:
#'
#' In this section define the concepts of spatial encoding and runs, and construct the main statistics necessary
#' for testing spatial homogeneity of categorical variables. In order to develop a general theoretical setting,
#' let us consider \eqn{\{X_s\}_{s \in S}}  to be the categorical spatial process of interest with Q different
#' categories, where S is a set of coordinates.\cr
#'
#' \strong{Spatial encoding:}
#' For a location \eqn{s \in S} denote by \eqn{N_s = \{s_1,s_2 ...,s_{n_s}\}}  the set of neighbours according
#' to the interaction scheme W, which are ordered from lesser to higher Euclidean distance with respect to location s.\cr
#' The sequence as \eqn{X_{s_i} , X_{s_i+1},...,, X_{s_i+r}} its elements have the same value (or are identified by the same class)
#' is called a \strong{spatial run} at location s of length r.\cr
#'
#' @section Spatial run statistic:
#'
#' The total number of runs is defined as:\cr
#' \deqn{SR^Q=n+\sum_{s \in S}\sum_{j=1}^{n_s}I_j^s}\cr
#' where \eqn{I_j^s = 1 \ if \ X_{s_j-1} \neq X_{s_j} \ and 0 \ otherwise} for \eqn{j=1,2,...,n_s}\cr
#' Following result by the Central Limit Theorem, the asymtotical distribution of \eqn{SR^Q} is:\cr
#' \deqn{SR^Q = N(\mu_{SR^Q},\sigma_{SR^Q})}
#'
#' In the one-tailed case, we must distinguish the lower-tailed test and the upper-tailed, which are associated
#'  with homogeneity and heterogeneity respectively. In the case of the lower-tailed test,
#'  the following hypotheses are used:\cr
#'
#' \eqn{H_0:\{X_s\}_{s \in S}} is i.i.d.\cr
#' \eqn{H_1}: The spatial distribution of the values of the categorical variable is more homogeneous than under the null hypothesis (according to the fixed association scheme).
#' In the upper-tailed test, the following hypotheses are used:\cr
#'
#' \eqn{H_0:\{X_s\}_{s \in S}} is i.i.d.\cr
#' \eqn{H_1}: The spatial distribution of the values of the categorical variable is more
#' heterogeneous than under the null hypothesis (according to the fixed association scheme).\cr
#'
#' These hypotheses provide a decision rule regarding the degree of homogeneity in the spatial distribution
#' of the values of the spatial categorical random variable.\cr
#'
#' @return A object of the \emph{htest} and \emph{sprunstest} class
#'   \tabular{ll}{
#'     \code{data.name} \tab a character string giving the names of the data.\cr
#'     \code{method} \tab the type of test applied ().\cr
#'     \code{SR} \tab total number of runs \cr
#'     \code{dnr} \tab empirical distribution of the number of runs \cr
#'     \code{statistic} \tab Value of the homogeneity runs statistic. Negative sign indicates global homogeneity \cr
#'     \code{alternative} \tab a character string describing the alternative hypothesis. \cr
#'     \code{p.value} \tab p-value of the SRQ \cr
#'     \code{pseudo.value} \tab the pseudo p-value of the SRQ test if nsim is not NULL\cr
#'     \code{MeanNeig} \tab Mean of the Maximum number of neighborhood \cr
#'     \code{MaxNeig} \tab Maximum number of neighborhood \cr
#'     \code{listw} \tab The list of neighborhood \cr
#'     \code{nsim} \tab number of boots (only for boots version) \cr
#'     \code{SRGP} \tab nsim simulated values of statistic. \cr
#'     \code{SRLP} \tab matrix with the number of runs for eacl localization. \cr
#'     }
#'
#' @section Control arguments:
#'   \tabular{ll}{
#'     \code{seedinit} \tab Numerical value for the seed (only for boot version). Default value seedinit=123 \cr
#'       }
#'
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Ruiz, M., López, F., and Páez, A. (2021).
#'     A test for global and local homogeneity of categorical data based on spatial runs.
#'       \emph{Working paper}.
#'   }
#'
#' @seealso
#' \code{\link{local.sp.runs.test}}, \code{\link{dgp.spq}}, \code{\link{Q.test}},
#' @export
#'
#' @examples
#'
#' # Case 1: SRQ test based on factor and knn
#' \donttest{
#' n <- 100
#' cx <- runif(n)
#' cy <- runif(n)
#' x <- cbind(cx,cy)
#' listw <- spdep::knearneigh(cbind(cx,cy), k=3)
#' p <- c(1/6,3/6,2/6)
#' rho <- 0.5
#' fx <- dgp.spq(listw = listw, p = p, rho = rho)
#' srq <- sp.runs.test(fx = fx, listw = listw)
#' print(srq)
#' plot(srq)
#'
#' # Boots Version
#' control <- list(seedinit = 1255)
#' srq <- sp.runs.test(fx = fx, listw = listw, distr = "bootstrap" , nsim = 299, control = control)
#' print(srq)
#' plot(srq)
#'
#' # Case 2: SRQ test with formula, a sf object (points) and knn
#' data("FastFood.sf")
#' x <- sf::st_coordinates(sf::st_centroid(FastFood.sf))
#' listw <- spdep::knearneigh(x, k=4)
#' formula <- ~ Type
#' srq <- sp.runs.test(formula = formula, data = FastFood.sf, listw = listw)
#' print(srq)
#' plot(srq)
#' # Version boots
#' srq <- sp.runs.test(formula = formula, data = FastFood.sf, listw = listw,
#' distr = "bootstrap", nsim = 199)
#' print(srq)
#' plot(srq)
#'
#' # Case 3: SRQ test (permutation) using formula with a sf object (polygons) and nb
#' library(sf)
#' fname <- system.file("shape/nc.shp", package="sf")
#' nc <- sf::st_read(fname)
#' listw <- spdep::poly2nb(as(nc,"Spatial"), queen = FALSE)
#' p <- c(1/6,3/6,2/6)
#' rho = 0.5
#' co <- sf::st_coordinates(sf::st_centroid(nc))
#' nc$fx <- dgp.spq(listw = listw, p = p, rho = rho)
#' plot(nc["fx"])
#' formula <- ~ fx
#' srq <- sp.runs.test(formula = formula, data = nc, listw = listw,
#' distr = "bootstrap", nsim = 399)
#' print(srq)
#' plot(srq)
#'
#' # Case 4: SRQ test (Asymptotic) using formula with a sf object (polygons) and nb
#' data(provinces_spain)
#' # sf::sf_use_s2(FALSE)
#' listw <- spdep::poly2nb(provinces_spain, queen = FALSE)
#' provinces_spain$Coast <- factor(provinces_spain$Coast)
#' levels(provinces_spain$Coast) = c("no","yes")
#' plot(provinces_spain["Coast"])
#' formula <- ~ Coast
#' srq <- sp.runs.test(formula = formula, data = provinces_spain, listw = listw)
#' print(srq)
#' plot(srq)
#'
#' # Boots version
#' srq <- sp.runs.test(formula = formula, data = provinces_spain, listw = listw,
#' distr = "bootstrap", nsim = 299)
#' print(srq)
#' plot(srq)
#'
#' # Case 5: SRQ test based on a distance matrix (inverse distance)
#' N <- 100
#' cx <- runif(N)
#' cy <- runif(N)
#' data <- as.data.frame(cbind(cx,cy))
#' data <- sf::st_as_sf(data,coords = c("cx","cy"))
#' n = dim(data)[1]
#' dis <- 1/matrix(as.numeric(sf::st_distance(data,data)),ncol=n,nrow=n)
#' diag(dis) <- 0
#' dis <- (dis < quantile(dis,.10))*dis
#' p <- c(1/6,3/6,2/6)
#' rho <- 0.5
#' fx <- dgp.spq(listw = dis , p = p, rho = rho)
#' srq <- sp.runs.test(fx = fx, listw = dis)
#' print(srq)
#' plot(srq)
#'
#' srq <- sp.runs.test(fx = fx, listw = dis, data = data)
#' print(srq)
#' plot(srq)
#'
#' # Boots version
#' srq <- sp.runs.test(fx = fx, listw = dis, data = data, distr = "bootstrap", nsim = 299)
#' print(srq)
#' plot(srq)
#'
#' # Case 6: SRQ test based on a distance matrix (inverse distance)
#' data("FastFood.sf")
#' # sf::sf_use_s2(FALSE)
#' n = dim(FastFood.sf)[1]
#' dis <- 1000000/matrix(as.numeric(sf::st_distance(FastFood.sf,FastFood.sf)), ncol = n, nrow = n)
#' diag(dis) <- 0
#' dis <- (dis < quantile(dis,.005))*dis
#' p <- c(1/6,3/6,2/6)
#' rho = 0.5
#' co <- sf::st_coordinates(sf::st_centroid(FastFood.sf))
#' FastFood.sf$fx <- dgp.spq(p = p, listw = dis, rho = rho)
#' plot(FastFood.sf["fx"])
#' formula <- ~ fx
#'
#' # Boots version
#' srq <- sp.runs.test(formula = formula, data = FastFood.sf, listw = dis,
#' distr = "bootstrap", nsim = 299)
#' print(srq)
#' plot(srq)
#' }
#'

sp.runs.test <-  function(formula = NULL, data = NULL, fx = NULL,
                       listw = listw, alternative = "two.sided" , distr = "asymptotic", nsim = NULL,
                      control = list()){

  alternative <- match.arg(alternative, c("greater", "less", "two.sided"))
  distr <- match.arg(distr, c("asymptotic", "bootstrap"))

  # Solo admite matrices knn, nb o matrix
  if (!inherits(listw, "knn")){
    if (!inherits(listw, "nb")){
      if (!inherits(listw, "matrix")){
      stop ("listw must be is an object of the class: knn, nb or matrix")
      }
    }
  }

# Controls
  con <- list(seedinit = 123)
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  if (length(noNms <- namc[!namc %in% nmsC]))
    warning("unknown names in control: ", paste(noNms, collapse = ", "))
  seedinit <- con$seedinit
################################################
## Tratamiento de la matrix
################################################

# Si se trata de un objeto sf y la matrix es tipo 'nb' / "matrix" hay que ordenar los m_i-entornos
if (sum(inherits(data, "sf")) == 1){
if (inherits(listw, "nb")){ # hay que ordenar los elementos
  listw <- nb2nb_order(listw=listw, sf = data)
}
if (inherits(listw, "matrix")){ # hay que ordenar los elementos
      listw <- mat2listw(listw)$neighbours
      class(listw) <- "nb"
      listw <- nb2nb_order(listw=listw, sf = data)
}
}

if (sum(inherits(data, "sf")) == 0){
  if (inherits(listw, "matrix")){ # hay que ordenar los elementos
    listw <- nb2nb_order(listw=listw)
  }
}
################################################
## Tratamiento del input de los datos
################################################
# Selecciona los argumentos. Bien con (formula + data) o bien incluye la variable (fx)
  if (!is.null(formula) && !is.null(data)) {
    if (inherits(data, "Spatial")) data <- as(data, "sf")
    mxf <- get_all_vars(formula, data)
  } else if (!is.null(fx)) {
    mxf <- fx
    # if (!is.matrix(mxf)) mxf <- as.matrix(mxf, ncol = 1)
    mxf <- as.data.frame(mxf)
    for (i in 1:ncol(mxf)) {
      if (!is.factor(mxf[,i])) mxf[,i] <- as.factor(mxf[,i])
    }
  } else stop("data wrong")

  # fx debe ser un factor. Lo transformo en var numerica para calcular
  if (is.factor(mxf[,1])){
    levels(mxf[,1]) <- as.character(1:length(levels(mxf[,1])))
    y <- as.numeric(mxf[,1])
  }
  if (is.character(mxf[,1])){
    y <- as.factor(mxf[,1])
    levels(fx[,1]) <- as.character(1:length(levels(mxf[,1])))
    y <- as.numeric(mxf[,1])
  }
  if (is.numeric(mxf[,1])){
    stop("Only factors are admitted")
  }

################################################
## Empezamos las cuentas del test
################################################

# Calculo valores previos para obtener media y varianza estadístico
nv <- creation_nvar_SR(listw = listw)
q <- max(y)
n <- length(y)
# Cont is a binary variable that takes on the value of 1 if data are
# continuous and 0 if data are categorical.

m <- numeric() # matrix(0,nrow=q,ncol=1)
pprod <- numeric() # matrix(0,nrow=q,ncol=1)
for (i in 1:q){
  m[i] <- sum(y==i)
  pprod[i]<- m[i]*(n-m[i])
}
if (inherits(listw, "knn")){
lnnb <- matrix(dim(listw$nn)[2],ncol = 1,nrow = dim(listw$nn)[1])}
if (inherits(listw, "nb")){
lnnb <- rowSums(nb2mat(listw, style = 'B',
                              zero.policy = TRUE))
}

MaxNeig <- max(lnnb)+1 # El elemento en cuestion + sus vecinos

# here we categorize the original data set y into the q categories
# compute the m_k needed for the computation of mean and variance
# pprod is needed for the computation of p
p <- sum(pprod)/(n*(n-1))


##### COMPUTING THE VARIANCE #####
## case 1 ##
aux1 <- numeric()
aux31 <- numeric()
aux3 <- numeric()

t1=0;
for (k in 1:q){
  for (c in 1:q){
    t1=t1+1
    aux1[t1]=m[k]*m[c]*(n-m[c]-1)
    aux31[t1]=m[k]*m[c]*((m[k]-1)*(n-m[k]-1)+(m[c]-1)*(n-m[c]-1))
    if(k==c){
      aux1[t1]=0
      aux31[t1]=0
    }
  }
}

t3=0
aux3 <- numeric()
for (k in 1:q){
  for (c in 1:q){
    for (d in 1:q){
      t3=t3+1
      aux3[t3]=m[k]*m[c]*m[d]*(n-m[d]-2)
      if (c==k){aux3[t3]=0}
      if (d==k){aux3[t3]=0}
      if (d==c){aux3[t3]=0}
    }
  }
}

var1 <- 1/(n*(n-1)*(n-2)*(n-3))*(sum(aux3)+sum(aux31));
var2 <- 1/(n*(n-1)*(n-2))*sum(aux1);
var3 <- p;

varSR <- p*(1-p)*sum(lnnb)+nv[1]*var1+nv[2]*var2+nv[3]*var3-(nv[1]+nv[2]+nv[3])*p^2

# Here we compute the runs starting at each location and it sum is the total number of runs
nruns <- matrix(0,ncol = 1,nrow = n)
for (i in 1:n){
  if (lnnb[i]!= 0){ # Solo calcula los test locales si el elemento tiene vecinos
  if (inherits(listw, "knn")){
    runs <- y[c(i,listw$nn[i,])]}
  if (inherits(listw, "nb")){
    runs <- y[c(i,listw[[i]])]}
nruns[i] <- 1 + sum(abs(diff(runs))>0)
  }
}
# The distribution of the number of runs
dnr <- table(factor(nruns, levels = c(1:MaxNeig))) # dnr <- table(SRQlocal[,1],exclude = 0) # Excluimos localizaciones con 0 vecinos
dnr <- data.frame(dnr)

SR <- sum(nruns)

# The mean of the statistic
meanSR <- n + p*sum(lnnb)
vec <- c(SR,meanSR,varSR)
names(vec) <- c("Total runs","Mean total runs","Variance total runs")
# The SRQ global test statistic which is N(0,1) distributed
SRQ <- (SR-meanSR)/sqrt(varSR)
if (alternative =="two.sided"){
p.value <- 2*(1 - pnorm(abs(SRQ), mean = 0, sd = 1))
} else if (alternative =="less"){
p.value <- pnorm(SRQ, mean = 0, sd = 1)
} else if (alternative =="greater"){
p.value <- 1 - pnorm(SRQ, mean = 0, sd = 1)
}

############################################################################
# Para la obtención de los intervalos de confianza por boots permutacional
############################################################################
if ((is.null(nsim) == FALSE) && (distr != "asymptotic")){
  if (!is.null(seedinit)) set.seed(seedinit)
SRGP <- matrix(0,ncol = 1,nrow = nsim)
SRLP <- matrix(0,ncol = nsim, nrow = n)
    for (i in 1:nsim){
    fx <- y[sample(1:n)]
    srqp <- SR_test_boots(xf = fx, listw = listw, nv = nv)
    SRGP[i] <- srqp$SRglobal
    SRLP[,i] <- srqp$nruns
    }

vec <- c(SR,mean(colSums(SRLP)),
         sd(colSums(SRLP))^2)
names(vec) <- c("Observed Total runs","Mean total runs boots","Variance total runs boots")

if (alternative =="greater"){
pseudo.value <- sum(SRGP > SRQ)/(nsim+1)
}
else if (alternative =="less"){
  pseudo.value <- sum(SRGP < SRQ)/(nsim+1)
}
else if (alternative =="two.sided"){
  pseudo.value <- (sum(SRGP < -abs(SRQ)) + sum(SRGP > abs(SRQ)))/(nsim+1)
  # p.valueSRQB <- (sum(SRGP < SRQ) + sum(SRGP > SRQ))/(nsim+1)
} else stop("alternative wrong")
}

# El número total de rachas es la suma de las rachas en cada m_i-entorno
# colSums(SRLP) da el número de rachas de las nsim repeticiones
# SRGP es un vector con los valores de SRGlobal bajo aleatoriedad
############################################################
# Salida en función si se piden intervalos IC o no
names(SRQ) <- "sp.runs test"
data.name <- names(mxf)
if ((is.null(nsim) == FALSE) && (distr != "asymptotic")){
srq <- list(method = "Runs test of spatial dependence for qualitative data. Boots version",
               SR = SR, dnr = dnr, statistic  = SRQ, alternative = alternative,
               SRGP = SRGP, p.value = pseudo.value,
               data.name = data.name, estimate = vec,
               nsim = nsim, SRLP = SRLP, MeanNeig=sum(lnnb)/n,
               listw = listw, MaxNeig = MaxNeig)

}
else
{
srq <- list(method = "Runs test of spatial dependence for qualitative data. Asymptotic version",
               SR = SR, dnr = dnr, statistic = SRQ, p.value = p.value, alternative = alternative,
               data.name = data.name, estimate = vec,
               MeanNeig=sum(lnnb)/n,weights= listw, MaxNeig = MaxNeig)


}
class(srq) <- c("htest","sprunstest")
srq
}
