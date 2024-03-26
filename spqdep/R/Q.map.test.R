#' @name Q.map.test
#' @rdname Q.map.test
#' @title Compute the QE and QI tests of Equivalence and Independence between maps
#' @usage Q.map.test(formula = formula, data = data, coor = NULL, m = m, r = 1,
#' type = "combinations", control = list())
#' @param formula a symbolic description of the two factors.
#' @param data (optional) a data frame or a sf object containing the variables to testing for.
#' @param m length of m-surrounding.
#' @param r maximum overlapping between any two m-surroundings (default = 1).
#' @param type Type of symbols: "permutations" or "combinations". Default "combinations"
#' @param coor (optional) a 2xN vector with coordinates.
#' @param control Optional argument. See Control Argument section.
#' @description This function compute the QE and QI tests for maps comparison based on symbolic entropy.
#' @section Control arguments:
#' Several parameters to construct the m-surrounding
#' \describe{
#' \item{dtmaxabs}{Delete degenerate surrounding based on the absolute distance between observations.}
#' \item{dtmaxpc}{A value between 0 and 1. Delete degenerate surrounding based on the distance.
#' Delete m-surrounding when the maximum distance between observation
#' is upper than k percentage of maximum distance between anywhere observation.}
#' \item{dtmaxknn}{A integer value 'k'. Delete degenerate surrounding based on the near neighborhood criteria. Delete m-surrounding
#' is a element of the m-surrounding is not include in the set of k near neighborhood of the first element}
#' \item{seedinit}{seed to select the initial element to star
#' the algorithm to compute the m-surroundings.}
#' }
#'
#' @details If \code{data} is not a sf object the \code{coor} argument with the coordinates
#' of each observation must be included. \cr
#'
#' @return A list with two objects of the class \code{htest}. The first one
#' is the QE test of Equivalence between maps and the second one is the QI test
#' of independence between maps. the elements of each test are: \cr
#'   \tabular{ll}{
#'     \code{method} \tab a character string giving description of the method. \cr
#'     \code{data.name} \tab a character string giving the name(s) of the data. \cr
#'     \code{statistic} \tab the value of the statistic QE or/and QI. \cr
#'     \code{alternative} \tab a character string describing the alternative hypothesis. \cr
#'     \code{p.value} \tab  p-value for QE or QI. \cr
#'     \code{parameter} \tab free degree of the statistic for QE or QI. \cr
#'     \code{symb} \tab A matrix with the symbols. \cr
#'     \code{mh} \tab m-surrounding of th map. \cr
#'     \code{Tm} \tab number of maps (ONLY 2). \cr
#'     \code{sample.size} \tab  number of symbolized observations. \cr
#'     \code{nsk} \tab a matrix Tm x symbols with the frequency of the number of symbols of each map. \cr
#'     }
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
#'     \item Ruiz M, López FA and A Páez (2011).
#'     Comparison of Thematic Maps Using Symbolic Entropy.
#'       \emph{International Journal of Geographical Information Science}, 26, 413-439.
#'     \item Ruiz, M., López, FA, and Páez, A. (2010).
#'     Testing for spatial association of qualitative data using symbolic dynamics.
#'     \emph{Journal of Geographical Systems}, 12(3), 281-309.0.
#'   }
#'
#' @seealso
#' \code{\link{dgp.spq}}, \code{\link{m.surround}}, \code{\link{Q.test}}
#' @export
#' @examples
#'
#' # Case 1:
#' N <- 200
#' cx <- runif(N)
#' cy <- runif(N)
#' x <- cbind(cx,cy)
#' listw <- spdep::nb2listw(spdep::knn2nb(
#'            spdep::knearneigh(cbind(cx,cy), k = 4)))
#' p <- c(1/6, 3/6, 2/6)
#' rho = 0.5
#' QY1 <- dgp.spq(p = p, listw = listw, rho = rho)
#' rho = 0.8
#' QY2 <- dgp.spq(p = p, listw = listw, rho = rho)
#' dt = data.frame(QY1,QY2)
#' m = 3
#' r = 1
#' formula <- ~ QY1 + QY2
#' control <- list(dtmaxknn = 10)
#' qmap <- Q.map.test(formula = formula, data = dt, coor = x, m = m, r = r,
#'                    type ="combinations", control = control)
#' print(qmap)
#' plot(qmap)
#' plot(qmap, ci=.6)
#' plot(qmap[[1]]$mh)
#' summary(qmap[[1]]$mh)
#'
#' control <- list(dtmaxknn = 20)
#' qmap <- Q.map.test(formula = formula, data = dt, coor = x, m = m, r = r,
#'                    type ="permutations", control = control)
#' print(qmap)
#' plot(qmap)
#' plot(qmap[[1]]$mh)
#' qmap <- Q.map.test(formula = formula, data = dt, coor = x, m = m, r = r,
#'                    type ="combinations")
#' print(qmap)
#' plot(qmap)
#' control <- list(dtmaxknn = 10)
#' qmap <- Q.map.test(formula = formula, data = dt, coor = x, m = m, r = r,
#'                    type ="combinations", control = control)
#' print(qmap)
#' plot(qmap)
#'
#' # Case 2:
#' data(provinces_spain)
#' # sf::sf_use_s2(FALSE)
#' m = 3
#' r = 1
#'
#' provinces_spain$Mal2Fml <- factor(provinces_spain$Mal2Fml > 100)
#' levels(provinces_spain$Mal2Fml) = c("men","woman")
#' provinces_spain$Coast <- factor(provinces_spain$Coast)
#' levels(provinces_spain$Coast) = c("no","yes")
#' formula <- ~ Coast + Mal2Fml
#' qmap <- Q.map.test(formula = formula, data = provinces_spain, m = m, r = r,
#'                    type ="combinations")
#' print(qmap)
#' plot(qmap)
#' plot(qmap[[1]]$mh)
#'
#' control <- list(dtmaxknn = 6)
#' qmap <- Q.map.test(formula = formula, data = provinces_spain, m = m, r = r,
#'                    type ="combinations", control = control)
#' print(qmap)
#' plot(qmap[[1]]$mh)
#' summary(qmap[[1]]$mh)
#'

Q.map.test <- function(formula = formula, data = data, coor = NULL, m = m, r = 1,
                       type = "combinations", control = list()) {

  con <- list(initobs = 1, dtmaxabs = 0, dtmaxpc = 0, dtmaxknn = 0)
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  if (length(noNms <- namc[!namc %in% nmsC]))
    warning("unknown names in control: ", paste(noNms, collapse = ", "))
  initobs <- con$initobs
  dtmaxabs <- con$dtmaxabs
  dtmaxpc <- con$dtmaxpc
  dtmaxknn <- con$dtmaxknn

  if (length(formula)!=2) {stop ("formula must have 2 elements")}
  type <- match.arg(type, c("permutations", "combinations"))

  if (inherits(data, "sf") && is.null(coor)){
    coor <- suppressWarnings(sf::st_centroid(
      sf::st_geometry(data)))
    coor <- st_as_sf(coor)
  } else if (!inherits(data, "sf") && is.matrix(coor)){
    coor <- sp::SpatialPoints(coor)
    coor <- as(coor, "sf")
  } else stop("input data wrong")

  df <- get_all_vars(formula, data)
  # Control of factors
  if (sum(sapply(df,is.factor)) !=2) {stop ("include two factors")}
  if (length(levels(df[,1])) != length(levels(df[,2]))) {stop ("both factos must have the same length of classes")}
  # R <- dim(df)[1]
  Tm <- dim(df)[2]
  # Transformar factores en numeros
  for ( t in 1: Tm){
    levels(df[,1]) <- as.character(1:length(levels(df[,t])))
    df[,t] <- as.numeric(df[,t])
  }

  # Calcular simbolos permutation
  symb <- cr_symb(m = m, max(df[,1]))$p_symb
  mh <- m.surround(x = coor, m = m, r = r, control = list(initobs = initobs,
                                                          dtmaxabs = dtmaxabs,
                                                          dtmaxpc = dtmaxpc,
                                                          dtmaxknn = dtmaxknn))
  R <- dim(mh$ms)[1]
  Z <- list()
  for (t in 1:Tm){
    Z[[t]] <- matrix(df[,t][mh$ms], ncol = m)
  }

  nusi <- dim(symb)[1]
  nsk <- matrix(0,ncol = nusi,nrow = Tm)
  for (t in 1:Tm){
    for (i in 1:nusi){
      nsk[t,i] <- length(which(apply(Z[[t]], 1, function(x) all.equal(x, symb[i,])) == "TRUE"))
    }
  }
  ##################################
  # Symbols product cartesiano
  # SYMB <- cbind(kronecker(matrix(1,ncol =1, nrow = nusi),symb),kronecker(symb,matrix(1,ncol =1, nrow = nusi)))
  # ZZ <- cbind(Z[[1]],Z[[2]])
  # NUSI <- dim(SYMB)[1]
  # NSK <- matrix(0,ncol = NUSI, nrow = 1)
  # for (i in 1:NUSI){
  #   NSK[i] <- length(which(apply(ZZ, 1, function(x) all.equal(x, SYMB[i,])) == "TRUE"))
  # }

  SYMB <- cbind(kronecker(matrix(1,ncol =1, nrow = nusi),symb),kronecker(symb,matrix(1,ncol =1, nrow = nusi)))
  ZZ <- cbind(Z[[1]],Z[[2]])
  symb.string <- as.factor(apply(SYMB, 1, function(x) paste0(x,collapse = "")))
  Z1.string <- factor(apply(ZZ, 1, function(x) paste0(x,collapse = "")),levels=levels(symb.string))
  NSK <- as.matrix(table(Z1.string))

  ##################################
  # Calcular simbolos combinaciones
  # Simbolizo la serie a partir de Z
  Z2 <- list()
  for (t in 1:Tm){
    Z2[[t]]<- t(apply(Z[[t]], 1, function(x) table(factor(x, levels = unique(sort(c(Z[[t]])))))))
    Z2[[t]] <- matrix(as.numeric(Z2[[t]]),ncol = dim(Z2[[t]])[2])
  }

  symb2 <- cr_symb(m=m,max(df[,1]))$c_symb
  symb2 <- matrix(as.numeric(symb2),ncol = dim(symb2)[2])
  nusi2 <- dim(symb2)[1]
  nsk2 <- matrix(0,ncol = nusi2,nrow = Tm)
  for (t in 1:Tm){
    for (i in 1:nusi2){
      # nsk2[t,i] <- sum(rowSums(Z2[[t]]==symb3[i,])==m)
      nsk2[t,i] <- length(which(apply(Z2[[t]], 1, function(x) all.equal(x, symb2[i,])) == "TRUE"))

    }
  }

  ##################################
  # Symbols product cartesiano
  # SYMB2 <- cbind(kronecker(matrix(1,ncol =1, nrow = nusi2),symb2),kronecker(symb2,matrix(1,ncol = 1, nrow = nusi2)))
  # ZZ2 <- cbind(Z2[[1]],Z2[[2]])
  # NUSI2 <- dim(SYMB2)[1]
  # NSK2 <- matrix(0,ncol = NUSI2, nrow = 1)
  # for (i in 1:NUSI2){
  #   NSK2[i] <- length(which(apply(ZZ2, 1, function(x) all.equal(x, SYMB2[i,])) == "TRUE"))
  # }

  SYMB2 <- cbind(kronecker(matrix(1,ncol=1, nrow = nusi2),symb2),kronecker(symb2,matrix(1,ncol = 1, nrow = nusi2)))
  ZZ2 <- cbind(Z2[[1]],Z2[[2]])
  NUSI2 <- dim(SYMB2)[1]
  symb.string <- as.factor(apply(SYMB2, 1, function(x) paste0(x,collapse = "")))
  Z1.string <- factor(apply(ZZ2, 1, function(x) paste0(x,collapse = "")),levels=levels(symb.string))
  NSK2 <- as.matrix(table(Z1.string))

  #####
  # AUNQUE CALCULA LAS ENTROPIAS CON LOS DOS TIPOS DE SIMBOLOS
  # SOLO DA EL OUTPUT DE UNO DE ELLOS
  # ESTO SE PUEDE CAMBIAR CLARO

  if (type == "combinations"){
    nsk <- nsk2
    nusi <- nusi2
    symb <- symb2
    NSK <- NSK2
    NUSI <- NUSI2
    SYMB <- SYMB2
  }
  #####
  # fnk <- nsk/(R*Tm)
  # fnT = colMeans(nsk)/(R*Tm)
  #
  # lnsk <- matrix(0,ncol = nusi,nrow = Tm)
  # for (t in 1:Tm){
  #   lnsk[t,] <-  log(fnT/fnk[t,])
  # }
  # lnsk[lnsk==Inf] <- 0
  # lnsk[is.na(lnsk)] <- 0

  fnk <- nsk/R
  lfnk <- log(fnk)
  lfnk[lfnk==-Inf] <- 0

  Fnk <- colSums(nsk)/(Tm*R)
  lFnk <- log(Fnk)
  lFnk[lFnk== -Inf] <- 0
  h <- sum(Fnk*lFnk)

  FNK <- NSK/R
  LFNK <- log(FNK)
  LFNK[LFNK== -Inf] <- 0
  hw <- sum(FNK*LFNK)


  qmap <- list()

  QE <- 4*R*(log(2) - h + (1/2)*sum(rowSums(fnk*lfnk)))
  gl.Equivalence <- nusi - 1
  p.value.Equivalence = pchisq(QE,
                                      df = gl.Equivalence,
                                      lower.tail = FALSE)

  QI <- 2*R*(-sum(rowSums(fnk*lfnk))+sum(FNK*LFNK))
  gl.Independence <- nusi*(nusi-2) + 1
  p.value.Independence = pchisq(QI,
                                       df = gl.Independence,
                                       lower.tail = FALSE)

  ##### .Equivalence
  names(QE) <- "QE"
  alternative = "two.sided"
  parameter.Equivalence <- gl.Equivalence
  names(parameter.Equivalence) <- "df"
  data.name <- paste(names(df),collapse = " and ")
  qmap[[1]] <- list(method = paste("Q-Map test of Equivalence for qualitative data.",
                                   "\nSymbols type: ", type,
                                   "\nRatio Symbolized observations/Num symbols = ",round(R/dim(symb)[1],2),
                                   sep = "\n"),
               data.name = data.name,
               statistic = QE,
               p.value = p.value.Equivalence,
               alternative = alternative,
               sample.size = R,
               parameter = parameter.Equivalence,
               symb = symb, Tm = Tm, mh = mh, nsk = t(nsk))
  class(qmap[[1]]) <- c("htest","list")

##### .Equivalence
names(QI) <- "QI"
alternative = "two.sided"
parameter.Independence <- gl.Independence
names(parameter.Independence) <- "df"
data.name <- paste(names(df),collapse = " and ")
qmap[[2]] <- list(method = paste("Q-Map test of Independence for qualitative data.",
                                 "\nSymbols type: ", type,
                                 "\nRatio Symbolized observations/Num symbols = ",round(R/dim(symb)[1],2),
                                 sep = "\n"),
                  data.name = data.name,
                  statistic = QI,
                  p.value = p.value.Independence,
                  alternative = alternative,
                  sample.size = R,
                  parameter = parameter.Independence,
                  symb = symb, Tm = Tm, mh = mh, nsk = t(nsk))
class(qmap[[2]]) <- c("htest","list")
if( R/dim(symb)[1] < 5 ) warning('The ratio between the number of symbolized observations and the number of symbols is lower than 5.')
class(qmap) <- c("qmap","list")
qmap
}
