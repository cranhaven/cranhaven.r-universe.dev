#'
#' @title Compute the scan test
#'
#' @description This function compute the spatial scan test for Bernoulli and
#' Multinomial categorical spatial process, and detect spatial clusters
#'
#' @param data an (optional) data frame or a sf object containing the variable to testing for.
#' @param formula a symbolic description of the factor (optional).
#' @param fx a factor (optional).
#' @param coor (optional) coordinates of observations.
#' @param case Only for Bernoulli distribution. A element of factor, there are cases and non-cases for testing for cases versus non-cases
#' @param nv Maximum windows size, default nv = N/2. The algorithm scan for clusters of geographic size between 1
#' and the upper limit (nv) defined by the user.
#' @param nsim Number of permutations.
#' @param alternative Only for Bernoulli spatial process. A character string specifying the type of cluster, must be one
#' of "High" (default), "Both" or "Low".
#' @param distr distribution of the spatial process: "bernoulli" for two levels or "multinomial" for three or more levels.
#' @param windows a string to select the type of cluster "circular" (default) of "elliptic".
#' @param listw only for flexible windows. A neighbours list (an object of the class listw, nb or knn frop spdep) or an adjacency matrix.
#' @param minsize Minimum number of observations inside of Most Likely Cluster and secondary clusters.
#' @param control List of additional control arguments.
#' @usage scan.test(formula = NULL, data = NULL, fx = NULL, coor = NULL, case = NULL,
#' nv = NULL, nsim = NULL, distr = NULL, windows = "circular", listw = NULL,
#' alternative = "High", minsize = 1, control = list())
#' @details
#' Two alternative sets of arguments can be included in this function to compute the scan test:
#' \itemize{
#'     \item {\code{Option 1}: A factor (fx) and coordinates (coor)}.
#'     \item {\code{Option 2}: A sf object (data) and the formula to specify the factor.
#'     The function consider the coordinates of the centroids of the elements of th sf object.}
#'     }
#'  The spatial scan statistics are widely used in epidemiology, criminology or ecology.
#'  Their purpose is to analyse the spatial distribution of points or geographical
#'  regions by testing the hypothesis of spatial randomness distribution on the
#'  basis of different distributions (e.g. Bernoulli, Poisson or Normal distributions).
#'  The \code{scan.test} function obtain the scan statistic for two relevant distributions related
#'  with categorical variables: the Bernoulli and Multinomial distribution.\cr
#'  The spatial scan statistic is based on the likelihood ratio test statistic and
#'  is formulated as follows:\cr
#'  \deqn{\Delta = { \max_{z \in Z,H_A} L(\theta|z) \over \max_{z \in Z,H_0} L(\theta|z)}}
#'  where Z represents the collection of scanning windows constructed on the study region,
#'  \eqn{H_A} is an alternative hypothesis, \eqn{H_0} is a null hypothesis, and \eqn{L(\theta|z)}
#'   is the likelihood function with parameter \eqn{\theta} given window Z\cr.
#'   The null hypothesis says that there is no spatial clustering on the study region, and
#'   the alternative hypothesis is that there is a certain area with high (or low) rates of
#'   outcome variables. The null and alternative hypotheses and the likelihood function may
#'   be expressed in different ways depending on the probability model under consideration.\cr
#'  To test independence in a spatial process, under the null, the type of windows is
#'  irrelevant but under the alternative the elliptic
#'  windows can to identify with more precision the cluster.
#'
#'  For big data sets (N >>) the windows = "elliptic" can be so slowly
#'
#'
#'
#' @return A object of the \emph{htest} and \emph{scantest} class
#'   \tabular{ll}{
#'     \code{method} \tab The type of test applied ().\cr
#'     \code{fx} \tab Factor included as input to get the scan test.\cr
#'     \code{MLC} \tab Observations included into the Most Likelihood Cluster (MLC).\cr
#'     \code{statistic} \tab Value of the scan test (maximum Log-likelihood ratio). \cr
#'     \code{N} \tab Total number of observations.\cr
#'     \code{nn} \tab Windows used to get the cluster.\cr
#'     \code{nv} \tab Maximum number of observations into the cluster.\cr
#'     \code{data.name} \tab A character string giving the name of the factor.\cr
#'     \code{coor} \tab coordinates.\cr
#'     \code{alternative} \tab Only for bernoulli spatial process. A character string describing
#'     the alternative hypothesis select by the user.\cr
#'     \code{p.value} \tab p-value of the scan test.\cr
#'     \code{cases.expect} \tab Expected cases into the MLC.\cr
#'     \code{cases.observ} \tab Observed cases into the MLC.\cr
#'     \code{nsim} \tab Number of permutations.\cr
#'     \code{scan.mc} \tab a (nsim x 1) vector with the loglik values under bootstrap permutation.\cr
#'     \code{secondary.clusters} \tab a list with the observations included into the secondary clusters.\cr
#'     \code{loglik.second} \tab a vector with the value of the secondary scan tests (maximum Log-likelihood ratio).\cr
#'     \code{p.value.secondary} \tab a vector with the p-value of the secondary scan tests.\cr
#'     \code{Alternative.MLC} \tab A vector with the observations included in another cluster with the same loglik than MLC.\cr
#'     }
#' @section Control arguments:
#'   \tabular{ll}{
#'     \code{seedinit} \tab Numerical value for the seed (only for boot version). Default value seedinit=123 \cr
#'       }
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Kulldorff M, Nagarwalla N. (1995).
#'     Spatial disease clusters: Detection and Inference.
#'       \emph{Statistics in Medicine}. 14:799-810
#'     \item Jung I, Kulldorff M, Richard OJ (2010).
#'     A spatial scan statistic for multinomial data.
#'       \emph{Statistics in Medicine}. 29(18), 1910-1918
#'      \item Páez, A., López-Hernández, F.A., Ortega-García, J.A., Ruiz, M. (2016).
#'      Clustering and co-occurrence of cancer types: A comparison of techniques with
#'      an application to pediatric cancer in Murcia, Spain.
#'      \emph{Spatial Analysis in Health Geography}, 69-90.
#'      \item Tango T., Takahashi K. (2005). A flexibly shaped spatial scan statistic
#'      for detecting clusters, \emph{International Journal of Health Geographics} 4:11.
#'   }
#' @details
#' \strong{Bernoulli version}\cr
#' \cr
#' When we have dichotomous outcome variables, such as cases and noncases of certain
#'  diseases, the Bernoulli model is used. The null hypothesis is written as
#'  \deqn{H_0 : p = q  \ \ for \ \  all  \ \ Z}
#'  and the alternative hypothesis is \cr
#'  \deqn{H_A : p \neq q  \ \ for  \ \ some  \ \ Z}
#'  where p and q are the outcome probabilities (e.g., the probability of being a case)
#'  inside and outside scanning window Z, respectively. Given window Z, the test statistic is:\cr
#'
#' where cz and nz are the numbers of cases and observations (cases and noncases) within z,
#'  respectively, and C and N are the total numbers of cases and observations in the whole
#'  study region, respectively.\cr
#' \deqn{\Delta = }
#' \strong{Multinomial version of the scan test}\cr
#' \cr
#' The multinomial version of the spatial scan statistic is useful to investigate clustering
#' when a discrete spatial variable can take one and only one of k possible outcomes
#' that lack intrinsic order information. If the region defined by the moving window
#' is denoted by Z, the null hypothesis for the statistic can be stated as follows:\cr
#' \deqn{H_0: p_1 = q_1;p_2 = q_2;...;p_k = q_k}
#' where \eqn{p_j} is the probability of being of event type j inside the window Z,
#' and \eqn{q_j} is the probability of being of event type j outside the window.
#' The alternative hypothesis is that for at least one type event the probability
#'  of being of that type is different inside and outside of the window.\cr
#'
#'  The statistic is built as a likelihood ratio, and takes the following
#'  form after transformation using the natural logarithm:
#'\deqn{\Delta = \max_Z \{\sum_j \{ S_j^Z log({ S_j^Z \over S^Z }) +
#'(S_j-S_j^Z) log({ {S_j-S_j^Z} \over {S-S^Z} })\}\}-\sum_j S_j log({ S_j \over S }) }
#' where S is the total number of events in the study area and \eqn{S_j} is the total number
#' of events of type j. The superscript Z denotes the same but for the sub-region
#' defined by the moving window. \cr
#' The theoretical distribution of the statistic under the null hypothesis is not known,
#'  and therefore significance is evaluated numerically by simulating neutral landscapes
#'  (obtained using a random spatial process) and contrasting the empirically calculated
#'  statistic against the frequency of values obtained from the neutral landscapes.
#'  The results of the likelihood ratio serve to identify the most likely cluster,
#'  which is followed by secondary clusters by the expedient of sorting them according to
#'   the magnitude of the test. As usual, significance is assigned by the analyst, and the
#'    cutoff value for significance reflects the confidence of the analyst, or tolerance for error.\cr
#'  When implementing the statistic, the analyst must decide the shape of the
#'  window and the maximum number of cases that any given window can cover.
#'  Currently, analysis can be done using circular or elliptical windows.\cr
#'  Elliptical windows are more time consuming to evaluate but provide greater
#'  flexibility to contrast the distribution of events inside and outside the window,
#'  and are our selected shape in the analyses to follow. Furthermore, it is recommended
#'  that the maximum number of cases entering any given window does not exceed 50\% of
#'  all available cases.
#' @seealso
#' \code{\link{local.sp.runs.test}}, \code{\link{dgp.spq}}, \code{\link{Q.test}},
#' @export
#'
#' @examples
#'
#' # Case 1: Scan test bernoulli
#' data(provinces_spain)
#' sf::sf_use_s2(FALSE)
#' provinces_spain$Mal2Fml <- factor(provinces_spain$Mal2Fml > 100)
#' levels(provinces_spain$Mal2Fml) = c("men","woman")
#' formula <- ~ Mal2Fml
#' scan <- scan.test(formula = formula, data = provinces_spain, case="men",
#' nsim = 99, distr = "bernoulli")
#' print(scan)
#' summary(scan)
#' plot(scan, sf = provinces_spain)
#' \donttest{
#' ## With maximum number of neighborhood
#' scan <- scan.test(formula = formula, data = provinces_spain, case = "woman",
#' nsim = 99, distr = "bernoulli")
#' print(scan)
#' plot(scan, sf = provinces_spain)
#'
#'
#' ## With elliptic windows
#' scan <- scan.test(formula = formula, data = provinces_spain, case = "men", nv = 25,
#' nsim = 99, distr = "bernoulli", windows ="elliptic")
#' print(scan)
#' scan <- scan.test(formula = formula, data = provinces_spain, case = "men", nv = 15,
#' nsim = 99, distr = "bernoulli", windows ="elliptic", alternative = "Low")
#' print(scan)
#' plot(scan, sf = provinces_spain)
#'
#' # Case 2: scan test multinomial
#' data(provinces_spain)
#' provinces_spain$Older <- cut(provinces_spain$Older, breaks = c(-Inf,19,22.5,Inf))
#' levels(provinces_spain$Older) = c("low","middle","high")
#' formula <- ~ Older
#' scan <- scan.test(formula = formula, data = provinces_spain, nsim = 99, distr = "multinomial")
#' print(scan)
#' plot(scan, sf = provinces_spain)
#'
#' # Case 3: scan test multinomial
#' data(FastFood.sf)
#' sf::sf_use_s2(FALSE)
#' formula <- ~ Type
#' scan <- scan.test(formula = formula, data = FastFood.sf, nsim = 99,
#' distr = "multinomial", windows="elliptic", nv = 50)
#' print(scan)
#' summary(scan)
#' plot(scan, sf = FastFood.sf)
#'
#' # Case 4: DGP two categories
#' N <- 150
#' cx <- runif(N)
#' cy <- runif(N)
#' listw <- spdep::knearneigh(cbind(cx,cy), k = 10)
#' p <- c(1/2,1/2)
#' rho <- 0.5
#' fx <- dgp.spq(p = p, listw = listw, rho = rho)
#' scan <- scan.test(fx = fx, nsim = 99, case = "A", nv = 50, coor = cbind(cx,cy),
#' distr = "bernoulli",windows="circular")
#' print(scan)
#' plot(scan)
#'
#' # Case 5: DGP three categories
#' N <- 200
#' cx <- runif(N)
#' cy <- runif(N)
#' listw <- spdep::knearneigh(cbind(cx,cy), k = 10)
#' p <- c(1/3,1/3,1/3)
#' rho <- 0.5
#' fx <- dgp.spq(p = p, listw = listw, rho = rho)
#' scan <- scan.test(fx = fx, nsim = 19, coor = cbind(cx,cy), nv = 30,
#' distr = "multinomial", windows = "elliptic")
#' print(scan)
#' plot(scan)
#'
#' # Case 6: Flexible windows
#' data(provinces_spain)
#' sf::sf_use_s2(FALSE)
#' provinces_spain$Mal2Fml <- factor(provinces_spain$Mal2Fml > 100)
#' levels(provinces_spain$Mal2Fml) = c("men","woman")
#' formula <- ~ Mal2Fml
#' listw <- spdep::poly2nb(provinces_spain, queen = FALSE)
#' scan <- scan.test(formula = formula, data = provinces_spain, case="men", listw = listw, nv = 6,
#'                   nsim = 99, distr = "bernoulli", windows = "flexible")
#' print(scan)
#' summary(scan)
#' plot(scan, sf = provinces_spain)
#' }

scan.test <- function(formula = NULL, data = NULL, fx = NULL, coor = NULL, case = NULL,
                      nv = NULL, nsim = NULL, distr = NULL, windows = "circular", listw = NULL,
                      alternative = "High", minsize = 1, control = list()) {

  if (is.null(distr))
    stop("Select a distribution, bernoulli or multinomial")

  coor.input <- coor
  distr <- match.arg(distr, c("bernoulli", "multinomial"))
  windows <- match.arg(windows, c("circular", "elliptic","flexible"))
  # if flexible windows
  if (windows=="flexible" && is.null(listw)) stop("A list of neigbourhood must be included")
  ## Define the W matrix
  if (windows=="flexible"){
    if (inherits(listw, "knn")){
      listw <- nb2listw(knn2nb(listw),zero.policy = TRUE)
    } else if (inherits(listw, "matrix")){
      listw <- (listw > 0)*1
      listw <- mat2listw(listw)
    } else if (inherits(listw, "nb")){
      listw <- nb2listw(listw,zero.policy = TRUE)
    }
  }
  #'
  # Select the arguments: (formula + data) or bien incluye la variable (fx)
  if (!is.null(formula) && !is.null(data)) {
    if (inherits(data, "Spatial")) data <- as(data, "sf")
    mfx <- get_all_vars(formula, data)[,1]
    data.name <- names(get_all_vars(formula, data))
  } else if (!is.null(fx)) {
    mfx <- fx
    if (is.null(names(fx))) data.name <- "fx"
  } else stop("data wrong")

  if (!is.factor(mfx))
    stop(paste(deparse(substitute(fx)), "is not a factor"))

  # Si se introduce un objeto sf. Para controlar errores en el metodo plot
  sf = FALSE
  if (inherits(data, "sf")) sf = TRUE

  if (distr == "bernoulli"){
    alternative <- match.arg(alternative, c("Low", "High", "Both"))
    if (is.null(case)) {stop ("case argument must be an element of the factor")}
    if (length(unique(mfx))!=2) {stop ("The factor mut be have 2 levels for bernoulli")}
    case <- match.arg(case, unique(mfx))
  }
  if (distr == "multinomial"){
    if (length(unique(mfx)) < 3) {stop ("The factor must be have almost 3 levels for multinomial")}
    case <- match.arg(case, unique(mfx))
  }

  # Controls
  con <- list(seedinit = 123)
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  if (length(noNms <- namc[!namc %in% nmsC]))
    warning("unknown names in control: ", paste(noNms, collapse = ", "))
  seedinit <- con$seedinit

  ## Scan
  N <- length(mfx)
  if (is.null(nv))  nv <- trunc(N/2)
  if (!is.null(nv) && nv > trunc(N/2)) stop("nv must be lower than N/2")

  if (minsize > nv/2)
    stop("minsize must be lower than nv/2")
  ## Previus

  if (is.null(coor) &&
      sum(inherits(data, "sf")) > 0){
    coor <- suppressWarnings(st_coordinates(st_centroid(data)))
  }

  cx <- coor[,1]
  cy <- coor[,2]

  if (windows=="circular"){
    nn <- suppressWarnings(cbind(1:N, knearneigh(cbind(cx, cy),
                                                 k = (nv-1))$nn))
  }
  if (windows=="elliptic"){
    nn <- suppressWarnings(nn_ellipse(coor = cbind(cx,cy), nv = nv, p = 30)$ellipses)
  }
  if (windows=="flexible"){
    nn <- nn_flexible(W = listw$neighbours, nv = nv)
  }
  # XF <- matrix(mfx[nn], ncol = nv, nrow = N)
  XF <- matrix(mfx[nn], ncol = nv, nrow = dim(nn)[1])

  #####################################
  ## Obtaining the scan statisitic
  #####################################
  ## Bernoulli
  if (distr == "bernoulli"){
    oz <- t(apply(XF == case, 1 , cumsum))
    nz <- t(matrix(rep(1:nv,dim(nn)[1]),nrow = nv))
    O <- sum(mfx== case)
    oznz <- oz/nz
    OozNnz <- (O-oz)/(N-nz)
    OozNnz.1 <- 1 - OozNnz
    OozNnz.1[OozNnz.1 < 0] <- 0
    a <- log(oznz)
    a[a==-Inf]<-0
    b <- log(1-oznz)
    b[b==-Inf]<-0
    c <- log(OozNnz)
    c[c==-Inf]<-0
    d <- log(OozNnz.1)
    d[d==-Inf] <- 0
    if (alternative == "Both"){
      lnlz <- exp(oz*a + (nz-oz)*b + (O-oz)*c + (N-O-nz+oz)*d)
    }
    if (alternative == "High"){
      lnlz <- exp(oz*a + (nz-oz)*b + (O-oz)*c + (N-O-nz+oz)*d)*(oznz>OozNnz)
    }
    if (alternative == "Low"){
      lnlz <- exp(oz*a + (nz-oz)*b + (O-oz)*c + (N-O-nz+oz)*d)*(oznz<OozNnz)
    }
    # lnlz <- oz*log(oz/nz) + (nz-oz)*log(1-oz/nz) + (O-oz)*log((O-oz)/(N-nz)) + (N-O-nz+oz)*log(1-(O-oz)/(N-nz))
    lnlz0 <- exp(O*log(O) + (N-O)*log(N-O) - N*log(N))
    lnlz <- log(lnlz/lnlz0)
    lnlz[lnlz==-Inf] <- 0
    lnlz[is.na(lnlz)] <- 0
    # if (is.null(minsize)){
    #   a <- which(lnlz == max(lnlz), arr.ind = TRUE)
    #   MLC <- nn[a[1,1],1:a[1,2]]
    #   loglik <- max(lnlz)} ## With out restrictions in the number of cases in the MLC
    # else{

    a <- which(lnlz[,minsize:nv] == max(lnlz[,minsize:nv]), arr.ind = TRUE)
    # MLC <- nn[a[1,1],1:(minsize+a[1,2])]
    MLC <- nn[a[1,1],1:min(dim(nn)[2],minsize+a[1,2])]
    loglik <- max(lnlz[,minsize:nv])
    # }
    a2 <- a
    cases.observ <- sum(mfx[MLC]== case)
    cases.expect <- a[2]*(O/N)
    cases.names <- names(table(mfx[MLC]))
    AlternativeMLC <- list()
    if (dim(a2)[1]>1){
      for (fl in 2:dim(a2)[1]){
        if (all.equal(sort(MLC),sort(nn[a2[fl,1],1:a2[fl,2]]))==FALSE){
          AlternativeMLC[[fl-1]] <- nn[a2[fl,1],1:a2[fl,2]]}
      }
    }
    if (length(AlternativeMLC) > 0) {
      warning(paste0("A total of ",length(AlternativeMLC), " clusters has the same value of the statistic.
    Report as MLC only the first.
    Use summary() to get all clusters with the same loglik"))
    }
  }
  ## Multinomial
  if (distr == "multinomial"){
    lnlz <- 0
    case <- unique(mfx)
    CZ <- t(matrix(rep(1:nv,dim(nn)[1]),nrow = nv))
    for (f in 1:length(case)){
      Ck <- sum(mfx == case[f])
      CkZ <- t(apply(XF == case[f] , 1 , cumsum))
      a <- log(CkZ/CZ)
      a[a==-Inf] <- 0
      b <- log((Ck-CkZ)/(N-CZ))
      b[b==-Inf] <- 0
      c <- log(Ck/N)
      c[c==-Inf] <- 0
      lnlz <- lnlz + (CkZ*a + (Ck-CkZ)*b - Ck*c)
    }
    lnlz[is.na(lnlz)] <- 0
    # Condición para seleccionar el MLC dependiendo si se exige un num min de observaciones
    # if (is.null(minsize)){
      # a <- which(lnlz == max(lnlz), arr.ind = TRUE)
      # MLC <- nn[a[1,1],1:a[1,2]]
      # loglik <- max(lnlz) ## With out restrictions in the number of cases in the MLC
      # } else {
      a <- which(lnlz[,minsize:nv] == max(lnlz[,minsize:nv]), arr.ind = TRUE)
      MLC <- nn[a[1,1],1:(minsize-1+a[1,2])]
      loglik <- max(lnlz[,minsize:nv])

    a2 <- a
    # if (dim(a)[1] > 1) {
    #   a <- a[1,]
    # }
    cases.observ <- addmargins(table(mfx[MLC]))
    cases.expect <- addmargins(table(mfx)*length(MLC)/N)
    cases.names <- names(table(mfx[MLC]))
    # Alternative clusters with the same loglik that MLC
    # Es posible encontrar más de un cluster con la misma loglik
    # Lo normal es que estén formados por las mismas observaciones
    # la siguiente lineas chequean si todos son iguales
    # Si no los son los llama alternativos y son listados
    AlternativeMLC <- list()
    if (dim(a2)[1]>1){
      for (fl in 2:dim(a2)[1]){
        if (all.equal(sort(MLC),sort(nn[a2[fl,1],1:a2[fl,2]]))==FALSE){
          AlternativeMLC[[fl-1]] <- nn[a2[fl,1],1:a2[fl,2]]}
      }
    }

    ####################################
    if (length(AlternativeMLC) > 0) {
      warning(paste0("A total of ",length(AlternativeMLC), " clusters has the same value of the statistic.
    Report as MLC only the first.
    Use summary() to get all clusters with the same loglik"))
    }
  }

  #####################################
  ## Scan mc
  #####################################
  if (distr == "bernoulli"){
    if (!is.null(seedinit)) set.seed(seedinit)
    scan.mc <- rep(0,nsim)

    for (f in 1:nsim){
      fxp <- mfx[sample(N)]

      XF <- matrix(fxp[nn], ncol = nv, nrow = dim(nn)[1])
      oz <- t(apply(XF==case, 1 , cumsum))
      # XF2 <- XF == case
      # XF2 <- split(XF2, row(XF2))
      # oz2 <- lapply(XF2, cumsum)
      # oz2 <- do.call("rbind", oz2)
      # oz <- t(apply_cumsum_col(t(XF == case)))
      oznz <- oz/nz
      OozNnz <- (O-oz)/(N-nz)
      OozNnz.1 <- 1 - OozNnz
      OozNnz.1[OozNnz.1 < 0] <- 0
      a <- log(oznz)
      a[a==-Inf] <- 0
      b <- log(1-oznz)
      b[b==-Inf] <- 0
      c <- log(OozNnz)
      c[c==-Inf] <- 0
      d <- log(OozNnz.1)
      d[d==-Inf] <- 0
      if (alternative == "Both"){
        lnlz <- exp(oz*a + (nz-oz)*b + (O-oz)*c + (N-O-nz+oz)*d)
      }
      if (alternative == "High"){
        lnlz <- exp(oz*a + (nz-oz)*b + (O-oz)*c + (N-O-nz+oz)*d)*(oznz > OozNnz)
      }
      if (alternative == "Low"){
        lnlz <- exp(oz*a + (nz-oz)*b + (O-oz)*c + (N-O-nz+oz)*d)*(oznz < OozNnz)
      }
      lnlz <- log(lnlz/lnlz0)
      lnlz[lnlz==-Inf] <- 0
      lnlz[is.na(lnlz)] <- 0
      scan.mc[f] <- max(lnlz)
    }
  }

  if (distr == "multinomial"){
    if (!is.null(seedinit)) set.seed(seedinit)
    scan.mc <- rep(0,nsim)
    case <- unique(mfx)
    CZ <- t(matrix(rep(1:nv,dim(nn)[1]),nrow = nv))
    for (k in 1:nsim){
      fxp <- mfx[sample(N)]
      XF <- matrix(fxp[nn], ncol = nv, nrow = dim(nn)[1])
      lnlz <- 0
      for (f in 1:length(case)){
        Ck <- sum(mfx == case[f])
        CkZ <- t(apply(XF == case[f] , 1 , cumsum))
        a <- log(CkZ/CZ)
        a[a==-Inf] <- 0
        b <- log((Ck-CkZ)/(N-CZ))
        b[b==-Inf] <- 0
        c <- log(Ck/N)
        c[c==-Inf] <- 0
        lnlz <- lnlz + (CkZ*a + (Ck-CkZ)*b - Ck*c)
      }
      lnlz[is.na(lnlz)] <- 0
      scan.mc[k] <- max(lnlz[,minsize:nv])

    }
  }
  p.value <- sum(scan.mc > loglik)/(nsim + 1)
  names(loglik) <- "scan-loglik"

  ####################################
  #### Looking for secondary clusters
  ####################################
  loglik.second <- NULL
  MLC2 <- list()
  mlc <- MLC

  for (f in 1:5){ # Como máximo busco 5 clusteres secundarios
    # qq <- nn
    # # Identifico en nn todas las observaciones que contienen algún elemento del MLC
    # pp <- qq[,1] %in% mlc
    # for (ii in 2:dim(qq)[2]){
    #   pp <- cbind(pp,qq[,ii] %in% mlc)
    # }
    pp <- matrix(nn %in% mlc, ncol = nv)
    # Pongo NA si las observación estan en el MLC
    pp[pp == TRUE] <- NA
    pp <- t(apply(pp, 1, cumsum)) # con cumcum se consigue poner NA
    LNLZ <- lnlz
    LNLZ[is.na(pp)] <- NA
    loglik.second[f] <- max(LNLZ[,minsize:nv], na.rm = TRUE)
    cc <- which(LNLZ == max(LNLZ[,minsize:nv], na.rm = TRUE), arr.ind = TRUE)
    # Este es el cluster secundario

    # MLC2[[f]] <- nn[cc[1,1],1:(minsize+cc[1,2])]
    # mlc <- c(mlc,nn[cc[1,1],1:(minsize+cc[1,2])])
    MLC2[[f]] <- nn[cc[1,1],1:min(dim(nn)[2],(minsize+cc[1,2]))]
    mlc <- c(mlc,nn[cc[1,1],1:min(dim(nn)[2],(minsize+cc[1,2]))])
  }
  rm(pp,LNLZ,cc,mlc)

  ####################################
  ## p-values for secondary clusters
  p.value.secondary <- NULL
  for (f in 1:5){
    p.value.secondary[f] <- sum(scan.mc > loglik.second[f])/(nsim + 1)
  }

  ####################################

  if (distr == "bernoulli"){
    vec <- matrix(c(length(MLC), cases.expect, cases.observ))
    rownames(vec) <- c("Total observations in the MLC =  ","Expected cases in the MLC =","Observed cases in the MLC =")
    colnames(vec) <- ""
    scan <- list(method = paste("Scan test. Distribution: ",distr),
                 fx = mfx, MLC = MLC, statistic = loglik, N = N, estimate = vec, nn = nn, nv = nv, coor = coor.input,
                 p.value = p.value, nsim = nsim, data.name = data.name, distr = distr,
                 scan.mc = scan.mc, minsize = minsize,
                 secondary.clusters = MLC2, loglik.second = loglik.second, p.value.secondary = p.value.secondary,
                 case = case,
                 alternative = alternative,
                 cases.expect = cases.expect,
                 cases.observ = cases.observ,
                 cases.names = cases.names,
                 sf = sf)
  }

  if (distr == "multinomial"){
    vec <- round(rbind(cases.expect,cases.observ),2)
    # names(vec) <- c("Number observ inside MLC","Expected cases in MLC","Observed cases in MLC")
    scan <- list(method = paste("Scan test. Distribution: ",distr),
                 fx = mfx, MLC = MLC, statistic = loglik, N = N, estimate = vec, nn = nn, nv = nv, coor = coor.input,
                 p.value = p.value, nsim = nsim, data.name = data.name, distr = distr,
                 scan.mc = scan.mc, minsize = minsize,
                 secondary.clusters = MLC2, loglik.second = loglik.second, p.value.secondary = p.value.secondary,
                 cases.expect = cases.expect,
                 cases.observ = cases.observ,
                 cases.names = cases.names,
                 sf = sf)
  }


  class(scan) <- c("htest","scantest")
  scan
}
