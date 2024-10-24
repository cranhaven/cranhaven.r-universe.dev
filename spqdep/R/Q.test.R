#' @name Q.test
#' @rdname Q.test
#' @title A function to compute Q test for spatial qualitative data
#' @description A function to compute Q test for spatial qualitative data.
#' @usage Q.test(formula = NULL, data = NULL, na.action,
#' fx = NULL, coor = NULL, m = 3, r = 1, distr = "asymptotic",
#' control = list())
#' @param formula a symbolic description of the factor(s).
#' @param data an (optional) data frame or a sf object
#'             with points/multipolygons geometry containing the
#'             variable(s) to be tested.
#' @param fx  a factor  or a matrix of factors in columns
#' @param na.action action with NA values
#' @param distr character. Distribution type "asymptotic" (default) or "mc".
#' @param m length of m-surrounding (default = 3).
#' @param r only for asimtotic distribution. Maximum overlapping between any two m-surroundings (default = 1).
#' @param coor (optional) a 2xN vector with spatial coordinates.
#'             Used when *data* is not a spatial object
#' @param control Optional argument. See Control Argument section.
#'
#' @return An list of two object of the class \code{htest}. Each element of the list return the:
#'   \tabular{ll}{
#'     \code{data.name} \tab a character string giving the names of the data.\cr
#'     \code{statistic} \tab Value of the Q test \cr
#'     \code{N} \tab total number of observations.\cr
#'     \code{R} \tab total number of symbolized observations.\cr
#'     \code{m} \tab length m-surrounding.\cr
#'     \code{r} \tab degree of overlapping.\cr
#'     \code{df} \tab degree of freedom.\cr
#'     \code{distr} \tab type of distribution used to get the significance of the Q test.\cr
#'     \code{type} \tab type of symbols.\cr
#'     }
#' @section Control arguments:
#' \describe{
#' \item{distance}{character to select the type of distance.
#' Default = "Euclidean" for Cartesian coordinates only: one of Euclidean,
#' Hausdorff or Frechet; for geodetic coordinates,
#' great circle distances are computed (see sf::st_distance())}
#' \item{dtmaxabs}{Delete degenerate surrounding based on the absolute distance between observations.}
#' \item{dtmaxpc}{A value between 0 and 1. Delete degenerate surrounding based on the distance. Delete m-surrounding when the maximum distance between observation
#' is upper than k percentage of maximum distance between anywhere observation.}
#' \item{dtmaxknn}{A integer value 'k'. Delete degenerate surrounding based on the near neighbourhood criteria. Delete m-surrounding
#' is a element of the m-surrounding is not include in the set of k near neighbourhood of the first element}
#' \item{nsim}{number of simulations for get the Monte Carlo distribution.
#'  Default = 999}
#' \item{seedinit}{seed to select the initial element to star
#' the algorithm to get compute the m-surroundings or to start the simulation}
#' }
#'
#' @details
#' The Q-test is a simple, consistent, and powerful statistic for qualitative spatial
#'  independence that we develop using concepts from symbolic dynamics and symbolic
#'   entropy. The Q test can be used to detect, given a spatial distribution of events,
#'   patterns of spatial association of qualitative variables in a wide variety of
#'   settings. \cr
#' The Q(m) statistic was introduced by Ruiz et al. (2010) as a tool to explore geographical
#' co-location/co-occurrence of qualitative data. Consider a spatial variable X which is the
#' result of a qualitative process with a set number of categorical outcomes \eqn{a_j} (j=1,...,k).
#' The spatial variable is observed at a set of fixed locations indexed by their coordinates
#' \eqn{s_i} (i=1,..., N), so that at each location si where an event is observed,
#' \eqn{X_i} takes one of the possible values \eqn{a_j}.\cr
#'
#' Since the observations are georeferenced, a spatial embedding protocol can be devised
#' to assess the spatial property of co-location. Let us define, for an observation at
#' a specified location, say \eqn{s_0}, a surrounding of size m, called an m-surrounding.\cr
#' The m-surrounding is the set of m-1 nearest neighbours from the perspective
#' of location \eqn{s_0}. In the case of distance ties, a secondary criterion can be
#' invoked based on direction.\cr
#' Once that an embedding protocol is adopted and the elements of the m-surrounding
#' for location \eqn{s_0} have been determined, a string can be obtained that collects
#' the elements of the local neighborhood (the m-1 nearest neighbors) of the observation
#' at \eqn{s_0}. The m-surrounding can then be represented in the following way:
#'
#' \deqn{X_m(s_0)=(X_{s_0},X_{s_1},...X_{s_{m-1}})}
#' Since each observation Xs takes one of k possible values, and there are m observations in
#' the m-surrounding, there are exactly k possible unique ways in which those values can
#' co-locate. This is the number of permutations with replacement.\cr
#' For instance, if k=2
#' (e.g. the possible outcomes are a1=0 and a2=1) and m=3, the following eight unique
#' patterns of co-location are possible (the number of symbols is \eqn{n_{\sigma}}=8): (0,0,0), (1,0,0),
#' (0,1,0), (0,0,1), (1,1,0), (1,0,1), (0,1,1), and (1,1,1). Each unique co-locationtype
#' can be denoted in a convenient way by means of a symbol \eqn{\sigma_i} \eqn{(i=1, 2,...,k^m)}. It follows
#' that each site can be uniquely associated with a specific symbol, in a process termed
#' symbolization. In this way, we say that a location s is of type \eqn{\sigma_i} if and only if \eqn{X_m(s)=\sigma_i}.\cr
#' Equivalent symbols (see Páez, et al. 2012) can be obtained by counting the number of
#' occurrences of each category within an m-surrounding. This surrenders some
#' topological information (ordering within the m-surrounding is lost) in favor of a more
#' compact set of symbols, since the number of combinations with replacement.\cr
#' \cr
#' \strong{Definition of Q(m) statistic}\cr
#' \cr
#' Let \eqn{\{X_s\}_{s \in R}} be a discrete spatial process and m be a fixed embedding
#' dimension. The statistic Q testing the null hypothesis:\cr
#' \cr
#' \eqn{H_0:\{X_s\}_{s \in R}} is spatially independent, against any other alternative.\cr
#' \cr
#' For a fixed \eqn{m \geq 2}, the relative frequency of symbols can be used to define the symbolic
#' entropy of the spatial process as the Shanon entropy of the distinct symbols:\cr
#' \deqn{h(m) = - \sum_j p_{\sigma_j}ln(p_{\sigma_j})}
#' where
#' \deqn{p_{\sigma_j}={ n_{\sigma_j} \over R}}
#' with \eqn{n_{\sigma_j}} is simply the
#' number of times that the symbol \eqn{\sigma_j} is observed and R the number of
#' symbolized locations.
#' The entropy function is bounded between \eqn{0 < h (m) \leq \eta}.\cr
#'
#' The Q statistic is essentially a likelihood ratio test between the symbolic entropy
#' of the observed pattern and the entropy of the system under the null hypothesis
#' of a random spatial sequence:
#'  \deqn{Q(m)=2R(\eta-h(m))}
#' with \eqn{\eta = ln(k^m)}. The statistic is asymptotically \eqn{\chi^2} distributed
#' with degrees of freedom equal to the number of symbols minus one.\cr
#'
#' @section Standard-Permutation vs Equivalent-Combination Symbols:
#' The symbolization protocol proposed by Ruiz et al. (2010) - call these
#' Standard-Permutation Symbols — contains a large amount of topological information
#' regarding the units of analysis, including proximity and direction.
#' In this sense, the protocol is fairly general. On the other hand, it is easy to see
#' that the combinatorial possibilities can very quickly become unmanageable.
#'  For a process with k = 3 outcomes and m = 5, the number of symbols becomes
#'  \eqn{3^5 = 243}; for k = 6 and m = 4 it is  \eqn{6^4 = 1,296}. Depending on the number
#'  of observations N, the explosion in the number of symbols can very rapidly consume
#'  degrees of freedom for hypothesis testing, because as a rule of thumb
#'  it is recommended that the number of symbolized locations be at least five times
#'  the number of symbols used (e.g., \eqn{R \geq 5k^m}), and R will usually be a fraction of N.\cr
#'
#'  As an alternative, we propose a symbolization protocol that sacrifices
#'  some amount of topological detail for conciseness. The alternative is based
#'  on the standard scheme; however, instead of retaining proximity and
#'  direction relationships, it maintains only the total number of occurrences
#'  of each outcome in an m-surrounding. We call these Equivalent-Combination Symbols.
#'  Because order in the sequence is not considered in this protocol, instead of a
#'  permutation with repetition, the number of symbols reflects a combination with
#'  repetition. \cr
#'
#' @section Selection of m-surrounding with Controlled Degree of Overlapping (r):
#'  To select S locations for the analysis, coordinates are selected such that
#'  for any two coordinates \eqn{s_i} , \eqn{s_j} the number of overlapping nearest
#'  neighbours of \eqn{s_i} and \eqn{s_j} are at most r. The set S, which is a subset of all the
#'  observations N, is defined recursively as follows. First choose a location \eqn{s_0} at random and fix an integer r
#'  with \eqn{0 \leq r < m}. The integer r is the degree of overlap, the maximum number of observations that contiguous
#'  m-surroundings are allowed to have in common.\cr
#'  Let \eqn{ \{s_1^0, s_2^0,...,s_{m-1}^0 \}} be the set of nearest neighbours
#'  to \eqn{s_0}, where the \eqn{s_i^0} are ordered by distance to \eqn{s_0}, or angle in the case of ties.
#'  Let us call \eqn{s_1 = s_{m-r-1}^0} and define \eqn{ A_0 = \{s_0,s_0^1,...,s^0_{m-r-2} \}  } . Take the set of
#'  nearest neighbours to \eqn{s_1}, namely, \eqn{ \{ s_1^1, s_2^1,...,s^1_{m-1} \} } in the
#'  set of locations \eqn{S \setminus A_0 } and define \eqn{s_2=s^1_{m-r-1} }. Nor for i>1 we define
#'  \eqn{s_i = s^{i-1}_{m-r-1}} where \eqn{s^{i-1}_{m-r-1}} is in the ser of nearest neighbors to \eqn{s_{i-1}},
#'   \eqn{ \{ s_1^{i-1},s_2^{i-1},...,s_{m-1}^{i-1} \} } ,of the set \eqn{S \setminus \{ \cup_{j=0}^{i-1} A_j \} }.
#'   Continue this process while there are locations to symbolize.\cr
#'
#' @section Selection of m-surroundings for bootstrap distribution:
#' The bootstrapped-based testing can provide an advantage since overlapping between
#' m-surroundings is not a consideration, and the full sample can be used.\cr
#'
#' @author
#' \tabular{ll}{
#' Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#' Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#' Antonio Páez \tab \email{paezha@@gmail.com} \cr
#' Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Ruiz M, López FA, A Páez. (2010). Testing for spatial association of qualitative
#'     data using symbolic dynamics. \emph{Journal of Geographical Systems}. 12 (3) 281-309
#'     \item López, FA, and A Páez. (2012). Distribution-free inference for Q(m) based on permutational bootstrapping: an application
#'     to the spatial co-location pattern of firms in Madrid \emph{Estadística Española}, 177, 135-156.
#'   }
#' @export
#' @examples
#'
#' # Case 1: With coordinates
#' N <- 200
#' cx <- runif(N)
#' cy <- runif(N)
#' coor <- cbind(cx,cy)
#' p <- c(1/6,3/6,2/6)
#' rho = 0.3
#' listw <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(cbind(cx,cy), k = 4)))
#' fx <- dgp.spq(list = listw, p = p, rho = rho)
#' q.test <- Q.test(fx = fx, coor = coor, m = 3, r = 1)
#' summary(q.test)
#' plot(q.test)
#' print(q.test)
#' \donttest{
#' q.test.mc <- Q.test(fx = fx, coor = coor, m = 3, distr = "mc", control = list(nsim = 999))
#' summary(q.test.mc)
#' plot(q.test.mc)
#' print(q.test.mc)
#'
#'
#' # Case 2: With a sf object
#' data("FastFood.sf")
#' f1 <- ~ Type
#' q.test <- Q.test(formula = f1, data = FastFood.sf, m = c(3, 4),
#' r = c(1, 2, 3), control = list(distance ="Euclidean"))
#' summary(q.test)
#' plot(q.test)
#' print(q.test)
#'
#' # Case 3: With a sf object with isolated areas
#' data("provinces_spain")
#' sf::sf_use_s2(FALSE)
#' provinces_spain$Mal2Fml<- factor(provinces_spain$Mal2Fml > 100)
#' levels(provinces_spain$Mal2Fml) = c("men","woman")
#' provinces_spain$Older <- cut(provinces_spain$Older, breaks = c(-Inf,19,22.5,Inf))
#' levels(provinces_spain$Older) = c("low","middle","high")
#' f1 <- ~ Older + Mal2Fml
#' q.test <- Q.test(formula = f1,
#' data = provinces_spain, m = 3, r = 1, control = list(seedinit = 1111))
#' summary(q.test)
#' print(q.test)
#' plot(q.test)
#' q.test.mc <- Q.test(formula = f1, data = provinces_spain, m = 4, r = 3, distr = "mc",
#' control = list(seedinit = 1111))
#' summary(q.test.mc)
#' print(q.test.mc)
#' plot(q.test.mc)
#'
#' # Case 4: Examples with multipolygons
#' library(sf)
#' fname <- system.file("shape/nc.shp", package="sf")
#' nc <- sf::st_read(fname)
#' qb79 <- quantile(nc$BIR79)
#' nc$QBIR79 <- (nc$BIR79 > qb79[2]) + (nc$BIR79 > qb79[3]) +
#' (nc$BIR79 >= qb79[4]) + 1
#' nc$QBIR79 <- as.factor(nc$QBIR79)
#' plot(nc["QBIR79"], pal = c("#FFFEDE","#FFDFA2", "#FFA93F", "#D5610D"),
#'      main = "BIR79 (Quartiles)")
#' sid79 <- quantile(nc$SID79)
#' nc$QSID79 <- (nc$SID79 > sid79[2]) + (nc$SID79 > sid79[3]) +
#' (nc$SID79 >= sid79[4]) + 1
#' nc$QSID79 <- as.factor(nc$QSID79)
#' plot(nc["QSID79"], pal = c("#FFFEDE","#FFDFA2", "#FFA93F", "#D5610D"),
#'      main = "SID79 (Quartiles)")
#' f1 <- ~ QSID79 + QBIR79
#' lq1nc <- Q.test(formula = f1, data = nc, m = 5, r = 2,
#' control = list(seedinit = 1111, dtmaxpc = 0.5, distance = "Euclidean") )
#' print(lq1nc)
#'
#' lq2nc <- Q.test(formula = f1, data = nc, m = 5, r = 2,
#' control = list(dtmaxpc = 0.2) )
#' print(lq2nc)
#'
#' lq3nc <- Q.test(formula = f1, data = nc, m = 5, r = 2,
#' control = list(dtmaxknn = 5) )
#' print(lq3nc)
#'
#' # Case 5: Examples with points and matrix of variables
#' fx <- matrix(c(nc$QBIR79, nc$QSID79), ncol = 2, byrow = TRUE)
#' mctr <- suppressWarnings(sf::st_centroid(st_geometry(nc)))
#' mcoor <- st_coordinates(mctr)[,c("X","Y")]
#' q.test <- Q.test(fx = fx, coor = mcoor, m = 5, r = 2,
#'                  control = list(seedinit = 1111, dtmaxpc = 0.5))
#' print(q.test)
#' plot(q.test)
#'
#' }

Q.test <- function(formula = NULL, data = NULL, na.action,
                 fx = NULL, coor = NULL,
                 m = 3, r = 1, distr = "asymptotic",
                 control = list()) {
  con <- list(distance = "Euclidean",
              nsim = 999, seedinit = 1111,
              dtmaxabs = 0, dtmaxpc = 0, dtmaxknn = 0)
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  if (length(noNms <- namc[!namc %in% nmsC]))
    warning("unknown names in control: ", paste(noNms, collapse = ", "))
  distance <- con$distance
  nsim <- con$nsim
  seedinit <- con$seedinit
  dtmaxabs <- con$dtmaxabs
  dtmaxpc <- con$dtmaxpc
  dtmaxknn <- con$dtmaxknn
  cl <- match.call()
   # Lectura Datos.
  if (!is.null(formula) && !is.null(coor) && !is.null(data)) {
    data$Lat <- coor[,1]
    data$Lon <- coor[,2]
    data <- sf::st_as_sf(data, coords = c("Lat","Lon"))
  }
  if (!is.null(formula) && !is.null(data)) {
    if (inherits(data, "Spatial")) data <- as(data, "sf")
    mfx <- model.frame(formula, data, na.action = na.action)
    #mfx <- get_all_vars(formula, data)
  } else if (!is.null(fx) && !is.null(coor)) {
    mfx <- fx
    if (!is.matrix(mfx) && !is.data.frame(mfx)) mfx <- as.matrix(mfx, ncol = 1)
    mfx <- as.data.frame(mfx)
    if (is.matrix(coor)) coor <- sp::SpatialPoints(coor)
    if (inherits(coor, "Spatial")) coor <- as(coor, "sf")
    data <- coor #sf object
  } else stop("input data wrong")
  for (i in 1:ncol(mfx)) {
    if (!is.factor(mfx[,i])) mfx[,i] <- as.factor(mfx[,i])
  }
  mfx <- as.matrix(mfx)
  lres <- NULL # Initialize the list of results
  # Same random initial observation for every m-sorrounding
  if (!is.null(seedinit)) set.seed(seedinit)
  initobs <- sample(1:nrow(mfx), 1)
  for (i in 1:ncol(mfx)) {
    xfi <- mfx[, i]
    if (!is.factor(xfi)) xfi <- as.factor(xfi)
    N <- length(xfi)
    ki <- length(levels(xfi))
    lms <- vector(mode = "list", length = length(m)) #List of m-sorroundings
    for (j in seq_len(length(m))) {
      mj <- m[j]
      symbi <- cr_symb(ki, mj)
      if (distr == "asymptotic") {

        rcut <- r[!(r > (mj - 1))]
        lms[[j]] <- vector(mode = "list",
                           length = length(rcut))
        for (h in seq_len(length(rcut))) {
          rh <- rcut[h]
           ### M-SURROUNDINGS...
          lms[[j]][[h]] <- m.surround(x = data, m = mj, r = rh,
                                      distance = distance,
                                      control = list(initobs = initobs,
                                                     dtmaxabs = dtmaxabs,
                                                     dtmaxpc = dtmaxpc,
                                                     dtmaxknn = dtmaxknn))
          # if (typems == "no") {
          R <- nrow(lms[[j]][[h]]$ms)
          #   lms[[j]][[h]] <- m.surround(x = data, m = mj, r = rh,
          #                        control = list(seedinit = seedinit,
          #                                       dtmaxabs = dtmaxabs,
          #                                       dtmaxpc = dtmaxpc))
          #   #lms__jkpr <- m_surr_no(x = st_coordinates(data), m = mj, s = rh)
          # } else if (typems == "cdt") {
          #   lms[[j]][[h]] <- m_surr_cdt(x = data, m = mj, r = rh,
          #                        control = list(dtmaxabs = dtmaxabs,
          #                                       dtmaxpc = dtmaxpc))
          # } else if (typems == "cbl") {
          #   lms[[j]][[h]] <- m_surr_cbl(x = data, m = mj, r = rh,
          #                        control = list(dtmaxabs = dtmaxabs,
          #                                       dtmaxpc = dtmaxpc))
          # } else stop("Value for argument 'typems' not valid.")

          qi <- q_symb_A2(xfi, lms[[j]][[h]]$ms, symbi)
          qi$qp_df <- nrow(symbi$p_symb) - 1
          qi$qc_df <- nrow(symbi$c_symb) - 1
          statistic <-  c(qi$qp, qi$qc)
          names(statistic) <- c("Qp", "Qc")
          method <- c("Qp (asymptotic distrib.) for standard symbolization based on permutations",
                      "Qc (asymptotic distrib.) for equivalent symbolization based on combinations")
          if (!is.null(colnames(mfx)[i])) {
            data.name <- rep(colnames(mfx)[i], 2)
          } else data.name <- c(NULL, NULL)
          parameter <- c(qi$qp_df, qi$qc_df)
          names(parameter) <- rep("df", 2)
           p.value <- rep(0, length(parameter))
          for (l in 1:length(p.value)) {
            p.value[l] <- pchisq(statistic[l], df = parameter[l],
                                 lower.tail = FALSE)
          }
          lres_i <- vector(mode = "list", length = length(statistic))
          for (t in 1:length(statistic)) {
            lres_i[[t]]<- list(statistic = statistic[t],
                               parameter = parameter[t],
                               p.value = p.value[t],
                               method = method[t],
                               data.name = paste("Variable",
                                                 data.name[t],
                                                 " m = ",mj," r = ", rh),
                               var.name = data.name[t],
                               N = N,
                               R = R,
                               m = mj, r = rh,
                               k = ki, df = parameter[t])
            lres_i[[t]]$distr <- distr
            lres_i[[t]]$ms <- lms[[j]][[h]]$ms
            lres_i[[t]]$mdtms <- lms[[j]][[h]]$mdtms
            lres_i[[t]]$initobs <- lms[[j]][[h]]$initobs
            lres_i[[t]]$distance <- lms[[j]][[h]]$distance
            if(names(statistic)[t] == "Qp") {
              lres_i[[t]]$type <- "standard-permutations"
              lres_i[[t]]$symb <- symbi$p_symb
              lres_i[[t]]$efp_symb <- qi$efp_symb
              lres_i[[t]]$qp_symb <- qi$qp_symb
              lres_i[[t]]$PSymb <- qi$PSymb
            } else if(names(statistic)[t] == "Qc") {
              lres_i[[t]]$type <- "equivalent-combinations"
              lres_i[[t]]$symb <- symbi$c_symb
              lres_i[[t]]$efc_symb <- qi$efc_symb
              lres_i[[t]]$qc_symb <- qi$qc_symb
              lres_i[[t]]$CSymb <- qi$CSymb
            } else { stop("statistic neither Qp or Qc") }
            lres_i[[t]]$n <- nrow(lres_i[[t]]$symb)
            class(lres_i[[t]]) <- c("htest")
          }
          lres <- c(lres, lres_i)
        } # end for (h in seq_len(length(rcut)))
      } else if (distr == "mc") {
        qi <- q_mc(fx = xfi, x = data,
                      m = mj, nsim = nsim,
                      distance = distance,
                      seedinit = seedinit)
        statistic <-  c(qi$qp, qi$qc)
        names(statistic) <- c("Qp", "Qc")
        method <- c("Qp (mc distrib.) for symbolization based on permutations",
                    "Qc (mc distrib.) for symbolization based on combinations")
        if (!is.null(colnames(mfx)[i])) {
          data.name <- rep(colnames(mfx)[i], 2)
        } else data.name <- c(NULL, NULL)
        parameter <- c("NA", "NA")
        names(parameter) <- rep("df", 2)
        p.value <- c(qi$pvaluemc_qp,
                     qi$pvaluemc_qc)
        lres_i <- vector(mode = "list",
                         length = length(statistic))
        for (t in 1:length(statistic)) {
          lres_i[[t]]<- list(statistic = statistic[t],
                             parameter = parameter[t],
                             p.value = p.value[t],
                             method = method[t],
                             data.name = paste("Variable",
                                               data.name[t],
                                               " m = ",mj,
                                               " r = ", mj-1),
                             var.name = data.name[t],
                             N = N,
                             R = nrow(qi$ms),
                             m = mj, r = mj-1,
                             k = ki, df = parameter[t])
          lres_i[[t]]$distr <- distr
          lres_i[[t]]$ms <- qi$ms
          lres_i[[t]]$mdtms <- qi$mdtms
          lres_i[[t]]$distance <- qi$distance
          if(names(statistic)[t] == "Qp") {
            lres_i[[t]]$type <- "standard-permutations"
            lres_i[[t]]$symb <- symbi$p_symb
            lres_i[[t]]$efp_symb <- qi$efp_symb
            lres_i[[t]]$qp_symb <- qi$qp_symb
            lres_i[[t]]$PSymb <- qi$PSymb
            lres_i[[t]]$qp_mc <- qi$qpmc
            lres_i[[t]]$efp_symb_mc <- qi$efp_symb_mc
          } else if(names(statistic)[t] == "Qc") {
            lres_i[[t]]$type <- "equivalent-combinations"
            lres_i[[t]]$symb <- symbi$c_symb
            lres_i[[t]]$efc_symb <- qi$efc_symb
            lres_i[[t]]$qc_symb <- qi$qc_symb
            lres_i[[t]]$CSymb <- qi$CSymb
            lres_i[[t]]$qc_mc <- qi$qcmc
            lres_i[[t]]$efc_symb_mc <- qi$efc_symb_mc
          } else { stop("statistic neither Qp or Qc") }
          lres_i[[t]]$n <- nrow(lres_i[[t]]$symb)
          class(lres_i[[t]]) <- c("htest")
        }
        lres <- c(lres, lres_i)
      }
    } # end for (j in seq_len(length(m)))
  } # end for (i in 1:ncol(mfx))
  class(lres) <- c("spqtest", class(lres))
  return(lres)
}
