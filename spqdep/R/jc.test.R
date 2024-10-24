#' @name jc.test
#' @rdname jc.test
#' @title A function to compute joint-count test for
#'   binomial and multinomial (asymptotic and
#'   permutation distributions).
#' @usage jc.test(formula = NULL,
#'                data = NULL,
#'                fx = NULL,
#'                listw = NULL,
#'                na.action,
#'                zero.policy = NULL,
#'                distr = "asymptotic",
#'                alternative = "greater",
#'                control =list())
#' @param na.action	A function (default \code{options("na.action")}),
#'   can also be \code{na.omit} or \code{na.exclude}.
#'   It may be necessary to set \code{zero.policy}
#'   to \code{TRUE} because this subsetting may
#'   create no-neighbour observations.
#' @param listw A \code{listw} object created for example by
#'   \code{\link[spdep]{nb2listw}} from \pkg{spdep} package;
#'   if \code{\link[spdep]{nb2listw}} not given,
#'   the spatial weights are built using the object given
#'   in \code{listw} argument (usually an \code{sf}
#'   object). Default = \code{NULL}.
#' @param zero.policy Similar to the corresponding parameter of
#'   \code{\link[spatialreg]{lagsarlm}} function in
#'   \pkg{spatialreg} package.
#'   If \code{TRUE} assign zero to the lagged value of zones without
#'   neighbours. Default = \code{NULL}.
#' @param control list of additional arguments.
#' @param alternative character string specifying the alternative hypothesis,
#' must be one of "greater" (default), or "less".
#' @inheritParams Q.test
#'
#' @description A function to compute joincount
#'   tests for spatial qualitative data. This function
#'   is a wrapper of \code{\link[spdep]{joincount.multi}}
#'   and \code{\link[spdep]{joincount.test}}
#'   in \pkg{spdep} package.
#'
#' @return An \code{spjctest} object. This type of object
#'   is a list of \code{htest} objects.
#'   The length of the list is the number of
#'   factor variables included in the
#'   \code{formula} or the number of columns
#'   in \code{fx}.
#'   Each element of the list can be a
#'   \code{jclist} object, for binomial factors,
#'   or a \code{jcmulti} object for multinomial
#'   factors. See \code{\link[spdep]{joincount.test}}
#'   or \code{\link[spdep]{joincount.multi}}
#'   for additional details.
#' @section Control arguments:
#'   \tabular{ll}{
#'     \code{nsim} \tab number of permutations used in
#'        the Monte Carlo distribution.
#'        Default \code{nsim} = 999. \cr
#'     \code{seedinit} \tab seed to select the initial
#'       element during the simulations.
#'       Default \code{seedinit} = 1111. \cr
#'    \code{adjust.n} \tab default *TRUE*,
#'      if *FALSE* the number of observations
#'      is not adjusted for no-neighbour
#'      observations, if *TRUE*, the number of
#'      observations is adjusted consistently.
#'      See \code{\link[spdep]{joincount.multi}}
#'      for aditional information. \cr
#'    \code{spChk} \tab	should the data vector
#'      names be checked against the spatial
#'      objects for identity integrity,
#'      *TRUE*, or *FALSE*, default *NULL* to
#'      use *get.spChkOption()*.
#'      See \code{\link[spdep]{joincount.multi}}
#'      for aditional information. \cr
#'    \code{sampling} \tab default *nonfree*,
#'      may be *free*.
#'      See \code{\link[spdep]{joincount.test}} for
#'      additional information. \cr
#'    \code{queen} \tab default *TRUE*. Defines
#'      the neighborhood criteria for *sf*
#'      objects. \cr
#'    \code{style} \tab defines the style for
#'      \code{listw}. Default = *B* (binary). \cr
#'    \code{knn} \tab chooses the number of
#'      neighboors when this criteria is used.
#'      Default \code{knn} = 5.
#' }
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
#'   \item Cliff, A. D., Ord, J. K. 1981 \emph{Spatial processes}, Pion, pp. 19-20.
#'   \item Upton, G., Fingleton, B. 1985 \emph{Spatial data analysis by example: point pattern and qualitative data}, Wiley, pp. 158–170.
#'   }
#'
#' @seealso
#'   \code{\link{print.summary.spqtest}},
#'   \code{\link[spdep]{joincount.test}},
#'   \code{\link[spdep]{joincount.multi}}
#'
#' @export
#' @examples
#'
#' ## Case 1
#' ## Multinomial + Binomial using a sf multipolygon
#' \donttest{
#' data("provinces_spain")
#' # sf::sf_use_s2(FALSE)
#' provinces_spain$Mal2Fml <- factor(provinces_spain$Mal2Fml > 100)
#' levels(provinces_spain$Mal2Fml) = c("men","woman")
#' provinces_spain$Older <- cut(provinces_spain$Older, breaks = c(-Inf,19,22.5,Inf))
#' levels(provinces_spain$Older) = c("low","middle","high")
#' f1 <- ~ Older + Mal2Fml
#' jc1 <- jc.test(formula = f1,
#'                data = provinces_spain,
#'                distr = "mc",
#'                alternative = "greater",
#'                zero.policy = TRUE)
#' summary(jc1)
#'
#' provinces_spain$Coast <- factor(provinces_spain$Coast)
#' levels(provinces_spain$Coast) = c("no","yes")
#' f2 <- ~ Mal2Fml + Coast
#' jc2 <- jc.test(formula = f2,
#'                data = provinces_spain,
#'                distr = "mc",
#'                zero.policy = TRUE)
#' summary(jc2)
#' }
#'
#' # Case 2:
#' ## Multinomial using a sf multipoint
#' data("FastFood.sf")
#' # sf::sf_use_s2(FALSE)
#' f1 <- ~ Type
#' jc3 <- jc.test(formula = f1,
#'                data = FastFood.sf,
#'                distr = "asymptotic",
#'                control = list(knn = 6))
#'  summary(jc3)
#'
#' # Examples function joincount.test
#' data(oldcol, package = "spdep")
#' HICRIME <- cut(COL.OLD$CRIME, breaks = c(0,35,80), labels = c("low","high"))
#' names(HICRIME) <- rownames(COL.OLD)
#' jc4 <- jc.test(fx = HICRIME,
#'                listw = spdep::nb2listw(COL.nb,
#'                style="B"))
#' summary(jc4)
#'  jc5 <- jc.test(fx = HICRIME,
#'                 listw = spdep::nb2listw(COL.nb, style="B"),
#'                 distr = "mc")
#'  summary(jc5)
#'  HICRIME <- cut(COL.OLD$CRIME, breaks = c(0, 35, 80),
#'                 labels = c("low","high"))
#'  names(HICRIME) <- rownames(COL.OLD)
#'  jc6 <- jc.test(fx = HICRIME,
#'                 listw = spdep::nb2listw(COL.nb,
#'                                  style = "B"))
#' summary(jc6)
#'
jc.test <- function (formula = NULL,
                     data = NULL,
                     fx = NULL,
                     listw = NULL,
                     na.action,
                     zero.policy = NULL,
                     distr = "asymptotic",
                     alternative = "greater",
                     control =list()) {
  con <- list(sampling = "nonfree",
              adjust.n = TRUE, spChk = NULL,
              nsim = 999, seedinit = 1111,
              queen = TRUE, style = "B",
              knn = 5)
  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  if (length(noNms <- namc[!namc %in% nmsC]))
    warning("unknown names in control: ",
            paste(noNms, collapse = ", "))
  queen <- con$queen
  style <- con$style
  knn <- con$knn
  nsim <- con$nsim
  sampling <- con$sampling
  adjust.n <- con$adjust.n
  spChk <- con$spChk
  cl <- match.call()
  if (is.null(zero.policy))
    zero.policy <- get.ZeroPolicyOption()
  stopifnot(is.logical(zero.policy))
  if (!is.null(formula) && !is.null(data)) {
    if (inherits(data, "Spatial")) data <- as(data, "sf")
    if (inherits(data, "sf")) {
      geom_data <- st_geometry(data)
      if (inherits(geom_data,
                   c("sfc_MULTIPOLYGON",
                     "sfc_POLYGON")))
        nbdata <- poly2nb(data,
                                 queen = queen)
      if (inherits(geom_data,
                   c("sfc_POINT")))
        nbdata <- knn2nb(
                    knearneigh(x = data,
                                      k = knn))
      listw <- nb2listw(neighbours = nbdata,
                               style = style,
                               zero.policy = zero.policy)
    }
    mfx <- model.frame(formula, data,
                              na.action = na.action)
  } else if (!is.null(fx)) { mfx <- fx }
  if (!is.null(listw)) listw = listw # listw provided as argument
  mfx <- as.matrix(mfx)
  lres <- vector(mode = "list", length = ncol(mfx))
  # Initialize the list of results
  for (i in 1:ncol(mfx)) {
    fxi <- mfx[,i]
    if (!is.factor(fxi)) fxi <- as.factor(fxi)
    if (!is.null(colnames(mfx)[i]))
      data.name <- colnames(mfx)[i]
    else {
      if (ncol(mfx) > 1)
        data.name <- paste("fx_",i)
      else data.name <- "fx"
    }
    N <- length(fxi)
    ki <- length(levels(fxi))
    if (ki == 2) {
      # Binomial
      if (distr == "asymptotic") {
        jci <- joincount.test(
          fx = fxi, listw = listw,
          zero.policy = zero.policy,
          sampling = sampling,
          spChk = spChk,
          adjust.n = adjust.n
        )
        jcimulti <- joincount.multi(fx = fxi,
                                      listw = listw,
                                      zero.policy = zero.policy,
                                      spChk = spChk,
                                      adjust.n = adjust.n)
        jci[[3]] <- jci[[1]]
        jci[[3]]$statistic <- jcimulti[3, 4]
        names(jci[[3]]$statistic) <- paste(
          "Std. deviate for ", rownames(jcimulti)[3], sep = "")
        jci[[3]]$estimate <- jcimulti[3, 1:3]
        names(jci[[3]]$estimate) <- c("Diferent colour statistic",
                                      "Expectation", "Variance")
        if (alternative == "greater") {
          jci[[3]]$p.value <- pnorm(jcimulti[3, c("z-value")],
                          mean = 0, sd = 1,
                          lower.tail = FALSE)
        } else if (alternative == "lower") {
          jci[[3]]$p.value <- pnorm(jcimulti[3, c("z-value")],
                          mean = 0, sd = 1,
                          lower.tail = TRUE)
        } else {
          jci[[3]]$p.value <- 2*pnorm(abs(jcimulti[3, c("z-value")]),
                            mean = 0, sd = 1,
                            lower.tail = FALSE)

        }
        rm(jcimulti)
      } else {
        # FALTA POR INCLUIR ESTADÍSTICO BLACK-WHITE EN ESTE CASO...
        jci <- joincount.mc(
          fx = fxi,
          listw = listw,
          nsim = nsim,
          zero.policy = zero.policy,
          spChk = spChk
        )
      }
      for (j in 1:length(jci)) {
        if (j < 3) {
          levelj <- levels(fxi)[j]
          jci[[j]]$level <- paste(levelj,
                                  levelj,
                                  sep = "-")
        } else {
          jci[[j]]$level <- paste(levels(fxi)[1],
                                  levels(fxi)[2], sep = "-")
        }
        jci[[j]]$data.name <- data.name
        attr(jci[[j]], 'distribution') <- distr
        attr(jci[[j]], 'alternative') <- alternative
      }
    } else {
      if (distr == "asymptotic") {
        jci <- joincount.multi(fx = fxi,
                                      listw = listw,
                                      zero.policy = zero.policy,
                                      spChk = spChk,
                                      adjust.n = adjust.n)
        if (alternative == "greater") {
          pvalue <- pnorm(jci[, c("z-value")],
                          mean = 0, sd = 1,
                          lower.tail = FALSE)
        } else if (alternative == "lower") {
          pvalue <- pnorm(jci[, c("z-value")],
                          mean = 0, sd = 1,
                          lower.tail = TRUE)
        } else {
          pvalue <- 2*pnorm(abs(jci[, c("z-value")]),
                            mean = 0, sd = 1,
                            lower.tail = FALSE)

        }
        jci <- cbind(jci, pvalue)
        } else {
        # Assumption: non-free sampling
        jci_obs <- joincount.multi(fx = fxi,
                                          listw = listw,
                                          zero.policy = zero.policy,
                                          spChk = spChk,
                                          adjust.n = adjust.n)
        joincount_obs <- jci_obs[,c("Joincount")]
        joincount_sim <- matrix(NA,
                                nrow = length(joincount_obs),
                                ncol = nsim)
        row.names(joincount_sim) <- names(joincount_obs)
        for (j in 1:nsim) {
          fx_sim <- sample(fxi, size = length(fxi),
                           replace = FALSE)
          jci_sim <- joincount.multi(fx = fx_sim,
                                   listw = listw,
                                   zero.policy = zero.policy,
                                   spChk = spChk,
                                   adjust.n = adjust.n)
          joincount_sim[, j] <- jci_sim[,c("Joincount")]
        }
        joincount_all <- cbind(joincount_obs,
                               joincount_sim)
        expectedjoincount_all <- apply(joincount_all,
                                       1, mean,
                                       na.rm = TRUE)
        variancejoincount_all <- apply(joincount_all,
                                       1, var,
                                       na.rm = TRUE)
        rankjoincount_all <- apply(joincount_all,
                                  1, rank,
                                  ties.method = "random")
        rankjoincount_obs <- rankjoincount_all[c("joincount_obs"), ]
        if (alternative == "greater") {
          pvalue_obs <- 1 - rankjoincount_obs / (nsim + 1)
        } else if (alternative == "lower") {
          pvalue_obs <- rankjoincount_obs / (nsim + 1)
        } else {
          pvalue_obs <- ifelse(rankjoincount_obs > ((nsim + 1) / 2),
                               1 - rankjoincount_obs / (nsim + 1),
                               rankjoincount_obs / (nsim + 1))
          pvalue_obs <- 2*pvalue_obs
        }
        jci <- cbind(joincount_obs, expectedjoincount_all,
                     variancejoincount_all,
                     as.integer(rankjoincount_obs),
                     pvalue_obs)
        colnames(jci) <- c("Joincount", "Expected",
                           "Variance", "rank_observed", "pvalue")
      }
      attr(jci,'data.name') <- data.name
      attr(jci, 'distribution') <- distr
      attr(jci, 'alternative') <- alternative
      class(jci) <- c("jcmulti", class(jci))
    }
    lres[[i]] <- jci
  }
  class(lres) <- c("spjctest", class(lres))
  return(lres)
}
