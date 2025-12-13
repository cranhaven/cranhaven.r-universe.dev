#' Plot for a given logistic regression model (glm with binomial family)
#' with one binary covariate.
#'
#' Can be used on its own but is also useable as plotfun in
#' \code{\link{node_pmterminal}}.
#'
#' @param mod A model of class glm with binomial family.
#' @param data optional data frame. If NULL the data stored in mod is used.
#' @param plot_data should the data in form of a mosaic type plot be plotted?
#' @param theme A ggplot2 theme.
#' @param ... ignored at the moment.
#'
#' @examples
#' set.seed(2017)
#'
#' # number of observations
#' n <- 1000
#'
#' # balanced binary treatment
#' # trt <- factor(rep(c("C", "A"), each = n/2),
#' #               levels = c("C", "A"))
#'
#' # unbalanced binary treatment
#' trt <- factor(c(rep("C", n/4), rep("A", 3*n/4)),
#'               levels = c("C", "A"))
#'
#' # some continuous variables
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#'
#' # linear predictor
#' lp <- -0.5 + 0.5*I(trt == "A") + 1*I(trt == "A")*I(x1 > 0)
#'
#' # compute probability with inverse logit function
#' invlogit <- function(x) 1/(1 + exp(-x))
#' pr <- invlogit(lp)
#'
#' # bernoulli response variable
#' y <- rbinom(n, 1, pr)
#' dat <- data.frame(y, trt, x1, x2)
#'
#' # logistic regression model
#' mod <- glm(y ~ trt, data = dat, family = "binomial")
#' binomial_glm_plot(mod, plot_data = TRUE)
#'
#' # logistic regression model tree
#' ltr <- pmtree(mod)
#' plot(ltr, terminal_panel = node_pmterminal(ltr,
#'                                            plotfun = binomial_glm_plot,
#'                                            confint = TRUE,
#'                                            plot_data = TRUE))
#'
#' @importFrom ggplot2 ggplot geom_point geom_errorbar geom_bar theme_classic aes_string aes ylim ylab theme
#' @export
binomial_glm_plot <- function(mod, data = NULL, plot_data = FALSE, theme = theme_classic(), ...) {

  if(!inherits(mod, "glm")) stop("model should be of class glm, is of class ", class(mod))
  fam <- stats::family(mod)$family
  if(fam != "binomial") stop("model should have family binomail, but has family ", fam)

  ## check if model has on factor covariate
  if(!one_factor(mod))
    stop("Plotting currently only works for models with a single factor covariate. 
        We recommend using partykit or ggparty plotting functionalities!")

  ## get formula and data
  modcall <- getCall(mod)
  modformula <- as.Formula(eval(modcall$formula))
  xformula <- formula(modformula, lhs = 0, rhs = 1)
  yformula <- formula(modformula, lhs = 1, rhs = 0)
  if(is.null(data)) data <- eval(modcall$data)
  xdat <- get_all_vars(xformula, data = data)
  uxdat <- unique(xdat)
  ydat <- get_all_vars(yformula, data = data)
  ynam <- names(ydat)

  ## get plotting data
  prd <- predict(mod, type = "link", se.fit = TRUE, newdata = uxdat)
  q <- stats::qnorm(0.975)

  probs <- predict(mod, type = "response", newdata = uxdat)
  linv <- mod$family$linkinv
  pdat <- data.frame(py = linv(prd$fit),
    lwr = linv(prd$fit - q * prd$se.fit),
    upr = linv(prd$fit + q * prd$se.fit),
    uxdat)

  ## points plot info
  pts <- geom_point(data = pdat, aes_string(x = colnames(xdat), y = "py"))
  ci <- geom_errorbar(data = pdat, width = 0.1,
    aes_string(x = colnames(xdat),
      ymin = "lwr", ymax = "upr"))

  if(plot_data) {
    ## get mosaic plot data
    x <- xdat[[1]]
    y <- ydat[[1]]
    jointTable <- prop.table(table(x, y))
    plotData <- as.data.frame(jointTable)
    plotData$marginVar1 <- prop.table(table(x))
    plotData$var2Height <- plotData$Freq / plotData$marginVar1

    ## add mosaic plot info
    p <- ggplot() + geom_bar(data = plotData,
      aes_string("x", "var2Height", fill = "x",
        alpha = "y", width = "marginVar1"),
      stat = "identity") + pts + ci
  } else {
    p <- ggplot() + ci + pts
  }
  p + ylim(0, 1) + ylab(paste("P(", ynam, ")")) + theme(legend.position = "none")
}


#' Density plot for a given lm model
#' with one binary covariate.
#'
#' Can be used on its own but is also useable as plotfun in
#' \code{\link{node_pmterminal}}.
#'
#' @param mod A model of class lm.
#' @param data optional data frame. If NULL the data stored in mod is used.
#' @param densest should additional to the model density kernel density estimates
#'  (see \code{\link[ggplot2]{geom_density}}) be computed?
#' @param theme A ggplot2 theme.
#' @param yrange Range of the y variable to be used for plotting.
#'  If NULL the range in the data will be used.
#'
#' @details In case of an offset, the value of the offset variable will be set to
#' the median of the values in the data.
#'
#' @examples
#' ## example taken from ?lm
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' data <- data.frame(weight, group)
#' lm.D9 <- lm(weight ~ group, data = data)
#' lm_plot(lm.D9)
#'
#' ## example taken from ?glm (modified version)
#' data(anorexia, package = "MASS")
#' anorexia$treatment <- factor(anorexia$Treat != "Cont")
#' anorex.1 <- glm(Postwt ~ treatment + offset(Prewt),
#'                 family = gaussian, data = anorexia)
#' lm_plot(anorex.1)
#'
#' @importFrom ggplot2 ggplot geom_line theme_classic aes_string xlim xlab scale_linetype_discrete
#' @export
lm_plot <- function(mod, data = NULL, densest = FALSE, theme = theme_classic(),
  yrange = NULL) {

  cl <- class(mod)
  if(!inherits(mod, "lm") & length(cl) != 1) stop("model should be of class lm, is of class ", cl)

  ## check if model has on factor covariate
  if(!one_factor(mod))
      stop("Plotting currently only works for models with a single factor covariate. 
        We recommend using partykit or ggparty plotting functionalities!")

  ## get formula and data
  modcall <- getCall(mod)
  modformula <- as.Formula(eval(modcall$formula))
  xformula <- formula(modformula, lhs = 0, rhs = 1)
  offset_id <- attr(terms(xformula), "offset")
  yformula <- formula(modformula, lhs = 1, rhs = 0)
  if(is.null(data)) data <- eval(modcall$data)
  all_x <- get_all_vars(xformula, data = data)
  if(!is.null(offset_id)) {
    # stop("don't know how to handle offsets yet")
    all_x[, offset_id] <- stats::median(all_x[, offset_id])
  }
  xdat <- unique(all_x)
  trtnam <- names(xdat)
  if(!is.null(offset_id)) trtnam <- trtnam[-offset_id]
  ydat <- get_all_vars(yformula, data = data)
  ynam <- names(ydat) # as.character(yformula[[2]])
  if(is.null(yrange)) yrange <- range(ydat)

  ## get density functions for each treatment group
  k <- 100
  means <- cbind(mean = predict(mod, newdata = xdat, type = "response"),
    xdat)
  sigma <- sigma(mod)
  ygrid <- seq(from = yrange[1], to = yrange[2], length.out = k)
  rows <- rep(seq_len(NROW(means)), each = k)

  dens <- cbind(ygrid, means[rows, ],
    density = dnorm(ygrid, mean = means$mean[rows], sd = sigma))

  if(densest) {
    des <- factor(c("model", "kernel"), levels = c("model", "kernel"))
    p <- ggplot() +
      geom_line(data = cbind(dens, estimate = des[1]),
        aes_string(x = "ygrid", y = "density", color = trtnam, linetype = "estimate")) +
      geom_line(data = cbind(data, estimate = des[2]),
        aes_string(x = ynam, color = trtnam, linetype = "estimate"),
        stat = "density") +
      scale_linetype_discrete(drop = FALSE)
  } else {
    p <- ggplot() +
      geom_line(data = dens,
        aes_string(x = "ygrid", y = "density", color = trtnam))
  }
  p + theme + xlim(yrange) + xlab(ynam)
}


#' Survival plot for a given survreg model
#' with one binary covariate.
#'
#' Can be used on its own but is also useable as plotfun in
#' \code{\link{node_pmterminal}}.
#'
#' @param mod A model of class survreg.
#' @param data optional data frame. If NULL the data stored in mod is used.
#' @param theme A ggplot2 theme.
#' @param yrange Range of the y variable to be used for plotting.
#'  If NULL it will be 0 to max(y).
#'
#' @examples
#' if(require("survival")) {
#'   survreg_plot(survreg(Surv(futime, fustat) ~ factor(rx), ovarian))
#' }
#'
#' @importFrom ggplot2 ggplot geom_line theme_classic aes_string coord_cartesian
#' @importFrom stats model.frame predict formula terms get_all_vars
#' @importFrom survival Surv survreg
#' @importFrom Formula as.Formula
#' @export
survreg_plot <- function(mod, data = NULL, theme = theme_classic(),
  yrange = NULL) {
  if(!inherits(mod, "survreg")) stop("model should be of class survreg, but is of class ", class(mod))

  ## check if model has on factor covariate
  if(!one_factor(mod))
    stop("Plotting currently only works for models with a single factor covariate. 
        We recommend using partykit or ggparty plotting functionalities!")

  ## get formula and data
  modcall <- getCall(mod)
  modformula <- as.Formula(eval(modcall$formula))
  xformula <- formula(modformula, lhs = 0, rhs = 1)
  yformula <- formula(modformula, lhs = 1, rhs = 0)
  if(is.null(data)) data <- eval(modcall$data)
  xdat <- unique(get_all_vars(xformula, data = data))
  if(is.null(yrange)) {
    ymax <- max(model.frame(yformula, data = data))
    yrange <- c(0, ymax)
  }

  ## get survivor functions for each treatment group
  p <- seq(.01, .99, by=.02)
  pr_raw <- predict(mod, newdata = xdat,
    type = "quantile", p = p)
  pr <- do.call("rbind",
    lapply(1:NROW(xdat),
      function(i) data.frame(xdat[i, , drop = FALSE],
        pr = pr_raw[i, ], probability = rev(p),
        row.names = NULL)))

  ## plot
  xnam <- attr(terms(xformula), "term.labels")
  ggplot(data = pr, aes_string(x = "pr", y = "probability", group = xnam,
    color = xnam)) +
    geom_line() + coord_cartesian(xlim = yrange) +
    xlab(as.character(yformula[[2]])[2]) +
    theme
}


#' Survival plot for a given coxph model
#' with one binary covariate.
#'
#' Can be used on its own but is also useable as plotfun in
#' \code{\link{node_pmterminal}}.
#'
#' @param mod A model of class coxph.
#' @param data optional data frame. If NULL the data stored in mod is used.
#' @param theme A ggplot2 theme.
#' @param yrange Range of the y variable to be used for plotting.
#'  If NULL it will be 0 to max(y).
#'
#' @examples
#' if(require("survival")) {
#'   coxph_plot(coxph(Surv(futime, fustat) ~ factor(rx), ovarian))
#' }
#'
#' @importFrom ggplot2 ggplot geom_step theme_classic aes_string coord_cartesian
#' @importFrom stats model.frame predict formula terms get_all_vars
#' @importFrom survival Surv survreg
#' @importFrom Formula as.Formula
#' @export
coxph_plot <- function(mod, data = NULL, theme = theme_classic(),
  yrange = NULL) {
  cl <- class(mod)
  if(!("coxph" %in% cl)) stop("model should be of class coxph, but is of class ", cl)

  ## check if model has on factor covariate
  if(!one_factor(mod))
      stop("Plotting currently only works for models with a single factor covariate. 
        We recommend using partykit or ggparty plotting functionalities!")

  ## get formula and data
  modcall <- getCall(mod)
  modformula <- as.Formula(eval(modcall$formula))
  xformula <- formula(modformula, lhs = 0, rhs = 1)
  yformula <- formula(modformula, lhs = 1, rhs = 0)
  if(is.null(data)) data <- eval(modcall$data)
  xdat <- unique(get_all_vars(xformula, data = data))
  if(is.null(yrange)) {
    ymax <- max(model.frame(yformula, data = data))
    yrange <- c(0, ymax)
  }

  ## get survivor functions for each treatment group
  p <- seq(.01, .99, by=.02)
  s_raw <- survival::survfit(mod, newdata = xdat)
  pr <- do.call("rbind",
    lapply(1:NROW(xdat),
      function(i) data.frame(xdat[i, , drop = FALSE],
        pr = s_raw$time,
        probability = s_raw$surv[ , i],
        row.names = NULL)))
  pr <- rbind(pr,
    data.frame(xdat, pr = rep(0, NROW(xdat)), probability = rep(1, NROW(xdat))))
  pr[[1]] <- as.factor(pr[[1]])

  ## plot
  xnam <- names(xdat)
  ggplot(data = pr, aes_string(x = "pr", y = "probability", group = xnam,
    color = xnam)) +
    geom_step() + coord_cartesian(xlim = yrange, ylim = 0:1) +
    xlab(as.character(yformula[[2]])[2]) +
    theme
}



#' Panel-Generator for Visualization of pmtrees
#'
#' The plot method for party and constparty objects are rather flexible and can
#' be extended by panel functions. The pre-defined panel-generating function of
#' class grapcon_generator for pmtrees is documented here.
#'
#' @param obj an object of class party.
#' @param coeftable logical or function. If logical: should a table with
#' coefficients be added to the plot (TRUE/FALSE)? If function: A function
#' comparable to \code{\link{coeftable.survreg}}.
#' @param digits integer, used for formating numbers.
#' @param confint Should a confidence interval be computed.
#' @param plotfun Plotting function to be used. Needs to be of format
#' \code{function(mod, data)} where \code{mod} is the model object.
#' See examples for more details.
#' @param nid function to retrieve info on what is plottet as node ids.
#' @param ... arguments passed on to plotfun.
#'
#' @examples
#' if(require("survival")) {
#' ## compute survreg model
#' mod_surv <- survreg(Surv(futime, fustat) ~ factor(rx), ovarian,
#'   dist = 'weibull')
#' survreg_plot(mod_surv)
#'
#' ## partition model and plot
#' tr_surv <- pmtree(mod_surv)
#' plot(tr_surv, terminal_panel = node_pmterminal(tr_surv, plotfun = survreg_plot,
#'                                                confint = TRUE))
#' }
#'
#' if(require("survival") & require("TH.data")) {
#'   ## Load data
#'   data(GBSG2, package = "TH.data")
#'
#'   ## Weibull model
#'   bmod <- survreg(Surv(time, cens) ~ horTh, data = GBSG2, model = TRUE)
#'
#'   ## Coefficient table
#'   grid.newpage()
#'   coeftable.survreg(bmod)
#'
#'   ## partitioned model
#'   tr <- pmtree(bmod)
#'
#'   ## plot with specific coeftable
#'   plot(tr, terminal_panel = node_pmterminal(tr, plotfun = survreg_plot,
#'     confint = TRUE, coeftable = coeftable.survreg))
#' }
#'
#' @export
#'
#' @importFrom grid viewport grid.layout unit popViewport
#' pushViewport grid.rect grid.draw gpar grid.text upViewport
#' @importFrom gridExtra tableGrob ttheme_minimal
#' @importFrom partykit id_node
node_pmterminal <- function(obj, coeftable = TRUE, digits = 2, confint = TRUE, plotfun,
  nid = function(node) paste0(nam[id_node(node)], ", n = ", node$info$nobs),
  ...)
{

  dots <- list(...)
  nam <- names(obj)
  mod <- obj$info$model
  wterminals <- predict(obj, type = "node")
  dat <- obj$data

  if(! "yrange" %in% names(dots)) {
    if("glm" %in% class(mod) && stats::family(mod)$family == "binomial") {
      dots$yrange <- c(0, 1)
    } else {
      modcall <- getCall(mod)
      modformula <- as.Formula(eval(modcall$formula))
      yformula <- formula(modformula, lhs = 1, rhs = 0)
      ydat <- get_all_vars(yformula, data = dat)
      dots$yrange <- range(ydat)
    }
  }

  ### panel function for the inner nodes
  rval <- function(node, .nid = nid) {

    nid <- .nid(node)

    ## model
    nmod <- update(mod, data = dat, subset = (wterminals == id_node(node)))

    top_vp <- viewport(layout = grid.layout(nrow = 2,
      heights = unit(c(0.1, 1), "null")))
    pushViewport(top_vp)

    ## table of coefficients
    if(is.logical(coeftable)) {

      ## if table should be printed
      if(coeftable) {
        coefs <- as.matrix(node$info$coefficients)
        if(confint) {
          ci <- confint(nmod)
          coefs <- cbind(coefs, ci)
        }
        cf <- format(round(coefs, digits), nsmall = digits)
        colnams <- colnames(cf)
        colnams[1] <- "theta"
        cftab <- tableGrob(cf, cols = colnams,
          theme = ttheme_minimal(colhead = list(fg_params = list(parse=TRUE))))
        tabwid <- sum(cftab$widths)

        ## viewport enclosing all
        node_vp <- viewport(
          layout.pos.row = 2,
          layout = grid.layout(2, 1),
          width = max(tabwid, unit(0.95, "npc")),
          height = unit(0.95, "npc"),
          y = unit(0.25, "npc"),
          just = "bottom"
        )
        pushViewport(node_vp)

        grid.rect(gp = gpar(fill = "white"))

        ## table (and viewport table)
        tablevp <- viewport(layout.pos.row = 1, layout.pos.col = 1)
        pushViewport(tablevp)
        grid.draw(cftab)
        popViewport()

        ## viewport plot
        plotvp <- viewport(layout.pos.row = 2, layout.pos.col = 1,
          width = unit(0.95, "npc"),
          height = unit(0.95, "npc"))
        pushViewport(plotvp)

      } else {
        ### no table should be printed
        ## viewport enclosing all
        node_vp <- viewport(
          layout.pos.row = 2,
          width = unit(0.95, "npc"),
          height = unit(0.95, "npc")
        )
        pushViewport(node_vp)

        grid.rect(gp = gpar(fill = "white"))

        ## viewport plot
        plotvp <- viewport(
          width = unit(0.95, "npc"),
          height = unit(0.95, "npc")
        )
        pushViewport(plotvp)
      }
    } else {

      ## if coeftable is a function
      if(!is.function(coeftable))
        stop("coeftable should either be logical or a function.")

      coeftable(model = nmod, confint = confint, digits = digits,
        intree = TRUE)

      ## viewport plot
      plotvp <- viewport(layout.pos.row = 2, layout.pos.col = 1,
        width = unit(0.95, "npc"),
        height = unit(0.95, "npc"))
      pushViewport(plotvp)
    }

    ## plot
    pl <- do.call("plotfun",
      args = c(list(mod = nmod,
        data = subset(dat, (wterminals == id_node(node)))),
        dots))
    print(pl, vp = plotvp)
    popViewport()


    nodeIDvp <- viewport(x = unit(0.5, "npc"), y = unit(1, "npc"),
      width = max(unit(1, "lines"), unit(1.3, "strwidth", nid)),
      height = max(unit(1, "lines"), unit(1.3, "strheight", nid)))
    pushViewport(nodeIDvp)
    grid.rect(gp = gpar(fill = "white"))
    grid.text(nid)
    popViewport()

    upViewport(n = 2)
  }

  return(rval)
}

class(node_pmterminal) <- "grapcon_generator"


#' @title Table of coefficients for survreg model
#' @description This function is mostly useful for plotting a pmtree.
#' The generic plotting does not show the estimate and confidence interval
#' of the scale parameter. This one does.
#' @param model model of class \code{\link[survival]{survreg}}
#' @param confint should a confidence interval be computed? Default: TRUE
#' @param digits integer, used for formating numbers. Default: 2
#' @param intree is the table plotted within a tree? Default: FALSE
#' @return None.
#' @examples
#' if(require("survival") & require("TH.data")) {
#'   ## Load data
#'   data(GBSG2, package = "TH.data")
#'
#'   ## Weibull model
#'   bmod <- survreg(Surv(time, cens) ~ horTh, data = GBSG2, model = TRUE)
#'
#'   ## Coefficient table
#'   grid.newpage()
#'   coeftable.survreg(bmod)
#'
#'   ## partitioned model
#'   tr <- pmtree(bmod)
#'
#'   ## plot
#'   plot(tr, terminal_panel = node_pmterminal(tr, plotfun = survreg_plot,
#'     confint = TRUE, coeftable = coeftable.survreg))
#' }
#'
#' @export
coeftable.survreg <- function(model, confint = TRUE, digits = 2, intree = FALSE) {

  coefs <- c(model$coefficients, "Log(scale)" = log(model$scale))

  if(confint) {
    ci <- cbind(
      "2.5 %" = coefs - stats::qnorm(0.975) * sqrt(diag(model$var)),
      "97.5 %" = coefs + stats::qnorm(0.975) * sqrt(diag(model$var))
    )
    coefs <- cbind(coefs, ci)
  }
  cf <- format(round(coefs, digits), nsmall = digits)
  colnams <- colnames(cf)
  colnams[1] <- "theta"
  cftab <- gridExtra::tableGrob(cf, cols = colnams,
    theme = ttheme_minimal(colhead = list(fg_params = list(parse=TRUE))))
  tabwid <- sum(cftab$widths)

  if(intree) {
    ## viewport enclosing table and plot
    ## needed only if table is plotted within tree
    node_vp <- viewport(
      layout.pos.row = 2,
      layout = grid.layout(2, 1),
      width = max(tabwid, unit(0.95, "npc")),
      height = unit(0.95, "npc"),
      y = unit(0.25, "npc"),
      just = "bottom"
    )
    pushViewport(node_vp)

    grid.rect(gp = gpar(fill = "white"))
  }

  ## table (and viewport table)
  tablevp <- viewport(layout.pos.row = 1, layout.pos.col = 1)
  pushViewport(tablevp)
  grid.draw(cftab)
  popViewport()
}
