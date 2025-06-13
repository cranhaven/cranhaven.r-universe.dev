#' Plot diagnostics for a RRglm object
#'
#' Six plots (selectable by \code{which}) are currently available: (1) a plot of estimated population prevalence per RR model,
#' (2) a plot of estimated population prevalence per protection level,
#' (3) a plot of ungrouped residuals against fitted response probability,
#' (4) a plot of grouped (on covariates) residuals against fitted response probability,
#' (5) a plot of grouped Hosmer-Lemeshow residuals against fitted response probability,
#' and (6) a Normal Q-Q plot of grouped (on covariates) residuals. By default, plots 1, 3, 4 and 6 are provided.
#'
#' @param x
#' an object of class RRglm.
#' @param which
#' if a subset of the plots is required, specify a subset of the numbers 1:6 (default: 1, 3, 4, 6).
#' @param type
#' the type of residuals which should be used to be used for plots 3, 4 and 6. The alternatives are: "deviance" (default) and "pearson".
#' @param ngroups
#' the number of groups to compute the Hosmer-Lemeshow residuals for (default: 10).
#' @param ...
#' further arguments passed to or from other methods.
#' @method plot RRglm
#' @export
#' @examples
#' out <- RRglm(response ~ Gender + RR + pp + age, link="RRlink.logit", RRmodel=RRmodel,
#'          p1=RRp1, p2=RRp2, data=Plagiarism, etastart=rep(0.01, nrow(Plagiarism)))
#' plot(out, which = 1:6, type = "deviance", ngroups = 50)
plot.RRglm <- function(x, which = c(1, 3, 4, 6), type = c("deviance", "pearson"), ngroups = 10, ...)
{
  type <- match.arg(type)
  show <- rep(FALSE, 6)
  show[which] <- TRUE

  eta <- x$linear.predictors
  #pi <- exp(eta) / (1 + exp(eta))
  pi <- x$fitted.values

  if (length(levels(as.factor(x$RRmodel))) <= 9) {
    col.set <- RColorBrewer::brewer.pal(9, "Set1")
  }
  else if (length(levels(as.factor(x$RRmodel))) > 9 && length(levels(as.factor(x$RRmodel))) <= 12) {
    col.set <- RColorBrewer::brewer.pal(12, "Set3")
  }
  else {
    col.set <- rep("blue", length(levels(as.factor(x$RRmodel))))
  }

  if(any(show[1], show[2]))
  {
    s <- summary(x)
  }
  if(show[3])
  {
    if (type == "deviance")
      ylab.res <- "Deviance residuals"
    else if (type == "pearson")
      ylab.res <- "Pearson residuals"
  }
  if(any(show[4], show[6]))
  {
    vars <- all.vars(x$formula)
    unique.groups <- getUniqueGroups(x$model[, vars[2:length(vars)]])

    if (type == "deviance")
    {
      grouped.res <- residuals(x, type = "deviance.grouped")
      ylab.res <- "Deviance residuals"
      ylab.grouped.res <- "Deviance residuals (grouped)"
    }
    else if (type == "pearson")
    {
      grouped.res <- residuals(x, type = "pearson.grouped")
      ylab.res <- "Pearson residuals"
      ylab.grouped.res <- "Pearson residuals (grouped)"
    }
  }

  oask <- devAskNewPage(TRUE)
  on.exit(devAskNewPage(oask))

  if(show[1])
  {
    p <- s$prevalence.weighted
    tmp <- list()
    tmp$Prevalence <- data.frame("Prevalence.Weighted" = p$estimate.weighted)
    attr(tmp$Prevalence, "postVar") = array((p$se.weighted^2), dim=c(1,1,length(p$se.weighted)))
    rownames(tmp$Prevalence) <- paste(p$Item, " (", p$RRmodel, ")", sep="")

    dev.hold()
    ignore <- capture.output(print(intDotplot(tmp, mtit = "Prevalence (95%-CI)", cex = 1, scales = list(cex = 1.05))))
    dev.flush()
  }
  if(show[2])
  {
    p <- s$prevalence
    tmp <- list()
    tmp$Prevalence <- data.frame("Prevalence" = p$estimate)
    attr(tmp$Prevalence, "postVar") = array(p$se^2, dim=c(1,1,length(p$se)))
    rownames(tmp$Prevalence) <- paste(p$Item, " (", p$model.level, ")", sep="")

    dev.hold()
    ignore <- capture.output(print(intDotplot(tmp, mtit = "Prevalence (95%-CI)", cex = 1, scales = list(cex = 1.05))))
    dev.flush()
  }
  if(show[3])
  {
    res <- residuals(x, type = type)
    #col <- addalpha(col.set, alpha = 0.4)[as.numeric(as.factor(x$RRmodel))]
    col <- col.set[as.numeric(as.factor(x$RRmodel))]
    col.legend <- col.set[1:length(levels(as.factor(x$RRmodel)))]
    pch <- as.numeric(as.factor(x$RRmodel)) + 14
    pch.legend <- 1:length(levels(as.factor(x$RRmodel))) + 14
    dev.hold()
    print(lattice::xyplot(res ~ pi, main = "Residuals vs. Fitted randomized response probability", ylab = list(label = ylab.res, cex=1.1),
         xlab = list(label = "Fitted randomized response probability", cex=1.1), cex = 1.2, pch = pch, col = col, scales = list(cex = 1.05)
         , key=list(corner=c(1,1), border = TRUE, padding.text = 2.5, points = list(pch=pch.legend, col=col.legend, cex=1.25)
         , text=list(levels(as.factor(x$RRmodel))))
         ))
    dev.flush()
  }
  if(show[4])
  {
    grouped.pi <- getCellMeans(y = pi, factor.groups = unique.groups)
    dev.hold()
    print(lattice::xyplot(grouped.res ~ grouped.pi, main = "Residuals vs. Fitted randomized response probability (grouped)", ylab = list(label = ylab.grouped.res, cex=1.1),
         xlab = list(label = "Fitted randomized response probability", cex=1.1), cex = 1.2, pch = 16, col = col.set[2], scales = list(cex = 1.05)))
    dev.flush()
  }
  if(show[5])
  {
    grouped.res.hl <- residuals(x, type = "hosmer-lemeshow", ngroups = ngroups)
    dev.hold()
    print(lattice::xyplot(grouped.res.hl ~ 1:ngroups, main = "Residuals vs. Fitted randomized response probability (HL)", ylab = list(label = "Hosmer-Lemeshow residuals", cex=1.1),
         xlab = list(label = "Groups by fitted randomized response probability", cex=1.1), cex = 1.2, pch = 16, col = col.set[3], scales = list(cex = 1.05)))
    dev.flush()
  }
  if(show[6])
  {
    dev.hold()
    print(lattice::qqmath( ~ grouped.res, main = "Normal Q-Q plot of grouped (on covariates) residuals", xlab = list(label = "Theoretical Quantiles", cex=1.1), ylab = list(label = ylab.grouped.res, cex=1.1), scales = list(cex = 1.05)
                           , prepanel = lattice::prepanel.qqmathline
                           , panel = function(x, y) {
                             lattice::panel.qqmathline(x, distribution = qnorm, lwd = 1)
                             lattice::panel.qqmath(x, cex = 1.2, pch = 16, col = col.set[2])
                           }))
    dev.flush()
  }

  invisible()
}

#' Plot diagnostics for a RRglmerMod object
#'
#' Five plots (selectable by \code{which}) are currently available: (1) a plot of estimated population prevalence per RR model,
#' (2) a plot of estimated population prevalence per protection level,
#' (3) a plot of random effects and their conditional variance (95%-cI),
#' (4) a plot of conditional pearson residuals against fitted randomized response probability,
#' and (5) a plot of unconditional pearson residuals against fitted randomized response probability.
#' By default, plots 1, 3, 4 and 5 are provided.
#'
#' @param x
#' an object of class RRglmerMod.
#' @param which
#' if a subset of the plots is required, specify a subset of the numbers 1:5 (default: 1, 3, 4, 5).
#' @param ...
#' further arguments passed to or from other methods.
#' @method plot RRglmerMod
#' @export
#' @examples
#' out <- RRglmer(response ~ Gender + RR + pp + (1+pp|age), link="RRlink.logit", RRmodel=RRmodel,
#'          p1=RRp1, p2=RRp2, data=Plagiarism, na.action = "na.omit",
#'          etastart = rep(0.01, nrow(Plagiarism)),
#'          control = glmerControl(optimizer = "Nelder_Mead", tolPwrss = 1e-03), nAGQ = 1)
#' plot(out, which = 1:5)
plot.RRglmerMod <- function(x, which = c(1, 3, 4, 5), ...)
{
  #3: random effects
  #4: conditional pearson
  #5: unconditional pearson

  show <- rep(FALSE, 3)
  show[which] <- TRUE

  # eta <- predict(x, type = "link")
  # pi <- exp(eta) / (1 + exp(eta))
  pi <- fitted(x)

  if(any(show[1], show[2]))
  {
    s <- summary(x)
  }

  if (length(levels(as.factor(x@RRparam$RRmodel))) <= 9) {
    col.set <- RColorBrewer::brewer.pal(9, "Set1")
  }
  else if (length(levels(as.factor(x@RRparam$RRmodel))) > 9 && length(levels(as.factor(x@RRparam$RRmodel))) <= 12) {
    col.set <- RColorBrewer::brewer.pal(12, "Set3")
  }
  else {
    col.set <- rep("blue", length(levels(as.factor(x@RRparam$RRmodel))))
  }


  oask <- devAskNewPage(TRUE)
  on.exit(devAskNewPage(oask))
  if(show[1])
  {
    p <- s$prevalence.weighted
    tmp <- list()
    tmp$Prevalence <- data.frame("Prevalence.Weighted" = p$estimate.weighted)
    attr(tmp$Prevalence, "postVar") = array((p$se.weighted^2), dim=c(1,1,length(p$se.weighted)))
    rownames(tmp$Prevalence) <- paste(p$Item, " (", p$RRmodel, ")", sep="")

    dev.hold()
    ignore <- capture.output(print(intDotplot(tmp, mtit = "Prevalence (95%-CI)", cex = 1, scales = list(cex = 1.05))))
    dev.flush()
  }
  if(show[2])
  {
    p <- s$prevalence
    tmp <- list()
    tmp$Prevalence <- data.frame("Prevalence" = p$estimate)
    attr(tmp$Prevalence, "postVar") = array(p$se^2, dim=c(1,1,length(p$se)))
    rownames(tmp$Prevalence) <- paste(p$Item, " (", p$model.level, ")", sep="")

    dev.hold()
    ignore <- capture.output(print(intDotplot(tmp, mtit = "Prevalence (95%-CI)", cex = 1, scales = list(cex = 1.05))))
    dev.flush()
  }
  if(show[3])
  {
    random.effects <- ranef(x, condVar=TRUE)
    dev.hold()
    ignore <- capture.output(print(intDotplot(random.effects, mtit = "Random effects (95%-CI)", cex = 1, scales = list(cex = 1.05))))
    dev.flush()
  }
  if(show[4])
  {
    res.conditional <- residuals(x, type = "pearson")
    ylab <- "Conditional pearson residuals"
    xlab <- "Fitted randomized response probability"

    col <- col.set[as.numeric(as.factor(x@RRparam$RRmodel))]
    col.legend <- col.set[1:length(levels(as.factor(x@RRparam$RRmodel)))]
    pch <- as.numeric(as.factor(x@RRparam$RRmodel)) + 14
    pch.legend <- 1:length(levels(as.factor(x@RRparam$RRmodel))) + 14
    dev.hold()
    print(lattice::xyplot(res.conditional ~ pi, main = "Residuals vs. Fitted randomized response probability", ylab = list(label = ylab, cex=1.1),
                          xlab = list(label =xlab, cex=1.1), cex = 1.2, pch = pch, col = col, scales = list(cex = 1.05)
                          , key=list(corner=c(1,1), border = TRUE, padding.text = 2.5, points = list(pch=pch.legend, col=col.legend, cex=1.25)
                                     , text=list(levels(as.factor(x@RRparam$RRmodel))))
    ))
    dev.flush()
  }
  if(show[5])
  {
    res.unconditional <- residuals(x, type = "unconditional.pearson")
    ylab <- "Unconditional pearson residuals"
    xlab <- "Fitted randomized response probability"

    col <- col.set[as.numeric(as.factor(x@RRparam$RRmodel))]
    col.legend <- col.set[1:length(levels(as.factor(x@RRparam$RRmodel)))]
    pch <- as.numeric(as.factor(x@RRparam$RRmodel)) + 14
    pch.legend <- 1:length(levels(as.factor(x@RRparam$RRmodel))) + 14
    dev.hold()
    print(lattice::xyplot(res.unconditional ~ pi, main = "Residuals vs. Fitted randomized response probability", ylab = list(label = ylab, cex=1.1),
                          xlab = list(label =xlab, cex=1.1), cex = 1.2, pch = pch, col = col, scales = list(cex = 1.05)
                          , key=list(corner=c(1,1), border = TRUE, padding.text = 2.5, points = list(pch=pch.legend, col=col.legend, cex=1.25)
                                     , text=list(levels(as.factor(x@RRparam$RRmodel))))
    ))
    dev.flush()
  }

  invisible()
}

#' Internal function to draw a dotplot
#'
#' Sligthly modified function dotplot.ranef.mer from lme4 package.
#'
#' @param x
#' data.
#' @param mtit
#' main title.
#' @keywords internal
#' @export
intDotplot <- function (x, mtit = "95%-CI", ...)
{
  prepanel.ci <- function(x, y, se, subscripts, ...) {
    if (is.null(se))
      return(list())
    x <- as.numeric(x)
    hw <- 1.96 * as.numeric(se[subscripts])
    list(xlim = range(x - hw, x + hw, finite = TRUE))
  }
  panel.ci <- function(x, y, se, subscripts, pch = 16, horizontal = TRUE,
                       col = dot.symbol$col, lty = dot.line$lty, lwd = dot.line$lwd,
                       col.line = dot.line$col, levels.fos = unique(y), groups = NULL,
                       ...) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    dot.line <- lattice::trellis.par.get("dot.line")
    dot.symbol <- lattice::trellis.par.get("dot.symbol")
    sup.symbol <- lattice::trellis.par.get("superpose.symbol")
    lattice::panel.abline(h = levels.fos, col = col.line, lty = lty,
                 lwd = lwd)
    lattice::panel.abline(v = 0, col = col.line, lty = lty, lwd = lwd)
    if (!is.null(se)) {
      se <- as.numeric(se[subscripts])
      lattice::panel.segments(x - 1.96 * se, y, x + 1.96 * se, y,
                     col = "black")
    }
    lattice::panel.xyplot(x, y, pch = pch, ...)
  }
  f <- function(nx, ...) {
    xt <- x[[nx]]
    ss <- stack(xt)
    ss$ind <- factor(as.character(ss$ind), levels = colnames(xt))
    ss$.nn <- factor(rownames(xt))# rep.int(reorder(factor(rownames(xt)), xt[[1]],
                              #FUN = mean, sort = sort), ncol(xt))
    se <- NULL
    if (!is.null(pv <- attr(xt, "postVar")))
      se <- unlist(lapply(1:(dim(pv)[1]), function(i) sqrt(pv[i,
                                                              i, ])))
    lattice::dotplot(.nn ~ values | ind, ss, se = se, prepanel = prepanel.ci,
            panel = panel.ci, xlab = NULL, main = mtit, ...)
  }
  setNames(lapply(names(x), f, ...), names(x))
}


