#' @name imdo
#' @title Iterative Missing Data Optimization (IMDO)
#' @description Identify optimal combination of variables to minimize 
#'   number of samples with missing data.
#' 
#' @param x data.frame or matrix to optimize.
#' @param groups vector of groups as long as number of rows in \code{x}.
#' @param plot generate a plot of the optimization results.
#' @param opt.smry data.frame of optimization summary results from run of 
#'   \code{imdo} in (\code{$opt.smry} element).
#' @param equal.axes show imdo plot with both axes on same scale?
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
imdo <- function(x, groups = NULL, plot = TRUE) {
  orig.x <- x
  if(is.data.frame(x)) x <- as.matrix(x)
  if(!is.matrix(x)) stop("'x' must be a data.frame or matrix")
  if(is.null(groups)) {
    groups <- rep(1, nrow(x))
  } else if(length(groups) != nrow(x)) {
    stop("length of 'groups' must be same as number of rows in 'x'")
  }
  if(is.null(rownames(x)) & is.null(names(groups))) {
    rownames(x) <- names(groups) <- paste0("id.", zero.pad(1:nrow(x)))
  } else if(is.null(rownames(x))) {
    rownames(x) <- names(groups)
  } else if(is.null(names(groups))) {
    names(groups) <- rownames(x)
  } else if(length(union(rownames(x), names(groups))) != nrow(x)) {
    stop("all rownames in 'x' and names in 'groups' must be the same")
  }
  if(any(is.na(groups))) stop("'groups' cannot have NA values")
  if(!is.factor(groups)) groups <- factor(groups)
  
  iter.smry <- list()
  vars <- colnames(x)
  while(length(vars) > 1) {
    iter.result <- .imdoIteration(x, vars, groups)
    if(is.null(iter.result)) break
    iter.smry <- c(iter.smry, list(iter.result))
    vars <- iter.result$vars
  }
  
  opt.smry <- do.call(rbind, lapply(iter.smry, function(smry) {
    data.frame(n.vars = length(smry$vars), n.ids = length(smry$ids))
  }))
  opt.smry$iter <- 1:nrow(opt.smry)
  
  p1 <- unlist(opt.smry[1, c("n.vars", "n.ids")])
  p2 <- unlist(opt.smry[nrow(opt.smry), c("n.vars", "n.ids")])
  pts <- as.matrix(opt.smry[, c("n.vars", "n.ids")])
  
  opt.smry <- cbind(opt.smry, intersectingPoint(pts, p1, p2))
  rownames(opt.smry) <- NULL
  lower.left <- opt.smry$n.vars < opt.smry$intersect.x &
    opt.smry$n.ids < opt.smry$intersect.y
  opt.smry$distance[lower.left] <- -opt.smry$distance[lower.left]
  opt.smry$distance[c(1, nrow(opt.smry))] <- NA
  opt.smry$D.opt <- opt.smry$distance == max(opt.smry$distance, na.rm = TRUE) 
  D.opt <- which(opt.smry$D.opt)[1]
  
  if(plot) imdoPlot(opt.smry)
  list(
    iter.smry = iter.smry, 
    D.opt = D.opt, 
    opt.smry = opt.smry,
    opt.mat = orig.x[iter.smry[[D.opt]]$ids, iter.smry[[D.opt]]$vars]
  )
}


#' @rdname imdo
#' @export
#' 
imdoPlot <- function(opt.smry, equal.axes = FALSE) {
  p1 <- opt.smry[1, c("n.vars", "n.ids")]
  p2 <- opt.smry[nrow(opt.smry), c("n.vars", "n.ids")]
  p <- ggplot2::ggplot(opt.smry, ggplot2::aes(.data$n.vars, .data$n.ids)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$intersect.x, y = .data$intersect.y, 
        xend = .data$n.vars, yend = .data$n.ids, 
        color = .data$D.opt
      ),
      linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      x = p1$n.vars, y = p1$n.ids, 
      xend = p2$n.vars, yend = p2$n.ids,
      color = "red"
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_label(
      ggplot2::aes(label = .data$iter, color = .data$D.opt), 
      fill = "white"
    ) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
    ggplot2::labs(x = "Number of variables", y = "Number of individuals") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")
  
  if(equal.axes) p <- p + ggplot2::coord_equal()
  invisible(print(p))
}


.imdoIteration <- function(x, vars, groups) {
  if(length(vars) <= 2) return(NULL)
  
  # create matrix of samples without complete measurements
  excluded <- x[, vars, drop = FALSE] 
  excluded <- excluded[!stats::complete.cases(excluded), , drop = FALSE]
  if(nrow(excluded) == 0) return(NULL)
  
  # convert excluded to matrix of logicals for presence of missing data
  miss.mat <- is.na(excluded)
  
  # group variables (columns) by pattern of missingness among individuals (rows)
  var.grp <- paste0(
    "VarGrp.", as.numeric(factor(apply(miss.mat, 2, paste, collapse = "")))
  )
  var.grp.list <- split(colnames(miss.mat), var.grp)
  var.grp.mat <- miss.mat[, !duplicated(var.grp), drop = FALSE]
  colnames(var.grp.mat) <- var.grp[!duplicated(var.grp)]
  
  # group individuals by pattern of missingness among variable groups
  id.grp <- paste0(
    "IDGrp.", as.numeric(factor(apply(var.grp.mat, 1, paste, collapse = "")))
  )
  id.grp.list <- split(rownames(var.grp.mat), id.grp)
  id.var.grp.mat <- var.grp.mat[!duplicated(id.grp), , drop = FALSE]
  rownames(id.var.grp.mat) <- id.grp[!duplicated(id.grp)]
  
  # correlations of all variables
  # var.cor <- if(is.numeric(x)) {
  #   stats::cor(x[, vars, drop = FALSE], use = "pairwise.complete.obs")
  # } else {
  #   matrix(1, nrow = ncol(x), ncol = ncol(x), dimnames = list(vars, vars))
  # }
  # diag(var.cor) <- NA
  
  # compute the average of the maximum correlation coefficient
  # meanMaxCor <- function(i, var.cor) {
  #   mean(apply(var.cor[i, , drop = FALSE], 1, max, na.rm = TRUE))
  # }
  
  # summarize id groups
  id.grp.smry <- sapply(unique(id.grp), function(grp) {
    x.var.grps <- id.var.grp.mat[grp, ]
    x.vars <- unlist(var.grp.list[names(which(x.var.grps))])
    ids <- id.grp.list[[grp]]
    group.freq <- table(groups[ids])
    smry <- as.data.frame(rbind(c(
      num.ids = length(ids),
      num.vars = length(x.vars),
      group.freq,
      ids.per.var = length(ids) / length(x.vars)
      #mean.max.cor = meanMaxCor(x.vars, var.cor)
    )))
    smry$grp <- grp
    smry
  }, simplify = FALSE)
  id.grp.smry <- dplyr::bind_rows(id.grp.smry)
  
  # does an id group have all populations
  freq <- table(groups)
  id.grp.smry$has.all.groups <- sapply(1:nrow(id.grp.smry), function(i) {
    all(id.grp.smry[i, names(freq)] > 0)
  })
  
  # does an id group have any of the smallest populations
  smallest <- names(freq)[freq == min(freq)]
  id.grp.smry$has.smallest.groups <- sapply(1:nrow(id.grp.smry), function(i) {
    any(id.grp.smry[i, smallest] > 0)
  })
  
  # order of priority (most important id groups to include):
  # 1. has individuals from all groups
  # 2. has individuals from smallest sample size groups
  # 3. has largest number of individuals
  # 4. maximum number of individuals per variable excluded
  # 5. variables to be excluded have minimum absolute mean correlation with
  #    variables being used
  id.grp.order <- order(
    id.grp.smry$has.all.groups,
    id.grp.smry$has.smallest.groups,
    id.grp.smry$num.ids,
    id.grp.smry$ids.per.var,
    # id.grp.smry$mean.max.cor,
    decreasing = TRUE
  )
  id.grp.smry <- id.grp.smry[id.grp.order, ]
  
  # choose the id.grp to include this round
  id.grp.to.include <- id.grp.smry$grp[1]
  
  # identify which variable groups will be excluded and update variables to include
  var.grps.to.exclude <- colnames(id.var.grp.mat)[id.var.grp.mat[id.grp.to.include, ]]
  vars.to.include <- setdiff(vars, unlist(var.grp.list[var.grps.to.exclude]))
  
  # create complete matrix for this iteration
  iter.x <- x[, vars.to.include, drop = FALSE]
  iter.x <- iter.x[stats::complete.cases(iter.x), , drop = FALSE]
  
  list(ids = rownames(iter.x), vars = vars.to.include)
}