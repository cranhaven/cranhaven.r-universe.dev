#' @title LDA_prepare_plot
#'
#' @description analyze limiting dilution assay (LDA) data and collect
#'      information for plotting.
#'
#' @param LDA_tab LDA data.frame
#'      ("cells", "wells", "positive", "group", "replicate")
#' @param uncertainty method for approximation of uncertainties of survival
#'      fractions (SF): activity based ("act") or by error propagation ("ep")
#'
#' @return none
#'
#' @examples
#' x <- data.frame("cells" = rep(c(10,50,100,250),times = 4),
#'                 "wells" = rep(25,16),
#'                 "positive" = c(2,5,10,20,1,2,6,11,3,4,8,22,1,1,7,12),
#'                 "group" = rep(c(rep("A",4),rep("B",4)),times = 2),
#'                 "replicate" = c(rep(1,8),rep(2,8)))
#' LDA_prepare_plot(x)
#' # data(LDAdata)
#' # Z1 <- subset.data.frame(LDAdata,subset = name == unique(LDAdata$name)[1])
#' # LDA_prepare_plot(Z1[,c("S-value","# Tested","# Clonal growth","Group",
#' #                      "replicate")])
#' @importFrom grDevices "colorRampPalette"
#' @importFrom stats "aggregate.data.frame"
#' @export
#'
LDA_prepare_plot <- function(LDA_tab,
                             uncertainty = "act"){
  if (class(LDA_tab)[1] != "data.frame"){
    stop("error: input must be of class data.frame")
  }
  if (!(uncertainty %in% c("ep","act"))){
    stop("error: uncertainty must be either
         'ep' (error propagation) or
         'act' (activity CIs)")
  }
  cnames <- c("cells","wells","positive","group","replicate")
  colnames(LDA_tab) <- cnames[seq_along(colnames(LDA_tab))]
  if (ncol(LDA_tab) == 3){
    LDA_tab$group <- 0
  }

  # setting options
  alpha <- 0.05

  # get data per treatment and replicate
  grps <- unique(LDA_tab$group)
  N_treat <- length(grps)

  out_act <- vector(mode = "list",
                length = N_treat)
  out_SF <- NULL

  colhex <- colorRampPalette(c("#43E08700", "#00612A00"))(N_treat) # D23264
  colors <- col2rgb(colhex) / 255

  InfPos <- NULL
  for (gi in seq_along(grps)){ #for each treatment
    this_trtmt <- list("Treatment" = grps[gi],
                       "color" = colhex[gi],
                       "rep.data" = NULL,
                       "mean.data" = NULL,
                       "model" = list("fit" = NULL,
                                      "line" = NULL,
                                      "unc.type" = NULL,
                                      "unc.band" = NULL))
    x_sp <- NULL
    d_g <- subset.data.frame(x = LDA_tab,
                             subset = LDA_tab$group == grps[gi])
    rplcts <- unique(d_g$replicate)
    for (ri in rplcts){
      ddd <- subset.data.frame(x = d_g,
                               subset = replicate == ri)
      d_a <- LDA_activity_single(x = ddd[,1:3])
      d <- d_a$model$data
      d$x <- exp(d_a$model$data$x)
      p <- aggregate.data.frame(x = d$y,
                                by = list(d$x),
                                FUN = mean)
      n <- aggregate.data.frame(x = !is.na(d$y),
                                by = list(d$x),
                                FUN = sum)
      x <- merge(p,n,by = "Group.1")
      colnames(x) <- c("x","y","n")
      x$ind <- x$y > (1 - 1e-14)
      x$pch <- c(3,6)[(x$ind)+1] # '+' for each replicate
      x$y[x$ind] <- ((x$y*(x$n*length(rplcts))-0.5)/(x$n*length(rplcts)))[x$ind]
      if (sum(x$ind)>0){ # find y-position for '-Inf'-indicator in plot
        InfPos <- min(InfPos,min(x$y[x$ind]))
      }
      #x$col <- colhex[gi]
      x_sp <- rbind(x_sp,x)
      x <- NULL
      rm(ddd,d_a,d,p,n)
    }
    this_trtmt$rep.data <- x_sp
    x_sp <- NULL
    d_a <- LDA_activity_single(x = d_g[,1:3])
    d <- d_a$model$data
    d$x <- exp(d_a$model$data$x)
    p <- aggregate.data.frame(x = d$y,
                              by = list(d$x),
                              FUN = mean)
    n <- aggregate.data.frame(x = !is.na(d$y),
                              by = list(d$x),
                              FUN = sum)
    x <- merge(p,n,by = "Group.1")
    colnames(x) <- c("x","y","n")
    x$ind <- x$y > (1 - 1e-14)
    x$pch <- c(19,6)[(x$ind)+1] # mean-symbol
    x$y[x$ind] <- ((x$y*x$n-0.5)/x$n)[x$ind]
    if (sum(x$ind)>0){ # find y-position for '-Inf'-indicator in plot
      InfPos <- min(InfPos,min(x$y[x$ind]))
    }
    this_trtmt$mean.data <- x
    x <- NULL
    out_act[[gi]] <- this_trtmt
    rm(d_a,d,p,n)
  }
  rm(x_sp,ri,gi)

  # set y-position for '-Inf'-indicator (no negative wells)
  for (gi in seq_along(grps)){
    out_act[[gi]]$rep.data$y[out_act[[gi]]$rep.data$ind] <- InfPos
    out_act[[gi]]$mean.data$y[out_act[[gi]]$mean.data$ind] <- InfPos
  }
  rm(InfPos)

  # new data - x-axis?
  xmax <- max(LDA_tab$cells)
  xmin <- min(LDA_tab$cells)
  new.data <- data.frame(
    "x" = seq(log(xmin/2),
                log(2*xmax),
                (log(2*xmax)-log(xmin/2))/5000))

  for (gi in seq_along(grps)){
    d_g <- subset.data.frame(x = LDA_tab,
                             subset = LDA_tab$group == grps[gi])
    d_a <- LDA_activity_single(x = d_g[,1:3])
    out_act[[gi]]$model$fit <- d_a$model
    pred <- predict(object = d_a$model,
                    newdata = new.data,
                    type = "response",
                    se.fit = TRUE)
    out_act[[gi]]$model$line <- data.frame("x" = exp(new.data$x),
                                       "y" = log(1-pred$fit))
    z <- qnorm(1-alpha/2)
    #z <- qnorm(1-0.165/2)

    polygon_y <- c(1-(pred$fit+z*pred$se.fit),
                   1-(pred$fit-z*pred$se.fit))
    polygon_y[(polygon_y <= 0)] <- NaN
    polygon_y <- log(polygon_y)
    polygon_y[is.nan(polygon_y)] <- min(polygon_y,na.rm = TRUE)

    out_act[[gi]]$model$unc.alpha <- alpha
    out_act[[gi]]$model$unc.band <- data.frame(
      "x" = exp(new.data$x),
      "y.ub" = polygon_y[seq_along(new.data$x)],
      "y.lb" = polygon_y[seq_along(new.data$x)+length(new.data$x)])

  }

  # +------------------------+
  # | Part II :      SF      |
  # +------------------------+
  if (N_treat > 1){
    out_SF <- data.frame(
      "treat" = 0,
      "sf" = 1,
      "sf.msd" = 1,
      "sf.psd" = 1,
      "un.type" = uncertainty,
      "col" = NA,
      "sf.msd.ep" = 1,
      "sf.psd.ep" = 1,
      stringsAsFactors = FALSE
    )
    SF <- LDA_survival(LDA_tab[,1:4])
    for (t in seq_along(SF)) {
      CurSF <- SF[[t]]
      out_SF <- rbind(out_SF, c(CurSF$treat,
                                CurSF$sf,
                                CurSF[[4]],
                                uncertainty,
                                NA,
                                CurSF[[3]]),stringsAsFactors = FALSE)
    }
    out_SF$col <- colhex
    out_SF[,2] <- as.numeric(out_SF[,2])
    out_SF[,3] <- as.numeric(out_SF[,3])
    out_SF[,4] <- as.numeric(out_SF[,4])
    out_SF[,7] <- as.numeric(out_SF[,7])
    out_SF[,8] <- as.numeric(out_SF[,8])
  }
  return(list("Fig.Act" = out_act,
              "Fig.SF" = out_SF))
}
