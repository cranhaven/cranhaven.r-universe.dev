#' Plots a Cumulative Logistic MDU model
#'
#' @param x an object of type clmdu
#' @param dims which dimensions to visualize
#' @param circles which circles to visualize
#' @param ycol colour for representation of response variables
#' @param xcol colour for representation of predictor variables
#' @param ocol colour for representation of row objects
#' @param \dots additional arguments to be passed.
#' @return Plot of the results obtained from clmdu
#'
#' @examples
#' \dontrun{
#' data(dataExample_clmdu)
#' Y = as.matrix(dataExample_clmdu[ , 1:8])
#' X = as.matrix(dataExample_clmdu[ , 9:13])
#' # unsupervised
#' output = clmdu(Y = Y, S = 2)
#' plot(output)
#' }
#'
#'
#' @import ggforce
#' @import ggplot2
#' @import ggrepel
#'
#'
#' @export

plot.clmdu = function(x, dims = c(1,2), circles = seq(1,R), ycol = "darkgreen", xcol = "lightskyblue", ocol = "grey",...)
{

  object = x
  ccol = ycol

  # retrieving information from object
  m = object$m
  U = object$U[, dims]
  V = object$V[, dims]
  R = nrow(V)
  X = object$X
  isx = !is.null(X)

  ynames = object$ynames
  xnames = object$xnames

  UU = as.data.frame(U)
  VV = as.data.frame(V)
  colnames(UU) = colnames(VV) = c("dim1", "dim2")
  rownames(VV) = ynames

  if(!is.null(circles)){
    a = -m[[circles[1]]]
    # circles1 = cbind(circles[1], outer( rep(1, length( m[[circles[1]]][m[[circles[1]]] > 0] ) ) , V[circles[1], ]), m[[circles[1]]][m[[circles[1]]] > 0] )
    circles1 = cbind(circles[1], outer( rep(1, length( a[a > 0] ) ) , V[circles[1], ]), a[a > 0] )
    if(length(circles)>1){
      for(r in circles[-1]){
        a = -1 * m[[r]]
        circles1 = rbind(circles1,
                         cbind(r, outer( rep(1, length( a[a > 0] ) ) , V[r, ]), a[a > 0]  )
        )
      }
    }

    colnames(circles1) = c("var", "x0", "y0", "r")
    cnames = rownames(circles1)
    circles2 = data.frame(circles1)
    # for labels
    circles3 = circles2
    circles3$x0 = circles2$x0 - circles2$r
  }

  if(isx){
    P = ncol(X)
    B = object$B[ , dims]
    Xo = object$Xoriginal

    # for solid line
    MCx1 <- data.frame(labs=character(),
                       varx = integer(),
                       dim1 = double(),
                       dim2 = double(), stringsAsFactors=FALSE)
    # for markers
    MCx2 <- data.frame(labs=character(),
                       varx = integer(),
                       dim1 = double(),
                       dim2 = double(), stringsAsFactors=FALSE)

    ll = 0
    lll = 0
    for(p in 1:P){
      b = matrix(B[p , ], 2, 1)
      # solid line
      minx = min(Xo[, p])
      maxx = max(Xo[, p])
      m.x1 = c(minx,maxx)
      markers1 = matrix((m.x1 - object$mx[p])/object$sdx[p], 2, 1)
      markerscoord1 = outer(markers1, b) # markers1 %*% t(b %*% solve(t(b) %*% b))
      MCx1[(ll + 1): (ll + 2), 1] = paste0(c("min", "max"), p)
      MCx1[(ll + 1): (ll + 2), 2] = p
      MCx1[(ll + 1): (ll + 2), 3:4] = markerscoord1
      ll = ll + 2
      # markers
      m.x2 = pretty(Xo[, p])
      m.x2 = m.x2[which(m.x2 > minx & m.x2 < maxx)]
      l.m = length(m.x2)
      markers2 = matrix((m.x2 - object$mx[p])/object$sdx[p], l.m, 1)
      markerscoord2 = outer(markers2, b) # markers2 %*% t(b %*% solve(t(b) %*% b))
      MCx2[(lll + 1): (lll + l.m), 1] = paste(m.x2)
      MCx2[(lll + 1): (lll + l.m), 2] = p
      MCx2[(lll + 1): (lll + l.m), 3:4] = markerscoord2
      lll = lll + l.m
    } # loop p
  } #isx


  ######################################################
  # baseplot with points for objects and response variables
  ######################################################
  plt = ggplot() +
    geom_point(data = UU, aes(x = .data$dim1, y = .data$dim2), col = ocol) +
    geom_point(data = VV, aes(x = .data$dim1, y = .data$dim2), colour = ycol, size = 5) +
    xlab(paste("Dimension", dims[1])) +
    ylab(paste("Dimension", dims[2]))

  margins <- c("l" = ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range[1] - .1,
               "r" = ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range[2] + .1,
               "b" = ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range[1] - .1,
               "t" = ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range[2] + .1)

  ######################################################
  # add circles representin thresholds
  ######################################################
  if(!is.null(circles)){
    plt = plt + geom_circle(data = circles2,
                          aes( x0 = .data$x0, y0 = .data$y0, r = r),
                          colour = ccol, fill= ccol,
                          alpha = 0.05, linetype = "dotted", show.legend = FALSE) +
    geom_text(data = circles3, aes(x = .data$x0, y = .data$y0, label = cnames), nudge_x = 0.1, size = 2)
  }

  ######################################################
  # variable axes with ticks and markers for predictors
  ######################################################
  if(isx){
    plt = plt + geom_abline(intercept = 0, slope = B[,2]/B[,1], colour = xcol, linetype = 3) +
      geom_line(data = MCx1, aes(x = .data$dim1, y = .data$dim2, group = .data$varx), col = xcol, linewidth = 1) +
      geom_point(data = MCx2, aes(x = .data$dim1, y = .data$dim2), col = xcol) +
      geom_text(data = MCx2, aes(x = .data$dim1, y = .data$dim2, label = labs), size = 1.5)
  }

  ######################################################
  # response variable labels
  ######################################################
  rownamesVV<-rownames(VV)
  plt = plt + geom_text_repel(data = VV, aes(x = .data$dim1, y = .data$dim2, label = rownamesVV,
                                             family = 'mono', fontface = 'bold'), col = "black", size = 5)

  ######################################################
  # variable labels
  ######################################################

  if(isx){
    BV = B
    names = object$xnames

    beta <- BV[,2]/BV[,1]

    lab <- data.frame("xname" = names,
                      "b" = beta,
                      "Yleft" = beta*margins["l"],
                      "Yright" = beta*margins["r"])

    orientation = sign(BV[,1]) #sign of dim1 defines direction l-r
    lab$side =  c("left","right")[ as.numeric(BV[,1] > 0)+1]
    lab$side[lab$Yleft < margins["b"] & orientation<0 ] = "bottom"
    lab$side[lab$Yleft > margins["t"] & orientation<0 ] = "top"
    lab$side[lab$Yright < margins["b"]& orientation>0] = "bottom"
    lab$side[lab$Yright > margins["t"]& orientation>0] = "top"

    lab$X <- lab$Y <- NA
    lab$X[lab$side == "bottom"] <- (margins["b"]/beta[lab$side == "bottom"])
    lab$X[lab$side == "top"] <- (margins["t"]/beta[lab$side == "top"])
    lab$Y[lab$side == "left"] <- margins["l"]*beta[lab$side == "left"]
    lab$Y[lab$side == "right"] <-margins["r"]*beta[lab$side == "right"]

    lab <- split(lab, lab$side)

    plt = plt +
      scale_x_continuous(breaks = lab$bottom$X, labels = lab$bottom$xname, sec.axis = sec_axis(trans ~ ., breaks = lab$top$X, labels = lab$top$xname)) +
      scale_y_continuous(breaks = lab$left$Y, labels = lab$left$xname, sec.axis = sec_axis(trans ~ ., breaks = lab$right$Y, labels = lab$right$xname))
  }

  plt = plt +
    coord_fixed(xlim = margins[c("l","r")], ylim = margins[c("b","t")], expand = F) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())

  suppressWarnings(print(plt))

  return(plt)
}
