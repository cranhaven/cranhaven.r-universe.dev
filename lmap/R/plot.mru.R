#' Plots a Multinomial Restricted MDU model
#' 
#'
#' @param x an object of type mru
#' @param dims which dimensions to visualize
#' @param class.regions whether a voronoi diagram with classification regions should be included
#' @param ycol colour for representation of response variables
#' @param xcol colour for representation of predictor variables
#' @param ocol colour for representation of row objects
#' @param \dots additional arguments to be passed.
#' @return Plot of the results obtained from mru
#'
#' @examples
#' \dontrun{
#' data(dataExample_mru)
#' y = as.matrix(dataExample_mru[ , 1])
#' X = as.matrix(dataExample_mru[ , 2:6])
#' output = mru(y = y, X = X, S = 2)
#' plot(output)
#' }
#'
#' @import ggforce
#' @import ggplot2
#' @import ggrepel
#'
#' @export
plot.mru = function(x, dims = c(1,2), class.regions = FALSE, ycol = "darkgreen", xcol = "lightskyblue", ocol = "grey",...)
{

  object = x

  # retrieving infomation from object
  U = object$U[, dims]
  V = object$V[, dims]
  X = object$X

  ynames = object$ynames
  xnames = object$xnames

  UU = as.data.frame(U)
  VV = as.data.frame(V)
  colnames(UU) = colnames(VV) = c("dim1", "dim2")


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

  if(is.factor(ocol)) plt = ggplot() + geom_point(data = UU, aes(x = .data$dim1, y = .data$dim2, col = ocol), show.legend = FALSE)
  else plt = ggplot() + geom_point(data = UU, aes(x = .data$dim1, y = .data$dim2), col = ocol)

  ######################################################
  # variable axes with ticks and markers for predictors
  ######################################################
  plt = plt + geom_abline(intercept = 0, slope = B[,2]/B[,1], colour = xcol, linetype = 3) +
    geom_line(data = MCx1, aes(x = .data$dim1, y = .data$dim2, group = .data$varx), col = xcol, linewidth = 1) +
    geom_point(data = MCx2, aes(x = .data$dim1, y = .data$dim2), col = xcol) +
    geom_text(data = MCx2, aes(x = .data$dim1, y = .data$dim2, label = labs), nudge_y = -0.08, size = 1.5)

  ######################################################
  # class points and names
  ######################################################
  plt = plt + geom_point(data = VV, aes(x = .data$dim1, y = .data$dim2), colour = ycol, size = 3) +
    geom_text_repel(data = VV, aes(x = .data$dim1, y = .data$dim2, label = ynames), family = "mono") +
    xlab("Dimension 1") +
    ylab("Dimension 2")

  ######################################################
  # variable labels
  ######################################################

  margins <- c("l" = ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range[1] - .1,
               "r" = ggplot_build(plt)$layout$panel_scales_x[[1]]$range$range[2] + .1,
               "b" = ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range[1] - .1,
               "t" = ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range[2] + .1)


  beta <- B[,2]/B[,1]
  lab <- data.frame("xname" = xnames,
                    "b" = beta,
                    "Yleft" = beta*margins["l"],
                    "Yright" = beta*margins["r"])

  orientation = sign(B[,1]) #sign of dim1 defines direction l-r
  lab$side =  c("left","right")[ as.numeric(B[,1] > 0)+1]
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

  plt = plt + coord_fixed(xlim = margins[c("l","r")], ylim = margins[c("b","t")], expand = F) +
    theme(axis.line = element_line(colour = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank())

  if(class.regions){
    plt = plt + geom_voronoi_segment(data = VV, aes(x = .data$dim1, y = .data$dim2),
                                   bound = margins,
                                   color = ycol,
                                   linetype = 4)
  }

  suppressWarnings(print(plt))

}
