# Create the 2D color scheme

normvec <- function(vec, res, lims = range(vec, na.rm = T)){
  vec <- vec - lims[1]
  vec <- vec/diff(lims)
  vec <- round(vec*(res-1)) / (res-1)
  vec
}


assign_col <- function(vec1, vec2, res = 10, colour = "green_purple", vec1lims = range(vec1, na.rm = T), vec2lims = range(vec2, na.rm = T), samelims = FALSE)
{
  if (samelims)
    vec1lims = vec2lims = range(c(vec1lims, vec2lims))
  ret <- vec1 * vec2
  vec1 <- vec1[!is.na(ret)]
  vec2 <- vec2[!is.na(ret)]
  vec1 <- normvec(vec1, res, vec1lims)
  vec2 <- normvec(vec2, res, vec2lims)
  if (colour == "green_purple")
    tmp <- rgb((vec1 + vec2)/2, vec1, vec2) else
    if (colour == "green_red_purple_cyan")
      tmp <- rgb((1-vec1 + vec2)/2, vec1, 1-vec2) else 
        if (colour == "green_red_purple_cyan_2")
          tmp <- rgb((vec1 + vec2)/2, 1-vec1, 1-vec2) else 
            stop("not recognized colour scheme")
  ret[!is.na(ret)] <- tmp
  
  retvec1 <- ret
  retvec2 <- ret
  retvec2[!is.na(ret)] <- vec2 * (res-1)
  retvec1[!is.na(ret)] <- vec1 * (res-1)
  attr(ret, "vec1") <- retvec1
  attr(ret, "vec2") <- retvec2
  ret
}


#legend 
plot_legend <- function(vec1, vec2, res = 10, show = T, colour = "green_purple", vec1lims = range(vec1, na.rm = T), vec2lims = range(vec2, na.rm = T), samelims = FALSE){
  if (samelims)
    vec1lims = vec2lims = range(c(vec1lims, vec2lims))
  v1 <- seq(vec1lims[1], vec1lims[2], length = res)
  v2 <- seq(vec2lims[1], vec2lims[2], length = res)
  pp = expand.grid((0:(res-1))/(res-1),(0:(res-1))/(res-1))
  pp$colorid <- 1:nrow(pp)
  pp$color <- assign_col(pp[,1], pp[,2], res, colour)
  if (show) image(list(x = v1, y = v2, z = matrix(1:(res^2), nrow = res)), col = pp$color)
  pp <- data.frame(pp, data.frame(t(col2rgb(pp$color))),  v1 = v1, v2 = v2)
  box(lwd = 0.5)
  invisible(pp)
}

dist_to_highest <- function(vec1, vec2){
  vec1 <- vec1 - min(vec1, na.rm = T)
  vec1 <- vec1/max(vec1, na.rm = T)
  vec2 <- vec2 - min(vec2, na.rm = T)
  vec2 <- vec2/max(vec2, na.rm = T)
  sqrt((1-vec1)^2 + (1-vec2)^2)
}

two_color_map <- function(distrib_data, vec1, vec2, res = 10, showlegend = T, legend_coords = c(0.2, 0.26, 0.36, 0.44), type = c("auto", "grid", "points"), colour = c("green_purple", "green_red_purple_cyan"), ...){
  colour <- match.arg(colour)
  retcol <- assign_col(vec1, vec2, res, colour)
  
  if (inherits(distrib_data, "distrib_data")){
    sit = sites(distrib_data)
    nsit = Nsites(distrib_data)
  } else {
    if (inherits(distrib_data, "SpatialPoints")) nsit = nrow(coordinates(distrib_data)) else nsit = nrow(distrib_data)
    if (!is.null(names(vec1))) sit = names(vec1) else sit = 1:nsit
  }

  plot_sitestat(distrib_data, 1:nsit, col = retcol, legend = F, ...)

  oldpar <- par(new = TRUE, pty = "s", plt = legend_coords, cex.axis = 0.4, las = 2)
  ret <- plot_legend(vec1, vec2, res, show = showlegend, colour = colour)
  par(oldpar)
  
  colorcoords = data.frame(site = sit, colorid = ret$colorid[match(retcol, ret$color)])
  invisible(list(colorlegend = ret, colorcoords, cd = data.frame(attr(retcol, "vec1"), attr(retcol, "vec2"), retcol, dist_to_highest(vec1, vec2))))
}








