
calcPoly <- function(fnc, npoly = 3, fnc.name = NULL, gls.covmod = list(covmod = expcov, d = 0.7, alpha = 0, se = 1), 
         pad = 1.2, resample = 100, range = NULL, verbose = FALSE){
  
  
  if(is.null(fnc.name)){
    
    fnc.name <- colnames(fnc)[3]
    
  }
  
  if(ncol(fnc) > 3){
    warning(paste("multiple functional traits provided, only using the first: ",fnc.name,"\n"), call. = F)
    
  }
  X <- na.omit(as.matrix(fnc))
  
  x <- X[,1]
  y <- X[,2]
  z <- X[,3]
  
  
  if (is.null(range)){
    range <- rbind(range(x)*pad,
                   range(y)*pad)
  }
  
  if (is.null(resample)){
    resample = length(x)
  }
  
  if(!is.null(gls.covmod)){
    
    # gls.covmod <- list(d = 0.7, covmod = "expcov")
    
    gls.covmod$np <- npoly
    gls.covmod$x <- x
    gls.covmod$y <- y
    gls.covmod$z <- z
    
    poly <- do.call(surf.gls, gls.covmod)
    
  } else{
    
    poly <- spatial::surf.ls(np = npoly, x = x, y = y, z = z) #fit polynomial least squares trend surface
    
  }
  
  surf <- spatial::trmat(poly, range[1,1], range[1,2],
                         range[2,1], range[2,2], resample) # evaluate grid points over surface
  
  surf$z <- scale.z(surf$z)
  
  
  grid <- cbind(expand.grid(surf$x, surf$y), c(surf$z))
  colnames(grid) <- c("x","y","z")
  range(grid$z)
  
  peak <- unlist(grid[which.max(grid$z),])
  
  
  poly_surf <- list(fnc.name = fnc.name, poly = poly, surface = surf, grid = grid, peak = peak)
  
  if(verbose){
    cat("-", fnc.name,"\n")
    cat(summary(poly),"\n")
    cat("\n")
  }
  class(poly_surf) <- "poly_surf"
  attr(poly_surf, "fnc.name") <- fnc.name
  attr(poly_surf, "npoly") <- npoly
  
  
  
  return(poly_surf)
}


# polysurf <- calcPoly(warps_fnc)

multiPoly <- function(fnc_df, npoly = 3, ...){
  
  if( any(class (fnc_df) != "fnc_df")){
    fnc_df <- as_fnc_df(fnc_df)
  }
  
  fnc <- fnc_df[,3:ncol(fnc_df)]
  
  if(length(npoly) > 1){
    if(length(npoly) != ncol(fnc)){
      stop("npoly should be either a single numeric, or a numeric vector with length =  number of fnc traits")
    }
  } 
  
  if(length(npoly)==1){
    npoly <- rep(npoly,ncol(fnc))
    names(npoly)<- colnames(fnc)
  }
  
  cat("polynomials:\n")
  print(npoly)
  
  
  multi_poly <- list()

    for(i in 1:ncol(fnc)){
      
      Y <- cbind(fnc_df[,1:2], fnc[,i])
      colnames(Y)[3] <- colnames(fnc)[i]
      multi_poly[[i]] <- calcPoly(Y,  ...)
    }
  
  
  names(multi_poly) = colnames(fnc)
  
  class(multi_poly) <- "multi_poly"
  attr(multi_poly, "npoly") <- npoly
  return(multi_poly)
  
}

# 
# multi_poly <- multiPoly(warps_fnc)
# 
# multi_poly

print.multi_poly <- function(x,...){
  
  cat("A multi_poly object\n")
  
  cat("- functional characteristics:\n")
  cat("\t", paste(names(x), collapse = ", "), "\n", sep = "")
  
  
}

# print.multiPoly(multi_poly)

summary.poly_surf <- function(object,...){
  
  cat("-", object$fnc.name,"\n")
  cat("npoly: ", object$poly$np, "\n")
  cat("peak: ", "\n")
  print(object$peak)
  cat("\n")
  summary(object$poly)
  cat("\n")
  
}

# summary.calcPoly(polysurf)

summary.multi_poly <- function(object,...){
  
  print(attributes(object))
  
  for(i in 1:length(object)){
    
    summary(object[[i]])
    
  }
  
}



# multi_poly
# 
# summary.multiPoly(multi_poly)


# surf <- polysurf$surface
# grid <- polysurf$grid
# 
# peak <- polysurf$peak[1:2]


plot.poly_surf <- function(x, ...){
  
  fnc.name <- attr(x, "fnc.name")
  
  surf <- x$surface
  peak <- x$peak[1:2]
  
  # if(method == "base"){
    plot(surf$x,surf$y, asp=1,type="n", xlab = "", ylab = "", axes= FALSE,
         xlim = range(surf$x), ylim = range(surf$y))
    title(main = fnc.name)
    
    levels <- pretty(range(surf$z), 10)
    
    
    .filled.contour(x=surf$x, y=surf$y, z=surf$z,
                    levels=levels, 
                    # plot.axes(points(peak, pch = 15, col = "black")),
                    col=viridisLite::viridis(length(levels)-1))
    
    contour(x=surf$x, y=surf$y, z=surf$z,
            levels=levels, col = scales::alpha("white", alpha = 0.7),
            add = T, drawlabels = T, lwd= 2 )
    
    points(x = peak[1], y = peak[2], pch = 4, col = "black", cex =2, lwd = 2)
    
    
    axis(side=1, at = axisTicks(usr = range(surf$x), log = F), pos = min(surf$y))
    axis(side=2, at = axisTicks(usr = range(surf$y), log = F), pos = min(surf$x))
    
    axis(side=1, at = range(surf$x), labels = F, lwd.ticks = 0, pos = min(surf$y))
    axis(side=2, at = range(surf$y), labels = F, lwd.ticks = 0, pos = min(surf$x))
    axis(side=3, at = range(surf$x), labels = F, lwd.ticks = 0, pos = max(surf$y))
    axis(side=4, at = range(surf$y), labels = F, lwd.ticks = 0, pos = max(surf$x))
  # }
  
  # if(method == "ggplot"){
  #   
  #   grid <- x$grid
  #   # plot using ggplot
  #   p <- ggplot() +
  #     geom_raster(data = grid, mapping = aes(x = .data$x, y = .data$y, fill = .data$z),
  #                 interpolate = T) +
  #     labs(title = fnc.name) +
  #     # facet_wrap(~func) +
  #     scale_fill_continuous(name = NULL, type = "viridis") +
  #     geom_point(aes(x = peak[1], y = peak[2]), shape = 4, size = 3, stroke = 2) +
  #     # scale_fill_binned(name = NULL, type = "viridis") +
  #     # coord_cartesian(expand = FALSE) +
  #     coord_fixed(expand = FALSE) + #uncomment to make the x and y axis the same scale (square pixels)
  #     theme(plot.title = element_text(hjust = 0.5),
  #           # axis.text = element_blank(),
  #           # axis.ticks = element_blank(),
  #           axis.title = element_blank(),
  #           panel.background = element_blank(),
  #           panel.border = element_rect(fill = NA, color = "black"),
  #           panel.grid = element_blank())
  #   p <- p  +
  #     metR::geom_contour2(aes(x = grid$x, y = grid$y, z = grid$z,
  #                             label = stat(level)),
  #                         color = "white", alpha = .7)
  #   
  #   p
  # }
  
}
# 


plot.multi_poly <- function(x, ...){
  
  # if(method == "base"){
    
    par(mfrow= c(length(x)/2 ,2), mar = c(1.5,1,2.5,0.5))
    
    for(i in 1:length(x)){
      plot(x[[i]])
    }
    
  # }
  
}

# plot.multiPoly(multi_poly)

