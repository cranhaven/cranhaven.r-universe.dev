#' Plot stData objects
#'
#' This function provides basic plotting for telefit package data.
#' 
#' @export
#' @method plot stData
#' 
#' @import ggplot2
#' @import dplyr
#' @importFrom stringr str_wrap
#' @importFrom reshape2 melt
#' @importFrom cowplot plot_grid
#' @importFrom stats cor.test
#' 
#' @param t timepoint to plot.  Will automatically plot the first timepoint if
#'  t=NULL.
#' @param p column index of local covariate to plot if type='covariate'. Will 
#'  automatically assume the local covariate data includes an intercept and will
#'  plot the second column if p=NULL.
#' @param map name of map provided by the maps package. These include county, 
#'  france, italy, nz, state, usa, world, world2.  By default, all stData plots
#'  will include us state outlines.
#' @param region name of subregions to include. Defaults to . which includes 
#'  all subregions. See documentation for map for more details.
#' @param type One of the following options to specify what type of plot to build
#'    \describe{
#'      \item{response}{  }
#'      \item{sd.response}{ Plot standard deviation of response variable at each location. }
#'      \item{cat.response}{  }
#'      \item{covariate}{  }
#'      \item{remote}{  }
#'      \item{teleconnection}{ This plot only applies if the stData object 
#'      contains information about teleconnection effects,
#'      i.e., if it is a simulated dataset or otherwise modified to include 
#'      estimates of teleconnection effects. }
#'      \item{remote_cor}{ This plot shows pointwise correlations between a local
#'       coordinate and the remote covariates.  }
#'      \item{eof}{  }
#'      \item{eof_scores}{  }
#'      \item{eof_scree}{ }
#'      \item{eof_cor}{ This plot shows pointwise correlations with EOF patterns. }
#'      \item{local_cor}{ This plot shows pointwise correlations with local covariates. }
#'      \item{teleconnection_knot_local}{ }
#'    }
#' @param x Object of class stData to plot.
#' @param coord.s if plot type is 'teleconnection', specifies the longitude and 
#'  latitude of local coordinate for which to plot teleconnection effects. if 
#'  NULL, the middle local coordinate will be plotted.
#' @param zlim c(min, max) vector that specifies the colorscale limits
#' @param zmid number that specifies the midpoint of the colorscale
#' @param lab.teleconnection label used for fill scale in teleconnection plot
#' @param fill.lab.width line width for fill scale label
#' @param category.breaks [ncoords x ncats] list of breakpoints used for binning
#'  responses into categories
#' @param coords.knots if plot type is 'remote', specifies the longitude and
#'  latitude of knot locations to overlay on the 'remote' plot
#' @param signif.telecon if TRUE, will highlight significant grid cells if the
#'  plotting data contain a signif column
#' @param signif.level significance level for eof_cor significance highlighting
#' @param coord.r if plot type is 'teleconnection_local', specifes the longitude
#'  and latitude of remote coordinate for which to plot associated teleconnection
#'  effects.  if NULL, the middle remote coordinate will be plotted.
#' @param pattern if type=='eof' this specifies which (remote) EOF pattern to plot
#'  or if type=='eof_scores' this (vector) specifies which (remote) EOF pattern
#'  scores to plot
#' @param lwd line width for when plotting with signif.telecon==T
#' @param cutoff Used to denote where this proportion of variance is achieved in
#'   the eof_scree plots
#' @param alpha the level of fading that should be applied to insignificant
#'   grid boxes when plotting significant effects
#' @param fill.lab Optional label to override the default fill scale labels
#' @param contour c(TRUE, TRUE) to plot local and remote responses as contours
#'   vs. observations
#' @param dots additional named arguments with defaults to pass to additional 
#'   functions
#' @param ... additional arguments to pass to functions
#'  
#' @return a ggplot object with the specified map
#'
#' @examples
#' 
#' data("coprecip")
#' p = plot(coprecip)
#' 

plot.stData = function( x, type='response', t=NULL, p=NULL,  
                        map='world', region='.', coord.s=NULL, coord.r=NULL,
                        zlim=NULL, fill.lab=NULL,
                        lab.teleconnection = expression(alpha),
                        fill.lab.width = 20, category.breaks = NULL,
                        coords.knots = NULL, signif.telecon = F, dots=NULL, 
                        pattern = 1, lwd=1.75, cutoff=.9, signif.level=.05, 
                        alpha = .2, zmid = 0, contour = c(F,F), ...) {
  stData = x
  # merge unique list of dots
    dots = c(dots, list(...))
    dots = dots[!duplicated(dots)]
  # overwrite arguments to function if they exist in dots
    for(x in setdiff(names(formals(eval(match.call()[[1]]))), c('dots', '...'))) {
      if(x %in% names(dots)) {
        assign(eval(x), dots[[x]])
      }
    }
  
  if(!is.null(coord.s))
    coord.s=unlist(coord.s)
    
  if(is.null(t))
    t=stData$tLabs[1]
  
  if(is.null(p))
    p=2
  
  if(!is.null(stData$Y)) {
    if(!inherits(stData$Y, 'matrix'))
      stData$Y = matrix(stData$Y, nrow = nrow(stData$coords.s))  
  }
  
  if(!is.null(stData$Y.cat)) {
    if(!inherits(stData$Y.cat, 'matrix'))
      stData$Y.cat = matrix(stData$Y.cat, nrow = nrow(stData$coords.s))  
  }
  
  if(!is.null(coords.knots)) {
    coords.knots = data.frame(coords.knots)
    colnames(coords.knots) = c('lon', 'lat')
    coords.knots = rbind(coords.knots, coords.knots %>% mutate(lon=lon-360))
  }
    
  ret = NULL
    
  # extract dataset to plot
  match.opts = c('response', 'covariate', 'remote', 'teleconnection', 'eof',
                 'eof_scores', 'cat.response', 'teleconnection_knot', 
                 'teleconnection_knot_local', 'eof_scree', 'eof_cor', 'local_cor',
                 'sd.response', 'remote_cor')
  type = match.opts[pmatch(type, match.opts)]
  if( type=='response' ) {
    Y = data.frame( Y = stData$Y[, match(t, stData$tLabs)],
                    lon.Y = stData$coords.s[,1], 
                    lat.Y = stData$coords.s[,2] )
    if(signif.telecon) { 
      Y = cbind(Y, signif = attr(stData$Y, 'signif')[, match(t, stData$tLabs)])
    }
    lab.col = ifelse(is.null(fill.lab), stData$Y.lab, fill.lab) 
    # scheme.col = list(low = "#a6611a", mid = '#f5f5f5', high = '#018571')
    scheme.col = list(low = "#0571b0", mid = '#f7f7f7', high = '#ca0020')
  } else if( type=='sd.response' ) {
    Y = data.frame( Y = apply(stData$Y, 1, sd),
                    lon.Y = stData$coords.s[,1], 
                    lat.Y = stData$coords.s[,2] )
    lab.col = ifelse(is.null(fill.lab), paste('S.D.', stData$Y.lab), fill.lab) 
    t = 'Response S.D.'
    # scheme.col = list(low = "#a6611a", mid = '#f5f5f5', high = '#018571')
    scheme.col = list(low = "#0571b0", mid = '#f7f7f7', high = '#ca0020')
  } else if( type=='covariate' ) {
    Y = data.frame( Y = stData$X[, p, match(t, stData$tLabs)],
                    lon.Y = stData$coords.s[,1], 
                    lat.Y = stData$coords.s[,2] )
    lab.col = ifelse(is.null(fill.lab), stData$X.lab, fill.lab) 
    scheme.col = list(low = "#008837", mid = '#f7f7f7', high = '#7b3294')
  } else if( type=='remote' ) {
    Y = data.frame( Y = stData$Z[, match(t, stData$tLabs)],
                    lon.Y = stData$coords.r[,1],
                    lat.Y = stData$coords.r[,2] )
    lab.col = ifelse(is.null(fill.lab), stData$Z.lab, fill.lab) 
    # scheme.col = list(low = "#008837", mid = '#f7f7f7', high = '#7b3294')
    scheme.col = list(low = "#0571b0", mid = '#f7f7f7', high = '#ca0020')
  } else if( type=='teleconnection' ) {
    
    n = nrow(stData$coords.s)
    r = nrow(stData$coords.r)
    
    if(is.null(coord.s))
      coord.s = stData$coords.s[round(n/2),]
    
    coord.s.ind = which.min(rdist.earth(matrix(coord.s, ncol=2), stData$coords.s))
    coord.s = stData$coords.s[coord.s.ind,]
    
    Y = data.frame( Y = as.numeric(stData$alpha),
                    signif = stData$alpha_signif,
                    lon.Z = stData$coords.r[,1], 
                    lat.Z = stData$coords.r[,2],
                    lon.Y = rep(stData$coords.s[,1], rep(r,n)),
                    lat.Y = rep(stData$coords.s[,2], rep(r,n)) ) %>% 
      filter(lon.Y==coord.s[1], lat.Y==coord.s[2]) %>% 
      mutate(lon.Y=lon.Z, lat.Y=lat.Z )
    
    lab.col = ifelse(is.null(fill.lab), lab.teleconnection, fill.lab) 
    scheme.col = list(low = "#0571b0", mid = '#f7f7f7', high = '#ca0020')
  } else if( type=='teleconnection_knot' ) {
    
    n = nrow(stData$coords.s)
    r_knots = nrow(stData$coords.knots)
    
    if(is.null(coord.s))
      coord.s = stData$coords.s[round(n/2),]
    
    coord.s.ind = which.min(rdist.earth(matrix(coord.s, ncol=2), stData$coords.s))
    coord.s = stData$coords.s[coord.s.ind,]
    
    Y = data.frame( Y = stData$alpha_knots,
                    signif = ifelse(stData$alpha_knots_signif, 3, 0),
                    lon.Z = stData$coords.knots[,1], 
                    lat.Z = stData$coords.knots[,2],
                    lon.Y = rep(stData$coords.s[,1], rep(r_knots,n)),
                    lat.Y = rep(stData$coords.s[,2], rep(r_knots,n)) ) %>% 
      filter(lon.Y==stData$coords.s[coord.s.ind, 1], 
             lat.Y==stData$coords.s[coord.s.ind, 2]) %>% 
      mutate(lon.Y=lon.Z, lat.Y=lat.Z )
    
    lab.col = ifelse(is.null(fill.lab), lab.teleconnection, fill.lab) 
    scheme.col = list(low = "#0571b0", mid = '#f7f7f7', high = '#ca0020')
  } else if( type=='teleconnection_knot_local' ) {
    
    n = nrow(stData$coords.s)
    r_knots = nrow(stData$coords.knots)
    
    if(is.null(coord.r))
      coord.r = stData$coords.s[round(r_knots/2),]
    
    Y = data.frame( Y = stData$alpha_knots,
                    lon.Z = stData$coords.knots[,1], 
                    lat.Z = stData$coords.knots[,2],
                    lon.Y = rep(stData$coords.s[,1], rep(r_knots,n)),
                    lat.Y = rep(stData$coords.s[,2], rep(r_knots,n)) ) %>% 
      filter(lon.Z==coord.r[1], lat.Z==coord.r[2])
    
    lab.col = ifelse(is.null(fill.lab), lab.teleconnection, fill.lab) 
    scheme.col = list(low = "#0571b0", mid = '#f7f7f7', high = '#ca0020')
  } else if( type=='cat.response' ) {
    
    # categorize Y according to given breakpoints, if necessary
    if(is.null(stData$Y.cat)) {
      stData$Y.cat = stData$Y
      for(s in 1:nrow(stData$Y)) {
        stData$Y.cat[s,] = 1 + findInterval(stData$Y[s,], category.breaks[s,])
      }
    }
    
    # auto-determine labels based on the number of category breaks
    if(ncol(category.breaks)==1) {
      cat.labels = c('Below average', 'Above average')
      scheme.col = c('Below average'='#fc8d59', 'Above average'='#91bfdb')
    } else if(ncol(category.breaks)==2) {
      cat.labels = c('Below average', 'Near average', 'Above average')
      scheme.col = c('Below average'='#fc8d59', 'Near average'='#ffffbf', 
                     'Above average'='#91bfdb')
    }
    
    Y.cat = as.numeric(stData$Y.cat[, match(t, stData$tLabs)])
    Y.cat = factor(Y.cat, labels=cat.labels[sort(unique(Y.cat))])
    
    # build plotting frame
    Y = data.frame( Y = Y.cat,
                    lon.Y = stData$coords.s[,1], 
                    lat.Y = stData$coords.s[,2] )
    lab.col = ifelse(is.null(fill.lab), paste(stData$Y.lab, 'level'), fill.lab) 
  } else if( type=='eof' ) {
    # build plotting frame
    Y = data.frame( Y = eof(stData$Z)$patterns[,pattern],
                    lon.Y = stData$coords.r[,1],
                    lat.Y = stData$coords.r[,2] )
    
    # set color and scale options
    lab.col = ifelse(is.null(fill.lab), '', fill.lab) 
    t = paste('EOF', pattern)
    scheme.col = list(low = "#0571b0", mid = '#f7f7f7', high = '#ca0020')
  } else if( type=='eof_scores') {
    
    ret = ggplot(melt(eof(stData$Z)$scores, varnames = c('t','EOF')) %>% 
             filter(EOF %in% pattern) %>% 
               mutate(EOF = factor(EOF),
                      t = as.numeric(stData$tLabs[t])), 
           aes(x=t, y=value, color=EOF)) +
      geom_line() + 
      ylab('Score') +
      xlab('Year')
  } else if( type=='eof_scree') {
    
    e = eof(stData$Z)
    
    ret = cowplot::plot_grid(
      ggplot(data.frame(EOF=1:length(e$sd), sd=e$sd), aes(x=EOF, y=sd)) +
        geom_line() + 
        ylab('Standard deviation') +
        xlab('EOF') + 
        ggtitle('EOF standard deviations'),
      ggplot(data.frame(EOF=1:length(e$sd), cum=cumsum(e$sd^2)/sum(e$sd^2)),
             aes(x=EOF, y=cum)) +
        geom_line() +
        ylab('Proportion of variance') +
        xlab('EOF') +
        ggtitle('EOF contributions') +
        geom_hline(yintercept = cutoff, lty=2),
      ncol=2
    )
      
  }  else if( type=='eof_cor' ) {
  
    # build plotting frame
    sc = eof(stData$Z)$scores[,pattern]
    Y = data.frame( Y = as.numeric(cor(sc, t(stData$Y))),
                    lon.Y = stData$coords.s[,1],
                    lat.Y = stData$coords.s[,2] )
    
    if(signif.telecon) {
      Y$signif = apply(stData$Y, 1, 
                       function(y) { cor.test(y, sc)$p.value }) < signif.level
    }
    
    # set color and scale options
    lab.col = ifelse(is.null(fill.lab), 'Cor.', fill.lab)
    t = paste('Pointwise correlations with EOF', pattern)
    scheme.col = list(low = "#0571b0", mid = '#f7f7f7', high = '#ca0020')
  } else if( type=='local_cor' ) {
    
    # build plotting frame
    Y = foreach(i = 1:nrow(stData$coords.s), .combine='rbind') %do% {
      r = data.frame( Y = cor(stData$Y[i,], stData$X[i,p,]),
                      lon.Y = stData$coords.s[i,1],
                      lat.Y = stData$coords.s[i,2] )
      
      if(signif.telecon) {
        r$signif = cor.test(stData$Y[i,], stData$X[i,p,])$p.value < signif.level
      }
      
      r
    }
    
    # set color and scale options
    lab.col = ifelse(is.null(fill.lab), 'Cor.', fill.lab)
    t = paste('Pointwise correlations with', colnames(stData$X)[p])
    scheme.col = list(low = "#0571b0", mid = '#f7f7f7', high = '#ca0020')
  } else if( type=='remote_cor' ) {
    
    n = nrow(stData$coords.s)
    
    if(is.null(coord.s))
      coord.s = stData$coords.s[round(n/2),]
    
    coord.s.ind = which.min(rdist.earth(matrix(coord.s, ncol=2), stData$coords.s))
    coord.s = stData$coords.s[coord.s.ind,]
    
    # build plotting frame
    Y = foreach(i = 1:nrow(stData$coords.r), .combine='rbind') %do% {
      r = data.frame( Y = cor(stData$Z[i,], stData$Y[coord.s.ind,]),
                      lon.Y = stData$coords.r[i,1],
                      lat.Y = stData$coords.r[i,2] )
      
      if(signif.telecon) {
        r$signif = cor.test(stData$Z[i,], stData$Y[coord.s.ind,])$p.value < signif.level
      }
      
      r
    }
    
    # set color and scale options
    lab.col = ifelse(is.null(fill.lab), 'Cor.', fill.lab)
    t = paste('Pointwise correlations with', round(coord.s,1))
    scheme.col = list(low = "#0571b0", mid = '#f7f7f7', high = '#ca0020')
    
  }
  
  if(!is.null(ret)) {
    return(ret)
  }
  
  # compute truncations and apply wrapping
  if(type %in% c('remote', 'teleconnection', 'eof', 'teleconnection_knot', 
                 'remote_cor')) {
    if(max(Y$lon.Y)>0) {
      if(min(Y$lon.Y)<0) {
        lon.E = max(Y %>% filter(lon.Y<=0) %>% dplyr::select(lon.Y))
        lon.W = min(Y %>% filter(lon.Y>0) %>% dplyr::select(lon.Y)) - 360
      } else {
        lon.E = max(Y$lon.Y) - 360
        lon.W = min(Y$lon.Y) - 360
      }
    } else {
      lon.E = max(Y$lon.Y)
      lon.W = min(Y$lon.Y)
    }
    lat.S = min(Y$lat.Y)
    lat.N = max(Y$lat.Y)
  } else {
    lon.W = min(Y$lon.Y)
    lon.E = max(Y$lon.Y)
    lat.S = min(Y$lat.Y)
    lat.N = max(Y$lat.Y)
  }
  

  # get us state outlines ggplot format
  world = map_data('state', region=region)
  # get country outlines ggplot format
  if(map=='world') {
    # get raw outline data
    world.raw = map_data('world') %>% filter(region!='USA')
    # duplicate countries for plotting with any map center
    world.raw = rbind(world.raw, world.raw %>% 
                        mutate(long=long-360, group=group+max(group)+1))
    # add outline data to state outlines
    world = rbind(world, world.raw %>% mutate(group=group+max(world$group)+1))
  }
  
  
  #
  # set commands to modify plotting options, if specified
  #
  
  if(type!='teleconnection_knot') {
    # tile.aes = aes(x=lon.Y, y=lat.Y, fill=Y, z=Y, color=factor(..level..))
    tile.aes = aes(x=lon.Y, y=lat.Y, fill=Y, z=Y)
    alpha = ifelse(signif.telecon, alpha, 1)
  } else {
    if(signif.telecon) {
      point.aes = aes(x=lon.Y, y=lat.Y, fill=Y, z=Y, stroke=signif, color=..level..)
    } else {
      point.aes = aes(x=lon.Y, y=lat.Y, fill=Y, z=Y, stroke=0, color=..level..)
    }
  }
  
  # wrap fill label
  if(!inherits(lab.col, 'expression')) {
    lab.col = str_wrap(lab.col, width=fill.lab.width)
  }
  
  if(type=='sd.response') {
    fillscale = scale_fill_distiller(lab.col, palette='YlOrRd', direction=1)
  } else if(type=='cat.response') {
    fillscale = scale_fill_manual(lab.col, values = scheme.col)
  } else if(is.null(zlim)) {
    fillscale = scale_fill_gradient2(lab.col,
                                     low = scheme.col$low, 
                                     mid = scheme.col$mid, 
                                     high = scheme.col$high,
                                     midpoint = zmid)
    colscale = scale_color_gradient2(lab.col,
                                     low = scheme.col$low, 
                                     mid = scheme.col$mid, 
                                     high = scheme.col$high,
                                     midpoint = zmid)
  } else  {
    fillscale = scale_fill_gradient2(lab.col,
                                     low = scheme.col$low, 
                                     mid = scheme.col$mid, 
                                     high = scheme.col$high,
                                     limits = zlim,
                                     midpoint = zmid)
    colscale = scale_color_gradient2(lab.col,
                                     low = scheme.col$low, 
                                     mid = scheme.col$mid, 
                                     high = scheme.col$high,
                                     limits = zlim,
                                     midpoint = zmid)
  } 
  
  # build base plot
  if(type!='teleconnection_knot') {
    worldmap = ggplot(world, aes(x=long, y=lat, group=group))
    if(contour[1]) {
      worldmap = worldmap + geom_contour(tile.aes, 
        data = Y  %>% mutate(lon.Y = ifelse(lon.Y<=0, lon.Y, lon.Y-360)), 
        inherit.aes = F, alpha = alpha)
    } else {
      worldmap = worldmap + geom_raster(tile.aes, 
        data = Y  %>% mutate(lon.Y = ifelse(lon.Y<=0, lon.Y, lon.Y-360)), 
        inherit.aes = F, alpha = alpha)
    }
    
    worldmap = worldmap +
      fillscale +
      scale_x_continuous(trans = lon_trans()) +
      scale_y_continuous(trans = lat_trans()) +
      xlab('Longitude') +
      ylab('Latitude') 
    
    if(type %in% c('remote', 'teleconnection', 'eof', 'remote_cor') ) {
      worldmap = worldmap + geom_polygon()
    } else {
      worldmap = worldmap + geom_path()
    }
    
    # add significant overlays, if applicable
    if(signif.telecon)
      if(sum(Y$signif) > 0) {
        worldmap = worldmap + 
          geom_tile(tile.aes, data = Y %>% filter(signif==T),
                    inherit.aes = F, color='black', lwd=lwd, alpha = 1) 
      }
    
    worldmap = worldmap + 
      theme_grey() +
      ggtitle(t)
    
    if(!is.null(coords.knots)) {
      worldmap = worldmap + geom_point(aes(x=lon, y=lat), data = coords.knots,
                                       col = 'black', fill = 'black', shape=21,
                                       inherit.aes = F)
    }
  } else {
    worldmap = ggplot(world, aes(x=long, y=lat, group=group)) +
      geom_point(point.aes, data = Y  %>% 
                  mutate(lon.Y = ifelse(lon.Y<=0, lon.Y, lon.Y-360)), 
                inherit.aes = F, size=4, shape=21) +
      fillscale +
      scale_color_manual('Significance', values=c('True'='black', 'False'='grey')) +
      scale_x_continuous(trans = lon_trans()) +
      scale_y_continuous(trans = lat_trans()) +
      xlab('Longitude') +
      ylab('Latitude') + 
      geom_path() +
      theme_grey() +
      ggtitle(t)
    
    if(!is.null(coords.knots)) {
      worldmap = worldmap + geom_point(aes(x=lon, y=lat), data = coords.knots,
                                       col = 'black', fill = 'white', shape=21,
                                       inherit.aes = F)
    }
  }
  
  
  # add coord.s to the plot and modify truncation
  if(!is.null(coord.s)) {
    worldmap = worldmap + geom_point(aes(x=lon.Y, y=lat.Y), 
                                     data = data.frame(lon.Y = coord.s[1],
                                                       lat.Y = coord.s[2]),
                                     col = 'black', fill = 'white', shape=21, 
                                     inherit.aes = F)
    
    lon.E = max(lon.E, coord.s[1])
    lon.W = min(lon.W, coord.s[1])
    lat.N = max(lat.N, coord.s[2])
    lat.S = min(lat.S, coord.s[2])
  }
  
  # apply map projection and truncation
  worldmap + coord_fixed(xlim=c(lon.W, lon.E), ylim=c(lat.S, lat.N), ratio=1.3)
}