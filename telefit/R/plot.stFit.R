#' Plot stFit objects
#'
#' This function provides basic plotting for telefit package data.
#' 
#' @export
#' @method plot stFit
#' 
#' @import grid
#' @import gtable
#' @import ggplot2
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom fields rdist.earth
#' @importFrom stats runif
#' @importFrom graphics plot
#' @importFrom grDevices gray
#' 
#' @param type One of the following options to specify what type of plot to build
#'    \describe{
#'      \item{traceplot}{  }
#'      \item{density}{  }
#'      \item{pairs}{  }
#'      \item{teleconnection}{  }
#'      \item{teleconnection_local}{  }
#'      \item{teleconnection_knot}{  }
#'      \item{teleconnection_knot_transect}{  }
#'      \item{teleconnection_knot_influence}{  }
#'      \item{beta}{ }
#'    }
#' @param x Object of class stFit to plot.
#' @param coord.knot if plot type is 'teleconnection_knot_influence' or 
#' 'teleconnection_knot_local', 
#'  specifies the longitude and latitude of knot coordinate 
#'  for which to plot influence of remote coefficient on remote covariates, or
#'  the teleconnection coefficients associated with coord.knot
#' @param coord.s if plot type is 'teleconnection', specifies the longitude and 
#'  latitude of local coordinate for which to plot estimated teleconnection 
#'  effects. if NULL, the middle local coordinate will be plotted.
#' @param title.text.size number specifying the size of title
#' @param text.size number specifying the size of text labels
#' @param axis.text.size number specifying the size of axis text labels
#' @param burn number of observations to exclude from graph
#' @param stData Object of class stData to provide coordinate and related
#'  information for plotting estimated teleconnection effects
#' @param signif.telecon if TRUE, will highlight significant teleconnection
#'  effects when type=='teleconnection'
#' @param p If stFit was fit with spatially varying coefficients, p specifies 
#'  the index of the spatially varying coefficient to plot
#' @param local.covariate data.frame with variables, 'lon.Y', 'lat.Y', 'x'
#'  that will be plotted against teleconnection effects if 
#'  type=='teleconnection_knot_transect'
#' @param facet.signif number of significant figures to round facet latitudes 
#'  and longitudes for if type=='teleconnection_knot_transect'
#' @param lwd specifies linewidth for plots that include reference lines
#' @param stat.smooth.bw if type=='teleconnection_knot_transect' this specifies
#'  the bandwith of the non-parametric smooth of the estimates
#' @param stat.smooth.degree if type=='teleconnection_knot_transect' this 
#'  specifies the degree of the non-parametric smooth of the estimates
#' @param dots additional named arguments with defaults to pass to additional 
#'   functions
#' @param ... additional arguments to pass to functions
#' 
#' @return a ggplot object with the specified map
#'
#' @examples
#' 
#' data("coprecip.fit")
#' plot(coprecip.fit, burn = 50, type = 'trace')
#' 

plot.stFit = function( x, type='density', stData=NULL, coord.s=NULL, 
                       coord.knot=NULL,
                       text.size=NULL, axis.text.size=NULL, title.text.size=NULL,
                       burn = 1, signif.telecon = F, p = 1, local.covariate=NULL, 
                       lwd=NULL, facet.signif = 3, stat.smooth.bw=NULL,
                       stat.smooth.degree=NULL,
                       dots=NULL, ...) {
  stFit = x
  # merge unique list of dots
    dots = c(dots, list(...))
    dots = dots[!duplicated(dots)]
  # overwrite arguments to function if they exist in dots
    for(x in setdiff(names(formals(eval(match.call()[[1]]))), c('dots', '...'))) {
      if(x %in% names(dots)) {
        assign(eval(x), dots[[x]])
      }
    }
  
  # determine which type of plot is requested
  match.opts = c('traceplot', 'density', 'pairs', 'teleconnection', 'beta',
                 'teleconnection_knot', 'teleconnection_knot_transect',
                 'teleconnection_knot_influence', 'teleconnection_knot_local',
                 'eof_alpha')
  type = match.opts[pmatch(type, match.opts)]
  
  # extract posterior samples if necessary
  if( type %in% c('traceplot', 'density', 'pairs') ) {
    # extract posterior samples
    if(stFit$varying) {
      listInd = which(!(names(stFit$parameters$samples) %in% c('beta', 'T')))
      res.df = data.frame(stFit$parameters$samples[listInd])
    } else {
      res.df = data.frame(stFit$parameters$samples)
    }
    maxIt = nrow(res.df)
    
    # discard burned samples
    res.df = res.df[burn:maxIt,]
    res.df$Iteration = burn:maxIt
    
    # add names for the betas
    if(!is.null(stFit$parameters$beta.names)) {
      colnames(res.df)[
        1:ncol(stFit$parameters$samples$beta)] = 
          stFit$parameters$beta.names[1:ncol(stFit$parameters$samples$beta)]
    }
    
    # coerce to plottable form
    res.plottable = melt(res.df, id.vars = 'Iteration', variable.name = 'param', 
                         value.name = 'Value')
  }
  
  
  # build plots
  ret = NULL
  if( type=='traceplot' ) {
    ret = ggplot(res.plottable, aes(x=Iteration, y=Value)) +
      geom_line() +
      facet_wrap(~param, scales='free')
    
  } else if( type=='density' ) {
    ret = ggplot(res.plottable, aes(x=Value)) +
      geom_density() +
      facet_wrap(~param, scales='free') +
      ylab('Posterior density')
    
  } else if( type=='pairs' ) {
    plot(res.df)
    
  } else if( type=='teleconnection' ) {
    if(is.null(stData)) {
      stop('stData object required for plotting estimated teleconnection effects.')
    }
    
    coord.s = unlist(coord.s)
    
    stData$alpha = stFit$alpha$summary$alpha
    stData$alpha_signif = stFit$alpha$summary$signif
    ret = plot.stData(stData, type='teleconnection', lab.teleconnection = 'alpha', 
                      dots=dots, signif.telecon = signif.telecon, ...) + 
      ggtitle('Estimated teleconnection effects')
      
    
  } else if(type=='teleconnection_knot_local') {
    
    if(is.null(stData)) {
      stop('stData object required for plotting estimated teleconnection effects.')
    }
    
    stData$alpha_knots = stFit$alpha_knots$summary$alpha
    stData$alpha_knots_signif = stFit$alpha_knots$summary$signif
    stData$coords.knots = stFit$coords.knots
    
    ret = plot.stData(stData, 'teleconnection_knot_local', 
                      lab.teleconnection = expression(hat(alpha)),
                      coord.r = coord.knot, dots=dots, ...) + 
      ggtitle('Estimated teleconnection effects')
    
  } else if( type=='teleconnection_knot' ) {
    if(is.null(stData)) {
      stop('stData object required for plotting estimated teleconnection effects.')
    }
    
    stData$alpha_knots = stFit$alpha_knots$summary$alpha
    stData$alpha_knots_signif = stFit$alpha_knots$summary$signif
    stData$coords.knots = stFit$coords.knots
    
    ret = plot.stData(stData, 'teleconnection_knot', 
                      lab.teleconnection = expression(hat(alpha)), 
                      dots=dots, ...) + 
      ggtitle('Estimated teleconnection effects')
    
  } else if( type == 'teleconnection_knot_influence' ) {
    if(is.null(stData)) {
      stop('stData object required for plotting estimated teleconnection effects.')
    }
    
    coord.knot = matrix(unlist(coord.knot), nrow=1)
    
    Dz_to_knots = rdist.earth(stData$coords.r, coord.knot, miles=stFit$miles)
    
    c_full = maternArray(Dz_to_knots, scale = 1, 
                         range = mean(stFit$parameters$samples$rho_r[-(1:burn)]),
                         smoothness = stFit$priors$cov.r$smoothness,
                         nugget = 0)

    stData$coords.knots = stFit$coords.knots
    stData$Z = c_full
    stData$Z.lab = expression(italic(Cor[alpha]))
    
    ret = plot.stData(stData, 'remote', coords.knots = coord.knot,
                      dots=dots, ...) + 
      ggtitle('Remote covariate influence on knot index')
    
  } else if( type == 'teleconnection_knot_transect' ) {
    # examine teleconnection effects across a transect
    
    coord.s = unlist(coord.s)
    
    df = stFit$alpha_knots$summary %>% 
      filter(lat.Y == coord.s[2]) %>% # choose a transect latitude
      mutate(lon.Z = signif(lon.Z, 3), 
             lat.Z = signif(lat.Z, 3)) # group/tidy knot locations
    
    # Order and format latitudes and longitudes for plotting in grid
    lon.trans = lon_trans()
    lat.trans = lat_trans()
    
    df$lon.Z = factor(lon.trans$format(df$lon.Z), 
                      levels = lon.trans$format(sort(unique(df$lon.Z %% 360))))
    
    df$lat.Z = factor(lat.trans$format(df$lat.Z), 
                      levels = lat.trans$format(sort(unique(df$lat.Z), 
                                                     decreasing = T)))
    
    if(!is.null(local.covariate)) {
      df = df %>% left_join(local.covariate, by=c('lon.Y', 'lat.Y'))
    }
      
    if(is.null(lwd))
      lwd=1
    
    # build base plot
    ret = ggplot(df, aes(x = lon.Y, y = alpha)) +
      # teleconnection effect CI
      geom_ribbon(aes(ymin = lower, ymax=upper), fill = 'grey70')
    if(is.null(stat.smooth.bw)) {
      ret = ret +
        # teleconnection effects with CI
        geom_line(lwd=lwd)
    } else {
      ret = ret + 
        # smoothed teleconnection effects with CI
        stat_smooth(span=stat.smooth.bw, se=F, col=1, method='loess',
                    method.args=list(degree=stat.smooth.degree))
    }
    ret = ret + 
      # reference line for significance
      geom_hline(yintercept = 0, lty=2, alpha=.6, lwd=lwd ) +
      # plot teleconnection effect transect for all knots
      facet_grid(lat.Z ~ lon.Z) +
      ylab('Teleconnection effect') +
      scale_x_continuous('Transect longitude', trans = lon_trans()) +
      ggtitle(paste('Teleconnection effects along', lat.trans$format(coord.s[2]),
                    'transect'))
    
    # change text size
    if(!is.null(text.size))
      ret = ret + theme( text = element_text(size=text.size))
    if(!is.null(axis.text.size))
      ret = ret + theme( axis.text = element_text(size=axis.text.size))
    if(!is.null(title.text.size))
      ret = ret + theme( plot.title = element_text(size=title.text.size))
    
    # add covariate information
    if(!is.null(local.covariate)) {
      ret = ret + geom_line(aes(x=lon.Y, y=x), col=2, alpha=.7, lwd=lwd)
    }
      
    #
    # add facet labels
    #
    
    # get gtable object
    ret.grob = ggplotGrob(ret)
    
    # text and background config for the labels
    label.bg = rectGrob(gp = gpar(col = NA, fill = gray(6/8)))
    label.textgp = gpar(fontsize=ifelse(axis.text.size, axis.text.size, 14), 
                        col = gray(.1))
    
    # add label for right strip
    right.pos = max(ret.grob$layout[which(ret.grob$layout$name=='strip-right'),]$r)
    ylab.range = ret.grob$layout[which(ret.grob$layout$name=='ylab'),c('t','b')]
    ret.grob = gtable_add_cols(ret.grob, ret.grob$widths[right.pos], right.pos)
    ret.grob = gtable_add_grob(ret.grob, 
                  list(label.bg, 
                       textGrob("Teleconnection latitude", rot = -90, 
                                gp = label.textgp)),
                  ylab.range$t, right.pos+1, ylab.range$b, right.pos+1, 
                  name = paste(runif(2)))
    
    # add label for top strip
    top.pos = min(ret.grob$layout[which(ret.grob$layout$name=='strip-top'),]$t)
    xlab.range = ret.grob$layout[which(ret.grob$layout$name=='xlab'),c('l','r')]
    ret.grob = gtable_add_rows(ret.grob, ret.grob$heights[top.pos], top.pos-1)
    ret.grob = gtable_add_grob(ret.grob, 
                    list(label.bg, 
                         textGrob("Teleconnection longitude", gp = label.textgp)),
                  top.pos, xlab.range$l, top.pos, xlab.range$r, 
                  name = paste(runif(2)))
    
    # add space between outer and inner labels
    ret.grob = gtable_add_cols(ret.grob, unit(1/8, "line"), right.pos)
    ret.grob = gtable_add_rows(ret.grob, unit(1/8, "line"), top.pos)
    
    # draw image
    grid.newpage()
    grid.draw(ret.grob)
    
    # return nothing
    ret = NULL
    
  } else if( type=='beta' ) {
    if(is.null(stData)) {
      stop('stData object required for plotting estimated spatially varying coefficients.')
    }
    
    # extract coefficient estimates
    betaH = colMeans(stFit$parameters$samples$beta[-(1:burn),
        seq(from = p, to = prod(dim(stData$X)[1:2]), by = ncol(stData$X) )])
    
    # put estimates into plottable structure
    stData$Y[,1] = betaH
    stData$Y.lab = 'Coefficient'
    
    # build plot
    ret = plot.stData(stData, 'response') + 
      ggtitle('Estimated spatially varying coefficient')
    
  }
  
  
  # modify text sizes if requested
  if(!is.null(text.size))
    ret = ret + theme( text = element_text(size=text.size))
  if(!is.null(axis.text.size))
    ret = ret + theme( axis.text = element_text(size=axis.text.size))

  # return plot
  if(!is.null(ret))
    ret
}