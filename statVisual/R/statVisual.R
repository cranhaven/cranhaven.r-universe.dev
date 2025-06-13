#v1.1.8 created on June 02, 2019
#  (1) For functions LinePlot, ErrBar, and barPlot, added an input parameter 'xFlag' to indicate if x-axis variable should be
#  regarded as a continuous variable ('xFlag = TRUE') or a factor ('xFlag = FALSE')
#
#v1.1.2 created on May 3, 2019
#  (1) force 'group' to be factor in Hist, Den, Dendro
#  (2) fixed a bug in 'Box': when x=NULL, xLevel was not assigned correctly
#
#v1.1.0 created on April 27, 2019
#  (1) added input parameter 'xLevel' to functions 'LinePlot', 'Box', 'ErrBar'
#  (2) set 'jitter.width = 0.2' in 'Box'
#  (3) added function 'addTheme'
#  (4) revised functions by adding background and grid through input 
#      parameter 'addThemeFlag'
#
#v1.0.9 created on April 10, 2019
#  (1) modify XYscatter. all plots have the same x-axis and y-axis range
#  (2) set delta = 0.2 (instead of 0.5) in BiAxisErrBar
#
#v1.0.8 created on April 5, 2019
#  (1) fixed a bug in 'BiAxisErrBar': no line connect means
#v1.0.7 created on April 4, 2019
#  (1) before set 'x' or 'group' as factor, first test if they are already factor
# modifed on April 3 to April 4, 2019
#  (1) fixed a bug in 'Box' function: added "+ scale_x_discrete(drop = F) + scale_fill_discrete(drop = F) +"
#  (2) set default value for jitter.width=1 and point.size = 1
#  (3) in 'PCA_score', add the input 'title'
#  (4) in 'PCA_score', if 'color' not NULL, then set data[, c(color)]=as.factor(data[, c(color)])
#  (5) added "+ xlab(label=xlab) + ylab(label=ylab)" to functions 'BoxROC', 
#      'XYscatter', 'PVCA', 'ImpPlot'
#  (6) fixed a bug in 'XYscatter', 'Hist', and 'Den': when 'group=NULL', there is an error
#  (7) in 'Heat', replace 'heatmap.2' by 'pheatmap'
#  (8) in 'LinePlot' and 'Box', set 'x' and 'group' as factor
#  (9) in 'BiAxisErrBar', set 'x' as factor

 
# modifed on Feb. 12, 2019
#  (1) fixed a bug in 'Den' function. The default value of 'ylab' should be 'density', not 'count'
#  (2) fixed a few bugs in plot functions to call ylab: Instead of using 'labs(x=xlab, y=ylab, title=title)', use 'labs(title=title)+xlab(label=xlab)+ylab(label=ylab)
#

# modifed on Feb. 7, 2019
#  (1) add the function iprcomp and update example in PCA_score.Rd
#  (2) use more meaningful default figure titles for functions 'Hist', 'Box', 'Den', 'ErrBar',
#  'LinePlot', 'XYscatter'
#  (3) add input 'semFlag' to function 'ErrBar' to indicate using se (semFlag = FALSE) or sem (semFlag=TRUE) in error bar
#
# modified on Dec. 5, 2018
#  (1) added "+ xlab(label=xlab) + ylab(label=ylab)" to functions 'Hist' and 'Den'

# modified on Nov. 30, 2018
#  (1) added input parameter 'dim' to pca functions so that can the user
#      can choose any two dimnesion to draw scatter plot; 
#      Also added percent of explained variance to xlab and ylab
#  (2) commented out the function PCA_scree since it is essentially the same
#      as 'fviz_eig' function in R package 'factoextra'
#  (3) revised the format of the input 'gene_data' in the function 'PVCA':
#      now the rows of 'gene_data' are genes and the columns are subjects.
#      Also added ", drop=FALSE" to account for the scenario where there
#      is only one factor
#  (4) added library 'golubEsets'
#  (5) revised the input parameters of the function 'Volcano': 
#      replace 'data' by 'resFrame';
#      replace 'odds.ratio' by 'stats';
#  (6) remove unused comments and R code

#
# modified on Oct. 22, 2018
#  (1) remove 'KM'
# modified on Oct. 17, 2018
#  (1) rename 'visual' to 'statVisual'
# modified on Oct. 16, 2018
#  (1) deleted unused functions
#  (2) replace 'T' by 'TRUE'
#  (3) replace 'F' by 'FALSE'
#
# modified on Oct. 15, 2018
#  (1) rename 'Graph' function to 'visual'
#
# modified on Oct. 11, 2018
#
#
#############################################
# This Graphical tool includes:             #
#     Scatter Plot                          #
#     Correlation Plot                      #
#     Boxplot                               #
#     Error Bar Plot                        #
#     Line Plot                             #
#     Histogram                             #
#     Density Plot                          #
#     Dendrogram                            #
#     Volcano Plot                          #
#     Boxplot with ROC curve                #
#############################################

# general graph function ####
statVisual = function(type, ...) {
  switch(type,
    barPlot = barPlot(...),
    BiAxisErrBar = BiAxisErrBar(...),
    Box = Box(...),
    BoxROC = BoxROC(...),
    cv_glmnet_plot = cv_glmnet_plot(...),
    Den = Den(...),
    Dendro = Dendro(...),
    ErrBar = ErrBar(...),
    Heat = Heat(...),
    Hist = Hist(...),
    ImpPlot = ImpPlot(...),
    LinePlot = LinePlot(...),
    PCA_score = PCA_score(...),
    PVCA = PVCA(...),
    stackedBarPlot = stackedBarPlot(...),
    Volcano = Volcano(...),
    XYscatter=XYscatter(...)
  )
}

# general ggplot layers ####
# facet layer
FACET = function(g, 
                 facet.var = NULL, facet.scales = 'fixed', facet.dir = 'h', 
                 facet.nrow = NULL, facet.ncol = NULL) {
  g = g + facet_wrap(as.formula(paste0('~', facet.var)), 
                 scales = facet.scales, 
                 dir = facet.dir, 
                 nrow = facet.nrow, 
                 ncol = facet.ncol)
  g
}




# theme layer
THEME = function(g, 
                 axis.text.x.angle = NULL, 
                 axis.text.x.hjust = NULL, 
                 axis.text.x.vjust = NULL, 
                 axis.text.x.size = NULL, 
                 axis.text.y.angle = NULL, 
                 axis.text.y.hjust = NULL, 
                 axis.text.y.vjust = NULL, 
                 axis.text.y.size = NULL, 
                 axis.title.x.angle = NULL, 
                 axis.title.x.hjust = NULL, 
                 axis.title.x.vjust = NULL, 
                 axis.title.x.size = NULL, 
                 axis.title.y.angle = NULL, 
                 axis.title.y.hjust = NULL, 
                 axis.title.y.vjust = NULL, 
                 axis.title.y.size = NULL, 
                 plot.title.size = NULL, 
                 plot.title.hjust = 0.5) {
  g = g + theme(axis.text.x = element_text(angle = axis.text.x.angle,
                                       hjust = axis.text.x.hjust,
                                       vjust = axis.text.x.vjust,
                                       size = axis.text.x.size),
            axis.text.y = element_text(angle = axis.text.y.angle,
                                       hjust = axis.text.y.hjust,
                                       vjust = axis.text.y.vjust,
                                       size = axis.text.y.size),
            axis.title.x = element_text(angle = axis.title.x.angle,
                                        hjust = axis.title.x.hjust,
                                        vjust = axis.title.x.vjust,
                                        size = axis.title.x.size),
            axis.title.y = element_text(angle = axis.title.y.angle,
                                        hjust = axis.title.y.hjust,
                                        vjust = axis.title.y.vjust,
                                        size = axis.title.y.size), 
            plot.title = element_text(size = plot.title.size, 
                                      hjust = plot.title.hjust), 
            strip.background = element_rect(color = NA))

  g
}



# Scatter Plot ####
# regular x-y scatterplot:
XYscatter = function(data, x, y, group = NULL, 
		     alpha = 1, 
		     point.size = 3, 
                     xlab = x, ylab = y, group.lab = group, 
		     title = "Scatter plot", 
                     theme_classic = TRUE, addThemeFlag = TRUE, ...) {
  # scatterplot x vs y

  myx=data[, c(x)]
  myy=data[, c(y)]
  
  myxlim=range(myx, na.rm=TRUE)
  myylim=range(myy, na.rm=TRUE)


  if(!is.null(group))
  {
    # QWL: update facet.var and facet.scales
    dots = list(...)
    if( !("facet.var" %in% names(dots)) )
    {
      facet.var = group
      dots = c(facet.var=facet.var, dots)
    }
    if( !("facet.scales" %in% names(dots)) )
    {
      facet.scales = 'free'
      dots = c(facet.scales=facet.scales, dots)
    }
    
    # data: data includes x, y columns for plot at least
    # x: variable on x axis
    # y: variable on y axis
    # group: grouping variable
    
    g = ggplot(data, aes_string(x = x, y = y, color = group)) +
      geom_point(alpha = alpha, size = point.size) + 
      labs(color = group.lab, title = title)+
      xlab(label = xlab) + ylab(label = ylab) + xlim(myxlim) + ylim(myylim)
  } else {
    # QWL: update facet.var and facet.scales
    dots = list(...)
   
    # data: data includes x, y columns for plot at least
    # x: variable on x axis
    # y: variable on y axis
    # group: grouping variable
    
    g = ggplot(data, aes_string(x = x, y = y)) +
      geom_point(alpha = alpha, size = point.size) + 
      labs(title = title)+
      xlab(label = xlab) + ylab(label = ylab) + xlim(myxlim) + ylim(myylim)

  }

  # add facet & theme arguments from ...
  #dots = list(...)
  FACET_args_names = FACET %>% formals %>% names
  FACET_args = dots[names(dots) %in% FACET_args_names]
  THEME_args_names = THEME %>% formals %>% names
  THEME_args = dots[names(dots) %in% THEME_args_names]
  
  # add facet
  if (length(FACET_args) != 0) {
    g = do.call(FACET, c(list(g = g), FACET_args))
  }
  # axis label position
  g = do.call(THEME, c(list(g = g), THEME_args))
  
  # classical theme
  if (theme_classic) {
    g = g + theme_classic()
  }

  if(addThemeFlag)
  {
    g=addTheme(g)
  }

  # return plot
  g
}

# Box Plot ####
Box=function (data, x = NULL, y, group = NULL, fill = NULL, theme_classic = TRUE, 
          fill.alpha = 0.7, box.width = 0.5, dodge.width = 0.8, jitter = TRUE, 
          jitter.alpha = 0.7, jitter.width = 0.2, point.size = 1, 
          xlab = x, ylab = y, group.lab = group, fill.lab = group, 
          title = "Boxplot", line = "mean", line.color = "black", 
	  xLevel = NULL, addThemeFlag = TRUE,
          ...) 
{
  
  
  # myx=paste0('`', j, '`')
  # mycol=paste0('`', j, '`')
  # g = ggplot(dat, aes_string(x = mycol, y = i, color = mycol)) +
  #   geom_boxplot(outlier.size = -1) +
  #   geom_jitter(width = 0.2, height = 0) +
  #   scale_x_discrete(drop = F) +
  #   scale_fill_discrete(drop = F) +
  #   labs(title = paste('p-value =', pval), color = j, x = NULL)

#  if(!is.null(x) && !is.null(xLevel))
#  {
#    data[, c(x)] = factor(data[, c(x)],
#                       levels = xLevel, 
#                       ordered = TRUE)
#  }
#


  if (is.null(x)) {
    if (!is.null(group)) 
      x = group
    if(!is.null(xLevel))
    {
      data[, c(x)] = factor(data[, c(x)],
                       levels = xLevel, 
                       ordered = TRUE)
    }

    if(!is.factor(data[, c(x)]))
    {
      data[,c(x)]=factor(data[, c(x)])
    }
    if(!is.factor(data[,c(group)]))
    {
      data[,c(group)]=factor(data[, c(group)])
    }

    g = ggplot(data, aes_string(x=x, y=y, color=group, fill=fill)) + 
      geom_boxplot(outlier.size = ifelse(jitter, 1.5, 0), width = box.width, alpha = fill.alpha) +
      scale_x_discrete(drop = F) +
      scale_fill_discrete(drop = F) +
      labs(title = title, color = group.lab) + xlab(label = xlab) + ylab(label = ylab)
      
  }
  else {
 
    if(!is.factor(data[, c(x)]))
    {
      data[, c(x)]=factor(data[, c(x)])
      if(!is.null(xLevel))
      {
        data[, c(x)] = factor(data[, c(x)],
                           levels = xLevel, 
                           ordered = TRUE)
      }
    } 

    if(!is.factor(data[,c(group)]))
    {
      data[,c(group)]=factor(data[, c(group)])
    }

    g = ggplot(data, aes_string(x, y)) + 
	       geom_boxplot(aes_string(color = group, fill = fill), 
			    outlier.size = ifelse(jitter, 1.5, 0), 
	                    width = box.width, position = position_dodge(width = dodge.width), 
                            alpha = fill.alpha) + 
               labs(title = title, color = group.lab, fill = fill.lab) + 
	       xlab(label = xlab) + 
	       ylab(label = ylab)
  }
  if (jitter) {
    if (!is.null(group) | !is.null(fill)) {
     g = g + geom_jitter(aes_string(color = group), 
			 position = position_jitterdodge(dodge.width = dodge.width, jitter.width = jitter.width), 
			 alpha = jitter.alpha, size = point.size)
    }
    else {
      g = g + geom_jitter(aes_string(color = group), width = jitter.width,
                          alpha = jitter.alpha, size = point.size)
    }
  }
  if (is.null(x) & is.null(group)) 
    line = NULL
  if (!is.null(line)) {
    if (!is.null(group)) {
      if (group != x) {
        g = g + stat_summary(fun.y = line, geom = "line", 
                             aes_string(group = group, color = group), 
                             position = position_dodge(dodge.width))
      }
      else {
        g = g + stat_summary(fun.y = line, geom = "line", 
                             aes_string(group = 1), color = line.color)
      }
    }
    else {
      g = g + stat_summary(fun.y = line, geom = "line", 
                           aes_string(group = 1), color = line.color)
    }
  }
  if (theme_classic) {
    g = g + theme_classic()
  }
  dots = list(...)
  FACET_args_names = FACET %>% formals %>% names
  FACET_args = dots[names(dots) %in% FACET_args_names]
  THEME_args_names = THEME %>% formals %>% names
  THEME_args = dots[names(dots) %in% THEME_args_names]
  if (length(FACET_args) != 0) {
    g = do.call(FACET, c(list(g = g), FACET_args))
  }
  g = do.call(THEME, c(list(g = g), THEME_args))

  if(addThemeFlag)
  {
    g = addTheme(g)
  }

  g
}

# Error Bar Plot ####

ErrBar = function(data, x = NULL, y, group = NULL, 
		  semFlag = TRUE,
		  xFlag = FALSE,
		  bar.width = 0.5, dodge.width = 0.8, 
                  jitter = TRUE, jitter.alpha = 0.7, jitter.width = 0.1, 
                  line = 'mean', line.color = 'black', 
                  xlab = x, ylab = line, theme_classic = TRUE, 
                  group.lab = group, title = "Dot plots", 
		  xLevel = NULL, addThemeFlag = TRUE, ...) {
  # main function of error bar
  
  # x: variable on x axis
  # y: variable on y axis
  # group: error bar color
  # bar.width: error bar width
  # dodge.width: dodge width for error bar and jitter (prevent overlapping)
  # jitter: logical, plot jitter or not, default TRUE
  # jitter.alpha: jitter transparency
  # jitter.width: jitter width in error bar
  # line: line connect error bar, default uses mean, can be set as 'median', NULL (no line)
  # line.color: connection line color, only available when group = NULL
  # xlab: x axis label
  # ylab: y axis label
  # theme_classic: Use classic background without grids (default: FALSE)
  # group.lab: label of group variable
  # title: title of plot
  
  if(xFlag == FALSE && !is.factor(data[, c(x)]))
  {
    data[, c(x)]=factor(data[, c(x)])
    if(!is.null(x) && !is.null(xLevel))
    {
      data[, c(x)] = factor(data[, c(x)],
                         levels = xLevel, 
                         ordered = TRUE)
    }
  } else {
    data[, c(x)]=as.numeric(data[, c(x)])
  }
 

  mu = NULL
  myse = NULL

  # standard error function
  se = function(s, na.rm = TRUE, semFlag = TRUE) {
    if (na.rm) {
      s = s[!is.na(s)]
    }
    res = sd(s)
    if(semFlag)
    {
      res = res/sqrt(length(s))
    }
    return(res)
  }

  # calculate mean and standard error by group
  if (!is.null(group) & !is.null(x)) {
    data.mu.se = data %>% group_by_(group, x)
  } else if (!is.null(group) & is.null(x)) {
    data.mu.se = data %>% group_by_(group)
  } else if (is.null(group) & !is.null(x)) {
    data.mu.se = data %>% group_by_(x)
  } else {
    data.mu.se = data
  }
  # QWL: rename 'se' as 'myse' to distinguish it with 'se' function 
  #      defined previously
  data.mu.se = data.mu.se %>%
    summarise_(mu = paste0('mean(', y, ', na.rm = TRUE)'), 
               myse = paste0('se(', y, ', na.rm = TRUE', ', semFlag=', semFlag, ")"))

  # merge mu/se summary data with original data to plot error bar with jitter
  # if (!is.null(group)) {
  data.plot = full_join(data.mu.se, data)
  # } else {
  #     data.plot = data.frame(data, data.mu.se)
  # }
  
  # create error bar layer
  if (is.null(x)) {
    if (!is.null(group)) {
      x = group

      g = ggplot(data.plot, aes_string(x = ifelse(is.null(x), 1, x), y, color = group)) +
        geom_errorbar(
          aes(ymin = mu - myse, ymax = mu + myse),
          width = bar.width
        ) +
        labs(title = title, color = group.lab) + xlab(label=xlab) + ylab(label=ylab)
        #labs(x = xlab, y = ylab, title = title)
    }
  } else {
    g = ggplot(data.plot, aes_string(x, y, color = group)) +
      geom_errorbar(
        aes(ymin = mu - myse, ymax = mu + myse),
        position = position_dodge(width = dodge.width), 
        width = bar.width
      ) +
      labs(title = title, color = group.lab) + xlab(label=xlab) + ylab(label=ylab)
      #labs(x = xlab, y = ylab, color = group.lab, title = title)
  }
  
  # add jitter layer
  if (jitter) {
    if (!is.null(group)) {
      g = g + geom_jitter(
        aes_string(color = group),
        position = position_jitterdodge( # set up dodge position for jitters
          dodge.width = dodge.width, # dodge width between jitters
          jitter.width = jitter.width # jitter width in error bar
        ),
        alpha = jitter.alpha
      )
    } else {
      g = g + geom_jitter(
        aes_string(color = group),
        width = jitter.width, # jitter width in error bar plot
        alpha = jitter.alpha
      )
    }
  }
  
  # add connect line
  if (is.null(x) & is.null(group)) line = NULL
  if (!is.null(line)) {
    if (!is.null(group)) {
      if (group != x) {
        g = g + stat_summary(fun.y = line, geom = 'line',
                             aes_string(group = group, color = group),
                             position = position_dodge(dodge.width))
      } else {
        g = g + stat_summary(fun.y = line, geom = 'line',
                             aes_string(group = 1), color = line.color)
      }
    } else {
      g = g + stat_summary(fun.y = line, geom = 'line',
                           aes_string(group = 1), color = line.color)
    }
  }
  
  # classical theme
  if (theme_classic) {
    g = g + theme_classic()
  }
  # add facet & theme arguments from ...
  dots = list(...)
  FACET_args_names = FACET %>% formals %>% names
  FACET_args = dots[names(dots) %in% FACET_args_names]
  THEME_args_names = THEME %>% formals %>% names
  THEME_args = dots[names(dots) %in% THEME_args_names]
  
  # add facet
  if (length(FACET_args) != 0) {
    g = do.call(FACET, c(list(g = g), FACET_args))
  }
  # axis label position
  g = do.call(THEME, c(list(g = g), THEME_args))

  if(addThemeFlag)
  {
    g = addTheme(g)
  }
  
  # return plot
  g
}


# line plot ####
LinePlot = function(data, x, y, sid, group = NULL, 
		    xFlag = FALSE,
                    points = TRUE, point.size = 1, theme_classic = TRUE, 
                    xlab = x, ylab = y, title = "Trajectory plot", 
		    xLevel = NULL, addThemeFlag = TRUE, ...) {
  # main function of line plot
  
  # x: variable on x axis
  # y: variable on y axis
  # xFlag = TRUE indicates 'x' is a continuous variable; otherwise, 'x' will be forced to be a factor
  # sid: subject id
  # group: varaible to group different lines
  # xlab: x axis label
  # ylab: y axis label
  # title: title of plot

  if(xFlag == FALSE && !is.factor(data[, c(x)]))
  {
    data[, c(x)]=factor(data[, c(x)])
    if(!is.null(xLevel))
    {
      data[, c(x)] = factor(data[, c(x)],
                         levels = xLevel, 
                         ordered = TRUE)
    }
  } else {
    data[, c(x)]=as.numeric(data[, c(x)])
  }
  
  if(!is.factor(data[, c(group)]))
  {
    data[, c(group)]=factor(data[, c(group)])
  }

  # QWL: added the following call of dots
  dots = list(...)
  if( !("facet.var" %in% names(dots)) )
  {
    facet.var = group
    dots = c(facet.var=facet.var, dots)
  }
  if( !("facet.nrow" %in% names(dots)) )
  {
    facet.nrow = 2
    dots = c(facet.nrow=facet.nrow, dots)
  }

  # default plot
  g = ggplot(data = data, aes_string(x = x, y = y, color = group, sid=sid)) + 
    geom_path(aes_string(group = sid)) +
    labs(title = title) + xlab(label=xlab) + ylab(label=ylab)
    #labs(x = xlab, y = ylab, title = title)
  # add point
  if (points) {
    g = g + geom_point(size = point.size)
  }
  # classical theme
  if (theme_classic) {
    g = g + theme_classic()
  }
  
  # add facet & theme arguments from ...
  #dots = list(...)
  FACET_args_names = FACET %>% formals %>% names
  FACET_args = dots[names(dots) %in% FACET_args_names]
  THEME_args_names = THEME %>% formals %>% names
  THEME_args = dots[names(dots) %in% THEME_args_names]
  
  # add facet
  if (length(FACET_args) != 0) {
    g = do.call(FACET, c(list(g = g), FACET_args))
  }
  # axis label position
  g = do.call(THEME, c(list(g = g), THEME_args))
  
  if(addThemeFlag)
  {
    g = addTheme(g)
  }
 
  # return plot
  g
}


# Histogram ####
Hist = function(data, y, group = NULL, fill = group, 
                border.color = NULL, inner.color = NULL, theme_classic = TRUE, 
                bins = NULL, binwidth = NULL, alpha = 0.8, 
                xlab = y, ylab = 'count', 
		group.lab = group, 
		title = "Histogram", 
		addThemeFlag = TRUE,
		...) {
  # main function of histogram
  
  # group: grouping variable, histogram border color
  # fill: grouping variable, histogram inside color
  # facet.var: by variable which separate histogram solely (default: NULL)
  # facet.horiz: facet histograms horizontally (default: TRUE)
  # scales: facet histograms scales (default: 'fixed', options: 'free_x', 'free_y', 'free')
  # border.color: histogram border color, only available when group & fill are NULL
  # inner.color: histogram inside color, only available when group & fill are NULL
  # theme_classic: Use classic background without grids (default: FALSE)
  # bins: number of bins of histogram (default: 30)
  # binwidth: bin width of histogram
  # alpha: transparency of histogram inside color
  # xlab: x axis label
  # ylab: y axis label
  # group.lab: label of group variable
  # title: title of plot

  if(!is.null(group))
  {
    data[, c(group)]=factor(data[, c(group)]) 

    # QWL: add the following call of dots
    dots = list(...)
    if( !("facet.var" %in% names(dots)) )
    {
      facet.var = group
      dots = c(facet.var=facet.var, dots)
    }
    if( !("facet.nrow" %in% names(dots)) )
    {
      facet.nrow = 2
      dots = c(facet.nrow=facet.nrow, dots)
    }
    
    if (is.null(bins) & is.null(binwidth)) {
      bins = 30
    }
    g = ggplot(data, aes_string(y)) + 
      geom_histogram(
        aes_string(color = group, fill = fill),  
        bins = bins, binwidth = binwidth, alpha = alpha) + 
      labs(title = title, color = group.lab) + xlab(label=xlab) + ylab(label=ylab)
  } else {
    # QWL: add the following call of dots
    dots = list(...)
   
    if (is.null(bins) & is.null(binwidth)) {
      bins = 30
    }
    if (is.null(fill)) {
      g = ggplot(data, aes_string(y)) + 
        geom_histogram(
          bins = bins, binwidth = binwidth, alpha = alpha, 
          color = ifelse(is.null(border.color), 'black', border.color), 
          fill = ifelse(is.null(inner.color), 'white', inner.color)
        ) + 
        labs(title = title) + xlab(label=xlab) + ylab(label=ylab)
    } else {
      g = ggplot(data, aes_string(y)) + 
        geom_histogram(
          aes_string(fill = fill),  
          bins = bins, binwidth = binwidth, alpha = alpha) + 
        labs(title = title) + xlab(label=xlab) + ylab(label=ylab)
    }

  }

  # classical theme
  if (theme_classic) {
    g = g + theme_classic()
  }
  # add facet & theme arguments from ...
  #dots = list(...)
  FACET_args_names = FACET %>% formals %>% names
  FACET_args = dots[names(dots) %in% FACET_args_names]
  THEME_args_names = THEME %>% formals %>% names
  THEME_args = dots[names(dots) %in% THEME_args_names]
  
  # add facet
  if (length(FACET_args) != 0) {
    g = do.call(FACET, c(list(g = g), FACET_args))
  }
  # axis label position
  g = do.call(THEME, c(list(g = g), THEME_args))

  if(addThemeFlag)
  {
    g = addTheme(g)
  }
 
  
  # return plot
  g
}



# Density Plot ####
Den = function(data, y, group = NULL, fill = group, 
               border.color = NULL, inner.color = NULL, theme_classic = TRUE, 
               xlab = y, ylab = 'density', group.lab = group, title = "Density plot",
               alpha = 0.3, 
	       addThemeFlag = TRUE,
	       ...) {
  # main function of density
  
  # group: grouping variable, density border color
  # fill: grouping variable, density inside color
  # facet.var: by variable which separate density solely (default: NULL)
  # facet.horiz: facet densitys horizontally (default: TRUE)
  # scales: facet densitys scales (default: 'fixed', options: 'free_x', 'free_y', 'free')
  # border.color: density border color, only available when group & fill are NULL
  # inner.color: density inside color, only available when group & fill are NULL
  # theme_classic: Use classic background without grids (default: FALSE)
  # alpha: transparency of density inside color
  # xlab: x axis label
  # ylab: y axis label
  # group.lab: label of group variable
  # title: title of plot

  if(!is.null(group))
  {
    data[, c(group)]=factor(data[, c(group)]) 

    # QWL: add the following call of dots
    dots = list(...)
    if( !("facet.var" %in% names(dots)) )
    {
      facet.var = group
      dots = c(facet.var=facet.var, dots)
    }
    if( !("facet.nrow" %in% names(dots)) )
    {
      facet.nrow = 2
      dots = c(facet.nrow=facet.nrow, dots)
    }
    g = ggplot(data, aes_string(y)) + 
      geom_density(
        aes_string(color = group, fill = fill), 
        alpha = alpha
      ) + 
      labs(title = title, color = group.lab) + xlab(label=xlab) + ylab(label=ylab)
  } else {
    # QWL: add the following call of dots
    dots = list(...)
   
    if (is.null(fill)) {
      g = ggplot(data, aes_string(y)) + 
        geom_density(alpha = alpha, 
                     color = ifelse(is.null(border.color), 'black', border.color), 
                     fill = ifelse(is.null(inner.color), 'white', inner.color)
        ) + 
        labs(title = title) + xlab(label=xlab) + ylab(label=ylab)
  
    } else {
      g = ggplot(data, aes_string(y)) + 
        geom_density(
          aes_string(fill = fill), 
          alpha = alpha
        ) + 
        labs(title = title) + xlab(label=xlab) + ylab(label=ylab)
  
    }

  }

  # classical theme
  if (theme_classic) {
    g = g + theme_classic()
  }
  
  # add facet & theme arguments from ...
  #dots = list(...)
  FACET_args_names = FACET %>% formals %>% names
  FACET_args = dots[names(dots) %in% FACET_args_names]
  THEME_args_names = THEME %>% formals %>% names
  THEME_args = dots[names(dots) %in% THEME_args_names]
  
  # add facet
  if (length(FACET_args) != 0) {
    g = do.call(FACET, c(list(g = g), FACET_args))
  }
  # axis label position
  g = do.call(THEME, c(list(g = g), THEME_args))

  if(addThemeFlag)
  {
    g = addTheme(g)
  }
 
  
  # return plot
  g
}


# gap boxplot ####
GapBox = function(data, x = NULL, y, gap, ratio = c(7, 1, 2)) {
  # main function for broken y axis boxplot
  
  # gap: 2 element numeric vector to specify gap range
  # ratio: y axis display ratio, eg(7, 1, 2) will give 7 for lower boxplot part, 
  #     1 for gap, 2 for upper jitter part
  
  # calculate transformed value range for boxplot
  min.y = min(data[, y], na.rm = TRUE)
  max.y = max(data[, y], na.rm = TRUE)
  a = min(gap)
  b = max(gap)
  alpha = ratio[1]/(a - min.y)
  charlie = ratio[3]/(max.y - b)
  y.new.1 = (data[data[, y] <= a, y] - min.y) * alpha
  y.new.3 = (data[data[, y] > b, y]) * charlie + (ratio[1] + ratio[2] - b* charlie)
  y.new = c(y.new.1, y.new.3)
  data[, paste0(y, '.new')] = y.new
  
  y.breaks = c(0, 
               max(y.new.1), 
               ratio[1], 
               ratio[1] + ratio[2], 
               max(y.new.3))
  y.labels = c(min(data[, y]) %>% signif(2), 
               max(data[data[, y] <= a, y]) %>% signif(2), 
               a, 
               b, 
               max(data[data[, y] > b, y]) %>% signif(2)) 
  
  if (!is.null(x)) {
    g = ggplot(data = data %>% filter_(paste0(y, '.new<min(y.new.3)')), aes_string(x, paste0(y, '.new'))) + 
      geom_boxplot(outlier.size = -1) + 
      geom_jitter() +
      geom_jitter(data = data %>% filter_(paste0(y, '.new>=min(y.new.3)')), 
                  aes_string(x, paste0(y, '.new'))) + 
      geom_rect(aes(xmin = 0, xmax = length(unique(data[, x])) + 1, 
                    ymin = ratio[1], ymax = ratio[1] + ratio[2]), fill = 'white') + 
      scale_y_continuous(breaks = y.breaks, labels = y.labels)
  }
  g
}

## QWL: commented out the test code; will put it to the file 'GapBox.Rd'
# set.seed(1)
# gap.test = data.frame(a = c(rnorm(100, 10, 5), rnorm(5, 100, 3)), b = sample(c('A', 'B'), size = 105, replace = T))
# gap = c(25, 95)
# ggplot(data = gap.test, aes(x = 1, y = a)) + 
#   geom_boxplot() + 
#   scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 90, 100)) + 
#   geom_rect(aes(xmin = 0, xmax = 2, ymin = 25, ymax = 90), fill = 'white')
# 
# gap.test = data.frame(a = c(rnorm(100, 10, 5), rnorm(5, 100, 3)))


# dendrogram ####
Dendro = function(x, group = NULL, xlab = NULL, ylab = NULL, title = NULL, 
                  cor.use = 'pairwise.complete.obs', cor.method = 'pearson', 
                  distance = 'rawdata', distance.method = 'euclidean', 
                  hclust.method = 'complete', yintercept = NULL, 
                  theme_classic = TRUE, addThemeFlag = TRUE, ...) {
  # main function of dendrogram
  # QWL: commented out 'require'
  #require(dplyr)
  #require(ggdendro)
  
  # cor.use: data use to compute correlation coefficients
  #     'everything', 'all.obs', 'complete.obs', 'na.or.complete', 'pairwise.complete.obs'
  # cor.method: type of correlation coefficients
  #     'pearson', 'kendall', 'spearman'
  # distance: distance used in calculate dist
  #     'rawdata': use raw data to calculate distance
  #     'cor': use correlation coefficients as distance
  #     '1-cor': use (1-correlation coefficients) as distance
  #     '1-|cor|': use (1-|correlation coefficients|) as distance
  # distance.method: distance measurement (available when distance == 'rawdata')
  #     "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
  # hclust.method: agglomeration method for hierarchical cluastering
  #     for details check ?hclust
  # yintercept: vertical line

  if(!is.null(group))
  {
    group=as.factor(group)
  }
  
  # calculate distance
  y = NULL
  xend = NULL
  yend = NULL
  as.dendrogram = NULL
  dendro_data = NULL
  if (distance == 'rawdata') {
    den_data = x %>% as.matrix %>%
      dist(method = distance.method) %>% 
      hclust(method = hclust.method) %>% 
      as.dendrogram %>% dendro_data
  } else if (distance == 'cor') {
    den_data = x %>% as.matrix %>%
      t %>% cor(use = cor.use, method = cor.method) %>%
      as.dist %>% 
      hclust(method = hclust.method) %>% 
      as.dendrogram %>% dendro_data
  } else if (distance == '1-cor') {
    den_data = (1 - (x %>% as.matrix %>% t %>% cor(use = cor.use, method = cor.method))) %>%
      as.dist %>% 
      hclust(method = hclust.method) %>% 
      as.dendrogram %>% dendro_data
  } else if (distance == '1-|cor|') {
    den_data = (1 - abs(x %>% as.matrix %>% t %>% cor(use = cor.use, method = cor.method))) %>%
      as.dist %>% 
      hclust(method = hclust.method) %>% 
      as.dendrogram %>% dendro_data
  } else {
    stop("distance has to be 'rawdata', 'cor', '1-cor' or '1-|cor|")
  }
  
  if (!is.null(group)) {
    # assign group to observations if group isn't NULL
    den_data$labels$group = group[match(den_data$labels$label %>% as.character, 
                                        rownames(x))]
    g = ggplot(segment(den_data)) + 
      geom_segment(aes(x, y, xend = xend, yend = yend)) +
      geom_text(data = label(den_data), 
                aes(label = label, x, y = 0, color = group), 
                hjust = 0)
  } else {
    g = ggplot(segment(den_data)) + 
      geom_segment(aes(x, y, xend = xend, yend = yend)) +
      geom_text(data = label(den_data), 
                aes(label = label, x, y = 0), 
                hjust = 0)
  }
  # retrieve y axis breaks
  y.breaks = ggplot_build(g)$layout$panel_params[[1]]$y.labels %>% as.numeric
  # add yintercept line
  if (!is.null(yintercept)) {
    g = g + geom_hline(yintercept = yintercept) + 
      scale_y_reverse(breaks = c(y.breaks, yintercept))
  }
  # classical theme
  if (theme_classic) {
    g = g + theme_classic()
  }
  # add theme arguments from ...
  dots = list(...)
  THEME_args_names = THEME %>% formals %>% names
  THEME_args = dots[names(dots) %in% THEME_args_names]
  # axis label position
  g = do.call(THEME, c(list(g = g), THEME_args))
  # output plot
  g = g + labs(title = title) + xlab(label=xlab) + ylab(label=ylab)
  #g = g + labs(x = ylab, y = xlab, title = title) +
    coord_flip()

  if(addThemeFlag)
  {
    g = addTheme(g)
  }
   
  g
}


# Volcano plot ####
# QWL: replace 'odds.ratio' by 'stats'
# QWL: replace 'data' by 'resFrame'
Volcano = function(resFrame, stats, p.value, group = NULL, 
                   xlab = 'logFC', ylab = '-log10(p value)', title = NULL, 
                   vline.col = 'orange', hline.col = 'dodgerblue', 
                   vline = list(xintercept = c(-1, 1), 
                                label = c(-1, 1)), 
                   hline = list(yintercept = c(-log10(0.05), 
                                               -log10(0.05/nrow(resFrame)), 
                                               -log10(max(resFrame[p.adjust(resFrame[, p.value], method = 'fdr') <= 0.05, p.value]))), 
                                label = c('p value: 0.05', 'Bonferroni: 0.05', 'FDR: 0.05')), 
                   rowname.var = NULL, point.size = 3, 
                   theme_classic = TRUE, addThemeFlag = TRUE, ...) {
  
  # QWL: commented out 'require'
  #require(ggrepel)
  # main function of volcano plot
  
  # resFrame: resFrame provides odds ratio and p value
  # stats: a string gives odds ratio column name
  # p.value: a string gives p value column name
  # group: a string gives grouping variable to color points
  # xlab: x axis label
  # ylab: y axis label
  # title: plot title
  # vline.col: vertical line color (default: orange)
  # hline.col: horizontal line color (default: dodgerblue)
  # vline: a list includes xintercepts and their labels
  # hline: a list includes yintercepts and their labels
  # rowname.var: a string gives rownames' column name
  
  g = ggplot(resFrame, aes_string(x = paste0(stats), 
                              y = paste0('-log10(', p.value, ')'), 
                              color = group)) + 
    geom_point(size = point.size) +
      labs(title = title) + xlab(label=xlab) + ylab(label=ylab)
  # add verticle line
  if (!is.null(vline)) {
    x.axis.breaks = c(ggplot_build(g)$layout$panel_params[[1]]$x.labels %>% as.numeric, vline$xintercept)
    x.axis.label = c(ggplot_build(g)$layout$panel_params[[1]]$x.labels, vline$label)
    g = g + geom_vline(xintercept = vline$xintercept, 
                       color = vline.col) + 
      scale_x_continuous(breaks = x.axis.breaks, labels = x.axis.label)
  }
  # add horizontal line and labels
  if (!is.null(hline)) {
    y.axis.breaks = c(ggplot_build(g)$layout$panel_params[[1]]$y.labels %>% as.numeric, hline$yintercept)
    y.axis.label = c(ggplot_build(g)$layout$panel_params[[1]]$y.labels, hline$label)
    g = g + geom_hline(yintercept = hline$yintercept, 
                       color = hline.col) + 
      scale_y_continuous(breaks = y.axis.breaks, labels = y.axis.label)
  }
  # add point name label
  if (!is.null(rowname.var)) {
    g = g + geom_text_repel(aes_string(label = rowname.var))
  }
  # classical theme
  if (theme_classic) {
    g = g + theme_classic()
  }
  # add facet & theme arguments from ...
  dots = list(...)
  FACET_args_names = FACET %>% formals %>% names
  FACET_args = dots[names(dots) %in% FACET_args_names]
  THEME_args_names = THEME %>% formals %>% names
  THEME_args = dots[names(dots) %in% THEME_args_names]
  
  # add facet
  if (length(FACET_args) != 0) {
    g = do.call(FACET, c(list(g = g), FACET_args))
  }
  # axis label position
  g = do.call(THEME, c(list(g = g), THEME_args))
  
  if(addThemeFlag)
  {
    g = addTheme(g)
  }
   
  # return plot
  g
}

#
## Heatmap ####
#Heat = function(data, 
#                group = NULL, 
#                cexCol = 0.2 + 1/log10(nrow(data)), 
#                cexRow = 0.2 + 1/log10(ncol(data)), 
#                dendrogram = "both", 
#                scale = "column", 
#                axis.text.x.angle = NULL, axis.text.y.angle = NULL, ...) {
#  # main function of heatmap with heatmap.2 in package gplots
#  # QWL: commented out 'require'
#  #require(gplots)
#  # cexRow: y axis label font size
#  # cexCol: x axis label font size
#  # dendrogram: dendrogram option: 'both' (default), 'row', 'column', 'none'
#  # scale: data scaling: 'none' (default), 'row', 'column'
#  # x.lab.angle: x axis label angle
#  # y.lab.angle: y axis label angle
#  # ...: additional parameter passed to heatmap.2, such as lhei, lwid, etc
#  #     for details check ?heatmap.2
#  
#  # create grouping color
#  color_hue <- function(n) {
#    hues = seq(15, 375, length = n + 1)
#    hcl(h = hues, l = 65, c = 100)[1:n]
#  }
#  # separate grouping column for y axis grouping color
#  if (is.null(group)) {
#    data.heat = data
#    colors = NULL
#  } else {
#    data.heat = data[, names(data) != group]
#    # assign colors to different groups
#    grps = unique(data[, group])
#    n = length(grps)
#    colors = color_hue(n)[match(data[, group], grps)]
#  }
#  
#  # create heatmap
#  heatmap.2(as.matrix(data.heat), col = redgreen(256), scale = scale, 
#            key = FALSE, trace = 'none', cexRow = cexCol, cexCol = cexRow, 
#            dendrogram = dendrogram, colRow = colors, 
#            srtRow = axis.text.y.angle, srtCol = axis.text.x.angle, ...)
#}
#

Heat = function(data, 
                 group = NULL, 
                 fontsize_row=10,
                 fontsize_col=10, 
                 scale = "none",
                 cluster_rows = TRUE,
                 cluster_cols = TRUE,
                 color = colorRampPalette(rev(brewer.pal(n = 7, name ="RdYlBu")))(100),
                 angle_col = c("270", "0", "45", "90", "315"), 
		 ...) {

  # separate grouping column for y axis grouping color
  if (is.null(group)) {
    data.heat = data

    annotation_row = NULL
  } else {
    data.heat = data[, names(data) != group]

    if(!is.factor(data[, c(group)]))
    {
      annotation_row = data.frame(
        rowClass = factor(data[, c(group)])
      )
    } else {
      annotation_row = data.frame(
        rowClass = data[, c(group)]
      )
    }
    rownames(annotation_row) = rownames(data)
      
  }
  

  # create heatmap
  pheatmap(mat=as.matrix(data.heat), 
           fontsize_row = fontsize_row, 
           fontsize_col = fontsize_col, 
           scale = scale, 
           cluster_rows = cluster_rows,
           cluster_cols = cluster_cols,
           color = color,
           angle_col = angle_col, 
           annotation_row = annotation_row,
           ...)
}

# https://www.datanovia.com/en/blog/ggplot-theme-background-color-and-grids/
  
addTheme=function(g)
{
# 1. Change plot panel background color to lightblue
# and the color of major/grid lines to white
g2 = g + theme(
  panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
}

# Boxplot with ROC curve ####
BoxROC = function(data, group.var, y, 
                  box.xlab = group.var, box.ylab = y, box.group.lab = group.var, 
                  jitter.alpha = 0.8, jitter.width = 0.1, point.size = 3, 
                  roc.xlab = 'Specificity', roc.ylab = 'Sensitivity',
		  addThemeFlag = TRUE) {      
  # main function of boxplot with roc curve
  
  # QWL: commented out 'require'
  #require(gridExtra)
  #require(pROC)
  # data: includes 3 variables, one for grouping, one for real value, one for prediction
  # group.var: grouping variable
  # y: real value of y
  # yhat: prediction of y
  # box.xlab: boxplot x axis label (default: group.var)
  # box.ylab: boxplot y axis label (default: y)
  # box.group.lab: boxplot legend label (default: group.var)
  # jitter.alpha: transparency of jitters
  # jitter.width: width of jitters
  # roc.xlab: roc curve x axis label (default: Specificities)
  # roc.ylab: roc curve y axis label (default: Sensitivities)
  
  specificities = NULL
  sensitivities = NULL
  # boxplot
  g.box = ggplot(data, aes_string(x = paste0('as.factor(', group.var, ')'), 
                                  y = y, 
                                  fill = paste0('as.factor(', group.var, ')'))) + 
    geom_boxplot(outlier.size = -1) + 
    geom_jitter(alpha = jitter.alpha, 
                position = position_jitterdodge(jitter.width), 
                size = point.size) +
    labs(fill = box.group.lab) + 
    xlab(label = box.xlab)+ylab(label = box.ylab)+
    theme_classic()

   if(addThemeFlag)
   {
     g.box=addTheme(g.box)
   }

  # roc curve
  #roc.results = roc(data[, group.var], data[, yhat])                 # Original
  # QWL: added 'pROC::' before 'roc'
  #roc.results = pROC::roc(data[, group.var], data[, yhat],levels = c(0,1),direction = "<")  # JL modification 2
  # QWL: no need to use yhat. replace 'yhat' by 'y'
  # QWL: also no need to specify 'levels' and 'direction'.
  # QWL: if the levels of 'y' are not 0 or 1, then set 'levels = c(0, 1)' will
  #      cause error message. Similarly, if set wrong 'direction', the ROC
  #      curve will under the diagonal line.
  roc.results = pROC::roc(data[, group.var], data[, y])  
  
  roc.data = data.frame(sensitivities = roc.results$sensitivities, 
                        specificities = roc.results$specificities)
  g.roc = ggplot(roc.data, aes(x = specificities, y = sensitivities)) + 
    geom_path() + 
    scale_x_reverse() + 
    geom_abline(slope = 1, intercept = 1, color = 'grey') + 
    labs(title = paste0('AUC: ', round(roc.results$auc, 4)))+ 
    xlab(label = roc.xlab)+ylab(label = roc.ylab) +
    theme_classic()

   if(addThemeFlag)
   {
     g.roc=addTheme(g.roc)
   }

  # combine boxplot and roc curve
  g = arrangeGrob(g.box, g.roc, widths = c(0.5, 0.5))
  g2 = grid.arrange(g)

  g2
}


# Bi-axis error bar plot cannot be achieved from ggplot2 easily
# here use base plot
# R base plot way

# group -
# y.left - character indicating the variable on left y-axis
# y.right - character indicating the variable on right y-axis
# col.left - color for variable on left y-axis
# col.right - color for variable on right y-axis

# standard error function
seFunc = function(s, na.rm = TRUE) {
  if (na.rm) {
    s = s[!is.na(s)]
    sd(s)/sqrt(length(s))
  } else sd(s)/sqrt(length(s))
}



## Bi-axis error bar plot cannot be achieved from ggplot2 easily
## here use base plot
## R base plot way
## y.left - character indicating the variable on left y-axis
## y.right - character indicating the variable on right y-axis
## col.left - color for variable on left y-axis
## col.right - color for variable on right y-axis
#BiAxisErrBar = function(dat, 
#			group, 
#			y.left, 
#			y.right, 
#			col.left = "blue", 
#			col.right = "red", 
#			delta=0.2, 
#			xlab=group, 
#			ylab.left=y.left, 
#			ylab.right=y.right, 
#			title = "Bi-Axis Error Bar Plot",
#			las=1,
#                        angle=90, 
#			length = 0.05, 
#			line=2, 
#			type="b", 
#			code=3, 
#			legend.position="topright",
#			...) {
#  # main function of bi-axis error bar plot
#  #require(dplyr)
#  # summary input data
#  muleft = NULL
#  muright = NULL
#  seleft = NULL
#  seright = NULL
#  dat.summary = dat %>% group_by_(group) %>%
#    summarise_(muleft = paste0('mean(', y.left, ', na.rm = TRUE)'), 
#               muright = paste0('mean(', y.right, ', na.rm = TRUE)'), 
#               seleft = paste0('seFunc(', y.left, ', na.rm = TRUE)'), 
#               seright = paste0('seFunc(', y.right, ', na.rm = TRUE)')) %>%
#    mutate(lowerleft = muleft - seleft, upperleft = muleft + seleft,
#           lowerright = muright - seright, upperright = muright + seright) %>%
#    as.data.frame()
#  
#  # set plot margins, leave more space on right side for second y axis labels
#  par(mar = c(5, 4, 2, 6) + 0.1)
#  
#  xnm=dat.summary[, group]
#  myx=seq_along(xnm)
#  # draw 1st error bar plot with y axis on left side
#  lower = dat.summary[, 'lowerleft']
#  upper = dat.summary[, 'upperleft']
#  len=length(myx)
#  myxlim=c(myx[1]-2*delta, myx[len]+2*delta)
#  
#  plot(x=myx, y=dat.summary[, 'muleft'], 
#       ylim = range(c(lower, upper)), col = col.left, 
#       xlab=xlab, ylab = NA, axes=FALSE, type=type,
#       xlim=myxlim, main=title, lty=1, pch=1, ...)
#  box()
#  axis(side = 2)
#  axis(side=1, at=myx, labels = xnm, las=las)
#  
#  # draw lower and upper values: 
#  # length: arrow width; angle: arrow angle; code: arrow type; code = 3 for error bar
#  arrows(x0=myx, y0=lower, x1=myx, y1=upper, length=length, angle=angle, code=code, col=col.left)
#  # choose which side of axis for editting: 1: bottom; 2: left; 3: top; 4: right
#  # add labels for axis: line: space between label and axis
#  mtext(text = ylab.left, side = 2, line = line)
#  
#  
#  # create new plot on top of first plot
#  par(new = TRUE)
#  
#  # draw 2nd error bar plot with y axis on right side
#  # axes = F: no repeated axes
#  # xlab = NA: no repeated x labels
#  lower = dat.summary[, 'lowerright']
#  upper = dat.summary[, 'upperright']
#  myx2=myx +delta
#  plot(x=myx2, y=dat.summary[, 'muright'], xlim=myxlim,
#       ylim = range(c(lower, upper)), col = col.right, 
#       xlab=NA, ylab = NA, axes=FALSE, type=type, lty=2, pch=2)
#  
#  # draw lower and upper values: 
#  # length: arrow width; angle: arrow angle; code: arrow type; code = 3 for error bar
#  arrows(x0=myx2, y0=lower, x1=myx2, y1=upper, length=length, angle=angle, code=code, col=col.right)
#  # add labels for axis: line: space between label and axis
#  mtext(text = ylab.right, side = 4, line = line)
#  
#  axis(side=4)
#  
#  legend(x=legend.position, legend=c(y.left, y.right),
#	 col=c(col.left, col.right), lty=1:2, pch=1:2)
#}

###########################################
# PVCA plot
# QWL: revise code so that the rows of gene_data are genes; columns are subjects
PVCA = function(clin_data, clin_subjid, gene_data, pct_threshold = 0.8, 
                batch.factors, theme_classic = FALSE, addThemeFlag = TRUE, 
		...) {
  
  # QWL: commented out 'require'
  #require(pvca)
  # clin_data: include clinical information and subject id corresponding to gene_data
  # clin_subjid: column indicating subject id used in rownames of gene_data
  ## gene_data: rownames: subjects name; columns: gene names
  # gene_data: colnames: subjects name; rownames: gene names
  
  designCov = clin_data[match(colnames(gene_data), clin_data[, clin_subjid]), ]
  rownames(designCov) = as.character(designCov[, clin_subjid])
  phenoData <- new("AnnotatedDataFrame",data=designCov)
  expr.set <- ExpressionSet(assayData=gene_data,phenoData=phenoData)
  pct_threshold <- pct_threshold
  batch.factors <- batch.factors
  # QWL: added ", drop=FALSE" to account for the scenario where there
  # is only one factor
  batch.factors.level<-apply(designCov[,batch.factors, drop=FALSE],2,function(x){length(unique(x))})
  batch.factors<-batch.factors[batch.factors.level>1]
  pvcaObj <- pvcaBatchAssess(expr.set, batch.factors, pct_threshold)
  
  pvca.bar = data.frame(PVCA = t(pvcaObj$dat), label = pvcaObj$label, stringsAsFactors = FALSE)
  pvca.bar$label = factor(pvca.bar$label, levels = pvca.bar$label)
  
  g = ggplot(data = pvca.bar, aes(x = label, y = PVCA)) + 
    #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_bar(stat = "identity") + 
    geom_text(size = 3, aes(label = round(PVCA, 3)), position = position_stack(), vjust = -1) +
    xlab(label = NULL)+ylab(label = 'Weighted average proportion variance')
  # classical theme
  if (theme_classic) {
     g = g + theme_classic()
  }

  # add facet & theme arguments from ...
  dots = list(...)
  THEME_args_names = THEME %>% formals %>% names
  THEME_args = dots[names(dots) %in% THEME_args_names]
  
  # axis label position
  g = do.call(THEME, c(list(g = g), THEME_args))

  if(addThemeFlag)
  {
    g = addTheme(g)
  }
 
  # return plot
  g
}


PCA_score = function(prcomp_obj, data, dims=c(1,2), color = NULL, 
                     MD = TRUE, loadings = FALSE, 
                     loadings.color = 'black', loadings.label = FALSE,
                     title = "pca plot",
		     addThemeFlag = TRUE) {
  # main function of PCA plot
  
  # QWL: commented out 'require'
  #require(ggfortify)
  #require(ggrepel)
  
  # prcomp_obj: prcomp object from prcomp function
  # color: grouping variable
  # loadings: loadings plot (default: TRUE)
  # loadings.color: loadings axis color
  # loadings.label: loadings label (default: TRUE)
  
  # use existed autoplot from ggplot2 supported by ggfortify
  # autoplot(prcomp_obj, data = data, colour = color, loadings = loadings,
  #          loadings.colour = loadings.color, loadings.label = loadings.label)
  
  # get eigen values and variance percent
  eig.obj=factoextra::get_eig(prcomp_obj)
  vp = eig.obj$variance.percent
  # extract data and plot by ggplot
  PC1 = NULL
  PC2 = NULL
  myxlab=paste("PC", dims[1], " (Percent variance=", round(vp[dims[1]],2), ")", sep="")
  myylab=paste("PC", dims[2], " (Percent variance=", round(vp[dims[2]],2), ")", sep="")
  score_data = data.frame(prcomp_obj$x[, dims])
  colnames(score_data)=c("PC1", "PC2")
  # add grouping column
  #score_data[color] = data[, color]  
  
  # add mahalanobis distance (distance between each point and mean value)
  if (!is.null(color)) {
    data[, c(color)]=as.factor(data[, c(color)])
    dist_data = data %>% dplyr::select_(paste0('-', color))
  } else {
    dist_data = data
  }
  #mahalanobis_dist = mahalanobis(dist_data, colMeans(dist_data), cov(dist_data))    # Original
  mahalanobis_dist = mahalanobis(score_data, colMeans(score_data), cov(score_data))   # JL modification 4
  score_data[color] = data[, color]  
  score_data[, 'Mahalanobis'] = mahalanobis_dist
  # create base PCA score plot
  # QWL: 'PC1' and 'PC2' are the column names of 'score_data'
  score_plot = ggplot(score_data, aes(PC1, PC2)) + xlab(myxlab) + ylab(myylab) +
    labs(title = title)
  # add mahalanobis distance and grouping color
  md_size = switch(MD, 'Mahalanobis', NULL)
  score_plot = score_plot  + 
    geom_point(aes_string(color = color, 
                          size = md_size))
  # add loadings plot
  if (loadings) {
    loading_data = prcomp_obj$rotation[, dims] %>% as.data.frame()
    colnames(loading_data)=c("PC1", "PC2")
    score_plot = score_plot + 
      geom_segment(data = loading_data, 
                   aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                   color = loadings.color, 
                   arrow = arrow())
    if (loadings.label) {
      score_plot = score_plot + 
        geom_text_repel(data = loading_data, aes(PC1,PC2, label = rownames(loading_data)))
    }
  }

  if(addThemeFlag)
  {
    score_plot = addTheme(score_plot)
  }
 
  score_plot
}

# QWL: change input 'fit_cv_glmnet' to a few inputs: 'x' and 'y'
#cv_glmnet_plot = function(fit_cv_glmnet) {
cv_glmnet_plot = function(x, y, family="binomial", addThemeFlag = TRUE,
			  ...) {

  fit_cv_glmnet = cv.glmnet(x = x, y = y, family=family, ...)

  # main function of glmnet cv plot
  
  # fit_cv_glmnet: cv.glmnet object
  
  lambda = NULL
  cvm = NULL
  cvsd = NULL

  # extract cv.glmnet model results
  fit_cv_glmnet_results = data.frame(
    lambda = fit_cv_glmnet$lambda, 
    cvm = fit_cv_glmnet$cvm, 
    cvsd = fit_cv_glmnet$cvsd
  )
  g = ggplot(fit_cv_glmnet_results, aes(log(lambda), cvm)) + 
    geom_point(color = 'red') + 
    geom_errorbar(aes(ymax = cvm + cvsd, ymin = cvm - cvsd)) + 
    geom_vline(xintercept = log(fit_cv_glmnet$lambda.min), color = 'red') + 
    geom_vline(xintercept = log(fit_cv_glmnet$lambda.1se), color = 'blue')
  # redefine x labels and breaks
  x_breaks_orig = ggplot_build(g)$layout$panel_ranges[[1]]$x.major_source
  x_breaks_new = c(x_breaks_orig, log(fit_cv_glmnet$lambda.min), log(fit_cv_glmnet$lambda.1se))
  x_labels_new = c(x_breaks_orig, 'lambda.min', 'lambda.1se')
  g = g + scale_x_continuous(breaks = x_breaks_new, labels = x_labels_new)

  if(addThemeFlag)
  {
    g = addTheme(g)
  }

  g
}


regularization_plot = function(fit_glmnet, var.label = TRUE, theme_classic = TRUE) {
  # get parameter and results from glmnet model
  lambda = NULL
  Coefficient = NULL
  variables = NULL
  model = NULL
  alpha = fit_glmnet$call$alpha
  fit_glmnet_results = full_join(
    melt(fit_glmnet$beta %>% as.matrix, 
         varnames = c('variables', 'model'), 
         value.name = 'Coefficient'), 
    data.frame(model = colnames(fit_glmnet$beta), 
               `L1 norm` = (
                 apply(fit_glmnet$beta, 2, function(x) {
                   (1-alpha) * sum(x^2)/2 + alpha * sum(abs(x))
                 })
               ), lambda = fit_glmnet$lambda,     # JL
               check.names = FALSE
    )
  )
  # regularization plot: Coefficient vs L1 norm
  #g = ggplot(fit_glmnet_results, aes(`L1 norm`, Coefficient, color = variables)) +
  g = ggplot(fit_glmnet_results, aes(log(lambda), Coefficient, color = variables)) +
    geom_line() + 
    theme(legend.position = 'none')
  # add variable labels at right side
  if (var.label) {
    g = g +geom_text_repel(
      data = as.data.frame(filter(fit_glmnet_results, 
                    model == unique(fit_glmnet_results$model)[length(unique(fit_glmnet_results$model))]
      )), 
      aes(log(lambda), Coefficient, label = variables))    # JL modification 5
    #aes(`L1 norm`, Coefficient, label = variables))     # Original 
  }
  if (theme_classic) {
    g = g + theme_classic()
  }
  g = g + theme(legend.position = 'none')

  g
}



ImpPlot = function(model, theme_classic = TRUE, n.trees = NULL, 
		   addThemeFlag = TRUE, ...) {
  # main function of variable importance for randomForest model &
  # relative influence for gbm model
  
  # model: randomForest or gbm model
  # ...:   other arguments passed to summary.gbm, list some useful ones below
  # method: method to calculate gbm relative influence
  #     relative.influece / permutation.test.gbm
  #     check out details by ?summary.gbm
  # n.trees: summary.gbm parameter to calculate relative influence
  #     (default n.trees = NULL uses n.trees in model)
  # normalize: if normalize relative influence (default: TRUE)
  
  rownames_to_column = NULL
  importance = NULL
  variables = NULL

  # get importance from randomForest model
  if (class(model) == 'randomForest') {
    if (summary(model)['y', 'Class'] == 'factor') {
      imp = model$importance[, 'MeanDecreaseGini']
    } else if (summary(model)['y', 'Class'] == 'numeric') {
      imp = model$importance[, 'IncNodePurity']
    } else {
      stop('Undefined random forest type')
    }
    # prepare variable importance data frame for plotting
    # QWL: 'importance' was stored in 'imp' data frame
    #rownames_to_column = NULL
    imp_data = data.frame(imp) %>% rownames_to_column %>%
      dplyr::rename('variables' = 'rowname', 'importance' = 'imp') %>%
      arrange(desc(importance))
  } else if (class(model) == 'gbm') {
    # get relative influence from gbm model
    if (is.null(n.trees)) {
      imp = summary(model, plotit = FALSE, ...)
    } else {
      imp = summary(model, n.trees, plotit = FALSE, ...)
    }
    # prepare relative influence data frame for plotting
    imp_data = imp %>% dplyr::rename('variables' = 'var', 
                                     'importance' = 'rel.inf') %>%
      arrange(desc(importance))
  } else {
    stop('Only randomForest and gbm models are supported now. ')
  }
  
  # order importance data frame order and plot label
  imp_data$variables = factor(imp_data$variables, levels = imp_data$variables)
  importance_label = ifelse(class(model) == 'randomForest',
                            'importance', 
                            'relative influence')
  # importance bar plot
  # QWL: 'variables' is defined in 'imp_data'
  g = ggplot(imp_data, aes(variables, importance)) + 
    geom_bar(aes(fill = variables), stat = 'identity') + 
    geom_text(aes(label = round(importance, 2)), hjust = 1)+
    coord_flip() + 
    ylab(label = importance_label)
  if (theme_classic) {
    g = g + theme_classic()
  }
  g = g + theme(legend.position = 'none')

  if(addThemeFlag)
  {
    g=addTheme(g)
  }

  g
}

