barPlot = function(data, x = NULL, y, group = NULL, 
                  semFlag = TRUE,
		  xFlag = FALSE,
                  bar.width = 0.5, dodge.width = 0.8, 
                  jitter = FALSE, jitter.alpha = 0.7, jitter.width = 0.1, 
                  line = NULL, line.color = 'black', 
                  xlab = x, ylab = line, theme_classic = TRUE, 
                  group.lab = group, title = "bar plots", 
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
      
      g = ggplot( data.mu.se, aes_string(x = ifelse(is.null(x), 1, x), 'mu', fill = group)) +
        geom_bar(stat="identity", color='grey', 
                 position=position_dodge())+
        geom_errorbar(
          aes(ymin = mu - myse, ymax = mu + myse),
          width = bar.width
        ) +
        labs(title = title, color = group.lab) + xlab(label=xlab) + ylab(label=ylab)
      #labs(x = xlab, y = ylab, title = title)
    }
  } else {
    g = ggplot( data.mu.se, aes_string(x, 'mu', fill = group)) +
      geom_bar(stat="identity", color='grey', 
               position=position_dodge())+
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


