heatmap.io = function(obj, RS_label = NULL, regions_x = 'all', sectors_x = 'all', regions_y = 'all', sectors_y = 'all',
                      ES_x = NULL, ES_y = NULL, FUN = NULL, low = NULL, high = NULL,
                      min = NA, max = NA){
  # Checking inputs are correctly specified
  if(is.null(RS_label)){stop('RS_label must be provided; it generates the labels for the heatmap.')}
  if(!is.null(ES_x) & class(ES_x) != 'EasySelect') stop('ES_x must be an "EasySelect" object. See ?easy.select')
  if(!is.null(ES_y) & class(ES_y) != 'EasySelect') stop('ES_y must be an "EasySelect" object. See ?easy.select')
  
  # Creating the RS_x and RS_y to select 
  # ================ #
  # ===== RS_x ===== #
  # ================ #
  RS_x = RS_label
  if(!is.null(ES_x)){   
    RS_x = RS_x[as.numeric(ES_x[,1]), ]  
  } else if(!'all' %in% regions_x){
    # Checking for valid regions
    if(class(regions_x) == "factor"){
      regions_x <- as.character(regions_x)
    }
    if(class(regions_x) == "character"){
      for(k in 1:length(regions_x)){
        if(!regions_x[k] %in% RS_label[, 1]) stop(paste(regions_x[k], "is not a region in RS_label. Check spelling, capitalization, and punctuation."))
      }
      i = which(RS_x[, 1] %in% regions_x)
      RS_x = RS_x[i, ]
    }
    else if(class(regions_x) == "numeric" | class(regions_x) == "integer"){
      region <- unique(RS_label[, 1])
      regions_x <- region[regions_x]
      i = which(RS_x[, 1] %in% regions_x)
      RS_x = RS_x[i, ]
    }
  } 
  if(!'all' %in% sectors_x & is.null(ES_x)){
    # Checking for valid sectors
    if(class(sectors_x) == "factor"){
      sectors_x <- as.character(sectors_x)
    }
    if(class(sectors_x) == "character"){
      for(k in 1:length(sectors_x)){
        if(!sectors_x[k] %in% RS_label[, 2]) stop(paste(sectors_x[k], "is not a sector in RS_label. Check spelling, capitalization, and punctuation."))
      }
      i = which(RS_x[, 2] %in% sectors_x)
      RS_x = RS_x[i, ]
    }
    else if(class(sectors_x) == "numeric" | class(sectors_x) == "integer"){
      sector <- unique(RS_label[, 2])
      sectors_x <- sector[sectors_x]
      i = which(RS_x[, 2] %in% sectors_x)
      RS_x = RS_x[i, ]
    }  }
  # ================ #
  # ===== RS_y ===== #
  # ================ #
  RS_y = RS_label
  if(!is.null(ES_y)){   
    RS_y = RS_y[as.numeric(ES_y[,1]), ]  
  } else if(!'all' %in% regions_y){
    # Checking for valid regions
    if(class(regions_y) == "factor"){
      regions_y <- as.character(regions_y)
    }
    if(class(regions_y) == "character"){
      for(k in 1:length(regions_y)){
        if(!regions_y[k] %in% RS_label[, 1]) stop(paste(regions_y[k], "is not a region in RS_label. Check spelling, capitalization, and punctuation."))
      }
      i = which(RS_y[, 1] %in% regions_y)
      RS_y = RS_y[i, ]
    }
    else if(class(regions_y) == "numeric" | class(regions_y) == "integer"){
      region <- unique(RS_label[, 1])
      regions_y <- region[regions_y]
      i = which(RS_y[, 1] %in% regions_y)
      RS_y = RS_y[i, ]
    }
  }
  if(!'all' %in% sectors_y & is.null(ES_y)){
    # Checking for valid sectors
    if(class(sectors_y) == "factor"){
      sectors_y <- as.character(sectors_y)
    }
    if(class(sectors_y) == "character"){
      for(k in 1:length(sectors_y)){
        if(!sectors_y[k] %in% RS_label[, 2]) stop(paste(sectors_y[k], "is not a sector in RS_label. Check spelling, capitalization, and punctuation."))
      }
      i = which(RS_y[, 2] %in% sectors_y)
      RS_y = RS_y[i, ]
    }
    else if(class(sectors_y) == "numeric" | class(sectors_y) == "integer"){
      sector <- unique(RS_y[, 2])
      sectors <- sector[sectors_y]
      i = which(RS_y[, 2] %in% sectors)
      RS_y = RS_y[i, ]
    }  }

  # ====== #
  # Colors #
  # ====== #
  if(is.null(low)){low = 'yellow'}
  if(is.null(high)){high = 'blue'}
  
  # Selecting the specified region-sector combinations
  if(!is.null(ES_x)){
    j = as.numeric(ES_x[, 1])
    obj = obj[, j]
  } else if(dim(RS_label)[1] != dim(RS_x)[1]){
    j = which(RS_label[, 1] %in% RS_x[, 1] & RS_label[,2] %in% RS_x[, 2] )
    obj = obj[, j]
  }
  
  if(!is.null(ES_y)){
    i = as.numeric(ES_y[, 1])
    obj = obj[i, ]
  } else if(dim(RS_label)[1] != dim(RS_y)[1]){
    i = which(RS_label[, 1] %in% RS_y[, 1] & RS_label[,2] %in% RS_y[, 2] )
    obj = obj[i, ]
  }
  
  # Preparing for plotting
  if(!is.null(FUN)){
    obj = FUN(obj)
  }
  obj = data.frame(obj)
  long = reshape(obj, varying = 1:dim(obj)[2], direction = 'long', sep = '')
  long$xaxis = factor(RS_x[long$time, 2], levels = unique(RS_x[,2]))
  long$xgroup = factor(RS_x[long$time, 1], levels = unique(RS_x[,1]))
  
  long$yaxis = factor(RS_y[long$id, 2], levels = rev(unique(RS_y[,2])))
  long$ygroup = factor(RS_y[long$id, 1], levels = unique(RS_y[,1]))
  
  # Don't do this. These three lines are for CRAN's autocompilation compatibility
  X = long$X
  xaxis = long$xaxis
  yaxis = long$yaxis
  
      limit = c(min, max)
  
  ggplot(long, aes(x = xaxis, y = yaxis, fill = X))+ 
    geom_tile(color = 'white') +
    scale_fill_gradient(low = low, high = high, limits = limit) +
    labs(x = '', y = '', fill = 'Value') + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 291, hjust = 0))  +
    facet_grid(ygroup ~ xgroup, scales = 'free', space = 'free')
} # John J. P. Wade










