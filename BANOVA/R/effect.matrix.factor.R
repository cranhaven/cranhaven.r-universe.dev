effect.matrix.factor <-
function (factors, assign = array(dim = 0), index_factor = NA, numeric_index = array(dim = 0), contrast = NULL){
  # generate the effect matrix for each factor, numerical covariates excluded, TODO: may have various levels for numeric variables
  # Args:
  #     factors       : values of factors to generate factor matrix
  #     assign        : index corresponding to each factor in the full design matrix, see X <- model.matrix(attr(mf1,'terms'), data = mf1)
  #     index_factor  : one number, the index of current factor in the full design matrix
  #     numeric_index : the index of numeric variables in the full design matrix
  # Returns:
  #     a matrix
  #
####
#  if (length(assign) != 0){
#    index <- which(assign == index_factor)
#    level <- length(index) + 1
#    effect_matrix <- matrix(0, nrow = level, ncol = length(assign))
#    effect_matrix[,index] <- contr.sum(level)
#    effect_matrix[,1] <- 1 # grand mean included
    #if (length(numeric_index) > 0) effect_matrix[, numeric_index] <- 1 # consider the covariates effect
#    attr(effect_matrix, 'levels') <- factors
#  }
  # new version
  tmp_contrasts <- getOption("contrasts")
  options(contrasts = rep("contr.sum",2))
  # TODO combine effect matrix factor with effect matrix interaction
  if (length(assign) != 0){
    defaultWarn <- getOption("warn") 
    #ignore of warning messages in the code below
    options(warn = -1)
    
    fac_levels <- levels(factors) #extract factor levels 
    if(is.na(as.numeric(fac_levels[1]))){
      #if the factor levels are not labeled with numeric values keep the same labeling
      level <- factor(fac_levels, levels = fac_levels, labels = fac_levels) 
    } else {
      fac_levels_numeric <- as.numeric(fac_levels)
      dummy_condition <- (length(fac_levels_numeric) == 2) && (0 %in% fac_levels_numeric) && (1 %in% fac_levels_numeric)
      effect_condition <- sum(fac_levels_numeric) == 0
      if (effect_condition || dummy_condition){
        #if factors are effect or dummy coded, levels count from postive to negative values
        level_values_sorted <- sort(fac_levels_numeric, decreasing = T)
        level <- factor(level_values_sorted, levels = level_values_sorted, labels = level_values_sorted)
      } else {
        #if not labeled keep the same labeling with numbers
        level <- factor(fac_levels, levels = fac_levels, labels = fac_levels) 
      }
    }
    level_label <-  levels(level)
    var_name <- attr(factors,'var_names')
    options(warn = defaultWarn)
    
    ### 1.1.2
    level <- assign_contrast_factor(level, var_name, contrast)
    ###
    #eval(parse(text = paste(var_name,'<- factor(c(1:',level,'))', sep = '')))
    eval(parse(text = paste(var_name,'<- level', sep = '')))
    # with column names, and include an intercept
    eval(parse(text = paste('effect_matrix <- model.matrix(~',var_name,', data = ', var_name,')', sep='')))
    attr(effect_matrix, 'levels') <- levels(factors)
  }else{
    effect_matrix = NA
  }
  options(contrasts = tmp_contrasts)
  return(effect_matrix)
}
