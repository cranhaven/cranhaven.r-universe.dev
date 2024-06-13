
checkModelArgument <- function(model){
  if(is.null(model))
    stop('argument \'model\' can not be null value')
  
  if (!model%in%c("Sarmanov","Independent")) 
    stop("Only 5he following model are supported: \'Independent\' and \'Sarmanov\'. ")
}

checkMeasureArgument <- function(measure){
  if(is.null(measure))
    stop('argument \'model\' can not be null value')
  if (!measure%in%c("RR","OR", 'RD')) 
    stop("Only the following measures are supported: \'OR\' , \'RR\' and \'RD\' are supported. ")
}

checkMethodArgument <- function(method, measure, verbose = TRUE){
  if(is.null(method))
    stop('argument \'method\' can not be null value')
  if(!method%in%c("exact","sampling"))
    stop('only two methods are supported: \'exact\' and \'sampling\'.')
  
  if (measure=="RD"& method=="exact") {
    if(verbose == TRUE)
      message("only sampling based mehtod is available for RD")
    method <- "sampling"
    return("sampling")
  }
  
  return(method)
  
}

checkAlpha <- function(alpha){
  if(!is.numeric(alpha))
    stop('alpha shoulbe be a number between 0 and 1.')
  if(alpha > 1 || alpha < 0)
    stop('alpha shoulbe be a number between 0 and 1.')
}


checkDigit <- function(digit){
  if(!is.numeric(digit) | !is_integer(digit) | digit <= 0)
    stop('digit should be a positive integer.')
}

checkPlotLayoutType <- function(type){
  if(is.null(type))
    stop('argument \'type\' can not be null value')
  if(!type %in% c("side_by_side","overlay"))
    stop("only 2 layout types are available: \'side_by_side\' and \'overlay\'")
}

checkPlotType <- function(plot_type){
  if(is.null(plot_type))
    stop('argument \'plot_type \' can not be null value')
  if(!plot_type %in% c("density","forest"))
    stop("only 2 layout types are available: \'density\' and \'forest\'")
  
}

checkPlotBy <- function(by){
  if(is.null(by))
    stop('argument \'by \' can not be null value')
  if(!by %in% c("line_type","color"))
    stop("only 2  types are available: \'line_type\' and \'color\'")
}

checkModelParameters <- function(a1 = a1, b1= b1, a2 = a2, b2 = b2, rho = rho, model = model, additional_infor = ''){
  ## non - null
  if(is.null(a1) | is.null(b1) | is.null(a2) | is.null(b2))
    stop(paste(additional_infor, ' The following parameters can not be null: a1, b1, a2, b2.'))
  
  if(is.infinite(a1) | is.infinite(b1) | is.infinite(a2) | is.infinite(b2))
    stop(paste(additional_infor,' The following parameters can not be infinite: a1, b1, a2, b2.'))
  
  if(is.na(a1) | is.na(b1) | is.na(a2) | is.na(b2))
    stop(paste(additional_infor,' The following parameters can not be NA: a1, b1, a2, b2.'))
  
  if(model == 'Sarmanov' & is.null(rho))
    stop(paste(additional_infor,' For Sarmanov model, rho can not be null.'))
  
  ## non-negative
  if (a1 < 0 | b1 < 0 | a2 < 0 | b2 < 0)
    stop(paste(additional_infor,' The following parameters should be non-negative: a1, b1, a2, b2'))
  
  ## boundary of rho for Sarmanov model
  if(model=="Sarmanov") {
    rho_range <- rhoBoundarySarmanov(a1, b1, a2, b2)
    if (rho < rho_range['lower_bound'] | rho > rho_range['upper_bound']) {
      message(additional_infor)
      message('For Sarmanov model, rho is subject to contrait.')
      stop(paste("rho is out of bound. The range is [ ", rho_range['lower_bound'], ',' , rho_range['upper_bound'], ' ]'))
    }
  }
}






checkSingleTableData <- function(y1 = y1, n1 = n1, y2 = y2, n2 = n2, additional_infor = ''){
  ## all input variables should be be scalar
  if(length(y1)>=2 |length(n1)>=2 |length(y2)>=2 |length(n2)>=2 )
    stop ("only for single table analysis \n")
  
  ## all input variables should be integer
  if (!is_integer(y1) | !is_integer(n1) | !is_integer(y2) | !is_integer(n2))
    stop('y1, y2, n1 and n2 should be integer ', additional_infor)
  
  if( y1 > n1)
    stop(' y1 should be < n1 ', additional_infor)
  
  if( y2 > n2)
    stop(' y2 should be < n2 ', additional_infor)
}



checkMultipleTablesData <- function(data){
  if(is.null(data))
    stop('Input data is missing.')
  
  ### required columns
  required_column_names = c('study_name','y1', 'n1','y2','n2')
  for(i in 1:length(required_column_names)){
    if(!required_column_names[i] %in% names(data))
      stop('The required column ', required_column_names[i] ,' is missing.')
  }
  
  study_names <- data$study_name
  unique_study_names <-unique(study_names)
  if(length(study_names) != length(unique_study_names))
    stop('The study_names column contains duplicate.')
  
  num_studies <- nrow(data)
  for (row in 1:num_studies) {
    y1 <- data[row, "y1"]
    n1  <- data[row, "n1"]
    y2<- data[row, "y2"]
    n2<- data[row, "n2"]
    study_name = data[row, "study_name"] 
    additional_infor <- paste('for study ', study_name )
    checkSingleTableData(y1 = y1, n1 = n1, y2 = y2, n2 = n2, additional_infor = additional_infor)
  }
} 


checkHesesianMat <- function(mat){
  if(sum(is.na(mat)))
    stop('The hessian matrix has NA values.')
  
  if(sum(is.infinite(mat)))
    stop('The hessian matrix has inifite values.')
  
  
  if((mean(abs(t(mat)-mat))>1e-4))
    stop('The hessian matrix is not symmetric.')
  
  
  if((sum(diag(mat) < 0)>0))
    stop('The negative hessian matrix is not positive definite.')
  
  if(!nrow(mat) %in% c(NUM_PARAMETERS_INDEPENDENT, NUM_PARAMETERS_SARMANOV))
    stop('The dimention of hessian matrix is incorrect.')
}  



checkSelectStudy <- function(selected_study_names, multiple_tables_object){
  
  if(is.null(selected_study_names)){
    selected_study_names <- c(multiple_tables_object$data$study_name)
    return(selected_study_names)
  }
  
  all_study_names <- c(multiple_tables_object$data$study_name)
  num_select_study <- length(selected_study_names)
  for(i in 1:num_select_study){
    if(!selected_study_names[i] %in% all_study_names){
      stop(paste('The selected study', selected_study_names[i],'is not valid.'))
    }
  }
  
  return(selected_study_names)
  
}

