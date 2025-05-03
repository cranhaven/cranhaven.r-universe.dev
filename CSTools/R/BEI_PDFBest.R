#' Computing the Best Index PDFs combining Index PDFs from two SFSs
#'
#'@author Eroteida Sanchez-Garcia - AEMET, \email{esanchezg@aemet.es}
#'
#'@description This function implements the computation to obtain the index 
#'Probability Density Functions (PDFs) (e.g. NAO index) obtained to combining 
#'the Index PDFs for two Seasonal Forecast Systems (SFSs), the Best Index 
#'estimation (see Sanchez-Garcia, E. et al (2019), 
#'\doi{10.5194/asr-16-165-2019} for more details about the 
#'methodology applied to estimate the Best Index).
#'
#'@references Regionally improved seasonal forecast of precipitation through 
#'Best estimation of winter NAO, Sanchez-Garcia, E. et al.,
#' Adv. Sci. Res., 16, 165174, 2019, \doi{10.5194/asr-16-165-2019}
#'
#'@param index_obs Index (e.g. NAO index) array from an observational database
#'  or reanalysis with at least a temporal dimension (by default 'time'), 
#'  which must be greater than 2.
#'@param index_hind1 Index (e.g. NAO index) array from a SFS (named SFS1)
#'  with at least two dimensions (time , member) or (time, statistic). 
#'  The temporal dimension, by default 'time', must be greater than 2. 
#'  The dimension 'member' must be greater than 1. The dimension 'statistic' 
#'  must be equal to 2, for containing the two paramenters of a normal 
#'  distribution (mean and sd) representing the ensemble of a SFS. It is not 
#'  possible to have the dimension 'member' and 'statistic' 
#'  at the same time.
#'@param index_hind2 Index (e.g. NAO index) array from a SFS (named SFS2)
#'  with at least two dimensions (time , member) or (time, statistic). 
#'  The temporal dimension, by default 'time', must be greater than 2. 
#'  The dimension 'member' must be greater than 1. 
#'  The dimension 'statistic' must be equal to 2, for containing the two 
#'  paramenters of a normal distribution (mean and sd) representing the ensemble 
#'  of a SFS. It is not possible to have the dimension 'member' and  'statistic' 
#'  together.
#'@param index_fcst1 (optional, default = NULL) Index (e.g. NAO index) array 
#'  from forescating of SFS1 with at least two dimensions (time , member) or 
#'  (time, statistic). The temporal dimension, by default 'time', must be equal 
#'  to 1, the forecast year target. The dimension 'member' must be greater than 
#'  1. The dimension 'statistic' must be equal to 2, for containing the two 
#'  paramenters of a normal distribution (mean and sd) representing the ensemble 
#'  of a SFS. It is not possible to have the dimension 'member' and  'statistic' 
#'  together.
#'@param index_fcst2 (optional, default = NULL) Index (e.g. NAO index) array 
#'  from forescating of SFS2 with at least two dimensions (time , member) or 
#'  (time, statistic). The temporal dimension, by default 'time', must be equal 
#'  to 1, the forecast year target. The dimension 'member' must be greater than 
#'  1. The dimension 'statistic' must be equal to 2, for containing the two 
#'  paramenters of a normal distribution (mean and sd) representing the ensemble 
#'  of a SFS. It is not possible to have the dimension 'member' and  'statistic' 
#'  together.
#'@param method_BC A character vector of maximun length 2 indicating the bias 
#'  correction methodology to be applied on each SFS. If it is 'none' or any of 
#'  its elements is 'none', the bias correction won't be applied. Available 
#'  methods developped are "ME" (a bias correction scheme based on the mean 
#'  error or bias between observation and predictions to correct the predicted 
#'  values), and "LMEV" (a bias correction scheme based on a linear model using 
#'  ensemble variance of index as predictor). (see Sanchez-Garcia, E. et al 
#'  (2019), \doi{10.5194/asr-16-165-2019} for more details).
#'@param time_dim_name A character string indicating the name of the temporal 
#'  dimension, by default 'time'.
#'@param na.rm Logical (default = FALSE). Should missing values be removed? 
#' 
#'@return BEI_PDFBest() returns an array with the parameters that caracterize
#'the PDFs, with at least a temporal dimension, by default 'time' and dimension 
#''statistic' equal to 2. The firt statistic is the parameter 'mean' of the PDF 
#'for the best estimation combining the two SFSs PDFs. The second statistic is 
#'the parameter 'standard deviation' of the PDF for the best estimation 
#'combining the two SFSs PDFs. If index_fcst1 and/or index_fcst2 are null, 
#'returns the values for hindcast period. Otherwise, it returns the values for a 
#'forecast year.
#' 
#'@examples
#' # Example 1 for the BEI_PDFBest function
#' index_obs<- rnorm(10, sd = 3)
#' dim(index_obs) <- c(time = 5, season = 2)
#' index_hind1 <- rnorm(40, mean = 0.2, sd = 3)
#' dim(index_hind1) <- c(time = 5, member = 4, season = 2)
#' index_hind2 <- rnorm(60, mean = -0.5, sd = 4)
#' dim(index_hind2) <- c(time = 5, member = 6, season = 2)
#' index_fcst1 <- rnorm(16, mean = 0.2, sd = 3)
#' dim(index_fcst1) <- c(time = 1, member = 8, season = 2)
#' index_fcst2 <- rnorm(18, mean = -0.5, sd = 4)
#' dim(index_fcst2) <- c(time = 1, member = 9, season = 2)
#' method_BC <- 'ME'
#' res <- BEI_PDFBest(index_obs, index_hind1, index_hind2, index_fcst1, 
#' index_fcst2, method_BC)  
#' # Example 2 for the BEI_PDFBest function
#' index_obs<- rnorm(10, sd = 3)
#' dim(index_obs) <- c(time = 5, season = 2)
#' index_hind1 <- rnorm(40, mean = 0.2, sd = 3)
#' dim(index_hind1) <- c(time = 5, member = 4, season = 2)
#' index_hind2 <- rnorm(60, mean = -0.5, sd = 4)
#' dim(index_hind2) <- c(time = 5, member = 6, season = 2)
#' index_fcst1 <- rnorm(16, mean = 0.2, sd = 3)
#' dim(index_fcst1) <- c(time = 1, member = 8, season = 2)
#' index_fcst2 <- rnorm(18, mean = -0.5, sd = 4)
#' dim(index_fcst2) <- c(time = 1, member = 9, season = 2)
#' method_BC <- c('LMEV', 'ME')
#' res <- BEI_PDFBest(index_obs, index_hind1, index_hind2, index_fcst1, 
#'                    index_fcst2, method_BC) 
#'@import multiApply
#'@importFrom verification verify
#'@export
BEI_PDFBest <- function(index_obs, index_hind1, index_hind2, index_fcst1 = NULL, 
                        index_fcst2 = NULL, method_BC = 'none',
                        time_dim_name = 'time', na.rm = FALSE) {
                    
  if (!is.logical(na.rm)) {
    stop("Parameter 'na.rm' must be a logical value.")
  }
  if (!is.character(time_dim_name)) {
    stop("Parameter 'time_dim_name' must be a character string ",
         "indicating the name of the temporal dimension.")
  }
  if (length(time_dim_name) > 1) {
    warning("Parameter 'time_dim_name' has length greater than 1 and ",
            "only the first element will be used.")
    time_dim_name <- time_dim_name[1]
  }
  if (!is.character(method_BC) || !is.vector(method_BC)){
    stop("Parameter 'method_BC' must be a character vector.")
  }
  if (!(length(method_BC) == 1 || length(method_BC) == 2)) {
    stop("Length of parameter 'method_BC' must be 1 or 2.")
  }
  if(!all(method_BC %in% c('ME', 'LMEV', 'none'))){
    stop("Elements of parameter 'method_BC' must be equals to ", 
         "'none, 'ME' or 'LMEV'")
  }
  if (!is.array(index_obs)) {
    stop("Parameter 'index_obs' must be an array.")
  }
  if (!is.array(index_hind1)) {
    stop("Parameter 'index_hind1' must be an array.")
  }
  if (!is.array(index_hind2)) {
    stop("Parameter 'index_hind2' must be an array.")
  }
  if (is.null(names(dim(index_hind1))) || 
      is.null(names(dim(index_obs))) ||
      is.null(names(dim(index_hind2)))) {
    stop("Parameters 'index_obs', 'index_hind1' and 'index_hind2' ",
         "should have dimmension names.")
  }
  if(!(time_dim_name %in% names(dim(index_obs)))) {
    stop("Parameter 'index_obs' must have temporal dimension.")
  }
  if(!(time_dim_name %in% names(dim(index_hind1)))) {
    stop("Parameter 'index_hind1' must have temporal dimension.")
  }
  if(!(time_dim_name %in% names(dim(index_hind2)))) {
    stop("Parameter 'index_hind2' must have temporal dimension.")
  }
  if (dim(index_obs)[time_dim_name] <= 2) {
    stop("Length of temporal dimension ",
         "of parameter 'index_obs', 'index_hind1' and 'index_hind2' ",
         "must be greater than 2.")
  }
  if (dim(index_obs)[time_dim_name] != dim(index_hind1)[time_dim_name]) {
    stop("Length of temporal dimensions ",
         "of parameter 'index_obs' and 'index_hind1' must be equals.")
  }
  if (dim(index_hind1)[time_dim_name] != dim(index_hind2)[time_dim_name]) {
    stop("Length of temporal dimensions ",
         "of parameter 'index_hind1' and 'index_hind2' must be equals.")
  }
  if('member' %in% names(dim(index_hind1)) & 
     'statistic' %in% names(dim(index_hind1))) {
    stop("Parameter 'index_hind1' must have at least ", 
         "dimension 'member' or 'statistic', not 'member' and 'statistic' ",
         "together.")
  }
  if('member' %in% names(dim(index_hind2)) & 
     'statistic' %in% names(dim(index_hind2))) {
    stop("Parameter 'index_hind2' must have at least ", 
         "dimension 'member' or 'statistic', not 'member' and 'statistic' ",
         "together.")
  }
  if(!('member' %in% names(dim(index_hind1))) & 
     !('statistic' %in% names(dim(index_hind1)))) {
    stop("Parameter 'index_hind1' must have dimension ", 
         "'member' or 'statistic'")
  }
  if(!('member' %in% names(dim(index_hind2))) & 
     !('statistic' %in% names(dim(index_hind2)))) {
    stop("Parameter 'index_hind2' must have dimension ", 
         "'member' or 'statistic'")
  }
  if ('member' %in% names(dim(index_hind1))){
    if (dim(index_hind1)['member'] == 1) {
      stop("Length of dimension 'member' ",
           "of parameter 'index_hind1' must be greater than 1.")
    }
  }
  if ('member' %in% names(dim(index_hind2))){
    if (dim(index_hind2)['member'] == 1) {
      stop("Length of dimension 'member' ",
           "of parameter 'index_hind2' must be greater than 1.")
    }
  }
  if ('statistic' %in% names(dim(index_hind1))){
    if (dim(index_hind1)['statistic'] != 2) {
      stop("Length of dimension 'statistic' ",
           "of parameter 'index_hind1' must be equal to 2.")
    }
  }
  if ('statistic' %in% names(dim(index_hind2))){
    if (dim(index_hind2)['statistic'] != 2) {
      stop("Length of dimension 'statistic' ",
           "of parameter 'index_hind2' must be equal to 2.")
    }
  }
  if (!is.null(index_fcst1)){
    if (!is.array(index_fcst1)) {
      stop("Parameter 'index_fcst1' must be an array.")
    }
    if (is.null(names(dim(index_fcst1)))){
      stop("Parameters 'index_fcst1' should have dimmension names.")
    }
    if(!(time_dim_name %in% names(dim(index_fcst1)))) {
      stop("Parameter 'index_fcst1' must have temporal dimension.")
    }
    if (dim(index_fcst1)[time_dim_name] != 1) {
      stop("Length of temporal dimension ",
           "of parameter 'index_fcst1' must be equal to 1.")
    }
    if((length(names(dim(index_hind1))) != length(names(dim(index_fcst1)))) 
       || (!all(names(dim(index_hind1)) == names(dim(index_fcst1))))){
      stop("Dimension names of parameter 'index_hind1' and 'index_fcst1' ",
           "must be equals.")
    }
    if ('member' %in% names(dim(index_fcst1))){
      if (dim(index_fcst1)['member'] == 1) {
        stop("Length of dimension 'member' ",
             "of parameter 'index_fcst1' must be greater than 1.")
      }
    }
    if ('statistic' %in% names(dim(index_fcst1))){
      if (dim(index_fcst1)['statistic'] != 2) {
        stop("Length of dimension 'statistic' ",
             "of parameter 'index_fcst1' must be equal to 2.")
      }
    }
  }
  if (!is.null(index_fcst2)){
    if (!is.array(index_fcst2)) {
      stop("Parameter 'index_fcst2' must be an array.")
    }
    if (is.null(names(dim(index_fcst2)))){
      stop("Parameters 'index_fcst2' should have dimmension names.")
    }
    if(!(time_dim_name %in% names(dim(index_fcst2)))) {
      stop("Parameter 'index_fcst2' must have temporal dimension.")
    }
    if (dim(index_fcst2)[time_dim_name] != 1) {
      stop("Length of temporal dimension ",
           "of parameter 'index_fcst2' must be equal to 1.")
    }
    if((length(names(dim(index_hind2))) != length(names(dim(index_fcst2)))) 
       || (!all(names(dim(index_hind2)) == names(dim(index_fcst2))))){
      stop("Dimension names of parameter 'index_hind2' and 'index_fcst2' ",
           "must be equals.")
    }
    if ('member' %in% names(dim(index_fcst2))){
      if (dim(index_fcst2)['member'] == 1) {
        stop("Length of dimension 'member' ",
             "of parameter 'index_fcst2' must be greater than 1.")
      }
    }
    if ('statistic' %in% names(dim(index_fcst1))){
      if (dim(index_fcst1)['statistic'] != 2) {
        stop("Length of dimension 'statistic' ",
             "of parameter 'index_fcst1' must be equal to 2.")
      }
    }
  }
  
  if (all(method_BC == 'none')){
    method_BC1 <- 'ME'
    method_BC2 <- 'ME'
    bc_dataset1 <- FALSE
    bc_dataset2 <- FALSE
  } else if (length(method_BC) == 1){
    method_BC1 <- method_BC
    method_BC2 <- method_BC
    bc_dataset1 <- TRUE
    bc_dataset2 <- TRUE
  } else {
    if(method_BC[1] == 'none'){
      method_BC1 <- 'ME'
      bc_dataset1 <- FALSE
    } else {
      method_BC1 <- method_BC[1]
      bc_dataset1 <- TRUE
    }
    if(method_BC[2] == 'none'){
      method_BC2 <- 'ME'
      bc_dataset2 <- FALSE
    } else {
      method_BC2 <- method_BC[2]
      bc_dataset2 <- TRUE
    }
  }
  
  pdf_hind1 <- PDFIndexHind(index_hind1, index_obs, method = method_BC1, 
                            time_dim_name = time_dim_name, na.rm = na.rm)
  pdf_hind2 <- PDFIndexHind(index_hind2, index_obs, method = method_BC2, 
                            time_dim_name = time_dim_name, na.rm = na.rm)
  if (is.null(index_fcst1) || is.null(index_fcst2)){
    pdf_best <- Apply(list(pdf_hind1, pdf_hind2),
                      target_dims = list('statistic',
                                         'statistic'),
                      fun = .BEI_PDFBest, 
                      bc_dataset1, bc_dataset2)
  } else {
    pdf_fcst1 <- PDFIndexFcst(index_hind1, index_obs, index_fcst1,
                              method_BC1, time_dim_name = time_dim_name,
                              na.rm = na.rm)
    pdf_fcst2 <- PDFIndexFcst(index_hind2, index_obs, index_fcst2,
                              method_BC2, time_dim_name = time_dim_name,
                              na.rm = na.rm)
    pdf_best <- Apply(list(pdf_fcst1, pdf_fcst2),
                      target_dims = list('statistic',
                                         'statistic'),
                      fun = .BEI_PDFBest, 
                      bc_dataset1, bc_dataset2)
  }
  Dim <- names(dim(index_hind1))
  if('member' %in% Dim){
    pos_aux <- match('member', Dim)
    Dim[pos_aux] <- 'statistic'
  }
  pos <- match(Dim, names(dim(pdf_best$output1)))
  res <- aperm(pdf_best$output1,pos)
  names(dim(res)) <- Dim
  return(res)
}

#' Atomic BEI_PDFBest
#'@param pdf_1 Statistics array for the first SFS PDF with one dimension
#'  'statistic' equal to 4.
#'@param pdf_2 Statistics array for the second SFS PDF with one dimension
#'  'statistic' equal to 4.
#'@param bc_dataset1 Logical (default = TRUE).
#'  If TRUE the Index PDFs for the first SFS has been computed with bias 
#'  corrected.
#'@param bc_dataset2 Logical (default = TRUE). If TRUE the Index PDFs for the 
#'  second SFS has been computed with bias corrected.
#' 
#'@return .BEI_PDFBest returns an array with dimensions (statistic = 2).
#'The firt statistic is the parameter 'mean' of the PDF for the best estimation
#'combining the two SFSs PDF. The second statistic is the parameter 'standard 
#'deviation' of the PDF for the best estimation combining the two SFSs PDF.
#' 
#'@examples
#' # Example for the Atomic BEI_PDFBest function
#' pdf_1 <- c(1.1,0.6,1.6,0.9)
#' dim(pdf_1) <-  c(statistic = 4)
#' pdf_2 <- c(1,0.5,1.5,0.8)
#' dim(pdf_2) <-  c(statistic = 4)
#' res <- .BEI_PDFBest(pdf_1, pdf_2, bc_dataset1 = TRUE, bc_dataset2 = FALSE)
#'@noRd
.BEI_PDFBest <- function(pdf_1, pdf_2, bc_dataset1 = TRUE, bc_dataset2 = TRUE) {
  if(bc_dataset1){
    # apply bias correction to model 1
    mean_1 <- pdf_1[3]
    sigma_1 <- pdf_1[4]
  } else{
    # not bias correction to model 1
    mean_1 <- pdf_1[1]
    sigma_1 <- pdf_1[2]
  }
  if(bc_dataset2){
    # apply bias correction to model 2
    mean_2 <- pdf_2[3]
    sigma_2 <- pdf_2[4]
  } else {
    # not bias correction to model 2
    mean_2 <- pdf_2[1]
    sigma_2 <- pdf_2[2]
  }
  a_1 <- (sigma_2^2)/((sigma_1^2)+(sigma_2^2))
  a_2 <- (sigma_1^2)/((sigma_1^2)+(sigma_2^2))
  pdf_mean <- a_1*mean_1 + a_2*mean_2
  pdf_sigma <- sqrt((sigma_1^2)*(sigma_2^2)/((sigma_1^2)+(sigma_2^2)))
  data <- c(pdf_mean, pdf_sigma)
  dim(data) <-  c(statistic = 2)
  return(data)
}

#'Computing the Index PDFs for a dataset of SFSs for a hindcats period.
#'
#'@author Eroteida Sanchez-Garcia - AEMET, \email{esanchezg@aemet.es}
#'
#'@description This function implements the computation to obtain the index PDFs
#' (e.g. NAO index) to improve the index estimate from SFSs for a hindcast period.
#'
#'@references Regionally improved seasonal forecast of precipitation through Best
#'estimation of winter NAO, Sanchez-Garcia, E. et al.,
#'Adv. Sci. Res., 16, 165174, 2019, \doi{10.5194/asr-16-165-2019}
#' 
#'@param index_hind Index (e.g. NAO index) array from SFSs
#'  with at least two dimensions (time , member) or (time, statistic). 
#'  The temporal dimension, by default 'time', must be greater than 2. 
#'  The dimension 'member' must be greater than 1. 
#'  The dimension 'statistic' must be equal to 2, for containing the two 
#'  paramenters of a normal distribution (mean and sd) representing the ensemble 
#'  of a SFS. It is not possible to have the dimension 'member' and  'statistic' 
#'  together.
#'@param index_obs Index (e.g. NAO index) array from an observational database
#'  or reanalysis with at least a temporal dimension (by default 'time'), 
#'  which must be greater than 2.
#'@param method A character string indicating which methodology is applied
#'  to compute the PDFs. One of "ME" (default) or "LMEV".
#'@param time_dim_name A character string indicating the name of the temporal 
#'  dimension, by default 'time'.
#'@param na.rm Logical (default = FALSE). Should missing values be removed?
#'
#'@return An array with at least two dimensions (time, statistic = 4). The firt 
#'statistic is the parameter 'mean' of the PDF with not bias corrected.
#'The second statistic is the parameter 'standard deviation' of the PDF with not 
#'bias corrected. The third statistic is the parameter 'mean' of the PDF with 
#'bias corrected. The fourth statistic is the parameter 'standard deviation' of 
#'the PDF with bias corrected.
#'@import multiApply
#'@examples
#' # Example for the PDFIndexHind function
#' # Example 1 
#' index_obs <- 1 : (5 * 3 ) 
#' dim(index_obs) <- c(time = 5, season = 3)
#' index_hind <- 1 : (5 * 4 * 3)
#' dim(index_hind) <- c(time = 5, member = 4, season = 3)
#' res <- PDFIndexHind(index_hind, index_obs)
#' dim(res)
#' # time statistic  season
#' #   5         4        3
#' # Example 2
#' index_obs <- 1 : (5 * 3) 
#' dim(index_obs) <- c(time = 5, season = 3)
#' index_hind <- 1 : (5 * 2 * 3)
#' dim(index_hind) <- c(time = 5, statistic = 2, season = 3)
#' res <- PDFIndexHind(index_hind, index_obs)
#'@import multiApply
#'@importFrom verification verify
#'@export
PDFIndexHind <- function(index_hind, index_obs, method ='ME', 
                         time_dim_name = 'time', na.rm = FALSE) {
  if (!is.character(time_dim_name)) {
    stop("Parameter 'time_dim_name' must be a character string indicating",
         " the name of the temporal dimension.")
  }
  if (length(time_dim_name) > 1) {
    warning("Parameter 'time_dim_name' has length greater than 1 and ",
            "only the first element will be used.")
    time_dim_name <- time_dim_name[1]
  }
  if (!(method %in% c('ME', 'LMEV'))){
    stop("Parameter 'method' must be equal to 'ME' or 'LMEV'")
  }
  if (!is.logical(na.rm)) {
    stop("Parameter 'na.rm' must be a logical value.")
  }
  if (!is.array(index_hind)) {
    stop("Parameter 'index_hind' must be an array.")
  }
  if (!is.array(index_obs)) {
    stop("Parameter 'index_obs' must be an array.")
  }
  if (is.null(names(dim(index_hind))) || is.null(names(dim(index_obs)))) {
    stop("Parameters 'index_hind' and 'index_obs'",
         " should have dimmension names.")
  }
  if(!(time_dim_name %in% names(dim(index_obs)))) {
    stop("Parameter 'index_obs' must have temporal dimension.")
  }
  if(!(time_dim_name %in% names(dim(index_hind)))) {
    stop("Parameter 'index_hind' must have temporal dimension.")
  }
  if (dim(index_obs)[time_dim_name] != dim(index_hind)[time_dim_name]) {
    stop("Length of the temporal dimensions ",
         "of the parameter 'index_obs' and 'index_hind' must be equals.")
  }
  if (dim(index_obs)[time_dim_name] <= 2) {
    stop("Length of temporal dimension ",
         "of parameter 'index_obs' and 'index_hind' must be greater than 2.")
  }
  if('member' %in% names(dim(index_hind)) & 
     'statistic' %in% names(dim(index_hind))) {
    stop("Parameter 'index_hind' must have at least dimension ", 
         "'member' or 'statistic', not 'member' and 'statistic' together.")
  }
  
  if(!('member' %in% names(dim(index_hind))) & 
     !('statistic' %in% names(dim(index_hind)))) {
    stop("Parameter 'index_hind' must have dimension ", 
         "'member' or 'statistic'")
  }
  
  if ('member' %in% names(dim(index_hind))){
    if (dim(index_hind)['member'] == 1) {
      stop("Length of dimension 'member' ",
           "of parameter 'index_hind' must be greater than 1.")
    }
  }
  if ('statistic' %in% names(dim(index_hind))){
    if (dim(index_hind)['statistic'] != 2) {
      stop("Length of dimension 'statistic' ",
           "of parameter 'index_hind' must be equal to 2.")
    }
  }
  
  if ('member' %in% names(dim(index_hind))) {
    PDFstatistics <- Apply(list(index_hind, index_obs),
                           target_dims = list(c(time_dim_name, 'member'), time_dim_name),
                           fun = .PDFIndexHind, method, time_dim_name, na.rm)
  } else if ('statistic' %in% names(dim(index_hind))){
    PDFstatistics <- Apply(list(index_hind, index_obs),
                           target_dims = list(c(time_dim_name, 'statistic'), time_dim_name),
                           fun = .PDFIndexHind, method, time_dim_name, na.rm)
  }
  return(PDFstatistics$output1)
}

#' Atomic PDFIndexHind
#'@param index_hind Index (e.g. NAO index) array from a SFS with dimensions
#'  (time, member) or (time, statistic) for a hindcast period. 
#'  The temporal dimension, by default 'time', must be greater than 2.
#'@param index_obs Index (e.g. NAO index) array from an observational dataset
#'  or reanalysis with dimension (time). The temporal dimension, 
#'  by default 'time', must be greater than 2.
#'@param method A character string indicating which methodology is applied
#'  to compute the PDF. One of "ME" (default) or "LMEV".
#'@param time_dim_name A character string indicating the name of the temporal 
#'  dimension, by default 'time'.
#'@param na.rm Logical. Should missing values be removed?
#'@return .PDFIndexHind returns an array with dimensions (time, statistic = 4).
#'The firt statistic is the parameter 'mean' of the PDF with not bias corrected
#'for the hindcast period. The second statistic is the parameter 'standard 
#'deviation' of the PDF with not bias corrected for the hindcast period.
#'The third statistic is the parameter 'mean' of the PDF with bias corrected 
#'for the hindcast period. The fourth statistic is the parameter 'standard 
#'deviation' of the PDF with bias corrected for the hindcast period.
#'@examples
#' # Example for the Atomic PDFIndexHind function
#' index_obs <- 1 : 10
#' dim(index_obs) <- c(time = length(index_obs))
#' index_hind <- 1 : (10 * 3)
#' dim(index_hind) <- c(time = 10, member = 3)
#' res <- .PDFIndexHind(index_hind, index_obs)
#'@import multiApply
#'@importFrom verification verify
#'@noRd
.PDFIndexHind <- function(index_hind, index_obs,  method = 'ME', 
                          time_dim_name = 'time', na.rm = FALSE) {
  dimnameshind <- names(dim(index_hind))
  
  if ('member' %in% dimnameshind) {
    pos <- match(c(time_dim_name, 'member'), names(dim(index_hind)))
    index_hind <- aperm(index_hind, pos)
    names(dim(index_hind)) <- c(time_dim_name, 'member')
    hind_ens_mean <- Apply(list(index_hind), target_dims = 'member',
                           fun = mean, na.rm = na.rm)$output1
    hind_ens_sd <- Apply(list(index_hind), target_dims = 'member',
                         fun = sd, na.rm = na.rm)$output1
  } else if ('statistic' %in% dimnameshind){
    pos <- match(c(time_dim_name, 'statistic'), names(dim(index_hind)))
    index_hind <- aperm(index_hind,pos)
    names(dim(index_hind)) <- c(time_dim_name, 'statistic')
    hind_ens_mean <- index_hind[,1]
    hind_ens_sd <- index_hind[,2]
  } else {
    stop("Wrong dimension of parameter 'index_hind'.")
  }
  error <- hind_ens_mean - index_obs
  pdfnotbc_mean <-  hind_ens_mean
  ind <- 1 : length(index_obs)
  pdfnotbc_sd <- unlist(lapply(ind, function(x) {Sigma_notBC(hind_ens_mean[-x],
                                                             index_obs[-x], 
                                                             na.rm)}))
  
  if (method == 'ME') {
    pdfbc <-  unlist(lapply(ind, function(x) {MEBC(hind_ens_mean[-x],
                                                   index_obs[-x], na.rm)}))
    pdfbc_mean  <- hind_ens_mean - pdfbc
    pdfbc_sd <-  unlist(lapply(ind, function(x) {Sigma_BC(pdfbc_mean[-x],
                                                          index_obs[-x],
                                                          na.rm)}))
  } else if (method == 'LMEV') {
    # linear model error-variance
    hind_ens_var <- hind_ens_sd^2
    dim_hind <- names(dim(index_hind))
    dim_obs <- names(dim(index_obs))
    coeff <- sapply(ind, function(x) {LMEV(index_hind[-x,], index_obs[-x], 
                                           dim_hind, dim_obs, na.rm)})
    pdfbc_mean <- hind_ens_mean - (unlist(coeff['coeff1',]) +
                                     unlist(coeff['coeff2',])*hind_ens_var)
   
    pdfbc_sd <-  unlist(lapply(ind, function(x) {Sigma_BC(pdfbc_mean[-x],
                                                          index_obs[-x],
                                                          na.rm)}))
  } else {
    stop("Error: Parameter 'method' is wrong")
  }
  data <- cbind(pdfnotbc_mean, pdfnotbc_sd, pdfbc_mean, pdfbc_sd)
  names(dim(data)) <- c(names(dim(index_obs)), 'statistic')
  return(data)
}

#' Computing the Index PDFs for a dataset of SFSs for a forecast year.
#'
#'@author Eroteida Sanchez-Garcia - AEMET, \email{esanchezg@aemet.es}
#' 
#'@description This function implements the computation to obtain the index PDFs
#' (e.g. NAO index) to improve the index estimate from SFSs for a forecast year.
#' 
#'@references Regionally improved seasonal forecast of precipitation through Best
#' estimation of winter NAO, Sanchez-Garcia, E. et al.,
#' Adv. Sci. Res., 16, 165174, 2019, \doi{10.5194/asr-16-165-2019}
#' 
#'@param index_hind Index (e.g. NAO index) array from SFSs
#'  with at least two dimensions (time , member) or (time, statistic). 
#'  The temporal dimension, by default 'time', must be greater than 2. 
#'  The dimension 'member' must be greater than 1. 
#'  The dimension 'statistic' must be equal to 2, for containing the two 
#'  paramenters of a normal distribution (mean and sd) representing the ensemble 
#'  of a SFS. It is not possible to have the dimension 'member' and  'statistic' 
#'  together.
#'@param index_obs Index (e.g. NAO index) array from an observational database
#'  or reanalysis with at least a temporal dimension (by default 'time'), 
#'  which must be greater than 2.
#'@param index_fcst Index (e.g. NAO index) array from SFSs with at least two 
#'  dimensions (time , member) or (time, statistic). The temporal dimension, by 
#'  default 'time', must be equal to 1, the forecast year target. The dimension 
#'  'member' must be greater than 1. The dimension 'statistic' must be equal to 
#'  2, for containing the two paramenters of a normal distribution (mean and sd) 
#'  representing the ensemble of a SFS. It is not possible to have the dimension 
#'  'member' and  'statistic' together.
#'@param method A character string indicating which methodology is applied
#'  to compute the PDFs. One of "ME" (default) or "LMEV".
#'@param time_dim_name A character string indicating the name of the temporal 
#'  dimension, by default 'time'.
#'@param na.rm Logical (default = FALSE). Should missing values be removed?
#'
#'@return An array with at least two dimensions (time = 1, statistic = 4).
#'The firt statistic is the parameter 'mean' of the PDF with not bias corrected
#'The second statistic is the parameter 'standard deviation' of the PDF with not 
#'bias corrected. The third statistic is the parameter 'mean' of the PDF with 
#'bias corrected. The fourth statistic is the parameter 'standard deviation' of 
#'the PDF with bias corrected.
#'@import multiApply
#'@examples
#' # Example for the PDFIndexFcst function
#' index_fcst <- 1 : (8 * 4)
#' dim(index_fcst) <- c(time = 1, member = 8, season = 4)
#' index_obs<- 1 : (5 * 4)
#' dim(index_obs) <- c(time = 5, season = 4)
#' index_hind <- 1 : (5 * 4 * 4)
#' dim(index_hind) <- c(time = 5, member = 4, season = 4)
#' res <- PDFIndexFcst(index_hind, index_obs, index_fcst)
#' dim(res)
#' # time statistic   season 
#' #   1         4         4
#'
#'@noRd
PDFIndexFcst <- function(index_hind, index_obs, index_fcst, 
                         method ='ME', time_dim_name = 'time', 
                         na.rm = FALSE) {
  if (!is.character(time_dim_name)) {
    stop("Parameter 'time_dim_name' must be a character string indicating",
         " the name of the temporal dimension.")
  }
  if (length(time_dim_name) > 1) {
    warning("Parameter 'time_dim_name' has length greater than 1 and ",
            "only the first element will be used.")
    time_dim_name <- time_dim_name[1]
  }
  if(!(method %in% c('ME', 'LMEV'))){
    stop("Parameter 'method' must be equal to 'ME' or 'LMEV'")
  }
  if (!is.logical(na.rm)) {
    stop("Parameter 'na.rm' must be a logical value.")
  }
  if (!is.array(index_hind)) {
    stop("Parameter 'index_hind' must be an array.")
  }
  if (!is.array(index_obs)) {
    stop("Parameter 'index_obs' must be an array.")
  }
  if (!is.array(index_fcst)) {
    stop("Parameter 'index_fcst' must be an array.")
  }
  if (is.null(names(dim(index_hind))) || 
      is.null(names(dim(index_obs))) ||
      is.null(names(dim(index_fcst)))) {
    stop("Parameters 'index_hind', 'index_obs' and 'index_fcst' ",
         "should have dimmension names.")
  }
  if(!(time_dim_name %in% names(dim(index_hind)))) {
    stop("Parameter 'index_hind' must have temporal dimension.")
  }
  if(!(time_dim_name %in% names(dim(index_obs)))) {
    stop("Parameter 'index_obs' must have temporal dimension.")
  }
  if(!(time_dim_name %in% names(dim(index_fcst)))) {
    stop("Parameter 'index_fcst' must have temporal dimension.")
  }
  if (dim(index_obs)[time_dim_name] != dim(index_hind)[time_dim_name]) {
    stop("Length of temporal dimensions ",
         "of parameter 'index_obs' and 'index_hind' must be equals.")
  }
  if (dim(index_obs)[time_dim_name] <= 2) {
    stop("Length of temporal dimension ",
         "of parameter 'index_obs' and 'index_hind' must be greater than 2.")
  }
  if (dim(index_fcst)[time_dim_name] != 1) {
    stop("Length of temporal dimension ",
         "of parameter 'index_fcst' must be equal to 1.")
  }
  if((length(names(dim(index_hind))) != length(names(dim(index_fcst)))) 
     || (!all(names(dim(index_hind)) == names(dim(index_fcst))))){
    stop("Dimension names of parameter 'index_hind' and 'index_fcst' ",
         "must be equals.")
  }
  if('member' %in% names(dim(index_hind)) & 
     'statistic' %in% names(dim(index_hind))) {
    stop("Parameter 'index_hind' and 'index_fcst' must have at least ", 
         "dimension 'member' or 'statistic', not 'member' and 'statistic' ",
         "together.")
  }
  if(!('member' %in% names(dim(index_hind))) & 
     !('statistic' %in% names(dim(index_hind)))) {
    stop("Parameter 'index_hind' and 'index_fcst' must have dimension ", 
         "'member' or 'statistic'")
  }
  if ('member' %in% names(dim(index_hind))){
    if (dim(index_hind)['member'] == 1) {
      stop("Length of dimension 'member' ",
           "of parameter 'index_hind' must be greater than 1.")
    }
  }
  if ('member' %in% names(dim(index_fcst))){
    if (dim(index_fcst)['member'] == 1) {
      stop("Length of dimension 'member' ",
           "of parameter 'index_fcst' must be greater than 1.")
    }
  }
  if ('statistic' %in% names(dim(index_hind))){
    if (dim(index_hind)['statistic'] != 2) {
      stop("Length of dimension 'statistic' ",
           "of parameter 'index_hind' must be equal to 2.")
    }
  }
  if ('statistic' %in% names(dim(index_fcst))){
    if (dim(index_fcst)['statistic'] != 2) {
      stop("Length of dimension 'statistic' ",
           "of parameter 'index_fcst' must be equal to 2.")
    }
  }
  
  if ('member' %in% names(dim(index_hind))){
    PDFstatistics <- Apply(list(index_hind, index_obs, index_fcst),
                           target_dims = list(c(time_dim_name, 'member'), 
                                              time_dim_name,
                                              c(time_dim_name,'member')),
                           fun = .PDFIndexFcst, method, time_dim_name, na.rm)
  } else if ('statistic' %in% names(dim(index_hind))){
    PDFstatistics <- Apply(list(index_hind, index_obs, index_fcst),
                           target_dims = list(c(time_dim_name, 'statistic'), 
                                              time_dim_name,
                                              c(time_dim_name,'statistic')),
                           fun = .PDFIndexFcst, method, time_dim_name, na.rm)
  }
  return(PDFstatistics$output1)
}

#'Atomic PDFIndexFcst
#'@param index_hind Index (e.g. NAO index) array from a SFS with dimensions
#'  (time, member) or (time, statistic) for a hindcast period. The temporal 
#'  dimension, by default 'time', must be greater than 2.
#'@param index_obs Index (e.g. NAO index) array from an observational dataset
#'  or reanalysis with dimension (time). The temporal dimension, by default 
#'  'time', must be greater than 2.
#'@param index_fcst Index (e.g. NAO index) array from SFSs with dimensions 
#'  (time , member) or (time, statistic). The temporal dimension, by default 
#'  'time', must be equal to 1, the forecast year target. The dimension 'member' 
#'  must be greater than 1. The dimension 'statistic' must be equal to 2, for 
#'  containing the two paramenters of a normal distribution (mean and sd) 
#'  representing the ensemble of a SFS. It is not possible to have the dimension 
#'  'member' and  'statistic' together.
#'@param method A character string indicating which methodology is applied
#'  to compute the PDF. One of "ME" (default) or "LMEV".
#'@param time_dim_name A character string indicating the name of the temporal 
#'  dimension, by default 'time'.
#'@param na.rm Logical. Should missing values be removed?
#'@return .PDFIndexFcst Returns an array with dimensions 
#'(time = 1, statistic = 4). The firt statistic is the parameter 'mean' of the 
#'PDF with not bias corrected for the forecast year. The second statistic is the 
#'parameter 'standard deviation' of the PDF with not bias corrected for the 
#'forecast year. The third statistic is the parameter 'mean' of the PDF with 
#'bias corrected for the forecast year. The fourth statistic is the parameter 
#''standard deviation' of the PDF with bias corrected for the forecast year.
#'@import multiApply
#'@importFrom verification verify
#'@examples
#' # Example 1 for the Atomic PDFIndexFcst function
#' index_fcst <- 1 : (1 * 6)
#' dim(index_fcst) <- c(time = 1, member = 6)
#' index_obs <- 1 : 10
#' dim(index_obs) <- c(time = length(index_obs))
#' index_hind <- 1 : (10 * 3)
#' dim(index_hind) <- c(time = 10, member = 3)
#' res <- .PDFIndexFcst(index_hind, index_obs, index_fcst)
#' dim(res)
#' # time statistic
#' #  1         4
#' # Example 2 for the Atomic PDFIndexFcst function
#' index_fcst <- 1 : (1 * 2)
#' dim(index_fcst) <- c(time = 1, statistic = 2)
#' index_obs <- 1 : 10
#' dim(index_obs) <- c(time = 10)
#' index_hind <- 1 : (10 * 2)
#' dim(index_hind) <- c(time = 10, statistic = 2)
#' res <- .PDFIndexFcst(index_hind, index_obs, index_fcst)
#' dim(res)
#' # time statistic
#' #  1         4
#' @noRd
.PDFIndexFcst <- function(index_hind, index_obs, index_fcst,
                          method = 'ME', 
                          time_dim_name = 'time',
                          na.rm = FALSE) {
  dimnameshind <- names(dim(index_hind))
  if ('member' %in% dimnameshind) {
    pos <- match(c(time_dim_name, 'member'), names(dim(index_hind)))
    index_hind <- aperm(index_hind, pos)
    names(dim(index_hind)) <- c(time_dim_name, 'member')
    exp_fcst_ens_mean <- Apply(list(index_fcst), target_dims = 'member',
                               fun = mean)$output1
    exp_fcst_ens_sd <- Apply(list(index_fcst), target_dims = 'member',
                             fun = sd)$output1
  } else {
    pos <- match(c(time_dim_name,'statistic'), names(dim(index_fcst)))
    index_fcst <- aperm(index_fcst,pos)
    names(dim(index_fcst)) <- c(time_dim_name, 'statistic')
    exp_fcst_ens_mean <- index_fcst[,1]
    exp_fcst_ens_sd <- index_fcst[,2]
  }
  data_hind <- .PDFIndexHind(index_hind, index_obs, method, time_dim_name, na.rm)  
  exp_hind_ens_mean <- data_hind[,1]
  pdfnotbc_mean <-  exp_fcst_ens_mean
  pdfnotbc_sd <- Sigma_notBC(exp_hind_ens_mean, index_obs, na.rm)
  if (method == 'ME') {
    pdfbc <-  MEBC(exp_hind_ens_mean, index_obs, na.rm)
    pdfbc_mean  <- exp_fcst_ens_mean - pdfbc
  } else if (method == 'LMEV') {
    # linear model error-variance
    exp_fcst_ens_var <- exp_fcst_ens_sd^2
    dim_hind <- names(dim(index_hind))
    dim_obs <- names(dim(index_obs))
    coeff <- LMEV(index_hind, index_obs, dim_hind, dim_obs, time_dim_name, na.rm)
    pdfbc_mean <- exp_fcst_ens_mean - (coeff$coeff1 + coeff$coeff2*exp_fcst_ens_var)
  } else {
    stop("Parameter 'method' must be 'ME' or 'LMEV'.")
  }
  pdfbc_sd <-  Sigma_BC(data_hind[,3], index_obs, na.rm)
  data <- cbind(pdfnotbc_mean, pdfnotbc_sd, pdfbc_mean, pdfbc_sd)
  names(dim(data)) <- c(names(dim(index_obs)), 'statistic')
  return(data)
}

# Auxiliar function to compute the mean squared error between 'exp' and 'obs'
Sigma_notBC <- function(exp, obs, na.rm = FALSE) {
  if (na.rm){
    indNA <- c(which(is.na(exp)),which(is.na(obs)))
    if (length(indNA) > 0){
      exp <- exp[-indNA]
      obs <- obs[-indNA]
    }
  }
  return(sqrt(verify(obs, exp, frcst.type = "cont",
                     obs.type = "cont")$MSE))
}

# Auxiliar function to compute the bias between 'exp' and 'obs'
MEBC <- function(exp, obs, na.rm = FALSE) {
  if (na.rm){
    indNA <- c(which(is.na(exp)),which(is.na(obs)))
    if (length(indNA) > 0){
      exp <- exp[-indNA]
      obs <- obs[-indNA]
    }
  }
  return(verify(obs, exp, frcst.type = "cont",
                obs.type = "cont")$ME)
}

# Auxiliar function to compute the standard deviation of errors
# between 'obs' and 'exp'
Sigma_BC <- function(exp, obs, na.rm = FALSE) {
  return(sd(obs - exp, na.rm = na.rm))
}

# Auxiliar function to compute the linear model used in the method
# called 'LMEV' to correct the bias in the Best NAO weighting 
# methogology based on a linear model using ensemble variance of NAO index 
# as predictor.
LMEV <- function(exp, obs, dim_exp, 
                 dim_obs, time_dim_name = 'time', na.rm = FALSE) {
  names(dim(exp)) <- dim_exp
  names(dim(obs)) <- dim_obs
  if (any(names(dim(exp)) == 'member')) {
    exp_ens_mean <- Apply(list(exp), target_dims = 'member',
                          fun = mean, na.rm = na.rm)$output1
    exp_ens_sd <- Apply(list(exp), target_dims = 'member',
                        fun = sd, na.rm = na.rm)$output1
  } else {
    if(na.rm & any(is.na(exp))){
      print("Some value are NA")
    }
    pos <- match(c(time_dim_name,'statistic'), names(dim(exp)))
    exp <- aperm(exp,pos)
    exp_ens_mean <- exp[,1]
    exp_ens_sd <- exp[,2]
  }
  error <- exp_ens_mean - obs
  exp_ens_var <- exp_ens_sd^2
  statModel <- lm(error ~ exp_ens_var)
  return(list(coeff1 = statModel$coefficients[1], coeff2 = statModel$coefficients[2]))
}


