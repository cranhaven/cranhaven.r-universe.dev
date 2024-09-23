checkdata <- function(formula,exposure_coeff,genotype_columns,data)
{
  if (inherits(exposure_coeff,"data.frame"))
  {
    tmp=exposure_coeff[,1]
    names(tmp)=rownames(exposure_coeff)
    exposure_coeff=tmp
  }
  call=match.call()
  indx <- match(c("formula", "exposure_coeff" ,"genotype_columns","data"),
                names(call), nomatch = 0L)
  if (indx[1] == 0L)
    stop("A 'formula' argument is required")
  if (indx[2] == 0L)
    stop("An 'exposure_coeff' argument is required")
  if (indx[3] == 0L)
    stop("A 'genotype_columns' argument is required")
    if (indx[4] == 0L)
  stop("A 'data' argument is required")
  if (inherits(formula,"character")) 
    formula=as.formula(formula)
  
  #check data related to formula
  outcome=as.character(formula)[2]
  if (inherits(data[,outcome],"factor"))
  {
    res=tryCatch(glm(formula,family="binomial",data=data),error = function(err){stop(paste0("When check the data related to formula,", err))})
  }else
  {
    res=tryCatch(glm(formula,data=data),error = function(err){stop(paste0("When check the data related to formula,", err))})
  }
  
  #check the exposure coefficients
  if (is.null(names(exposure_coeff)) | !inherits(exposure_coeff, "numeric")) 
    stop("The exposure coefficients should be a named vector or dataframe.")
  for (i in 1:length(exposure_coeff))
  {
    if (! names(exposure_coeff)[i] %in% colnames(data))
      stop(paste0("Exposure coefficient: ",names(exposure_coeff)[i]," is not found in the data frame."))
  }
  if (sum(names(exposure_coeff) %in% colnames(data)[genotype_columns]) != length(exposure_coeff)) 
    stop("some exposure coefficients were not found in the genotype_columns of data frame")
}
