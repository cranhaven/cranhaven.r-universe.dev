acoarm <- function (data, svtime, event, treatment, BaselineMarker, 
                     subcohort, esttype = 1, augment = 1, weight=NULL, extra=NULL)
{
  
  if (! augment %in% c(0,1,2) ) 
  stop("augment variable must be either 0, 1, or 2 only.")  
  
  #subcohort was drawn from both the active treatment arm and the placebo arm 
  if (augment==2)
  {
    res <- aco2arm(data, svtime, event, treatment, BaselineMarker,subcohort, esttype, weight, extra) 
  }else #subcohort was drawn from either the active treatment arm or the placebo arm
  {
    res <- aco1arm(data, svtime, event, treatment, BaselineMarker,subcohort, esttype, augment, extra) 
  }
  res  
}