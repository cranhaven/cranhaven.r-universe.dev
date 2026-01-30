check_trafo_predict <- function(object, trafo) {
  
  if (!inherits(object, "trafo_lm")) { 
    stop(paste0("object is of class", class(object), " but it needs to be of 
                class trafo_lm." ))
  }
}