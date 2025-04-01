prepanel.trim <- function(x, y, trim.x = 0, trim.y = .05, min.trim = 20,... )
{
   ## check the args
   for(nm in c("trim.x", "trim.y")){
      z <- get(nm)
      if(!isTRUE( length(z) == 1 && is.numeric(z) && is.finite(z)
                  && z >= 0 && z < .5)){
         warning(paste("Bad ",nm, " argument. Using default."))
         assign(nm, formals()[[nm]])
      }
   }
   if(!isTRUE(length(min.trim) == 1 && is.numeric(min.trim) &&
              min.trim >= 0 && is.finite(min.trim))){
      warning( "Bad min.trim argument. Using default")
      min.trim <- 20
   }
   x <- if(is.numeric(x)) x[is.finite(x)] else x
   y <- if(is.numeric(y)) y[is.finite(y)] else y
   if(!isTRUE(length(x) > 0 && length(y) > 0)) {
      warning("Can't calculate prepanel.trim limits -- no finite values.",
              "\nNA returned\n")
      return(NA)
   }
   f <- function(z,trim){
      if(is.factor(z)){
         list(unique(as.numeric(z)), levels(z))
      } else if(!isTRUE(is.numeric(z) && all(is.finite(z)))) {
         warning("prepanel.trim limits can only be computed for ",
                 "finite numerics or factors: NA returned\n")
         return(NA)
      } else if(length(z) < min.trim){ list(range(z))
      } else list(quantile(z, probs = c(trim, 1-trim), type = 8))
   }
   out <- lapply(list(x="x",y ="y"),function(z){
      f(get(z),get(paste0("trim.",z)))}
   )
   for(nm in names(out)){
      if(length(out[[nm]]) == 1){
         names(out[[nm]])<- paste0(nm,"lim")} else{
            names(out[[nm]])<- paste0(nm,c("at","lim"))}
   }
   structure(unlist(out, recursive = FALSE), names = unlist(lapply(out,names)))
}
