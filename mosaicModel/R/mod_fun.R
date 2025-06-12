#' Transforms a model into a function of inputs -> output
#' 
#' Implicit in many statistical models is a function that takes the explanatory 
#' variables as inputs and returns the corresponding model value at those inputs.
#' `mod_fun` creates an R function that works this way. The function returned 
#' by `mod_fun`` has arguments named for each of the explanatory variables. In calling
#' that returned function, you can specify as many or as few of these as you like.  
#' 
#' @return a function whose arguments are the explanatory variable used in the model
#' 
#' @details When you evaluate the function, you can set the values of all, any, or none of
#' the arguments. Any arguments that you do not set will automatically be set 
#' to "typical values" as in `mod_eval`. 
#' 
#' There's nothing essential about the behavior of `mod_eval`` that explicitly
#' names the arguments to the model function with the names of the explanatory variables.
#' This has been done purely for pedagogical reasons, as a reminder of what those variables
#' are and to make it possible to spot mistaken inputs to models.
#' 
#' @param mod the model to be rendered in a functional form
#' @param nlevels the number of levels for which to find "typical levels" 
#' for those arguments not specified in the call to the returned function
#' @examples
#' my_mod <- lm(mpg ~ hp * cyl, data = mtcars)
#' f <- mod_fun(my_mod)
#' names(formals(f)) # the arguments will be the explanatory variables
#' f(hp = 1:2)
#' f(hp = 1:2, cyl = 3:4)
#' f() # typical values for inputs
#' @export
mod_fun <- function(mod, nlevels = 1) {
  xvars <- explanatory_vars(mod)
  args <- paste("alist(", paste(xvars, "=", collapse = ",", 
                                sep = ""), ")")
  args <- eval(parse(text = args))
  f <- function(...) {
    the_call <- sys.call()
    the_call[[1]] <- list
    args <- eval(the_call) # a list with the arguments handed off to f()
    args <- c(list(mod), args, list(nlevels = nlevels))
    do.call(mod_eval, args)
    
  }
  formals(f) <- args
  
  f
}
