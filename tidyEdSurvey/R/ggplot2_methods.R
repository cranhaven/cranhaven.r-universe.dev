#' @importFrom cli cli_abort
#' @importFrom rlang quo_name
#' @importFrom stats reshape

#' @importFrom ggplot2 fortify
#' @export
fortify.edsurvey.data.frame <- function(model,data,...){
  vars <- model$dataList$Student$lafObject@column_names
  vars <- vars[vars!="version"]
  # if the edsurvey.data.frame has been attached, we'll just bind
  # everything together
  try(z <- data.frame(sapply(vars,FUN=function(v){as.name(eval(v))},
                             simplify = TRUE,USE.NAMES = TRUE)),
      silent=TRUE)

  if(exists("z")){
    ggplot2::fortify(z,data,...)
  }else{ # otherwise, we need to call getdata
    suppressWarnings(
      z <- EdSurvey::getData(model, varnames=vars,
                   addAttributes=TRUE, returnJKreplicates=FALSE)
    )
    ggplot2::fortify(z,data,...)
  }
}

#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @export
ggplot.edsurvey.data.frame <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    cli::cli_abort(c(
      "{.arg mapping} should be created with {.fn aes}.",
      "x" = "You've supplied a {.cls {class(mapping)[1]}} object"
    ))
  }
  sdf <- data
  data <- fortify(data, ...)

  if(hasPlausibleValue(quo_name(mapping$x),sdf) & !hasPlausibleValue(quo_name(mapping$y),sdf)) {
    pv_vars <- getPlausibleValue(quo_name(mapping$x),sdf)
    data$idvar <- as.character(1:nrow(data))
    data <- reshape(data=data,
                    idvar = "idvar",
                    varying = list(pv_vars),
                    direction="long",
                    v.names=quo_name(mapping$x))
  }

  if(!hasPlausibleValue(quo_name(mapping$x),sdf) & hasPlausibleValue(quo_name(mapping$y),sdf)) {
    pv_vars <- getPlausibleValue(quo_name(mapping$y),sdf)
    data$idvar <- as.character(1:nrow(data))
    data <- reshape(data=data,
                    idvar = "idvar",
                    varying = list(pv_vars),
                    direction="long",
                    v.names=quo_name(mapping$y))
  }

  if(hasPlausibleValue(quo_name(mapping$x),sdf) & hasPlausibleValue(quo_name(mapping$y),sdf)) {
    pv_vars1 <- getPlausibleValue(quo_name(mapping$x),sdf)
    pv_vars2 <- getPlausibleValue(quo_name(mapping$y),sdf)
    data$idvar <- as.character(1:nrow(data))
    data <- reshape(data = data,
            idvar = "idvar",
            varying = list(pv_vars1,pv_vars2),
            direction="long",
            v.names = c(quo_name(mapping$x),quo_name(mapping$y)))
  }

  NextMethod()

}
