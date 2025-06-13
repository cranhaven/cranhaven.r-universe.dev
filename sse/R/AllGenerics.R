## calculates the array with the power, stores it into powCalc
setGeneric(name = "powCalc",
           def = function(object, statistic, n.iter = NA, cluster = FALSE){
             standardGeneric("powCalc")
           })

## for generating the example object in one step (replacing powEx and
## merge that are kept for back compatibility (and internal usage))
setGeneric(name = "powEx",
           def = function(x, theta,
                          xi = NA,
                          endpoint = NA,
                          power = 0.9,
                          drop = 0,
                          method = c("default", "lm", "step"),
                          lm.range = NA,
                          forceDivisor = FALSE){
             standardGeneric("powEx")
           })

setGeneric(name = "powFunGen",
           def = function(object, ...){
             standardGeneric("powFunGen")
           })

setGeneric(
           name = "workhorse",
           def = function(object, ...){
             standardGeneric("workhorse")
           })

## for programming on powPar parameters
setGeneric(name = "pp",
           def = function(x, name){
             standardGeneric("pp")
           })

## for programming on powPar parameters
setGeneric(name = "n",
           def = function(x){
             standardGeneric("n")
           }
           #,
           # useAsDefault = function(object = pow, ...){object@n.act}
           )

setGeneric(name = "refine",
           def = function(object, factor = 10){
             standardGeneric("refine")
           })

## for programming on powPar parameters
setGeneric(name = "theta",
           def = function(x){
             standardGeneric("theta")
           })

## for programming on powPar parameters
setGeneric(name = "xi",
           def = function(x){
             standardGeneric("xi")
           })

## estimats n from objects of class "power"
setGeneric(name = "sampleSize",
           def = function(x, ...) {
             standardGeneric("sampleSize")
           })
## estimats n from power
setGeneric(name = "exDat",
           def = function(x, y, ...) {
             standardGeneric("exDat")
           })

## estimats n from power
setGeneric(name = "inspect",
           def = function(object) {
             standardGeneric("inspect")
           })

## for reporting the sampling schema
setGeneric(name = "tex",
           def = function(x, type, ...){
             standardGeneric("tex")
           })
