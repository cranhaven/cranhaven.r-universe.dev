# Internal function to extracting the relevant parameters from the copula object
# called by gofco() and gof()
.extract.cop.object = function(copulaobject) {
  dispstr <- "ex"
  switch(class(copulaobject),
         normalCopula = {
           copula <- "normal"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
           dispstr <- copulaobject@dispstr
         },
         tCopula = {
           copula <- "t"
           param <- copulaobject@parameters[-length(copulaobject@parameters)]
           param.est <- if (is.na(copulaobject@parameters[1])) {
             TRUE
           } else {
             FALSE
           }
           df <- copulaobject@parameters[length(copulaobject@parameters)]
           df.est <- if (copulaobject@df.fixed == FALSE) {
             TRUE
           } else if (copulaobject@df.fixed == TRUE) {
             FALSE
           }
           dispstr <- copulaobject@dispstr
         },
         claytonCopula = {
           copula <- "clayton"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
         },
         gumbelCopula = {
           copula <- "gumbel"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
         },
         frankCopula = {
           copula <- "frank"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
         },
         joeCopula = {
           copula <- "joe"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
         },
         amhCopula = {
           copula <- "amh"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
         },
         galambosCopula = {
           copula <- "galambos"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
         },
         huslerReissCopula = {
           copula <- "huslerReiss"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
         },
         tawnCopula = {
           copula <- "tawn"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
         },
         tevCopula = {
           copula <- "tev"
           param <- copulaobject@parameters[-length(copulaobject@parameters)]
           param.est <- if (is.na(copulaobject@parameters[1])) {
             TRUE
           } else {
             FALSE
           }
           df <- copulaobject@parameters[length(copulaobject@parameters)]
           df.est <- if (copulaobject@df.fixed == FALSE) {
             TRUE
           } else if (copulaobject@df.fixed == TRUE) {
             FALSE
           }
         },
         fgmCopula = {
           copula <- "fgm"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
         },
         plackettCopula = {
           copula <- "plackett"
           param <- copulaobject@parameters
           param.est <- if (is.na(copulaobject@parameters)) {
             TRUE
           } else {
             FALSE
           }
           df <- 4
           df.est <- TRUE
         },
         stop("The class of the object is not supported.")
  )
  return(list(copula, param, param.est, df, df.est, dispstr))
}