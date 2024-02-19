list2mkm <- function(list_of_models){ #rever e completar os outros slots
  model <- methods::new('mkm')
  model@km <- list_of_models
  model@design <- as.data.frame(list_of_models[[1]]@X)
  model@response <- as.data.frame(lapply(list_of_models, function(model) model@y))
  model@d <- ncol(model@design)
  model@n <- nrow(model@design)
  model@m <- length(model@objective)
  model@j <- length(model@km) - model@m
  return(model)
}

mco2ps <- function(mco){
  if(all(class(mco)!='mco'))
    stop('object must be of "mco" class')
  ps <- NULL
  ps$set <- mco$value
  ps$x <- mco$par
  ps$index <- NULL
  ps$n <- nrow(ps$set)
  ps$m <- ncol(ps$set)
  class(ps) <- 'ps'
  return(ps)
}

normalize <- function(data, ran = NULL){
  if (is.null(ran))
    ran <- apply(data, 2, range)
  data <- data.frame(t(apply(data, 1, function(data) (data - ran[1,])/(ran[2,]-ran[1,]))))
  return(data)
} # arrumar problema de NA (divizao por 0 )
