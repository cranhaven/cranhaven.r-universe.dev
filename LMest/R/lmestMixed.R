lmestMixed <- function(responsesFormula = NULL,
                       data, index, k1, k2, start = 0,
                       weights = NULL, tol = 10^-8, maxit = 1000,
                       out_se = FALSE, seed = NULL)
{

  if(inherits(data, "lmestData"))
  {
    data <- data$data
  }else if(!is.data.frame(data))
  {
    data <- as.data.frame(data)
    stop("A data.frame must be provided")
  }

  if(length(index) != 2)
  {
    stop("index and time must be indicated")
  }

  if(!is.null(seed))
  {
    set.seed(seed)
  }

  if(length(index) !=2)
  {
    stop("id and time must be provided")
  }
  id.which <- which(names(data) == index[1])
  tv.which <- which(names(data) == index[2])

  if(length(id.which) == 0)
  {
    stop("the id column does not exist")
  }

  if(length(tv.which) == 0)
  {
    stop("the time column does not exist")
  }

  id <- data[,id.which]
  tv <- data[,tv.which]

  if(is.character(id) | is.factor(id))
  {
    warning("id column must be numeric. Coerced in numeric.", call. = FALSE)
    id <- as.numeric(id)
  }

  if(is.character(tv) | is.factor(tv))
  {
    warning("time column must be numeric. Coerced in numeric.", call. = FALSE)
    tv <- as.numeric(tv)
  }

  data.new <- data[,-c(id.which,tv.which), drop = FALSE]

  if(is.null(responsesFormula))
  {
    Y <- data.new
  }else{
    temp <- getResponses(data = data,formula = responsesFormula)
    Y <- temp$Y
  }



  tmp <- long2matrices.internal(Y = Y, id = id, time = tv,yv = weights,
                          Xinitial = NULL, Xmanifest = NULL, Xtrans = NULL)
  Y <- tmp$Y
  if(any(is.na(Y)))
  {
    stop("Missing data in the dataset")
  }
  if(is.null(weights))
  {
    freq = tmp$freq
  }else{
    freq = weights
    if(nrow(Y)!=length(weights)) stop("dimensions mismatch between data and weights")
  }
  if(min(Y,na.rm=TRUE)>0){
    cat("|------------------- WARNING -------------------|\n")
    cat("|The first response category must be coded as 0 |\n")
    cat("|-----------------------------------------------|\n")
    for(i in 1:dim(Y)[3])
    {
      Y[,,i] <- Y[,,i]-min(Y[,,i],na.rm = TRUE)
    }
  }
  out <- (lmmixed(S = Y,yv = freq, k1 = k1,k2 = k2,
                          start = start, tol = tol, maxit = maxit, out_se = out_se))

  class <- class(out)
  out <- do.call(c, list(out,
                        list(call = match.call(),data = data)))
  #out <- append(out, list(call = match.call(),data = data))
  attributes(out)$responsesFormula = responsesFormula
  attributes(out)$whichid = id.which
  attributes(out)$whichtv = tv.which
  attributes(out)$id = id
  attributes(out)$time = tv
  class(out) <- class
  return(out)
}


