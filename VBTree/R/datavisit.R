#' Extract subset of data using different methods
#'
#' @description Extract the subset of data by column names using tensor, array, double list, integer vector, or vector binary tree.
#' @param data A data.frame with structured column names.
#' @param inq An argument to determine the subset to be extracted by column names. A tensor, array, double list, integer vector and
#'  vector binary tree is available format of \code{inq}.
#'
#' @return Return a list which contains the item index, column name, column coordinate and the data in corresponding column for each
#' element contained in the assignment of \code{inq}.
#' @export datavisit
#' @seealso \code{\link[VBTree:vbtinq]{vbtinq}}, \code{\link[VBTree:advbtinq]{advbtinq}}, \code{\link[VBTree:trvseleinq]{trvseleinq}},
#' \code{\link[VBTree:trvsidxinq]{trvsidxinq}}, \code{\link[VBTree:trvssubinq]{trvssubinq}}.
#'
#' @examples
#' #View the data to be visited:
#' summary(datatest)
#' colnames(datatest)
#'
#' #Structurize colnames of data into vector binary tree:
#' dl <- chrvec2dl(colnames(datatest))
#' vbt <- dl2vbt(dl)
#' vbt
#'
#' #Setting subset in different forms, for example the pattern
#' #"Strain-(900~1100)-(0.01, 1)-0.6" is desired:
#' subunregdl <- list(c(1), c(1:5), c(2,4), c(1)) # undefined double list
#' subregdl <- advbtinq(vbt, subunregdl) # regularized double list
#' subvbt <- dl2vbt(subregdl) # sub vector binary tree
#' subts <- vbt2ts(subvbt) # tensor
#' subarr <- vbt2arr(subvbt) # array
#' subchrvec <- as.vector(subarr) # character vector
#'
#' #Visit the data through different methods:
#' datavisit(datatest, subunregdl) # by handmade double list
#' datavisit(datatest, subregdl) # by defined double list
#' datavisit(datatest, subvbt) # by vector binary tree
#' datavisit(datatest, subts) # by tensor
#' datavisit(datatest, subarr) # by array
#' datavisit(datatest, subchrvec) # by character vector
#' @keywords data.frame tensor array Double.List vector Vector.Binary.Tree
datavisit <- function(data, inq){

  # data input diagnose
  if (!is.data.frame(data)){
    warning("Your data format will be convert into data.frame.", call. = FALSE)
    data <- as.data.frame(data)
  }

  # treat different methods to vector binary tree visit
  if(inherits(inq, "tensor")){
    inq <- ts2vbt(inq)
  }
  if(inherits(inq, "array")){
    inq <- arr2vbt(inq)
  }
  if(inherits(inq, "Double.List")){
    inq <- dl2vbt(inq)
  }
  titles <- as.character(colnames(data))
  dltitles <- chrvec2dl(titles)
  vbttitles <- dl2vbt(dltitles)
  if(length(inq)==length(vbttitles$dims) & all(is.numeric(unlist(inq)))){
    inq <- advbtsub(vbttitles, inq = inq) # deal with handmade double list
  }
  if(is.vector(inq) & all(is.character(inq))){
    inq <- dl2vbt(chrvec2dl(inq)) # deal with character element or vector
  }
  if(is.vector(inq) & all(is.numeric(inq))){
    if(all(inq==round(inq))){
      inq <- vbtsub(vbttitles, inq = inq)
    } else {
      warning("all elements in inq will be converted into integers.", call. = FALSE)
      inq <- vbtsub(vbttitles, inq = round(inq))
    }
  }
  if (!inherits(inq, "Vector.Binary.Tree")){
    stop("input argument inq must be a character vector, an integer vector, a double list, a tensor, an array or a vector binary tree.", call. = FALSE)
  }

  trvstitles <- trvs(vbttitles)
  titlestrvschart <- trvssubinq(trvstitles, inq)

  # special treatment for single visit using integer vector
  if(prod(inq$dims)==1){
    coldata <- data[,titlestrvschart[[2]]]
    result <- list("itemid"=titlestrvschart[[1]], "colnames"=titlestrvschart[[2]], "coordinate"=titlestrvschart[[3]], "coldata"=coldata)
  } else {
    rpt <- length(titlestrvschart[,1])

    # attach export data into the "Trav.Inq" and make new class
    i <- 1
    result <- list()
    for (i in 1:rpt) {
      coldata <- data[, titlestrvschart[i,2][[1]]] # mapping
      item <- list("itemid"=titlestrvschart[i,1][[1]], "colnames"=titlestrvschart[i,2][[1]], "coordinate"=titlestrvschart[i,3][[1]], "coldata"=coldata)
      result <- rbind(result, item)
    }
  }

  class(result) <- "Trav.Inq.Data"
  return(result)
}
