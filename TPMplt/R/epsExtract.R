#' Auto output for strain rate vs. temperature table
#'
#' @description Automatically output the strain rate vs. temperature table, by a specified strain condition.
#' @param data A data frame with \code{\link[VBTree:VBTree-package]{VBTree}} style. Pay attention, all factors in column names
#' should be separated by "-" symbol, and factors for temperatures and strain rates should be saved in pure numeric style.
#' @param eps A numeric value to specify strain condition.
#' @param lyT An integer to specify the layer for temperature attribute in the vector binary tree.
#' @param lySR An integer to specify the layer for strain rate attribute in the vector binary tree.
#' @param manual An integer vector with the length of 3 where the 1st element denotes the layer for Stress and Strain,
#' the 2nd and 3rd elements represent the levels for Strain and Stress, respectively. The default setting is NULL, which
#' can call the function \code{\link[TPMplt:lyIDdetector]{lyIDdetector}} for automatical completion this vector.
#'
#' @return A list consist of a matrix table arranged by rows for strain rates while columns for temperatures, and a numeric
#' value as strain condition for this strain rate-temperature table.
#' @import VBTree
#' @export epsExtract
#' @seealso \code{\link[VBTree:VBTree-package]{VBTree}}, \code{\link[TPMplt:lyIDdetector]{lyIDdetector}}
#'
#' @examples
#' require(VBTree)
#' # Find locations for temperature and strain rate:
#' dl2vbt(chrvec2dl(colnames(TPMdata)))
#' epsExtract(TPMdata, eps = 0.7, lyT = 2, lySR = 3)
#' @keywords SR-T.table epsExtract lyIDdetector
epsExtract <- function(data, eps, lyT, lySR, manual=NULL){
  # data: data.frame format
  # eps: epsilon of a specific strain
  # lyT: an integer to specify the layer of temperature in vector binary tree;
  # lySR: an integer to specify the layer of strain rate in vector binary tree;
  # manual: should be a vector with length 3 while the 1st integer indicate the layer of Stress-Strain,
  #         the 2nd and 3rd to identify the dimensions of Strain and Stress, respectively.

  # # test section
  # data <- TPMdata
  # manual <- NULL
  # lyT <- 2
  # lySR <- 3
  # eps <- 0.7

  # input data diagnose:
  if(!is.data.frame(data)){
    warning("input data will be convert to data.frame.", call. = FALSE)
    data <- as.data.frame(data)
  }

  # check for epsilon:
  if(!is.numeric(eps)|length(eps)!=1){
    stop("the arg eps should be a numer.", call. = FALSE)
  }

  # method of manual:
  if(is.null(manual)){
    SSptr <- lyIDdetector(data)
    lySS <- as.numeric(SSptr$layer)
    strainID <- as.numeric(SSptr$strainID)
    stressID <- as.numeric(SSptr$stressID)
  } else {
    lySS <- manual[1]
    strainID <- manual[2]
    stressID <- manual[3]
  }

  # make sub double lists to generate strain and stress vector binary trees
  subdl <- chrvec2dl(colnames(data))
  subdl <- vbt2dl(dl2vbt(subdl)) # Fix the order

  vbtfull <- dl2vbt(subdl)

  skip_layer <- c(lySS, lyT, lySR) # the layers which should be maintained
  unfixlayerlevels <- vbtfull[[2]][-skip_layer]
  trvs.time <- prod(unfixlayerlevels)

  # make sub vector binary trees for strain and stress
  strain.subdl <- subdl
  strain.subdl[[lySS]] <- subdl[[lySS]][strainID]
  strain.vec <- as.vector(dl2arr(strain.subdl))

  stress.subdl <- subdl
  stress.subdl[[lySS]] <- subdl[[lySS]][stressID]
  stress.subarr <- dl2arr(stress.subdl)
  stress.vec <- as.vector(stress.subarr)

  trvsfull <- trvs(vbtfull)
  arrfull <- vbt2arr(vbtfull)

  rpt1 <- length(dim(arrfull)) - 1

  layerlevels <- dl2vbt(subdl)[[2]]

  dataints <- array(NA, dim = c(layerlevels[lyT], layerlevels[lySR], trvs.time))

  testmat <- array(c(1:28), dim=c(7,4,1))

  rpt2 <- length(strain.vec)
  for (i in 1:rpt2){
    strain.idx <- which(trvsfull[,1] == strain.vec[i])
    stress.idx <- which(trvsfull[,1] == stress.vec[i])

    strain.coord <- as.vector(trvsfull[strain.idx,2][[1]])
    stress.clname <- trvsfull[stress.idx,1][[1]]

    strain <- data[,trvsfull[strain.idx,1][[1]]]
    stress <- data[,trvsfull[stress.idx,1][[1]]]

    idx <- which.min(abs(strain-eps))
    fill <- stress[idx]

    #print(fill)

    stress.subarr[[which(stress.subarr== stress.clname)]] <- fill
  }

  result_mat <- array(NA, dim = dim(stress.subarr))
  for (i in 1:length(stress.subarr)){
    result_mat[[i]] <- as.numeric(stress.subarr[[i]])
  }

  result_mat <- apply(result_mat, c(lySR, lyT), mean)

  rownames(result_mat) <- subdl[[lySR]]
  colnames(result_mat) <- subdl[[lyT]]

  result <- result_mat
  result <- list("SRT.table"=result, "epsilon"=eps)

  class(result) <- "SR-T.table"
  return(result)
}
