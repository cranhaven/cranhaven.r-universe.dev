#' Enhanced Kalman filter to reduce the noise of raw data
#'
#' @description Kalman filter reducing the noise in raw data if it is necessary.
#'
#' @param x A data frame with \code{\link[VBTree:VBTree-package]{VBTree}} style. Pay attention, all factors
#' in column names should be separated by "-" symbol, and factors for temperatures and strain rates should be
#' saved in pure numeric style.
#' @param manual An integer vector with the length of 3 where the 1st element denotes the layer for Stress and
#' Strain, the 2nd and 3rd elements represent the levels for Strain and Stress, respectively. The default setting is
#' NULL, which can call the function \code{\link[TPMplt:lyIDdetector]{lyIDdetector}} for automatic completion.
#' @param ... The arguments dV and dW passed on to \code{\link[dlm:dlmModPoly]{dlmModPoly}} in the function
#' \code{\link[dlm:dlmSmooth]{dlmSmooth}}.
#'
#' @return A data frame with the identical shape as input data, but with the smoothed stress values.
#' @importFrom dlm dlmSmooth
#' @importFrom dlm dlmModPoly
#' @export KFprocess
#' @seealso \code{\link[VBTree:VBTree-package]{VBTree}}, \code{\link[dlm:dlmModPoly]{dlmModPoly}}, \code{\link[dlm:dlmSmooth]{dlmSmooth}}
#'
#' @examples
#' \donttest{
#' # raw data without smoothing:
#' SSplots(TPMdata, 2, mfrow=c(2, 2))
#'
#' # Smoothing to reduce the noise:
#' KFdt <-KFprocess(TPMdata, dV = 0.3, dW = 0.006)
#' SSplots(KFdt, 2, mfrow=c(2, 2))
#' }
#' @keywords KFprocess
KFprocess <- function(x, manual=NULL, ...){
  # x <- TPMdata
  # manual <- NULL
  # # test for: dV = 0.3, dW = 0.006

  if(is.null(manual)){
    SSptr <- lyIDdetector(x)
    lySS <- as.numeric(SSptr$layer)
    strainID <- as.numeric(SSptr$strainID)
    stressID <- as.numeric(SSptr$stressID)
  } else {
    lySS <- manual[1]
    strainID <- manual[2]
    stressID <- manual[3]
  }

  vbt <- dl2vbt(chrvec2dl(colnames(x)))
  x1 <- rep(-1, length(vbt[[2]]))
  x1[lySS] <- strainID
  y1 <- x1
  y1[lySS] <- stressID
  strainvec <- as.vector(vbt2arr(vbtsub(vbt, x1)))
  stressvec <- as.vector(vbt2arr(vbtsub(vbt, y1)))

  rpt <- length(strainvec)

  for(i in 1:rpt){
    ts1 <- ts(x[,stressvec[i]])
    mod1 <- dlmModPoly(1, ...)
    s <- dlmSmooth(ts1, mod1)
    x[,stressvec[i]] <- as.vector(s$s)[1:(length(s$s)-1)]
  }
  return(x)
}


#' Calculating the adiabatic heating effect.
#'
#' @param x A data frame with \code{\link[VBTree:VBTree-package]{VBTree}} style. Pay attention, all factors
#' in column names should be separated by "-" symbol, and factors for temperatures and strain rates should be
#' saved in pure numeric style.
#' @param eta1 Efficiency of deformation heating.
#' @param rho1 Density of materials, unit is g/cm^3.
#' @param c1 Heat capacity of materials, unit is J/(kg*K).
#' @param ACorrect1 Coefficient to modify for density or heat capacity using different units. Default value
#' is 1.
#' @param manual1 An integer vector with the length of 3 where the 1st element denotes the layer for Stress and Strain,
#' the 2nd and 3rd elements represent the levels for Strain and Stress, respectively. The default setting is NULL, which
#' can call the function \code{\link[TPMplt:lyIDdetector]{lyIDdetector}} for automatical completion this vector.
#'
#' @return A data frame with the identical shape as input data. The values of stress was replaced by adiabatic heating effect.
#' @export T_get
#'
#' @examples
#' # Constants of steels as example:
#' T_get(TPMdata, 0.9, 7.8, 502.416)
#' @keywords internals
T_get <- function(x, eta1, rho1, c1, ACorrect1=1, manual1=NULL){
  # manual <- NULL
  # x <- TPMdata
  #
  # eta1 <- 0.9
  # rho <- 7.8
  # c <- 502.416
  # ACorrect <- 1

  if(is.null(manual1)){
    SSptr <- lyIDdetector(x)
    lySS <- as.numeric(SSptr$layer)
    strainID <- as.numeric(SSptr$strainID)
    stressID <- as.numeric(SSptr$stressID)
  } else {
    lySS <- manual1[1]
    strainID <- manual1[2]
    stressID <- manual1[3]
  }

  vbt <- dl2vbt(chrvec2dl(colnames(x)))
  inqx <- rep(-1, length(vbt[[2]]))
  inqy <- inqx

  inqx[lySS] <- strainID
  inqy[lySS] <- stressID

  x1 <- as.vector(vbt2arr(vbtsub(vbt, inqx)))
  y1 <- as.vector(vbt2arr(vbtsub(vbt, inqy)))

  Delta_T <- x

  for (i in 1:length(x1)) {
    work <- c(0, x[,x1[i]])[-(length(x[,x1[1]])+1)] * x[,y1[i]] # infinitesimal of integral
    Delta_T[,y1[i]] <- cumsum(work) * (eta1/(rho1*c1)) * ACorrect1 # unit: rho = g/cm^3; c = J/(kg*K)
  }
  return(Delta_T)
}


#' Adiabatic heating effect correction
#'
#' @param x A data frame with \code{\link[VBTree:VBTree-package]{VBTree}} style. Pay attention, all factors
#' in column names should be separated by "-" symbol, and factors for temperatures and strain rates should be
#' saved in pure numeric style.
#' @param lyT An integer to specify the layer for temperature attribute in the vector binary tree.
#' @param lySR An integer to specify the layer for strain rate attribute in the vector binary tree.
#' @param eta Efficiency of deformation heating.
#' @param rho Density of materials, unit is g/cm^3.
#' @param c Heat capacity of materials, unit is J/(kg*K).
#' @param StrainSeq A vector to specify the sequence for flow strain.
#' @param ACorrect Coefficient to modify for density or heat capacity using different units. Default value
#' is 1.
#' @param manual An integer vector with the length of 3 where the 1st element denotes the layer for Stress and Strain,
#' the 2nd and 3rd elements represent the levels for Strain and Stress, respectively. The default setting is NULL, which
#' can call the function \code{\link[TPMplt:lyIDdetector]{lyIDdetector}} for automatical completion this vector.
#' @param Amplifier Amplifier for linear fitting. Default value is 1000.
#'
#' @return A data frame with \code{\link[VBTree:VBTree-package]{VBTree}} style including the adiabatic heating corrected
#' flow stress using the StrainSeq as conditions.
#' @export TCorrect
#'
#' @examples
#' \donttest{
#' # Constants of steels as example:
#' dt_TC <- TCorrect(TPMdata, 2, 3, 0.9, 7.8, 502.416, seq(0, 0.9, 0.005))
#'
#' # Data without adiabatic heating correction
#' SSplots(TPMdata, 2, mfrow=c(2, 2))
#'
#' # Data with adiabatic heating correction
#' SSplots(dt_TC, 2, mfrow=c(2, 2))
#' }
#' @keywords TCorrect
TCorrect <- function(x, lyT, lySR, eta, rho, c, StrainSeq, ACorrect=1, manual=NULL, Amplifier=1000){
  # x <- TPMdata
  # manual <- NULL
  # lyT <- 2
  # lySR <- 3
  # eta <- 0.9
  # rho <- 7.8
  # c <- 502.416
  # StrainSeq <- seq(0, 0.9, 0.02)

  x_TC <- as.data.frame(matrix(NA, length(StrainSeq), length(x[1,])))
  colnames(x_TC) <- colnames(x)

  if(is.null(manual)){
    SSptr <- lyIDdetector(x)
    lySS <- as.numeric(SSptr$layer)
    strainID <- as.numeric(SSptr$strainID)
    stressID <- as.numeric(SSptr$stressID)
  } else {
    lySS <- manual[1]
    strainID <- manual[2]
    stressID <- manual[3]
  }

  Delta_T <- T_get(x, eta1 = eta, rho1 = rho, c1 = c, ACorrect1 = ACorrect, manual1 = manual)

  vbt <- dl2vbt(chrvec2dl(colnames(x)))
  inq_ctr <- rep(-1, length(vbt[[2]]))
  for (i in 1:length(StrainSeq)) {
    dt <- epsExtract(x, StrainSeq[i], lyT, lySR, manual = manual)[[1]]
    Temp <- epsExtract(Delta_T, StrainSeq[i], lyT, lySR, manual = manual)[[1]]
    for (j in 1:vbt[[2]][lySR]) {
      inq_ctr[lySR] <- j
      inq_ctr[lySS] <- stressID
      colnames1 <- as.vector(vbt2arr(vbtsub(vbt, inq_ctr)))
      T1 <- Amplifier/(as.numeric(colnames(dt))+273.15)
      mod1 <- as.vector(lm(dt[j,]~T1)[[1]])
      T11 <- Amplifier/(as.numeric(colnames(dt))+273.15-as.vector(Temp[j,]))
      x_TC[i,colnames1]<- mod1[1]+mod1[2]*T11

      inq_ctr[lySS] <- strainID
      colnames1 <- as.vector(vbt2arr(vbtsub(vbt, inq_ctr)))
      x_TC[i,colnames1] <- rep(StrainSeq[i], length(colnames1))
    }
  }
  return(x_TC)
}
