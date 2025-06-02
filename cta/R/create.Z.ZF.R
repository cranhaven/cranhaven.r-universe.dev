######################  begin create.Z.ZF ######################################

create.Z.ZF <- function(strata,nrowZ=length(strata),fixed.strata="all") {
  #
  #   Author:  Joseph B. Lang,  Univ of Iowa
  #   Created: 4/29/09
  #
  #   This program uses strata and sampling constraint information found in
  #   input variables 'strata' and 'fixed.strata' to create the
  #   population (aka strata) matrix Z and the sampling constraint matrix ZF.
  #
  #   This program is used in mph.fit, versions 3.0 and above.
  #
  A <- factor(strata)
  if (length(levels(A)) > 1) {Z <- model.matrix(~A-1)}
  else {Z <- matrix(1,nrowZ,1); dimnames(Z)[[2]] <- list(paste(sep="","A",levels(A)))}
  dZ <- dimnames(Z)[[2]]
  dZ <- substr(dZ,2,nchar(dZ))
  dimnames(Z)[[2]] <- dZ
  
  allnone <- 0
  if ((length(fixed.strata)==1)&(fixed.strata[1] == "all" )&!("all" %in% dZ)) {ZF <- Z; allnone <-1}
  if ((length(fixed.strata)==1)&(fixed.strata[1] == "none")&!("none" %in% dZ)) {ZF <- 0; allnone <-1}
  if (allnone == 0) {
    ZF <- as.matrix(Z[,dZ %in% fixed.strata])
  }
  list(Z=Z,ZF=ZF)
}

######################  end create.Z.ZF    #####################################