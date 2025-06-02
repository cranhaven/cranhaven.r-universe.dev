##############  begin M.fct ############################################


M.fct <- function(strata,ncells=length(strata)) {
  #
  #   Author:  Joseph B. Lang,  Univ of Iowa
  #   Created: 5/1/09
  #
  #   This program uses strata information found in
  #   input variable 'strata'  to create the marginalizing matrix M.
  #   That is,  M%*%p  gives the marginal probabilities corresponding to the
  #   levels of factor strata.
  #
  #   NOTE!  Marginals are ordered according to the levels of factor strata.
  #
  #   Example...
  #    V1   V2      y
  #     b   yes    15
  #     a   no     12
  #     a   yes    13
  #     b   yes     5
  #     b   no      1
  #
  #   M1 <- M.fct(V1)     M2 <- M.fct(V2)    M12 <- M.fct(paste(V1,V2))
  #   M1%*%y              M2%*%y             M12%*%y
  #       [,1]                [,1]                [,1]
  #     a   25            no    13           a no   12
  #     b   21            yes   33           a yes  13
  #                                          b no    1
  #                                          b yes  20
  #
  A <- factor(strata)
  if (length(levels(A)) > 1) {Z <- model.matrix(~A-1)}
  else {Z <- matrix(1,ncells,1); dimnames(Z)[[2]] <- list(paste(sep="","A",levels(A)))}
  dZ <- dimnames(Z)[[2]]
  dZ <- substr(dZ,2,nchar(dZ))
  dimnames(Z)[[2]] <- dZ
  
  M <- t(Z)
  M
}

##################   end M.fct #############################################
