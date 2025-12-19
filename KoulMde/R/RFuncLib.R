

#'@keywords internal
DiffMatrix2=function(SMat, cVec){

  dimS = dim(SMat)
  n = dimS[1]

  cLen=length(cVec)

  ans = matrix(0, (n-cLen),2)

  nInc=1
  nIndex=1
  for(i in 1:n){

    if(nIndex<=cLen){
      Ind = cVec[nIndex]
    }


    if(i!=Ind){
      ans[nInc,1] = SMat[i,1]
      ans[nInc,2] = SMat[i,2]
      nInc=nInc+1

    }else{
      nIndex=nIndex+1
    }

  }

  return(ans)

}


#'@keywords internal
CheckStatus = function(x, y, xc, yc, r){
  r2 = r^2
  val = (x-xc)^2+(y-yc)^2
  if(val<r2){
    ans=1
  }else{
    ans=0
  }
  return(ans)
}

#'@keywords internal
GenerateCircle = function(nx, ny, r){

  Totaln = nx*ny

  XCenter = floor(nx/2)+1
  YCenter = floor(ny/2)+1

  TS = matrix(0, Totaln, 2)

  nS1Inc = 1
  nS2Inc = 1

  S1 = matrix(0, Totaln, 2)
  S2 = matrix(0, Totaln, 2)

  for(i in 1:nx){
    for(j in 1:ny){
      nIndex = (i-1) * ny + j
      TS[nIndex, 1] = i
      TS[nIndex, 2] = j

      bStatus = CheckStatus(i, j, XCenter, YCenter, r)
      if(bStatus == 1){
        S1[nS1Inc, 1] = i
        S1[nS1Inc, 2] = j
        nS1Inc = nS1Inc+1
      }else{
        S2[nS2Inc, 1] = i
        S2[nS2Inc, 2] = j
        nS2Inc = nS2Inc+1
      }

    }
  }

  nS1Inc = nS1Inc-1
  nS2Inc = nS2Inc-1

  lst = list()
  lst[[1]] = S1[1:nS1Inc, ]
  lst[[2]] = S2[1:nS2Inc, ]
  lst[[3]] = TS
  return(lst)
}
