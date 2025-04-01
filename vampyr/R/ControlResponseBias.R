ControlResponseBias<-function(x,content_factors,SD_items,unbalanced_items,contSD = FALSE,contAC = FALSE,corr = "Pearson",rotat = "promin",target,factor_scores = FALSE, PA = FALSE, path = FALSE, display = TRUE){

  ######################################################################
  #  x : Raw sample scores
  ######################################################################

  if (missing(x)){
    stop("The argument x is not optional, please provide a valid raw sample scores")
  }
  else {
    N<-size(x)[1]
    m<-size(x)[2]
    if (N==m){
      corr<-0 #square matrix, take it as a covariance/correlation matrix
      R <- x
      factor_scores = FALSE
      PA = FALSE
    }

    buff_na <- is.na(x)
    if (any(buff_na)){
      #check if missing data
      stop("The data contains missing values, please provide a full dataset to be analyzed.")
    }

    x_o<-x
  }

  ######################################################################
  #  content_factors : Number of content factors to be retained
  ######################################################################

  if (missing(content_factors)){
    stop("The argument content_factors is not optional, please provide the number of content factors to be retained")
  }
  else {
    content_factors<-round(content_factors)
    if (content_factors>(m/4)){
      stop("The argument content_factors has to be greater than the number of items / 4")
    }
  }
  r<-content_factors

  ######################################################################
  #  SD_items : A vector containing the Social Desirability markers
  ######################################################################

  ######################################################################
  #  unbalanced_items : A vector containing the the items that wouldn't be part of the balanced core
  ######################################################################

  ######################################################################
  #  contSD : logical variable determining if the method for controlling Social Desirability will be used.
  ######################################################################

  if (is.logical(contSD)!=TRUE){
    stop("contSD argument should be a logical variable.")
  }

  ######################################################################
  #  factor_scores : logical variable determining if factor scores wil be computed
  ######################################################################

  if (is.logical(factor_scores)!=TRUE){
    stop("factor_scores argument should be a logical variable.")
  }

  if (missing(contSD)) {
    if (missing(SD_items)){
      n_SD_items<-0
      contSD<-FALSE
      SD_items<-0
    }
    else {
      #check the number of SD items
      n_SD_items<-size(SD_items)[2]
      if (n_SD_items<4){
        stop("The argument SD_items has to be a vector with at least 4 items")
      }
      contSD<-TRUE
    }
  }
  else {
    if (contSD==TRUE){
      if (missing(SD_items)){
        stop("When contSD is TRUE, the argument SD_items can not be missing")
      }
      else {
        #check the number of SD items
        n_SD_items<-size(SD_items)[2]
        if (n_SD_items<4){
          stop("The argument SD_items has to be a vector with at least 4 items")
        }
      }
      contSD<-TRUE
    }
    else if (contSD==FALSE){
      n_SD_items<-0
      contSD<-FALSE
      SD_items<-0
    }
  }

  if (factor_scores!=TRUE && factor_scores!=FALSE){
    stop("The argument factor_scores has to be a logical value")
  }

  ######################################################################
  #  contAC : logical variable determining if the method for controlling Acquiescence will be used.
  ######################################################################

  if (missing(contAC)){
    if (missing(unbalanced_items)){
      n_unbalanced_items<-0
      contAC<-FALSE
      unbalanced_items<-0
      n_unbalanced_items<-0
    }
    else {
      n_unbalanced_items<-size(unbalanced_items)[2]
      contAC<-TRUE
    }
  }
  else {
    if (contAC==TRUE){
      #doesn't matter if unbalanced_items is missing
      if (missing(unbalanced_items)){
        unbalanced_items<-0
        n_unbalanced_items<-0
      }
      else {
        n_unbalanced_items<-size(unbalanced_items)[2]
      }
      contAC<-TRUE

    }
    else if (contAC==FALSE){
      n_unbalanced_items<-0
      contAC<-FALSE
    }
  }

  ######################################################################
  #  corr: Determine if Pearson or Polychoric matrix will be used "Pearson" or "Polychoric"
  ######################################################################

  if (missing(corr)){
    corr<-1 #Pearson by default
  }
  else{
    if (corr!=0){
      if (corr!="Pearson" && corr!="Polychoric"){
        if (corr=="pearson"){
          corr<-1
        }
        else if (corr=="polychoric"){
          corr<-2
        }
        else if (corr==0){
          corr<-0
        }
        else {
          stop("corr argument has to be 'Pearson' for computing Pearson correlation or 'Polychoric' for computing Polychoric/Tetrachoric correlation)")
        }
      }
      else {
        if (corr=="Pearson"){
          corr<-1
        }
        else if (corr=="Polychoric"){
          corr<-2
        }
      }
    }
  }

  ######################################################################
  #  target : The semi-specified target if procustes rotations are selected
  ######################################################################

  if (missing(target)==FALSE){
    #check size
    f1<-size(target)[1]
    f2<-size(target)[2]

    N <- size(x)[1]
    m <- size(x)[2]

    if (missing(target)==F){
      target=as.matrix(target)
      if ((f1==m)==F){
        if (contSD==T){
          if ((f1==(m-n_SD_items))){
            #the target contains content items only
          }
        }
      }
      else {
         #the target contains all the items, has to be adjusted to the content items

      }
    }
  }



  ######################################################################
  #  rotat: Determine if the factor loading matrix will be rotated, and using which method
  ######################################################################

  if (missing(target)==FALSE){
    #target has been provided, the rotations available are:
    rotat_list_target<-c("targetT","targetQ","pstT","pstQ","bentlerT","bentlerQ","cfT","cfQ","bifactorT","bifactorQ")

    if (is.na(match(rotat,rotat_list_target))){
      stop("When providing a target, the rotation methods available are: targetT, targetQ, pstT, pstQ, bentlerT, bentlerQ, cfT, cfQ, bifactorT and bifactorQ")
    }
    else { #ok
      rotat_package<-1
    }
  }

  rotat_list_GPArotation<-c("oblimin","quartimin","targetT","targetQ","pstT","pstQ","oblimax","entropy","quartimax",
                            "Varimax","simplimax","bentlerT","bentlerQ","tandemI","tandemII","geominT","geominQ",
                            "cfT","cfQ","infomaxT","infomaxQ","mccammon","bifactorT","bifactorQ","vgQ.oblimin",
                            "vgQ.quartimin","vgQ.target","vgQ.pst","vgQ.oblimax","vgQ.entropy","vgQ.quartimax",
                            "vgQ.varimax","vgQ.simplimax","vgQ.bentler","vgQ.tandemI","vgQ.tandemII","vgQ.geomin",
                            "vgQ.cf","vgQ.infomax","vgQ.mccammon","vgQ.bifactor")

  rotat_list_stats<-c("varimax","promax")

  rotat_list_PCovR<-c("promin","wvarim")

  is_GPArotation<-match(rotat,rotat_list_GPArotation)
  if(is.na(is_GPArotation)){
    is_GPArotation<-0
  }

  is_stats<-match(rotat,rotat_list_stats)
  if(is.na(is_stats)){
    is_stats<-0
  }

  is_PCovR<-match(rotat,rotat_list_PCovR)
  if(is.na(is_PCovR)){
    is_PCovR<-0
  }


  if(rotat=="none"){
    rotat_package<-0
  }
  else{
    if(r==1){
#      message('\nRotation methods are only available when retaining more than one content factor. The obtained loading matrix will not be rotated.')
      rotat<-"none"
      rotat_package<-0
    }
    else{
      if(is_GPArotation>0){
        rotat_package<-1 #the rotation is available on GPArotation package
      }
      else {
        if(is_stats>0){
          rotat_package<-2 #the rotation is available on stats package
        }
        else {
          if(is_PCovR>0){
            rotat_package<-3 #the rotation is available on PCovR package
          }
          else{
            rotat<-"none"
            rotat_package<-0
            message("\nrotat argument has to be available through GPArotation, stats or PCovR packages (case sensitive). rotat has been switch to none")
          }
        }
      }

    }
  }

  ######################################################################
  #  PA: If Parallel Analysis will be computed
  ######################################################################

  if (is.logical(PA)!=TRUE){
    stop("PA argument should be a logical variable.")
  }

  ######################################################################
  #  path: If path diagram  will be plotted
  ######################################################################

  if (is.logical(path)!=TRUE){
    stop("path argument should be a logical variable.")
  }

  if (path == TRUE){
    if (m > 40 || r >5){
      path = FALSE
      message("\npath argument is limited to a maximum of 40 items and 5 content factors. The path diagram will not be plotted")
    }
  }


  ######################################################################
  #  display: What should be displayed in console
  #            - TRUE: (default) display the complete output
  #            - FALSE: no output will be printed, the result will be passed silently
  #            - Available options (multiple selection available):
  #                   "Summary", "Descriptives", "Adequacy", "GOF" (Goodness of Fit indices), "Loadings" and "PA"
  ######################################################################

  # true logical display

  if (is.logical(display)){
    if (isTRUE(display)){
      displayL <- TRUE
      display_summary <- TRUE
      display_descriptives <- TRUE
      display_adequacy <- TRUE
      display_GOF <- TRUE
      display_loadings <- TRUE

      if (factor_scores == TRUE){
        display_scores <- TRUE
      }
      else {
        display_scores <- FALSE
      }

      if (PA == TRUE){
        display_PA <- TRUE
      }
      else {
        display_PA <- FALSE
      }
    }
    else {#FALSE
      displayL <- FALSE
      display_summary <- FALSE
      display_descriptives <- FALSE
      display_adequacy <- FALSE
      display_GOF <- FALSE
      display_loadings <- FALSE
      display_scores <- FALSE
      display_PA <- FALSE
    }
  }

  else { #check selected options
    displayL <- TRUE
    display_summary <- FALSE
    display_descriptives <- FALSE
    display_adequacy <- FALSE
    display_GOF <- FALSE
    display_loadings <- FALSE
    display_scores <- FALSE
    display_PA <- FALSE

    if (is.character(display)){
      #char or char list
      if (length(grep("Summary",display)==1) > 0){
        display_summary <- TRUE
      }

      if (length(grep("Descriptives",display)==1) > 0){
        display_descriptives <- TRUE
      }

      if (length(grep("Adequacy",display)==1) > 0){
        display_adequacy <- TRUE
      }

      if (length(grep("GOF",display)==1) > 0){
        display_GOF <- TRUE
      }

      if (length(grep("Loadings",display)==1) > 0){
        display_loadings <- TRUE
      }

      if (length(grep("Scores",display)==1) > 0){
        if (factor_scores == TRUE){
          display_scores <- TRUE
        }
        else {
          display_scores <- FALSE
        }
      }

      if (length(grep("PA",display)==1) > 0){
        if (PA == TRUE){
          display_PA <- TRUE
        }
        else {
          display_PA <- FALSE
        }
      }

    }
    else {
      stop("The display argument should be or a logical value, or a character array containing the sections to be printed (see documentation).")
    }
  }


  ################ BEGIN ##################

  n_content_items<-m-n_SD_items

  G<-c(n_SD_items,n_content_items)


  content_items<-c()
  buff1<-1
  buff2<-1
  cont<-0
  token<-1

  ib<-c()
  iub<-c()

  headnames<-character(length = r+contSD+contAC)
  for (i in 1:(r+contSD+contAC)){
    if (contSD==TRUE && contAC==TRUE){
      if (i==1){
        headnames[i]=("Factor SD")
      }
      if (i==2){
        headnames[i]=("Factor AC")
      }
      if (i>2) {
        headnames[i]=sprintf("Factor %.0f",i-2)
      }
    }
    if (contSD==TRUE && contAC==FALSE){
      if (i==1){
        headnames[i]=("Factor SD")
      }
      else {
        headnames[i]=sprintf("Factor %3.0f",i-1)
      }
    }
    if (contSD==FALSE && contAC==TRUE){
      if (i==1){
        headnames[i]=("Factor AC")
      }
      else {
        headnames[i]=sprintf("Factor %.0f",i-1)
      }
    }
    if (contSD==FALSE && contAC==FALSE){
      headnames[i]=sprintf("Factor %.0f",i)
    }
  }
  itemnames<-character(length = m)

  #define the subscripts for SD items, content items, balanced subset and unbalanced subset
  for (i in 1:m){
    if (SD_items[buff1]==i){
      cont=cont+1
      itemnames[buff1]<-sprintf("Item %3.0f",i)
      if (buff1<n_SD_items){
        buff1=buff1+1
      }
      else {
        token<-0
      }
    }
    else {
      content_items<-c(content_items,i)
      itemnames[i+n_SD_items-buff1+token]<-sprintf("Item %3.0f",i)
      if (buff2<=n_unbalanced_items){
        if (i==unbalanced_items[buff2]){
          buff2=buff2+1
          iub=c(iub,i-cont)
        }
        else {
          ib=c(ib,i-cont)
        }
      }
      else {
        ib=c(ib,i-cont)
      }
    }
  }

  buff_SD<-x[,SD_items]
  buff_content<-x[,content_items]

  #adjust target
  if (missing(target)==F){
    target <- target[content_items,]
  }

  if (corr==1){
    #new matrix with the SD items at the beginning
    x<-cbind(buff_SD,buff_content)
    R<-cor(x)
  }
  else if (corr==2){
    #new matrix with the SD items at the beginning
    x<-cbind(buff_SD,buff_content)
    #Polychoric matrix
    R<-(psych::polychoric(x,smooth = FALSE, correct = FALSE))$rho

    #check if the matrix is positive definite
    D<-eigen(R)$values

    smoothing_done <- FALSE

    if (min(D)<0){
      #R<-psych::cor.smooth(R,eig.tol = 0)
      # Bentler and Yuan (2011) smoothing:
      R_smooth <- fungible::smoothBY(R, const = 1)$RBY

      # smooth_indices = R_smooth != R
      # smooth_v = 0
      # for (i in 1:m){
      #   if (R_smooth[i,i] != R[i,i]){
      #     smooth_v = smooth_v + 1
      #   }
      # }
      #
      # R_smooth[R!=R]
      smoothing_done <- TRUE
      R <- R_smooth
    }

    #Thresholds
    xmin<-apply(x,2,min)

    xmax<-apply(x,2,max)
    THRES<-thresholds(x,xmin,xmax)
    m<-size(THRES)[1]
    k<-size(THRES)[2]
    THRES<-t(cbind(THRES,matrix(0,m,1)))

  }
  else {
    new_order<-c(SD_items,content_items)
    R<-R[new_order,new_order]
  }

  #ib<-ib-G[1]
  #iub<-iub-G[1]

  m<-dim(R)
  m<-m[2]

  Rf<-EFA.MRFA::mrfa(R,dimensionality = 1,display = FALSE)$Matrix
  dRr<-diag(Rf)

  ##########REDUCED MATRIX###########
  Rr<-R-diag(diag(R))+diag(dRr)


  ##########Social Desirability###########

  if (contSD==TRUE){
    Rds<-Rr[1:G[1],1:G[1]]


    out_fa<-suppressMessages(psych::fa(Rds,nfactors=1,rotate="none", covar=TRUE))
    Ads<-out_fa$loadings

    k<-Ads[1]
    rm<-transpose(Rds[2:G[1]])
    prod<-solve(transpose(rm)%*%rm)*k

    lambda<-matrix(0,G[2],1)
    j=1
    i=G[1]+1
    for(i in 1:G[2]+G[1]){
      ri<-Rr[2:G[1],i]
      lambda[j]=ri%*%rm*prod
      j=j+1
    }
    #SOCIAL DESIRABILITY FACTOR#
    Pds<-rbind(Ads,lambda)

    Rr1<-Pds%*%transpose(Pds)

    #Rae: Matrix without SD
    Rae<-Rr-Rr1
    a<-G[1]+1
    b<-G[1]+G[2]
    Rae<-Rae[a:b,a:b]
  }
  else {
    Rae<-Rr
  }

  if (contAC==TRUE){
    #BALANCED CORE#
    m_b<-length(ib)
    Rae_b<-Rae[ib,ib]

    #Ten Berge's Method
    #Computing centroid using balanced core (Acquiescence)
    u<-matrix(1,m_b,1)
    a1<-Rae_b%*%u
    a2<-(transpose(u)%*%Rae_b%*%u)^(1/2)
    a<-a1 %*% (1/a2)

    #a<-Rae_b%*%u/(transpose(u)%*%Rae_b%*%u)^(1/2)
    #a<-matrix(0,m_b,1)

    #ACQUIESCENCE FACTOR#

    a_g<-matrix(0,m_b,1)
    m_k<-length(iub)
    a_g[ib]<-a
    if(m_k>0){
      for(i in 1:m_k){
        buff1<-transpose(Rae[iub[i],ib])
        buff2<-(transpose(u)%*%Rae_b%*%u)^(1/2)
        a_g[iub[i]]<-sum(buff1)/buff2
      }
    }

    Rr2<-matrix(a_g)%*%t(a_g)

    #Re <- matrix without Acquiescence and SD
    Re<-Rae-Rr2
  }
  else {
    Re<-Rae
  }

  # CONTENT FACTORS

  out_eigen<-eigen(Re)
  VV<-out_eigen$vectors[,1:r]
  if (r==1){
    VV<-transpose(VV)
  }
  LL<-diag(out_eigen$values[1:r])
  if (r==1){
    LL<-out_eigen$values[1:r]
  }
  A<-VV%*%sqrt(LL)

  #FINAL FACTORIAL PATTERN#
  buff<-matrix(0,G[1],contAC+r)
  if (contAC==TRUE){
    a_gA<-cbind(a_g,A)
    buff<-rbind(buff,a_gA)
  }
  else {
    buff<-rbind(buff,A)
  }

  if (contSD==TRUE){
    P<-cbind(Pds,buff)
  }
  else {
    P<-buff
  }

  #ROTATION

  Pc<-P[(G[1]+1):(G[1]+G[2]),(1+contSD+contAC):(contSD+contAC+content_factors)] # Content factors

  #Explained Common Variance proportion
  Rf<-EFA.MRFA::mrfa(R,dimensionality = r+contSD+contAC,display = FALSE)$Matrix
  dRr<-transpose(diag(Rf))
  EV<-transpose(as.numeric(transpose(diag(t(P)%*%P))/sum(dRr)))

  #Produced final matrix
  Rpf<-P%*%t(P)
  Rpf2<-Rpf + diag(1,size(R)[1]) - diag(diag(Rpf))

  #Residual final matrix
  Ref<-R-Rpf

  #KMO
  adeq<-EFA.MRFA::adequacymatrix(R,N)

  if (adeq$kmo_index < 0.5 && corr == 2){
    stop(sprintf("The KMO obtained using Polychoric correlation is inacceptable. Please, select Pearson Correlation. KMO = %7.5f", adeq$kmo_index))
  }

  ### GOF INDICES: Deprecated, instead using robust fit indices by lavaan
  #
  # #GFI
  # RES<-R-Rpf
  # RES<-RES-diag(diag(RES))
  # R0<-R-diag(diag(R))
  # GFI<-1-(sum(diag(RES%*%RES)))/(sum(diag(R0%*%R0)))
  #
  # #AGFI
  # k<-3
  # p<-m
  # Dk<-(1/2 * ((p-k) * (p-k+1)) - p)
  # AGFI<-1 - ((1 - GFI)*( (p*(p-1)/2)/Dk))
  #
  #RMSR
  A<-matrix(0,m*(m-1)/2,1)
  h<-1
  for (i in 1:m){
    for (j in (i+1):m){
      if (j>m){
        break
      }
      A[h]<-Ref[j,i]
      h<-h+1
    }
  }

  me<-mean(A)
  s<-apply(A,2,sd)
  RMSR<-sqrt(s*s+me*me)

  kelley<-sqrt(mean(diag(R))/(N-1))

  Pc<-P[(G[1]+1):m,(contSD+contAC+1):(contSD+contAC+r)]

  if (rotat=="none"){ #no rotation

    buffT<-Pc
  }
  else {
    if (rotat_package==1){ #GPArotation
      #check if there is a rotation target
      if (missing(target)==FALSE){
        if (rotat=="pstT"||rotat=="pstQ"){
          #Weights required
          W=(array(1, dim(Pc)))
          W[target>0]=0
          W[target<0]=0
          W[target==0]=1
          buffT<-eval(parse(text=paste("GPArotation::",as.name(rotat),"(Pc, W=W, Target=target)",sep="")))
          Pc<-buffT$loadings
        }
        else {
          buffT<-eval(parse(text=paste("GPArotation::",as.name(rotat),"(Pc, Target=target)",sep="")))
          Pc<-buffT$loadings
        }
      }
      else {
        buffT<-eval(parse(text=paste("GPArotation::",as.name(rotat),"(Pc)",sep="")))
        Pc<-buffT$loadings
      }
    }
    else if(rotat_package==2){ #stats
      buffT<-eval(parse(text=paste("stats::",as.name(rotat),"(Pc)",sep="")))
      Pc<-buffT$loadings
    }
    else if (rotat_package==3){ #PCovR
      buffT<-eval(parse(text=paste("PCovR::",as.name(rotat),"(Pc)",sep="")))
      Pc<-buffT$loadings
      if (rotat=="promin"){
        U<-buffT$U
      }
    }
  }

  P[(G[1]+1):(G[1]+G[2]),(1+contSD+contAC):(contSD+contAC+content_factors)]<-Pc

  #round to 8 decimals
  P<-round(P,8)

  #Correlation between content factors
  PHI_total=diag(r+contSD+contAC)

  if (rotat=="promin"){
    #promin
    PHI<-t(U)%*%U
    PHI_total[(contSD+contAC+1):(contSD+contAC+r),(contSD+contAC+1):(contSD+contAC+r)]<-PHI
  }
  else {
    if ("Phi" %in% names(buffT)){
      #oblique rotations
      PHI<-buffT$Phi
      PHI_total[(contSD+contAC+1):(contSD+contAC+r),(contSD+contAC+1):(contSD+contAC+r)]<-PHI
    }
    else {
      #none or orthogonal rotation, no PHI
      PHI<-diag(r)
    }
  }


  if (r==1 && (size(P)[1]==1)){
    if (contSD==0 && contAC == 0){
      P <- transpose(P)
    }
    Pc<-transpose(Pc)
  }

  if (r==1){
    if (any(sum(P[,(contSD+contAC+1):(contSD+contAC+r)])<0)){
      if (sum(P[,1+contSD+contAC])<0){
        P[,1+contSD+contAC] <- -P[,1+contSD+contAC]
        PHI[,1] <- -PHI[,1]
        PHI[1,] <- -PHI[1,]
      }
    }
  }
  else {
    if (any(colSums(P[,(contSD+contAC+1):(contSD+contAC+r)])<0)){
      for (i in 1:(r)){
        if (sum(P[,i+contSD+contAC])<0){
          P[,i+contSD+contAC] <- -P[,i+contSD+contAC]
          PHI[,i] <- -PHI[,i]
          PHI[i,] <- -PHI[i,]
        }
      }
      PHI_total[(contSD+contAC+1):(contSD+contAC+r),(contSD+contAC+1):(contSD+contAC+r)]<-PHI
    }
  }




  if (r==1){
    bent<-1
    ls_index<-1
  }
  else {

    # Bentler's simplicity
    C <- Pc*Pc
    D <- sqrt(diag(diag(t(C)%*%C)))
    D2 <- diag(diag(D)^(-1))
    S <- D2 %*% transpose(C) %*% C %*% D2
    bent <- det(S)

    # Lorenzo-Seva simplicity index

    L <- Pc
    D <- diag(diag(t(L)%*%L))
    D2 <- diag(diag(D)^(-1))
    H <- diag(diag(L%*%D2%*%t(L)))
    D3 <- diag(diag(D)^(-1/2))
    H3 <- diag(diag(H)^(-1/2))
    B <- H3 %*% L %*% D3
    C <- B*B

    p <- size(C)[1]
    r <- size(C)[2]

    s <- 0
    for (i in 1:p) {
      for (j in 1:r) {
        s <- s + (C[i,j]+0.000001)^(10*C[i,j])
      }
    }

    s <- s/(p*r)

    e <- 0

    for (i in 1:p) {
      for (j in 1:r) {
        e <- e + ((1/r)+0.000001)^(10*(1/r))
      }
    }

    e <- e/(p*r)

    ls_index <- (s-e)/(1-e)
  }

  ###################################################

  ## Robust fit indices using lavaan testing ##
  ## We are going to use lavaan for computing the chi squares and degrees of freedom,
  ## since the indices are estimated using the wrong number of dof (like all the values are fixed)

  #TESTING
  P_lavaan <- zapsmall(matrix(round(P,2),nrow = m, (r+contSD+contAC)))

  rownames(P_lavaan)<- colnames(as.data.frame(x)) # preventing issues with numerical matrices

  for (i in 1:(r+contSD+contAC)){ #for each factor

    if (i ==1){
      if (contSD==TRUE){
        buff_names <- c("SD")
      }
      else{
        if (contSD==FALSE && contAC==TRUE){
          buff_names <- c("AC")
        }
        else {
          buff_names <- paste("F", toString(i-contSD-contAC), sep ="")
        }
      }
    }
    else {
      if (i == 2 && contSD==TRUE && contAC==TRUE){
        buff_names <- c(buff_names,"AC")
      }
      else {
        buff_names <- c(buff_names,paste("F", toString(i-contSD-contAC), sep =""))
      }
    }
  }
  colnames(P_lavaan)=buff_names


  terms <- vector()
  for (i in 1:(r+contSD+contAC)) {
    terms[i] <-
      paste0(colnames(P_lavaan)[i],"=~ ", paste0(c(P_lavaan[,i]), "*", names(P_lavaan[,1]), collapse = "+"))
  }
  terms <- paste(terms, collapse = "\n")


  #define correlations between factors
  if (rotat == "promin" || "Phi" %in% names(buffT)){
    #oblique rotations, restrict SD and AC and fix PHI
    buff_terms <- vector()
    if (contSD == TRUE && contAC == FALSE){
      for (i in 1:(r)){
        buff_terms[i] <- paste0("SD ~~ 0*", colnames(P_lavaan)[i+1])
      }
    }

    if (contSD == TRUE && contAC == TRUE){
      for (i in 1:(r+contAC)){
        buff_terms[i] <- paste0("SD ~~ 0*", colnames(P_lavaan)[i+1])
      }
      for (j in 1:(r)){
        buff_terms[j+r+contAC] <- paste0("AC ~~ 0*", colnames(P_lavaan)[j+2])
      }
    }

    if (contSD == FALSE && contAC == TRUE){
      for (i in 1:(r)){
        buff_terms[i] <- paste0("AC ~~ 0*", colnames(P_lavaan)[i+1])
      }
    }

    if (contSD == FALSE && contAC == FALSE){
      #dont restrict anything
    }

    buff_terms2 <- vector() # fix the ones
    for (i in 1:(r+contSD+contAC)){
      buff_terms2[i] <- paste0(colnames(P_lavaan)[i]," ~~ 1*" ,colnames(P_lavaan)[i])
    }

    buff_terms3 <- vector() # the correlations between content factors
    b <- 0
    for (i in 1:(r-1)){
      for (j in i:(r-1)){
        buff_terms3[b+1] <- paste0(colnames(P_lavaan)[i+contSD+contAC], "~~", PHI[j+1,i], "*", colnames(P_lavaan)[j+contSD+contAC+1])
        b <- b+1
      }
    }

    buff_terms <- paste(buff_terms, collapse = "\n")
    buff_terms2 <- paste(buff_terms2, collapse = "\n")
    buff_terms3 <- paste(buff_terms3, collapse = "\n")
    terms <- paste(terms, "\n", buff_terms, "\n", buff_terms2, "\n", buff_terms3)

    if (corr == 2){
      suppressWarnings(fit  <- lavaan::cfa(terms, data=x, std.lv=T, ordered = c(colnames(x)), estimator = "ULSM"))
    }
    else {
      suppressWarnings(fit  <- lavaan::cfa(terms, data=x, std.lv=T, estimator = "ULSM"))
    }


  }
  else {
    #none or orthogonal rotation, no PHI
    if (rotat == "none"){
      if (corr == 2){
        suppressWarnings(fit  <- lavaan::cfa(terms, data=x, std.lv=T, ordered = c(colnames(x)), estimator = "ULSM"))
      }
      else {
        suppressWarnings(fit  <- lavaan::cfa(terms, data=x, std.lv=T, estimator = "ULSM"))
      }
    }
    else { #orthogonal
      if (corr == 2){
        suppressWarnings(fit  <- lavaan::cfa(terms, data=x, std.lv=T, ordered = c(colnames(x)), estimator = "ULSM", orthogonal = TRUE))
      }
      else {
        suppressWarnings(fit  <- lavaan::cfa(terms, data=x, std.lv=T, estimator = "ULSM", orthogonal = TRUE))
      }
    }

  }


  gof_lavaan <- lavaan::fitmeasures(fit)

  # we need the chi squared scaled, the baseline chi square scaled and the degrees of freedom

  chi_0 <- as.numeric(gof_lavaan["baseline.chisq.scaled"])

  df_0 <- as.numeric(gof_lavaan["baseline.df.scaled"])

  chi_model <- as.numeric(gof_lavaan["chisq.scaled"])

  if (contSD == TRUE){
    #not all parameters are free
    free_parameters <- ((contSD+contAC+r) * m) - (n_SD_items * (r + contAC))
  }
  else {
    #all parameters are free
    free_parameters <- ((contSD+contAC+r) * m)
  }

  # calculate the degrees of freedom from the model

  df_model <- df_0 - free_parameters

  gof_warning <- FALSE

  if (df_model <=0){
    warning("The model can not be properly identified with that few items, goodness of fit indices are not available.")
    TLI <- NaN
    CFI <- NaN
    RMSEA <- NaN
    GFI <- NaN
    gof_warning <- TRUE
  }
  else {
    # ROBUST TLI
    TLI <- ((chi_0 / df_0) - (chi_model / df_model)) / ((chi_0 / df_0) - 1)

    if (TLI >= 1){
      TLI <- 0.999
    }

    # ROBUST CFI
    CFI <- ((chi_0 - df_0) - (chi_model - df_model)) / (chi_0 - df_0)

    if (CFI >= 1){
      CFI <- 0.999
    }

    # ROBUST RMSEA
    #check if chi_model - df_model < 1
    if ((chi_model - df_model) > 1){
      RMSEA <- (sqrt(chi_model - df_model)) / (sqrt(df_model * (N - 1)))
    }
    else {
      RMSEA <- 0.0001
    }

    # GFI using the model chi

    GFI <- 1 - (chi_model / chi_0)

    if (GFI >= 1){
      GFI <- 0.999
    }
  }

  ###################################################

  # FACTOR SCORES ESTIMATION

  if (factor_scores==TRUE){
    if (corr==1){ #continuous
      out<-eap_continuous_obli(x,P,PHI_total)

      th<-out$th
      th_li<-out$th_li
      th_ls<-out$th_ls
      se<-diag(out$se)
      reli<-as.vector(out$reli)
    }
    else {

      sigj <- diag(sqrt(diag(diag(R - P%*%PHI_total%*%t(P)))))
      g2 <- size(P)[2]

      x<-as.matrix(x)

      out<-reap_grad_obli(x,P,PHI_total,THRES,sigj,displayL)

      th<-out$th
      th_li<-out$th_li
      th_ls<-out$th_ls
      se<-out$se
      reli<-out$reli
      incons <- out$incons
    }
  }

  #set the proper headers and rownames to all the matrices
  rownames(P)<-itemnames
  colnames(P)<-headnames
  rownames(Ref)<-itemnames
  colnames(Ref)<-itemnames
  rownames(Rpf2)<-itemnames
  colnames(Rpf2)<-itemnames

  if (factor_scores==TRUE){
    colnames(th)<-headnames
    colnames(th_li)<-headnames
    colnames(th_ls)<-headnames
    if (corr==2){
      colnames(se)<-headnames
      colnames(reli)<-headnames
    }
  }

  colnames(EV)<-c("ECV")
  rownames(EV)<-headnames

  colnames(dRr)<-c("Comunalities")
  rownames(dRr)<-itemnames

  colnames(PHI)<-headnames[(contSD+contAC+1):(r+contSD+contAC)]
  rownames(PHI)<-headnames[(contSD+contAC+1):(r+contSD+contAC)]

  if (factor_scores==TRUE){
    if (corr == 2){
      precision_matrix <- array(0,dim=c(N,6,(r+contSD+contAC)))
      colnames(precision_matrix) <- c("Factor Score","90% lower CI","90% upper CI","PSD","Reliability", "Inconsistent")

      for (j in 1:(r+contSD+contAC)){
        precision_matrix[,,j] <- cbind(th[,j],th_li[,j],th_ls[,j],se[,j],reli[,j],incons[,j])
      }

    matrices<-list("loadings"=P,"Phi"=PHI_total,"Factor_scores"=th,"Precision_scores" =precision_matrix,"comunalities"=dRr,"ECV"=EV,"reduced"=Ref,"produced"=Rpf2,"RMSEA" = RMSEA,"Chi"=chi_model,"TLI"=TLI,"CFI"=CFI,"GFI"=GFI,"RMSR"=RMSR,"kelley"=kelley)
    }
    else {
      matrices<-list("loadings"=P,"Phi"=PHI_total,"Factor_scores"=th,"comunalities"=dRr,"ECV"=EV,"reduced"=Ref,"produced"=Rpf2,"RMSEA" = RMSEA,"Chi"=chi_model,"TLI"=TLI,"CFI"=CFI,"GFI"=GFI,"RMSR"=RMSR,"kelley"=kelley)
    }
  }
  else {
    matrices<-list("loadings"=P,"Phi"=PHI_total,"comunalities"=dRr,"ECV"=EV,"reduced"=Ref,"produced"=Rpf2,"RMSEA" = RMSEA,"Chi"=chi_model,"TLI"=TLI,"CFI"=CFI,"GFI"=GFI,"RMSR"=RMSR,"kelley"=kelley)
  }



  ################## PATH DIAGRAM  ##################

  if (path == T){

    lambdas <- fit@Model@GLIST$lambda
    lambdas[which(lambdas > -.2 & lambdas < .2)] = 0.000
    fit@Model@GLIST$lambda <- lambdas

    lambdas2 <- fit@ParTable$est
    lambdas3 <- lambdas2[1:(m*(r+contSD+contAC))]

    # REARRANGE ITEMS ORDER
    # lambdas4 <- matrix(lambdas3, ncol = (r+contSD+contAC))
    # lambdas_cont <- lambdas4[,((1+contSD+contAC) : (r+contSD+contAC))]
    #
    # max_loading <- max.col(abs(lambdas_cont))
    #
    # n_max_factor <- vector()
    # pos <- matrix(nrow = m, ncol = r)
    # new_order <- vector()
    #
    # for (i in 1:r){
    #   buff_sum <- sum(max_loading == i)
    #
    #   pos[,i] <- order(lambdas_cont[,i],decreasing = T)
    # }
    #
    # for (i in 1:m){
    #   max_loading <- which.max(abs(lambdas_cont[i,]))
    #   new_order[i] <- pos[i,max_loading]
    # }
    #
    # lambdas5 <- lambdas4[new_order,]

    lambdas5 <- lambdas3

    #lambdas3[which(lambdas3 > -.2 & lambdas3 < .2)] = 0.00

    AC_index <- c((contSD*m+1):(m*(contSD+contAC)))

    for (i in 1:(m*(r+contSD+contAC))){ #different criteria for AC
      if (any(i == AC_index) ){
        if (lambdas5[i] > -.2 && lambdas5[i] < .2){
          lambdas5[i] <- 0.00
        }
      }
      else {
        if (lambdas5[i] > -.3 && lambdas5[i] < .3){
          lambdas5[i] <- 0.00
        }
      }
    }

    fit@ParTable$est[1:(m*(r+contSD+contAC))] <- lambdas5

    if (contAC == T){
        base_plot <- c("semPaths(fit, what = 'est', layout = 'tree3', intercepts = F, residuals = F, rotation = 1, fade = F, exoCov = F, edge.width = 0.25, edge.label.cex = 1, ask = FALSE, mar = c(3,1,3,1), thresholds = FALSE,")
    }
    else {
      base_plot <- c("semPaths(fit, what = 'est', layout = 'tree3', intercepts = F, residuals = F, rotation = 1, fade = F, exoCov = F, edge.width = 0.25, edge.label.cex = 1, ask = FALSE, mar = c(3,3,3,3), thresholds = FALSE,")

    }

    if (contAC == T){
      base_plot <- paste(base_plot, " bifactor = 'AC',")
    }

    if (m < 20){
      base_plot <- paste(base_plot, " sizeMan = 5,")
    }
    if (m > 20 && m <= 30){
      base_plot <- paste(base_plot, " sizeMan = 4,")
    }

    if (m > 30 && m <= 40){
      base_plot <- paste(base_plot, " sizeMan = 3,")
    }

    ############  NODE COLOR

    buff_color <- character(5)
    buff_color[1] <- 'color = list( lat = c('

    if (contSD == T && contAC == T){
      buff_color[1] <- 'color = list( lat = c(rgb(255,200,135, maxColorValue = 255),'
      buff_color[2] <- 'rgb(204,110,110, maxColorValue = 255),'
    }
    if (contSD == T && contAC == F){
      buff_color[1] <- 'color = list( lat = c(rgb(255,200,135, maxColorValue = 255),'
    }
    if (contSD == F && contAC == T){
      buff_color[1] <- 'color = list( lat = c(rgb(204,110,110, maxColorValue = 255),' #buff_color[2] <- 'rgb(255,200,135, maxColorValue = 255),'
    }
    if (contSD == F && contAC == F){
      buff_color[1] <- 'color = list( lat = c('
    }

    n_colors <- vector()
    n_colors[1] <- 'rgb(180,235,215, maxColorValue = 255),'  #GREEN
    n_colors[2] <- 'rgb(200,205,235, maxColorValue = 255),'  #BLUE
    n_colors[3] <- 'rgb(255,180,180, maxColorValue = 255),'  #PINK
    n_colors[4] <- 'rgb(128,206,225, maxColorValue = 255),'  #AZUL ANA
    n_colors[5] <- 'rgb(120,220,120, maxColorValue = 255),'  #VERDE MANZANA

    n_colors2 <- vector()
    n_colors2[1] <- 'rgb(14,165,135, maxColorValue = 255),'  #GREEN
    n_colors2[2] <- 'rgb(130,105,165, maxColorValue = 255),'  #BLUE
    n_colors2[3] <- 'rgb(205,100,110, maxColorValue = 255),'  #PINK
    n_colors2[4] <- 'rgb(40,80,180, maxColorValue = 255),'  #AZUL ANA
    n_colors2[5] <- 'rgb(60,140,60, maxColorValue = 255),'  #VERDE MANZANA

    for (i in (1+contSD+contAC):(r+contSD+contAC)){
      #for each factor, change the colors of the nodes
      if (i == 1){
        #no SD or AC
        buff_color[1] <- sprintf('color = list( lat = c(%s',n_colors[i-(contSD+contAC)])
      }
      else {
        buff_color[i] <- n_colors[i-(contSD+contAC)]
      }
    }
    #remove last ','
    buff_color[i] <- substr(buff_color[i],1,nchar(buff_color[i])-1)

    buff_color[i+1] <- '),'

    buff_color <- paste(buff_color, collapse = '')

    ########### END NODE COLOR

    ########### ITEM COLOR

    if (contSD == T){
      #SD markers with different color, loop for each item
      buff_color2 <- vector()
      for (i in 1:(size(SD_items)[2])){
        if (i == 1){
          buff_color2[1] <- ' man = c(rgb(255,200,135, maxColorValue = 255),'
        }
        else {
          buff_color2[i] <- 'rgb(255,200,135, maxColorValue = 255),'
        }
      }

      for (j in 1:(m-i)){
        buff_color2[j+i] <- 'rgb(230,230,230, maxColorValue = 255),'
      }

      #remove last ','
      buff_color2[j+i] <- substr(buff_color2[j],1,nchar(buff_color2[j])-1)


      buff_color2[j+i+1] <- ')),'

      buff_color2 <- paste(buff_color2,collapse = '')

    }
    else {
      buff_color2 <- 'man = rgb(230,230,230, maxColorValue = 255)),'
    }

    ########## END ITEM COLOR

    ########## EDGE COLOR

    buff_color3 <- vector()

    if (contSD == T && contAC == T){

      for (i in 1:m){ ### SD ###
        # SD loadings
        if (i == 1){
          buff_color3[1] <- 'edge.color = c(rgb(225,140,75, maxColorValue = 255),'
        }
        else {
          buff_color3[i] <- 'rgb(225,140,75, maxColorValue = 255),'
        }
      }

      for (j in 1:m){ ### AC ###
        buff_color3[j+i] <- 'rgb(180,90,90, maxColorValue = 255),'
      }

      for (k in 1:(m*r)){ ## content ###
        if (k <= m){ # 1st content factor
          buff_color3[k+i+j] <- n_colors2[1]
        }
        if ((k <= (m*2)) && (k > m)){
          buff_color3[k+i+j] <- n_colors2[2]
        }
        if ((k <= (m*3)) && (k > (m*2))){
          buff_color3[k+i+j] <- n_colors2[3]
        }
        if ((k <= (m*4)) && (k > (m*3))){
          buff_color3[k+i+j] <- n_colors2[4]
        }
        if ((k <= (m*5)) && (k > (m*4))){
          buff_color3[k+i+j] <- n_colors2[5]
        }
      }

    }
    if (contSD == T && contAC == F){

      for (i in 1:m){ ### SD ###
        # SD loadings
        if (i == 1){
          buff_color3[1] <- 'edge.color = c(rgb(225,140,75, maxColorValue = 255),'
        }
        else {
          buff_color3[i] <- 'rgb(225,140,75, maxColorValue = 255),'
        }
      }

      j <- 0
      for (k in 1:(m*r)){ ## content ###
        if (k <= m){ # 1st content factor
          buff_color3[k+i+j] <- n_colors2[1]
        }
        if ((k <= (m*2)) && (k > m)){
          buff_color3[k+i+j] <- n_colors2[2]
        }
        if ((k <= (m*3)) && (k > (m*2))){
          buff_color3[k+i+j] <- n_colors2[3]
        }
        if ((k <= (m*4)) && (k > (m*3))){
          buff_color3[k+i+j] <- n_colors2[4]
        }
        if ((k <= (m*5)) && (k > (m*4))){
          buff_color3[k+i+j] <- n_colors2[5]
        }

        #buff_color3[k+i+j] <- 'rgb(40,160,60, maxColorValue = 255),'
      }

    }
    if (contSD == F && contAC == T){
      i <- 0

      for (j in 1:m){ ### AC ###
        if (j == 1){
          buff_color3[1] <- 'edge.color = c(rgb(180,90,90, maxColorValue = 255),'
        }
        else {
          buff_color3[j+i] <- 'rgb(180,90,90, maxColorValue = 255),'
        }
      }

      for (k in 1:(m*r)){ ## content ###
        if (k <= m){ # 1st content factor
          buff_color3[k+i+j] <- n_colors2[1]
        }
        if ((k <= (m*2)) && (k > m)){
          buff_color3[k+i+j] <- n_colors2[2]
        }
        if ((k <= (m*3)) && (k > (m*2))){
          buff_color3[k+i+j] <- n_colors2[3]
        }
        if ((k <= (m*4)) && (k > (m*3))){
          buff_color3[k+i+j] <- n_colors2[4]
        }
        if ((k <= (m*5)) && (k > (m*4))){
          buff_color3[k+i+j] <- n_colors2[5]
        }
        #buff_color3[k+i+j] <- 'rgb(40,160,60, maxColorValue = 255),'
      }

    }
    if (contSD == F && contAC == F){
      i <- 0
      j <- 0

      for (k in 1:(m*r)){ ## content ###
        if (k == 1){
          buff_color3[1] <- sprintf('edge.color = c(%s',n_colors2[1])
        }
        else {
          if (k <= m){ # 1st content factor
            buff_color3[k+i+j] <- n_colors2[1]
          }
          if ((k <= (m*2)) && (k > m)){
            buff_color3[k+i+j] <- n_colors2[2]
          }
          if ((k <= (m*3)) && (k > (m*2))){
            buff_color3[k+i+j] <- n_colors2[3]
          }
          if ((k <= (m*4)) && (k > (m*3))){
            buff_color3[k+i+j] <- n_colors2[4]
          }
          if ((k <= (m*5)) && (k > (m*4))){
            buff_color3[k+i+j] <- n_colors2[5]
          }
          #buff_color3[k+i+j] <- 'rgb(40,160,60, maxColorValue = 255),'
        }
      }

    }

    #remove last ','
    buff_color3[i+j+k] <- substr(buff_color3[i+j+k],1,nchar(buff_color3[i+j+k])-1)

    buff_color3[i+j+k+1] <-'),'

    buff_color3 <- paste(buff_color3,collapse = '')

    ########## END EDGE COLOR

    ########## LABEL POSITION

    # Indistinctly of SD and content, AC independent

    labels_index <- .8/(r+contSD)
    if (labels_index == .8){
      labels_index <- .6
    }

    buff_labels_pos <- vector()

    # if ((r+contSD) == 1){
    #   buff_labels_pos <- 'edge.label.position = 0.5'
    # }
    # else{

      buff <- 0
      buff2 <- 0.0
      incr <- signif(labels_index/4,2)
      n_jumps <- 1
        for (i in 1:(r+contSD+contAC)){
          for (j in 1:m){
            if (i == 1 && j == 1){
              buff_labels_pos[1] <- sprintf(' edge.label.position = c(%.2f ,',(1-labels_index+buff2))
              pos <- 2
              if (n_jumps != 3){
                buff2 <- buff2 + incr
                n_jumps <- n_jumps + 1
              }
              else {
                buff2 <- 0.0
                n_jumps <- 1
              }
              #buff2 <- - buff2
            }
            else {
              if (i == 2 && contAC == T) {#AC
                buff_labels_pos[pos] <- sprintf('%.2f,',0.6+buff2)
                if (n_jumps != 3){
                  buff2 <- buff2 + incr
                  n_jumps <- n_jumps + 1
                }
                else {
                  buff2 <- 0.0
                  n_jumps <- 1
                }
                #buff2 <- - buff2
                pos <- pos + 1
                buff <- 1
              }
              else {
                buff_labels_pos[pos] <- sprintf('%.2f ,', (1-labels_index*(i-buff) + buff2))
                pos <- pos + 1
                if (n_jumps != 3){
                  buff2 <- buff2 + incr
                  n_jumps <- n_jumps + 1
                }
                else {
                  buff2 <- 0.0
                  n_jumps <- 1
                }
                #buff2 <- - buff2
              }
            }
          }
        }

      #remove last ','
      buff_labels_pos[i*j] <- substr(buff_labels_pos[i*j],1,nchar(buff_labels_pos[i*j])-1)

      buff_labels_pos[(i*j)+1] <-')'

      buff_labels_pos <- paste(buff_labels_pos,collapse = '')
    #}



    ######### END LABEL POSITION


    plot_final <- paste(base_plot,'',buff_color,'',buff_color2,'',buff_color3,buff_labels_pos,')')


    result_path <- tryCatch({eval(parse(text=plot_final))}, error = function(e) message("\nPath diagram failed."))

  }

  ##################### Printing time #####################

  if (displayL==F){
    invisible(matrices)
  }
  else if (displayL==T){

    if (display_summary == T){

      cat('\n\n')
      cat('DETAILS OF ANALYSIS\n\n')

      cat(sprintf('Number of participants                      : %5.0f \n',N))
      cat(sprintf('Number of items                             : %5.0f \n',m))
      if (contSD==1){
        buff<-''
        f2<-size(SD_items)[2]
        for (i in 1:f2){
          if (i==1){
            buff2<-sprintf('%.0f',SD_items[i])
          }
          else {
            buff2<-sprintf(', %.0f',SD_items[i])
          }
          buff<-paste(buff,buff2,sep = "")
        }

        cat(sprintf('Items selected as SD items                  :  %s',buff))
        cat('\n')
      }

      if (contAC==1){
        f2=size(unbalanced_items)[2]
      }
      if (contAC==1 && f2>0){
        buff<-''
        for (i in 1:f2){
          if (i==1){
            buff2<-sprintf('%.0f',unbalanced_items[i])
          }
          else {
            buff2<-sprintf(', %.0f',unbalanced_items[i])
          }
          buff<-paste(buff,buff2,sep = "")
        }
        cat(sprintf('Items selected as unbalanced                :  %s',buff))
        cat('\n')
      }

      if (corr==0){
        cat('Dispersion Matrix                           : User Defined')
      }
      else if (corr==1){
        cat('Dispersion Matrix                           : Pearson Correlations')
      }
      else if (corr==2){
        cat('Dispersion Matrix                           : Polychoric Correlations')
      }

      cat('\n')
      cat('Method for factor extraction                : Unweighted Least Squares (ULS)')
      cat('\n')

      cat(sprintf('Rotation Method                             : %s',rotat))

      cat('\n\n')
      cat('-----------------------------------------------------------------------')

    }

    if (display_descriptives == T){

      Kur<-moments::kurtosis(x_o)
      Skew<-moments::skewness(x_o)

      cat('\n\n')
      cat('Univariate item descriptives')
      cat('\n\n')
      cat('Item       Mean        Variance    Skewness     Kurtosis (Zero centered)\n\n')
      for (i in 1:m){
        buff<-sprintf('Item %3.0f ',i)
        buff1<-sprintf('% 2.3f      % 2.3f      % 2.3f       % 2.3f',mean(as.matrix(x_o[,i])),(sd(as.matrix(x_o[,i]))^2),Skew[i],Kur[i]-3)
        cat(paste(buff,buff1))
        cat('\n')
      }
      cat('\n')

      kurmax<-max(Kur)-3
      kurmin<-min(Kur)-3
      if (kurmax>1 || kurmin<(-1)){
        cat('Polychoric correlation is advised when the univariate distributions of ordinal items are\n')
        cat('asymmetric or with excess of kurtosis. If both indices are lower than one in absolute value,\n')
        cat('then Pearson correlation is advised. You can read more about this subject in:\n\n')
        cat('Muthen, B., & Kaplan D. (1985). A Comparison of Some Methodologies for the Factor Analysis of\n')
        cat('Non-Normal Likert Variables. British Journal of Mathematical and Statistical Psychology, 38, 171-189.\n\n')
        cat('Muthen, B., & Kaplan D. (1992). A Comparison of Some Methodologies for the Factor Analysis of\n')
        cat('Non-Normal Likert Variables: A Note on the Size of the Model. British Journal of Mathematical\n')
        cat('and Statistical Psychology, 45, 19-30. \n\n')
      }

      cat('-----------------------------------------------------------------------')

    }

    if (display_adequacy == T && display_PA == FALSE){

      cat('\n\n')
      cat('Adequacy of the dispersion matrix')
      cat('\n\n')
      cat(sprintf('Determinant of the matrix     = %17.15f\n',adeq$d))
      cat(sprintf('Bartlett\'s statistic          = %7.1f (df = %5.0f; P = %7.6f)\n',adeq$chisq,adeq$df,adeq$p_value))
      cat(sprintf('Kaiser-Meyer-Olkin (KMO) test = %7.5f ',adeq$kmo_index))
      if     (adeq$kmo_index >= 0.9){ cat(sprintf('(very good)'))}
      else if (adeq$kmo_index >= 0.8){ cat(sprintf('(good)'))}
      else if (adeq$kmo_index >= 0.7){ cat(sprintf('(fair)'))}
      else if (adeq$kmo_index >= 0.6){ cat(sprintf('(mediocre)'))}
      else if (adeq$kmo_index >= 0.5){ cat(sprintf('(bad)'))}
      else                     { cat(sprintf('(inaceptable)'))}
      cat('\n\n')
      cat('-----------------------------------------------------------------------')

    }

    if (PA == TRUE){
      if (display_PA == TRUE){

        cat('\n\n')
        if (corr == 1){
          out_PA <- EFA.MRFA::parallelMRFA(x)
        }
        else {
          out_PA <- EFA.MRFA::parallelMRFA(x, Ndatsets = 100, corr = "Polychoric")
        }

        cat('-----------------------------------------------------------------------')

      }
      else {
        if (corr == 1){
          out_PA <- EFA.MRFA::parallelMRFA(x, display = FALSE, graph = FALSE)
        }
        else {
          out_PA <- EFA.MRFA::parallelMRFA(x, Ndatsets = 100, corr = "Polychoric", display = FALSE, graph = FALSE)
        }
      }

      # change matrices list

      if (factor_scores==TRUE){
        if (corr == 2){
          matrices<-list("loadings"=P,"Phi"=PHI_total,"Factor_scores"=th,"Precision_scores" =precision_matrix,"comunalities"=dRr,"ECV"=EV,"reduced"=Ref,"produced"=Rpf2,"RMSEA" = RMSEA,"Chi"=chi_model,"TLI"=TLI,"CFI"=CFI,"GFI"=GFI,"RMSR"=RMSR,"kelley"=kelley,"PA_Real_Data"=out_PA$Real_Data,"PA_Mean_Random"=out_PA$Mean_random,"PA_Percentile_Random"=out_PA$Percentile_random,"N_factors_mean"=out_PA$N_factors_mean,"N_factors_percentiles"=out_PA$N_factors_percentiles)
        }
        else {
          matrices<-list("loadings"=P,"Phi"=PHI_total,"Factor_scores"=th,"comunalities"=dRr,"ECV"=EV,"reduced"=Ref,"produced"=Rpf2,"RMSEA" = RMSEA,"Chi"=chi_model,"TLI"=TLI,"CFI"=CFI,"GFI"=GFI,"RMSR"=RMSR,"kelley"=kelley,"PA_Real_Data"=out_PA$Real_Data,"PA_Mean_Random"=out_PA$Mean_random,"PA_Percentile_Random"=out_PA$Percentile_random,"N_factors_mean"=out_PA$N_factors_mean,"N_factors_percentiles"=out_PA$N_factors_percentiles)
        }
      }
      else {
        matrices<-list("loadings"=P,"Phi"=PHI_total,"comunalities"=dRr,"ECV"=EV,"reduced"=Ref,"produced"=Rpf2,"RMSEA" = RMSEA,"Chi"=chi_model,"TLI"=TLI,"CFI"=CFI,"GFI"=GFI,"RMSR"=RMSR,"kelley"=kelley,"PA_Real_Data"=out_PA$Real_Data,"PA_Mean_Random"=out_PA$Mean_random,"PA_Percentile_Random"=out_PA$Percentile_random,"N_factors_mean"=out_PA$N_factors_mean,"N_factors_percentiles"=out_PA$N_factors_percentiles)
      }

      ##################### Printing time #####################

      if (displayL==F){
        invisible(matrices)
      }

    }

    if (display_loadings == T || display_GOF == T){

      # FACTOR ANALYSIS
      cat('\n')
      if (contSD==F && contAC==F){
        cat('EXPLORATORY FACTOR ANALYSIS')
      }
      if (contSD==T && contAC==F){
        cat('EXPLORATORY FACTOR ANALYSIS CONTROLLING SOCIAL DESIRABILITY')
      }
      if (contSD==F && contAC==T){
        cat('EXPLORATORY FACTOR ANALYSIS CONTROLLING ACQUIESCENCE')
      }
      if (contSD==T && contAC==T){
        cat('EXPLORATORY FACTOR ANALYSIS CONTROLLING SOCIAL DESIRABILITY AND ACQUIESCENCE')
      }
      cat('\n')
      cat('-----------------------------------------------------------------------')

    }


    if (display_GOF == T && gof_warning == FALSE){

      cat('\n\n')
      cat('Robust Goodness of Fit statistics')
      cat('\n\n')
      cat(sprintf('          Root Mean Square Error of Approximation (RMSEA) = %4.3f',RMSEA))
      cat('\n\n')
      cat(sprintf(' Robust Mean-Scaled Chi Square with %.0f degrees of freedom = %.3f',df_model,chi_model))
      cat('\n\n')
      cat(sprintf('              Non-Normed Fit Index (NNFI; Tucker & Lewis) = %4.3f', TLI))
      cat('\n')
      cat(sprintf('                              Comparative Fit Index (CFI) = %4.3f', CFI))
      cat('\n')
      cat(sprintf('                              Goodness of Fit Index (GFI) = %4.3f',GFI))
      cat('\n\n')
      cat('-----------------------------------------------------------------------')
      cat('\n\n')

      cat(sprintf('                  Root Mean Square Residuals (RMSR) = %5.4f',RMSR))
      cat('\n')
      cat(sprintf('Expected mean value of RMSR for an acceptable model = %5.4f (Kelley\'s criterion)',kelley))
      if (RMSR > kelley){
        cat('\n(Kelley, 1935,page 146; see also Harman, 1962, page 21 of the 2nd edition)')
        cat('\nNote: if the value of RMSR is much larger than Kelley\'s criterion value the model cannot be considered as good')
      }
      cat('\n\n')
      cat('-----------------------------------------------------------------------')

    }

    if (display_loadings == T){

      cat('\n\n')
      if (rotat=="none"){
        cat('Unrotated loading matrix\n\n')
      }
      else {
        cat('Rotated loading matrix\n\n')
      }

      prmatrix(round(P,5))
      cat('\n')



      if (r!=1){

        if (PHI[1,2]!=0){
          cat('Inter-factor correlations\n\n')
          prmatrix(PHI)
          cat('\n\n')
        }

        cat(sprintf('Bentler\'s simplicity = % 5.4f \n\n',bent))

        cat(sprintf('Lorenzo-Seva\'s simplicity = % 5.4f \n\n',ls_index))
      }

      if (display_scores == TRUE){
        cat('-----------------------------------------------------------------------')
      }
    }

    if (factor_scores==TRUE){
      if (display_scores == TRUE){

        if (corr ==2){
          reli_good <- matrix(nrow = N,ncol = contSD+contAC+r)
          j = 1
          for (i in 1:N){
            if (sum(reli[i,] > 0.10) == (r+contSD+contAC)){
              reli_good[j,] <- reli[i,]
              j = j + 1
            }
          }

          reli_good <- reli_good[1:(j-1),]
        }

        cat('\n\n')
        cat(sprintf('RELIABILITY OF EAP SCORES\n\n'))
        prov <- contSD + contAC
        cat(sprintf('Factor         EAP Reliability estimate\n\n'))

        for (i in 1:(r+prov)){

          buff <- sprintf('Factor %.0f     :',i-prov)

          if (i ==1 && contSD == T){
            buff <- 'SD           :'
          }
          if (i == 1 && contSD == F && contAC == T){
            buff <- 'Acquiescence :'
          }
          if (i == 2 && contSD == T && contAC == T){
            buff <- 'Acquiescence :'
          }

          if (corr == 1){
            buff2 <- sprintf(' %.4f', reli[i])
          }
          else {
            buff2 <- sprintf(' %.4f',psych::harmonic.mean(reli_good[,i]))
          }

          cat(buff, buff2,'\n')

        }

        cat(sprintf('\nPARTICIPANTS\'S SCORES ON FACTORS:\n'))
        cat(sprintf('Rescaled to mean = 50 and standard deviation = 10 in the sample\n\n'))
        prmatrix(th*10+50)

        if (corr == 2){

          # If printing precision matrices for each factor, the output will be too large, the precision matrix are being saved in output$Precision_matrix
          cat(sprintf('\nNOTE: The precision matrices for the %2.0f factors were not printed for preventing console spacing issues.\n',contSD+contAC+r))
          cat(sprintf('These matrices are stored in $Precision_matrix in the output variable.\n\n'))

          # for (j in 1:(r+contSD+contAC)){
          #
          #   cat('\n\n')
          #   buff <- sprintf('FACTOR: CONTENT %1.0f      ',j-prov);
          #
          #   if (j ==1 && contSD == T){
          #     buff <- 'FACTOR: SOCIAL DESIRABILITY      '
          #   }
          #   if (j == 1 && contSD == F && contAC == T){
          #     buff <- 'FACTOR: ACQUIESCENCE      '
          #   }
          #   if (j == 2 && contSD == T && contAC == T){
          #     buff <- 'FACTOR: ACQUIESCENCE      '
          #   }
          #
          #   cat(buff, '\n\n')
          #
          #   cat(sprintf('RELIABILITY OF EAP FACTOR SCORES:   % .3f          \n\n',psych::harmonic.mean(reli[,j])))
          #   cat('Participant   Estimate        Approximate 90%      Posterior   Reliability\n')
          #   cat('              factor score    confidence interval  SD (PSD)\n\n')
          #
          #   for (i in 1:N){
          #     buff <- sprintf('%4.0f         % 7.3f        ',i,th[i,j])
          #     buff1 <- sprintf('(% 7.3f   % 7.3f)   ',th_li[i,j],th_ls[i,j])
          #     if (reli[i,j] > 0.10){
          #       buff2 <- sprintf('% .3f      % .3f',se[i,j],reli[i,j])
          #     }
          #     else{
          #       buff2 <- sprintf('% .3f       Inconsistent responder',se[i,j])
          #     }
          #     cat(buff,buff1,buff2,'\n')
          #   }
          #
          #   precision_matrix <- cbind(th[i,],th_li[i,],th_ls[i,],se[i,],reli[i,])
          #
          #
          # }
        }
      }
    }

    if (corr == 2 && smoothing_done == TRUE){
      # Smoothing performed
#      cat(sprintf('\nNOTE: The Polychoric/Tetrachoric matrix was not positive definite. An smoothing procedure was applied for %2.0f variables.\n',smoothing_v))
      message('\nNOTE: The Polychoric/Tetrachoric matrix was not positive definite. An smoothing procedure was applied (Bentler and Yuan, 2011).\n\n')
    }

    invisible(matrices)
  }


}
