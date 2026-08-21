
# TO DO:
# permit data to be a cor matrix
# SMT, EMPKC
# MAP not the default
# Nfactors=NULL
# permit extraction=='image'



EXTENSION_FA <- function(data, 
                         Ncore=ncol(data), Next=0, higherorder=TRUE, 
                         roottest='EMPKC',  
                         corkind='pearson',  
                         extraction='PAF', rotation='promax', 
                         Nfactors, NfactorsHO, 
                         Ndatasets=100, percentile=95, 
                         salvalue=.4, numsals=3, 
                         iterpaf=200, 
                         ppower=4, 
                         verbose=TRUE,
                         factormodel, rotate){
  
  #  Factor & Extension Analysis 
  
  #  primary sources: 
  #  Gorsuch, R. L. (1997a). Exploratory factor analysis: Its role 
  #  in item analysis. Journal of Personality Assessment, 68, 532-560
  
  #  Gorsuch, R. L. (1997b). New procedure for extension analysis in 
  #  exploratory factor analysis. Educational and Psychological 
  #  Measurement, 57, 725-740.
  
  #data(any(isnan(data)'),:) = [];
  
  # deprecated  
  if (!missing(factormodel))  extraction <- factormodel
  if (!missing(rotate))       rotation <- rotate
  
  data <- MISSING_DROP(data)
  
  Ncases <- nrow(data)
  Nvars  <- ncol(data) 
  
  # improper specification warnings
  if ((Ncore+Next) > Nvars) {
    message('\n\nWARNING: More core and/or extension variables were specified than 
	     there are variables in the raw data matrix: Expect error messages') 
  }
  if (Nvars > (Ncore+Next)){
    message('\n\nWARNING: There are more variables in the raw data set
         than there are core & extension variables. The data 
         matrix was trimmed to core+extension variables.\n')
    data <- data[,1:(Ncore+Next)]
    Nvars <- ncol(data) 
  }
  
  # specification notices
  if (corkind=='pearson')    ctype <- 'Pearson correlations' 
  if (corkind=='kendall')    ctype <- 'Kendall correlations' 
  if (corkind=='spearman')   ctype <- 'Spearman correlations' 
  if (corkind=='polychoric') ctype <- 'polychoric correlations' 
  if (corkind=='gamma')      ctype <- 'Goodman-Kruskal correlations' 
  
  if (roottest=='Nsalient')  ntype <- '# of Salient Loadings' 
  if (roottest=='parallel')  ntype <- 'Parallel Analysis' 
  if (roottest=='PA_PAF')    ntype <- 'Parallel Analysis of Principal Axis / Common Factors' 
  if (roottest=='MAP')       ntype <- 'Velicers MAP test' 
  if (roottest=='SEscree')   ntype <- 'Standard Error Scree' 
  if (roottest=='#evals>1')  ntype <- '# of Eigenvalues > 1' 
  if (roottest=='user')      ntype <- 'User-Specified' 
  
  if (extraction=='PAF')   etype <- 'Principal Axis / Common Factor Analysis' 
  if (extraction=='PCA')   etype <- 'Principal Components' 
  if (extraction=='ML')    etype <- 'Maximum Likelihood' 
  #if (extraction=='image') etype <- 'Image Analysis' 
  
  if (rotation=='promax')  rtype <- 'Promax' 
  if (rotation=='varimax') rtype <- 'Varimax'
  if (rotation=='none')    rtype <- 'No Rotation' 
  
  # correlation matrix
  if (corkind=='pearson')      rdata <- cor(data, method='pearson') 
  if (corkind=='kendall')      rdata <- cor(data, method='kendall') 
  if (corkind=='spearman')     rdata <- cor(data, method='spearman') 
  if (corkind=='polychoric')   rdata <- POLYCHORIC_R(data, verbose=FALSE)
  if (corkind=='gamma')        rdata <- Rgamma(data, verbose=FALSE)
  
  # initializing output objects because some may not be computed
  fits1 <- rff <- corelding <- extcorrel <- fits2 <- rfflding <- ldingsef <- extsef <- NA
  rcore <- rdata[1:Ncore,1:Ncore];  
  
  # eigenvalues for the core variables
  evals1 <- cbind(eigen(rcore) $values)
  
  # fit coefficients for the core variables
  fits1 <- ROOTFIT(data, corkind='pearson', extraction=extraction, verbose = 'FALSE')
  
  # number of factors for the core variables
  if (roottest == 'user') { Nfactors1 <- Nfactors
  } else { Nfactors1 <- DIMTESTS(rcore, tests=roottest, corkind=corkind, Ncases=Ncases, 
                                 display=0)$NfactorsDIMTESTS }
  
  warnnf1 <- 0
  if (Nfactors1 == 0) { Nfactors1 <- 1; warnnf1 <- 1 }
  
  
  # factor extraction
  if (extraction=='PCA') {
    pca.out <- PCA(rcore, corkind=corkind, Nfactors=Nfactors1, Ncases=Ncases, 
                   rotation='none', verbose=FALSE)
    lding1 <- pca.out$loadingsNOROT
  }
  if (extraction=='PAF')  {
    paf.out <- PA_FA(rcore, Nfactors=Nfactors1, Ncases=Ncases, iterpaf=iterpaf)
    lding1 <- paf.out$loadingsNOROT
  }
  if (extraction=='ML') {	
    maxlike.out <- MAXLIKE_FA(rcore, Nfactors=Nfactors1, Ncases=Ncases)
    lding1 <- maxlike.out$loadingsNOROT
  }
  #if (extraction=='image') lding1 <- IMAGE_FA(rcore, Nfactors=Nfactors1, 
                                             # Ncases=Ncases, 'loadings')
  
  
  # rotation, which also gives the factor intercorrelations
  if (Nfactors1==1)  rff <- 1 
  
  if (Nfactors1 > 1) {
    if (rotation=='promax') {
      promax.out <- PROMAX(lding1, ppower=ppower, verbose=FALSE) 
      lding1 <- promax.out$structure
      rff    <- promax.out$phi
      diag(rff) <- 1  # some other functions sometimes read the diagonal 1s as not 1s
    }
    if (rotation=='varimax') lding1 <- VARIMAX(lding1, verbose=FALSE)$loadingsV             
  }
  lding1 <- cbind(lding1)
  
  
  # for regular extension analysis
  # supermatrix: core variables, extension variables, & the factor loadings
  sup <- matrix(0, (ncol(rdata)+Nfactors1) , (ncol(rdata)+Nfactors1))
  sup[1:nrow(rdata),1:ncol(rdata)] <- rdata
  sup[ (nrow(sup)-(Nfactors1-1)):nrow(sup), 1:nrow(lding1) ] <- t(lding1[1:Ncore,])
  # correlations between extension variables & the factors
  ppp <- matrix(0, nrow(sup) , ncol(sup))
  resid <- sup
  for (lupe  in 1:Ncore) {
    for (luper in lupe:nrow(sup)) { ppp[luper,lupe] <- resid[luper,lupe] / sqrt(resid[lupe,lupe]) }
    resid <-  resid - ppp[,lupe] %*% t(ppp[,lupe]) 
  }
  pvo <- ppp[1:Ncore,1:Ncore]
  peo <- ppp[(Ncore+1):(Ncore+Next),1:Ncore]
  pfo <- ppp[ (Ncore+Next+1):(nrow(ppp)) , 1:Ncore]
  if (Nfactors1 > 1)  sef1 <- peo %*% t(pfo)
  if (Nfactors1 == 1) sef1 <- peo %*% pfo
  
  # higher order factor/extension analysis
  if (higherorder==TRUE & Nfactors1 > 1 & rotation=='promax') {
    
    # eigenvalues for the higher order variables (factors)
    evals2 <- cbind(eigen(rff)$values)     
    
    # fit coefficients for the higher order factors
    fits2 <- ROOTFIT(rff, corkind='pearson', Ncases=Ncases, extraction=extraction, verbose = 'FALSE')
    
    # number of factors for the higher order factors
    if (roottest == 'user') { Nfactors2 <- NfactorsHO
    } else { Nfactors2 <- DIMTESTS(rff, tests=roottest, corkind=corkind, Ncases=Ncases, 
                                   display=0)$NfactorsDIMTESTS }
    
    
    warnnf2 <- 0
    if (Nfactors2==0) { 
      Nfactors2 <- 1
      warnnf2 <- 1 
    }
    
    # factor extraction
    if (extraction=='PCA') {
      pca.out <- PCA(rff, corkind=corkind, Nfactors=Nfactors2, Ncases=Ncases, 
                     rotation='none', verbose=FALSE)
      lding2 <- pca.out$loadingsNOROT
    }
    if (extraction=='PAF')  {
      paf.out <- PA_FA(rff, Nfactors=Nfactors2, Ncases=Ncases, iterpaf=iterpaf)
      lding2 <- paf.out$loadingsNOROT
    }
    if (extraction=='ML') {	
      maxlike.out <- MAXLIKE_FA(rff, Nfactors=Nfactors2, Ncases=Ncases)
      lding2 <- maxlike.out$loadingsNOROT
    }
    #if (extraction=='image') lding2 <- IMAGE_FA(rff, Nfactors=Nfactors2, Ncases=Ncases, 'loadings')
    
    
    # rotation, which also gives the factor intercorrelations
    if (Nfactors1 > 1) {
      if (rotation=='promax') {
        promax.out <- PROMAX(lding2, ppower=ppower, verbose=FALSE) 
        lding2 <- promax.out$structure
        #rff    <- promax.out$phi
      }
    }
    
    # supermatrix: core variables, extension variables, & the factor loadings
    sup <- matrix(0, (Nfactors1+Ncore+Next+Nfactors2) , (Nfactors1+Ncore+Next+Nfactors2))
    rff    <- cbind(rff)
    lding2 <- cbind(lding2)
    
    # the core variables for the higher order analysis = the factor intercorrelations
    sup[1:nrow(rff),1:ncol(rff)] <- rff
    
    # the first set of extension variables
    sup[ (Nfactors1+1):(Nfactors1+Ncore), 1:(ncol(lding1)) ] <- lding1
    
    # the possible additional extension variables
    if (Next > 0) {
      sup[ (Nfactors1+Ncore+1):(Nfactors1+Ncore+Next) , 1:Nfactors2 ] <-  
        rdata[ (Ncore+1):(Ncore+Next), 1:Nfactors2 ] }
    
    # the loadings (correlations) from the higher order factor analysis
    sup[ (nrow(sup)-(Nfactors2-1)):nrow(sup), 1:nrow(lding2) ] <- t(lding2)
    Next2 <- Ncore + Next;  
    Ncore2 <- Nfactors1; 
    
    # correlations between extension variables & the higher order factors
    ppp <- matrix(0, nrow(sup) , ncol(sup))
    resid <- sup
    for (lupe  in 1:Ncore2) {
      for (luper in  lupe:nrow(sup)){ ppp[luper,lupe] <- resid[luper,lupe] / sqrt(resid[lupe,lupe]) }
      resid <-  resid - ppp[,lupe] %*% t(ppp[,lupe]) 
    }
    pvo <- ppp[1:Ncore2,1:Ncore2]
    peo <- ppp[(Ncore2+1):(Ncore2+Next2),1:Ncore2]
    nnn <- (nrow(ppp)) -  (Ncore2+Next2+1) + 1
    ccc <- Ncore2
    pfo <- as.matrix(ppp[ (Ncore2+Next2+1):(nrow(ppp)) , 1:Ncore2],  nrow=ccc ,  ncol=nnn)
    sef <- peo %*% pfo
    # if (nrow(pfo) == 2)  {
    # sef <- peo %*% pfo[1,]
    # sef <- peo * pfo[1,]
  }
  
  if (higherorder==FALSE | (higherorder==TRUE & Nfactors1 == 1) | (higherorder==TRUE & rotation != 'promax')){
    # Core Variable Loadings on the Factors
    corelding <- cbind((1:nrow(lding1)), lding1)
    colnames(corelding) <- cbind(matrix(('Variable'),1,1), (matrix(('Factor'),1,ncol(lding1))))
    #rownames(corelding) <- matrix((''),nrow(lding1),1)
    rnoms <- data.frame(colnames(data, do.NULL = FALSE, prefix = 'row'))
    rownames(corelding) <- rnoms[1:nrow(lding1),1]
    if (Next > 0) {
      # Extension Variable Correlations with the Factors 
      #extcorrel <- cbind((1:nrow(sef1)), sef1)
      extcorrel <- cbind((nrow(lding1)+1):ncol(data), sef1)
      #rownames(extcorrel) <- matrix((''),nrow(extcorrel),1)
      rownames(extcorrel) <- rnoms[(nrow(lding1)+1):ncol(data),1]
      colnames(extcorrel) <- cbind(matrix(('Variable'),1,1), (matrix(('Factor'),1,ncol(sef1))))
    }
  }
  
  # Warning message
  if (higherorder==TRUE & Nfactors1 == 1) {
    message('\n\nWARNING: Higher Order Factor/Extension Analysis was requested, but there was
         only one factor in the lower-order data.
         The higher order analyses were not performed.') 
  }
  
  # Warning message
  if (higherorder==TRUE & rotation != 'promax') {
    message('\n\nWARNING: Higher Order Factor/Extension Analysis was requested, as was
         a ROTATE option that does not produce correlations between between factors.
         The higher order analyses were not performed.') 
  }
  
  if (higherorder==TRUE & Nfactors1 > 1 & rotation == 'promax') {
    # Factor Intercorrelations from the First Factor Analysis and, in the
    # far-right collumn(s), the Loadings on the Higher Order Factor(s):
    rfflding <- cbind((1:nrow(lding2)), rff, lding2)
    colnames(rfflding) <- cbind(matrix(('Factor'),1,1), 
                                (matrix(('r'),1,ncol(rff))), (matrix(('Loading'),1,ncol(lding2))))
    rownames(rfflding) <- matrix((''),nrow(rfflding),1)
    
    # Core Variable Loadings on the Lower Order Factors and, in the
    # far-right collumn(s), their Correlations with the Higher Order Factor(s):
    ldingsef <- cbind((1:nrow(lding1)), lding1, sef[1:Ncore,])
    colnames(ldingsef) <- cbind(('Variable'), matrix(('Loadings'),1,ncol(lding1)),
                                matrix(('       r'),1,ncol(sef)))
    #rownames(ldingsef) <- matrix((''),nrow(ldingsef),1)
    rnoms <- data.frame(colnames(data, do.NULL = FALSE, prefix = 'row'))
    rownames(ldingsef) <- rnoms[1:nrow(ldingsef),1]
    if (Next > 0) {
      # Extension Variable Correlations with the Lower Order Factor(s) and, in the
      # far-right collumn(s), their Correlations with the Higher Order Factor(s):
      extsef <- cbind((1:Next), sef1, sef[ (Ncore+1):nrow(sef),])
      colnames(extsef) <- cbind(matrix(('Variable'),1,1), matrix(('r(LO)'),1,ncol(sef1)),
                                matrix(('r(HO)'),1,ncol(sef)))
      #rownames(extsef) <- matrix((''),nrow(extsef),1)
      rownames(extsef) <- rnoms[(nrow(ldingsef)+1):ncol(data),1]
    }
  }
  
  if (verbose) { 
    
    cat('\n\n\nEXTENSION FACTOR ANALYSIS')
    cat('\n\nNumber of Cases = ', Ncases)
    cat('\n\nTotal Number of Variables = ', Nvars)
    cat('\n\nNumber of Core Variables = ', Ncore)
    cat('\n\nNumber of Extension Variables = ', Next)
    cat('\n\nCorrelations to be Analyzed: ', ctype)
    cat('\n\nTest for Number of Factors: ', ntype)
    cat('\n\nFactor Extraction Procedure: ', etype)
    cat('\n\nRotation Procedure: ', rtype, '\n')
    
    if (higherorder==FALSE | (higherorder==TRUE & Nfactors1 == 1) | (higherorder==TRUE & rotation != 'promax')) {
      cat('\nEigenvalues & Fit Coefficients for the Core Variables:\n\n')
      print(round(fits1,2))
      
      if (warnnf1 == 1) {
        cat('\n\nWARNING: Zero factors were found in the data.
         The number of factors was therefore set at 1.') 
      }
      
      cat('\nNumber of factors in the core variables = ', Nfactors1, '\n')
      
      if (Nfactors1 == 1)  cat('\n\nWARNING: There was only one factor, rotation not performed\n') 
      
      if (rotation == 'promax' & Nfactors1 > 1) {
        cat('\nFactor Intercorrelations:\n\n')
        print(round(rff,2))
      }
      
      cat('\nCore Variable Loadings on the Factors:\n\n')
      corelding <- cbind((1:nrow(lding1)), lding1)
      colnames(corelding) <- cbind(matrix(('Variable'),1,1), (matrix(('Factor'),1,ncol(lding1))))
      #rownames(corelding) <- matrix((''),nrow(lding1),1)
      rnoms <- data.frame(colnames(data, do.NULL = FALSE, prefix = 'row'))
      rownames(corelding) <- rnoms[1:nrow(lding1),1]
      print(round(corelding,2))
      
      if (Next > 0) {
        cat('\nExtension Variable Correlations with the Factors\n\n') 
        #extcorrel <- cbind((1:nrow(sef1)), sef1)
        extcorrel <- cbind((nrow(lding1)+1):ncol(data), sef1)
        #rownames(extcorrel) <- matrix((''),nrow(extcorrel),1)
        rownames(extcorrel) <- rnoms[(nrow(lding1)+1):ncol(data),1]
        colnames(extcorrel) <- cbind(matrix(('Variable'),1,1), (matrix(('Factor'),1,ncol(sef1))))
        print(round(extcorrel,2)) 
      }
    }
    
    if (higherorder==TRUE & Nfactors1 > 1 & rotation == 'promax') {
      cat('\nEigenvalues & Fit Coefficients for the First Set of Core Variables:\n')
      print(round(fits1,2))
      
      cat('\nNumber of Factors in the First Set of Core Variables = ', Nfactors1)
      cat('\nEigenvalues & Fit Coefficients for the Higher Order Factor Analysis:\n')
      print(round(fits2,2))
      
      if (warnnf2 == 1) {
        cat('\n\nWARNING: Zero factors were found in the higher order data.
         The number of factors was therefore set at 1.')
      }
      
      cat('\nNumber of Factors for the Higher Order Factor Analysis = ', Nfactors2) 
      if (Nfactors2 == 1) {
        cat('\n\nWARNING: There was only one higher order factor, rotation not performed\n') 
      }
      
      cat('\nFactor Intercorrelations from the First Factor Analysis and, in the
       far-right collumn(s), the Loadings on the Higher Order Factor(s):\n')
      print(round(rfflding,2))
      
      cat('\nCore Variable Loadings on the Lower Order Factors and, in the
far-right collumn(s), their Correlations with the Higher Order Factor(s):\n')
      print(round(ldingsef,2))
      
      if (Next > 0) {
        cat('\nExtension Variable Correlations with the Lower Order Factor(s) and, in the
far-right collumn(s), their Correlations with the Higher Order Factor(s):\n')
        print(round(extsef,2)) 
      }
    }
    
  }
  
  extensionOutput <- list(
    fits1=fits1,
    rff=rff,
    corelding=corelding,
    extcorrel=extcorrel,
    fits2=fits2,
    rfflding=rfflding,
    ldingsef=ldingsef,
    extsef=extsef)
  
  return(invisible(extensionOutput))
}