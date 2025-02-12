
# X <- Z <- K <- EVD <- NULL; scale = TRUE; pairwise=TRUE; verbose = TRUE
getGenCov <- function(y, X = NULL, Z = NULL, K = NULL, trn = NULL,
                      EVD = NULL, ID_geno, ID_trait, scale = TRUE,
                      pairwise = TRUE, verbose = TRUE, ...)
{
  # K=G0; X = NULL; Z = NULL; EVD = NULL; scale = TRUE
  if(length(dim(y)) == 2){
    y <- as.matrix(y)
    if(missing(ID_geno) & missing(ID_trait)){
      ID_geno <- as.vector(row(y))
      ID_trait <- as.vector(col(y))
    }
  }
  y <- as.vector(y)

  if(is.null(trn)){
    trn <- which(!is.na(y))
    if(any(is.na(y)) & verbose){
      message("The training set was composed of the non-NA entries in the response 'y'")
    }
  }

  dat0 <- get_common_trn(y=y, ID_geno=ID_geno, ID_trait=ID_trait, trn=trn)
  if(length(dat0) == 0){
    stop("No common training set to all traits was found")
  }
  if(verbose){
    message("Calculating genetic covariances using nTRN = ",nrow(dat0)," records")
  }

  ntraits <- length(table(dat0$ID_trait))
  if(ntraits < 2L){
    stop("'ID_trait' should be a vector with at least 2 levels")
  }

  dat <- reshape2::dcast(dat0, ID_geno ~ ID_trait, value.var="y")
  geno_names <- dat$ID_geno
  trait_names <- colnames(dat)[-1]

  if(scale){
    sdy <- apply(dat[,-1], 2, sd, na.rm=TRUE)
    dat[,-1] <- scale(dat[,-1], center=FALSE, scale=sdy)
  }else{
    sdy <- rep(1,ntraits)
    names(sdy) <- trait_names
  }

  # Create an index for pairwise models
  tmp <- expand.grid(j=1:ntraits, i=1:ntraits)
  INDEX <- data.frame(pos=ntraits+seq(ntraits*(ntraits-1)/2),
                      tmp[tmp$i < tmp$j,c("i","j")])
  if(!pairwise){
    INDEX <- INDEX[INDEX$i==1,]
  }

  # Create a data.frame with only the TRN data from 'y'
  # followed by the sum of responses 1+2, 1+3, ...,(ntraits-1)+ntraits
  names0 <- names1 <- rep(NA, nrow(INDEX))
  tmp <- reshape2::melt(dat, id="ID_geno", variable.name="ID_trait", value.name="y")
  y0 <- tmp$y
  ID_geno0 <- tmp$ID_geno
  ID_trait0 <- as.character(tmp$ID_trait)
  for(k in 1:nrow(INDEX)){
    tt <- INDEX[k,]
    names0[k] <- paste0(tt$i,"_",tt$j)
    names1[k] <- paste0(trait_names[tt$i],"_",trait_names[tt$j])
    y0 <- c(y0, dat[,1+tt$i] + dat[,1+tt$j])
    ID_geno0 <- c(ID_geno0, geno_names)
    ID_trait0 <- c(ID_trait0, rep(names1[k],nrow(dat)))
  }
  names0 <- c(seq(ntraits),names0)
  names1 <- c(trait_names,names1)
  ID_trait0 <- factor(ID_trait0, levels=names1)

  fm <- fitBLUP(y=y0, X=X, Z=Z, K=K, EVD=EVD, BLUP=FALSE,
                ID_geno=ID_geno0, ID_trait=ID_trait0, verbose=verbose, ...)

  varUi <- fm$varU[trait_names]*(sdy^2)   # Scale using their initial SD
  varEi <- fm$varE[trait_names]*(sdy^2)

  # Fixed effects
  if(is.null(fm$b)){
    b <- NULL
  }else{
    b <- sweep(fm$b[,trait_names,drop=F],2L,sdy,FUN="*")
  }

  if(pairwise){
    varU <- diag(as.vector(varUi))
    varE <- diag(as.vector(varEi))
    dimnames(varU) <- dimnames(varE) <- list(trait_names,trait_names)
  }else{
    # Only the gencov between the first y[,1] and the
    # remaining y[,2:q] response variables
    covU <- covE <- rep(NA, ntraits-1)
    names(covU) <- names(covE) <- names1[INDEX$pos]
  }

  for(k in 1:nrow(INDEX)){
    pos <- INDEX$pos[k]
    i <- INDEX$i[k]
    j <- INDEX$j[k]

    # Genetic and Environmental covariances
    varUij <- 0.5*sdy[i]*sdy[j]*(fm$varU[pos] - fm$varU[i] - fm$varU[j])
    varEij <- 0.5*sdy[i]*sdy[j]*(fm$varE[pos] - fm$varE[i] - fm$varE[j])

    if(pairwise){
      varU[i,j] <- varU[j,i] <- varUij
      varE[i,j] <- varE[j,i] <- varEij
    }else{
      covU[k] <- varUij
      covE[k] <- varEij
    }
  }

  convergence <- fm$convergence
  names(convergence) <- names0

  # Output
  if(pairwise){
    out <- list(varU=varU, varE=varE, b=b, convergence=convergence)
  }else{
    out <- list(varU=varUi, varE=varEi, covU=covU, covE=covE, b=b, convergence=convergence)
  }
  return(out)
}
