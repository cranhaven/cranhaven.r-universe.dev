###ecpc method to learn from co-data:
#Fit a generalised linear (linear, logistic) or Cox model, penalised with adaptive multi-group penalties.
#The method combines empirical Bayes estimation for the group hyperparameters with an extra level of shrinkage
#to be able to handle various co-data, including overlapping groups, hierarchical groups and continuous co-data.
#In R-studio: press Alt+O to fold code into sections

ecpc <- function(Y,X,
                 Z=NULL,paraPen=NULL,paraCon=NULL,intrcpt.bam=TRUE,bam.method="ML",
                 groupsets=NULL,groupsets.grouplvl=NULL,hypershrinkage=NULL, #co-data former
                 unpen=NULL,intrcpt=TRUE,model=c("linear", "logistic", "cox"),
                 postselection="elnet,dense",maxsel=10,
                 lambda=NULL,fold=10,sigmasq=NaN,w=NULL,
                 nsplits=100,weights=TRUE,profplotRSS=FALSE,
                 Y2=NULL,X2=NULL,compare=TRUE,
                 mu=FALSE,normalise=FALSE,silent=FALSE,
                 datablocks=NULL,est_beta_method=c("glmnet","multiridge")#,standardise_Y=FALSE
                 #nIt=1,betaold=NaN
                 ){
  #-1. Description input --------------------------------------------------------------------------
  #
  #Data and co-data:
  # Y: nx1 vector with response data
  # X: nxp matrix with observed data
  # Z: pxG matrix with co-data on the p covariates 
  # groupsets: list of m elements, each element one co-data group set
  #            with each group set a list of groups containing the indices of covariates in that group 
  # groupsets.grouplvl: (optional) hierarchical groups define a group set on group level.
  #            list of m elements (corresponding to groupsets), with NULL if there is no structure on group level, or 
  #            with a list of groups containing the indices of groups of covariates in that group
  #
  #Model:
  # hypershrinkage: vector of m strings indicating shrinkage type used in level of extra shrinkage for each of the m group sets, 
  #                either in the the form "type" 
  #                or "type1,type2", in which type1 is used to select groups, and type 2 to estimate selected group parameters
  # unpen: vector with indices of covariates that should not be penalised (TD: adapt .mlestlin)
  # intrcpt: TRUE/FALSE to use/not use intercept (always unpenalised)
  # model: type of response Y (linear, logistic or Cox)
  # postselection: TRUE/FALSE if parsimonious model is/is not needed, or string with type of posterior selection method;
  #               "elnet", or "DSS" for corresponding post-selection method used
  # maxsel: vector with maximum number of penalised covariates to be selected (additional to all unpenalised covariates)
  #
  #Global hyperparameter estimation:
  #NOTE: tau^2_{global}=sigma^2/lambda (linear) or 1/lambda (logistic,Cox)
  # lambda: -numeric value for lambda to be used to compute initial beta on which EB estimates are based, or
  #         -"ML" or "CV" if (approximate) ML criterium or cross-validation needs to be used to estimate overall lambda
  # fold: number of folds used in inner CV if tau^_{global}^2 is estimated with CV
  # sigmasq: (linear model) given variance of Y~N(X*beta,sd=sqrt(sigmasq)), else estimated
  # w: (optional) mx1 vector to fix group set weights at given value.
  #
  #Local hyperparameter estimation:
  # nsplits: number of splits used in RSS criterion in the extra level of shrinkage
  # weights: TRUE/FALSE to use weights in ridge hypershrinkage to correct for group size
  #
  #Optional settings:
  # Y2,X2: (optional) independent data set for performance check 
  # compare: -TRUE if to grridge to be compared with glmnet, with same lambda. 
  #          -if "CV" or "ML", (possibly) different lambda used in glmnet (lambda "ML", "CV" specifies which method is used for grridge lambda)
  #          -for logistic/cox: if "MoM", CV approximation as initial value + MoM for moment iteration on whole group
  # silent: set to TRUE to suppress output messages
  #
  #Experimental settings:
  # mu: TRUE/FALSE to include/exclude group prior means (default FALSE)
  # normalise: TRUE if group variances should be normalised to sum to 1 (default FALSE)
  # nIt: number of times ecpc is iterated (default 1)
  # betaold: if beta known for similar, previous study, can be used as weights for group mean beta_old*mu (default unknown)

  nIt=1;
  betaold=NaN
  gammaForm=FALSE #co-data with bam
  minlam <- 0
  if(!all(est_beta_method %in% c("glmnet", "multiridge"))){
    warning("Estimation method for betas should be either glmnet or multiridge, set to multiridge")
    est_beta_method <- "multiridge"
  }
  if(length(est_beta_method)>1){
    est_beta_method <- "multiridge"
  }

  #-1.1 Check variable input is as expected--------------------------------------------------
  #check response Y, missings not allowed
  checkmate::assert(checkVector(Y, any.missing=FALSE), checkMatrix(Y, any.missing=FALSE), 
         checkArray(Y, max.d=2, any.missing=FALSE))
  checkmate::assert(checkLogical(Y), checkFactor(Y, n.levels=2), checkNumeric(Y))
  
  #check observed data X, missings not allowed
  checkmate::assert(checkMatrix(X, any.missing=FALSE), checkArray(X, max.d=2, any.missing=FALSE))
  checkmate::assertNumeric(X)
  
  #check whether dimensions Y and X match
  if(checkVector(Y)){
    if(length(Y)!=dim(X)[1]) stop("Length of vector Y should equal number of rows in X")
  }else{
    if(dim(Y)[1]!=dim(X)[1]) stop("Number of rows in Y and X should be the same")
  }
  
  #check co-data provided in either Z or groupsets and check format
  if(!is.null(Z)&!is.null(groupsets)){
    stop("Provide co-data either in Z or in groupsets, both not possible")
  }else if(is.null(Z)&is.null(groupsets)){
    print("No co-data provided. Regular ridge is computed corresponding to an 
          intercept only co-data model.")
    groupsets <- list(list(1:p))
  }else if(!is.null(Z)){
    #check if Z is provided as list of (sparse) matrices/arrays
    checkmate::assertList(Z, types=c("vector", "matrix", "array", "dgCMatrix"), min.len=1, any.missing=FALSE)
    for(g in 1:length(Z)){
      checkmate::assert(checkNumeric(Z[[g]], any.missing=FALSE), class(Z[[g]])[1]=="dgCMatrix")
      if(is.vector(Z[[g]])) Z[[g]] <- matrix(Z[[g]],length(Z[[g]]),1)
      #check if number of rows of Z match number of columns X
      if(dim(Z[[g]])[1]!=dim(X)[2]){
        stop(paste("The number of rows of co-data matrix",g, "should equal the 
                   number of columns of X, including (missing) co-data values for 
                   unpenalised variables."))
      } 
      #check number of co-data variables < variables
      if(dim(Z[[g]])[2] > dim(X)[2]){
        stop("Number of co-data variables in Z should be smaller than number of 
             variables in X")
      } 
      
      #check paraPen 
      checkmate::assertList(paraPen, types="list", null.ok = TRUE) #should be NULL or list
      if(!is.null(paraPen)){
        #check if names match Zi, i=1,2,..
        checkmate::assertSubset(names(paraPen), paste("Z", 1:length(Z), sep=""), empty.ok=FALSE)
        for(g in 1:length(Z)){
          nameZg <- paste("Z",g,sep="")
          if(nameZg%in%names(paraPen)){
            checkmate::assertList(paraPen[[nameZg]]) #should be named list
            #paraPen for mgcv may obtain L, rank, sp, Si with i a number 1,2,3,..
            checkmate::assertSubset(names(paraPen[[nameZg]]), 
                         c("L", "rank", "sp", paste("S",1:length(paraPen[[nameZg]]), sep="")))
            #elements Si, i=1,2,.. should be matrices and match number of columns of Zg
            for(nameSg in paste("S",1:length(paraPen[[nameZg]]), sep="")){
              if(nameSg%in%names(paraPen[[nameZg]])){
                #check whether Sg is a matrix or 2-dimensional array
                checkmate::assert(checkMatrix(paraPen[[nameZg]][[nameSg]]),
                       checkArray(paraPen[[nameZg]][[nameSg]], d=2))
                #check square matrix and dimension match co-data matrix
                if(dim(Z[[g]])[2]!=dim(paraPen[[nameZg]][[nameSg]])[1] |
                   dim(Z[[g]])[2]!=dim(paraPen[[nameZg]][[nameSg]])[2]){
                  stop(paste("Dimensions of the square penalty matrix",nameSg,
                             "should be equal to the number of columns in 
                             co-data matrix",g))
                }
              }
            }
          }
        }
      } 
      
      #check paraCon
      checkmate::assertList(paraCon, types="list", null.ok = TRUE) #should be NULL or list
      if(!is.null(paraCon)){
        #check if names match Zi, i=1,2,..
        checkmate::assertSubset(names(paraCon), paste("Z", 1:length(Z), sep=""), empty.ok=FALSE)
        for(g in 1:length(Z)){
          nameZg <- paste("Z",g,sep="")
          if(nameZg%in%names(paraCon)){
            checkmate::assertList(paraCon[[nameZg]], types=c("vector", "matrix", "array")) #should be named list
            #paraCon may obtain elements ("M.ineq" and "b.ineq") and/or ("M.eq" and "b.eq")
            namesparaCon <- names(paraCon[[nameZg]])
            checkmate::assertSubset(namesparaCon, c("M.ineq", "b.ineq", "M.eq", "b.eq"))
            if( ("M.ineq"%in%namesparaCon)&!("b.ineq"%in%namesparaCon) |
                !("M.ineq"%in%namesparaCon)&("b.ineq"%in%namesparaCon)){
              stop("Neither/both M.ineq and b.ineq should be provided in paraCon")
            }
            if( ("M.eq"%in%namesparaCon)&!("b.eq"%in%namesparaCon) |
                !("M.eq"%in%namesparaCon)&("b.eq"%in%namesparaCon)){
              stop("Neither/both M.eq and b.eq should be provided in paraCon")
            }
          }
        }
      }
      
      #check intercept term used for Z; intrcpt.bam
      checkmate::assertLogical(intrcpt.bam)
      
      #check type of method used for Z; bam.method
      checkmate::assertSubset(bam.method, c("GCV.Cp", "GACV.Cp", "REML", "P-REML", "ML", "P-ML", "fREML"))
    }
  }else{ #!is.null(groupsets)
    #check groupsets is a list of lists of vectors of integers (covariate indices)
    checkmate::assertList(groupsets, types=c("list"), min.len=1)
    for(g in 1:length(groupsets)){
      checkmate::assertList(groupsets[[g]], types="integerish", null.ok = TRUE)
    }
    
    #check groupsets.grouplvl (NULL or list)
    checkmate::assertList(groupsets.grouplvl, types=c("list","null"), null.ok = TRUE)
    if(length(groupsets.grouplvl)>0){
      #length should equal the number of group sets
      if(length(groupsets.grouplvl)!=length(groupsets)){
        stop("groupsets.grouplvl should be either NULL, or a list with at least one 
           element not equal to NULL, with the length of the list
           matching the length of the list provided in groupsets")
      }
      #elements in the group set on the group level should contain integers
      for(g in 1:length(groupsets.grouplvl)){
        checkmate::assertList(groupsets.grouplvl[[g]], types="integerish", null.ok = TRUE)
      }
    }
    
    #check hypershrinkage
    checkVector(hypershrinkage, null.ok = TRUE)
    if(is.null(hypershrinkage)){
      hypershrinkage<-rep("ridge", length(groupsets))
    }
    if(length(hypershrinkage)>0){
      if(length(hypershrinkage)!=length(groupsets)){
        stop("Number of elements in hypershrinkage should match that of groupsets")
      }
      for(g in 1:length(hypershrinkage)){
        checkmate::assertString(hypershrinkage[g]) #should be string
        checkmate::assertSubset(unlist(strsplit(hypershrinkage[g], split=',')),
                     c("none","ridge","lasso","hierLasso")) #should be combination of these types
      }
    }
  }
  
  #check unpen
  checkmate::assertIntegerish(unpen, null.ok=TRUE)
  
  #check intrcpt
  checkmate::assertLogical(intrcpt, len = 1)
  
  #check model
  checkmate::assert(checkSubset(model, c("linear", "logistic", "cox")),
         class(model)[1]=="family")
  
  #check postselection
  checkmate::assertScalar(postselection)
  checkmate::assert(postselection==FALSE,
         checkSubset(postselection,c( "elnet,dense", "elnet,sparse", 
                     "BRmarginal,dense", "BRmarginal,sparse", "DSS")))
  
  #check maxsel
  checkmate::assertIntegerish(maxsel, lower=1, upper=dim(X)[2]-1) #must be integers and fewer than number of variables
  
  #check lambda
  checkmate::assertScalar(lambda, null.ok=TRUE, na.ok = TRUE)
  checkmate::assert(checkNumeric(lambda, null.ok=TRUE),
         checkString(lambda))
  if(testString(lambda)){
    checkmate::assert(grepl("ML",lambda), grepl("CV", lambda))
  }
  
  #check fold
  checkmate::assertIntegerish(fold, lower=2, upper = dim(X)[1])
  
  #check sigmasq
  checkmate::assertNumeric(sigmasq, lower=0, len=1, null.ok=TRUE)
  
  #check w
  checkmate::assertNumeric(w, lower=0, len=ifelse(!is.null(Z), length(Z), length(groupsets)), 
                null.ok = TRUE)
  
  #check nsplits
  checkmate::assertIntegerish(nsplits, lower=1)
  
  #check weights
  checkmate::assertLogical(weights, len=1)
  
  #check profplotRSS
  checkmate::assertLogical(profplotRSS, len=1)
  
  #check test response Y2
  checkmate::assert(checkVector(Y2, null.ok=TRUE), checkMatrix(Y2, null.ok=TRUE), 
         checkArray(Y2, max.d=2, null.ok=TRUE))
  checkmate::assert(checkLogical(Y2, null.ok=TRUE), checkFactor(Y2, n.levels=2, null.ok=TRUE), 
         checkNumeric(Y2, null.ok=TRUE))
  
  #check test observed data X2
  checkmate::assert(checkMatrix(X2, null.ok=TRUE), checkArray(X2, max.d=2, null.ok=TRUE))
  checkmate::assertNumeric(X2, null.ok=TRUE)
  
  #check whether dimensions Y2 and X2 match
  if(!is.null(Y2)){
    if(checkVector(Y2)){
      if(length(Y2)!=dim(X2)[1]) stop("Length of vector Y2 should equal number of rows in X2")
    }else{
      if(dim(Y2)[1]!=dim(X2)[1]) stop("Number of rows in Y2 and X2 should be the same")
    }
  }
  #check whether number of variables in test data and training data match
  if(!is.null(X2)){
    if(dim(X2)[2]!=dim(X)[2]){
      stop("Number of columns in test data X2 should match that of training data X")
    }
  }
  
  #check compare
  checkmate::assertScalar(compare)
  checkmate::assert(checkLogical(compare),
         checkString(compare))
  if(testString(compare)){
    checkmate::assert(grepl("ML",compare), grepl("CV", compare))
  }
  
  #check mu
  checkmate::assertLogical(mu, len=1)
  
  #check normalise
  checkmate::assertLogical(normalise, len=1)
  
  #check silent
  checkmate::assertLogical(silent, len=1)
  
  #check datablocks
  checkmate::assertList(datablocks, types="integerish", null.ok=TRUE)
  
  #check est_beta_method
  checkmate::assertSubset(est_beta_method, c("glmnet", "multiridge"))
  
  #Save input colnames/rownames to return in output
  colnamesX <- colnames(X)
  if(!is.null(Z)){
    colnamesZ <- names(Z)
    codataNames <- unlist(lapply(1:length(Z),function(x){rep(paste("Z",x,sep=""),dim(Z[[x]])[2])}))
    codataSource <- unlist(lapply(1:length(Z),function(x){rep(x,dim(Z[[x]])[2])}))
    if(!is.null(names(Z))){
      codataNames <- unlist(lapply(1:length(Z),function(x){rep(names(Z)[x],dim(Z[[x]])[2])}))
    }
    codatavarNames <- unlist(lapply(Z,function(x){
      if(is.null(colnames(x))) return(1:dim(x)[2])
      colnames(x)
    }))
    namesZ <- paste(codataNames,codatavarNames,sep=".")
  }else{
    colnamesZ <- names(groupsets)
    codataNames <- unlist(lapply(1:length(groupsets),function(x){rep(paste("Z",x,sep=""),length(groupsets[[x]]))}))
    codataSource <- unlist(lapply(1:length(groupsets),function(x){rep(x,length(groupsets[[x]]))}))
    if(!is.null(names(groupsets))){
      codataNames <- unlist(lapply(1:length(groupsets),function(x){rep(names(groupsets)[x],length(groupsets[[x]]))}))
    }
    codatavarNames <- unlist(lapply(groupsets,function(x){
      if(is.null(colnames(x))) return(1:length(x))
      colnames(x)
    }))
    namesZ <- paste(codataNames,codatavarNames,sep=".")
  }

  
  #-2. Set-up variables ---------------------------------------------------------------------------
  n <- dim(X)[1] #number of samples
  p <- dim(X)[2] #number of covariates 
  
  if(!is.null(X2)) n2<-dim(X2)[1] #number of samples in independent data set x2 if given
  multi <- FALSE; if(!is.null(datablocks)) multi <- TRUE #use multiple global tau, one for each data block
  if(multi==FALSE) datablocks <- list((1:p)[!((1:p)%in%unpen)])

  cont_codata <- FALSE; if(!is.null(Z)) cont_codata <- TRUE
  
  if(class(model)[1]=="family"){
    #use glmnet package and cross-validation to compute initial global lambda en beta estimates
    fml <- model
    model <- "family"
    est_beta_method <- "glmnet"
    multi <- FALSE
    if(!is.numeric(lambda)) lambda <- "CV_glmnet"
  } 
  if(length(model)>1){
    if(all(is.element(Y,c(0,1))) || is.factor(Y)){
      model <- "logistic" 
    } else if(all(is.numeric(Y)) & !(is.matrix(Y) && dim(Y)[2]>1)){
      model <- "linear"
    }else{
      model <- "cox"
    }
  }
  levelsY<-NaN
  if(is.null(lambda)||is.nan(lambda)) lambda <- ifelse(model=="linear","ML","CV")
  if(model=="logistic"){
    levelsY<-cbind(c(0,1),c(0,1))
    if(lambda=="ML"){
      lambda <- "CV"
      if(!silent) print("For logistic model, use CV for overall tau.")
    }
    if(!all(is.element(Y,c(0,1)))){
    oldLevelsY<-levels(Y)
    levels(Y)<-c("0","1")
    Y<-as.numeric(Y)-1
    levelsY<-cbind(oldLevelsY,c(0,1))
    colnames(levelsY)<-c("Old level names","New level names")
    if(!is.null(Y2)){
      levels(Y2)<-c("0","1")
      Y2<-as.numeric(Y2)-1
    }
    #if(!silent) print("Y is put in 0/1 format, see levelsY in output for new names")
    print("Y is put in 0/1 format:")
    print(levelsY)
    }
  }
  if(model=='cox'){
    intrcpt <- FALSE
    if(length(lambda)==0) lambda <- "CV"
    if(lambda=="ML"){
      if(!silent) print("For cox model, no ML approximation for overall tau available. Use CV instead.")
      lambda <- "CV"
    }
    if(class(Y)[1]!="Surv"){
      Y <- survival::Surv(Y)
    }
  }
  switch(model,
         'linear'={
           Y <- c(Y) #make numeric vector 
           fml <- 'gaussian'
           sd_y <- sqrt(var(Y)*(n-1)/n)[1]
           # if(standardise_Y){
           #   Y <- Y/sd_y
           #   sd_y_former <- sd_y
           #   sd_y <- 1
           # }
         },
         'logistic'={
           Y <- c(Y)
           fml <- 'binomial'
           sd_y <- 1 #do not standardise y in logistic setting
           #sd_y_former <- sd_y
         },
         'cox'={
           fml <- 'cox'
           sd_y <- 1 #do not standardise y in cox regression setting
           #sd_y_former <- sd_y
         },
         "family"={
           sd_y <- 1
           if(fml$family%in%c("gaussian")) sd_y <- sqrt(var(Y)*(n-1)/n)[1]
         }
  )
  mutrgt<-0
  if(mu==FALSE){mu <- 0}else{
    if(cont_codata){
      warning("Co-data provided in Z instead of groupsets. This option does not 
               yet support inclusion of prior means. Prior means set to 0.")
      mu <- 0 
    }else{
      mu <- NaN
    }
  } 
  tauglobal<-NaN
  tausq<-NaN
  hyperlambdas<-c(NaN,NaN)
  
  # check whether unpenalised covariates are not in partition
  # and set penalty.factor of unpenalised covariates to 0 for glmnet
  penfctr <- rep(1,p) #factor=1 for penalised covariates
  if(length(unpen)>0){
    penfctr[unpen] <- 0 #factor=0 for unpenalised covariates
  }
  
  #-2.1 Variables describing groups and partition(s) =================================================
  if(!cont_codata){ #settings when co-data is provided in groupsets
    #remove any unpenalised covariates from group sets
    if(length(unpen)>0){
      if(any(unlist(groupsets)%in%unpen)){
        warning("Unpenalised covariates removed from group set")
        for(i in 1:length(groupsets)){
          for(j in 1:length(groupsets[[i]])){
            if(all(groupsets[[i]][[j]]%in%unpen)){
              groupsets[[i]][[j]] <- NULL #remove whole group if all covariates unpenalised
            }else{
              groupsets[[i]][[j]] <- groupsets[[i]][[j]][!(groupsets[[i]][[j]]%in%unpen)]
            }
          }
        }
      }
    }
    
    G <- sapply(groupsets,length) #1xm vector with G_i, number of groups in partition i
    m <- length(G) #number of partitions
    if(any(grepl("hierLasso",hypershrinkage))){
      if(length(groupsets.grouplvl)==0){
        stop("Group set on group level for hierarchical groups is missing")
      }
    }
    indGrpsGlobal <- list(1:G[1]) #global group index in case we have multiple partitions
    if(m>1){
      for(i in 2:m){
        indGrpsGlobal[[i]] <- (sum(G[1:(i-1)])+1):sum(G[1:i])
      }
    }
    Kg <- lapply(groupsets,function(x)(sapply(x,length))) #m-list with G_i vector of group sizes in partition i
    #ind1<-ind
    
    #ind <- (matrix(1,G,1)%*%ind)==(1:G)#sparse matrix with ij element TRUE if jth element in group i, otherwise FALSE
    i<-unlist(sapply(1:sum(G),function(x){rep(x,unlist(Kg)[x])}))
    j<-unlist(unlist(groupsets))
    ind <- Matrix::sparseMatrix(i,j,x=1) #sparse matrix with ij element 1 if jth element in group i (global index), otherwise 0
    
    Ik <- lapply(1:m,function(i){
      x<-rep(0,sum(G))
      x[(sum(G[1:i-1])+1):sum(G[1:i])]<-1
      as.vector(x%*%ind)}) #list for each partition with px1 vector with number of groups beta_k is in
    #sparse matrix with ij element 1/Ij if beta_j in group i
    
    #make co-data matrix Z (Zt transpose of Z as in paper, with co-data matrices stacked for multiple groupsets)
    Zt<-ind; 
    if(G[1]>1){
      Zt[1:G[1],]<-Matrix::t(Matrix::t(ind[1:G[1],])/apply(ind[1:G[1],],2,sum))
    }
    if(m>1){
      for(i in 2:m){
        if(G[i]>1){
          Zt[indGrpsGlobal[[i]],]<-Matrix::t(Matrix::t(ind[indGrpsGlobal[[i]],])/
                                               apply(ind[indGrpsGlobal[[i]],],2,sum))
        }
      }
    }
    if(dim(Zt)[2]<p) Zt <- cbind(Zt,matrix(rep(NaN,(p-dim(Zt)[2])*sum(G)),c(sum(G),p-dim(Zt)[2])))
    if(length(G)==1 && G==1){
      PenGrps <- matrix(sum(Zt^2),c(1,1))
    }else{
      PenGrps <- as.matrix(Zt[,!((1:p)%in%unpen)]%*%Matrix::t(Zt[,!((1:p)%in%unpen)])) #penalty matrix groups
    } 
    
  }else{ #settings when co-data is provided in list Z
    m <- length(Z)
    names(Z) <- paste("Z",1:m,sep="")
    for(i in 1:m){
      if(length(unpen)>0) Z[[i]][unpen,] <- NaN
    }
    G <- sapply(Z,function(x)dim(x)[2]) #1xm vector with G_i, number of variables in co-data source i
    
    indGrpsGlobal <- list(1:G[1]) #global group index in case we have multiple partitions
    if(m>1){
      for(i in 2:m){
        indGrpsGlobal[[i]] <- (sum(G[1:(i-1)])+1):sum(G[1:i])
      }
    }
    Zt <- t(Z[[1]])
    if(m>1){
      for(i in 2:m){
        Zt <- rbind(Zt,Matrix::t(Z[[i]]))
      }
    }
    Kg <- list(apply(Zt,1,function(x)(sum(!is.na(x))))) #m-list with G_i vector of group sizes in partition i
    
    if(is.null(hypershrinkage)){
      hypershrinkage <- rep("none",m)
      for(i in 1:m){
        bool.Pen <- names(Z)[i]%in%names(paraPen) #boolean indicating whether or not co-data weights should be penalised
        bool.Con <- names(Z)[i]%in%names(paraCon) #boolean indicating whether or not co-data weights should be constrained
        tempMat <- cbind(c("none","ridge"), c("none+constraints","ridge+constraints"))
        hypershrinkage[i] <- tempMat[1+bool.Pen, 1+bool.Con]
      }
      if(all(!grepl("constraints",hypershrinkage)) & sum(G)>1) hypershrinkage <- "mgcv"
    }else if(!all(hypershrinkage%in%c("none","ridge","mgcv","none+constraints","ridge+constraints"))){
      stop("Hypershrinkage should be one of none, ridge, none+constraints, ridge+constraints or mgcv.
              For co-data provided as matrix, hypershrinkage can set automatically.")
    }
    normalise <- FALSE
    
    Ik <- lapply(1:m,function(x) rep(1,p))
    PenGrps <- as.matrix(Zt[,!((1:p)%in%unpen)]%*%Matrix::t(Zt[,!((1:p)%in%unpen)])) #penalty matrix groups
  } 
  

  #-2.2 Weight variables for extra shrinkage on group parameters =====================================
  # Compute weights and corresponding weight matrix
  #Note: for logistic regression, another, different weight matrix W is defined below
  if(weights){
    weights <- unlist(Kg)
  }else{
    weights <- rep(1,sum(G))
  }
  if(length(weights)==1){Wminhalf<-1/sqrt(weights)}
  else{Wminhalf <- diag(1/sqrt(weights))} #W^{-0.5} (element-wise), with W diagonal matrix with weights for prior parameters

  #-3. The ecpc method possibly using extra shrinkage ---------------------------------------------
  #-3.1 Set-up variables =======================================================================================
  #-3.1.1 Variables adapted for intercept ####################################################################
  # copy X: add column with ones for intercept if included in the model
  if(intrcpt){
    Xc <- cbind(X,rep(1,n))
    unpen<-c(unpen,p+1) #add index of last column for intercept to unpenalised covariates 
  } else{
    Xc <- X
  }
  if(model%in%c("logistic","cox","family")){
    Xcinit<-Xc
  }
  
  #-3.1.2 Variables used in initialisation of beta (and afterwards) #########################################
  muhat <- array(mu,c(sum(G),nIt+1)); #group means, optional: fixed at mu if given
  gammatilde <- array(tausq,c(sum(G),nIt+1)) #group variances before truncating negative tau to 0, optional: fixed at tausq if given (tausq 1 value for all groups)
  gamma <- array(max(0,tausq),c(sum(G),nIt+1)) #group variances truncated at 0 for negative values, optional: fixed at tausq if given
  gamma0 <- 0
  gamma0tilde <- 0
  colnames(muhat)<-paste("Itr",0:nIt,sep="")
  colnames(gammatilde)<-paste("Itr",0:nIt,sep="")
  colnames(gamma)<-paste("Itr",0:nIt,sep="")
  tempRow <- unlist(lapply(1:length(G),function(x){paste("Z",x,".",1:G[x],sep="")}))
  rownames(muhat)<-tempRow;  rownames(gammatilde)<-tempRow;  rownames(gamma)<-tempRow
  
  
  weightsMu <- array(NaN,c(sum(G),nIt+1))
  if(is.nan(mu)){
    partWeightsMu <- array(1,c(m,nIt+1))
    partWeightsMuG<- array(1,c(sum(G),nIt+1))
  }else{
    partWeightsMu <- array(NaN,c(m,nIt+1))
    partWeightsMuG<- array(NaN,c(sum(G),nIt+1))
  }
  if(is.nan(tausq)){
    partWeightsTau <-array(1,c(m,nIt+1))
    partWeightsTauG<-array(1,c(sum(G),nIt+1))
  }else{
    partWeightsTau <-array(NaN,c(m,nIt+1))
    partWeightsTauG<-array(NaN,c(sum(G),nIt+1))
  }
  
  
  #-3.1.3 Variables used in iterations #######################################################################
  if(!is.null(X2)){
    if(model=="cox") YpredGR <- array(NaN,c(n2,nIt+1))
    else YpredGR <- array(NaN,c(n2,nIt+1))
    MSEecpc<-rep(NaN,nIt+1)
  } else { 
      YpredGR<-NaN
      MSEecpc<-NaN
      if(!is.nan(compare)){
        Ypredridge<-NaN
        MSEridge<-NaN
      }
  }
  ind0 <- c(); #keep track of index of groups which have 0 variance
  indnot0 <- 1:sum(G) #and keep track of index of groups which have variance > 0
  lambdashat<-array(NaN,c(2,nIt+1,m)) #hyperpenalties for extra level of shrinkage
  colnames(lambdashat)<-paste("Itr",0:nIt,sep="")
  rownames(lambdashat)<-c("PenMu","PenTau")
  lambdashat[1,,]<-hyperlambdas[1]; lambdashat[2,,]<-hyperlambdas[2] #optional: fix extra hyperpenalty if given 
  
  #-3.2 Initial tau and beta ========================================================================================
  if(!silent) print(paste("Estimate global tau^2 (equiv. global ridge penalty lambda)"))
  intrcptGLM <- intrcpt

  #inital tau given
  if(!is.nan(tausq)){
    lambda <- 1/tausq
    if(model=="linear" | (model=="family"&!is.nan(sigmasq))) lambda <- sigmasq/tausq
    if(!is.nan(compare) & compare!=FALSE){ #compare not false
      lambdaridge <- 1/tausq
    }
  }
  if(is.numeric(lambda) & compare!=FALSE) lambdaridge <- lambda
  
  datablockNo <- rep(1,p) #in case only one data type
  if(multi!=FALSE){
    if(!is.null(datablocks)){
      datablockNo <- c(unlist(lapply(1:length(datablocks),function(x){rep(x,length(datablocks[[x]]))}))) #p-dimensional vector with datablock number
    }else{
      datablocks <- list(1:p)
    }
    datablockNo[(1:p)%in%unpen]<-NA
    
    #compute multi-lambda; from multiridge package demo:
    #datablocks: list with each element a data type containing indices of covariates with that data type 
    Xbl <- multiridge::createXblocks(lapply(datablocks,function(ind) X[,intersect(ind,ind[!(ind%in%unpen)])]))
    XXbl <- multiridge::createXXblocks(lapply(datablocks,function(ind) X[,intersect(ind,ind[!(ind%in%unpen)])]))
    
    #Find initial lambda: fast CV per data block, separately using SVD. CV is done using the penalized package
    if(!is.numeric(lambda)){
      if(sum((1:p)%in%unpen)>0){
        capture.output({cvperblock <- multiridge::fastCV2(Xbl,Y=Y,kfold=fold,fixedfolds = FALSE,
                                                          X1=X[,(1:p)%in%unpen],intercept=intrcpt)})
      }else{
        capture.output({cvperblock <- multiridge::fastCV2(Xbl,Y=Y,kfold=fold,fixedfolds = FALSE,
                                                          intercept=intrcpt)})
      }
      lambdas <- cvperblock$lambdas
      lambdas[lambdas==Inf] <- 10^6
      
      #Find joint lambdas:
      if(length(lambdas)>1){
        leftout <- multiridge::CVfolds(Y=Y,kfold=fold,nrepeat=3,fixedfolds = FALSE) #Create (repeated) CV-splits of the data
        if(sum((1:p)%in%unpen)>0){
          capture.output({jointlambdas <- multiridge::optLambdasWrap(penaltiesinit=lambdas, XXblocks=XXbl,Y=Y,folds=leftout,
                                     X1=X[,(1:p)%in%unpen],intercept=intrcpt,
                                     score=ifelse(model == "linear", "mse", "loglik"),model=model)})
        }else{
          capture.output({jointlambdas <- multiridge::optLambdasWrap(penaltiesinit=lambdas, XXblocks=XXbl,Y=Y,folds=leftout,
                                     intercept=intrcpt,
                                     score=ifelse(model == "linear", "mse", "loglik"),model=model)})
        }
        
        lambda <- jointlambdas$optpen
      }else{
        lambda <- lambdas
      }
      
    }else{
      if(length(lambda)==1) lambdas <- rep(lambda, max(datablockNo))
    }
    
    lambdap <- rep(0,p)
    lambdap[!((1:p)%in%unpen)] <- lambda[datablockNo[!((1:p)%in%unpen)]]

    sigmahat <- 1 #sigma not in model for logistic: set to 1
    if(model=="linear"){
      if(!is.nan(sigmasq)) sigmahat <- sigmasq
      else{
        XtDinvX <- multiridge::SigmaFromBlocks(XXblocks = XXbl,lambda)
        if(length(unpen)>0){
          Xunpen <- X[,(1:p)%in%unpen]
          if(intrcpt) Xunpen <- cbind(Xunpen,rep(1,n))
          if(intrcpt && length(unpen)==1){
            betaunpenML <- sum(Y)/n
          }else{
            temp <- solve(XtDinvX+diag(rep(1,n)),Xunpen)
            betaunpenML <- solve(t(Xunpen)%*%temp , t(temp)%*%Y)
          }
          sigmahat <- c(t(Y-Xunpen%*%betaunpenML)%*%solve(XtDinvX+diag(rep(1,n)),Y-Xunpen%*%betaunpenML)/n)
        }else{
          sigmahat <- c(t(Y)%*%solve(XtDinvX+diag(rep(1,n)),Y)/n)
        }
        
        #sigmahat <- 1/n * (y-X[unpen] %*% solve(X%*%Lambda^{-1}%*%t(X) + diag(1,n))
        # lambdaoverall <- exp(mean(log(lambdap[lambdap!=0])))
        # Xacc <- X
        # Xacc[,!((1:p)%in%unpen)] <- as.matrix(X[,!((1:p)%in%unpen)] %*% 
        #                                         sparseMatrix(i=1:sum(!((1:p)%in%unpen)),j=1:sum(!((1:p)%in%unpen)),
        #                                                  x=c(1/sqrt(lambdap[lambdap!=0]/lambdaoverall))))
        # #Use ML for sigma estimation and/or initial lambda (tausq) estimate and/or mu
        # par <- .mlestlin(Y,Xacc,lambda=lambdaoverall,sigmasq=NaN,mu=0,tausq=NaN) #use maximum marginal likelihood
        # sigmahat <- par[2] #sigma could be optimised with CV in the end if not known
      }
    }
    muhat[,1] <- 0
    gamma[,1] <- rep(1,sum(G))
    tauglobal<- sigmahat/lambda #set global group variance
    mutrgt <- 0 #TD: with offset
    
    #Compute betas
    XXT <- multiridge::SigmaFromBlocks(XXbl,penalties=lambda) #create nxn Sigma matrix = sum_b [lambda_b)^{-1} X_b %*% t(X_b)]
    if(model!="cox"){
      if(sum((1:p)%in%unpen)>0){
        fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
      }else{
        fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt) #Fit. fit$etas contains the n linear predictors
      }
    }else{
      if(sum((1:p)%in%unpen)>0){
        fit <- multiridge::IWLSCoxridge(XXT,Y=Y, model=model,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
      }else{
        fit <- multiridge::IWLSCoxridge(XXT,Y=Y) #Fit. fit$etas contains the n linear predictors
      }
    }
    
    betas <- multiridge::betasout(fit, Xblocks=Xbl, penalties=lambda) #Find betas.
    intrcptinit <- c(betas[[1]][1]) #intercept
    betasinit <- rep(0,p) 
    betasinit[(1:p)%in%unpen] <- betas[[1]][-1] #unpenalised variables
    for(i in 1:length(datablocks)){
      betasinit[datablocks[[i]][!(datablocks[[i]]%in%unpen)]] <- betas[[1+i]]
    }  
    muinitp <- rep(0,p) #TD: with offset for mu
    
    rm(betas)
    #compare multiridge
    if(compare!=FALSE){
      lambdaridge <- lambda
    }
  }else{
    switch(model,
           'linear'={
             #Use Cross-validation to compute initial lambda (tausq)
             if((!is.nan(compare) & grepl("CV",compare)) | grepl("CV",lambda)){
               #use glmnet to do CV; computationally more expensive but other optimising criteria possible
               if(grepl("glmnet",lambda)){ 
                 lambdaGLM<-glmnet::cv.glmnet(X,Y,nfolds=fold,alpha=0,family=fml,
                                              standardize = FALSE,intercept=intrcpt,
                                              penalty.factor=penfctr,keep=TRUE) #alpha=0 for ridge
               }#else if(grepl("penalized",lambda)){ #use penalized to do CV
               #   Xsvd <- svd(X[,penfctr!=0])
               #   XF <- X[,penfctr!=0]%*% Xsvd$v
               #   Xunpen <- cbind(X[,penfctr==0]) #if empty vector, no unpenalised and no intercept
               #   if(intrcpt){
               #     Xunpen <- cbind(X[,penfctr==0],rep(1,n))
               #   }
               #   if(length(unpen)==0){
               #     ol1 <- penalized::optL2(Y,penalized=XF,fold=fold,trace=FALSE)
               #   }else{
               #     ol1 <- penalized::optL2(Y,penalized=XF, unpenalized =Xunpen,fold=fold,trace=FALSE)
               #   }
               #   
               #   #ol2 <- penalized::optL2(Y,penalized=X[,penfctr!=0],unpenalized=Xunpen, fold=ol1$fold ) #gives same result, but the first is much faster for large p
               #   itr2<-1
               #   while((ol1$lambda>10^12 | ol1$lambda<10^-5 ) & itr2 < 10){
               #     if(length(unpen)==0){
               #       ol1 <- penalized::optL2(Y,penalized=XF,fold=fold,trace=FALSE)
               #     }else{
               #       ol1 <- penalized::optL2(Y,penalized=XF, unpenalized =Xunpen,fold=fold,trace=FALSE)
               #     }
               #     itr2 <- itr2 + 1
               #   } 
               #   if(itr2==10 & ol1$lambda>10^12){
               #     if(ol1$lambda>10^10){
               #       ol1$lambda <- 10^12
               #       warning("Cross-validated global penalty lambda was >10^12 and set to 10^12")
               #     }
               #     if(ol1$lambda<10^-5){
               #       ol1$lambda <- 1
               #       warning("Cross-validated global penalty lambda was <10-5 and set to 1")
               #     }
               #   }
               #}
               else{ #do CV with fastCV2 from multiridge package
                 if(length(setdiff(unpen,p+1))==0){
                   Xbl <- X%*%t(X)
                   capture.output({fastCVfit <- multiridge::fastCV2(XXblocks=list(Xbl),Y=Y,intercept=intrcpt,
                                                        fixedfolds=FALSE,model=model,kfold=fold)})
                 }else{
                   Xbl <- X[,penfctr!=0]%*%t(X[,penfctr!=0])
                   capture.output({fastCVfit <- multiridge::fastCV2(XXblocks=list(Xbl),Y=Y,intercept=intrcpt,
                                    fixedfolds=FALSE,model=model,X1=X[,penfctr==0],kfold=fold)})
                 }
               }
               
               if((!is.nan(compare) & grepl("CV",compare)) | (!is.nan(compare) & compare==TRUE)){
                 # lambdaridge<-lambdaGLM$lambda.min/sd_y*n #using glmnet
                 if(grepl("glmnet",lambda)) lambdaridge <- lambdaGLM$lambda.min/sd_y*n #fitted lambda
                 #else if(grepl("penalized",lambda)) lambdaridge <- ol1$lambda
                 else lambdaridge <- fastCVfit$lambdas
               } 
               if(grepl("CV",lambda)){
                 if(grepl("glmnet",lambda)) lambda <- lambdaGLM$lambda.min/sd_y*n #using glmnet
                 #else if(grepl("penalized",lambda)) lambda <- ol1$lambda #using penalized
                 else lambda <- fastCVfit$lambdas
                 sigmahat <- sigmasq
                 muhat[,1] <- mutrgt
                 gamma[,1] <- 1/lambda
                 tauglobal<- 1/lambda
               } 
             }
             #Use ML for sigma estimation and/or initial lambda (tausq) estimate and/or mu
             if(is.nan(sigmasq) | (!is.nan(compare) & grepl("ML",compare)) | grepl("ML",lambda) | is.nan(mutrgt)){
               #Estimate sigma^2, lambda and initial estimate for tau^2 (all betas in one group), mu=0 by default
               if(grepl("ML",lambda)){ lambda <- NaN}
               Xrowsum <- apply(X,1,sum)
               
               XXt <- X[,penfctr!=0]%*%t(X[,penfctr!=0])
               
               Xunpen <- NULL #if empty vector, no unpenalised and no intercept
               if(sum(penfctr==0)>0){
                 Xunpen <- X[,penfctr==0]
               }
               par <- .mlestlin(Y=Y,XXt=XXt,Xrowsum=Xrowsum,
                                intrcpt=FALSE,Xunpen=NULL,  #Ignore unpenalised/intercept for initialisation
                                lambda=lambda,sigmasq=sigmasq,mu=mutrgt,tausq=tausq) #use maximum marginal likelihood

               lambda <- par[1] 
               sigmahat <- par[2] #sigma could be optimised with CV in the end if not known
               muhat[,1] <- par[3] 
               gamma[,1] <- par[4]
               
               tauglobal<- par[4] #set target group variance (overall variance if all covariates in one group)
               mutrgt <- par[3] #set target group mean (overall mean if all covariates in one group), 0 by default
               
               if((!is.nan(compare) & grepl("ML",compare)) | (!is.nan(compare) & compare==TRUE)) lambdaridge<- par[1]
             }
             
             #initial estimate for beta
             lambdap <- rep(lambda,p) #px1 vector with penalty for each beta_k, k=1,..,p
             lambdap[(1:p)%in%unpen] <- 0
             
             if(cont_codata){ 
               muinitp <- rep(0,p)
             }else{
               muinitp <- as.vector(c(muhat[,1])%*%Zt) #px1 vector with estimated prior mean for beta_k, k=1,..,p (0 for unpenalised covariates) 
               muinitp[(1:p)%in%unpen] <- 0
             }
             if(est_beta_method=="glmnet"){
               glmGRtrgt <- glmnet::glmnet(X,Y,alpha=0,
                                           #lambda = lambda/n*sd_y,
                                           family=fml,
                                           offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)], 
                                           intercept = intrcpt, standardize = FALSE,
                                           penalty.factor=penfctr)
               #minlam <- min(glmGRtrgt$lambda)*n/sd_y
               if(lambda < minlam){
                 warning("Estimated lambda value found too small, set to minimum to for better numerical performance")
                 lambda <- minlam
                 lambdap <- rep(lambda,p) #px1 vector with penalty for each beta_k, k=1,..,p
                 lambdap[(1:p)%in%unpen] <- 0
                 
                 Xunpen <- NULL #if empty vector, no unpenalised and no intercept
                 if(sum(penfctr==0)>0){
                   Xunpen <- X[,penfctr==0]
                 }
                 
                 #re-estimate sigma and tau_global for new lambda value
                 Xrowsum <- apply(X,1,sum)
                 XXt <- X[,penfctr!=0]%*%t(X[,penfctr!=0])
                 par <- .mlestlin(Y=Y,XXt=XXt,Xrowsum=Xrowsum,
                                  intrcpt=intrcpt,Xunpen=Xunpen, #TD: adapt for intercept and Xunpen
                                  lambda=lambda,sigmasq=NaN,mu=mutrgt,tausq=tausq) #use maximum marginal likelihood
                 sigmahat <- par[2] #sigma could be optimised with CV in the end if not known
                 gamma[,1] <- par[4]
                 tauglobal<- par[4] #set target group variance (overall variance if all covariates in one group)
               }
               #betasinit <- as.vector(glmGRtrgt$beta)
               betasinit <- coef(glmGRtrgt,s=lambda/n*sd_y,thresh = 10^-10, exact=TRUE,
                                 x=X,y=Y,
                                 family=fml,
                                 offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)], 
                                 intercept = intrcpt,
                                 penalty.factor=penfctr)[-1]
               betasinit[!((1:p)%in%unpen)] <- betasinit[!((1:p)%in%unpen)] + muinitp[!((1:p)%in%unpen)]
               #intrcptinit <- glmGRtrgt$a0
               intrcptinit <- coef(glmGRtrgt,s=lambda/n*sd_y,thresh = 10^-10, exact=TRUE,
                                   x=X,y=Y,
                                   family=fml,
                                   offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)], 
                                   intercept = intrcpt,
                                   penalty.factor=penfctr)[1]
             }else{ #use multiridge package
               XXbl <- list(X[,penfctr!=0]%*%t(X[,penfctr!=0]))
               #Compute betas
               XXT <- multiridge::SigmaFromBlocks(XXbl,penalties=lambda) #create nxn Sigma matrix = sum_b [lambda_b)^{-1} X_b %*% t(X_b)]
               if(sum((1:p)%in%unpen)>0){
                 fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
               }else{
                 fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt) #Fit. fit$etas contains the n linear predictors
               }
               
               betas <- multiridge::betasout(fit, Xblocks=list(X[,penfctr!=0]), penalties=lambda) #Find betas.
               intrcptinit <- c(betas[[1]][1]) #intercept
               betasinit <- rep(0,p) 
               betasinit[(1:p)%in%unpen] <- betas[[1]][-1] #unpenalised variables
               betasinit[!((1:p)%in%unpen)] <- betas[[2]]
               rm(betas)
             }
             
           },
           'logistic'={
             #Use Cross-validation to compute initial lambda (tausq)
             if((!is.nan(compare) & grepl("CV",compare)) | grepl("CV",lambda)){
               #use glmnet to do CV; computationally more expensive but other optimising criteria possible
               if(grepl("glmnet",lambda)){ 
                 lambdaGLM<-glmnet::cv.glmnet(X,Y,nfolds=fold,alpha=0,family=fml,
                                      standardize = FALSE,intercept=intrcpt,
                                      penalty.factor=penfctr,keep=TRUE) #alpha=0 for ridge
               }#else if(grepl("penalized",lambda)){ #use penalized to do CV
                 # Xsvd <- svd(X[,penfctr!=0])
                 # XF <- X[,penfctr!=0]%*% Xsvd$v
                 # if(intrcpt){
                 #   Xunpen <- cbind(X[,penfctr==0],rep(1,n))
                 # }else{
                 #   Xunpen <- cbind(X[,penfctr==0]) #if empty vector, no unpenalised and no intercept
                 # }
                 # 
                 # ol1 <- penalized::optL2(Y,penalized=XF, unpenalized =Xunpen,fold=fold,trace=FALSE,minlambda2=10^-6)
                 # #ol2 <- penalized::optL2(Y,penalized=X[,penfctr!=0],unpenalized=Xunpen, fold=ol1$fold ) #gives same result, but the first is much faster for large p
                 # itr2<-1
                 # while((ol1$lambda>10^12 | ol1$lambda<10^-5 ) & itr2 < 10){
                 #   ol1 <- penalized::optL2(Y,penalized=XF, unpenalized =Xunpen,fold=fold,trace=FALSE,minlambda2=10^-6)
                 #   itr2 <- itr2 + 1
                 #   # if(ol1$lambda>10^12){
                 #   #   ol1$lambda <- 10^2
                 #   #   warning("Cross-validated global penalty lambda was >10^12 and set to 100")
                 #   # }
                 # } 
                 # if(itr2==10 & (ol1$lambda>10^10 | ol1$lambda<10^-5 )){
                 #   if(ol1$lambda>10^10){
                 #     ol1$lambda <- 10^12
                 #     warning("Cross-validated global penalty lambda was >10^12 and set to 10^12")
                 #   }
                 #   if(ol1$lambda<10^-5){
                 #     ol1$lambda <- 1
                 #     warning("Cross-validated global penalty lambda was <10-5 and set to 1")
                 #   }
                 # }
                 # 
               #}
               else{ #do CV with fastCV2 from multiridge package
                 if(length(setdiff(unpen,p+1))==0){
                   Xbl <- X%*%t(X)
                   capture.output({fastCVfit <- multiridge::fastCV2(XXblocks=list(Xbl),Y=Y,intercept=intrcpt,
                                                    fixedfolds=FALSE,model=model,kfold=fold)})
                 }else{
                   Xbl <- X[,penfctr!=0]%*%t(X[,penfctr!=0])
                   capture.output({fastCVfit <- multiridge::fastCV2(XXblocks=list(Xbl),Y=Y,intercept=intrcpt,
                                                    fixedfolds=FALSE,model=model,X1=X[,penfctr==0],kfold=fold)})
                 }
               }
               
               if((!is.nan(compare) & grepl("CV",compare)) | (!is.nan(compare) & compare==TRUE)){
                 if(grepl("glmnet",lambda)) lambdaridge <- lambdaGLM$lambda.min/sd_y*n #fitted lambda
                 #else if(grepl("penalized",lambda)) lambdaridge <- ol1$lambda
                 else lambdaridge <- fastCVfit$lambdas
               } 
               if(grepl("CV",lambda)){
                 if(grepl("glmnet",lambda)) lambda <- lambdaGLM$lambda.min/sd_y*n #using glmnet
                 #else if(grepl("penalized",lambda)) lambda <- ol1$lambda #using penalized
                 else lambda <- fastCVfit$lambdas
               } 
               #print(lambda)
             }
             gamma[,1] <- 1/lambda
             tauglobal <- 1/lambda
             sigmahat <- 1 #sigma not in model for logistic: set to 1
             muhat[,1] <- mu #use initial mean 0 in logistic setting
             mutrgt <- mutrgt #default: 0
             
             #initial estimate for beta
             lambdap <- rep(lambda,p) #px1 vector with penalty for each beta_k, k=1,..,p
             lambdap[(1:p)%in%unpen] <- 0
             
             if(cont_codata){ 
               muinitp <- rep(0,p)
             }else{
               muinitp <- as.vector(c(muhat[,1])%*%Zt) #px1 vector with estimated prior mean for beta_k, k=1,..,p (0 for unpenalised covariates) 
               muinitp[(1:p)%in%unpen] <- 0
             }
             if(est_beta_method=="glmnet"){
               glmGRtrgt <- glmnet::glmnet(X,Y,alpha=0,
                                           #lambda = lambda/n*sd_y,
                                           family=fml,
                                           offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)], intercept = intrcpt, standardize = FALSE,
                                           penalty.factor=penfctr)
               #minlam <- min(glmGRtrgt$lambda)*n/sd_y
               if(lambda < minlam){
                 warning("Estimated lambda value found too small, set to minimum for better numerical performance")
                 lambda <- minlam
                 lambdap <- rep(lambda,p) #px1 vector with penalty for each beta_k, k=1,..,p
                 lambdap[(1:p)%in%unpen] <- 0
                 
                 #re-estimate tau_global for new lambda value
                 gamma[,1] <- 1/lambda
                 tauglobal <- 1/lambda
               }
               
               #betasinit <- as.vector(glmGRtrgt$beta)
               betasinit <- coef(glmGRtrgt,s=lambda/n*sd_y,thresh = 10^-10, exact=TRUE,
                                 x=X,y=Y,
                                 family=fml,
                                 offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)], intercept = intrcpt,
                                 penalty.factor=penfctr)[-1]
               betasinit[!((1:p)%in%unpen)] <- betasinit[!((1:p)%in%unpen)] + muinitp[!((1:p)%in%unpen)]
               #intrcptinit <- glmGRtrgt$a0
               intrcptinit <- coef(glmGRtrgt,s=lambda/n*sd_y,thresh = 10^-10,exact=TRUE,
                                   x=X,y=Y,
                                   family=fml,
                                   offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)], intercept = intrcpt,
                                   penalty.factor=penfctr)[1]
             }else{ #use multiridge package
               XXbl <- list(X[,penfctr!=0]%*%t(X[,penfctr!=0]))
               #Compute betas
               XXT <- multiridge::SigmaFromBlocks(XXbl,penalties=lambda) #create nxn Sigma matrix = sum_b [lambda_b)^{-1} X_b %*% t(X_b)]
               if(sum((1:p)%in%unpen)>0){
                 fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
               }else{
                 fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt) #Fit. fit$etas contains the n linear predictors
               }
               
               betas <- multiridge::betasout(fit, Xblocks=list(X[,penfctr!=0]), penalties=lambda) #Find betas.
               intrcptinit <- c(betas[[1]][1]) #intercept
               betasinit <- rep(0,p) 
               betasinit[(1:p)%in%unpen] <- betas[[1]][-1] #unpenalised variables
               betasinit[!((1:p)%in%unpen)] <- betas[[2]]
               rm(betas)
             }
             
           },
           'cox'={
             #Cross-validation lambda
             if((!is.nan(compare) & grepl("CV",compare)) | grepl("CV",lambda)){
               if(grepl("glmnet",lambda)){ 
                 lambdaGLM<-glmnet::cv.glmnet(X,as.matrix(Y),nfolds=fold,alpha=0,family=fml,
                                      standardize = FALSE,
                                      penalty.factor=penfctr,keep=TRUE) #alpha=0 for ridge
               }#else if(grepl("penalized",lambda)){ #use penalized to do CV
               #   Xsvd <- svd(X[,penfctr!=0])
               #   XF <- X[,penfctr!=0]%*% Xsvd$v
               #   Xunpen <- cbind(X[,penfctr==0]) #if empty vector, no unpenalised and no intercept
               #   
               #   ol1 <- penalized::optL2(survival::Surv(Y[,1],Y[,2]),penalized=XF, unpenalized =Xunpen,fold=fold,trace=FALSE,minlambda2=10^-6)
               #   #ol2 <- penalized::optL2(Y,penalized=X[,penfctr!=0],unpenalized=Xunpen, fold=ol1$fold ) #gives same result, but the first is much faster for large p
               #   itr2<-1
               #   while((ol1$lambda>10^10 | ol1$lambda<10^-5 ) & itr2 < 10){
               #     ol1 <- penalized::optL2(survival::Surv(Y[,1],Y[,2]),penalized=XF, unpenalized =Xunpen,fold=fold,trace=FALSE,minlambda2=10^-6)
               #     itr2 <- itr2 + 1
               #     # if(ol1$lambda>10^12){
               #     #   ol1$lambda <- 10^2
               #     #   warning("Cross-validated global penalty lambda was >10^12 and set to 100")
               #     # }
               #   } 
               #   if(itr2==10 & (ol1$lambda>10^10 | ol1$lambda<10^-5 )){
               #     if(ol1$lambda>10^10){
               #       ol1$lambda <- 10^12
               #       warning("Cross-validated global penalty lambda was >10^12 and set to 10^12")
               #     }
               #     if(ol1$lambda<10^-5){
               #       ol1$lambda <- 1
               #       warning("Cross-validated global penalty lambda was <10-5 and set to 1")
               #     }
               #   }
               # }
               else{ #do CV with fastCV2 from multiridge package
                 if(length(setdiff(unpen,p+1))==0){
                   Xbl <- X%*%t(X)
                   capture.output({fastCVfit <- multiridge::fastCV2(XXblocks=list(Xbl),Y=Y,
                                        fixedfolds=FALSE,model=model,kfold=fold)})
                 }else{
                   Xbl <- X[,penfctr!=0]%*%t(X[,penfctr!=0])
                   capture.output({fastCVfit <- multiridge::fastCV2(XXblocks=list(Xbl),Y=Y,
                                        fixedfolds=FALSE,model=model,X1=X[,penfctr==0],kfold=fold)})
                 }
               }
               
               if((!is.nan(compare) & grepl("CV",compare))| (!is.nan(compare) & compare==TRUE)){
                 if(grepl("glmnet",lambda)) lambdaridge <- lambdaGLM$lambda.min/sd_y*n/2 #fitted lambda
                 #else if(grepl("penalized",lambda)) lambdaridge <- ol1$lambda
                 else lambdaridge <- fastCVfit$lambdas
               } 
               if(grepl("CV",lambda)){
                 if(grepl("glmnet",lambda)) lambda <- lambdaGLM$lambda.min/sd_y*n/2 #fitted lambda
                 #else if(grepl("penalized",lambda)) lambda <- ol1$lambda
                 else lambda <- fastCVfit$lambdas
               } 
             }
             sigmahat <- 1 #sigma not in model for cox: set to 1
             muhat[,1] <- 0 #use initial mean 0 in cox setting
             gamma[,1] <- 1/lambda
             mutrgt <- 0
             tauglobal <- 1/lambda
             
             #initial estimate for beta
             lambdap <- rep(lambda,p) #px1 vector with penalty for each beta_k, k=1,..,p
             lambdap[(1:p)%in%unpen] <- 0
             if(cont_codata){ 
               muinitp <- rep(0,p)
             }else{
               muinitp <- as.vector(c(muhat[,1])%*%Zt) #px1 vector with estimated prior mean for beta_k, k=1,..,p (0 for unpenalised covariates) 
               muinitp[(1:p)%in%unpen] <- 0
             }
             if(est_beta_method=="glmnet"){
               glmGRtrgt <- glmnet::glmnet(X,Y,alpha=0,
                                           #lambda = 2*lambda/n*sd_y,
                                           family=fml,
                                           offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)], standardize = FALSE,
                                           penalty.factor=penfctr)
               #minlam <- min(glmGRtrgt$lambda)*n/sd_y/2
               if(lambda < minlam){
                 warning("Estimated lambda value found too small, set to minimum to for better numerical performance")
                 lambda <- minlam
                 lambdap <- rep(lambda,p) #px1 vector with penalty for each beta_k, k=1,..,p
                 lambdap[(1:p)%in%unpen] <- 0
                 
                 #re-estimate tau_global for new lambda value
                 gamma[,1] <- 1/lambda
                 tauglobal <- 1/lambda
               }
               
               intrcptinit <- NULL #NULL for Cox
               #betasinit <- as.vector(glmGRtrgt$beta)
               betasinit <- coef(glmGRtrgt,s=lambda/n*sd_y,thresh = 10^-10,exact=TRUE,
                                 x=X,y=Y,
                                 family=fml,
                                 offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)],
                                 penalty.factor=penfctr)
               betasinit[!((1:p)%in%unpen)] <- betasinit[!((1:p)%in%unpen)] + muinitp[!((1:p)%in%unpen)]
               #intrcptinit <- glmGRtrgt$a0
             }else{
               XXbl <- list(X[,penfctr!=0]%*%t(X[,penfctr!=0]))
               #Compute betas
               XXT <- multiridge::SigmaFromBlocks(XXbl,penalties=lambda) #create nxn Sigma matrix = sum_b [lambda_b)^{-1} X_b %*% t(X_b)]
               if(sum((1:p)%in%unpen)>0){
                 fit <- multiridge::IWLSCoxridge(XXT,Y=Y, model=model,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
               }else{
                 fit <- multiridge::IWLSCoxridge(XXT,Y=Y) #Fit. fit$etas contains the n linear predictors
               }
               
               betas <- multiridge::betasout(fit, Xblocks=list(X[,penfctr!=0]), penalties=lambda) #Find betas.
               intrcptinit <- c(betas[[1]][1]) #intercept
               betasinit <- rep(0,p) 
               betasinit[(1:p)%in%unpen] <- betas[[1]][-1] #unpenalised variables
               betasinit[!((1:p)%in%unpen)] <- betas[[2]]
               rm(betas)
             }
           },
           'family'={
             #Use Cross-validation to compute initial lambda (tausq)
             if((!is.nan(compare) & grepl("CV",compare)) | grepl("CV",lambda)){
               #use glmnet to do CV; computationally more expensive but other optimising criteria possible
               if(grepl("glmnet",lambda)){ 
                 lambdaGLM<-glmnet::cv.glmnet(X,Y,nfolds=fold,alpha=0,family=fml,
                                              standardize = FALSE,intercept=intrcpt,
                                              penalty.factor=penfctr,keep=TRUE) #alpha=0 for ridge
               }
               else{ #do CV with fastCV2 from multiridge package
                 if(length(setdiff(unpen,p+1))==0){
                   Xbl <- X%*%t(X)
                   capture.output({fastCVfit <- multiridge::fastCV2(XXblocks=list(Xbl),Y=Y,intercept=intrcpt,
                                                                    fixedfolds=FALSE,model=model,kfold=fold)})
                 }else{
                   Xbl <- X[,penfctr!=0]%*%t(X[,penfctr!=0])
                   capture.output({fastCVfit <- multiridge::fastCV2(XXblocks=list(Xbl),Y=Y,intercept=intrcpt,
                                                                    fixedfolds=FALSE,model=model,X1=X[,penfctr==0],kfold=fold)})
                 }
               }
               
               if((!is.nan(compare) & grepl("CV",compare)) | (!is.nan(compare) & compare==TRUE)){
                 if(grepl("glmnet",lambda)) lambdaridge <- lambdaGLM$lambda.min/sd_y*n #fitted lambda
                 #else if(grepl("penalized",lambda)) lambdaridge <- ol1$lambda
                 else lambdaridge <- fastCVfit$lambdas
               } 
               if(grepl("CV",lambda)){
                 if(grepl("glmnet",lambda)) lambda <- lambdaGLM$lambda.min/sd_y*n #using glmnet
                 #else if(grepl("penalized",lambda)) lambda <- ol1$lambda #using penalized
                 else lambda <- fastCVfit$lambdas
               } 
               #print(lambda)
             }
             gamma[,1] <- 1/lambda
             tauglobal <- 1/lambda
             sigmahat <- 1 #sigma not in model for logistic: set to 1
             muhat[,1] <- mu #use initial mean 0 in logistic setting
             mutrgt <- mutrgt #default: 0
             
             #initial estimate for beta
             lambdap <- rep(lambda,p) #px1 vector with penalty for each beta_k, k=1,..,p
             lambdap[(1:p)%in%unpen] <- 0
             
             if(cont_codata){ 
               muinitp <- rep(0,p)
             }else{
               muinitp <- as.vector(c(muhat[,1])%*%Zt) #px1 vector with estimated prior mean for beta_k, k=1,..,p (0 for unpenalised covariates) 
               muinitp[(1:p)%in%unpen] <- 0
             }
             if(est_beta_method=="glmnet"){
               glmGRtrgt <- glmnet::glmnet(X,Y,alpha=0,
                                           #lambda = lambda/n*sd_y,
                                           family=fml,
                                           offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)], intercept = intrcpt, standardize = FALSE,
                                           penalty.factor=penfctr)
               #minlam <- min(glmGRtrgt$lambda)*n/sd_y
               if(lambda < minlam){
                 warning("Estimated lambda value found too small, set to minimum for better numerical performance")
                 lambda <- minlam
                 lambdap <- rep(lambda,p) #px1 vector with penalty for each beta_k, k=1,..,p
                 lambdap[(1:p)%in%unpen] <- 0
                 
                 #re-estimate tau_global for new lambda value
                 gamma[,1] <- 1/lambda
                 tauglobal <- 1/lambda
               }
               
               #betasinit <- as.vector(glmGRtrgt$beta)
               betasinit <- coef(glmGRtrgt,s=lambda/n*sd_y,thresh = 10^-10, exact=TRUE,
                                 x=X,y=Y,
                                 family=fml,
                                 offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)], intercept = intrcpt,
                                 penalty.factor=penfctr)[-1]
               betasinit[!((1:p)%in%unpen)] <- betasinit[!((1:p)%in%unpen)] + muinitp[!((1:p)%in%unpen)]
               #intrcptinit <- glmGRtrgt$a0
               intrcptinit <- coef(glmGRtrgt,s=lambda/n*sd_y,thresh = 10^-10,exact=TRUE,
                                   x=X,y=Y,
                                   family=fml,
                                   offset = X[,!((1:p)%in%unpen)] %*% muinitp[!((1:p)%in%unpen)], intercept = intrcpt,
                                   penalty.factor=penfctr)[1]
             }#multiridge package not yet possible for general glm families
             # }else{ #use multiridge package
             #   XXbl <- list(X[,penfctr!=0]%*%t(X[,penfctr!=0]))
             #   #Compute betas
             #   XXT <- multiridge::SigmaFromBlocks(XXbl,penalties=lambda) #create nxn Sigma matrix = sum_b [lambda_b)^{-1} X_b %*% t(X_b)]
             #   if(sum((1:p)%in%unpen)>0){
             #     fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
             #   }else{
             #     fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt) #Fit. fit$etas contains the n linear predictors
             #   }
             #   
             #   betas <- multiridge::betasout(fit, Xblocks=list(X[,penfctr!=0]), penalties=lambda) #Find betas.
             #   intrcptinit <- c(betas[[1]][1]) #intercept
             #   betasinit <- rep(0,p) 
             #   betasinit[(1:p)%in%unpen] <- betas[[1]][-1] #unpenalised variables
             #   betasinit[!((1:p)%in%unpen)] <- betas[[2]]
             #   rm(betas)
             # }
             
           }
    )
  }
  
  
  #-3.3 Start iterations (usually just one iteration) ========================================================================================
  Itr<-1
  while(Itr<=nIt){
    #-3.3.1 Compute penalty matrix and weight matrix for logistic #############################################
    #copy penalty parameter matrix ridge: add 0 for unpenalised intercept if included
    if(intrcpt | intrcptGLM){
      #Deltac <- diag(c(lambdap,0))
      Deltac <- Matrix::sparseMatrix(i=1:(length(lambdap)+1),j=1:(length(lambdap)+1),x=c(lambdap,0))
      if(model=="logistic"){
        #Deltac<-2*Deltac
        #reweight Xc for logistic model
        expminXb<-exp(-Xcinit%*%c(betasinit,intrcptinit))
        Pinit<-1/(1+expminXb)
        W<-diag(c(sqrt(Pinit*(1-Pinit))))
        Xc<-W%*%Xcinit
      }else if(model=="family"){
        lp <- Xcinit%*%c(betasinit,intrcptinit) #linear predictor
        meansY <- fml$linkinv(lp) #mu=E(Y)
        W <- diag(c(sqrt(fml$variance(meansY))))
        Xc<-W%*%Xcinit
      }
    }else{
      #Deltac <- diag(c(lambdap))
      Deltac <- Matrix::sparseMatrix(i=1:length(lambdap),j=1:length(lambdap),x=c(lambdap))
      if(model=="logistic"){
        #Deltac<-2*Deltac
        #reweight Xc for logistic model
        expminXb<-exp(-Xcinit%*%c(betasinit))
        Pinit<-1/(1+expminXb)
        W<-diag(c(sqrt(Pinit*(1-Pinit))))
        Xc<-W%*%Xcinit
      }else if(model=="cox"){
        #Deltac<-2*Deltac
        #reweight Xc for cox model
        expXb<-exp(Xcinit%*%c(betasinit))
        h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXb[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
        H0 <- sapply(Y[,1],function(Ti){sum(h0[Y[,1]<=Ti])})
        
        W <- diag(c(sqrt(H0*expXb)))
        Xc<-W%*%Xcinit
      }else if(model=="family"){
        lp <- Xcinit%*%c(betasinit) #linear predictor
        meansY <- fml$linkinv(lp) #mu=E(Y)
        W <- diag(c(sqrt(fml$variance(meansY)))) #square root of variance matrix
        Xc<-W%*%Xcinit
      }
    }
    if(model%in%c("logistic","cox","family") && all(W==0)){
      #browser()
      if(!silent) print("Overfitting: only 0 in weight matrix W")
      if(!silent) print(paste("Iterating stopped after",Itr-1,"iterations",sep=" "))
      break;
    }
    
    #NOTE: in glmnet not yet unpenalised covariates other than intercept
    
    #-3.3.2 Compute matrices needed for MoM ###################################################################
    # XtXD <- t(Xc)%*%Xc+Deltac
    # XtXDinv <- solve(XtXD)
    # L<-XtXDinv %*% t(Xc)
    pen <- (1:dim(Xc)[2])[!(1:dim(Xc)[2]%in%unpen)] #index covariates to be penalised
    if(p>n){
      if(length(unpen)>0){
        P1<-diag(1,n) - Xc[,unpen]%*%solve(t(Xc[,unpen])%*%Xc[,unpen],t(Xc[,unpen])) #compute orthogonal projection matrix
        eigP1<-eigen(P1,symmetric = TRUE)
        if(!all(round(eigP1$values,digits=0)%in%c(0,1))){
          warning("Check unpenalised covariates")
        }
        eigP1$values<-pmax(0,eigP1$values) #set eigenvalues that are small negative due to numerical errors to 0
        CP1 <- eigP1$vectors %*% diag(sqrt(eigP1$values))
        Xpen <- as.matrix(t(CP1)%*%Xc[,pen]%*%Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),x=Matrix::diag(Deltac)[pen]^(-0.5)))
      } else{
        pen <- 1:p
        CP1<- diag(1,n)
        Xpen <- as.matrix(Xc[,pen]%*%Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),x=Matrix::diag(Deltac)[pen]^(-0.5)))
      }
      svdX<-svd(Xpen) #X=UDV^T=RV^T
      svdXR<-svdX$u%*%diag(svdX$d) #R=UD
      L2 <- as.matrix(Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),
                 x=Matrix::diag(Deltac)[pen]^(-0.5))%*%svdX$v%*%solve(t(svdXR)%*%
                                                    svdXR+diag(1,n),t(svdXR)%*%t(CP1)))
      L<-array(0,c(p+intrcpt,n))
      L[pen,]<-L2 #compute only elements corresponding to penalised covariates
      
      R<-Xc
      V2<-sigmahat*apply(L2,1,function(x){sum(x^2)})
      #V2<-apply(L2,1,function(x){sum(x^2)})
      V<-rep(NaN,p+intrcpt)
      V[pen]<-V2 #variance beta ridge estimator
      zeroV <- which(V==0)
      
      # #should be same as:
      # XtXD <- t(Xc)%*%Xc+Deltac
      # XtXDinv <- solve(XtXD) #inverting pxp matrix really slow, use SVD instead
      # L1<-XtXDinv %*% t(Xc)
      # L1 <- solve(XtXD,t(Xc))
      # R<-Xc
      # V1<-sigmahat*apply(L,1,function(x){sum(x^2)})
      # #same as: V <- sigmahat*diag(L%*%R %*% XtXDinv) 
    }else{ #n>p
      XtXD <- Matrix::as.matrix(t(Xc)%*%Xc+Deltac)
      XtXDinv <- solve(XtXD) #inverting pxp matrix
      L<-XtXDinv %*% t(Xc)
      R<-Xc
      #C<-L%*%R
      V<-sigmahat*apply(L,1,function(x){sum(x^2)})
      zeroV <- which(V==0)
      #same as: V3 <- sigmahat*diag(L%*%R %*% XtXDinv)
    }
 
    #-3.3.3 Update group parameters ###########################################################################
    if(nIt>1){
      if(!silent) print(paste("Compute group penalty estimates, iteration ",Itr,"out of maximal ",nIt," iterations."))
    }
    ### Function Method of Moments to compute group weights for (possibly) multiple parameters 
    MoM <- function(Partitions,hypershrinkage=NaN,groupsets.grouplvl=NaN,
                    fixWeightsMu=NaN,fixWeightsTau=NaN,pars){
      #Partitions: vector with index of partitions
      #fixWeightsMu,fixWeightsTau: when fixed group weights for different partitions/co-data are given, 
      #                            the MoM-function will calculate partition/co-data weights (without extra shrinkage)
      #extract parts of global variables for local copy
      if(length(Partitions)<m | m==1){
        if(cont_codata){
          Zt <- Zt[unlist(indGrpsGlobal[Partitions]),,drop=FALSE]
        }else{
          Zt <- Zt[unlist(indGrpsGlobal[Partitions]),,drop=FALSE]
        }
        
        if(missing(pars)){
          ind0 <- which(unlist(indGrpsGlobal[Partitions])%in%ind0)
          indnot0 <- which(unlist(indGrpsGlobal[Partitions])%in%indnot0)
        }else{
          indnot0 <- pars[[1]]
          ind0 <- pars[[2]]
        }
        if(length(dim(Wminhalf))>1){
          Wminhalf <- Wminhalf[unlist(indGrpsGlobal[Partitions]),unlist(indGrpsGlobal[Partitions]),drop=FALSE]
          # initgamma <- as.vector(ind[unlist(indGrpsGlobal[Partitions]),]%*%betasinit^2 / 
          #                           apply(ind[unlist(indGrpsGlobal[Partitions]),],1,sum))
          initgamma <- rep(1,length(indnot0))
        }else initgamma <- 1#tauglobal  
        G <- G[Partitions] #number of groups in these partitions
        PenGrps <- PenGrps[unlist(indGrpsGlobal[Partitions]),unlist(indGrpsGlobal[Partitions]),drop=FALSE]
        eqfun <- function(gamma,b,A,lam)  return(sum(t(Zt[indnot0,,drop=FALSE])%*%gamma)/length(pen) ) #equality constraint for average prior variance
      }
      #keep local copies of variables to return
      muhat <- muhat[unlist(indGrpsGlobal[Partitions]),Itr]
      gammatilde <- rep(0,sum(G))
      gamma <- gamma[unlist(indGrpsGlobal[Partitions]),Itr]
      lambdashat<-lambdashat[,Itr+1,Partitions] 
      
      #if two shrinkage penalties are given, first is used to select groups, second to shrink estimates
      if(!is.nan(hypershrinkage)){
        temp <- strsplit(hypershrinkage,",") 
        hypershrinkage <- temp[[1]][1]  
        ExtraShrinkage2 <- temp[[1]][-1]
        if(!cont_codata){
          if(grepl("none",hypershrinkage)){
            if(length(Partitions)==1){
              if(!silent) print(paste("Group set ",Partitions,": estimate group weights, hypershrinkage type: ",hypershrinkage,sep=""))
            }
          }else{
            if(!silent) print(paste("Group set ",Partitions,": estimate hyperlambda for ",hypershrinkage," hypershrinkage",sep=""))
          }
        }else{
          if(grepl("none",hypershrinkage)){
            if(length(Partitions)==1){
              if(!silent) print(paste("Co-data matrix ",Partitions,
                                      ": estimate weights, hypershrinkage type: ",
                                      hypershrinkage,sep=""))
            }
          }else if(hypershrinkage=="mgcv"){
            if(!silent) print(paste("Estimate co-data weights and (if included) hyperpenalties with mgcv",sep=""))
          }else{
            if(length(Partitions)==1){
              if(!silent) print(paste("Co-data matrix ",Partitions,": estimate hyperlambda for ",hypershrinkage," hypershrinkage",sep=""))
            }
          }
        }
        
      }
      if(!cont_codata){ #Set up MoM equations in fast GxG linear system in case of group sets
        if(length(G)==1 && G==1 && !cont_codata){
          lambdashat <- c(0,0)
          muhat <- mutrgt
          weightsMu <- NaN
          if(is.nan(tausq)){
            gamma <- 1
            gammatilde <- gamma
          }
        }else if(!cont_codata & !grepl("none",hypershrinkage) & !all(G==1) & length(indnot0)>1){ 
          #-3.3.3|1 With extra shrinkage -----------------------------------------------------------------
          # Use splits to penalise for too many groups
          # Minimise RSS over lambda1, lambda2 to find optimal penalties for shrinkage on group level
          # Splits group randomly in half for nsplits times, INDin: one half of the split
          INDin <- lapply(1:m,function(prt){ 
            if(!(prt%in%Partitions)){return(NULL)}else{
              replicate(nsplits,lapply(groupsets[[prt]],function(x){sample(x,floor(length(x)/2),replace=FALSE)}),simplify=FALSE)  
            }
          }) #keep list of m elements such that index same as groupsets
          INDout <- lapply(1:m,function(i){ #for each partition
            if(!(i%in%Partitions)){return(NULL)}else{
              lapply(INDin[[i]],function(indin){ #for each split
                lapply(1:length(groupsets[[i]]),function(x){groupsets[[i]][[x]][!(groupsets[[i]][[x]]%in%indin[[x]])]})})
            }
          })
          #INDin[[Partitions]] <- lapply(groupsets[Partitions],function(prt){
          #  replicate(nsplits,lapply(prt,function(x){sample(x,floor(length(x)/2),replace=FALSE)}),simplify=FALSE)
          #})
          #INDout <- lapply(Partitions,function(i){ #for each partition
          #  lapply(INDin[[i]],function(indin){ #for each split
          #    lapply(1:G[i],function(x){groupsets[[i]][[x]][!(groupsets[[i]][[x]]%in%indin[[x]])]})})
          #})
          
          #-3.3.3|1.1 EB estimate group means ============================================================
          muhatp <-as.vector(rep(mu,sum(G))%*%Zt) #px1 vector with estimated prior mean for beta_k, k=1,..,p
          muhatp[(1:p)%in%unpen] <- 0
          
          weightsMu <- rep(NaN,sum(G))
          if(is.nan(mu)){
            if(is.nan(lambdashat[1])){
              #-3.3.3|1.1.1 Compute linear system for whole partition ####################################
              A.mu <- matrix(unlist(
                lapply(Partitions,function(i){ #for each partition
                  sapply(1:length(Kg[[i]]),function(j){ #for each group
                    #compute row with gamma_{xy}
                    x<-groupsets[[i]][[j]]
                    unlist(sapply(Partitions,function(prt){sapply(groupsets[[prt]],function(y){sum(L[x,]%*%t(t(R[,y])/Ik[[prt]][y]))/Kg[[i]][j]})}))
                  }, simplify="array")
                })
              ),c(sum(G),sum(G)),byrow=TRUE) #reshape to matrix of size sum(G)xsum(G)
              Bmu <- unlist(
                lapply(Partitions,function(i){ #for each partition
                  sapply(1:length(Kg[[i]]),function(j){ #for each group
                    x<-groupsets[[i]][[j]]
                    sum(betasinit[x]-muinitp[x]+L[x,]%*%(R[,pen]%*%muinitp[pen]))/Kg[[i]][j]
                  })
                })
              )
              
              sdA.mu <- c(apply(A.mu,2,function(x){sd(x,na.rm=TRUE)}))
              A.mu<-A.mu%*% diag(1/sdA.mu) #normalise columns
              
              #-3.3.3|1.1.2 For each split, compute linear system ########################################
              mutrgtG <- mutrgt
              if(length(mutrgt)==1){ mutrgtG <- rep(mutrgt,sum(G))}
              mutrgtG<-diag(sdA.mu)%*%mutrgtG
              
              #in-part
              A.muin <- lapply(1:nsplits,function(split){
                matrix(unlist(
                  lapply(Partitions,function(i){ #for each partition
                    sapply(1:length(Kg[[i]]),function(j){ #for each group
                      #compute row with gamma_{xy}
                      x<-INDin[[i]][[split]][[j]]
                      #compute row with gamma_{xy}
                      unlist(sapply(Partitions,function(prt){sapply(groupsets[[prt]],function(y){sum(L[x,]%*%t(t(R[,y])/Ik[[prt]][y]))/Kg[[i]][j]})}))
                    }, simplify="array")
                  })
                ),c(sum(G),sum(G)),byrow=TRUE) %*% diag(1/sdA.mu) #reshape to matrix of size sum(G)xsum(G)
              })
              #rhs vector
              Bmuin <- lapply(1:nsplits,function(split){unlist(
                lapply(Partitions,function(i){ #for each partition
                  sapply(1:length(Kg[[i]]),function(j){ #for each group
                    x<-INDin[[i]][[split]][[j]]
                    sum(betasinit[x]-muinitp[x]+L[x,]%*%(R[,pen]%*%muinitp[pen]))/Kg[[i]][j]
                  })
                })
              )
              })
              
              #weight matrix
              A.muinAcc <- lapply(1:nsplits,function(i){
                A.muinAcc <- A.muin[[i]] %*% Wminhalf #weight matrix 
              })
              
              #out-part: use A.mu_{out}=A.mu-A.mu_{in}, B_{out}=B-B_{in}
              A.muout <- lapply(1:nsplits,function(split){
                A.mu-A.muin[[split]]
              })
              Bmuout <- lapply(1:nsplits,function(split){
                Bmu-Bmuin[[split]]
              })
              
              
              #-3.3.3|1.1.3 Define function RSSlambdamu, ################################################
              # using the extra shrinkage penalty function corresponding to parameter hypershrinkage
              rangelambda1 <- c(-100,100)
              switch(hypershrinkage,
                     "ridge"={
                       #standard deviation needed for glmnet
                       sd_Bmuin<- lapply(1:nsplits,function(i){
                         if(length(ind0)>0){
                           sd_Bmuin <- sqrt(var(Bmuin[[i]][indnot0]- 
                                                  as.matrix(A.muin[[i]][indnot0,ind0],c(length(c(indnot0,ind0))))%*%muhat[ind0] -
                                                  A.muin[[i]][indnot0,indnot0] %*% mutrgtG[indnot0])*(length(indnot0)-1)/length(indnot0))[1]
                         }else{
                           sd_Bmuin <- sqrt(var(Bmuin[[i]][indnot0]- 
                                                  A.muin[[i]][indnot0,indnot0] %*% mutrgtG[indnot0])*(length(indnot0)-1)/length(indnot0))[1]
                         }
                       })
                       RSSlambdamu <- function(lambda1){
                         #Ridge estimates for given lambda
                         lambda1<-exp(lambda1)
                         
                         ### Estimate group means in-part for given lambda1
                         muhatin <- lapply(1:nsplits,function(i){
                           #ridge estimate for group means
                           muhatin <- rep(NaN,sum(G))
                           muhatin[ind0]<-muhat[ind0] #groups with variance 0 keep same prior parameters
                           if(length(ind0)>0){
                             glmMuin <- glmnet::glmnet(A.muinAcc[[i]][indnot0,indnot0],Bmuin[[i]][indnot0]- 
                                                         as.matrix(A.muinAcc[[i]][indnot0,ind0],c(length(indnot0),length(ind0)))%*%muhat[ind0],
                                                       alpha=0,
                                                       #lambda = 2*lambda1/length(indnot0)*sd_Bmuin[[i]],
                                                       family="gaussian",
                                                       offset = A.muin[[i]][indnot0,indnot0] %*% mutrgtG[indnot0], intercept = FALSE, standardize = FALSE)
                           }else{
                             glmMuin <- glmnet::glmnet(A.muinAcc[[i]][indnot0,indnot0],Bmuin[[i]][indnot0],
                                                       alpha=0,
                                                       #lambda = 2*lambda1/length(indnot0)*sd_Bmuin[[i]],
                                                       family="gaussian",
                                                       offset = A.muin[[i]][indnot0,indnot0] %*% mutrgtG[indnot0], intercept = FALSE, standardize = FALSE)
                           }
                           #muhatin[indnot0] <- Wminhalf[indnot0,indnot0] %*% as.vector(glmMuin$beta) + mutrgtG[indnot0]
                           muhatin[indnot0] <- Wminhalf[indnot0,indnot0] %*% 
                             coef(glmMuin,s=2*lambda1/length(indnot0)*sd_Bmuin[[i]])[-1] + mutrgtG[indnot0]
                           return(muhatin)
                         }
                         ) #group estimate for mu_in
                         
                         ### Compute RSS on left-out part
                         A.muoutmuin <- lapply(1:nsplits,function(split){A.muout[[split]][indnot0,]%*%muhatin[[split]]})
                         RSSmu <- sum(sapply(1:nsplits,function(split){sum((A.muoutmuin[[split]]-Bmuout[[split]][indnot0])^2)/nsplits}))
                         return(RSSmu)
                       }
                     },
                     "lasso"={
                       ### Fit glmnet for global range of lambda
                       fitMu <- lapply(1:nsplits,function(i){
                         if(length(ind0)>0){
                           glmMuin <- glmnet::glmnet(A.muinAcc[[i]][indnot0,indnot0],Bmuin[[i]][indnot0]- 
                                                       as.matrix(A.muinAcc[[i]][indnot0,ind0],c(length(indnot0),length(ind0)))%*%muhat[ind0],
                                                     alpha=1,family="gaussian",
                                                     offset = A.muin[[i]][indnot0,indnot0] %*% mutrgtG[indnot0], intercept = FALSE, standardize = FALSE,
                                                     thresh = 1e-10)
                         }else{
                           glmMuin <- glmnet::glmnet(A.muinAcc[[i]][indnot0,indnot0],Bmuin[[i]][indnot0],
                                                     alpha=1,family="gaussian",
                                                     offset = A.muin[[i]][indnot0,indnot0] %*% mutrgtG[indnot0], intercept = FALSE, standardize = FALSE,
                                                     thresh = 1e-10)
                         }
                       })
                       
                       RSSlambdamu <- function(lambda1){
                         #Ridge estimates for given lambda
                         lambda1<-exp(lambda1)
                         
                         ### Estimate group means in-part for given lambda1
                         muhatin <- lapply(1:nsplits,function(i){
                           #ridge estimate for group means
                           muhatin <- rep(NaN,sum(G))
                           muhatin[ind0]<-muhat[ind0] #groups with variance 0 keep same prior parameters
                           coefMu<- coef(fitMu[[i]], s = lambda1, exact = FALSE)[-1,]
                           muhatin[indnot0] <- Wminhalf[indnot0,indnot0] %*% as.vector(coefMu) + mutrgtG[indnot0]
                           return(muhatin)
                         }
                         ) #group estimate for mu_in
                         
                         ### Compute RSS on left-out part
                         A.muoutmuin <- lapply(1:nsplits,function(split){A.muout[[split]][indnot0,]%*%muhatin[[split]]})
                         RSSmu <- sum(sapply(1:nsplits,function(split){sum((A.muoutmuin[[split]]-Bmuout[[split]][indnot0])^2)/nsplits}))
                         return(RSSmu)
                       }
                     },
                     "hierLasso"={
                       #TD: acc or not?
                       #Hierarchical overlapping group estimates for given lambda
                       #no target for mu (shrunk to 0)
                       #A.muxtnd <- lapply(A.muinAcc,function(X){return(X[,unlist(groupsets.grouplvl)])}) #extend matrix such to create artifical non-overlapping groups
                       A.muxtnd <- lapply(A.muin,function(X){return(X[,unlist(groupsets.grouplvl)])}) #extend matrix such to create artifical non-overlapping groups
                       #create new group indices for Axtnd
                       Kg2 <- c(1,sapply(groupsets.grouplvl,length)) #group sizes on group level (1 added to easily compute hier. group numbers)
                       G2 <- length(Kg2)-1
                       groupxtnd <- lapply(2:length(Kg2),function(i){sum(Kg2[1:(i-1)]):(sum(Kg2[1:i])-1)}) #list of indices in each group
                       groupxtnd2 <- unlist(sapply(1:G2,function(x){rep(x,Kg2[x+1])})) #vector with group number
                       
                       ### Fit gglasso for global range of lambda
                       fit1<-lapply(1:nsplits,function(i){
                         gglasso::gglasso(x=A.muxtnd[[i]],y=Bmuin[[i]],group = groupxtnd2, loss="ls", 
                                          intercept = FALSE, pf = rep(1,G2))
                       })
                       rangelambda1 <- log(range(sapply(fit1,function(i){range(i$lambda)})))
                       
                       RSSlambdamu <- function(lambda1){
                         lambda1<-exp(lambda1)
                         
                         ### Estimate prior gammas for given lambda2 (and mutrgt=0)
                         muhatin <- lapply(1:nsplits,function(i){
                           vtilde <- coef(fit1[[i]],s=lambda1)[-1]
                           v<-lapply(groupxtnd,function(g){
                             x<-rep(0,G)
                             x[unlist(groupsets.grouplvl)[g]]<-x[unlist(groupsets.grouplvl)[g]]+vtilde[g]
                             return(x)
                           })
                           muhatin <- Wminhalf %*% c(apply(array(unlist(v),c(G,G2)),1,sum))
                           return(muhatin)
                         })
                         
                         ### Compute MSE on left-out part
                         A.muoutmuin <- lapply(1:nsplits,function(split){A.muout[[split]]%*%muhatin[[split]]})
                         RSSmu <- sum(sapply(1:nsplits,function(split){sum((A.muoutmuin[[split]]-Bmuout[[split]])^2)/nsplits}))
                         return(RSSmu)
                         
                         # lamb<-seq(exp(rangelambda1[1]),exp(rangelambda1[2]),diff(exp(rangelambda1))/200)
                         # RSS<-sapply(log(lamb),RSSlambdamu)
                         # plot(lamb,RSS)
                       }
                     }
              )
              
              #First find optimal lambda_1
              lambda1<- optimise(RSSlambdamu,rangelambda1)
              lambdashat[1] <- exp(lambda1$minimum)
            }
            
            #-3.3.3|1.1.4 Compute group mean estimates for optimised hyperpenalty lambda #################
            if(lambdashat[1]==0){
              #groups with zero group variance already in muhat
              if(length(ind0)>0){ #only update groups with positive group variance
                muhat[indnot0] <- solve(A.mu[indnot0,indnot0],Bmu[indnot0]-
                                          as.matrix(A.mu[indnot0,ind0],c(length(indnot0),length(ind0)))%*%muhat[ind0])
              }else{
                muhat[indnot0] <- solve(A.mu[indnot0,indnot0],Bmu[indnot0])
              }
              muhat[indnot0] <- diag(1/sdA.mu[indnot0]) %*% muhat[indnot0] #restore sd columns mu
            }else{
              #-3.3.3|1.1.5 Compute mu for given hyperpenalty  ###########################################
              switch(hypershrinkage,
                     "ridge"={
                       A.muAcc <- A.mu %*% Wminhalf
                       if(length(ind0)>0){
                         sd_Bmu <- sqrt(var(Bmu[indnot0] - as.matrix(A.mu[indnot0,ind0],c(length(indnot0),length(ind0)))%*%muhat[ind0]
                                            -as.matrix(A.mu[indnot0,indnot0],c(length(indnot0),length(ind0))) %*% mutrgtG[indnot0])*(length(indnot0)-1)/length(indnot0))[1]
                         #ridge estimate for group means
                         glmMu <- glmnet::glmnet(A.muAcc[indnot0,indnot0],Bmu[indnot0]-
                                                   as.matrix(A.muAcc[indnot0,ind0],c(length(indnot0),length(ind0)))%*%muhat[ind0],alpha=0,
                                                 #lambda = 2*lambdashat[1]/length(indnot0)*sd_Bmu,
                                                 family="gaussian",
                                                 offset = A.mu[indnot0,indnot0] %*% mutrgtG[indnot0], intercept = FALSE, standardize = FALSE)
                       }else{
                         sd_Bmu <- sqrt(var(Bmu[indnot0]
                                            -as.matrix(A.mu[indnot0,indnot0],c(length(indnot0),length(ind0))) %*% mutrgtG[indnot0])*(length(indnot0)-1)/length(indnot0))[1]
                         #ridge estimate for group means
                         glmMu <- glmnet::glmnet(A.muAcc[indnot0,indnot0],Bmu[indnot0],alpha=0,
                                                 #lambda = 2*lambdashat[1]/length(indnot0)*sd_Bmu,
                                                 family="gaussian",
                                                 offset = A.mu[indnot0,indnot0] %*% mutrgtG[indnot0], intercept = FALSE, standardize = FALSE)
                       }
                       #groups with variance 0 keep same prior parameters, update other groups
                       #muhat[indnot0] <- Wminhalf[indnot0,indnot0] %*% as.vector(glmMu$beta) + mutrgtG[indnot0]
                       muhat[indnot0] <- Wminhalf[indnot0,indnot0] %*% 
                         coef(glmMu, s=2*lambdashat[1]/length(indnot0)*sd_Bmu)[-1] + mutrgtG[indnot0]
                       muhat[indnot0] <- diag(1/sdA.mu[indnot0]) %*% muhat[indnot0] #restore sd columns A.mu
                       
                     },
                     "lasso"={
                       A.muAcc <- A.mu %*% Wminhalf
                       #ridge estimate for group means
                       if(length(ind0)>0){
                         glmMu <- glmnet::glmnet(A.muAcc[indnot0,indnot0],Bmu[indnot0]-
                                                   as.matrix(A.muAcc[indnot0,ind0],c(length(indnot0),length(ind0)))%*%muhat[ind0],
                                                 alpha=1,family="gaussian",
                                                 offset = A.mu[indnot0,indnot0] %*% mutrgtG[indnot0], intercept = FALSE, standardize = FALSE)
                       }else{
                         glmMu <- glmnet::glmnet(A.muAcc[indnot0,indnot0],Bmu[indnot0],
                                                 alpha=1,family="gaussian",
                                                 offset = A.mu[indnot0,indnot0] %*% mutrgtG[indnot0], intercept = FALSE, standardize = FALSE)
                       }
                       coefMu <- coef(glmMu,s=lambdashat[1])
                       #groups with variance 0 keep same prior parameters, update other groups
                       muhat[indnot0] <- Wminhalf[indnot0,indnot0] %*% as.vector(coefMu[-1,]) + mutrgtG[indnot0]
                       muhat[indnot0] <- diag(1/sdA.mu[indnot0]) %*% muhat[indnot0] #restore sd columns A.mu
                     },
                     "hierLasso"={
                       #Hierarchical overlapping group estimates for given lambda
                       #no target for mu (shrunk to 0)
                       #A.muAcc <- A.mu %*% Wminhalf
                       #A.muxtnd <- A.muAcc[,unlist(groupsets.grouplvl)] #extend matrix such to create artifical non-overlapping groups
                       A.muxtnd <- A.mu[,unlist(groupsets.grouplvl)] #extend matrix such to create artifical non-overlapping groups
                       #create new group indices for Axtnd
                       Kg2 <- c(1,sapply(groupsets.grouplvl,length)) #group sizes on group level (1 added to easily compute hier. group numbers)
                       G2 <- length(Kg2)-1
                       groupxtnd <- lapply(2:length(Kg2),function(i){sum(Kg2[1:(i-1)]):(sum(Kg2[1:i])-1)}) #list of indices in each group
                       groupxtnd2 <- unlist(sapply(1:G2,function(x){rep(x,Kg2[x+1])})) #vector with group number
                       
                       #Hierarchical group lasso estimate for group variances
                       fit1<-gglasso::gglasso(x=A.muxtnd,y=Bmu,group = groupxtnd2, loss="ls", 
                                              intercept = FALSE, pf = rep(1,G2),lambda=lambdashat[1])
                       vtilde <- coef(fit1,s=lambdashat[1])[-1]
                       v<-lapply(groupxtnd,function(g){
                         x<-rep(0,G)
                         x[unlist(groupsets.grouplvl)[g]]<-x[unlist(groupsets.grouplvl)[g]]+vtilde[g]
                         return(x)
                       })
                       muhat <- Wminhalf %*% c(apply(array(unlist(v),c(G,G2)),1,sum))
                       muhat <- diag(1/sdA.mu) %*% muhat #restore sd columns A
                     })
            }
            
            weightsMu <- muhat*p/sum(as.vector(c(muhat)%*%Zt))
            muhatp <-as.vector(c(muhat)%*%Zt) #px1 vector with estimated prior mean for beta_k, k=1,..,p
            muhatp[(1:p)%in%unpen] <- 0
            # if(normalise){ #TRUE by default
            #   C<-mutrgt*p/sum(muhatp)
            #   muhat[,Itr+1]<-muhat[,Itr+1]*C
            #   muhatp <-as.vector(c(muhat[,Itr+1])%*%Zt) #px1 vector with estimated prior mean for beta_k, k=1,..,p
            # }
            
            #should be same as:
            #muhat2 <- solve(t(A.mu)%*%A.mu+lambdashat[1]*diag(weights),t(A.mu)%*%Bmu+lambdashat[1]*diag(weights)%*%rep(mutrgt,G))
            #muhat2 <- solve(t(A.mu)%*%diag(c(Kg))%*%A.mu+lambdashat[1]*diag(1,G),t(A.mu)%*%diag(c(Kg))%*%Bmu+lambdashat[1]*diag(1,G)%*%rep(mutrgt,G))
          }
          
          #-3.3.3|1.2 EB estimate group variances =========================================================
          gamma <- rep(1,sum(G))
          if(is.nan(tausq)){
            if(is.nan(lambdashat[2])){
              #-3.3.3|1.2.1 Compute linear system for whole partition #####################################
              Btau <- unlist(
                lapply(Partitions,function(i){ #for each partition
                  sapply(1:length(Kg[[i]]),function(j){ #for each group
                    if(j%in%ind0) return(NaN)
                    #compute row with gamma_{xy}
                    x<-groupsets[[i]][[j]]
                    x<-setdiff(x,zeroV) #ad-hoc fix: remove covariates with 0 variance (will be set to 0 anyways)
                    #sum(pmax(0,(betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,pen]%*%
                    #      (muhatp[pen]-muinitp[pen])))^2)/V[x]-1),na.rm=TRUE)/Kg[[i]][j]
                    sum((betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,pen]%*%
                                                               (muhatp[pen]-muinitp[pen])))^2)/V[x]-1,na.rm=TRUE)/Kg[[i]][j]
                  })
                })
              )
              A <- matrix(unlist(
                lapply(Partitions,function(i){ #for each partition
                  sapply(1:length(Kg[[i]]),function(j){ #for each group
                    if(j%in%ind0) return(rep(NaN,sum(G)))
                    #compute row with gamma_{xy}
                    x<-groupsets[[i]][[j]]
                    x<-setdiff(x,zeroV) #ad-hoc fix: remove covariates with 0 variance (will be set to 0 anyways)
                    #compute row with gamma_{xy}
                    unlist(sapply(Partitions,function(prt){sapply(groupsets[[prt]],function(y){
                      y<-setdiff(y,zeroV) #ad-hoc fix: remove covariates with 0 variance (will be set to 0 anyways)
                      sum(t(c(1/V[x])*L[x,])%*%L[x,]*(R[,y]%*%(t(R[,y])/c(Ik[[prt]][y])*c(tauglobal[datablockNo[y]]))),na.rm=TRUE)/Kg[[i]][j]
                    })}))
                  }, simplify="array")
                })
              ),c(sum(G),sum(G)),byrow=TRUE) #reshape to matrix of size sum(G)xsum(G)
              
              constA <- 1 #mean(diag(A),na.rm=TRUE)
              Btau <- Btau/constA
              A <- A/constA
              
              #if(Itr==2) browser()
              #-3.3.3|1.2.2 For each split, compute linear system #########################################
              gammatrgtG <- rep(1,sum(G))
              gammatrgtG[ind0]<-0 
              
              #in-part
              flag <- TRUE; itr2 <- 1
              indNewSplits <- 1:nsplits; 
              Btauin <- list(); Btauout <- list()
              while(flag & itr2 <= 50){
                Btauin[indNewSplits] <- lapply(indNewSplits,function(split){unlist(
                  lapply(Partitions,function(i){ #for each partition
                    sapply(1:length(Kg[[i]]),function(j){ #for each group
                      if(j%in%ind0) return(NaN)
                      #compute row with gamma_{xy}
                      x<-INDin[[i]][[split]][[j]]
                      x<-setdiff(x,zeroV) #ad-hoc fix: remove covariates with 0 variance (will be set to 0 anyways)
                      #sum(pmax(0,(betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,pen]%*%(muhatp[pen]-muinitp[pen])))^2)/V[x]-1),na.rm=TRUE)/Kg[[i]][j]
                      sum((betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,pen]%*%(muhatp[pen]-muinitp[pen])))^2)/V[x]-1,na.rm=TRUE)/Kg[[i]][j]
                    })
                  })
                )/constA
                })
                
                #check split: at least two elements of Btauin (of selected groups) should be larger than 0
                checkSplit <- sapply(Btauin,function(b){sum(b[!is.nan(b)]!=0)>=2 }) 
                if(all(checkSplit)){ #all splits are fine
                  flag <- FALSE
                }else{ 
                  itr2 <- itr2 + 1
                  indNewSplits <- which(!checkSplit) #index of splits that have to be resampled
                  #resample split
                  INDin[[Partitions]][indNewSplits] <- replicate(length(indNewSplits),lapply(groupsets[[Partitions]],
                                                                                             function(x){sample(x,floor(length(x)/2),replace=FALSE)}),simplify=FALSE)  
                  INDout[[Partitions]][indNewSplits] <- lapply(INDin[[Partitions]][indNewSplits],function(indin){ #for each split
                    lapply(1:length(groupsets[[Partitions]]),
                           function(x){groupsets[[Partitions]][[x]][!(groupsets[[Partitions]][[x]]%in%indin[[x]])]})})
                }
              }
              if(itr2==51) warning("Check splits")
              
              Ain <- lapply(1:nsplits,function(split){
                matrix(unlist(
                  lapply(Partitions,function(i){ #for each partition
                    sapply(1:length(Kg[[i]]),function(j){ #for each group
                      if(j%in%ind0) return(rep(NaN,sum(G)))
                      #compute row with gamma_{xy}
                      x<-INDin[[i]][[split]][[j]]
                      x<-setdiff(x,zeroV) #ad-hoc fix: remove covariates with 0 variance (will be set to 0 anyways)
                      #compute row with gamma_{xy}
                      unlist(sapply(Partitions,function(prt){sapply(groupsets[[prt]],function(y){
                        y<-setdiff(y,zeroV) #ad-hoc fix: remove covariates with 0 variance (will be set to 0 anyways)
                        sum(t(c(1/V[x])*L[x,])%*%L[x,]*(R[,y]%*%(t(R[,y])/c(Ik[[prt]][y])*c(tauglobal[datablockNo[y]]))),na.rm=TRUE)/Kg[[i]][j]
                      })}))
                    }, simplify="array")
                  })
                ),c(sum(G),sum(G)),byrow=TRUE)/constA #reshape to matrix of size sum(G)xsum(G)
              })
              #weight matrix
              AinAcc <- lapply(1:nsplits,function(i){
                AinAcc <- Ain[[i]] %*% Wminhalf #weight matrix 
              })
              
              Btauout <- lapply(1:nsplits,function(split){unlist(
                lapply(Partitions,function(i){ #for each partition
                  sapply(1:length(Kg[[i]]),function(j){ #for each group
                    if(j%in%ind0) return(NaN)
                    #compute row with gamma_{xy}
                    x<-INDout[[i]][[split]][[j]]
                    x<-setdiff(x,zeroV) #ad-hoc fix: remove covariates with 0 variance (will be set to 0 anyways)
                    #sum(pmax(0,(betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,pen]%*%(muhatp[pen]-muinitp[pen])))^2)/V[x]-1),na.rm=TRUE)/Kg[[i]][j]
                    sum((betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,pen]%*%(muhatp[pen]-muinitp[pen])))^2)/V[x]-1,na.rm=TRUE)/Kg[[i]][j]
                  })
                })
              )/constA
              })
              # Btauout <- lapply(1:nsplits,function(split){
              #   Btau - Btauin[[split]]
              # })
              Aout <- lapply(1:nsplits,function(split){
                A - Ain[[split]]
              })
              
              #-3.3.3|1.2.3 Define function RSSlambdatau, #################################################
              # using the extra shrinkage penalty function corresponding to parameter hypershrinkage
              rangelambda2 <- c(10^-5,10^6)
              switch(hypershrinkage,
                     "ridge"={
                       gammatrgtG[indnot0] <- 1
                       meanWhalf <- mean(diag(Wminhalf)^-1)
                       #standard deviation needed for glmnet
                       sd_Btauin<- lapply(1:nsplits,function(i){
                         sd_Btauin <- sqrt(var(Btauin[[i]][indnot0] - Ain[[i]][indnot0,indnot0] %*% gammatrgtG[indnot0])*(length(indnot0)-1)/length(indnot0))[1]
                       })
                       
                       #function to compute tau for linear system given a hyperpenalty lambda2
                       gammas <- function(lambda2){
                         sd_Btau <- sqrt(var(Btau[indnot0] - A[indnot0,indnot0] %*% gammatrgtG[indnot0])*(length(indnot0)-1)/length(indnot0))[1]
                         
                         gammas <- rep(0,G)
                         Aacc <- A %*% (Wminhalf * meanWhalf)
                         #ridge estimate for group variances
                         glmTau <- glmnet::glmnet(Aacc[indnot0,indnot0],Btau[indnot0],alpha=0,
                                                  family="gaussian",
                                                  offset = Aacc[indnot0,indnot0] %*% gammatrgtG[indnot0], intercept = FALSE, standardize = FALSE)
                         coefTau <- coef(glmTau,s=lambda2,exact=TRUE,
                                         x=Aacc[indnot0,indnot0],y=Btau[indnot0],
                                         offset = Aacc[indnot0,indnot0] %*% gammatrgtG[indnot0])[-1,]
                         gammas[indnot0] <- (Wminhalf[indnot0,indnot0]*meanWhalf) %*% as.vector(coefTau) + gammatrgtG[indnot0] 
                         #gammas <- pmax(gammas,0) #truncate at 0
                         return(gammas)
                       }
                       
                       ### Fit glmnet lasso for global range of lambda
                       fitTau <- lapply(1:nsplits,function(i){
                         glmTauin <- glmnet::glmnet(AinAcc[[i]][indnot0,indnot0],Btauin[[i]][indnot0],
                                                    alpha=0,family="gaussian",
                                                    offset = Ain[[i]][indnot0,indnot0] %*% gammatrgtG[indnot0],
                                                    intercept = FALSE, standardize = FALSE,
                                                    thresh=1e-6)
                       })
                       rangelambda2 <- range(sapply(fitTau,function(x)range(x$lambda)))
                       rangelambda2[1] <- rangelambda2[1]/100
                       
                       #function to compute the Residual Sum of Squares on the splits given lambda2
                       RSSlambdatau <- function(lambda2){
                         lambda2 <- exp(lambda2)
                         #Ridge estimates for given lambda
                         ### Estimate prior gammas for given lambda2 and mutrgt
                         gammain <- lapply(1:nsplits,function(i){
                           gammain <- rep(NaN,sum(G))
                           gammain[ind0] <- 0
                           #ridge estimate for group variances
                           # glmTauin <- glmnet::glmnet(AinAcc[[i]][indnot0,indnot0],Btauin[[i]][indnot0],alpha=0,
                           #                    lambda = 2*lambda2/length(indnot0)*sd_Btauin[[i]],family="gaussian",
                           #                    offset = Ain[[i]][indnot0,indnot0] %*% gammatrgtG[indnot0], intercept = FALSE, standardize = FALSE)
                           #gammain[indnot0] <- Wminhalf[indnot0,indnot0] %*% as.vector(glmTauin$beta) + gammatrgtG[indnot0] 
                           coefTau <- coef(fitTau[[i]],s=lambda2,exact=TRUE,
                                           x=AinAcc[[i]][indnot0,indnot0],y=Btauin[[i]][indnot0],
                                           offset = Ain[[i]][indnot0,indnot0] %*% gammatrgtG[indnot0])[-1,]
                           gammain[indnot0] <- Wminhalf[indnot0,indnot0] %*% as.vector(coefTau) + gammatrgtG[indnot0] 
                           #gammain <- pmax(gammain,0)
                           return(gammain)
                         })
                         
                         ### Compute MSE on left-out part
                         Aouttauin <- lapply(1:nsplits,function(split){Aout[[split]][indnot0,]%*%gammain[[split]]})
                         RSStau <- sum(sapply(1:nsplits,function(split){sum((Aouttauin[[split]]-Btauout[[split]][indnot0])^2)/nsplits}))
                         return(RSStau)
                       }
                     },
                     "ridgeGAM"={
                       gammatrgtG[indnot0] <- 1
                       
                       #function to compute tau for linear system given a hyperpenalty lambda2
                       gammas <- function(lambda2){
                         gammas <- rep(0,G)
                         
                         #ridge estimate for group variances with fused penalty for overlapping groups
                         dat<-list(Y=Btau[indnot0],X=A[indnot0,indnot0])
                         gamTau <- mgcv::gam(Y~ 0 + X, data=dat,
                                             family="gaussian",offset = A[indnot0,indnot0] %*% gammatrgtG[indnot0],
                                             paraPen=list(X=list(S1=PenGrps,sp=lambda2)))
                         gammas[indnot0] <- gamTau$coefficients + gammatrgtG[indnot0]
                         
                         return(gammas)
                       }
                       
                       #function to compute the Residual Sum of Squares on the splits given lambda2
                       RSSlambdatau <- function(lambda2){
                         lambda2 <- exp(lambda2)
                         
                         ### Estimate prior gammas for given lambda2 and mutrgt
                         gammain <- lapply(1:nsplits,function(i){
                           gammain <- rep(NaN,sum(G))
                           gammain[ind0] <- 0
                           #ridge estimate for group variances
                           dat<-list(Y=Btauin[[i]][indnot0],X=Ain[[i]][indnot0,indnot0])
                           gamTauin <- mgcv::gam(Y~ 0 + X, data=dat,
                                                 family="gaussian",offset = Ain[[i]][indnot0,indnot0] %*% gammatrgtG[indnot0],
                                                 paraPen=list(X=list(S1=PenGrps,sp=lambda2)))
                           gammain[indnot0] <- gamTauin$coefficients + gammatrgtG[indnot0]
                           
                           return(gammain)
                         })
                         
                         ### Compute MSE on left-out part
                         Aouttauin <- lapply(1:nsplits,function(split){Aout[[split]][indnot0,]%*%gammain[[split]]})
                         RSStau <- sum(sapply(1:nsplits,function(split){sum((Aouttauin[[split]]-Btauout[[split]][indnot0])^2)/nsplits}))
                         return(RSStau)
                       }
                     },
                     "lasso"={#TD: adapt lasso&hierLasso, check other hyperpenalties on inclusion function gammas
                       meanWhalf <- mean(diag(Wminhalf)^-1)
                       ### Fit glmnet lasso for global range of lambda
                       fitTau <- lapply(1:nsplits,function(i){
                         glmTauin <- glmnet::glmnet(AinAcc[[i]][indnot0,indnot0]*meanWhalf,Btauin[[i]][indnot0],
                                                    alpha=1,family="gaussian",
                                                    intercept = FALSE, standardize = FALSE,
                                                    thresh=1e-6)
                       })
                       rangelambda2 <- range(sapply(fitTau,function(x)range(x$lambda)))
                       
                       #function to compute tau for linear system given a hyperpenalty lambda2
                       gammas <- function(lambda2){
                         gammas <- rep(0,G)
                         Aacc <- A %*% (Wminhalf * meanWhalf)
                         glmTau <- glmnet::glmnet(Aacc[indnot0,indnot0],Btau[indnot0],
                                                  alpha=1,family="gaussian",
                                                  intercept = FALSE, standardize = FALSE)
                         coefTau <- coef(glmTau,s=lambda2,exact=TRUE,
                                         x=Aacc[indnot0,indnot0],y=Btau[indnot0])
                         gammas[indnot0] <- (Wminhalf[indnot0,indnot0]*meanWhalf) %*% as.vector(coefTau[-1,])
                         return(gammas)
                       }
                       
                       #function to compute the Residual Sum of Squares on the splits given lambda2
                       RSSlambdatau <- function(lambda2){
                         lambda2 <- exp(lambda2)
                         #Ridge estimates for given lambda
                         ### Estimate prior gammas for given lambda2 and mutrgt
                         gammain <- lapply(1:nsplits,function(i){
                           gammain <- rep(NaN,sum(G))
                           gammain[ind0] <- 0
                           #ridge estimate for group variances
                           coefTau<- coef(fitTau[[i]], s = lambda2, exact = TRUE,
                                          x=AinAcc[[i]][indnot0,indnot0]*meanWhalf,y=Btauin[[i]][indnot0])[-1,]
                           gammain[indnot0] <- (Wminhalf[indnot0,indnot0]*meanWhalf) %*% as.vector(coefTau)
                           return(gammain)
                         })
                         
                         ### Compute MSE on left-out part
                         Aouttauin <- lapply(1:nsplits,function(split){Aout[[split]][indnot0,]%*%gammain[[split]]})
                         RSStau <- sum(sapply(1:nsplits,function(split){sum((Aouttauin[[split]]-Btauout[[split]][indnot0])^2)/nsplits}))
                         return(RSStau)
                       }
                     },
                     "hierLasso"={
                       maxit_gglasso <- 1e+04
                       #Hierarchical overlapping group estimates for given lambda
                       #no target for tau (shrunk to 0)
                       #remove groups that are already set to 0
                       if(length(groupsets.grouplvl)!=length(indnot0)){
                         INDgrps2 <- lapply(groupsets.grouplvl[indnot0],function(x){x[x%in%indnot0]})
                       }else{
                         INDgrps2 <- groupsets.grouplvl
                       }
                       
                       #Axtnd <- lapply(AinAcc,function(A){return(A[indnot0,unlist(INDgrps2),drop=FALSE])}) #extend matrix such to create artifical non-overlapping groups
                       Axtnd <- lapply(Ain,function(A){return(A[indnot0,unlist(INDgrps2),drop=FALSE])}) #extend matrix such to create artifical non-overlapping groups
                       #create new group indices for Axtnd
                       Kg2 <- c(1,sapply(INDgrps2,length)) #group sizes on group level (1 added to easily compute hier. group numbers)
                       G2 <- length(Kg2)-1
                       groupxtnd <- lapply(2:length(Kg2),function(i){sum(Kg2[1:(i-1)]):(sum(Kg2[1:i])-1)}) #list of indices in each group
                       groupxtnd2 <- unlist(sapply(1:G2,function(x){rep(x,Kg2[x+1])})) #vector with group number
                       
                       ### Fit gglasso for global range of lambda
                       fit2<-lapply(1:nsplits,function(i){
                         capture.output({temp <- gglasso::gglasso(x=Axtnd[[i]],y=Btauin[[i]][indnot0],
                                                  group = groupxtnd2, loss="ls",
                                                  intercept = FALSE, pf = rep(1,G2),maxit = 1e+04)})
                         # temp <- gglasso::gglasso(x=Axtnd[[i]],y=Btauin[[i]][indnot0],
                         #                                          group = groupxtnd2, loss="ls",
                         #                                          intercept = FALSE, pf = rep(1,G2),maxit = maxit_gglasso)
                         return(temp)
                       })
                       rangelambda2 <- range(sapply(fit2,function(i){range(i$lambda)}), na.rm=TRUE)
                       
                       #Find grid to search optimal lambda over
                       gammas <- function(lambda2){
                         gammas <- rep(0,G)
                         Axtnd <- A[indnot0,unlist(INDgrps2),drop=FALSE] #extend matrix such to create artifical non-overlapping groups
                         
                         #create new group indices for Axtnd
                         Kg2 <- c(1,sapply(INDgrps2,length)) #group sizes on group level (1 added to easily compute hier. group numbers)
                         G2 <- length(Kg2)-1
                         groupxtnd <- lapply(2:length(Kg2),function(i){sum(Kg2[1:(i-1)]):(sum(Kg2[1:i])-1)}) #list of indices in each group
                         groupxtnd2 <- unlist(sapply(1:G2,function(x){rep(x,Kg2[x+1])})) #vector with group number
                         
                         #Hierarchical group lasso estimate for group variances
                         fit2<-gglasso::gglasso(x=Axtnd,y=Btau[indnot0],group = groupxtnd2, loss="ls",
                                                intercept = FALSE, pf = rep(1,G2),maxit = maxit_gglasso)
                         gamma <- rep(0,sum(G))
                         vtilde <- try(coef(fit2,s=lambda2)[-1],silent=TRUE)
                         if(class(vtilde)[1]=="try-error") return(gamma) #return 0 vector
                         v<-lapply(groupxtnd,function(g){
                           x<-rep(0,sum(G))
                           x[unlist(INDgrps2)[g]]<-x[unlist(INDgrps2)[g]]+vtilde[g]
                           return(x)
                         })
                         #gammatilde[indnot0] <- Wminhalf[indnot0,indnot0] %*% c(apply(array(unlist(v),c(sum(G),G2)),1,sum))[indnot0]
                         gammas[indnot0] <- c(apply(array(unlist(v),c(sum(G),G2)),1,sum))[indnot0]
                         return(gammas)
                       }
                       
                       #function to compute the Residual Sum of Squares on the splits given lambda2
                       RSSlambdatau <- function(lambda2){
                         lambda2 <- exp(lambda2)
                         ### Estimate prior gammas for given lambda2 (and mutrgt=0)
                         gammain <- lapply(1:nsplits,function(i){
                           gammain <- rep(0,sum(G))
                           vtilde <- try(coef(fit2[[i]],s=lambda2)[-1],silent=TRUE)
                           if(class(vtilde)[1]=="try-error") return(gammain) #return 0 vector
                           v<-lapply(groupxtnd,function(g){
                             x<-rep(0,sum(G))
                             x[unlist(INDgrps2)[g]]<-x[unlist(INDgrps2)[g]]+vtilde[g]
                             return(x)
                           })
                           #gammain[indnot0] <- Wminhalf[indnot0,indnot0] %*% c(apply(array(unlist(v),c(sum(G),G2)),1,sum))[indnot0]
                           gammain[indnot0] <- c(apply(array(unlist(v),c(sum(G),G2)),1,sum))[indnot0]
                           gammain[gammain<0] <- 0
                           return(gammain)
                         })
                         
                         ### Compute MSE on left-out part
                         Aouttauin <- lapply(1:nsplits,function(split){Aout[[split]][indnot0,indnot0]%*%gammain[[split]][indnot0]})
                         RSStau <- sum(sapply(1:nsplits,function(split){sum((Aouttauin[[split]]-Btauout[[split]][indnot0])^2)/nsplits}))
                         return(RSStau)
                       }
                     },
                     "ridge+positive"={
                       meanWhalf <- mean(diag(Wminhalf)^-1)
                       trgt <- 1/diag(Wminhalf)/meanWhalf
                       initgamma <- diag(Wminhalf)^(-1)/meanWhalf
                       
                       #define function for MSE penalised with ridge prior with target 1
                       penMSE <- function(gamma,b,A,lam){ 
                         return(sum((b-A%*%gamma)^2) + lam*sum((gamma-trgt[indnot0])^2)) }
                       
                       #function to compute tau for linear system given a hyperpenalty lambda2
                       gammas <- function(lambda2){
                         gammas <- rep(0,G)
                         Aacc <- A %*% (Wminhalf * meanWhalf)
                         initSelected <- initgamma[indnot0]
                         fitTau <- Rsolnp::solnp(par = initSelected, fun=penMSE, b=Btau[indnot0],
                                                 A=Aacc[indnot0,indnot0], lam=lambda2,
                                                 LB = rep(0,length(indnot0)),control=list(trace=0))
                         gammas[indnot0] <- (Wminhalf[indnot0,indnot0] *meanWhalf) %*%as.vector(fitTau$pars)
                         return(gammas)
                       }
                       
                       #function to compute the Residual Sum of Squares on the splits given lambda2
                       RSSlambdatau <- function(lambda2){
                         lambda2 <- exp(lambda2)
                         #Ridge estimates for given lambda
                         ### Estimate prior gammas for given lambda2 and mutrgt
                         gammain <- lapply(1:nsplits,function(i){
                           gammain <- rep(NaN,sum(G))
                           gammain[ind0] <- 0
                           
                           Ainacc <- Ain[[i]]%*%(Wminhalf * meanWhalf)
                           fitTauin <- Rsolnp::solnp(par = initgamma[indnot0], fun=penMSE, b=Btauin[[i]][indnot0],
                                                     A=Ainacc[indnot0,indnot0],lam=lambda2,
                                                     LB = rep(0,length(indnot0)),control=list(trace=0))
                           gammain[indnot0] <- (Wminhalf[indnot0,indnot0] * meanWhalf) %*% as.vector(fitTauin$pars)
                           return(gammain)
                         })
                         ### Compute MSE on left-out part
                         Aouttauin <- lapply(1:nsplits,function(split){Aout[[split]][indnot0,]%*%gammain[[split]]})
                         RSStau <- sum(sapply(1:nsplits,function(split){sum((Aouttauin[[split]]-Btauout[[split]][indnot0])^2)/nsplits}))
                         return(RSStau)
                       }
                     },
                     "ridgeGAM+positive"={
                       trgt <- 1
                       
                       #define function for MSE penalised with ridge prior with target 1
                       penMSE <- function(gamma,b,A,lam){ 
                         return(sum((b-A%*%gamma)^2) + 
                                  lam*sum((gamma-trgt)%*%PenGrps%*%(gamma-trgt))) }
                       
                       gammas <- function(lambda2){
                         #gammas <- rep(0,sum(G))
                         initSelected <- initgamma[indnot0]
                         fitTau <- Rsolnp::solnp(par = initSelected, fun=penMSE, b=Btau[indnot0], 
                                                 A=A[indnot0,indnot0], lam=lambda2,
                                                 LB = rep(0,length(indnot0)),control=list(trace=0))
                         gammain[indnot0] <- as.vector(fitTau$pars)
                         return(gammain)
                       }
                       
                       RSSlambdatau <- function(lambda2){
                         lambda2 <- exp(lambda2)
                         #Ridge estimates for given lambda
                         ### Estimate prior gammas for given lambda2 and mutrgt
                         gammain <- lapply(1:nsplits,function(i){
                           gammain <- rep(0,sum(G))
                           
                           initSelected <- initgamma[indnot0]
                           fitTauin <- Rsolnp::solnp(par = initSelected, fun=penMSE, b=Btauin[[i]][indnot0], 
                                                     A=Ain[[i]][indnot0,indnot0], lam=lambda2,
                                                     LB = rep(0,length(indnot0)),control=list(trace=0))
                           gammain[indnot0] <- as.vector(fitTauin$pars)
                           return(gammain)
                         })
                         
                         ### Compute MSE on left-out part
                         Aouttauin <- lapply(1:nsplits,function(split){Aout[[split]][indnot0,]%*%gammain[[split]]})
                         RSStau <- sum(sapply(1:nsplits,function(split){sum((Aouttauin[[split]]-Btauout[[split]][indnot0])^2)/nsplits}))
                         return(RSStau)
                       }
                     },
                     "invgamma+mean1"={
                       meanWhalf <- mean(diag(Wminhalf)^-1)
                       
                       #define function for MSE penalised with inverse gamma prior with mean 1
                       initgamma <- diag(Wminhalf)^(-1)/meanWhalf
                       #MSE penalised by inverse gamma penalty
                       penMSE <- function(gamma,b,A,lam){ 
                         Kg <- diag(Wminhalf)^(-2) #group sizes
                         alphaIG <- pmax(1,2 + (lam-1/min(Kg))*Kg) #alpha in range [1,infty)
                         betaIG <- pmax(0,sqrt(Kg)* (1+(lam-1/min(Kg))* Kg) / meanWhalf) #beta in range [0,infty)
                         
                         minlogLikeInvGamma <- (alphaIG[indnot0] + 1)*log(gamma) + betaIG[indnot0]/gamma
                         return(sum((b-A%*%gamma)^2) + sum(minlogLikeInvGamma) ) }
                       
                       #function to compute tau for linear system given a hyperpenalty lambda2
                       gammas <- function(lambda2){
                         gammas <- rep(0,G)
                         Aacc <- A %*% (Wminhalf * meanWhalf)
                         initSelected <- initgamma[indnot0]
                         fitTau <- Rsolnp::solnp(par = initSelected, fun=penMSE, b=Btau[indnot0],
                                                 A=Aacc[indnot0,indnot0], lam=lambda2,
                                                 LB = rep(0,length(indnot0)),control=list(trace=0))
                         gammas[indnot0] <- (Wminhalf[indnot0,indnot0] *meanWhalf) %*%as.vector(fitTau$pars)
                         return(gammas)
                       }
                       
                       #function to compute the Residual Sum of Squares on the splits given lambda2
                       RSSlambdatau <- function(lambda2){
                         lambda2 <- exp(lambda2)
                         ### Estimate prior gammas for given lambda2
                         gammain <- lapply(1:nsplits,function(i){
                           Ainacc <- Ain[[i]]%*%(Wminhalf * meanWhalf)
                           gammain <- rep(NaN,sum(G))
                           gammain[ind0] <- 0
                           initSelected <- initgamma[indnot0]
                           fitTauin <- Rsolnp::solnp(par = initSelected, fun=penMSE, b=Btauin[[i]][indnot0],
                                                     A=Ainacc[indnot0,indnot0],lam=lambda2,
                                                     LB = rep(0,length(indnot0)),control=list(trace=0))
                           gammain[indnot0] <- (Wminhalf[indnot0,indnot0] *meanWhalf)%*%as.vector(fitTauin$pars)
                           return(gammain)
                         })
                         
                         ### Compute MSE on left-out part
                         Aouttauin <- lapply(1:nsplits,function(split){Aout[[split]][indnot0,]%*%gammain[[split]]})
                         RSStau <- sum(sapply(1:nsplits,function(split){sum((Aouttauin[[split]]-Btauout[[split]][indnot0])^2)/nsplits}))
                         return(RSStau)
                       }
                     },
                     "invgamma+mode1"={
                       meanWhalf <- mean(diag(Wminhalf)^-1)
                       
                       #define function for MSE penalised with inverse gamma prior with mean 1
                       initgamma <- diag(Wminhalf)^(-1)/meanWhalf
                       
                       #MSE penalised by inverse gamma penalty
                       penMSE <- function(gamma,b,A,lam){ 
                         Kg <- diag(Wminhalf)^(-2) #group sizes
                         prmsIG<-.prmsIGMode1(lam,Kg)
                         alphaIG <- prmsIG[[1]]
                         betaIG<-prmsIG[[2]] * diag(Wminhalf)^(-1)/meanWhalf
                         
                         minlogLikeInvGamma <- (alphaIG[indnot0] + 1)*log(gamma) + betaIG[indnot0]/gamma
                         return(sum((b-A%*%gamma)^2) + sum(minlogLikeInvGamma) ) }
                       
                       #function to compute tau for linear system given a hyperpenalty lambda2
                       gammas <- function(lambda2){
                         gammas <- rep(0,G)
                         Aacc <- A %*% (Wminhalf * meanWhalf)
                         initSelected <- initgamma[indnot0]
                         fitTau <- Rsolnp::solnp(par = initSelected, fun=penMSE, b=Btau[indnot0],
                                                 A=Aacc[indnot0,indnot0], lam=lambda2,
                                                 LB = rep(0,length(indnot0)),control=list(trace=0))
                         gammas[indnot0] <- (Wminhalf[indnot0,indnot0] *meanWhalf) %*%as.vector(fitTau$pars)
                         return(gammas)
                       }
                       
                       #function to compute the Residual Sum of Squares on the splits given lambda2
                       RSSlambdatau <- function(lambda2){
                         lambda2 <- exp(lambda2)
                         ### Estimate prior gammas for given lambda2
                         gammain <- lapply(1:nsplits,function(i){
                           Ainacc <- Ain[[i]]%*%(Wminhalf * meanWhalf)
                           gammain <- rep(NaN,sum(G))
                           gammain[ind0] <- 0
                           initSelected <- initgamma[indnot0]
                           fitTauin <- Rsolnp::solnp(par = initSelected, fun=penMSE, b=Btauin[[i]][indnot0],
                                                     A=Ainacc[indnot0,indnot0],lam=lambda2,
                                                     LB = rep(0,length(indnot0)),control=list(trace=0))
                           gammain[indnot0] <- (Wminhalf[indnot0,indnot0] *meanWhalf)%*%as.vector(fitTauin$pars)
                           return(gammain)
                         })
                         
                         ### Compute MSE on left-out part
                         Aouttauin <- lapply(1:nsplits,function(split){Aout[[split]][indnot0,]%*%gammain[[split]]})
                         RSStau <- sum(sapply(1:nsplits,function(split){sum((Aouttauin[[split]]-Btauout[[split]][indnot0])^2)/nsplits}))
                         return(RSStau)
                       }
                     },
                     "gamma+positive"={
                       #define function for MSE penalised with gamma prior with mean 1
                       spike<-0.001
                       penMSE <- function(gamma,b,A,lam){ 
                         #logLikeInvGamma <- sapply(gamma,function(x){logdinvgamma(x,alp=lam,bet=lam-1)})
                         logLikeGamma <- sapply(gamma,function(x){
                           #return(lam*log(lam)-log(gamma(lam))+(lam-1)*log(x)-lam*x)
                           if(x==0) return(log(spike))
                           return(log(1-spike)+(lam-1)*log(x)-lam*x)
                         })
                         return(sum((b-A%*%gamma)^2) - sum(logLikeGamma) ) }
                       #eqfun2 <- function(gamma,b,A,lam)  return(sum(t(Zt[indnot0,])%*%(Wminhalf[indnot0,indnot0]%*%gamma))/length(pen) ) #equality constraint for average prior variance
                       
                       RSSlambdatau <- function(lambda2){
                         lambda2 <- exp(lambda2)
                         #if(lambda2<100) lambda2<-log(exp(lambda2)+1)
                         
                         browser()
                         ### Estimate prior gammas for given lambda2
                         #compute gamma for first split
                         i<-1
                         #Ainacc <- Ain[[i]]%*%Wminhalf
                         gammain1 <- rep(NaN,sum(G))
                         gammain1[ind0] <- 0
                         fitTauin1 <- Rsolnp::solnp(par = initgamma, fun=penMSE, b=Btauin[[i]][indnot0], 
                                                    A=Ain[[i]][indnot0,indnot0],lam=lambda2,
                                                    LB = rep(0,G), eqfun=eqfun, eqB = 1,control=list(trace=0))
                         #gammain1[indnot0] <- Wminhalf%*%as.vector(fitTauin1$pars)
                         gammain1[indnot0] <- as.vector(fitTauin1$pars)
                         
                         gammain <- lapply(2:nsplits,function(i){
                           #Ainacc <- Ain[[i]]%*%Wminhalf
                           gammain <- rep(NaN,sum(G))
                           gammain[ind0] <- 0
                           fitTauin <- Rsolnp::solnp(par = initgamma, fun=penMSE, b=Btauin[[i]][indnot0], 
                                                     A=Ain[[i]][indnot0,indnot0],lam=lambda2,
                                                     LB = rep(0,G), eqfun=eqfun, eqB = 1,control=list(trace=0))
                           #gammain[indnot0] <- Wminhalf%*%as.vector(fitTauin$pars)
                           gammain[indnot0] <- as.vector(fitTauin$pars)
                           
                           return(gammain)
                         })
                         gammain <- c(list(gammain1),gammain)
                         
                         ### Compute MSE on left-out part
                         Aouttauin <- lapply(1:nsplits,function(split){Aout[[split]][indnot0,]%*%gammain[[split]]})
                         RSStau <- sum(sapply(1:nsplits,function(split){sum((Aouttauin[[split]]-Btauout[[split]][indnot0])^2)/nsplits}))
                         return(RSStau)
                       }
                     }
              )
              
              #find optimal lambda_2 given muhat
              
              tic<-proc.time()[[3]]
              lambda2 <- optim(mean(log(rangelambda2)),RSSlambdatau,method="Brent",
                               lower = log(rangelambda2[1]),upper = log(rangelambda2[2]))
              lambdashat[2] <- exp(lambda2$par)
              #exp(lambda2$par)
              toc <- proc.time()[[3]]-tic
              # lambda2 <- optimise(RSSlambdatau,rangelambda2) #regular optimiser can get stuck in flat region
              # lambdashat[2] <- lambda2$minimum
              #browser()
              
              if(profplotRSS){ #profile plot lambda vs RSS
                lambdas <- 10^seq(-5,6,length.out=30)
                if(all(lambdas>rangelambda2[2] | lambdas<rangelambda2[1])){
                  lambdas <- 10^seq(log(rangelambda2)[1],log(rangelambda2)[2],length.out=30)
                } 
                FRSS <- sapply(log(lambdas),RSSlambdatau)
                profPlot <- plot(log10(lambdas),FRSS,xlab="hyperlambda (log10-scale)",ylab="RSS",
                                 main=paste("Group set ",Partitions,", ",hypershrinkage," hypershrinkage",sep=""))
                abline(v=log10(lambdashat[2]),col="red")
                abline(v=log10(rangelambda2[1]),col="blue",lty=2)
                abline(v=log10(rangelambda2[2]),col="blue",lty=2)
                if(!silent) print(paste("Estimated hyperlambda: ",lambdashat[2],sep=""))
              }
              
              # #first find range for lambda
              # tic <- proc.time()[[3]]
              # minTau <- gammas(10^-9) #minimally penalised tau
              # maxTau <- gammas(10^9) #maximally penalised tau
              # lb <- 10^-8
              # ub <- 10^8
              # diff <- (minTau-maxTau)^2*10^-2 #1 percent relative difference
              # while(all(abs(gammas(lb)[indnot0]-minTau[indnot0])<diff[indnot0]) & lb<ub){
              #   lb <- lb*10
              # }
              # while(all(abs(gammas(ub)[indnot0]-maxTau[indnot0])<diff[indnot0]) & ub>lb){
              #   ub <- ub/10
              # }
              # rangelambda2 <- c(lb/10,ub*10) #take values just outside the range
              # 
              # #then fit for range of lambda and take minimizer
              # if(hypershrinkage=="ridge"){
              #   lambdas <- 10^seq(log10(rangelambda2[1]),log10(rangelambda2[2]),length.out=100)
              # }else{
              #   lambdas <- 10^seq(log10(rangelambda2[1]),log10(rangelambda2[2]),length.out=30)
              # }
              # FRSS<-sapply(log(lambdas),RSSlambdatau)
              # minFRSS <- which.min(FRSS)
              # if(minFRSS==1) minFRSS <- rev(1:length(lambdas))[which.min(rev(FRSS))] #take least extreme lambda with same RSS
              # lambdashat[2] <- lambdas[minFRSS]
              # if(profplotRSS){ #profile plot lambda vs RSS
              #   profPlot <- plot(log10(lambdas),FRSS,xlab="hyperlambda (log10-scale)",ylab="RSS",
              #                    main=paste("Group set ",Partitions,", ",hypershrinkage," hypershrinkage",sep=""))
              #   abline(v=log10(lambdas[minFRSS]),col="red")
              #   if(!silent) print(paste("Estimated hyperlambda: ",lambdashat[2],sep=""))
              # }    
              # toc <- proc.time()[[3]]-tic
              
            }
            
            #-3.3.3|1.2.4 Compute group variance estimates for optimised hyperpenalty lambda ##############
            if(length(ExtraShrinkage2)==0){
              if(!silent) print(paste("Estimate group weights of group set ",Partitions,sep=""))
            }
            if(lambdashat[2]==0){
              gammatilde[indnot0] <- solve(A[indnot0,indnot0],Btau[indnot0])
              gamma <- pmax(0,gammatilde) #set negative tau to 0
            }else{
              gammatilde <- gammas(lambdashat[2])
              gamma <- pmax(0,gammatilde)
              
              if(length(ExtraShrinkage2)>0){
                if(!silent) print(paste("Select groups of group set ",Partitions,sep=""))
                if(all(gammatilde==0)){ #none selected
                  gamma <- rep(0,G)
                  gamma[indnot0] <- 1
                }else if(sum(gammatilde[indnot0]!=0)==1){ #just one selected
                  gamma <- gammatilde
                  Cnorm <- p/sum(c(gamma)%*%Zt)
                  gamma <- gamma*Cnorm
                }else{
                  #2.
                  output <- MoM(Partitions,hypershrinkage=ExtraShrinkage2,
                                pars=list(indnot0=which(gammatilde!=0),ind0=which(gammatilde==0)))
                  return(output)
                }
              }
              
            }
            if(normalise){
              Cnorm <- p/sum(c(gamma)%*%Zt)
              gammatilde <- gammatilde*Cnorm
              gamma <- gamma*Cnorm
            }
            
            if(any(is.nan(gamma))){warning("NaN in group variance");browser()}
          }
        }else{ 
          #-3.3.3|2 Without extra shrinkage---------------------------------------------------------------
          lambdashat <- c(0,0) 
          
          #-3.3.3|2.1 EB estimate group means ============================================================
          muhatp <-as.vector(rep(mu,sum(G))%*%Zt) #px1 vector with estimated prior mean for beta_k, k=1,..,p
          muhatp[(1:p)%in%unpen] <- 0
          weightsMu <- rep(NaN,sum(G))
          if(!is.nan(mu)){
            muhat<-rep(mu,length(muhat))
          }else{
            if(all(is.nan(betaold))){
              betaold<-rep(1,p) #used as weights
            }else{
              #normalise=FALSE #make sure tau not scaled back to target
            }
            A.mu <- matrix(unlist(
              lapply(Partitions,function(i){ #for each partition
                sapply(1:length(Kg[[i]]),function(j){ #for each group
                  #compute row with gamma_{xy}
                  x<-groupsets[[i]][[j]]
                  unlist(sapply(Partitions,function(prt){sapply(groupsets[[prt]],function(y){sum(L[x,]%*%t(t(R[,y])/Ik[[prt]][y])%*%diag(betaold[y]))/Kg[[i]][j]})}))
                }, simplify="array")
              })
            ),c(sum(G),sum(G)),byrow=TRUE) #reshape to matrix of size sum(G)xsum(G)
            Bmu <- unlist(
              lapply(Partitions,function(i){ #for each partition
                sapply(1:length(Kg[[i]]),function(j){ #for each group
                  x<-groupsets[[i]][[j]]
                  sum(betasinit[x]-muinitp[x]+L[x,]%*%(R[,pen]%*%muinitp[pen]))/Kg[[i]][j]
                })
              })
            )  
            if(any(is.nan(fixWeightsMu))){ #compute group means for specific partition
              #correct for fixed group means corresponding to groups with variance 0
              if(length(ind0)>0){
                muhat[indnot0] <- solve(A.mu[indnot0,indnot0],Bmu[indnot0]- 
                                          as.matrix(A.mu[indnot0,ind0],c(length(indnot0),length(ind0)))%*%muhat[ind0])
              }else{
                muhat[indnot0] <- solve(A.mu[indnot0,indnot0],Bmu[indnot0])
              }
              #muhat[ind0,Itr+1] <- muhat[ind0,Itr] #means of groups with variance 0 stay the same
              muhatp <-as.vector(c(muhat)%*%Zt)*betaold #px1 vector with estimated prior mean for beta_k, k=1,..,p
              muhatp[(1:p)%in%unpen] <- 0
              weightsMu <- muhat*p/sum(as.vector(c(muhat)%*%Zt))
            }else{ #compute partition weights/co-data weights
              weightsPart <- sqrt(G[indGrpsGlobal[Partitions]])
              weightMatrixMu <- matrix(rep(0,sum(G)*length(G)),sum(G),length(G))
              for(i in 1:length(G)){
                weightMatrixMu[indGrpsGlobal[[Partitions[i]]],i] <- fixWeightsMu[indGrpsGlobal[[Partitions[i]]]]
              }
              if(!all(round(fixWeightsMu,10)==1)){ #all partitions shrunk to overall mu
                weightsMu <- rep(1/length(Partitions),length(Partitions)) #partition/co-data weights
                muhat<-weightMatrixMu%*%weightsMu #group weights multiplied with partition/co-data weights
              }else{
                A.mutilde <- A.mu%*%weightMatrixMu%*%diag(weightsPart)
                muhat <- solve(t(A.mutilde)%*%A.mutilde,t(A.mutilde)%*%c(Bmu)) / weightsPart
                muhat<- pmax(0,muhat)
                weightsMu <- muhat/sum(muhat) #partition/co-data weights
                muhat<-weightMatrixMu%*%weightsMu #group weights multiplied with partition/co-data weights
              }
            }
            
            
            # if(normalise){ #TRUE by default
            #   C<-mutrgt*p/sum(muhatp)
            #   muhat[,Itr+1]<-muhat[,Itr+1]*C
            #   muhatp <-as.vector(c(muhat[,Itr+1])%*%Zt)*betaold #px1 vector with estimated prior mean for beta_k, k=1,..,p
            # }
            ## Should be same as:
            # A.mu <- ind %*% C %*% diag(betaold) %*% t(ind) /c(Kg)
            # Bmu <- ind %*% (betasinit - Cacc%*%rep(muinit,p)) /c(Kg) #betasinit depend on initial mutrgt
            # muhat <- solve(A.mu,Bmu)
          }
          
          #-3.3.3|2.2 EB estimate group variances ========================================================
          if(!is.nan(tausq)){
            gamma <- rep(1,length(gamma))
          }else{
            Btau <- unlist(
              lapply(Partitions,function(i){ #for each partition
                sapply(1:length(Kg[[i]]),function(j){ #for each group
                  #compute row with gamma_{xy}
                  x<-groupsets[[i]][[j]]
                  x<-setdiff(x,zeroV) #ad-hoc fix: remove covariates with 0 variance (will be set to 0 anyways)
                  #sum(pmax(0,(betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,pen]%*%(muhatp[pen]-muinitp[pen])))^2)/V[x]-1),na.rm=TRUE)/Kg[[i]][j]
                  sum((betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,pen]%*%(muhatp[pen]-muinitp[pen])))^2)/V[x]-1,na.rm=TRUE)/Kg[[i]][j]
                })
              })
            )
            A <- matrix(unlist(
              lapply(Partitions,function(i){ #for each partition
                sapply(1:length(Kg[[i]]),function(j){ #for each group
                  #compute row with gamma_{xy}
                  x<-groupsets[[i]][[j]]
                  x<-setdiff(x,zeroV) #ad-hoc fix: remove covariates with 0 variance (will be set to 0 anyways)
                  #compute row with gamma_{xy}
                  unlist(sapply(Partitions,function(prt){sapply(groupsets[[prt]],function(y){
                    y<-setdiff(y,zeroV)
                    sum(t(c(1/V[x])*L[x,])%*%L[x,]*(R[,y]%*%(t(R[,y])/c(Ik[[prt]][y])*c(tauglobal[datablockNo[y]]))),na.rm=TRUE)/Kg[[i]][j]
                  })}))
                }, simplify="array")
              })
            ),c(sum(G),sum(G)),byrow=TRUE) #reshape to matrix of size sum(G)xsum(G)
            
            if(any(is.nan(fixWeightsTau))){
              if(!cont_codata){
                if(grepl("positive",hypershrinkage)){
                  # penMSE <- function(gamma,b,A,lam) return(sum((b-A%*%gamma)^2)) 
                  # #Aacc <- A%*%Wminhalf
                  # gamma <- rep(0,G)
                  # fitTau <- Rsolnp::solnp(par = rep(1,length(indnot0)), fun=penMSE, b=Btau[indnot0],
                  #                         A=A[indnot0,indnot0],
                  #                         LB = rep(0,length(indnot0)),control=list(trace=0))
                  # gamma[indnot0] <- as.vector(fitTau$pars)
                  # gammatilde <- gamma
                  
                  gamma <- rep(0,G)
                  fitTau <- nnls::nnls(A[indnot0,indnot0],Btau[indnot0])
                  gamma[indnot0] <- as.vector(fitTau$x)
                  gammatilde <- gamma
                }else{
                  gamma <- rep(0,G)
                  gammatilde <- solve(t(A[indnot0,indnot0])%*%A[indnot0,indnot0],
                                      t(A[indnot0,indnot0])%*%Btau[indnot0])
                  gamma <- pmax(0,gammatilde)
                  
                  if(normalise){
                    Cnorm <- p/sum(c(gamma)%*%Zt[,pen,drop=FALSE])
                    gamma<-gamma*Cnorm
                  }
                }
              }else{
                if(grepl("positive",hypershrinkage)){
                  # penMSE <- function(gamma,b,A,lam) return(sum((b-A%*%gamma)^2))
                  # #Aacc <- A%*%Wminhalf
                  # 
                  # gamma <- rep(0,G)
                  # fitTau <- Rsolnp::solnp(par = rep(1,length(indnot0)), fun=penMSE, b=Btau,
                  #                         A=A[,indnot0,drop=FALSE],
                  #                         LB = rep(0,length(indnot0)),control=list(trace=0))
                  # gamma[indnot0] <- as.vector(fitTau$pars)
                  # gammatilde <- gamma
                  
                  gamma <- rep(0,G)
                  fitTau <- nnls::nnls(A[,indnot0,drop=FALSE],Btau)
                  gamma[indnot0] <- as.vector(fitTau$x)
                  gammatilde <- gamma
                }else{
                  gamma <- rep(0,G)
                  gammatilde <- solve(t(A[,indnot0,drop=FALSE])%*%A[,indnot0,drop=FALSE],
                                      t(A[,indnot0,drop=FALSE])%*%Btau)
                  gamma <- gammatilde
                  #gamma <- pmax(0,gammatilde)
                  # if(normalise){
                  #   Cnorm <- p/sum(c(gamma)%*%Zt[,pen])
                  #   gamma<-gamma*Cnorm
                  # }
                }
              }
              
              if(any(is.nan(gamma))){warning("NaN in group variance")}
            }else{ #compute partition weights/co-data weights
              if(!silent) print("Estimate group set weights")
              weightsPart <- sqrt(G[Partitions])
              weightMatrixTau <- matrix(rep(0,sum(G)*length(G)),sum(G),length(G))
              for(i in 1:length(G)){
                weightMatrixTau[indGrpsGlobal[[Partitions[i]]],i] <- fixWeightsTau[indGrpsGlobal[[Partitions[i]]]]
              }
              if(all(round(fixWeightsTau,10)==1)){ #all partitions shrunk to overall mu
                gamma <- rep(1/length(Partitions),length(Partitions)) #partition/co-data weights
              }else{
                if(any(partWeightsTau[,Itr]==0)){
                  set0 <- unlist(indGrpsGlobal[which(partWeightsTau[,Itr]==0)])
                  ind0 <- union(ind0,set0)
                  indnot0 <- setdiff(indnot0,set0)
                }
                
                Atilde <- A[indnot0,indnot0]%*%weightMatrixTau[indnot0,partWeightsTau[,Itr]!=0] 
                
                #Three options to solve for partition weights (use only one):
                #Solve for tau and truncate negative values to 0
                #browser()
                cosangle<-t(as.matrix(t(Zt[,pen,drop=FALSE])%*%weightMatrixTau))%*%as.matrix(t(Zt[,pen,drop=FALSE])%*%weightMatrixTau)
                cosangle<-abs(t(cosangle/sqrt(diag(cosangle)))/sqrt(diag(cosangle)))
                cosangle <- cosangle-diag(rep(1,m))
                if(any(cosangle>0.999)){
                  indAngle <- which(cosangle>0.999,arr.ind=TRUE)
                  indAngle <- indAngle[indAngle[,1]<indAngle[,2],,drop=FALSE]
                  print(paste("Estimated group weights for group sets",indAngle[,1],
                              "and",indAngle[,2],"found to be similar"))
                  print("Switch to constrained optimisation such that group set weights >0 for stability")
                  
                }
                
                gammatilde<-rep(0,m)
                temp<-try(solve(t(Atilde)%*%Atilde,t(Atilde)%*%c(Btau[indnot0])),silent=TRUE)
                if(class(temp)[1]=="try-error" | any(cosangle>0.999)){
                  # #Solve for tau>=0 with convex optimisation package
                  D<-length(G)
                  w <- CVXR::Variable(D)
                  objective <- CVXR::Minimize(sum((Atilde%*%w-c(Btau[indnot0]))^2))
                  constraint1 <- diag(rep(1,D))%*%w >= 0
                  problem <- CVXR::Problem(objective,constraints = list(constraint1))
                  result <- solve(problem)
                  gamma <- c(result$getValue(w))
                  gammatilde <- gamma
                  gamma <- pmax(0,gammatilde) #correct round-off errors: partition/co-data weights
                  if(all(is.na(gamma))){
                    #infeasible CVXR problem; remove one of similar group sets and give equal weight
                    indremove <- unique(indAngle[,2]) #index of group sets to be removed
                    indmatch <- sapply(indremove,function(k){ #index to map removed back to matches
                      indmatch <- indAngle[which(indAngle[,2]==k),1]
                      indmatch <- setdiff(indmatch,indremove)[1] #take first in case multiple matches
                      return(indmatch)
                    })
                    temp<-try(solve(t(Atilde[,-indremove])%*%Atilde[,-indremove],
                                    t(Atilde[,-indremove])%*%c(Btau[indnot0])),silent=TRUE)
                    if(class(temp)[1]=="try-error"){
                      # #Solve for tau>=0 with convex optimisation package
                      D<-length(G)-length(indremove)
                      w <- CVXR::Variable(D)
                      objective <- CVXR::Minimize(sum((Atilde[,-indremove]%*%w-c(Btau[indnot0]))^2))
                      constraint1 <- diag(rep(1,D))%*%w >= 0
                      problem <- CVXR::Problem(objective,constraints = list(constraint1))
                      result <- solve(problem)
                      gamma <- rep(0,length(G))
                      gamma[-indremove] <- c(result$getValue(w))
                      gamma[indremove] <- gamma[indmatch]
                      gammatilde <- gamma
                      gamma <- pmax(0,gammatilde) #correct round-off
                    }else{
                      gammatilde<-rep(0,m)
                      gammatilde[-indremove] <- temp
                      gamma[indremove] <- gamma[indmatch]
                      gamma <- pmax(0,gammatilde)
                    }
                  }
                }else{
                  gammatilde[partWeightsTau[,Itr]!=0] <- temp
                  gamma <- pmax(0,gammatilde)
                  #temp<-optim(gamma,function(x){sum((Atilde%*%x-c(Btau[indnot0]))^2)},lower=rep(0,length(gamma)),method="L-BFGS-B")
                  #gamma<-temp$par 
                }
                
                if(0){
                  #solve for w\in[0,1] with convex optimisation package when Atilde is computationally singular
                  #library(CVXR)
                  D<-length(G)
                  w <- CVXR::Variable(D)
                  objective <- CVXR::Minimize(sum((Atilde%*%w-c(Btau[indnot0]))^2))
                  constraint1 <- diag(rep(1,D))%*%w >= 0
                  constraint2 <-  matrix(rep(1,D), nrow = 1)%*%w ==1
                  problem <- CVXR::Problem(objective,constraints = list(constraint1, constraint2))
                  result <- CVXR::solve(problem)
                  gammatilde <- c(result$getValue(w))
                  gamma <- pmax(0,gammatilde) #correct round-off errors: partition/co-data weights
                }
                
                if(normalise){
                  gammatilde <- gammatilde/sum(gamma)
                  gamma <- gamma/sum(gamma)
                }
              }
            }
          }
        }
      }else{ #Set up MoM equations using pxG co-data matrix 
        #-3.3.3|2 With co-data provided in Z matrix-----------------------------------------------------
        #-3.3.3|2.1 EB estimate group means ============================================================
        #Not yet supported
        muhatp <-as.vector(rep(mu,sum(G))%*%Zt) #px1 vector with estimated prior mean for beta_k, k=1,..,p
        muhatp[(1:p)%in%unpen] <- 0
        weightsMu <- rep(NaN,sum(G))
        if(!is.nan(mu)){
          muhat<-rep(mu,length(muhat))
        }else{
          if(cont_codata) stop("Not implemented for prior means, provide co-data
                                in groupsets")
        }
        
        #-3.3.3|2.2 EB estimate group variances ========================================================
        if(!is.nan(tausq)){
          gamma <- rep(1,length(gamma))
        }else{
          #-3.3.3|1.2.1 Compute linear system for whole partition #####################################
          x<-pen
          x<-setdiff(x,zeroV)
          Btau <- ((betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,x]%*%(muhatp[x]-muinitp[x])))^2)/V[x]-1)
          #Btau <- pmax(0,((betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,x]%*%(muhatp[x]-muinitp[x])))^2)/V[x]-1))
          
          #break up in parts as pxp matrix takes too much memory
          #most intensive matrix multplicatian step*(np + pG)
          #for step*(np + pG)=15*10^9 memory still fine
          
          nsteps <- ceiling(length(x)^2*(n+sum(G))/15/10^9)
          step <- ceiling(length(x)/nsteps)
          start <- 1
          part <- start:(start+step-1)
          A <- Matrix::tcrossprod((L[x[part],,drop=FALSE]%*%R[,x,drop=FALSE])^2/c(V[x[part]]),
                                  Zt[,x,drop=FALSE]*c(tauglobal[datablockNo[x]]))
          start <- start+step
          while(start < length(x)){
            part <- start:min(length(x),(start+step-1))
            A2 <- Matrix::tcrossprod((L[x[part],,drop=FALSE]%*%R[,x,drop=FALSE])^2/c(V[x[part]]),
                                     Zt[,x,drop=FALSE]*c(tauglobal[datablockNo[x]]))
            A <- rbind(A, A2)
            rm(A2)
            start <- start+step
          }
          A <- as.matrix(A)
          
          
          # (L[block,]%*%R)^2%*%Z
          # diag(AD_yB^T)
          # diag(LR*diag(Z)*R^TL^T)
          # R2 = R*diag(Z)*R^T = (R[,x,drop=FALSE]%*%(t(R[,x,drop=FALSE])*c(Zt[j,x])
          # diag(L*R2*L^T)
          # print("Memory problems, switching to slower but more memory-efficient computation")
          #same as below, but this one is really slow, but more memory-efficient
          # A <- sapply(1:dim(Zt)[1],function(j){ #for each co-data variable
          #   if(j%in%ind0) return(rep(NaN,p))
          #   #compute row with gamma_{xy}
          #   sapply(x,function(k){
          #     sum(t(c(1/V[k])*L[k,,drop=FALSE])%*%L[k,,drop=FALSE]*
          #           (R[,x,drop=FALSE]%*%(t(R[,x,drop=FALSE])*c(Zt[j,x])* #opslaan apart kan niet tenzij n^2 matrix
          #                                  c(tauglobal[datablockNo[x]]))),na.rm=TRUE)
          #   })
          # }, simplify="array") #matrix of size pxsum(G)
          #Same as straightforwardly computing:
          #A2 <- as.matrix(((L[x,]%*%R[,x])^2/c(V[x]))%*%t(Zt[,x,drop=FALSE])*c(tauglobal[datablockNo[x]]))
          
          #other format of A needed for mgcv
          if(hypershrinkage=="mgcv"){
            Alist <- lapply(indGrpsGlobal, function(ind) as.matrix(A[,ind,drop=FALSE]))
            names(Alist) <- paste("Z",1:length(Alist),sep="")
            
            #make intercept
            if(intrcpt.bam){
              start <- 1
              part <- start:(start+step-1)
              Aintrcpt <- ((L[x[part],,drop=FALSE]%*%R[,x,drop=FALSE])^2/c(V[x[part]]))%*%c(tauglobal[datablockNo[x]])
              start <- start+step
              while(start < length(x)){
                part <- start:min(length(x),(start+step-1))
                A2 <- ((L[x[part],,drop=FALSE]%*%R[,x,drop=FALSE])^2/c(V[x[part]]))%*%c(tauglobal[datablockNo[x]])
                Aintrcpt <- rbind(Aintrcpt, A2)
                rm(A2)
                start <- start+step
              }
              Aintrcpt <- as.matrix(Aintrcpt)
            }
            
            
            # Aintrcpt <- sapply(x,function(k){
            #     sum(t(c(1/V[k])*L[k,,drop=FALSE])%*%L[k,,drop=FALSE]*
            #           (R[,x,drop=FALSE]%*%(t(R[,x,drop=FALSE])*c(rep(1,length(x)))*
            #                                  c(tauglobal[datablockNo[x]]))),na.rm=TRUE)
            #   })
            #same as below, but more memory-efficient:
            # Aintrcpt2 <- as.matrix(((L[pen,]%*%R[,pen])^2/c(V[pen]))%*%rep(1,length(x))*
            #                         c(tauglobal[datablockNo[x]]))
          }
          
          if(cont_codata & !grepl("none",hypershrinkage) & length(indnot0)>1){ 
            #-3.3.3|1 With extra shrinkage -----------------------------------------------------------------
            # Use splits to find hyperpenalty
            # Minimise RSS over lambda1, lambda2 to find optimal penalties for shrinkage on co-data level
            # Randomly sample half for in-part and other half for out-part
            tempx <- 1:length(c(Btau))
            INDin <- replicate(nsplits, sample(tempx, floor(length(tempx)/2), replace=FALSE)) #matrix ~(p/2)xnsplits
            INDout <- apply(INDin,2,function(x) setdiff(tempx,x)) #matrix ~(p/2)xnsplits
            
            #-3.3.3|1.2 EB estimate group variances =========================================================
            gamma <- rep(1,sum(G))
            if(is.nan(tausq)){
              if(is.nan(lambdashat[2])){
                #-3.3.3|1.2.2 For each split, compute linear system #########################################
                
                #Btauin for split i is simply Btau[INDin[,i]], Ain is simply A[INDin[,i]]
                
                #-3.3.3|1.2.3 Define function RSSlambdatau, #################################################
                # using the extra shrinkage penalty function corresponding to parameter hypershrinkage
                rangelambda2 <- c(10^-5,10^6)
                sc <- 1
                switch(hypershrinkage,
                       "ridge+constraints"={
                         #scale matrix and vector for numerical performance
                         sc <- sqrt(sum(A^2, na.rm=TRUE))
                         rangelambda2 <- rangelambda2*sc
                         
                         name <- paste("Z",Partitions,sep="")
                         M.ineq <- b.ineq <- M.eq <- b.eq <- NULL
                         if("M.ineq"%in%names(paraCon[[name]])){
                           M.ineq <- paraCon[[name]][["M.ineq"]]
                           b.ineq <- paraCon[[name]][["b.ineq"]]
                         }
                         if("M.eq"%in%names(paraCon[[name]])){
                           M.eq <- paraCon[[name]][["M.eq"]]
                           b.eq <- paraCon[[name]][["b.eq"]]
                         }
                         S1 <- paraPen[[name]][["S1"]] #generalised ridge penalty matrix,
                         if(is.null(S1)) S1 <- diag(rep(1,dim(A)[2])) #use ordinary ridge penalty
                         if("S2"%in%names(paraPen[[name]])) warning("Only first penalty matrix S1 is included")
                         
                         #function to compute tau for linear system given a hyperpenalty lambda2
                         gammas <- function(lambda2){
                           gammas <- rep(0,G)
                           
                           Bext <- c(Btau,rep(0,length(indnot0)))
                           Aext <- rbind(A[,indnot0], lambda2*S1[indnot0,indnot0]) #include ridge penalty
                           # gammas[indnot0] <- pracma::lsqlincon(C=Aext, d=Bext, 
                           #                     A=M.ineq[,indnot0], b=b.ineq, 
                           #                     Aeq=M.eq[,indnot0], beq=b.eq)
                           temp <- try(pracma::lsqlincon(C=Aext/sc, d=Bext/sc, 
                                                                   A=M.ineq[,indnot0], b=b.ineq, 
                                                                   Aeq=M.eq[,indnot0], beq=b.eq),silent=TRUE)
                           if(class(temp)[1]=="try-error") gammas <- rep(0,G)#return(Inf)
                           else gammas[indnot0] <- temp
                           return(gammas)
                         }
                         
                         #function to compute the Residual Sum of Squares on the splits given lambda2
                         RSSlambdatau <- function(lambda2){
                           lambda2 <- exp(lambda2)
                           ### Compute MSE on left-out part
                           MSEout <- sapply(1:nsplits,function(split){
                             #Compute gamma for in-part
                             gammain <- rep(0,G)
                             Bextin <- c(Btau[INDin[,split]],rep(0,length(indnot0)))
                             Aextin <- rbind(A[INDin[,split],indnot0], lambda2*S1[indnot0,indnot0]) #include ridge penalty
                             # gammain[indnot0] <- pracma::lsqlincon(C=Aextin, d=Bextin, 
                             #                               A=M.ineq[,indnot0], b=b.ineq, 
                             #                               Aeq=M.eq[,indnot0], beq=b.eq)
                             
                             #for very large or small lambda, may obtain inconsistent constraints
                             #fix by setting to Inf
                             temp <- try(pracma::lsqlincon(C=Aextin/sc, d=Bextin/sc, 
                                                               A=M.ineq[,indnot0], b=b.ineq, 
                                                               Aeq=M.eq[,indnot0], beq=b.eq),silent=TRUE)
                             if(class(temp)[1]=="try-error") gammain <- rep(0,G)#return(Inf)
                             else gammain[indnot0] <- temp
                             #compute MSE on out-part
                             MSE <- mean((A[INDout[,split],]%*%gammain - Btau[INDout[,split]])^2)
                             return(MSE)
                           })
                           
                           RSStau <- mean(MSEout) #mean over splits
                           return(RSStau)
                         }
                       },
                       "ridge"={
                         sc <- sqrt(sum(A^2, na.rm=TRUE))
                         rangelambda2 <- rangelambda2*sc
                         
                         name <- paste("Z",Partitions,sep="")
                         S1 <- paraPen[[name]][["S1"]] #generalised ridge penalty matrix
                         
                         #function to compute tau for linear system given a hyperpenalty lambda2
                         gammas <- function(lambda2){
                           gammas <- rep(0,G)
                           
                           Bext <- c(Btau,rep(0,length(indnot0)))/sc
                           #include ridge penalty
                           Aext <- rbind(A[,indnot0], lambda2*S1[indnot0,indnot0])/sc
                           
                           gammas[indnot0] <- solve(t(Aext)%*%Aext,t(Aext)%*%Bext)
                           
                           return(gammas)
                         }
                         
                         #function to compute the Residual Sum of Squares on the splits given lambda2
                         RSSlambdatau <- function(lambda2){
                           lambda2 <- exp(lambda2)
                           ### Compute MSE on left-out part
                           MSEout <- sapply(1:nsplits,function(split){
                             #Compute gamma for in-part
                             gammain <- rep(0,G)
                             Bextin <- c(Btau[INDin[,split]],rep(0,length(indnot0)))/sc
                             #include ridge penalty
                             Aextin <- rbind(A[INDin[,split],indnot0], lambda2*S1[indnot0,indnot0])/sc
                             temp <- try(solve(t(Aextin)%*%Aextin,t(Aextin)%*%Bextin),silent=TRUE)
                             if(class(temp)[1]=="try-error"){ #for too small lambda, matrix is singular
                               return(NA)
                             }else{
                               gammain[indnot0] <- temp
                             }
                             #compute MSE on out-part
                             MSE <- mean((A[INDout[,split],]%*%gammain - Btau[INDout[,split]])^2)
                             return(MSE)
                           })
                           
                           RSStau <- mean(MSEout) #mean over splits
                           return(RSStau)
                         }
                       },
                       "mgcv"={ #use mgcv for multiple co-data sources simultaneously
                         #fit bam on U directly with penalty matrix splineS
                         if(intrcpt.bam){
                           fmla <- as.formula(paste("Btau ~ -1 + Aintrcpt +", paste(names(Alist), collapse= "+")))
                           fit2 <- mgcv::bam( formula=fmla, data=c(list(Btau=Btau,Aintrcpt=Aintrcpt),Alist), 
                                              paraPen = paraPen,
                                              #paraPen = list(A=list(S1=splineS)),
                                              method=bam.method) #can include ridge penalty if desired (with list(splineS, DeltaRidge))
                           gamma <- fit2$coefficients
                           
                           # #try least absolute deviation 
                           # lam <- 10^12
                           # Btauplus <- c(Btau,rep(0,dim(splineS)[1]))
                           # Aplus <- rbind(cbind(Aintrcpt,A),cbind(rep(0,dim(splineS)[1]),lam*splineS))
                           # qfit <- rq(Btauplus ~ Aplus - 1)
                           # a <- coefficients(qfit)
                           # plot(z,splineB%*%a[-1]+a[1])
                           
                           #gamma formulation
                           if(gammaForm){
                             fmla <- as.formula(paste("Btau2 ~ -1 + Aintrcpt +", paste(names(Alist), collapse= "+")))
                             Btau2 <- ((betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,x]%*%(muhatp[x]-muinitp[x])))^2)/V[x])
                             
                             fit2 <- mgcv::bam( formula = fmla, data=c(list(Btau2=Btau2,Aintrcpt=Aintrcpt),Alist), 
                                                paraPen = paraPen,
                                                method=bam.method,family = Gamma(link="identity"),
                                                offset=rep(1,length(Btau2))) 
                             gamma <- fit2$coefficients
                           }
                           
                         }else{
                           fmla <- as.formula(paste("Btau ~ -1 +", paste(names(Alist), collapse= "+")))
                           fit2 <- mgcv::bam( formula = fmla, data=c(list(Btau=Btau), Alist), 
                                              #paraPen = list(A=list(S1=splineS)),
                                              paraPen = paraPen,
                                              method=bam.method) #can include ridge penalty if desired (with list(splineS, DeltaRidge))
                           gamma <- rep(0,sum(G)+1)
                           gamma[-1] <- fit2$coefficients
                           
                           #gamma formulation
                           if(gammaForm){
                             fmla <- as.formula(paste("Btau2 ~ -1 +", paste(names(Alist), collapse= "+")))
                             Btau2 <- ((betasinit[x]^2-(muinitp[x]+L[x,]%*%(R[,x]%*%(muhatp[x]-muinitp[x])))^2)/V[x])
                             
                             fit2 <- mgcv::bam( formula = fmla, data=c(list(Btau2=Btau2),Alist), 
                                                #paraPen = list(A=list(S1=splineS)),
                                                paraPen = paraPen,
                                                method=bam.method,family = Gamma(link="identity"),
                                                offset=rep(1,length(Btau2))) 
                             gamma <- rep(0,sum(G)+1)
                             gamma[-1] <- fit2$coefficients
                           }
                         }
                         lambdashat[2] <- NaN
                         gammatilde <- gamma
                         
                       }
                )
                if(hypershrinkage!="mgcv"){
                  #find optimal lambda_2 given muhat
                  tic<-proc.time()[[3]]
                  lambda2 <- optim(mean(log(rangelambda2)),RSSlambdatau,method="Brent",
                                   lower = log(rangelambda2[1]),upper = log(rangelambda2[2]))
                  lambdashat[2] <- exp(lambda2$par)
                  #exp(lambda2$par)
                  toc <- proc.time()[[3]]-tic
                  # lambda2 <- optimise(RSSlambdatau,rangelambda2) #regular optimiser can get stuck in flat region
                  # lambdashat[2] <- lambda2$minimum
                  #browser()
                  
                  if(profplotRSS){ #profile plot lambda vs RSS
                    lambdas <- 10^seq(-5,6,length.out=30)*sc
                    FRSS <- sapply(log(lambdas),RSSlambdatau)
                    profPlot <- plot(log10(lambdas),FRSS,xlab="hyperlambda (log10-scale)",ylab="RSS",
                                     main=paste("Co-data source Z",Partitions,", ",hypershrinkage," hypershrinkage",sep=""))
                    abline(v=log10(lambdashat[2]),col="red")
                    abline(v=log10(rangelambda2[1]),col="blue",lty=2)
                    abline(v=log10(rangelambda2[2]),col="blue",lty=2)
                    if(!silent) print(paste("Estimated hyperlambda: ",lambdashat[2],sep=""))
                  }
                }
                
                
              }
              
              #-3.3.3|1.2.4 Compute group variance estimates for optimised hyperpenalty lambda ##############
              if(hypershrinkage!="mgcv"){
                if(length(ExtraShrinkage2)==0){
                  if(!silent) print(paste("Estimate weights of co-data source ",Partitions,sep=""))
                }
                gammatilde <- gammas(lambdashat[2])
                gamma <- gammatilde
              }
              
              if(length(ExtraShrinkage2)>0){warning(paste("Only first hypershrinkage taken:",hypershrinkage))}
              
              if(any(is.nan(gamma))){warning("NaN in group variance");browser()}
            }
          }else{ 
            #-3.3.3|2 Without extra shrinkage---------------------------------------------------------------
            lambdashat <- c(0,0) 
            #-3.3.3|2.2 EB estimate group variances ========================================================
              if(any(is.nan(fixWeightsTau))){
                switch(hypershrinkage,
                       "none+constraints"={
                         sc <- sqrt(sum(A^2, na.rm=TRUE))
                         
                         name <- paste("Z",Partitions,sep="")
                         M.ineq <- b.ineq <- M.eq <- b.eq <- NULL
                         if("M.ineq"%in%names(paraCon[[name]])){
                           M.ineq <- paraCon[[name]][["M.ineq"]]
                           b.ineq <- paraCon[[name]][["b.ineq"]]
                         }
                         if("M.eq"%in%names(paraCon[[name]])){
                           M.eq <- paraCon[[name]][["M.eq"]]
                           b.eq <- paraCon[[name]][["b.eq"]]
                         }
                         
                         gamma <- rep(0,G)
                         gamma[indnot0] <- pracma::lsqlincon(C=A[,indnot0]/sc, d=Btau/sc, 
                                                      A=M.ineq[,indnot0], b=b.ineq, 
                                                      Aeq=M.eq[,indnot0], beq=b.eq)
                         gammatilde <- gamma
                       },
                       "none"={
                         sc <- sqrt(sum(A^2, na.rm=TRUE))
                         
                         gamma <- rep(0,G)
                         gamma[indnot0] <- solve(t(A[,indnot0]/sc)%*%(A[,indnot0]/sc),
                                                 t(A[,indnot0]/sc)%*%(Btau/sc))
                         gammatilde <- gamma
                       }
                       )
                
                if(any(is.nan(gamma))){warning("NaN in group variance")}
              }else{ #compute partition weights/co-data weights
                if(!silent) print("Estimate co-data source weights")
                weightMatrixTau <- matrix(rep(0,sum(G)*length(G)),sum(G),length(G))
                for(i in 1:length(G)){
                  weightMatrixTau[indGrpsGlobal[[Partitions[i]]],i] <- fixWeightsTau[indGrpsGlobal[[Partitions[i]]]]
                }
                if(all(round(fixWeightsTau,10)==1)){ #all partitions shrunk to overall mu
                  gamma <- rep(1/length(Partitions),length(Partitions)) #partition/co-data weights
                }else{
                  if(any(partWeightsTau[,Itr]==0)){
                    set0 <- unlist(indGrpsGlobal[which(partWeightsTau[,Itr]==0)])
                    ind0 <- union(ind0,set0)
                    indnot0 <- setdiff(indnot0,set0)
                  }
                  
                  Atilde <- A[,indnot0]%*%weightMatrixTau[indnot0,partWeightsTau[,Itr]!=0] 
                  
                  #solve with constraint w>=0
                  gammatilde<-rep(0,m)
                  w <- try(pracma::lsqlincon(C=as.matrix(Atilde), d=Btau,lb=0),silent=TRUE)
                  
                  # #solve with constraint w>=0 and sum(w)>=1
                  # gammatilde<-rep(0,m)
                  # w <- try(pracma::lsqlincon(C=as.matrix(Atilde), d=Btau,lb=0,
                  #                            A=matrix(rep(-1,dim(Atilde)[2]),1,dim(Atilde)[2]),b=-1),
                  #          silent=TRUE)
                  if(class(w)[1]=="try-error" | all(w==0)) w <- rep(1/m,m)
                  gammatilde <- w
                  gamma <- w
                }
              }
          }
        }  
      }
      
      #MoM output
      return(list(
        lambdashat=lambdashat,
        muhat=muhat,
        gamma=gamma,
        gammatilde=gammatilde,
        hypershrinkage=hypershrinkage,
        weightsMu=weightsMu
      ))
    }
    
    #For each partition/dataset, use MoM to get group weights
    if(!cont_codata){
      #NOTE: possible to use different penalty functions
      MoMGroupRes <- lapply(1:m,function(i){
        if(partWeightsTau[i,Itr]!=0){
          MoM(Partitions=i,hypershrinkage=hypershrinkage[i],groupsets.grouplvl=groupsets.grouplvl[[i]])
        }else{
          return(list(muhat = muhat[indGrpsGlobal[[i]],Itr],
                      gammatilde = gammatilde[indGrpsGlobal[[i]],Itr],
                      gamma = gamma[indGrpsGlobal[[i]],Itr],
                      weightsMu = weightsMu[indGrpsGlobal[[i]],Itr],
                      lambdashat = lambdashat[i, Itr,],
                      hypershrinkage=hypershrinkage[i]))
        }
      }
      )
      
      #global update group parameters
      muhat[,Itr+1]<-unlist(lapply(MoMGroupRes,function(prt){prt$muhat}))
      gammatilde[,Itr+1] <- unlist(lapply(MoMGroupRes,function(prt){prt$gammatilde}))
      gamma[,Itr+1] <- unlist(lapply(MoMGroupRes,function(prt){prt$gamma}))
      weightsMu[,Itr+1] <- unlist(lapply(MoMGroupRes,function(prt){prt$weightsMu}))
      lambdashat[, Itr+1,] <- array(unlist(lapply(MoMGroupRes,function(prt){prt$lambdashat})),c(2,1,m))
      
      
      #For fixed group weights, use MoM to get partition/co-data weights
      if(m>1){
        if(!is.null(w)){
          if(is.nan(mu)){
            partWeightsMu[,Itr+1] <- w
            partWeightsMuG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsMu[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
          }
          if(is.nan(tausq)){
            partWeightsTau[,Itr+1] <- w
            partWeightsTauG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsTau[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
          }
        }else{
          if(!all(round(gamma[,Itr+1],10)==1)){
            #if(!any(partWeightsTau[,Itr]==0)){
            MoMPartRes <- MoM(Partitions=1:m,hypershrinkage="none",fixWeightsMu=weightsMu[,Itr+1],fixWeightsTau=gamma[,Itr+1])
            # }else{
            #   partNot0 <- which(partWeightsTau[,Itr]!=0)
            #   MoMPartRes <- MoM(Partitions=partNot0,hypershrinkage="none",
            #                     fixWeightsMu=weightsMu[unlist(indGrpsGlobal[partNot0]),Itr+1],
            #                     fixWeightsTau=gamma[unlist(indGrpsGlobal[partNot0]),Itr+1])
            # }
            
          }
          if(is.nan(mu)){
            partWeightsMu[,Itr+1] <- MoMPartRes$weightsMu
            partWeightsMuG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsMu[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
          }
          if(is.nan(tausq)){
            if(all(round(gamma[,Itr+1],10)==1)){
              partWeightsTau[,Itr+1] <- rep(1/m,m)
              partWeightsTauG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsTau[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
            }else{
              partWeightsTau[,Itr+1] <- pmax(MoMPartRes$gamma,0)
              partWeightsTauG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsTau[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
            }
          }
        }
      }
    }else{
      if(all(hypershrinkage=="mgcv")){
        #compute co-data variable weights
        if(partWeightsTau[i,Itr]!=0){
          #TD: if all hypershrinkage==mgcv?
          MoMGroupRes <- MoM(Partitions=1:m,hypershrinkage=hypershrinkage[1],
                             groupsets.grouplvl=NULL)
        }else{
          MoMGroupRes <- list(muhat = muhat[indGrpsGlobal[[i]],Itr],
                              gammatilde = gammatilde[indGrpsGlobal[[i]],Itr],
                              gamma = gamma[indGrpsGlobal[[i]],Itr],
                              weightsMu = weightsMu[indGrpsGlobal[[i]],Itr],
                              lambdashat = lambdashat[i, Itr,],
                              hypershrinkage=hypershrinkage[1])
        }
        #global update group parameters
        muhat[,Itr+1] <- MoMGroupRes$muhat
        gamma0tilde <- MoMGroupRes$gammatilde[1]
        gammatilde[,Itr+1] <- MoMGroupRes$gammatilde[-1]
        gamma0 <- MoMGroupRes$gamma[1]
        gamma[,Itr+1] <- MoMGroupRes$gamma[-1]
        weightsMu[,Itr+1] <- MoMGroupRes$weightsMu
        #lambdashat[, Itr+1,] <- MoMGroupRes$lambdashat #TD: maybe want bam penalties
        
        #group set weights intrinsic; set to 1
        if(m>1){
          if(!is.null(w)){
            if(is.nan(mu)){
              partWeightsMu[,Itr+1] <- w
              partWeightsMuG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsMu[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
            }
            if(is.nan(tausq)){
              partWeightsTau[,Itr+1] <- w
              partWeightsTauG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsTau[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
            }
          }else{
            if(is.nan(mu)){
              partWeightsMu[,Itr+1] <- rep(1,m)
              partWeightsMuG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsMu[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
            }
            if(is.nan(tausq)){
              partWeightsTau[,Itr+1] <- rep(1,m)
              partWeightsTauG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsTau[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
            }
          }
        }
      }else{
        #NOTE: possible to use different penalty functions
        MoMGroupRes <- lapply(1:m,function(i){
          if(partWeightsTau[i,Itr]!=0){
            MoM(Partitions=i,hypershrinkage=hypershrinkage[i],groupsets.grouplvl=NULL)
          }else{
            return(list(muhat = muhat[indGrpsGlobal[[i]],Itr],
                        gammatilde = gammatilde[indGrpsGlobal[[i]],Itr],
                        gamma = gamma[indGrpsGlobal[[i]],Itr],
                        weightsMu = weightsMu[indGrpsGlobal[[i]],Itr],
                        lambdashat = lambdashat[i, Itr,],
                        hypershrinkage=hypershrinkage[i]))
          }
        }
        )
        
        #global update group parameters
        muhat[,Itr+1]<-unlist(lapply(MoMGroupRes,function(prt){prt$muhat}))
        gammatilde[,Itr+1] <- unlist(lapply(MoMGroupRes,function(prt){prt$gammatilde}))
        gamma[,Itr+1] <- unlist(lapply(MoMGroupRes,function(prt){prt$gamma}))
        weightsMu[,Itr+1] <- unlist(lapply(MoMGroupRes,function(prt){prt$weightsMu}))
        lambdashat[, Itr+1,] <- array(unlist(lapply(MoMGroupRes,function(prt){prt$lambdashat})),c(2,1,m))
        
        
        #For fixed group weights, use MoM to get partition/co-data weights
        if(m>1){
          if(!is.null(w)){
            if(is.nan(mu)){
              partWeightsMu[,Itr+1] <- w
              partWeightsMuG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsMu[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
            }
            if(is.nan(tausq)){
              partWeightsTau[,Itr+1] <- w
              partWeightsTauG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsTau[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
            }
          }else{
            if(!all(round(gamma[,Itr+1],10)==1)){
              #if(!any(partWeightsTau[,Itr]==0)){
              MoMPartRes <- MoM(Partitions=1:m,hypershrinkage="none",fixWeightsMu=weightsMu[,Itr+1],fixWeightsTau=gamma[,Itr+1])
              # }else{
              #   partNot0 <- which(partWeightsTau[,Itr]!=0)
              #   MoMPartRes <- MoM(Partitions=partNot0,hypershrinkage="none",
              #                     fixWeightsMu=weightsMu[unlist(indGrpsGlobal[partNot0]),Itr+1],
              #                     fixWeightsTau=gamma[unlist(indGrpsGlobal[partNot0]),Itr+1])
              # }
              
            }
            if(is.nan(mu)){
              partWeightsMu[,Itr+1] <- MoMPartRes$weightsMu
              partWeightsMuG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsMu[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
            }
            if(is.nan(tausq)){
              if(all(round(gamma[,Itr+1],10)==1)){
                partWeightsTau[,Itr+1] <- rep(1/m,m)
                partWeightsTauG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsTau[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
              }else{
                partWeightsTau[,Itr+1] <- pmax(MoMPartRes$gamma,0)
                partWeightsTauG[,Itr+1] <- unlist(sapply(1:m,function(x){rep(partWeightsTau[x,Itr+1],G[x])})) #total number of groups x 1 vector with partition weights
              }
            }
          }
        }
      }
    }
    

    if(all(is.nan(betaold))){
      betaold <-rep(1,p) #px1 vector with estimated prior mean for beta_k, k=1,..,p
    }
    if(is.nan(mu)){
      muhatp <- as.vector(c(partWeightsMuG[,Itr+1]*muhat[,Itr+1])%*%Zt[,pen,drop=FALSE])*betaold[pen]
      muhatp[(1:p)%in%unpen] <- 0
    }else{
      muhatp<-rep(mu,p)
    }
    
    #-3.3.4 Update group-specific penalties ###################################################################
    if(is.nan(tausq)){
      if(all(gamma[,Itr+1]==0) & gamma0==0){
        if(all(muhatp==0)){
          warning("All group weights estimated at 0 and set to 1 to retrieve ordinary ridge performance")
          gamma[,Itr+1]<-rep(1,sum(G))
        }
      }
      if(all(partWeightsTauG==0)){#set all partition/group weights to 1 (i.e. no difference in partitions/groups)
        lambdap<-sigmahat/(tauglobal[datablockNo]*as.vector(c(gamma[,1])%*%Zt[,pen,drop=FALSE]+gamma0)) #target tau/overall
      }else{
        if(!cont_codata){
          lambdap<-sigmahat/(tauglobal[datablockNo]*as.vector(c(partWeightsTauG[,Itr+1]*gamma[,Itr+1])%*%Zt[,pen,drop=FALSE]+gamma0)) #specific penalty for beta_k
          lambdap[lambdap<0]<-Inf 
        }else{
          taup<-tauglobal[datablockNo]*as.vector(c(partWeightsTauG[,Itr+1]*gammatilde[,Itr+1])%*%Zt[,pen,drop=FALSE]+gamma0)
          lambdap<-sigmahat/taup
          lambdap[lambdap<0]<-Inf 
        }
        
      } 
      lambdap[(1:p)%in%unpen] <- 0
    }else{
      lambdap<-rep(sigmahat/tausq,p)
      lambdap[(1:p)%in%unpen] <- 0
    }
    #should be the same as
    #lambdap2<-sigmahat/as.vector(c(partWeightsTauG*gamma*tauglobal)%*%Zt[,pen])
  
    #-3.3.5 Update beta using glmnet or multiridge#######################################################################
    if(!silent) print("Estimate regression coefficients")
    if(all(gamma[,Itr+1]==0) | all(lambdap==Inf)){
      lambdaoverall <- exp(mean(log(sigmahat/tauglobal[datablockNo[pen]])))
      beta <- muhatp
      if(intrcpt){
        if(model=="linear"){
          glmGR <- list(a0=sum(Y-X%*%beta)/n)
          a0 <- sum(Y-X%*%beta)/n
        }else if(model=='logistic'){
          glmGR <- list(a0=sum(Y-exp(X%*%beta)/(1+exp(X%*%beta)))/n) 
          a0 <- sum(Y-exp(X%*%beta)/(1+exp(X%*%beta)))/n
        }else{
          glmGR <- list(a0=0)
          a0 <- 0
        }
      }else{
        glmGR <- list(a0=0)
        a0 <- 0
      }
      warning("All regression coefficients (set to) 0 due to too large penalties")
    }else{
      if(model=="linear"){
        sd_y2 <- sqrt(var(Y-X %*% muhatp)*(n-1)/n)[1]
      }else if(model%in%c('logistic','cox',"family")){
        sd_y2 <- 1 #do not standardize Y-offset for logistic/cox model
        if(model=="family" && fml$family=="gaussian"){
          sd_y2 <- sqrt(var(Y-X %*% muhatp)*(n-1)/n)[1]
        }
      }
      if(any(is.nan(sqrt(lambdap[pen])))){browser()}
      
      if(est_beta_method=="glmnet"){ #glmnet
        lambdaoverall <- exp(mean(log(sigmahat/tauglobal[datablockNo[pen]])))
        Xacc <- X
        Xacc[,pen] <- as.matrix(X[,pen] %*% Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),
                                                         x=c(1/sqrt(lambdap[pen]/lambdaoverall))))
        
        if(model=="cox"){
          glmGR <- glmnet::glmnet(Xacc,as.matrix(Y),alpha=0,
                          #lambda = lambdaoverall/n*sd_y2*2,
                          family=fml,
                          offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)], standardize = FALSE,
                          penalty.factor=penfctr)
          
          beta <- coef(glmGR, s=lambdaoverall/n*sd_y2*2,thresh = 10^-10, exact=TRUE,
                       x=Xacc,y=as.matrix(Y),
                       family=fml,
                       offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)],
                       penalty.factor=penfctr) 
          beta[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * beta[pen] + muhatp[pen]
          a0 <- NULL
        }else{
          glmGR <- glmnet::glmnet(Xacc,Y,alpha=0,
                          #lambda = lambdaoverall/n*sd_y2,
                          family=fml,
                          offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)], intercept = intrcpt, standardize = FALSE,
                          penalty.factor=penfctr)
          
          beta <- coef(glmGR, s=lambdaoverall/n*sd_y2,thresh = 10^-10, exact=TRUE,
                       x=Xacc, y=Y, family=fml,
                       offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)], intercept = intrcpt,
                       penalty.factor=penfctr)[-1]
          beta[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * beta[pen] + muhatp[pen]
          a0 <- coef(glmGR, s=lambdaoverall/n*sd_y2,thresh = 10^-10, exact=TRUE,
                           x=Xacc, y=Y, family=fml,
                           offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)], intercept = intrcpt,
                           penalty.factor=penfctr)[1]
        }
        # beta <- as.vector(glmGR$beta) 
        #beta[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * beta[pen] + muhatp[pen]
        
        # beta[pen] <- as.vector(glmGR$beta)[pen] + muhatp
        
        # if(standardise_Y){
        #   beta <- beta*sd_y_former
        #   glmGR$a0 <- glmGR$a0*sd_y_former
        # } 
      }else{ #use multiridge package to estimate betas
        lambdaoverall <- exp(mean(log(sigmahat/tauglobal[datablockNo[pen]])))
        minlam <- max(mean(lambdap[pen][lambdap[pen]<Inf]) , 10^-5)
        if(lambdaoverall<minlam) lambdaoverall <- minlam
        Xacc <- X
        Xacc[,pen] <- as.matrix(X[,pen] %*% Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),
                                                                 x=c(1/sqrt(lambdap[pen]/lambdaoverall))))
        XXbl <- list(Xacc%*%t(Xacc))
        #Compute betas
        XXT <- multiridge::SigmaFromBlocks(XXbl,penalties=lambdaoverall) #create nxn Sigma matrix = sum_b [lambda_b)^{-1} X_b %*% t(X_b)]
        if(model!="cox"){
          if(sum((1:p)%in%unpen)>0){
            fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
          }else{
            fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcpt) #Fit. fit$etas contains the n linear predictors
          }
        }else{
          if(sum((1:p)%in%unpen)>0){
            fit <- multiridge::IWLSCoxridge(XXT,Y=Y, model=model,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
          }else{
            fit <- multiridge::IWLSCoxridge(XXT,Y=Y) #Fit. fit$etas contains the n linear predictors
          }
        }

        betas <- multiridge::betasout(fit, Xblocks=list(Xacc[,pen]), penalties=lambdaoverall) #Find betas.
        a0 <- c(betas[[1]][1]) #intercept
        if(is.null(a0) & model!="cox") a0 <- 0 
        beta <- rep(0,p)
        beta[(1:p)%in%unpen] <- betas[[1]][-1] #unpenalised variables
        beta[pen] <- betas[[2]]
        beta[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * beta[pen]
        rm(betas)
        
        # browser()
        # #here it is the same for different global lambda as it should
        # #note: not for when intercept is included
        # Xacc <- X
        # Xacc[,pen] <- as.matrix(X[,pen] %*% Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),
        #                                                          x=c(1/sqrt(lambdap[pen]/lambdaoverall))))
        # Xacc <- cbind(Xacc,rep(1,n))
        # beta <- solve(t(Xacc)%*%Xacc + diag(c(rep(lambdaoverall,p))), t(Xacc)%*%Y)
        # a0 <- rev(beta)[1]
        # beta <- rev(rev(beta)[-1])
        # beta[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * beta[pen]
        # beta2<-beta
        
      }
    }
    
    #-3.3.6 Update predictions on independent data (if given) ################################################
    if(!is.null(X2)){
      if(intrcpt){
        X2c <- cbind(X2,rep(1,n2))
      }else{
        X2c <- X2
      }
      #Ypredridge <- predict(glmGR,newx=X2)
      if(model=="linear"){
        YpredGR[,Itr+1] <- X2 %*% beta + a0
        MSEecpc[Itr+1]<- sum((YpredGR[,Itr+1]-Y2)^2)/n2
      }else if(model=='logistic'){
        X2c <- cbind(X2,rep(1,n2))
        YpredGR[,Itr+1] <- 1/(1+exp(-X2 %*% beta - a0))
        MSEecpc[Itr+1]<- sum((YpredGR[,Itr+1]-Y2)^2)/n2
        if(any(is.nan(YpredGR[,Itr+1]))){browser()}
      }else if(model=='cox'){ 
        expXb<-exp(X %*% c(beta))
        h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXb[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
        H0 <- sapply(sort(c(Y[,1],Y2[,1])),function(Ti){sum(h0[Y[,1]<=Ti])})
        YpredGR <- outer(c(exp(X2 %*% beta)),c(H0))
        colnames(YpredGR)<-paste("Time",signif(sort(c(Y[,1],Y2[,1]))),6)  
        YpredGR <- cbind(rep(NA,n2),YpredGR) #first column is removed in returning output
        MSEecpc[Itr+1]<- NaN #sum((YpredGR[,Itr+1]-Y2[,2])^2)/n2
      }else if(model=="family"){
        X2acc <- X2
        X2acc[,pen] <- as.matrix(X2[,pen] %*% Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),
                                                                 x=c(1/sqrt(lambdap[pen]/lambdaoverall))))
        YpredGR[,Itr+1] <- predict(glmGR, s=lambdaoverall/n*sd_y2,thresh = 10^-10, exact=TRUE,
                                newx=X2acc, x=Xacc, y=Y, family=fml,
                                offset = X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)], 
                                newoffset = X2[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)], 
                                intercept = intrcpt,
                                penalty.factor=penfctr)
        MSEecpc[Itr+1]<- sum((YpredGR[,Itr+1]-Y2)^2)/n2
      }
    }
    
    #-3.3.7 Set current estimates as initial estimates for next iteration #####################################
    betasinit <- beta
    muinitp <- muhatp
    intrcptinit <- a0
    ind0 <- which(gamma[,Itr+1]==0) #update index of groups with zero variance
    indnot0 <- which(gamma[,Itr+1]>0)
    Itr <- Itr+1
    
    # if(standardise_Y){
    #   betasinit <- betasinit/sd_y_former
    #   intrcptinit <- intrcptinit/sd_y_former
    # } 

    if(length(ind0)==sum(G)){
      if(!silent) print(paste("All prior group variances estimated to be 0, iterating stopped after",Itr,"iteration(s)"))
      break;
    }
  }
  
  #-4. (optional) Posterior variable selection-------------------------------------------------------
  #postselection: indicates which of the two methods possible is used to do post-hoc variable selection
  #maxsel: maximum number of variables selected
  if(postselection!=FALSE){
    if(!silent) print("Sparsify model with posterior selection")
      #for multi==FALSE; tauglobal=sigmahat/lambdaoverall
    
    if(model=="family"){
      #insert model=fml for family object
      postSel <- postSelect(X=X,Y=Y,beta=beta,intrcpt=a0,penfctr=penfctr, 
                            postselection=postselection,maxsel=maxsel, 
                            penalties=lambdap,model=fml,tauglobal=sigmahat/lambdaoverall,
                            sigmahat=sigmahat,muhatp=muhatp, 
                            X2=X2,Y2=Y2,silent=silent)
    }else{
      postSel <- postSelect(X=X,Y=Y,beta=beta,intrcpt=a0,penfctr=penfctr, 
                            postselection=postselection,maxsel=maxsel, 
                            penalties=lambdap,model=model,tauglobal=sigmahat/lambdaoverall,
                            sigmahat=sigmahat,muhatp=muhatp, 
                            X2=X2,Y2=Y2,silent=silent)
    }
      
    
  }
  
  #-5. (optional) Compare with glmnet --------------------------------------------------------------
  betaridge<-NaN; Ypredridge<-NaN; 
  if(!is.nan(compare) & compare!=FALSE){
    if(est_beta_method=="glmnet"){ #glmnet
      lambdaoverall <- exp(mean(log(lambdaridge[datablockNo[pen]])))
      Xacc <- X
      Xacc[,pen] <- as.matrix(X[,pen] %*% Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),
                                                               x=c(1/sqrt(lambdaridge[datablockNo[pen]]/lambdaoverall))))
      if(model=="cox"){
        glmR <- glmnet::glmnet(Xacc,as.matrix(Y),family=fml,alpha=0,
                               #lambda=lambdaoverall*sd_y/n*2,
                               standardize = FALSE,
                               penalty.factor=penfctr)
        betaridge <- coef(glmR, s=lambdaoverall*sd_y/n,thresh = 10^-10, exact=TRUE,
                          x=Xacc, y=as.matrix(Y),
                          family=fml,
                          penalty.factor=penfctr)
        betaridge[pen] <- c(1/sqrt(lambdaridge[datablockNo[pen]]/lambdaoverall)) * betaridge[pen]
        a0_ridge <- NULL
      }else{
        glmR <- glmnet::glmnet(X,Y,family=fml,alpha=0,
                               #lambda=lambdaoverall*sd_y/n,
                               standardize = FALSE,intercept=intrcptGLM,
                               penalty.factor=penfctr)
        betaridge <- coef(glmR, s=lambdaoverall*sd_y/n,thresh = 10^-10, exact=TRUE,
                          x=X, y=Y, family=fml,
                          intercept=intrcptGLM,
                          penalty.factor=penfctr)[-1]
        a0_ridge <- coef(glmR, s=lambdaoverall*sd_y/n,thresh = 10^-10, exact=TRUE,
                        x=X, y=Y, family=fml,
                        intercept=intrcptGLM,
                        penalty.factor=penfctr)[1]
        betaridge[pen] <- c(1/sqrt(lambdaridge[datablockNo[pen]]/lambdaoverall)) * betaridge[pen]
      }
      # betaridge <- as.vector(glmR$beta)
      # betaridge[pen] <- c(1/sqrt(lambdaridge[datablockNo[pen]]/lambdaoverall)) * betaridge[pen]
    }else{ #use multiridge package to update ordinary ridge betas
      Xbl <- multiridge::createXblocks(lapply(datablocks,function(ind) X[,intersect(ind,ind[!(ind%in%unpen)])]))
      XXbl <- multiridge::createXXblocks(lapply(datablocks,function(ind) X[,intersect(ind,ind[!(ind%in%unpen)])]))
      
      #Compute betas
      XXT <- multiridge::SigmaFromBlocks(XXbl,penalties=lambdaridge) #create nxn Sigma matrix = sum_b [lambda_b)^{-1} X_b %*% t(X_b)]
      if(model!="cox"){
        if(sum((1:p)%in%unpen)>0){
          fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcptGLM,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
        }else{
          fit <- multiridge::IWLSridge(XXT,Y=Y, model=model,intercept=intrcptGLM) #Fit. fit$etas contains the n linear predictors
        }
      }else{
        if(sum((1:p)%in%unpen)>0){
          fit <- multiridge::IWLSCoxridge(XXT,Y=Y, model=model,X1=X[,(1:p)%in%unpen]) #Fit. fit$etas contains the n linear predictors
        }else{
          fit <- multiridge::IWLSCoxridge(XXT,Y=Y) #Fit. fit$etas contains the n linear predictors
        }
      }
      
      betas <- multiridge::betasout(fit, Xblocks=Xbl, penalties=lambdaridge) #Find betas.
      a0_ridge <- c(betas[[1]][1]) #intercept-
      if(is.null(a0_ridge) & model!="cox") a0_ridge <- 0
      betaridge <- rep(0,p) 
      betaridge[(1:p)%in%unpen] <- betas[[1]][-1] #unpenalised variables
      betaridge[pen] <- betas[[2]]
      rm(betas)
    }
    if(!is.null(X2)){
      #Ypredridge <- predict(glmR,newx=X2)
      #browser()
      if(intrcptGLM){
        X2c <- cbind(X2,rep(1,n2))
      }else{
        X2c <- X2
        a0_ridge <- 0
      }
      if(model=="linear"){
        Ypredridge <- X2 %*% betaridge + a0_ridge
        MSEridge <- sum((Ypredridge-Y2)^2)/n2
      } 
      if(model=='logistic'){
        Ypredridge <- 1/(1+exp(-X2 %*% betaridge - a0_ridge))
        MSEridge <- sum((Ypredridge-Y2)^2)/n2
      }else if(model=="cox"){
        expXb<-exp(X %*% c(betaridge))
        h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXb[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
        H0 <- sapply(sort(c(Y[,1],Y2[,1])),function(Ti){sum(h0[Y[,1]<=Ti])})
        Ypredridge <- outer(c(exp(X2 %*% betaridge)),c(H0))
        colnames(Ypredridge)<-paste("Time",signif(sort(c(Y[,1],Y2[,1]))),6)
        MSEridge<- NaN #sum((Ypredridge-Y2[,2])^2)/n2
      }else if(model=="family"){
        X2acc <- X2
        X2acc[,pen] <- as.matrix(X2[,pen] %*% Matrix::sparseMatrix(i=1:length(pen),j=1:length(pen),
                                                                 x=c(1/sqrt(lambdaridge[datablockNo[pen]]/lambdaoverall))))
        Ypredridge <- predict(glmR, s=lambdaoverall/n*sd_y2,thresh = 10^-10, exact=TRUE,
                              newx=X2acc, x=X, y=Y, family=fml,
                              intercept = intrcptGLM,
                              penalty.factor=penfctr)
        MSEridge <- sum((Ypredridge-Y2)^2)/n2
      }
    }
  }

  #-6. Output -------------------------------------------------------------------------------------
  names(gamma0)<-NULL
  names(beta) <- colnamesX
  rownames(gamma) <- namesZ
  gamma_temp<-gamma[,nIt+1]
  attributes(gamma_temp)$codataSource <- codataSource
  rownames(gammatilde) <- namesZ
  names(lambdap) <- colnamesX
  w <- partWeightsTau[,nIt+1]
  names(w) <- colnamesZ
  
  output <- list(
    beta=beta, #beta from ecpc (with Group Ridge penalties)
    intercept=a0, #unpenalised intercept covariate
    tauglobal=tauglobal, #overall tauglobal
    gammatilde = gammatilde[,nIt+1], #EB estimated prior group variance before truncating
    gamma=gamma_temp, #group weights variance
    gamma0 = gamma0,
    w = w, #group set weights in local variances
    penalties = lambdap, #penalty parameter on all p covariates
    hyperlambdas = lambdashat[2,nIt+1,], #hyperpenalties for all group sets
    #weights = weights, #weights used in ridge hypershrinkage
    #levelsY = levelsY, #in case of logistic
    sigmahat=sigmahat, #estimated sigma^2 (linear model)
    model=model
  )
  if(nIt>1){
    output$gamma <- gamma[,-1]
    output$gammatilde <- gammatilde[,-1]
    output$w <- partWeightsTau[,-1]
    output$hyperlambdas <- lambdashat[2,-1,]
  }
  if(!is.null(X2)){
    output$Ypred<-YpredGR[,-1] #predictions for test set
    output$MSEecpc <- MSEecpc[nIt+1] #MSE on test set
    if(nIt>1){
      output$Ypred<-YpredGR[,-1] #predictions for test set
      output$MSEecpc <- MSEecpc[-1] #MSE on test set
    }
  }
  
  if(mutrgt!=0){ #prior group means are estimated as well
    output$muhat <- muhat[,nIt+1] #EB estimated prior group means: mu_global*muweight
    output$muglobal <- mutrgt #overall mutrgt
    output$gamma.mu <- weightsMu[,nIt+1] #group weights mean
    output$w.mu <- partWeightsMu[,nIt+1] #group set weights mean
    output$hyperlambdas.mu <- lambdashat[1,nIt+1,] #hyperpenalties for all group sets for the group means
    output$offset.lp <- X[,!((1:p)%in%unpen)] %*% muhatp[!((1:p)%in%unpen)] #offset used in computing final beta (default 0),
  }

  if(!is.nan(compare) & compare!=FALSE){ #comparison with ordinary ridge obtained with glmnet
    names(betaridge) <- colnamesX
    output$betaridge <- betaridge #ordinary ridge or multiridge beta
    output$interceptridge <- a0_ridge
    output$lambdaridge <- lambdaridge #ordinary ridge lambda or multilambda
    if(!all(is.null(X2))){
      output$Ypredridge <- Ypredridge
      output$MSEridge <- MSEridge
    }
  }
  if(postselection!=FALSE){ #posterior selection is performed
    output$betaPost <- postSel$betaPost
    output$interceptPost <- postSel$a0
    if(!is.null(X2)){
      output$MSEPost <- postSel$MSEPost #MSE on independent data set (if given)
      output$YpredPost <- postSel$YpredPost #predictions for independent data set (if given)
    }
  }
  
  class(output) <- "ecpc"
  return(output)
}

### Other functions 
#Select covariates a posteriori----
postSelect <- function(object, X, Y, beta=NULL,intrcpt=0,penfctr=NULL, #input data
                          postselection=c("elnet,dense", "elnet,sparse", "BRmarginal,dense", 
                                          "BRmarginal,sparse","DSS"), #posterior selection method
                          maxsel=30, #maximum number of selected variables
                          penalties=NULL,model=c("linear", "logistic", "cox"),
                          tauglobal=NULL,sigmahat=NULL,muhatp=0, #needed for method "elnet"
                          X2=NULL,Y2=NULL,silent=FALSE){
  #Description:
  #Post-hoc variable selection: select maximum maxsel covariates of all penalised covariates
  #Unpenalised covariates (e.g. intercept) are always selected, on top of the maxsel number of selected penalised covariates
  #
  #Input data:
  #-X: (nxp) data matrix: data of p penalised and unpenalised covariates on n samples
  #-Y: (nx1) vector: response
  #-beta: previous estimate in dense model
  #-intrcpt: value of intercept in dense model
  #-penfctr: as in glmnet penalty.factor. Default: 0 if covariate is not penalised, 1 if covariate is penalised
  #
  #Input options:
  #-postselection: "elnet,dense" (default), "elnet,sparse", "BRmarginal,dense", "BRmarginal,sparse" or "DSS" 
  #               for corresponding post-selection method used and dense/sparse indicating if the global tau^2 is recalibrated 
  #              (for sparse settings) or not (for dense settings)
  #-maxsel: maximum number of penalised covariates to be selected (additional to all unpenalised covariates)
  #
  #Input additional parameters method "elnet":
  #-penalties: (length(pen)x1) vector: ridge penalties for all penalised covariates found in dense model
  #-model: "linear", "logistic" or "cox" for type of regression model used
  #-tauglobal, sigmahat, muhatp: parameter estimates of dense model fit
  #
  #Input optional:
  #X2,Y2 (optional): independent data and response on which predictions and MSE is computed
  
  #set variables given as in fitted ecpc-object, or to given values
  if(missing(object)) object <- NULL
  if(is.null(beta)) beta <- object$beta
  if(intrcpt==0 && !is.null(object$intercept) && object$intercept!=0) intrcpt <- object$intercept
  if(is.null(penalties)) penalties <- object$penalties
  if(is.null(sigmahat)) sigmahat <- object$sigmahat
  if(is.null(tauglobal)) tauglobal <- object$tauglobal
  if(length(tauglobal)>1) tauglobal <- exp(mean(log(tauglobal)))

  n<-dim(X)[1] #number of samples
  p<-dim(X)[2] #number of covariates (penalised and unpenalised)
  if(is.null(penfctr)) penfctr <- rep(1,p) #all covariates penalised the same
  if(length(postselection)>1) postselection <- "elnet+dense"
  if(class(model)[1]=="family"){
    #use glmnet package and cross-validation to compute initial global lambda en beta estimates
    fml <- model
    model <- "family"
    #use elastic net posterior selection
    postselection <- "elnet+dense"
    print("Posterior selection set to elnet+dense as only available option for general glm family")
  } 
  if(length(model)>1){
    if(all(is.element(Y,c(0,1))) || is.factor(Y)){
      model <- "logistic" 
    } else if(all(is.numeric(Y)) & !(is.matrix(Y) && dim(Y)[2]>1)){
      model <- "linear"
    }else{
      model <- "cox"
    }
  }

  
  maxsel2 <- pmin(maxsel, p)
  if(any(maxsel2<2)){
    warning("Number of variables to be selected should be at least 2 (out of convenience)")
    if(!silent) print("Maxsel values smaller than 2 are set to 2")
    maxsel2[maxsel2<2] <- 2
  }
  nonzeros <- beta!=0 #Fix beta that are already 0 

  lambdap<-penalties #ridge penalties for the penalised and non-zero covariates
  pen<-which(penfctr!=0) #index of penalised covariates

  switch(model,
         'linear'={
           fml <- 'gaussian'
           sd_y <- sqrt(var(Y)*(n-1)/n)[1]
         },
         'logistic'={
           fml <- 'binomial'
           sd_y <- 1 #do not standardise y in logistic setting
         },
         'cox'={
           fml <- 'cox'
           sd_y <- 1 #do not standardise y in cox regression setting
         },
         "family"={
           sd_y <- 1
           if(fml$family%in%c("gaussian")) sd_y <- sqrt(var(Y)*(n-1)/n)[1]
         }
  )
  
  if(grepl("elnet",postselection)){
    #multiple numbers possible of maximum number of covariates to be selected
    output<-lapply(maxsel2,function(x){
      
      #if already less than maxsel covariates selected
      if(sum(beta[pen]!=0)<=x){
        betaPost <- beta
        
        output<-list()
        output$betaPost <- betaPost #post-selection beta using Elastic Net
        output$whichPost <- which(nonzeros & (1:p)%in%pen) #index of selected penalised covariates
        output$a0 <- intrcpt
        
        if(!all(is.null(X2))){
          if(model=="linear"){
            YpredPost <- X2 %*% c(betaPost) + intrcpt
            MSEPost <- sum((YpredPost-Y2)^2)/length(Y2)
          } 
          if(model=='logistic'){
            YpredPost <- 1/(1+exp(-(X2 %*% c(betaPost) + intrcpt)))
            MSEPost <- sum((YpredPost-Y2)^2)/length(Y2)
            if(any(is.nan(YpredPost))){browser()}
          }else if(model=='cox'){ 
            expXbPost<-exp(X %*% c(betaPost))
            h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXbPost[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
            H0 <- sapply(sort(c(Y[,1],Y2[,1])),function(Ti){sum(h0[Y[,1]<=Ti])})
            YpredPost <- outer(c(exp(X2 %*% betaPost)),c(H0))
            colnames(YpredPost)<-paste("Time",signif(sort(c(Y[,1],Y2[,1]))),6)  
            MSEPost<- NaN #sum((YpredPost-Y2[,2])^2)/length(Y2[,2])
          }else if(model=="family"){
            YpredPost <- NaN
            MSEPost <- NaN
          }
          
          output$MSEPost <- MSEPost #MSE on independent data set (if given)
          output$YpredPost <- YpredPost #predictions for independent data set (if given)
        }
        
        return(output)
      }else{
        if(length(intrcpt)==0||intrcpt==0){intrcpt <- FALSE}else{intrcpt <- TRUE}
        if(length(muhatp)==1) muhatp <- rep(muhatp,p)
        if(all(muhatp==0)){
          offset <- rep(0,n)
          offset2 <- rep(0,n)
        } 
        else{
          offset <- X[,pen] %*% muhatp[pen] #in case prior mean of penalised covariates is not equal to 0
          offset2 <- X2[,pen] %*% muhatp[pen] #in case prior mean of penalised covariates is not equal to 0
        } 
        
        lambdaoverall <- sigmahat/tauglobal
        lam2 <- sigmahat/tauglobal/n*sd_y
        Xacc <- X
        Xacc[,pen] <- as.matrix(X[,pen] %*% Matrix::sparseMatrix(i=1:length(lambdap[pen]),j=1:length(lambdap[pen]),
                                                   x=c(1/sqrt(lambdap[pen]/lambdaoverall))) )
        if(!is.null(X2)){
          X2acc <- X2
          X2acc[,pen] <- as.matrix(X2[,pen] %*% Matrix::sparseMatrix(i=1:length(lambdap[pen]),j=1:length(lambdap[pen]),
                                                                     x=c(1/sqrt(lambdap[pen]/lambdaoverall))) )
        }

        #define function with output number of selected variables minus maximum possible
        #find root of function such that we have at most maxsel variables
        fsel <- function(alpha, maxselec = x) {
          if(alpha == 0) 
            return(p - maxselec)
          else {
            if(model=="cox"){
              glmPost <- glmnet::glmnet(Xacc[,nonzeros],as.matrix(Y),alpha=alpha,
                              #lambda = lam2/(1-alpha),
                              family=fml,
                              offset = offset, standardize = FALSE,
                              penalty.factor=penfctr[nonzeros])
              betaPost <- rep(0,p)
              betaPost[nonzeros] <- coef(glmPost, s=lam2/(1-alpha), exact=TRUE, 
                                         x=Xacc[,nonzeros],y=as.matrix(Y),
                                         offset = offset, penalty.factor=penfctr[nonzeros],
                                         family=fml,alpha=alpha,thresh = 10^-10)
              betaPost[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * betaPost[pen] + muhatp[pen]
            }else if(model %in% c("logistic","linear","family")){
              glmPost <- glmnet::glmnet(Xacc[,nonzeros],Y,alpha=alpha,
                              #lambda = lam2/(1-alpha),
                              family=fml,
                              offset = offset, standardize = FALSE,
                              intercept= intrcpt,
                              penalty.factor=penfctr[nonzeros])
              betaPost <- rep(0,p)
              betaPost[nonzeros] <- coef(glmPost, s=lam2/(1-alpha), exact=TRUE,
                                         x=Xacc[,nonzeros],y=Y,
                                         offset = offset, penalty.factor=penfctr[nonzeros],
                                         family=fml,alpha=alpha,intercept= intrcpt,thresh = 10^-10)[-1]
              glmPost$a0 <- coef(glmPost, s=lam2/(1-alpha), exact=TRUE,
                                 x=Xacc[,nonzeros],y=Y,
                                 offset = offset, penalty.factor=penfctr[nonzeros],
                                 family=fml,alpha=alpha,intercept= intrcpt,thresh = 10^-10)[1]
              betaPost[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * betaPost[pen] + muhatp[pen]
            }
            # betaPost <- rep(0,p)
            # betaPost[nonzeros] <- as.vector(glmPost$beta)
            # betaPost[pen] <- c(1/sqrt(lambdap[pen]/lambdaoverall)) * betaPost[pen] + muhatp[pen]
            return(sum(betaPost[pen] != 0) - maxselec ) #number of non-zero penalised covariates
          }
        }
        #Elastic net uses both lasso penalty lam1 and ridge penalty lam2
        #Keep lam2 fixed, use alpha and lambda arguments of glmnet to search in range lam1\in[0,10*lam2]
        #Equivalent to searching in alpha\in[0,10/11] and corresponding lambda
        rangeAlpha <- c(0,10/11)
        ItrAlp <- 1
        #if (sign(fsel(0))==sign(fsel(10/11))) browser()
        while(sign(fsel(0))==sign(fsel(rangeAlpha[2])) & ItrAlp <=50){
          rangeAlpha[2] <- rangeAlpha[2] + 0.5*(1-rangeAlpha[2])
          ItrAlp <- ItrAlp +1 
        }
        alpha <- uniroot(fsel, interval = rangeAlpha, maxiter = 200,tol=10^(-10))$root
        
        #for found alpha, refit model to see which beta are selected
        if(model=="cox"){
          glmPost0 <- glmnet::glmnet(Xacc[,nonzeros],as.matrix(Y),alpha=alpha,
                           #lambda = lam2/(1-alpha),
                           family=fml,
                           offset = offset, standardize = FALSE,
                           penalty.factor=penfctr[nonzeros])
          betaPost0 <- rep(0,p)
          betaPost0[nonzeros] <- coef(glmPost0, s=lam2/(1-alpha), exact=TRUE,
                                      x=Xacc[,nonzeros],y=as.matrix(Y),
                                      offset = offset, penalty.factor=penfctr[nonzeros],
                                      family=fml,alpha=alpha,thresh = 10^-10)
          betaPost0[pen] <- c(1/sqrt(lambdap[pen]/sigmahat*tauglobal)) * betaPost0[pen] + muhatp[pen]
          whichPostboth <- betaPost0 != 0 #both unpenalised covariates and selected penalised
        }else{
          glmPost0 <- glmnet::glmnet(Xacc[,nonzeros],Y,alpha=alpha,
                           #lambda = lam2/(1-alpha) ,
                           family=fml,
                           offset = offset, intercept = intrcpt, standardize = FALSE,
                           penalty.factor=penfctr[nonzeros])
          betaPost0 <- rep(0,p)
          betaPost0[nonzeros] <- coef(glmPost0, lam2/(1-alpha), exact=TRUE,
                                      x=Xacc[,nonzeros],y=Y,
                                      offset = offset, penalty.factor=penfctr[nonzeros],
                                      family=fml,alpha=alpha,intercept= intrcpt,thresh = 10^-10)[-1]
          glmPost0$a0 <- coef(glmPost0, lam2/(1-alpha), exact=TRUE,
                              x=Xacc[,nonzeros],y=Y,
                              offset = offset, penalty.factor=penfctr[nonzeros],
                              family=fml,alpha=alpha,intercept= intrcpt,thresh = 10^-10)[1]
          betaPost0[pen] <- c(1/sqrt(lambdap[pen]/sigmahat*tauglobal)) * betaPost0[pen] + muhatp[pen]
          whichPostboth <- betaPost0 != 0 #both unpenalised covariates and selected penalised
        }
        # betaPost0 <- rep(0,p)
        # betaPost0[nonzeros] <- as.vector(glmPost0$beta)
        # betaPost0[pen] <- c(1/sqrt(lambdap[pen]/sigmahat*tauglobal)) * betaPost0[pen] + muhatp[pen]
        # whichPostboth <- betaPost0 != 0 #both unpenalised covariates and selected penalised

        if(sum(whichPostboth)<=1){
          warning("At least two variables should be selected for glmnet")
          return(list(betaPost=betaPost0,whichPost=NULL,a0=glmPost0$a0))
        }
        if(grepl("dense",postselection)){ #use weighted penalty
          if(!all(muhatp==0)){
            offset <- X[,whichPostboth & (1:p)%in%pen, drop=FALSE] %*% muhatp[whichPostboth & (1:p)%in%pen, drop=FALSE]
            offset2 <- X2[,whichPostboth & (1:p)%in%pen, drop=FALSE] %*% muhatp[whichPostboth & (1:p)%in%pen, drop=FALSE]
          } 
          
          #recalibrate overall lambda using cross-validation on selected variables only
          if(grepl("dense2",postselection)){ 
            if(model=="cox"){
              lambdaGLM<-glmnet::cv.glmnet(Xacc[,whichPostboth, drop=FALSE],as.matrix(Y),alpha=0,
                                   family=fml,offset = offset ,standardize = FALSE,
                                   penalty.factor=penfctr[whichPostboth]) #alpha=0 for ridge
              lam2<-lambdaGLM$lambda.min
            }else{
              lambdaGLM<-glmnet::cv.glmnet(Xacc[,whichPostboth, drop=FALSE],Y,alpha=0,
                                   family=fml,offset = offset , intercept = intrcpt, standardize = FALSE,
                                   penalty.factor=penfctr[whichPostboth]) #alpha=0 for ridge
              lam2<-lambdaGLM$lambda.min
            }
          }
          
          #Recompute beta using only selected beta and group ridge penalty (without lasso penalty)
          if(model=="cox"){
            glmPost <- glmnet::glmnet(Xacc[,whichPostboth, drop=FALSE],as.matrix(Y),alpha=0,
                            #lambda = lam2,
                            family=fml,
                            offset = offset ,standardize = FALSE,
                            penalty.factor=penfctr[whichPostboth])
            betaPost <- rep(0,p)
            betaPost[whichPostboth] <- coef(glmPost, s=lam2, exact=TRUE, 
                                            x=Xacc[,whichPostboth, drop=FALSE],y=as.matrix(Y),
                                            offset = offset ,penalty.factor=penfctr[whichPostboth],
                                            family=fml,thresh = 10^-10)
          }else{
            glmPost <- glmnet::glmnet(Xacc[,whichPostboth, drop=FALSE],Y,alpha=0,
                            #lambda = lam2,
                            family=fml,
                            offset = offset , intercept = intrcpt, standardize = FALSE,
                            penalty.factor=penfctr[whichPostboth])
            betaPost <- rep(0,p)
            betaPost[whichPostboth] <- coef(glmPost, s=lam2, exact=TRUE,
                                            x=Xacc[,whichPostboth, drop=FALSE],y=Y,
                                            offset = offset, penalty.factor=penfctr[whichPostboth],
                                            family=fml,intercept= intrcpt,thresh = 10^-10)[-1]
            glmPost$a0 <- coef(glmPost, s=lam2, exact=TRUE,
                               x=Xacc[,whichPostboth, drop=FALSE],y=Y,
                               offset = offset, penalty.factor=penfctr[whichPostboth],
                               family=fml,intercept= intrcpt,thresh = 10^-10)[1]
          }
          # betaPost <- rep(0,p)
          # betaPost[whichPostboth] <- as.vector(glmPost$beta)
          indPostpen <- whichPostboth[pen] #penalised covariates in range 1:length(pen) that are selected and non-zero
          indPostp <- whichPostboth&((1:p)%in%pen) #penalised covariates in range 1:p that are selected and non-zero
          betaPost[indPostp] <- c(1/sqrt(lambdap[indPostp]/sigmahat*tauglobal)) * betaPost[indPostp] + muhatp[indPostp]
          
          whichPost <- which(indPostp) #index of selected penalised covariates 
          output<-list()
          output$betaPost <- betaPost #post-selection beta using Elastic Net
          output$whichPost <- whichPost #index of selected covariates
          output$a0 <- glmPost$a0
          #output$offsetPost <- offset #offset used in Post
        }else{# if(grepl("sparse",postselection)){ #refit standard ridge with newly cross-validated lambda
          Xacc <- X
          X2acc <- X2
          if(model=="cox"){
            lambdaGLM<-glmnet::cv.glmnet(X[,whichPostboth, drop=FALSE],as.matrix(Y),alpha=0,family=fml,
                                 standardize = FALSE,penalty.factor=penfctr[whichPostboth]) #alpha=0 for ridge
            lam2<-lambdaGLM$lambda.min
          }else{
            lambdaGLM<-glmnet::cv.glmnet(X[,whichPostboth, drop=FALSE],Y,alpha=0,family=fml,
                                 standardize = FALSE,penalty.factor=penfctr[whichPostboth]) #alpha=0 for ridge
            lam2<-lambdaGLM$lambda.min
          }
          if(model=="cox"){
            glmPost <- glmnet::glmnet(X[,whichPostboth, drop=FALSE],as.matrix(Y),alpha=0,
                            #lambda = lam2,
                            family=fml,
                            offset = offset ,standardize = FALSE,
                            penalty.factor=penfctr[whichPostboth])
            betaPost <- rep(0,p)
            betaPost[whichPostboth] <- coef(glmPost, s=lam2, exact=TRUE,
                                            x=X[,whichPostboth, drop=FALSE],y=as.matrix(Y),
                                            offset = offset, penalty.factor=penfctr[whichPostboth],
                                            family=fml,thresh = 10^-10)
          }else{
            glmPost <- glmnet::glmnet(X[,whichPostboth, drop=FALSE],Y,alpha=0,
                            #lambda = lam2,
                            family=fml,
                            intercept = intrcpt, standardize = FALSE,
                            penalty.factor=penfctr[whichPostboth])
            betaPost <- rep(0,p)
            betaPost[whichPostboth] <- coef(glmPost, s=lam2,exact=TRUE,
                                            x=X[,whichPostboth, drop=FALSE],y=Y,
                                            penalty.factor=penfctr[whichPostboth],
                                            family=fml,intercept= intrcpt,thresh = 10^-10)[-1]
            glmPost$a0 <- coef(glmPost, s=lam2, exact=TRUE,
                               x=Xacc[,whichPostboth, drop=FALSE],y=Y,
                               penalty.factor=penfctr[whichPostboth],
                               family=fml,intercept= intrcpt,thresh = 10^-10)[1]
          }
          # betaPost <- rep(0,p)
          # betaPost[whichPostboth] <- as.vector(glmPost$beta)
          indPostpen <- whichPostboth[pen] #penalised covariates in range 1:length(pen) that are selected and non-zero
          indPostp <- whichPostboth&((1:p)%in%pen) #penalised covariates in range 1:p that are selected and non-zero
          
          whichPost <- which(indPostp) #index of selected penalised covariates 
          output<-list()
          output$betaPost <- betaPost #post-selection beta using Elastic Net
          output$whichPost <- whichPost #index of selected covariates
          output$a0 <- glmPost$a0
          #output$offsetPost <- offset #offset used in Post
        }
        
        if(!all(is.null(X2))){
          if(model=="linear"){
            YpredPost <- X2 %*% c(betaPost) + output$a0
            MSEPost <- sum((YpredPost-Y2)^2)/length(Y2)
          } 
          if(model=='logistic'){
            YpredPost <- 1/(1+exp(-(X2 %*% c(betaPost) + output$a0)))
            MSEPost <- sum((YpredPost-Y2)^2)/length(Y2)
            if(any(is.nan(YpredPost))){browser()}
          }else if(model=='cox'){ 
            expXbPost<-exp(X %*% c(betaPost))
            h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXbPost[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
            H0 <- sapply(sort(c(Y[,1],Y2[,1])),function(Ti){sum(h0[Y[,1]<=Ti])})
            YpredPost <- outer(c(exp(X2 %*% betaPost)),c(H0))
            colnames(YpredPost)<-paste("Time",signif(sort(c(Y[,1],Y2[,1]))),6)  
            MSEPost<- NaN #sum((YpredPost-Y2[,2])^2)/length(Y2[,2])
          }else if(model=="family"){
            YpredPost <- predict(glmPost, s=lam2,exact=TRUE,thresh=10^-10,
                                 newx=X2acc[,whichPostboth,drop=FALSE],
                                 x=Xacc[,whichPostboth, drop=FALSE], y=Y,
                                 penalty.factor=penfctr[whichPostboth],
                                 family=fml,intercept= intrcpt,
                                 newoffset=offset2, offset=offset)
            MSEPost <- sum((YpredPost-Y2)^2)/length(Y2)
          }
          
          output$MSEPost <- MSEPost #MSE on independent data set (if given)
          output$YpredPost <- YpredPost #predictions for independent data set (if given)
        }
        
        return(output)
      }
    })
  }else if(grepl("DSS",postselection)){
      #Hahn and Carvalho 2014
      #Use adaptive lasso penalty on linear predictiors as in Equation (22) of their paper
      Xacc <- X
      Xacc[,pen] <- as.matrix(Xacc[,pen]%*% Matrix::sparseMatrix(i=1:length(beta[pen]),j=1:length(pen),
                                   x=c(sqrt(abs(beta[pen])))) )
      Ygamma <- Xacc%*%beta
      
      
      if(grepl("fast",postselection)){
        #use glmnet to compute beta for a whole range of lambda simultaneously
        #faster, but might result in fewer selected variables than asked for
        glmPost <- glmnet::glmnet(x=Xacc[,nonzeros],y=Ygamma,alpha=1,nlambda=100,
                          family="gaussian",
                          intercept = intrcpt, standardize = FALSE,
                          penalty.factor=penfctr[nonzeros], thresh=10^-10)
      }else{ 
        #retrieve exactly maxsel non-zero covariates
        if(length(intrcpt==0)||intrcpt==0){intrcpt <- FALSE}else{intrcpt <- TRUE}
        glmPost <- glmnet::glmnet(x=Xacc[,nonzeros],y=Ygamma,alpha=1,
                                  #lambda = lam1,
                                  family="gaussian",
                                  intercept = intrcpt, standardize = FALSE,
                                  penalty.factor=penfctr[nonzeros])
        fsel <- function(lam1,maxselec=maxsel2){
          return(sum(coef(glmPost, s=lam1, exact=TRUE,x=Xacc[,nonzeros],y=Ygamma,
                          penalty.factor=penfctr[nonzeros],intercept= intrcpt,thresh = 10^-10)[-1] !=0) - maxselec)
        }
      }
      
      output <- lapply(maxsel2,function(x){
        #if already less than maxsel covariates selected
        if(length(beta[pen]!=0)<=x){
          betaPost <- beta
          
          output<-list()
          output$betaPost <- betaPost #post-selection beta using Elastic Net
          output$whichPost <- which(betaPost!=0) #index of selected covariates
          output$a0 <- intrcpt
          
          if(!all(is.null(X2))){
            if(model=="linear"){
              YpredPost <- X2 %*% c(betaPost) + intrcpt
              MSEPost <- sum((YpredPost-Y2)^2)/length(Y2)
            } 
            if(model=='logistic'){
              YpredPost <- 1/(1+exp(-(X2 %*% c(betaPost) + intrcpt)))
              MSEPost <- sum((YpredPost-Y2)^2)/length(Y2)
              if(any(is.nan(YpredPost))){browser()}
            }else if(model=='cox'){ 
              expXbPost<-exp(X %*% c(betaPost))
              h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXbPost[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
              H0 <- sapply(sort(c(Y[,1],Y2[,1])),function(Ti){sum(h0[Y[,1]<=Ti])})
              YpredPost <- outer(c(exp(X2 %*% betaPost)),c(H0))
              colnames(YpredPost)<-paste("Time",signif(sort(c(Y[,1],Y2[,1]))),6)  
              MSEPost<- NaN #sum((YpredPost-Y2[,2])^2)/length(Y2[,2])
            }
            
            output$MSEPost <- MSEPost #MSE on independent data set (if given)
            output$YpredPost <- YpredPost #predictions for independent data set (if given)
          }
          
          return(output)
        }else{
          if(grepl("fast",postselection)){
            whlam <- which(glmPost$df<=x)
            takelam <- rev(whlam)[1]
            lam <- glmPost$lambda[takelam]
            
            betaPost <- rep(0,p)
            betaPost[nonzeros]<- as.vector(glmPost$beta[,takelam])
            betaPost[pen] <- c(sqrt(abs(beta[pen]))) * betaPost[pen]
            whichPost <- which(betaPost!=0 & ((1:p)%in%pen))
            a0 <- glmPost$a0[takelam]
          }else{
            lam1 <- uniroot(fsel,maxselec=x, interval=range(c(glmPost$lambda,c(0, max(lambdap[lambdap<Inf],na.rm=TRUE)*sigmahat/n))),
                            maxiter = 200,tol=10^(-10))$root
            glmPost <- glmnet::glmnet(x=Xacc[,nonzeros],y=Ygamma,alpha=1,
                              #lambda = lam1,
                              family="gaussian",
                              intercept = intrcpt, standardize = FALSE,
                              penalty.factor=penfctr[nonzeros])
            betaPost <- rep(0,p)
            betaPost[nonzeros]<- coef(glmPost, s=lam1, exact=TRUE, x=Xacc[,nonzeros],y=Ygamma,
                                      penalty.factor=penfctr[nonzeros],intercept= intrcpt,thresh = 10^-10)[-1]
            betaPost[pen] <- c(sqrt(abs(beta[pen]))) * betaPost[pen]
            whichPost <- which(betaPost!=0 & ((1:p)%in%pen))
            a0<- coef(glmPost, s=lam1, exact=TRUE, x=Xacc[,nonzeros],y=Ygamma,
                      penalty.factor=penfctr[nonzeros],intercept= intrcpt,thresh = 10^-10)[1]
          }
          
          output<-list()
          output$betaPost <- betaPost #post-selection beta using Elastic Net
          output$whichPost <- whichPost #index of selected covariates
          output$a0 <- a0
        
          if(!all(is.null(X2))){
            if(model=="linear"){
              YpredPost <- X2 %*% c(betaPost) + output$a0
              MSEPost<- sum((YpredPost-Y2)^2)/length(Y2)
            } 
            if(model=='logistic'){
              YpredPost <- 1/(1+exp(-(X2 %*% c(betaPost) + output$a0)))
              MSEPost<- sum((YpredPost-Y2)^2)/length(Y2)
              if(any(is.nan(YpredPost))){browser()}
            }else if(model=='cox'){ 
              expXbPost<-exp(X %*% c(betaPost))
              h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXbPost[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
              H0 <- sapply(sort(c(Y[,1],Y2[,1])),function(Ti){sum(h0[Y[,1]<=Ti])})
              YpredPost <- outer(c(exp(X2 %*% betaPost)),c(H0))
              colnames(YpredPost)<-paste("Time",signif(sort(c(Y[,1],Y2[,1]))),6)  
              MSEPost<- NaN #sum((YpredPost-Y2[,2])^2)/length(Y2[,2])
            }
            
            output$MSEPost <- MSEPost #MSE on independent data set (if given)
            output$YpredPost <- YpredPost #predictions for independent data set (if given)
          }
          return(output)
        }
    })
  }else if(grepl("BR",postselection)){
    #Bondell & Reich propose two options:
    #1. (joint): joint credible set approach using posterior covariance matrix
    #2. (marginal): marginal credible set approach using posterior standard deviations
    #After selecting, predict with following options:
    #1. (same): Keep output selection procedure
    #2. (dense): Refit with previously estimated weighted ridge
    #3. (sparse): Refit using ordinary ridge with newly cross-validated lambda 
    if(grepl("joint",postselection)){
      #joint credible set approach
      if(!silent) print("Bondell&Reich joint credible set used to select covariates")
      
      ind <- which(penfctr!=0 & penalties!=Inf) #index of beta that are penalised and not already set to 0
      #compute prediction weight matrix W
      if(model=="linear"){
        W<-diag(rep(1,n))
      }else if(model=="logistic"){
        expminXb<-exp(-X%*%c(beta) - intrcpt)
        Pi<-1/(1+expminXb)
        W<-diag(c(sqrt(Pi*(1-Pi))))
      }else if(model=="cox"){
        expXb<-exp(X%*%c(beta))
        h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXb[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
        H0 <- sapply(Y[,1],function(Ti){sum(h0[Y[,1]<=Ti])})
        W <- diag(c(sqrt(H0*expXb)))
      }
      
      Sigminhalf<-expm::sqrtm(t(X[,ind])%*%W%*%X[,ind]+diag(lambdap[ind])) / sqrt(sigmahat) #posterior covariance matrix Sigma, Sigma^{-0.5}
      #D<- diag(beta[ind]^2) #diagonal matrix with beta_k^2 on the diagonal
      
      Xstar <- t(t(Sigminhalf) * beta[ind]^2) #Sigma^{-0.5} D
      Ystar <- Sigminhalf %*% beta[ind] 
      
      #first fit for range of lambda 
      glmPost <- glmnet::glmnet(Xstar,Ystar,alpha=1,family="gaussian",
                      standardize = FALSE,intercept= FALSE)
     
      output<-lapply(maxsel2,function(x){
        fsel <- function(lambda,maxselec = x){
          fit<-glmnet::coef.glmnet(glmPost,s=lambda,exact=TRUE,x=Xstar,y=Ystar,thresh = 10^-10)
          return(sum(fit!=0)- maxselec) #number of non-zero penalised covariates
        }
        lambda <- uniroot(fsel, interval = c(0, max(glmPost$lambda)), maxiter = 200,tol=10^(-10))$root
        fit<-glmnet::coef.glmnet(glmPost,s=lambda,exact=TRUE,x=Xstar,y=Ystar,thresh = 10^-10)
      
        if(grepl("same",postselection)){
          if(x==maxsel2[1]) if(!silent) print("Selected covariates are not refit, unpenalised covariates are kept the same")
          betaPost <- beta
          betaPost[ind] <- fit*beta[ind]^2
          whichPost <- which(betaPost!=0 & penfctr!=0) #index of selected penalised covariates 
          output<-list()
          output$betaPost <- betaPost #post-selection beta using Elastic Net
          output$whichPost <- whichPost #index of selected covariates
          output$a0 <- intrcpt
          
          if(!all(is.null(X2))){
            if(model=="linear"){
              YpredPost <- X2 %*% c(betaPost) + intrcpt
              MSEPost <- sum((YpredPost-Y2)^2)/length(Y2)
            } 
            if(model=='logistic'){
              YpredPost <- 1/(1+exp(-(X2 %*% c(betaPost) + intrcpt)))
              MSEPost <- sum((YpredPost-Y2)^2)/length(Y2)
              if(any(is.nan(YpredPost))){browser()}
            }else if(model=='cox'){ 
              expXbPost<-exp(X %*% c(betaPost))
              h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXbPost[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
              H0 <- sapply(sort(c(Y[,1],Y2[,1])),function(Ti){sum(h0[Y[,1]<=Ti])})
              YpredPost <- outer(c(exp(X2 %*% betaPost)),c(H0))
              colnames(YpredPost)<-paste("Time",signif(sort(c(Y[,1],Y2[,1]))),6)  
              MSEPost<- NaN #sum((YpredPost-Y2[,2])^2)/length(Y2[,2])
            }
          
            output$MSEPost <- MSEPost #MSE on independent data set (if given)
            output$YpredPost <- YpredPost #predictions for independent data set (if given)
          }
          return(output)
          }else if(grepl("sparse",postselection)){
            if(!silent) print("Selected covariates are refit with previously estimated weighted ridge prior")
            
          }else{# if(grepl("dense",postselection)){
            if(!silent) print("Selected covariates are refit with an ordinary ridge prior using newly cross-validated penalty")
            
          }
        })
    }else{ #if(grepl("marginal")){
      #marginal credible set approach
      if(!silent) print("Bondell&Reich marginal credible set used to select covariates")
      
      ind <- which(penfctr!=0 & beta!=0) #index of betas that are penalised and not already set to 0
      indpen <- which((beta[penfctr!=0])!=0) #index in 1:length(pen) of betas that are penalise and not yet set to 0
      #compute prediction weight matrix W
      if(model=="linear"){
        diagWhalf<-rep(1,n)
      }else if(model=="logistic"){
        expminXb<-exp(-X%*%c(beta) - intrcpt)
        Pi<-1/(1+expminXb)
        diagWhalf<-c(Pi*(1-Pi)) #W^0.5
      }else if(model=="cox"){
        expXb<-exp(X%*%c(beta))
        h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXb[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
        H0 <- sapply(Y[,1],function(Ti){sum(h0[Y[,1]<=Ti])})
        diagWhalf <- c(H0*expXb)
      }
      Xtilde <- t(t(diagWhalf * X[,ind]) / sqrt(lambdap[ind]))
      svdXtilde <- svd(Xtilde)
      sdBetas <- 1/sqrt(lambdap[ind]) * sqrt(1-diag(svdXtilde$v %*% (1/(1+1/svdXtilde$d^2) * t(svdXtilde$v))))
      s <- sdBetas/min(sdBetas) #covariate specific posterior sd divided by minimum sd
      
      output<-lapply(maxsel2,function(x){
        fsel <- function(t,maxselec = x){
          An <- which(abs(beta[ind]) > t*s) #marginal posterior credible set selection rule for some t
          
          return(length(An)- maxselec) #number of non-zero penalised covariates
        }
        maxT <- max(abs(beta[ind])/s)*2
        t <- uniroot(fsel, interval = c(0, maxT), maxiter = 200,tol=10^(-20))$root
        indPost <- ind[which(abs(beta[ind]) > t*s)] #index of selected penalised covariates
        indPostpen <- which(which(penfctr!=0) %in% indPost) #index in 1:length(pen) of selected penalised covariates
        indAll <- c(indPost,which(penfctr==0)) #index of selected penalised covariates and unpenalised covariates

        
        if(grepl("dense",postselection)){
          if(x==maxsel2[1]) if(!silent) print("Selected covariates are refit with previously estimated weighted ridge prior")
          
          lambdaoverall <- sigmahat/tauglobal
          lam2 <- sigmahat/tauglobal/n*sd_y
          #Recompute beta using only selected beta and group ridge penalty (without lasso penalty)
          offset <- rep(0,n)
          if(length(muhatp)==1) muhatp <- rep(muhatp,p)
          if(!all(muhatp==0)) offset <- X[,indAll, drop=FALSE] %*% muhatp[indAll, drop=FALSE]
          Xacc <- X
          Xacc[,pen] <- as.matrix(X[,pen] %*% Matrix::sparseMatrix(i=1:length(lambdap[pen]),j=1:length(lambdap[pen]),
                                                           x=c(1/sqrt(lambdap[pen]/lambdaoverall))) )

          if(model=="cox"){
            glmPost <- glmnet::glmnet(Xacc[,indAll, drop=FALSE],as.matrix(Y),alpha=0,
                            #lambda = lam2,
                            family=fml,
                            offset = offset ,standardize = FALSE,
                            penalty.factor=penfctr[indAll])
            betaPost <- rep(0,p)
            betaPost[indAll] <- coef(glmPost, s=lam2, exact=TRUE, 
                                     x=Xacc[,indAll, drop=FALSE],y=as.matrix(Y),
                                     offset = offset, penalty.factor=penfctr[indAll],family=fml,thresh = 10^-10)
            betaPost[indPost] <- c(1/sqrt(lambdap[indPost]/lambdaoverall)) * betaPost[indPost] + muhatp[indPost]
          }else{
            glmPost <- glmnet::glmnet(Xacc[,indAll, drop=FALSE],Y,alpha=0,
                            #lambda = lam2,
                            family=fml,
                            offset = offset , intercept = intrcpt, standardize = FALSE,
                            penalty.factor=penfctr[indAll])
            betaPost <- rep(0,p)
            betaPost[indAll] <- coef(glmPost, s=lam2, exact=TRUE,
                                     x=Xacc[,indAll, drop=FALSE], y=Y,
                                     offset = offset, penalty.factor=penfctr[indAll],
                                     family=fml,intercept= intrcpt,thresh = 10^-10)[-1]
            glmPost$a0 <- coef(glmPost, s=lam2, exact=TRUE,
                               x=Xacc[,indAll, drop=FALSE], y=Y,
                               offset = offset, penalty.factor=penfctr[indAll],
                               family=fml,intercept= intrcpt,thresh = 10^-10)[1]
            betaPost[indPost] <- c(1/sqrt(lambdap[indPost]/lambdaoverall)) * betaPost[indPost] + muhatp[indPost]
          }
          # betaPost <- rep(0,p)
          # betaPost[indAll] <- as.vector(glmPost$beta)
          # betaPost[indPost] <- c(1/sqrt(lambdap[indPost]/lambdaoverall)) * betaPost[indPost] + muhatp[indPost]
        }else{ #sparse; refit with newly cross-validated lambda
          if(model=="cox"){
            lambdaGLM<-glmnet::cv.glmnet(X[,indAll, drop=FALSE],as.matrix(Y),alpha=0,family=fml,
                                 standardize = FALSE,penalty.factor=penfctr[indAll]) #alpha=0 for ridge
            lam2<-lambdaGLM$lambda.min
          }else{
            lambdaGLM<-glmnet::cv.glmnet(X[,indAll, drop=FALSE],Y,alpha=0,family=fml,
                                 standardize = FALSE,penalty.factor=penfctr[indAll]) #alpha=0 for ridge
            lam2<-lambdaGLM$lambda.min
          }
          if(model=="cox"){
            glmPost <- glmnet::glmnet(X[,indAll, drop=FALSE],as.matrix(Y),alpha=0,
                            #lambda = lam2,
                            family=fml,
                            offset = offset ,standardize = FALSE,
                            penalty.factor=penfctr[indAll])
            betaPost <- rep(0,p)
            betaPost[indAll] <- coef(glmPost, s=lam2, exact=TRUE,
                                     x=X[,indAll, drop=FALSE],y=as.matrix(Y),
                                     offset = offset, penalty.factor=penfctr[indAll],
                                     family=fml,thresh = 10^-10)
          }else{
            glmPost <- glmnet::glmnet(X[,indAll, drop=FALSE],Y,alpha=0,
                            #lambda = lam2,
                            family=fml,
                            intercept = intrcpt, standardize = FALSE,
                            penalty.factor=penfctr[indAll])
            betaPost <- rep(0,p)
            betaPost[indAll] <- coef(glmPost, s=lam2, exact=TRUE,
                                     x=X[,indAll, drop=FALSE],y=Y,
                                     penalty.factor=penfctr[indAll],
                                     family=fml,intercept= intrcpt,thresh = 10^-10)[-1]
            glmPost$a0 <- coef(glmPost, s=lam2, exact=TRUE,
                               x=X[,indAll, drop=FALSE],y=Y,
                               penalty.factor=penfctr[indAll],
                               family=fml,intercept= intrcpt,thresh = 10^-10)[1]
          }
          # betaPost <- rep(0,p)
          # betaPost[indAll] <- as.vector(glmPost$beta)
        }
        
        whichPost <- indPost
        output<-list()
        output$betaPost <- betaPost #post-selection beta using Elastic Net
        output$whichPost <- whichPost #index of selected covariates
        output$a0 <- glmPost$a0
        #output$offsetPost <- offset #offset used in Post

        if(!all(is.null(X2))){
          if(model=="linear"){
            YpredPost <- X2 %*% c(betaPost) + glmPost$a0
            MSEPost <- sum((YpredPost-Y2)^2)/length(Y2)
          } 
          if(model=='logistic'){
            YpredPost <- 1/(1+exp(-(X2 %*% c(betaPost) + glmPost$a0)))
            MSEPost<- sum((YpredPost-Y2)^2)/length(Y2)
            if(any(is.nan(YpredPost))){browser()}
          }else if(model=='cox'){ 
            expXbPost<-exp(X %*% c(betaPost))
            h0 <- sapply(1:length(Y[,1]),function(i){Y[i,2]/sum(expXbPost[Y[,1]>=Y[i,1]])})#updated baseline hazard in censored times for left out samples
            H0 <- sapply(sort(c(Y[,1],Y2[,1])),function(Ti){sum(h0[Y[,1]<=Ti])})
            YpredPost <- outer(c(exp(X2 %*% betaPost)),c(H0))
            colnames(YpredPost)<-paste("Time",signif(sort(c(Y[,1],Y2[,1]))),6)  
            MSEPost<- NaN #sum((YpredPost-Y2[,2])^2)/length(Y2[,2])
          }
          
          output$MSEPost <- MSEPost #MSE on independent data set (if given)
          output$YpredPost <- YpredPost #predictions for independent data set (if given)
        }
        
        return(output)
      })
      
    }
    
  }
  #reshape output data
  res<-list()
  size <- length(maxsel2)
  for(attr in names(output[[1]])){
    if(model=="cox" & attr=="YpredPost"){
      res[[attr]] <- lapply(1:size,function(i) output[[i]][[attr]])
      names(res[[attr]])<-paste("MaxSelec: ",maxsel2,sep="")
    }else if(attr != "whichPost"){
      res[[attr]] <- try(matrix(unlist(lapply(output,function(x){x[[attr]]})),
                                length(output[[1]][[attr]]),size),silent=TRUE)
      if(class(res[[attr]])[1]=="try-error") res[[attr]] <- NULL
      else colnames(res[[attr]])<-paste("MaxSelec: ",maxsel2,sep="")
    }  
  }
  return(res)
}

#Produce balanced folds----
produceFolds <- function(nsam,outerfold,response,model=c("logistic", "cox", "other"),
                         balance=TRUE){
  if(length(model)>1){
    if(all(is.element(response,c(0,1))) || is.factor(response)){
      model <- "logistic" 
    } else if(all(is.numeric(response)) & !(is.matrix(response) && dim(response)[2]>1)){
      model <- "linear"
    }else{
      model <- "cox"
    }
  }
  if(model=="linear") balance=FALSE
  if(!balance){
    rand<-sample(1:nsam)
    grs1 <- floor(nsam/outerfold)
    grs2 <- grs1+1
    ngr1 <- outerfold*grs2 - nsam
    folds <- lapply(1:outerfold,function(xg) {
      if(xg <= ngr1) els <- rand[(1+(xg-1)*grs1):(xg*grs1)] else els <- rand[(ngr1*grs1 + 1+(xg-ngr1-1)*grs2):(ngr1*grs1 + (xg-ngr1)*grs2)]
      return(els)
    }
    )} else {  #balanced folds
      if(model=="logistic") if(class(response)[1]=="factor") nev <- which((as.numeric(response)-1)==1) else nev <- which(response==1)  
      if(model=="cox") nev <- which(response[,2]==1)    
      nsamev <- length(nev) 
      randev<-sample(nev)
      grs1 <- floor(nsamev/outerfold)
      grs2 <- grs1+1
      ngr1 <- outerfold*grs2 - nsamev
      foldsev <- lapply(1:outerfold,function(xg) {
        if(xg <= ngr1) els <- randev[(1+(xg-1)*grs1):(xg*grs1)] else els <- randev[(ngr1*grs1 + 1+(xg-ngr1-1)*grs2):(ngr1*grs1 + (xg-ngr1)*grs2)]
        return(els)
      }
      )
      nonev <- setdiff(1:nsam,nev)
      nsamnonev <- length(nonev) 
      randnonev<-sample(nonev)
      grs1 <- floor(nsamnonev/outerfold)
      grs2 <- grs1+1
      ngr1 <- outerfold*grs2 - nsamnonev
      foldsnonev <- lapply(1:outerfold,function(xg) {
        if(xg <= ngr1) els <- randnonev[(1+(xg-1)*grs1):(xg*grs1)] else els <- randnonev[(ngr1*grs1 + 1+(xg-ngr1-1)*grs2):(ngr1*grs1 + (xg-ngr1)*grs2)]
        return(els)
      }
      )
      folds <- lapply(1:outerfold,function(i) c(foldsev[[i]],foldsnonev[[i]]))
    }
  return(folds)
}

#Create group set----
#This function is derived from CreatePartition from the GRridge package, available on Bioconductor
#Author: Mark van de Wiel
#The function name and some variable names are changed to match those in the ecpc-package

createGroupset <- function(values,index=NULL,grsize=NULL,ngroup=10,
                           decreasing=TRUE,uniform=FALSE,
                           minGroupSize = 50){
  #values <- 1:67;ngroup=10;index=NULL;grsize=NULL;decreasing=TRUE;uniform=TRUE;mingr=50
  #values <- sdsF;grsize=5000;decreasing=FALSE;uniform=TRUE
  if(is.factor(values)){
    firstcl <- lapply(as.character(levels(values)),function(xg) which(values==xg))
    names(firstcl) <- levels(values)
  }else if(is.logical(values)){
    firstcl <- lapply(c(TRUE,FALSE),function(x) which(values==x))
    names(firstcl) <- c("True","False")
  } else {
    if(is.numeric(values)){
      if(uniform){
        if(is.null(grsize)){
          grsize <- floor(length(values)/ngroup)
          print(paste("Group size set to:",grsize))
        } else {
          print(paste("Group size",grsize))
        }
        if(decreasing) {
          print("Sorting values in decreasing order, assuming small values are LESS relevant")
          orderp2 <- order(values,decreasing=TRUE) 
          lroep <- length(values)
        } else {
          print("Sorting values in increasing order, assuming small values are MORE relevant")
          orderp2 <- order(values,decreasing=FALSE) 
          lroep <- length(values)   
        }
        if(!is.null(grsize)){
          ngr <- floor(lroep/grsize)
          firstcl <- lapply(1:ngr,function(xg) {
            if(xg < ngr) els <- orderp2[(1+(xg-1)*grsize):(xg*grsize)] else 
              els <- orderp2[(1+(xg-1)*grsize):lroep]
            return(els)
          }
          )
        } else {
          ngr <- ngroup
          remain <- length(values) %% ngroup
          firstcl <- lapply(1:ngr,function(xg) {
            if(xg <= remain) els <- orderp2[(1+(xg-1)*(grsize+1)):(xg*(grsize+1))] else 
              els <- orderp2[(1+(xg-1-remain)*grsize+remain*(grsize+1)):((xg-remain)*grsize+remain*(grsize+1))]
            return(els)
          }
          ) 
          
        }  
        names(firstcl) <- sapply(1:length(firstcl),function(i) paste("group",i,sep=""))
      } else {
        if(decreasing) {
          print("Sorting values in decreasing order, assuming small values are LESS relevant")
          orderp2 <- order(values,decreasing=TRUE) 
          lroep <- length(values)
        } else {
          print("Sorting values in increasing order, assuming small values are MORE relevant")
          orderp2 <- order(values,decreasing=FALSE) 
          lroep <- length(values)   
        }
        p <- length(values) 
        if(ngroup*minGroupSize >= p) {
          print("ERROR: Number of groups (ngroup) times minimal group size (minGroupSize) is larger 
          than number of variables. Please use uniform = TRUE or decrease either ngroup or minGroupSize.")
          return(NULL)  
        }
        povermin <- p/minGroupSize
        parint <-povermin^{1/ngroup}
        
        lefts <- povermin+1
        gfun2 <- function(x){1-x^(ngroup+1) - lefts*(1-x)}
        root <- uniroot(f=gfun2, lower=1.000001,upper=parint)$root
        
        grs <- sapply(1:ngroup,function(i) if(i==1) floor(minGroupSize*root^i) else round(minGroupSize*root^i)) 
        sm <- sum(grs)
        grs[ngroup] <- grs[ngroup] -(sm-p)
        print("Summary of group sizes:")
        print(summary(grs))
        cumul <- cumsum(c(0,grs))
        firstcl <- lapply(1:ngroup,function(xg) {
          els <- orderp2[(cumul[xg]+1):cumul[xg+1]]
          return(els)
        }
        )
        names(firstcl) <- sapply(1:length(firstcl),function(i) paste("group",i,sep=""))
      }
    } else {  #assume character
      if(!is.character(values)){
        print("Argument values is not correctly specified")
        return(NULL)
      } else {
        firstcl <- lapply(unique(values),function(x) which(values==x))
        names(firstcl) <- unique(values)
      }
    }
  }
  if(!is.null(index)){ #remapping 
    if(length(values) != length(index)){
      print("ERROR: Length of values does not match length of index")
      return(NULL)
    } else {
      firstcl <- lapply(firstcl,function(vector) index[vector])
    }
  }
  print("Summary of group sizes:")
  print(unlist(lapply(firstcl,length)))
  return(firstcl)
}


#Estimate maximum marginal likelihood estimates for linear model----
.mlestlin <- function(Y,XXt,Xrowsum,Xunpen=NULL,lambda=NaN,sigmasq=NaN,mu=NaN,tausq=NaN,intrcpt=TRUE){
  #lambda,sigmasq,mu are possibly fixed
  maxv <- var(Y)
  #if(intrcpt) Y <- Y - mean(Y)
  
  #p<-dim(X)[2]
  n<-length(Y)
  
  prms <- paste(c("l","s","m","t")[is.nan(c(lambda,sigmasq,mu,tausq))],collapse='') #unknown parameters: l lambda, s sigma, m mu
  if(prms=='t'||prms=='mt'){tausq <- sigmasq/lambda; prms <- gsub("t","",prms)}
  if(prms=='l'||prms=='lm'){lambda <- sigmasq/tausq; prms <- gsub("l","",prms)}
  if(prms=='s'||prms=='sm'){sigmasq <- lambda*tausq; prms <- gsub("s","",prms)}
  switch(prms,
         'lsmt'={ #lambda, sigma, mu, tau unknown
           #TD: add unpenalised covariates
           sim2 = function(ts){
             tausq<-ts[1];sigmasq<-ts[2];mu<-ts[3]
             varY <- XXt * exp(tausq) + diag(rep(1,n))*exp(sigmasq)
             meanY <- mu*Xrowsum 
             mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE)
             return(mlk)
           }
           op <- optim(c(log(0.01),log(maxv),0),sim2)
           tausq <- exp(op$par[1]); sigmasq <- exp(op$par[2])
           mu <- op$par[3]; lambda <- sigmasq/tausq
         },
         'lsm'={ #lambda, sigma, mu unknown, tau known
           #TD: add unpenalised covariates
           sim2 = function(ts){
             sigmasq<-ts[1];mu<-ts[2]
             varY <- XXt * exp(tausq) + diag(rep(1,n))*exp(sigmasq)
             meanY <- mu*Xrowsum #X%*%rep(mu,p)
             mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE)
             return(mlk)
           }
           op <- optim(c(log(maxv),0),sim2)
           sigmasq <- exp(op$par[1])
           mu <- op$par[2]; lambda <- sigmasq/tausq
         },
         'lst'={ #lambda, sigma, tau unknown, mu known
           minsig <- 0#10^-5*maxv
           sim2 = function(ts){
             tausq<-exp(ts[1]);sigmasq<- minsig+exp(ts[2])
             varY <- XXt * tausq + diag(rep(1,n))*sigmasq
             meanY <- mu*Xrowsum

             #compute unpenalised variable estimates given lambda, sigma, tausq
             #add this to meanY
             if(length(dim(Xunpen))>0 | intrcpt){
               XtDinvX <- multiridge::SigmaFromBlocks(XXblocks = list(XXt),sigmasq/tausq)
               if(intrcpt) Xunpen <- cbind(Xunpen,rep(1,n))
               if(intrcpt && dim(Xunpen)[2]==1){
                 betaunpenML <- sum(Y)/n
               }else{
                 temp <- solve(XtDinvX+diag(rep(1,n)),Xunpen)
                 betaunpenML <- solve(t(Xunpen)%*%temp , t(temp)%*%Y)
               }
               meanY <- meanY + Xunpen%*%betaunpenML
             }

             mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE)
             return(mlk)
           }
           op <- optim(c(log(0.01),log(maxv)),sim2)
           tausq <- exp(op$par[1]); sigmasq <- minsig + exp(op$par[2])
           lambda <- sigmasq/tausq
           
           #when sigma is too small, set estimate to be the largest that is 
           #still close to the minimum value
           if(sigmasq < 10^-4*maxv){
             lb <- sim2(log(c(tausq,10^-6)))
             ub <- sim2(log(c(tausq,2*maxv)))
             abstol <- 10^-3*abs(ub-lb)
             froot <- function(sigmasq){
               sim2(log(c(tausq,sigmasq))) - op$value - abstol
             }
             sigmasq <- uniroot(froot,c(sigmasq,2*maxv))$root
             lambda <- sigmasq/tausq
           }
           
           # #optimise over lambda, and sigmahat given lambda
           # minlam <- 10^-3
           # sim2 = function(ts){
           #   lambda<-minlam + exp(ts[1])
           #   
           #   meanY <- mu*Xrowsum 
           #   
           #   #MMLE sigma known analytically given lambda
           #   if(length(dim(Xunpen))>0 | intrcpt){
           #     XtDinvX <- multiridge::SigmaFromBlocks(XXblocks = list(XXt),lambda)
           #     if(intrcpt) Xunpen <- cbind(Xunpen,rep(1,n))
           #     if(intrcpt && dim(Xunpen)[2]==1){
           #       betaunpenML <- sum(Y)/n
           #     }else{
           #       temp <- solve(XtDinvX+diag(rep(1,n)),Xunpen)
           #       betaunpenML <- solve(t(Xunpen)%*%temp , t(temp)%*%Y)
           #     }
           #     sigmasq <- c(t(Y-Xunpen%*%betaunpenML)%*%solve(XtDinvX+diag(rep(1,n)),Y-Xunpen%*%betaunpenML)/n)
           #     meanY <- meanY + Xunpen%*%betaunpenML
           #   }else{
           #     sigmasq <- c(t(Y)%*%solve(XtDinvX+diag(rep(1,n)),Y)/n)
           #   }
           #   tausq <- sigmasq/lambda
           #   varY <- XXt * tausq + diag(rep(1,n))*sigmasq
           #   
           #   mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE)
           #   return(mlk)
           # }
           # op <- optim(c(log(0.01)),sim2,method="Brent",lower=log(1e-5),upper=log(10^6))
           # lambda <- minlam + exp(op$par[1])
           # #MMLE sigma known analytically given lambda
           # if(length(dim(Xunpen))>0 | intrcpt){
           #   XtDinvX <- multiridge::SigmaFromBlocks(XXblocks = list(XXt),lambda)
           #   if(intrcpt) Xunpen <- cbind(Xunpen,rep(1,n))
           #   if(intrcpt && dim(Xunpen)[2]==1){
           #     betaunpenML <- sum(Y)/n
           #   }else{
           #     temp <- solve(XtDinvX+diag(rep(1,n)),Xunpen)
           #     betaunpenML <- solve(t(Xunpen)%*%temp , t(temp)%*%Y)
           #   }
           #   sigmasq <- c(t(Y-Xunpen%*%betaunpenML)%*%solve(XtDinvX+diag(rep(1,n)),Y-Xunpen%*%betaunpenML)/n)
           # }else{
           #   sigmasq <- c(t(Y)%*%solve(XtDinvX+diag(rep(1,n)),Y)/n)
           # }
           # tausq <- sigmasq/lambda
           
           # lambdaseq <- 10^seq(-5,6,length.out=30)
           # MLs <- sapply(log(lambdaseq),sim2)
           # plot(log(lambdaseq),MLs)
           # abline(v=log(lambda),col="red")
         },
         'lmt'={ #lambda, tau, mu unknown, sigma known
           #TD: add unpenalised variables
           sim2 = function(ts){
             tausq<-ts[1]; mu <- ts[2]
             varY <- XXt * exp(tausq) + diag(rep(1,n))*sigmasq
             meanY <- mu*Xrowsum 
             mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE)
             return(mlk)
           }
           op <- optim(c(log(0.01),0),sim2)
           tausq <- exp(op$par[1])
           mu <- op$par[2]; lambda <- sigmasq/tausq
         },
         'lt'={ #lambda, tau unknown, sigma, mu known
           sim2 = function(ts){
             tausq<-exp(ts[1]);
             varY <- XXt * tausq + diag(rep(1,n))*sigmasq
             meanY <- mu*Xrowsum 
             
             #compute unpenalised variable estimates given lambda, sigma, tausq
             #add this to meanY
             if(length(dim(Xunpen))>0 | intrcpt){
               XtDinvX <- multiridge::SigmaFromBlocks(XXblocks = list(XXt),sigmasq/tausq)
               if(intrcpt) Xunpen <- cbind(Xunpen,rep(1,n))
               if(intrcpt && dim(Xunpen)[2]==1){
                 betaunpenML <- sum(Y)/n
               }else{
                 temp <- solve(XtDinvX+diag(rep(1,n)),Xunpen)
                 betaunpenML <- solve(t(Xunpen)%*%temp , t(temp)%*%Y)
               }
               meanY <- meanY + Xunpen%*%betaunpenML
             }
             
             mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE)
             return(mlk)
           }
           op <- optim(c(log(0.01)),sim2,method="Brent",lower=log(1e-5),upper=log(10^6))
           #op <- optimize(sim2,c(-100,100))
           #op <- optim(c(log(0.01)),sim2)
           #browser()
           tausq <- exp(op$par[1])
           lambda <- sigmasq/tausq
           
           # sigmarange <- 10^seq(-5,5,length.out=20)
           # taurange <- sapply(sigmarange, function(x){
           #   sigmasq <- x
           #   sim2 = function(ts){
           #     tausq<-exp(ts[1]);
           #     varY <- XXt * tausq + diag(rep(1,n))*sigmasq
           #     meanY <- mu*Xrowsum 
           #     
           #     #compute unpenalised variable estimates given lambda, sigma, tausq
           #     #add this to meanY
           #     if(length(dim(Xunpen))>0 | intrcpt){
           #       XtDinvX <- multiridge::SigmaFromBlocks(XXblocks = list(XXt),sigmasq/tausq)
           #       if(intrcpt) Xunpen <- cbind(Xunpen,rep(1,n))
           #       if(intrcpt && dim(Xunpen)[2]==1){
           #         betaunpenML <- sum(Y)/n
           #       }else{
           #         temp <- solve(XtDinvX+diag(rep(1,n)),Xunpen)
           #         betaunpenML <- solve(t(Xunpen)%*%temp , t(temp)%*%Y)
           #       }
           #       meanY <- meanY + Xunpen%*%betaunpenML
           #     }
           #     
           #     mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE)
           #     return(mlk)
           #   }
           #   op <- optim(c(log(0.01)),sim2,method="Brent",lower=log(1e-5),upper=log(10^6))
           #   tausq <- exp(op$par[1])
           #   return(tausq)
           # })
           # lambda <- sigmarange/taurange
         },
         'smt'={ #sigma, tau, mu unknown, lambda known
           sim2 = function(ts){
             tausq<-log(exp(ts[1])/lambda); sigmasq<-ts[1]; mu<-ts[2]
             varY <- XXt * exp(tausq) + diag(rep(1,n))*exp(sigmasq)
             meanY <- mu*Xrowsum 
             mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE)
             return(mlk)
           }
           op <- optim(c(log(maxv),0),sim2,method="L-BFGS-B",lower=c(log(1e-10),-10^{99}),upper=c(log(2*maxv),10^{99}))
           tausq <- exp(op$par[1])/lambda; sigmasq <- exp(op$par[1])
           mu <- op$par[2]
           #or for small n
           #sigmasq <- sum(diag(solve(X%*%t(X)+lambda*diag(n),lambda*Y%*%t(Y))))/n
           #mlests <- c(sigmasq,sigmasq/lambda)
         },
         'st'={ #sigma, tau unknown, lambda, mu known
           #MMLE sigma known analytically
           XtDinvX <- multiridge::SigmaFromBlocks(XXblocks = list(XXt),lambda)
           if(length(dim(Xunpen))>0 | intrcpt){
             if(intrcpt) Xunpen <- cbind(Xunpen,rep(1,n))
             if(intrcpt && dim(Xunpen)[2]==1){
               betaunpenML <- sum(Y)/n
             }else{
               temp <- solve(XtDinvX+diag(rep(1,n)),Xunpen)
               betaunpenML <- solve(t(Xunpen)%*%temp , t(temp)%*%Y)
             }
             sigmasq <- c(t(Y-Xunpen%*%betaunpenML)%*%solve(XtDinvX+diag(rep(1,n)),Y-Xunpen%*%betaunpenML)/n)
           }else{
             sigmasq <- c(t(Y)%*%solve(XtDinvX+diag(rep(1,n)),Y)/n)
           }
           tausq <- sigmasq/lambda
           
           # #or minimise numerically:
           # sim2 = function(ts){
           #   tausq<-exp(ts)/lambda; sigmasq<-exp(ts)
           #   varY <- XXt * tausq + diag(rep(1,n))*sigmasq
           #   meanY <- mu*Xrowsum 
           #   
           #   #compute unpenalised variable estimates given lambda, sigma, tausq
           #   #add this to meanY
           #   if(length(dim(Xunpen))>0 | intrcpt){
           #     XtDinvX <- multiridge::SigmaFromBlocks(XXblocks = list(XXt),lambda)
           #     if(intrcpt) Xunpen <- cbind(Xunpen,rep(1,n))
           #     if(intrcpt && dim(Xunpen)[2]==1){
           #       betaunpenML <- sum(Y)/n
           #     }else{
           #       temp <- solve(XtDinvX+diag(rep(1,n)),Xunpen)
           #       betaunpenML <- solve(t(Xunpen)%*%temp , t(temp)%*%Y)
           #     }
           #     meanY <- meanY + Xunpen%*%betaunpenML
           #   }
           #   
           #   mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE)
           #   return(mlk)
           # }
           # op <- optim(c(log(maxv)),sim2,method="Brent",lower=log(1e-5),upper=log(2*maxv))
           # tausq <- exp(op$par[1])/lambda; sigmasq <- exp(op$par[1])
           
           # sigmaseq <- 10^seq(-1,log10(5*maxv),length.out=30)
           # MLs <- sapply(log(sigmaseq),sim2)
           # plot(log(sigmaseq),MLs)
           # abline(v=log(sigmasq),col="red")
         },
         'ls'={ #lambda, sigma unknown, tau, mu known
           sim2 = function(ts){
             sigmasq<-exp(ts[1]);
             varY <- XXt * tausq + diag(rep(1,n))*sigmasq
             meanY <- mu*Xrowsum 
             
             #compute unpenalised variable estimates given lambda, sigma, tausq
             #add this to meanY
             if(length(dim(Xunpen))>0 | intrcpt){
               XtDinvX <- multiridge::SigmaFromBlocks(XXblocks = list(XXt),sigmasq/tausq)
               if(intrcpt) Xunpen <- cbind(Xunpen,rep(1,n))
               if(intrcpt && dim(Xunpen)[2]==1){
                 betaunpenML <- sum(Y)/n
               }else{
                 temp <- solve(XtDinvX+diag(rep(1,n)),Xunpen)
                 betaunpenML <- solve(t(Xunpen)%*%temp , t(temp)%*%Y)
               }
               meanY <- meanY + Xunpen%*%betaunpenML
             }
             
             mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE) 
             return(mlk)
           }
           op <- optim(c(log(maxv)),sim2,method="Brent",lower=log(1e-6),upper=log(2*maxv))
           sigmasq <- exp(op$par[1])
           lambda <- sigmasq/tausq
           
           #when sigma is too small, set estimate to be the largest that is 
           #still close to the minimum value
           if(sigmasq < 10^-4*maxv){
             lb <- sim2(log(10^-6))
             ub <- sim2(log(2*maxv))
             abstol <- 10^-3*abs(ub-lb)
             froot <- function(sigmasq){
               sim2(log(sigmasq)) - op$value - abstol
             }
             sigmasq <- uniroot(froot,c(sigmasq,2*maxv))$root
             lambda <- sigmasq/tausq

             # sigmarange <- exp(seq(-5,5,length.out=20))
             # ML <- sapply(log(sigmarange),sim2)
             # plot(log(sigmarange),ML)
             # abline(v=log(sigmasq),col="red")
           }
           
         },
         'm'={ #mean unknown, lambda, sigma, tau known
           sim2 = function(ts){
             mu<-ts
             varY <- XXt * tausq + diag(rep(1,n))*sigmasq
             meanY <- mu*Xrowsum 
             mlk <- -mvtnorm::dmvnorm(c(Y),mean=meanY,sigma=varY,log=TRUE)
             return(mlk)
           }
           op <- optim(0,sim2,method="Brent",lower=-10^{99},upper=10^{99})
           mu <- op$par[1]
         }
  )
  mlests <- c(lambda,sigmasq,mu,tausq)
  return(mlests)
}



#simulate data for linear or logistic----
simDat <- function(n,p,n2=20,muGrp,varGrp,indT,sigma=1,
                   model=c("linear", "logistic"),flag=FALSE){
  #Simulate data:
  #X~MVN(0,1)
  #beta_k~N(mu_{g(k)},sig_{g(k)}
  #Linear regression: Y~N(X*beta,sigma^2)
  #Logistic regression: Y~binom(expit(X*beta))
  #
  #Input:
  #n: number of observations
  #p: number of covariates
  #n2: number of observations of independent data sample
  #muGrp: prior mean of grouped betas
  #varGrp: variance of grouped betas
  #indT: px1 vector of group indices 1,..,G
  #model: generate data for "linear" or "logistic" setting
  #flag: True if plots and histograms of generated data have to be shown
  
  if(length(model)>1) model <- "linear"
  #generate expression data
  X <- mvtnorm::rmvnorm(n,rep(0,p),diag(rep(1,p))) #X~MVN(0,1)
  X2 <- mvtnorm::rmvnorm(n2,rep(0,p),diag(rep(1,p))) #X2 independent data
  
  #center X
  meansX<-apply(X,2,mean)
  Xctd <- t(t(X) - meansX) #center X
  meansX2<-apply(X2,2,mean)
  X2ctd <- t(t(X2) - meansX2) #center X
  beta <- c(mvtnorm::rmvnorm(1,muGrp[indT],
                  diag(varGrp[indT]))) #beta_k~N(mu_{g(k)},tau_{g(k)}^2)
  
  if(model=='linear'){
    Y <- Xctd %*% beta + rnorm(n,0,sd=sigma) #Y~N(X*beta,sigma^2)
    Y2 <- X2ctd %*% beta + rnorm(n2,0,sd=sigma) #Y~N(X*beta,sigma^2)
    
    if(flag){
      print(paste("Simulated data for",model,"regression"))
      #plot data
      plot(Y)
      points(Xctd %*% beta, col='red') #Y without the added noise
      legend('right',c('Y','X*beta'),pch=c(1,1),col=c('black','red'))
      hist(Y)
    }
    
  } 
  if(model=='logistic'){
    expXb <- exp(Xctd %*% beta)
    Y <-  rbinom(n,1,expXb/(1+expXb)) #Y~binom(expit(X*beta))
    expX2b <- exp(X2ctd %*% beta)
    Y2 <- rbinom(n2,1,expX2b/(1+expX2b))
    
    if(flag){
      print(paste("Simulated data for",model,"regression"))
      #plot data
      plot(Y)
      points(expXb/(1+expXb), col='red') #Y without the added noise
      legend('right',c('Y','expit(X*beta)'),pch=c(1,1),col=c('black','red'))
      hist(expXb/(1+expXb))
    }
  }
  
  return(
    list(
      beta=beta,
      Xctd=Xctd, 
      Y=Y,
      X2ctd=X2ctd,
      Y2=Y2
    )
  )
}

#Perform cross-validation----
cv.ecpc <- function(Y,X,type.measure=c("MSE", "AUC"),outerfolds=10,
                    lambdas=NULL,ncores=1,balance=TRUE,silent=FALSE,...){
  
  #initialise global variables for CMD check
  Method <- NULL; Fold <- NULL; AUC <- NULL; Ypred <- NULL; Truth <- NULL
  NumberSelectedVars <- NULL
  
  if(requireNamespace("magrittr")) `%>%` <- magrittr::`%>%`
  
  if(length(type.measure)>1) type.measure <- "MSE"
  ecpc.args <- list(...)
  if(!is.element("model",names(ecpc.args))){
    if(all(is.element(Y,c(0,1))) || is.factor(Y)){
      model <- "logistic" 
    } else if(all(is.numeric(Y)) & !(is.matrix(Y) && dim(Y)[2]==2)){
      model <- "linear"
    }else{
      model <- "cox"
    }
  }else{
    model <- ecpc.args$model; ecpc.args$model <- NULL
  }
  if(!is.element("postselection",names(ecpc.args))){
    postselection <- "elnet,dense"
  }else{
    postselection <- ecpc.args$postselection; ecpc.args$postselection <- NULL
  }
  if(!is.element("maxsel",names(ecpc.args))){
    maxsel <- 10
  }else{
    maxsel <- ecpc.args$maxsel; ecpc.args$maxsel <- NULL
  }
  
  n <- dim(X)[1]
  p <- dim(X)[2]
  if(is.numeric(outerfolds)){
    folds2<-produceFolds(n,outerfolds,Y,balance=balance,model=model) #produce folds balanced in response
  }else{
    folds2 <- outerfolds
  }
  nfolds <- length(folds2)
  if(length(lambdas)==0){
    lambdas <- rep(NaN,length(folds2))
  }
  
  grpsno <- c(unlist(sapply(ecpc.args$groupsets,function(x){1:length(x)}))) #vector with group numbers in all group sets
  grpngsno <- c(unlist(sapply(1:length(ecpc.args$groupsets),function(i){rep(i,length(ecpc.args$groupsets[[i]]))}))) #vector with group sets numbers
  dfGrps<-data.frame() #data frame in which group and group set weights are stored
  df<-data.frame() #data frame in which predicted values are stores for each of the samples in the left-out fold
  Res<-list() #list in which raw output of ecpc is stored (e.g. estimated regression coefficients)
  if(ncores>1){
    if(!(requireNamespace("doParallel")&requireNamespace("parallel")&requireNamespace("foreach"))){
      warning("Packages doParallel, parallel and foreach should be installed for parallel processing")
      print("ncores set to 1")
      ncores <- 1
      `%dopar%` <- foreach::`%dopar%`
    }
    cl <- parallel::makeCluster(ncores) #set up parallel cluster
    doParallel::registerDoParallel(cl)
    finalMatrix <- foreach::foreach(i=1:nfolds, .combine=rbind, 
                           .packages = c("glmnet","mvtnorm","gglasso",
                                         "Matrix","ecpc")) %dopar% {
         
         tic<-proc.time()[[3]]
         Res[[i]]<-do.call(ecpc,args=c(list(Y=Y[-folds2[[i]]],X=X[-folds2[[i]],],Y2=Y[folds2[[i]]],
                                            X2=X[folds2[[i]],],lambda=lambdas[i],postselection=postselection,
                                            maxsel = maxsel,model=model,silent=silent),ecpc.args))
         Res[[i]]$time <- proc.time()[[3]]-tic
         
         if(postselection!=FALSE){
           df2<-data.frame("Ypred"=c(Res[[i]]$Ypred,Res[[i]]$Ypredridge,c(Res[[i]]$YpredPost)))
           df2$Method <- rep(c("ecpc","ordinary.ridge",paste("ecpc",maxsel,"vars",sep="")),each=length(folds2[[i]]))
           df2$NumberSelectedVars <- rep(c(sum(Res[[i]]$beta!=0),p,maxsel),each=length(folds2[[i]]))
           df2$Fold <- i
           df2$Sample <- rep(folds2[[i]],2+length(maxsel))
           df2$Time <-  Res[[i]]$time
           df2$Truth <- rep(Y[folds2[[i]]],2+length(maxsel))
         }else{
           df2<-data.frame("Ypred"=c(Res[[i]]$Ypred,Res[[i]]$Ypredridge))
           df2$Method <- rep(c("ecpc","ordinary.ridge"),each=length(folds2[[i]]))
           df2$NumberSelectedVars <- rep(c(sum(Res[[i]]$beta!=0),p),each=length(folds2[[i]]))
           df2$Fold <- i
           df2$Sample <- rep(folds2[[i]],2)
           df2$Time <-  Res[[i]]$time
           df2$Truth <- rep(Y[folds2[[i]]],2)
         }
         
         df3<-data.frame("Group"=grpsno,
                         "Groupset"=grpngsno,
                         "Group weight"=Res[[i]]$gamma,
                         "Groupset weight"=Res[[i]]$w[grpngsno])
         df3$Tau.ecpc <- Res[[i]]$tauglobal #global tau^2
         df3$Tau.ridge <- 1/Res[[i]]$lambdaridge #ordinary ridge tau^2
         df3$Method <- "ecpc"
         df3$Fold <- i
         
         if(!silent) print(paste(Sys.time(),"fold",i,"of",nfolds,"done"))
         
         list("Res"=Res,"df"=df2,"dfGrps"=df3)
       }
    
    Res <- lapply(1:nfolds,function(i) finalMatrix[i,1][[1]][[i]])
    df2 <- lapply(1:nfolds,function(i) finalMatrix[i,2][[1]])
    dfGrps2 <- lapply(1:nfolds,function(i) finalMatrix[i,3][[1]])
    df <- df2[[1]]; for(i in 2:nfolds) df <- rbind(df,df2[[i]])
    dfGrps <- dfGrps2[[1]]; for(i in 2:nfolds) dfGrps <- rbind(dfGrps,dfGrps2[[i]])
    parallel::stopCluster(cl); rm(cl)
  }else{
    for(i in 1:nfolds){
       tic<-proc.time()[[3]]
       Res[[i]]<-do.call(ecpc,args=c(list(Y=Y[-folds2[[i]]],X=X[-folds2[[i]],],Y2=Y[folds2[[i]]],
                                          X2=X[folds2[[i]],],lambda=lambdas[i],postselection=postselection,
                                          maxsel = maxsel,model=model,silent=silent),ecpc.args))
       Res[[i]]$time <- proc.time()[[3]]-tic
       if(postselection!=FALSE){
         df2<-data.frame("Ypred"=c(Res[[i]]$Ypred,Res[[i]]$Ypredridge,c(Res[[i]]$YpredPost)))
         df2$Method <- rep(c("ecpc","ordinary.ridge",paste("ecpc",maxsel,"vars",sep="")),each=length(folds2[[i]]))
         df2$NumberSelectedVars <- rep(c(sum(Res[[i]]$beta!=0),p,maxsel),each=length(folds2[[i]]))
         df2$Fold <- i
         df2$Sample <- rep(folds2[[i]],2+length(maxsel))
         df2$Time <-  Res[[i]]$time
         df2$Truth <- rep(Y[folds2[[i]]],2+length(maxsel))
       }else{
         df2<-data.frame("Ypred"=c(Res[[i]]$Ypred,Res[[i]]$Ypredridge))
         df2$Method <- rep(c("ecpc","ordinary.ridge"),each=length(folds2[[i]]))
         df2$NumberSelectedVars <- rep(c(sum(Res[[i]]$beta!=0),p),each=length(folds2[[i]]))
         df2$Fold <- i
         df2$Sample <- rep(folds2[[i]],2)
         df2$Time <-  Res[[i]]$time
         df2$Truth <- rep(Y[folds2[[i]]],2)
       }
       df<-rbind(df,df2)
       
       df3<-data.frame("Group"=grpsno,
                       "Groupset"=grpngsno,
                       "Group weight"=Res[[i]]$gamma,
                       "Groupset weight"=Res[[i]]$w[grpngsno])
       df3$Tau.ecpc <- Res[[i]]$tauglobal #global tau^2
       df3$Tau.ridge <- 1/Res[[i]]$lambdaridge #ordinary ridge tau^2
       df3$Method <- "ecpc"
       df3$Fold <- i
       dfGrps<-rbind(dfGrps,df3)
       
       if(!silent) print(paste(Sys.time(),"fold",i,"of",nfolds,"done"))
    }
  }

  df$Method<-as.factor(df$Method)
  dfGrps$Group<-as.factor(dfGrps$Group)
  
  #data frame with performance measure
  if(is.factor(df$Truth)){
    warning("Response Y given as factor, transformed to numeric to compute AUC")
    if(!silent) print(levels(df$Truth)[1],"transformed to",0)
    if(!silent) print(levels(df$Truth)[2],"transformed to",1)
    df$Truth <- as.numeric(df$Truth)-1
  }
  if(type.measure=="MSE"){
    dfCVM <- df %>% dplyr::group_by(Method,Fold) %>% dplyr::summarise(CVM = mean((Ypred-Truth)^2),Type="MSE",
                                                   NumberSelectedVars=mean(NumberSelectedVars)) %>% dplyr::ungroup()
  }
  else if(type.measure=="AUC"){
    requireNamespace("pROC")
    
    # dfROC<-data.frame()
    # for(i in levels(df$Method)){
    #   temp<-data.frame()
    #   cutoffs<-rev(seq(0,1,by=0.001))
    #   rocGR <- roc(probs=df$Ypred[df$Method==i],true=df$Truth[df$Method==i],cutoffs=cutoffs)
    #   temp<-data.frame("FPR"=rocGR[1,],"TPR"=rocGR[2,],"Accuracy"=rocGR[3,])
    #   temp$Method <- i
    #   temp$AUC<-c(auc(rocGR))
    #   temp$NumberSelectedVars<-mean(df$NumberSelectedVars[df$Method==i])
    #   dfROC<-rbind(dfROC,temp)
    # }
    dfCVM<-data.frame()
    for(i in levels(df$Method)){
      temp<-data.frame("Method"=i)
      rocpROC <- pROC::roc(predictor=df$Ypred[df$Method==i],response=df$Truth[df$Method==i],
                         smooth=FALSE,auc=TRUE,levels=c(0,1),direction="<")
      temp$CVM<-rocpROC$auc[1]
      temp$NumberSelectedVars<-mean(df$NumberSelectedVars[df$Method==i])
      dfCVM<-rbind(dfCVM,temp)
    }
    dfCVM$Type <- "AUC"
    
  }else{
    warning(paste("The type of measure",type.measure,"is not yet supported."))
  }
  
  return(list(ecpc.fit=Res, #list with model fit in each fold
              dfPred = df, #data frame with information about out-of-bag predictions
              dfGrps=dfGrps, #data frame with information about estimated group and group set weights across folds
              dfCVM = dfCVM #data frame with cross-validated performance metric
  ))
}

#Discretise continuous co-data hierarchically----
#Output: list of groups of covariates of varying size
splitMedian <- function(values,index=NULL,depth=NULL,minGroupSize=50,first=TRUE,
                        split=c("both","lower","higher")){
  #split: "both" or "lower" if both groups have to be split recursively or only lower, lower-valued group
  if(length(split)==3) split <- "both"
  if(length(depth)==0){
    depth <- floor(log2(length(values)/minGroupSize)) #number of layers in hierarchical tree such that lowest level contains at least minSize group members (start from 2 groups)
  }
  if(length(index)==0&first){
    index <- 1:length(values)
  }
  if(split=="higher"){
    values <- -values
    split <- "lower"
  }
  
  medianX <- median(values) #compute median
  indSmall <- values<=medianX #index of values smaller than median
  indLarge <- values>medianX #index of values larger than median

  if(first){
    groups <- list(index,index[indSmall],index[indLarge]) #split group at median, include group with all variables
  }else{
    groups <- list(index[indSmall],index[indLarge]) #split group at median
  }
  if(depth>1){ #TD: improve recursion for leaves only splitting in one side
    if(split=="both"){
      return(c(groups,
               splitMedian(values[indSmall],index[indSmall],depth-1,minGroupSize=minGroupSize,first=FALSE,split=split),
               splitMedian(values[indLarge],index[indLarge],depth-1,minGroupSize=minGroupSize,first=FALSE,split=split)))
    }else if(split=="lower"){
      return(c(groups,
               splitMedian(values[indSmall],index[indSmall],depth-1,minGroupSize=minGroupSize,first=FALSE,split=split)))
    }else{
      stop("split should be one of both or lower")
    }
  }else{
    #Stop if one of the two groups has less than minGroupSize variables (could happen if lots of variables have the same value)
    LargeEnough <- sapply(groups,function(x){length(x)>=minGroupSize})
    if(all(LargeEnough)){
      return(groups)
    }else{
      return(NULL)
    }
  }
}

#Obtain group set on group level for hierarhical groups----
#Output: list of groups of group numbers (i.e. on group level) defining the hierarchy
obtainHierarchy <- function(groupset,penalty="LOG"){
  if(penalty=="LOG"){
    #if Latent Overlapping Group Lasso hypershrinkage is used (for now the only option in ecpc), 
    # the hierarchy on the covariate groups is defined as:
    # for each group number (node in the hierarchical tree),
    # make a group of the group number, say g, and all group numbers of groups that are supersets of group g
    groupset.grplvl <- lapply(groupset,function(i){as.numeric(which(sapply(groupset,function(j){all(i%in%j)})))})
  }else if(penalty=="GL"){
    #NOTE: this option is not yet available in ecpc, use LOG for hierarchical groups;
    #if Group Lasso hypershrinkage is used, the hierarchy on the covariate groups is defined as:
    # for each group number (node in the hierarchical tree): 
    # make a group of the group number, say g, and all group numbers of groups that are subsets of group g
    groupset.grplvl <- lapply(groupset,function(j){as.numeric(which(sapply(groupset,function(i){all(i%in%j)})))})
  }
  return(groupset.grplvl)
}

#Fit hierarchical lasso using LOG penalty----
hierarchicalLasso <- function(X,Y,groupset,lambda=NULL){
  #X: nxp matrix with observed data
  #Y: nx1 vector with response
  #groupset: list with hierarchical group indices
  #lambda: if not given, estimated with CV
  
  p<-dim(X)[2]
  Xxtnd <- X[,unlist(groupset)] #extend matrix such to create artifical non-overlapping groups
  #create new group indices for Xxtnd
  Kg2 <- c(1,sapply(groupset,length)) #group sizes on group level (1 added to easily compute hier. group numbers)
  G2 <- length(Kg2)-1
  groupxtnd <- lapply(2:length(Kg2),function(i){sum(Kg2[1:(i-1)]):(sum(Kg2[1:i])-1)}) #list of indices in each group
  groupxtnd2 <- unlist(sapply(1:G2,function(x){rep(x,Kg2[x+1])})) #vector with group number
  
  if(length(lambda)==0){
    fit <- gglasso::cv.gglasso(x=Xxtnd,y=Y, group = groupxtnd2,pred.loss = c("L2"))
    lambda <- fit$lambda.min
  }else{
    fit <- gglasso::gglasso(x=Xxtnd,y=Y, group = groupxtnd2)
  }
  
  #Hierarchical group lasso estimate for group variances
  #fit2<-gglasso(x=Xxtnd,y=Y,group = groupxtnd2, loss="ls")
  intrcptOG <- coef(fit,s=lambda)[1]
  vtilde <- coef(fit,s=lambda)[-1]
  v<-lapply(groupxtnd,function(g){
    x<-rep(0,p)
    x[unlist(groupset)[g]]<-x[unlist(groupset)[g]]+vtilde[g]
    return(x)
  })
  betahat <- c(apply(array(unlist(v),c(p,G2)),1,sum))
  
  return(list("betas"=betahat,"a0"=intrcptOG, "lambdarange"=fit$lambda,
              "lambda"=fit$lambda.min,"group.weights"=sqrt(Kg2[-1])))
}

#Multiply slices of 3D-arrays----
.matmult3d <- function(A,B){
  dimA <- dim(A)
  dimB <- dim(B)
  if(length(dimB)==2){                      #B matrix not an array, A 3D array
    return(array(apply(A,3,function(x){x%*%B}),c(dimA[1],dimB[2],dimA[3])))
  } else if(length(dimA)==2){               #A matrix not an array, B 3D array
    return(array(apply(B,3,function(x){A%*%x}),c(dimA[1],dimB[2],dimB[3])))
  } else if(dimB[3]==dimA[3]){              #A*B "matrix-wise" nsplits different A with nsplits different B
    return(array(apply(array(1:dimB[3],c(dimB[3],1,1)),1,function(i){A[,,i]%*%B[,,i]}),c(dimA[1],dimB[2],dimB[3])))
  } else if(dimB[3]==1){                    #A*B for nsplits different A and one B
    return(array(apply(A,3,function(x){x%*%B[,,1]}),c(dimA[1],dimB[2],dimA[3])))
  } else if(dimA[3]==1){                    #A*B for one A and nsplits different B
    return(array(apply(B,3,function(x){A[,,1]%*%x}),c(dimA[1],dimB[2],dimB[3])))
  } else{warning("matmult3d error")
    browser()}
}

#Iterative weighted least squares algorithm for logistic regression----
.IWLS <- function(X,Y,penalties,targets,eps=1e-7,maxItr=100){
  #Input:
  #X: nxp data matrix
  #Y: nx1 response
  #penalties: px1 vector with penalties (prop. to prior variance)
  #targets (optional): px1 vector with targets (equiv. to prior mean)
  #eps: numerical bound for convergence
  #maxItr: maximum number of iterations used in IWLS
  #Output:
  #betas: estimate for regression coefficients
  #convergence (TRUE/FALSE): TRUE if IWLS has converged under threshold eps, FALSE otherwise 
  #nIt: number of iterations needed for convergence
  
  p<- dim(X)[2]
  
  if(missing(targets)) targets <- rep(0,dim(X)[2])
  
  betaold <- rep(0,p) #intial value
  it <- 0; nrm <- Inf
  while(nrm>eps & it<maxItr){
    Xb <- X %*% betaold #linear predictor
    Ypred<-1/(1+exp(-Xb)) #predicted probability 
    W<-diag(c((Ypred*(1-Ypred)))) #Var(Y)
    
    XtXDinv <- solve(t(X) %*% W %*% X + 2*diag(penalties)) #inverting pxp matrix
    z <-  Xb + (Y - Ypred)/c(diag(W))
    betas <- XtXDinv %*% (t(X) %*% W %*% z + 2*diag(penalties)%*%targets)
    nrm <- sum((betas-betaold)^2)
    betaold <- betas
    it <- it + 1
  }
  convergence <- nrm<eps
  return(list(betas=betas,convergence=convergence,nIt=it))
}

#Compute inverse gamma penalty parameters with mode 1 for given group size and hyperpenalty lambda----
.prmsIGMode1 <- function(lam,Kg){
  #Input:
  #lam: penalty parameter lambda\in(0,\infty)
  #Kg: vector of group sizes
  #Output:
  #list with vector of alpIG, betIG:
  #the parameters of inverse gamma penalty such that:
  #Mode=1: betIG/(alpIG+1)=1
  #Variance=1/(lam*Kg): betIG^2/(alp-1)^2/(alp-2)=1/(lam*Kg)
  
  const <- 1/(lam*Kg)
  alpIG <- sapply(const,function(const){
    #solve cubic equation
    a <- 1
    b <- -(4+1/const)
    c <- 5-2/const
    d<- -(2+1/const)
    
    Delt0 <- b^2 - 3*a*c
    Delt1 <- 2*b^3 - 9*a*b*c + 27*a^2*d
    C <- (0.5*(Delt1+sqrt(as.complex(Delt1^2 - 4*Delt0^3))))^(1/3)
    if(Re(C)==0&&Im(C)==0) C <- (0.5*(Delt1-sqrt(Delt1^2 - 4*Delt0^3)))^(1/3)
    
    xi <- (0.5*(-1+sqrt(as.complex(-3))))^c(0,1,2)
    
    x <- -1/(3*a)*(b + xi*C + Delt0/(xi * C)) 
    xReal <- Re(x[abs(Im(x))<10^-12]) #real roots
    if(length(xReal)==0) xReal <- Re(x[which.min(abs(Im(x)))])
    return(xReal)
  })
  betIG <- alpIG+1
  
  return(list(alpIG,betIG))
}

###Plotting functions
#Visualise group set----
visualiseGroupset <- function(Groupset,groupweights,groupset.grouplvl,
                              nodeSize=10,ls=1){
  #return ggplot object with graphical visualisation of one group set:
  #graph with nodes, possibly connected if hierarchy is given
  #if group weights are given, nodes are coloured accordingly: 
  #  (blue if >1, red if <1, ivory if =1 or no group weight given, white if not selected (=0))
  #
  #Input:
  #Groupset: list of G elements containing the indices of the variables in that group
  #groupset.grouplvl (optional): hierarchical structure on the groups
  #groupweights (optional): group weights
  #Output:
  #Plot in a ggplot2 object
  if(!(requireNamespace("ggplot2")&requireNamespace("ggraph")&
     requireNamespace("igraph")&requireNamespace("scales")&requireNamespace("dplyr"))){
    stop("Packages ggplot2, ggraph, igraph and scales should be installed")
  }
  #initialise global variables and operators
  `%>%` <- dplyr::`%>%`
  name <- NULL; GrpWeight <- NULL; name2 <- NULL; Selected <- NULL
  from <- NULL; node1.visible <- NULL
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cols<-c("#FC8D59","ivory","#91BFDB") #brewer.pal(3,"RdYlBu")[c(1,3)]
  
  Kg <- sapply(Groupset,length) #group size of each group
  G<-length(Groupset) #number of groups in group set
  groupNames <- names(Groupset) #group names
  if(length(names(Groupset))==0) groupNames <- 1:G
  if(missing(groupset.grouplvl)||length(groupset.grouplvl)==0){
    basicGraph <- TRUE
    groupset.grouplvl <- lapply(1:G,function(x) x)
  }else basicGraph <- FALSE
  i<-unlist(sapply(1:G,function(x){rep(x,unlist(Kg)[x])}))
  j<-unlist(unlist(Groupset))
  ind <- sparseMatrix(i,j,x=1) #sparse matrix with ij element 1 if jth element in group i (global index), otherwise 0
  
  #Co-data matrix: sparse matrix with ij element 1/Ij if beta_j in group i
  Zt<-ind; 
  if(G[1]>1){
    Zt[1:G[1],]<-t(t(ind[1:G[1],])/apply(ind[1:G[1],],2,sum))
  }
  
  #create data frame with nodes
  # We can add a second data frame with information for each node!
  vertices <- data.frame(name=groupNames)
  if(missing(groupweights)){
    vertices <- vertices %>% dplyr::mutate("name2"=name,"Selected"=TRUE,"GrpWeight"=rep(1,G))
    showFillLegend <- FALSE
  } 
  else{
    vertices <- vertices %>% dplyr::mutate("GrpWeight"=round(groupweights,digits=2),
                                    "Selected"=groupweights!=0,
                                    "name2"=paste(name," (",GrpWeight,")",sep=""))
    showFillLegend <- TRUE
  }  
  
  temp<-obtainHierarchy(groupset.grouplvl)
  children<-lapply(1:length(temp),function(i){
    children <- setdiff(temp[[i]],i)
    for(j in children){
      children <- setdiff(children,setdiff(temp[[j]],j))
    }
    if(length(children)==0){return(NULL)}
    return(children)
  })
  # create an edge list data frame giving the hierarchical structure of your individuals
  edgeList <- data.frame(from=as.character(unlist(sapply(1:length(children),function(x){rep(x,length(children[[x]]))}))),
                         to=as.character(unlist(children)))
  mygraph <- igraph::graph_from_data_frame( edgeList ,vertices=vertices)
  
  if(basicGraph){ # Basic groups (no hierarchy)
    # Create a graph object
    mygraph <- igraph::graph_from_data_frame( edgeList ,vertices=vertices)
    
    lay <- ggraph::create_layout(mygraph, layout = 'linear')
    p<-ggraph::ggraph(lay) +
      ggraph::geom_node_label(ggplot2::aes(label=name2,fill=GrpWeight,col=Selected), show.legend = showFillLegend, fontface = "bold",
                      size=nodeSize,repel=TRUE,
                      point.padding = NA, box.padding = 0, force = 0.1)+
      #scale_fill_gradientn(colors=c("white",cols),breaks=c(0,1,NA),limits=c(0,NA))+
      ggplot2::scale_fill_gradientn(colors=c("white",cols),
                           #breaks=c(0,1,NA),
                           limits=c(0,NA),
                           values = scales::rescale(
                             c(0,min(10^-6,lay$GrpWeight[lay$GrpWeight>0]), #white for 0
                               min(10^-6,lay$GrpWeight[lay$GrpWeight>0]),1-1e-6,  #red for <1
                               1-1e-6,1+1e-6,
                               1+1e-6, max(lay$GrpWeight))),
                           name="Group weight")+
      ggplot2::scale_color_manual(values=c("TRUE"="black","FALSE"=cbPalette[1]))+
      ggplot2::theme_void()
    return(p)
  }
  
  nodesNoParents <- groupNames[!groupNames%in%edgeList$to]
  vertices <- rbind(vertices,rep(TRUE,dim(vertices)[2]))
  vertices[dim(vertices)[1],c("name","Selected")] <- c("origin",TRUE)
  vertices$visible <- TRUE
  vertices$visible[vertices$name=="origin"] <- FALSE
  edgeList <- rbind(edgeList,data.frame(from=rep("origin",length(nodesNoParents)),to=as.character(nodesNoParents)))
  edgeList <- edgeList %>% dplyr::mutate(visible=from!="origin")
  mygraph <- igraph::graph_from_data_frame( edgeList ,vertices=vertices)
  lay <- ggraph::create_layout(mygraph, layout = 'dendrogram', circular=FALSE)
  
  #str(get_edges("short")(lay))
  #lay$y[lay$name=="2"]<-lay$y[lay$name=="1"]
  p<- ggraph::ggraph(lay) + 
    ggraph::geom_edge_link(ggplot2::aes(alpha=node1.visible),edge_width=ls,
                   arrow=ggplot2::arrow(ends="last",length=ggplot2::unit(3,"mm"),type="closed"),
                   end_cap = ggraph::circle(6, 'mm')) +
    ggraph::scale_edge_alpha_discrete(range=c(0,1),guide=FALSE)+
    ggraph::geom_node_label(ggplot2::aes(label=name2,fill=GrpWeight,col=Selected),
                    show.legend=showFillLegend, fontface = "bold",size=nodeSize)+
    #scale_fill_gradient(low="white",high=cbPalette[4])+
    ggplot2::scale_fill_gradientn(colors=c("white",cols),
                         #breaks=c(0,1,NA),
                         limits=c(0,NA),
                         values = scales::rescale(
                           c(0,min(lay$GrpWeight[lay$GrpWeight>0],10^-6), #white for 0
                             min(10^-6,lay$GrpWeight[lay$GrpWeight>0]),1-1e-6,  #red for <1
                             1-1e-6,1+1e-6,
                             1+1e-6, max(lay$GrpWeight))),
                         name="Group weight")+
    #geom_node_label(ggplot2::aes(label=name2,fill=factor(Selected)), fontface = "bold")+
    ggplot2::scale_color_manual(values=c("FALSE"=cbPalette[1],"TRUE"="black"))+
    ggplot2::coord_cartesian(ylim=c(min(lay$y)-0.5,max(lay$y[lay$name!="origin"])+0.5),xlim=c(min(lay$x)-0.5,max(lay$x)+0.5))+
    #geom_node_text(ggplot2::aes(label=GrpWeight, filter=leaf) , angle=90 , hjust=1, nudge_y = -0.2) +
    ggplot2::theme_void()
  
  return(p)
}

#Visualise group set weights in CV folds----
visualiseGroupsetweights <- function(dfGrps,GroupsetNames,hist=FALSE,boxplot=TRUE,
                                     jitter=TRUE,ps=1.5,width=0.5){
  #Plot cross-validated group set weights
  #
  #Input:
  #-dfGrps: dataframe with the following variables:
  #   -Groupset: factor with group set names
  #   -Groupset.weight: group set weight of each group set
  #   -Fold: number indicating which fold in the cross-validation is used
  #   -hist: if hist=TRUE, a histogram is plotted
  #-GroupsetNames: vector with group set names in order levels(dfGrps)
  #
  #Output:
  #-p1: plot in ggplot object
  
  if(!(requireNamespace("ggplot2")&requireNamespace("scales")&requireNamespace("dplyr"))){
    stop("Packages ggplot2, dplyr and scales should be installed")
  }
  #initialise global variables and operators
  `%>%` <- dplyr::`%>%`
  Groupset <- NULL; Groupset.weight <- NULL; q75Weight <- NULL; q25Weight <- NULL;
  meanWeight <- NULL; minWeight <- NULL; maxWeight <- NULL; Fold <- NULL
  
  #change group set names to those in GroupsetNames if given
  if(!missing(GroupsetNames) && length(GroupsetNames)>0){
    if(is.factor(dfGrps$Groupset)){
      dfGrps$Groupset <- factor(dfGrps$Groupset,levels = levels(dfGrps$Groupset)[levels(dfGrps$Groupset)%in%unique(dfGrps$Groupset)],
                                 labels = GroupsetNames)
    }else{
      dfGrps$Groupset <- factor(GroupsetNames[dfGrps$Groupset],levels=GroupsetNames,labels=GroupsetNames)
    }
  }else{
    dfGrps$Groupset <- as.factor(dfGrps$Groupset)
  }
  
  if(hist){
    p1 <- ggplot2::ggplot(dfGrps)+
      ggplot2::aes(x=Groupset.weight)+
      ggplot2::geom_histogram(ggplot2::aes(fill=Groupset),position = "identity",bins=20,alpha=0.6)+
      ggplot2::scale_fill_discrete(name="Group set")+
      ggplot2::labs(x="Group set weight")+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x=ggplot2::element_text(size=12),
            axis.text.y=ggplot2::element_text(size=12),
            axis.title.x=ggplot2::element_text(size=14),
            axis.title.y=ggplot2::element_text(size=14),
            legend.text=ggplot2::element_text(size=12),
            legend.title=ggplot2::element_text(size=14))
    
  }else if(!(boxplot|jitter)){
    SummdfGrps <- dfGrps %>% dplyr::group_by(Groupset) %>% dplyr::summarise("meanWeight"=mean(Groupset.weight),
                                                               "q25Weight"=quantile(Groupset.weight,0.25),
                                                               "q75Weight"=quantile(Groupset.weight,0.75),
                                                               "maxWeight"=max(Groupset.weight),
                                                               "minWeight"=min(Groupset.weight)) %>% dplyr::ungroup()
    
    p1<-ggplot2::ggplot(SummdfGrps)+
      ggplot2::geom_linerange(ggplot2::aes(x=Groupset,ymax=q75Weight,ymin=q25Weight,col=Groupset),linetype="dashed")+
      ggplot2::geom_point(ggplot2::aes(x=Groupset,y=minWeight,col=Groupset),shape=2)+
      ggplot2::geom_point(ggplot2::aes(x=Groupset,y=meanWeight,col=Groupset),shape=1)+
      ggplot2::geom_point(ggplot2::aes(x=Groupset,y=maxWeight,col=Groupset),shape=6)+
      ggplot2::scale_color_discrete(name="Group set")+
      ggplot2::labs(y="Group set weight",x="Group set")+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x=ggplot2::element_text(size=12),
            axis.text.y=ggplot2::element_text(size=12),
            axis.title.x=ggplot2::element_text(size=14),
            axis.title.y=ggplot2::element_text(size=14),
            legend.text=ggplot2::element_text(size=12),
            legend.title=ggplot2::element_text(size=14))
  }else{
    dfGrps2 <- dfGrps %>% dplyr::group_by(Fold) %>% dplyr::distinct(Groupset, .keep_all=TRUE) %>% dplyr::ungroup()
    p1 <- ggplot2::ggplot(dfGrps2)+
      ggplot2::scale_color_discrete(name="Group set")+
      ggplot2::labs(y="Group set weight",x="Group set")+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x=ggplot2::element_text(size=12),
            axis.text.y=ggplot2::element_text(size=12),
            axis.title.x=ggplot2::element_text(size=14),
            axis.title.y=ggplot2::element_text(size=14),
            legend.text=ggplot2::element_text(size=12),
            legend.title=ggplot2::element_text(size=14))
    if(boxplot){
      p1 <- p1+ggplot2::geom_boxplot(ggplot2::aes(x=Groupset,y=Groupset.weight,col=Groupset),outlier.shape = NA,width=width)
    } 
    if(jitter){
      p1<- p1+ggplot2::geom_jitter(ggplot2::aes(x=Groupset,col=Groupset,y=Groupset.weight),
                          size=ps,alpha=0.6,height=0,width=width/2)
    } 
  }
  
  
  
  return(p1)
}

#Visualise group weights in CV folds----
visualiseGroupweights <- function(dfGrps,Groupset,groupset.grouplvl,
                                  values,widthBoxplot=0.05,boxplot=TRUE,
                                  jitter=TRUE,ps=1.5,ls=1){
  #Plot cross-validated group weights for one group set
  #
  #Input:
  #-dfGrps: dataframe with the following variables:
  #   -Group: factor with group names
  #   -Group.weight: group weight of each group
  #   -Fold: number indicating which fold in the cross-validation is used
  #-Groupset: list of G elements containing covariate indices for each group
  #-groupset.grouplvl (optional): groups of groups, e.g. in a hierarchical structure. 
  #-values (optional): values of continuous co-data. If given, group weights are plotted against these value
  #
  #Output:
  #-p1: plot in ggplot object
  
  if(!(requireNamespace("ggplot2")&requireNamespace("scales")&requireNamespace("dplyr"))){
    stop("Packages ggplot2, dplyr and scales should be installed")
  }
  #initialise global variables and operators
  `%>%` <- dplyr::`%>%`
  Group <- NULL; Group.weight <- NULL; q75Weight <- NULL; q25Weight <- NULL
  minWeight <- NULL; maxWeight <- NULL; meanWeight <- NULL;
  newLeafGrp <- NULL; Continuous.values <- NULL; Weight <- NULL
  originalLeafGrp <- NULL; q50Value <- NULL; minValue <- NULL; maxValue <- NULL
  q50Weight <- NULL; Fold <- NULL; MedianGroupValue <- NULL
  
  #change group names to those in Groupset if given
  if(!missing(Groupset) && length(names(Groupset))>0){
    if(is.factor(dfGrps$Group)){
      dfGrps$Group <- factor(dfGrps$Group,levels = levels(dfGrps$Group)[levels(dfGrps$Group)%in%unique(dfGrps$Group)],
                             labels = names(Groupset))
    }else{
      dfGrps$Group <- factor(names(Groupset)[dfGrps$Group])
    }
  }
  
  if(missing(values) || length(values)==0){
    SummdfGrps <- dfGrps %>% dplyr::group_by(Group) %>% dplyr::summarise("meanWeight"=mean(Group.weight),
                                                           "q25Weight"=quantile(Group.weight,0.25),
                                                           "q75Weight"=quantile(Group.weight,0.75),
                                                           "maxWeight"=max(Group.weight),
                                                           "minWeight"=min(Group.weight)) %>% dplyr::ungroup()
    
    if(!(boxplot|jitter)){
      p1<-ggplot2::ggplot(SummdfGrps)+
        ggplot2::geom_hline(yintercept=1,col="grey",linetype="dashed",size=ls,alpha=0.6)+
        ggplot2::geom_linerange(ggplot2::aes(x=Group,ymax=q75Weight,ymin=q25Weight,col=Group),linetype="dashed")+
        ggplot2::geom_point(ggplot2::aes(x=Group,y=minWeight,col=Group),shape=2)+
        ggplot2::geom_point(ggplot2::aes(x=Group,y=meanWeight,col=Group),shape=1)+
        ggplot2::geom_point(ggplot2::aes(x=Group,y=maxWeight,col=Group),shape=6)+
        ggplot2::labs(y="Prior variance weight")+
        ggplot2::theme_bw()+
        ggplot2::theme(axis.text.x=ggplot2::element_text(size=12),
              axis.text.y=ggplot2::element_text(size=12),
              axis.title.x=ggplot2::element_text(size=14),
              axis.title.y=ggplot2::element_text(size=14),
              legend.text=ggplot2::element_text(size=12),
              legend.title=ggplot2::element_text(size=14))
    }else{
      p1<-ggplot2::ggplot(dfGrps)+
        ggplot2::geom_hline(yintercept=1,col="grey",linetype="dashed",size=ls,alpha=0.6)+
        ggplot2::labs(y="Prior variance weight")+
        ggplot2::theme_bw()+
        ggplot2::theme(axis.text.x=ggplot2::element_text(size=12),
              axis.text.y=ggplot2::element_text(size=12),
              axis.title.x=ggplot2::element_text(size=14),
              axis.title.y=ggplot2::element_text(size=14),
              legend.text=ggplot2::element_text(size=12),
              legend.title=ggplot2::element_text(size=14))
      if(boxplot){
        p1<- p1+ggplot2::geom_boxplot(ggplot2::aes(x=Group,y=Group.weight,col=Group),outlier.shape = NA,show.legend=FALSE)
      } 
      if(jitter){
        p1<- p1+ggplot2::geom_jitter(ggplot2::aes(x=Group,y=Group.weight,col=Group),show.legend=FALSE,
                            size=ps,alpha=0.6,height=0,width=widthBoxplot/2)
      } 
    }
    return(p1)
  }else{
    getIndBetas<-function(p,ind){
      indBetas <- lapply(1:p,function(x){
        return(unlist(sapply(1:length(ind),function(grp){
          if(any(x==ind[[grp]])) return(grp)
          else return(NULL)
        })))
      }) #for each beta 1,..,p: group numbers
      return(indBetas)
    }
    
    GetWeight <- function(p,indBetas,groupweights,groupnumber){
      averageweight <- unlist(sapply(1:p,function(x){
        grps <- indBetas[[x]]
        size <- length(grps)
        return(mean(groupweights[groupnumber%in%grps]))
      }))
      return(averageweight)
    }
    
    #find leaves (groups that have no children)
    if(all(sapply(obtainHierarchy(Groupset),length)==1)) leaves <- 1:length(Groupset) 
    else leaves <- which(sapply(obtainHierarchy(groupset.grouplvl),length)==1) 
    
    IndBetas<-getIndBetas(p=length(values),Groupset)
    avrg <- sapply(Groupset[leaves],function(x){mean(values[x])}) #average of continuous co-data in leaf groups
    medians <- sapply(Groupset[leaves],function(x){median(values[x])}) #median of continuous co-data in leaf groups
    ord <- order(avrg) 
    leaves<-leaves[ord]
    newLeaf <- as.factor(sapply(IndBetas,function(x){
      which(leaves%in%x)
    }))
    originalLeaf <- as.factor(sapply(IndBetas,function(x){
      leaves[which(leaves%in%x)]
    }))
    originalLeaf <- factor(originalLeaf,levels=leaves,labels=leaves) #order labels
    
    dfBeta <- data.frame()
    for(l in unique(dfGrps$Fold)){
      dfBeta2 <- data.frame("index"=1:length(values),
                            "Weight"=GetWeight(length(values),indBetas=IndBetas,
                                               groupweights=dfGrps$Group.weight[dfGrps$Fold==l],
                                               groupnumber=dfGrps$Group[dfGrps$Fold==l]),
                            "Continuous.values"=values,
                            "newLeafGrp"=newLeaf,
                            "originalLeafGrp"=originalLeaf
      )
      dfBeta2$Fold <- l
      dfBeta2$AverageGroupValue <- avrg[ord[dfBeta2$originalLeafGrp]]
      dfBeta2$MedianGroupValue <- medians[ord[dfBeta2$originalLeafGrp]]
      dfBeta <- rbind(dfBeta,dfBeta2)
    }
    dfBeta$Fold <- as.factor(dfBeta$Fold)
    
    SummdfBeta <- dfBeta %>% dplyr::group_by(newLeafGrp) %>% dplyr::summarise("meanValue"=mean(Continuous.values),
                                                                "minValue"=min(Continuous.values),
                                                                "maxValue"=max(Continuous.values),
                                                                "q50Value"=quantile(Continuous.values,0.5),
                                                                "meanWeight"=mean(Weight),
                                                                "q25Weight"=quantile(Weight,0.25),
                                                                "q75Weight"=quantile(Weight,0.75),
                                                                "maxWeight"=max(Weight),
                                                                "minWeight"=min(Weight),
                                                                "q50Weight"=quantile(Weight,0.5),
                                                                "originalLeafGrp"=originalLeafGrp[1]) %>% dplyr::ungroup()
    
    if(any(is.na(SummdfBeta$meanValue))){ #missing data group
      missingGroup <- which(is.na(SummdfBeta$meanValue))
      SummdfBeta[missingGroup,"minValue"]<-min(SummdfBeta$minValue,na.rm=TRUE)
      SummdfBeta[missingGroup,"maxValue"]<-max(SummdfBeta$maxValue,na.rm=TRUE)
      SummdfBeta[missingGroup,"meanValue"]<-mean(c(SummdfBeta[[missingGroup,"minValue"]],SummdfBeta[[missingGroup,"maxValue"]]))
      levels(SummdfBeta$originalLeafGrp)[missingGroup] <- paste(levels(SummdfBeta$originalLeafGrp)[missingGroup]," (missing data group)",sep="")
    }
    
    if(!(boxplot|jitter)){
      p1<-ggplot2::ggplot(SummdfBeta)+
        ggplot2::geom_hline(yintercept=1,col="grey",linetype="dashed",size=ls,alpha=0.6)+
        ggplot2::geom_linerange(ggplot2::aes(x=q50Value,ymax=q75Weight,ymin=q25Weight,col=originalLeafGrp),linetype="dashed")+
        ggplot2::geom_point(ggplot2::aes(x=q50Value,y=minWeight,col=originalLeafGrp),shape=2)+
        ggplot2::geom_point(ggplot2::aes(x=q50Value,y=meanWeight,col=originalLeafGrp),shape=1)+
        ggplot2::geom_point(ggplot2::aes(x=q50Value,y=maxWeight,col=originalLeafGrp),shape=6)+
        ggplot2::geom_segment(ggplot2::aes(x=minValue,xend=maxValue,y=q50Weight,yend=q50Weight,col=originalLeafGrp))+
        ggplot2::scale_color_discrete(name="Group")+
        ggplot2::labs(x="Continuous co-data value",y="Prior variance weight")+
        ggplot2::theme_bw()+
        ggplot2::theme(axis.text.x=ggplot2::element_text(size=12),
              axis.text.y=ggplot2::element_text(size=12),
              axis.title.x=ggplot2::element_text(size=14),
              axis.title.y=ggplot2::element_text(size=14),
              legend.text=ggplot2::element_text(size=12),
              legend.title=ggplot2::element_text(size=14))
    }else{
      dfBeta2 <- dfBeta %>% dplyr::group_by(Fold) %>% dplyr::distinct(originalLeafGrp, .keep_all=TRUE) %>% dplyr::ungroup()
      
      p1<-ggplot2::ggplot(dfBeta2)+
        ggplot2::geom_hline(yintercept=1,col="grey",linetype="dashed",size=ls,alpha=0.6)+
        ggplot2::geom_segment(data=SummdfBeta,ggplot2::aes(x=minValue,xend=maxValue,y=q50Weight,yend=q50Weight,col=originalLeafGrp),size=ls)+
        ggplot2::labs(x="Continuous co-data value",y="Prior variance weight")+
        ggplot2::scale_color_discrete(name="Group")+
        ggplot2::theme_bw()+
        ggplot2::theme(axis.text.x=ggplot2::element_text(size=12),
              axis.text.y=ggplot2::element_text(size=12),
              axis.title.x=ggplot2::element_text(size=14),
              axis.title.y=ggplot2::element_text(size=14),
              legend.text=ggplot2::element_text(size=12),
              legend.title=ggplot2::element_text(size=14))
      if(boxplot){
        p1<- p1 +ggplot2::geom_boxplot(ggplot2::aes(x=MedianGroupValue,col=originalLeafGrp,y=Weight),width=widthBoxplot,
                              varwidth=FALSE,position="identity",outlier.shape = NA)
      } 
      if(jitter){
        p1<- p1+ggplot2::geom_jitter(data=dfBeta2,ggplot2::aes(x=MedianGroupValue,col=originalLeafGrp,y=Weight),
                            size=ps,alpha=0.6,height=0,width=widthBoxplot/4)
      }
      
    }
    return(p1)
  }
}

#get weights of leaf groups in continuous codata
.getWeightLeaves <- function(dfGrps,Groupset,groupset.grouplvl,values){
  #Return data frame with group weights of the leaf groups in a continuous co-data,
  #instead of the group weights per group in the hierarchical tree
  #
  #Input:
  #-dfGrps: dataframe with the following variables:
  #   -Group: factor with group names
  #   -Group.weight: group weight of each group
  #   -Fold: number indicating which fold in the cross-validation is used
  #-Groupset: list of G elements containing covariate indices for each group
  #-groupset.grouplvl (optional): groups of groups, e.g. in a hierarchical structure. 
  #-values (optional): values of continuous co-data. If given, group weights are plotted against these value
  #
  #Output:
  #-df: dataframe similar to dfGrps but with group weights for leaf groups
  
  Fold <- NULL; originalLeafGrp <- NULL
  if(!all(c("Group","Group.weight","Fold")%in%names(dfGrps))){
    stop("dfGrps should contain the variables Group, Group.weight and Fold")
  } 
  if(requireNamespace("magrittr")) `%>%` <- magrittr::`%>%`
  
  #change group names to those in Groupset if given
  if(!missing(Groupset) && length(names(Groupset))>0){
    if(is.factor(dfGrps$Group)){
      dfGrps$Group <- factor(dfGrps$Group,levels = levels(dfGrps$Group)[levels(dfGrps$Group)%in%unique(dfGrps$Group)],
                             labels = names(Groupset))
    }else{
      dfGrps$Group <- factor(names(Groupset)[dfGrps$Group])
    }
  }
  
  
  getIndBetas<-function(p,ind){
    indBetas <- lapply(1:p,function(x){
      return(unlist(sapply(1:length(ind),function(grp){
        if(any(x==ind[[grp]])) return(grp)
        else return(NULL)
      })))
    }) #for each beta 1,..,p: group numbers
    return(indBetas)
  }
  
  GetWeight <- function(p,indBetas,groupweights,groupnumber){
    averageweight <- unlist(sapply(1:p,function(x){
      grps <- indBetas[[x]]
      size <- length(grps)
      return(mean(groupweights[groupnumber%in%grps]))
    }))
    return(averageweight)
  }
  
  #find leaves (groups that have no children)
  if(all(sapply(obtainHierarchy(Groupset),length)==1)) leaves <- 1:length(Groupset) 
  else leaves <- which(sapply(obtainHierarchy(groupset.grouplvl),length)==1) 
  
  IndBetas<-getIndBetas(p=length(values),Groupset)
  avrg <- sapply(Groupset[leaves],function(x){mean(values[x])}) #average of continuous co-data in leaf groups
  ord <- order(avrg) 
  leaves<-leaves[ord]
  newLeaf <- as.factor(sapply(IndBetas,function(x){
    which(leaves%in%x)
  }))
  originalLeaf <- as.factor(sapply(IndBetas,function(x){
    leaves[which(leaves%in%x)]
  }))
  originalLeaf <- factor(originalLeaf,levels=leaves,labels=leaves) #order labels
  
  dfBeta <- data.frame()
  for(l in unique(dfGrps$Fold)){
    dfBeta2 <- data.frame("index"=1:length(values),
                          "Weight"=GetWeight(length(values),indBetas=IndBetas,
                                             groupweights=dfGrps$Group.weight[dfGrps$Fold==l],
                                             groupnumber=dfGrps$Group[dfGrps$Fold==l]),
                          "Continuous.values"=values,
                          "newLeafGrp"=newLeaf,
                          "originalLeafGrp"=originalLeaf
    )
    dfBeta2$Fold <- l
    dfBeta <- rbind(dfBeta,dfBeta2)
  }
  dfBeta$Fold <- as.factor(dfBeta$Fold)
  
  #data frame with the group weight of all betas in the leaf group
  dfGrps2 <- dfBeta %>% dplyr::group_by(Fold) %>% dplyr::distinct(originalLeafGrp, .keep_all = TRUE) %>% 
    dplyr::select(c("originalLeafGrp","Weight","Fold","newLeafGrp")) %>% dplyr::ungroup()
  dfGrps2$Group <- dfGrps2$originalLeafGrp
  dfGrps2$Groupset<-dfGrps$Groupset[1]
  dfGrps2$Group.weight <- dfGrps2$Weight
  dfGrps2$Method <- dfGrps$Method[1]
  dfGrps2 <- dfGrps2 %>% dplyr::group_by(Fold) %>% dplyr::mutate(Groupset.weight=dfGrps$Groupset.weight[which(dfGrps$Fold==unique(Fold))[1]],
                                                   Taugr=dfGrps$Taugr[which(dfGrps$Fold==unique(Fold))[1]],
                                                   Tauglm=dfGrps$Tauglm[which(dfGrps$Fold==unique(Fold))[1]]) %>% dplyr::ungroup()
  if(!all(names(dfGrps)%in%names(dfGrps2))) warning("Check here, dfGrps has some variables that are not in new data frame")
  dfGrps2 <- dfGrps2 %>% dplyr::select(names(dfGrps))
  return(dfGrps2)
    
  
}
