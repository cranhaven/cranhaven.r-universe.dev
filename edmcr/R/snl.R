#'Sensor Network Localization
#'
#'\code{snl} solves the sensor network problem with
#' partial distance (squared) matrix D, and anchor positions anchors, in
#' dimension d. 
#' 
#' @details 
#' Set anchors=NULL to solve the anchorless (Euclidean distance matrix completion) 
#' problem in dimension d.
#' 
#' NOTE:  When anchors is specified, the distances between the anchors must be in the
#' bottom right corner of the matrix D, and anchors must have d columns.
#' 
#' @param D The partial distance matrix specifying the known distances between nodes. 
#' If anchors is specified (and is a pxr matrix), the p final columns and p final rows specify the 
#' distances between the anchors specified in anchors.
#' @param d the dimension for the resulting completion
#' @param anchors a pxr matrix specifying the d dimensional locations of the p anchors. If the anchorless problem
#' is to be solved, anchors = NULL
#' 
#' @return X the d-dimensional positions of the localized sensors. Note that it may be the case
#' that not all  sensors could be localized, in which case X contains the positions of only the localized sensors.
#' 
#' @examples 
#' D <- matrix(c(0,NA,.1987,NA,.0595,NA,.0159,.2251,.0036,.0875,
#'               NA,0,.0481,NA,NA,.0515,NA,.2079,.2230,NA,
#'               .1987,.0481,0,NA,NA,.1158,NA,NA,.1553,NA,
#'               NA,NA,NA,0,NA,NA,NA,.2319,NA,NA,
#'               .0595,NA,NA,NA,0,NA,.1087,.0894,.0589,.0159,
#'               NA,.0515,.1158,NA,NA,0,NA,NA,NA,NA,
#'               .0159,NA,NA,NA,.1087,NA,0,.3497,.0311,.1139,
#'               .2251,.2079,NA,.2319,.0894,NA,.3497,0,.1918,.1607,
#'               .0036,.2230,.1553,NA,.0589,NA,.0311,.1918,0,.1012,
#'               .0875,NA,NA,NA,.0159,NA,.1139,.1607,.1012,0),nrow=10, byrow=TRUE)
#'               
#' anchors <- matrix(c(.5131,.9326,
#'                     .3183,.3742,
#'                     .5392,.7524,
#'                     .2213,.7631), nrow=4,byrow=TRUE)
#' d <- 2
#'
#' #Anchorless Problem
#' edmc(D, method="snl", d=2, anchors=NULL)
#'
#' #Anchored Problem
#' edmc(D, method="snl", d=2, anchors=anchors)
#'
#' @references 
#' 
#' Nathan Krislock and Henry Wolkowicz. Explicit sensor network localization
#' using semidefinite representations and facial reductions. SIAM Journal on
#' Optimization, 20(5):2679-2708, 2010.
#' 
#' @export

snl <- function(D,d,anchors=NULL){
  
  ########### Input Checking ##################
  
  #General Checks for D
  if(nrow(D) != ncol(D)){
    stop("D must be a symmetric matrix")
  }else if(!is.numeric(D)){
    stop("D must be a numeric matrix")
  }else if(!is.matrix(D)){
    stop("D must be a numeric matrix")
  }else if(any(diag(D) != 0)){
    stop("D must have a zero diagonal")
  }else if(!isSymmetric(unname(D),tol=1e-8)){
    stop("D must be a symmetric matrix")
  }else if(!any(is.na(D))){
    stop("D must be a partial distance matrix. Some distances must be unknown.")
  }
  
  if(!is.null(anchors)){
    if(!is.numeric(anchors)){
      stop("anchors must be a numeric matrix")
    }else if(!is.matrix(anchors)){
      stop("anchors must be a numeric matrix")
    }
  }
  
  #General Checks for d
  if(!is.numeric(d)){
    stop("d must be a numeric integer")
  }else if(length(d) != 1){
    stop("d must be a numeric integer")
  }else if(!(d %% 2 == 1 | d %% 2 == 0)){
    stop("d must an integer")
  } 
  
  ####################################################
  ############# MAIN FUNCTION START ##################
  ####################################################
  
  #Square Partial Distance matrix
  D <- (D)^2
  
  #Turn NAs to 0s
  D[is.na(D)] <- 0
  
  #Initialize parameters
  condtolerscaling <- 1e2
  maxscaling <- 1e8
  
  #Process inputs and error checking
  
  n <- max(nrow(D),ncol(D))
  if(is.null(anchors)){
    m <- 0
  }else{
    m <- nrow(anchors)
  }

  ###### Nested Function: GrowCliques ######
  temp <- snlGrowCliques(D,anchors,d,n,m)
  
  Dcq <- temp$Dcq
  Cp <- temp$Cp
  eigvs <- temp$eigvs
  ic <- temp$ic
  ##########################################
  
  intcliques <- t(Dcq) %*% Dcq
  csizes <- as.matrix(diag(intcliques))
  intcliques[lower.tri(intcliques,diag=TRUE)] <- 0
  
  #needed at end and in node absorption step
  Dcqinit <- Dcq
  csizesinit <- csizes
  
  #Initialization indicators
  Co <- t(as.matrix(which(Cp > 0)))
  
  grow <- 1
  paramchange <- 0
  mainwiters <- 0
  
  while(length(Co) > 1 & (grow|paramchange)){
    
    #Reset Indicators
    grow <- 0
    paramchange <- 0
    mainwiters <- mainwiters + 1
    
    #Rigid Clique Union
    for(ict in Co){
      if(Cp[ict]){
        if(sum(Dcq[,ict]) >= d+1){
          
          #Find the cliques intersecting clique ic
          icintcliques <- t(Dcq) %*% Dcq[,ict]
          icintcliques[ict] <- 0
          Copj <- t(as.matrix(which(icintcliques > 0)))
          
          for(jc in Copj){
            
            ######## NESTED FUNCTION: RIGID CLIQUE UNION #########
            temp <- snlRigidCliqueUnion(ict,jc,Dcq,Cp,eigvs,grow,condtolerscaling,n,D,d)
            Dcq <- temp$Dcq
            Cp <- temp$Cp
            eigvs <- temp$eigvs
            grow <- temp$grow
            flagred <- temp$flagred
            
            ######################################################
            
            if(flagred == 1){
              paramchange <- 1
            }
          }
        }
      }
    }
    
    #Rigid Node Absorption
    
    if(!grow){
      for(ict in Co){
        
        p <- Dcq[,ict]>0
        if(sum(p) >= d+1){
          
          #Find nodes connected to at least d+1 nodes in clique ic
          NeighbourDegrees <- rowSums(D[,p] > 0) * !p
          SortNeighbours <- sort.int(NeighbourDegrees, decreasing=TRUE,index.return=TRUE)
          sND <- SortNeighbours$x
          inds <- SortNeighbours$ix
          
          icConnectedNodes <- t(as.matrix(inds[sND >= d+1]))
          numnodes <- min(length(icConnectedNodes),d+1)
          
          if(numnodes != 0){
          for(jc in icConnectedNodes[seq(1,numnodes,by=1)]){
            
            ############ NESTED FUNCTION: RIGID NODE ABSORPTION #############
            
            temp <- snlRigidNodeAbsorption(ict,jc,D,Dcq,eigvs,grow,Dcqinit,condtolerscaling,d,n,csizesinit)
            
            Dcq <- temp$Dcq
            eigvs <- temp$eigvs
            grow <- temp$grow
            flagred <- temp$flagred
            
            #################################################################
            
            if(flagred == 1){
              paramchange = 1
            }
          }
          }
        }
      }
    }
    
    #Compute sizes of clique intersections and clique sizes
    
    intcliques <- t(Dcq) %*% Dcq
    csizes <- as.matrix(diag(intcliques))
    intcliques[lower.tri(intcliques,diag=TRUE)] <- 0
    
    Co <- t(which(Cp > 0))
    
    #Scaling
    
    if(!grow & paramchange){
      if(condtolerscaling < maxscaling){
        condtolerscaling <- 10*condtolerscaling
      }else{
        paramchange <- 0
      }
    }
  }
  
  #Compute Sensor Positions
  
  if(is.null(anchors)){
    ic <- which(csizes == max(csizes))[1]
    
    ############ COMPLETE CLIQUE ###################
    
    temp <- snlCompleteClique(ic,Dcq,eigvs,D,Dcqinit,d,n,csizesinit,anchors)
    
    eigvs <- temp$eigvs
    P <- temp$P
    flagred <- temp$flagred
  
    ###############################################
    
    if(flagred){
      X <- c()
    }else{
      p <- Dcq[,ic] > 0
      X <- P[p,]
    }
  }else{
    anchors2 <- as.matrix(rep(FALSE,n))
    anchors2[seq(n-m+1,length(anchors2),by=1)] <- TRUE
    acliques <- t((t(anchors2) %*% Dcq == m))
    temp <- csizes * acliques
    ic <- which(temp == max(temp))
    
    if(csizes[ic] >= d+1){
      
      ######## COMPLETE CLIQUE #############
      
      temp <- snlCompleteClique(ic,Dcq,eigvs,D,Dcqinit,d,n,csizesinit,anchors2)
      eigvs <- temp$eigvs
      P <- temp$P
      flagred <- temp$flagred
      
      ######################################
    }else{
      flagred <- 1
    }
    
    if(flagred){
      X <- NULL
    }else{
      icensors <- Dcq[,ic] > anchors2
      if(length(which(icensors > 0)) > 0){
        
        #Translate P so that P2 is centred at zero
        P2 <- P[seq(n-m+1,n,by=1),]
        mp2 <- colSums(P2)/m
        P <- P - matrix(rep(mp2,n),ncol=ncol(P),byrow=TRUE)

        #Translate anchors so that anchors is centred at zero
        ma <- colSums(anchors)/m
        anchors <- as.matrix(anchors - matrix(rep(ma,m),ncol=ncol(anchors),byrow=TRUE))
        
        #Solve the Orthogonal Procrustes Problem
        C <- t(P2) %*% anchors
        temp <- svd(C)
        Uc <- temp$u
        Vc <- temp$v
        Q <- Uc %*% t(Vc)
        
      #Compute the Final X
        P <- P %*% Q
        P <- P + matrix(rep(ma,n),ncol=ncol(P),byrow=TRUE)
        anchors <- anchors + matrix(rep(ma,m),ncol=ncol(anchors),byrow=TRUE)
        P2 <- P[seq(n-m+1,n,by=1),]
        X <- P[icensors,]
      }else{
        X <- NULL
      }
    }
  }

  return(list(X=X))
  
}