#'Compute Minimum Spanning Tree
#'
#'\code{mst} Compute a minimum spanning tree using Prim's algorithm
#'
#'@param D A distance matrix
#'
#'@return MST a data frame object of 3 columns containing the parent nodes, child nodes, and corresponding weight of the MST edge
#'
#'@examples
#'
#'X <- runif(10,0,1)
#'Y <- runif(10,0,1)
#'D <- dist(cbind(X,Y))
#'
#'mst(as.matrix(D))
#'
#'@export

mst <- function(D){
  
  #General Checks for D
  if(!is.matrix(D)){
    stop("D must be a numeric matrix")
  }else if(nrow(D) != ncol(D)){
    stop("D must be a symmetric matrix")
  }else if(!is.numeric(D)){
    stop("D must be a numeric matrix")
  }else if(any(diag(D) != 0)){
    stop("D must have a zero diagonal")
  }else if(!isSymmetric(D,tol=1e-8)){
    stop("D must be a symmetric matrix")
  }
  
  #Set the diagonal to Infinity so we don't pick it
  diag(D) <- Inf
  
  #Some initial parameters
  n <- nrow(D)
  
  #Initialize Vectors of size n-1
  
  Parent <- numeric(n-1)
  Child <- numeric(n-1)
  Edge <- numeric(n-1)
  InMST <- c()
  NotInMST <- seq(1,n,1)
  
  #Initialize at Node 1
  
  Parent[1] <- 1
  Child[1] <- which.min(D[1,])[1]
  Edge[1] <- D[Parent[1],Child[1]]
  InMST <- c(InMST,Parent[1],Child[1])
  NotInMST <- NotInMST[-c(Parent[1],Child[1])]
  
  #Fill in the Remaining Nodes
  if((n-2) > 0){
    for(i in 1:(n-2)){
      #Extract the rows of all nodes currently in the MST
      InNodes <- D[InMST,]
      
      #Extract the columns that are not currently in the MST
      AvailableNodes <- InNodes[,NotInMST]
      
      #Find the Max Value in each row
      RowMin <- sapply(seq_len(nrow(as.matrix(AvailableNodes))), function(x)min(as.matrix(AvailableNodes)[x,]))
      
      #Find the Min Min
      MinMin <- which.min(RowMin)
      
      #Find Out where that max was
      NewParent <- InMST[MinMin][1]
      NewChild <- as.numeric(which(D[InMST[MinMin],] == min(RowMin)))
      
      #In the event of a tie, ensure that only a new child is added
      if(length(NewChild) > 1){
        AlreadyIn <- which(NewChild %in% InMST)
        if(length(AlreadyIn > 0)){
          NewChild <- NewChild[-AlreadyIn][1]
        }else{
          NewChild <- NewChild[1]
        }
      }
      
      NewEdge <- D[NewParent,NewChild]
      
      #Add Them to the current vectors
      
      Parent[i+1] <- NewParent
      Child[i+1] <- NewChild
      Edge[i+1] <- NewEdge
      
      #Update All Pointers
      
      InMST <- c(InMST,NewChild)
      NotInMST <- NotInMST[-which(NotInMST == NewChild)]
      i <- i+1
      InMST
      NotInMST
    }
  }
  
  Ordered <- order(Child)
  temp <- Parent
  Parent <- Child[Ordered]
  Child <- temp[Ordered]
  Weight <- Edge[Ordered]
  
  MST <- data.frame(parent=Parent,child=Child,weight=Weight)
  return(MST)
}