#' MRSort with imprecise evaluations
#' 
#' This method is an extension of the classical MRSort, that allows the
#' handling of problems where the decision alternatives contain imprecise or
#' even missing evaluations. Unlike MRSort, where an alternative is assigned to
#' one category, MRSortInterval offers the possibility of assigning an
#' alternative to one or more neighboring categories.
#' 
#' 
#' @param performanceTable Two-dimmensionnal list containing the performance
#' table.  Each row corresponds to an alternative, and each column to a
#' criterion.  Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).  This list may contain imprecise performances
#' of alternatives on the criteria, represented by interval evaluations, as
#' well as missing performances.
#' @param categoriesLowerProfiles Matrix containing, in each row, the lower
#' profiles of the categories.  The columns are named according to the
#' criteria, and the rows are named according to the categories except of the
#' last one.
#' @param categoriesRanks A vector containing the ranks of the categories (1
#' for the best, with higher values for increasingly less preferred
#' categories). The vector needs to be named with the categories names, whereas
#' the ranks need to be a range of values from 1 to the number of categories.
#' @param criteriaWeights Vector containing the weights of the criteria.  The
#' elements are named according to the IDs of the criteria.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).
#' @param majorityThresholdPes The cut threshold for the pessimistic
#' concordance relation.
#' @param majorityThresholdOpt The cut threshold for the optimistic concordance
#' relation.
#' @return The function returns a list containing the assignments of the
#' alternatives to all possibles categories.
#' @examples
#' 
#' # the performance table
#' 
#' performanceTable <- as.list(numeric(6*5))
#' dim(performanceTable)=c(6,5)
#' performanceTable[[1,1]]<-0
#' performanceTable[[1,2]]<-0
#' performanceTable[[1,3]]<-0
#' performanceTable[[1,4]]<-0
#' performanceTable[[1,5]]<-0
#' performanceTable[[2,1]]<-0
#' performanceTable[[2,2]]<-0
#' performanceTable[[2,3]]<-1
#' performanceTable[[2,4]]<-0
#' performanceTable[[2,5]]<-0
#' performanceTable[[3,1]]<-0
#' performanceTable[[3,2]]<-0
#' performanceTable[[3,3]]<-2
#' performanceTable[[3,4]]<-0
#' performanceTable[[3,5]]<-0
#' performanceTable[[4,1]]<-0
#' performanceTable[[4,2]]<-0
#' performanceTable[[4,3]]<-0:1
#' performanceTable[[4,4]]<-0
#' performanceTable[[4,5]]<-0
#' performanceTable[[5,1]]<-0
#' performanceTable[[5,2]]<-0
#' performanceTable[[5,3]]<-NA
#' performanceTable[[5,4]]<-0
#' performanceTable[[5,5]]<-0
#' performanceTable[[6,1]]<-0
#' performanceTable[[6,2]]<-0
#' performanceTable[[6,3]]<-0
#' performanceTable[[6,4]]<-0
#' performanceTable[[6,5]]<-NA
#' 
#' rownames(performanceTable)<-c("a1","a2","a3","a4","a5","a6")
#' colnames(performanceTable)<-c("c1","c2","c3","c4","c5")
#' 
#' # lower profiles of the categories (best category in the first position of the list)
#' 
#' categoriesLowerProfiles <- rbind(c(1,1,1,1,1),c(0,0,0,2,2))
#' colnames(categoriesLowerProfiles) <- colnames(performanceTable)
#' 
#' rownames(categoriesLowerProfiles)<-c("Medium","Good")
#' 
#' categoriesRanks <-c(1,2,3)
#' 
#' names(categoriesRanks) <- c("Good","Medium","Bad")
#' 
#' # weights
#' 
#' criteriaWeights <- c(1/5,1/5,1/5,1/5,1/5)
#' names(criteriaWeights) <- colnames(performanceTable)
#' 
#' #pessimistic and optimistic majority thresholds
#' majorityThresholdPes=majorityThresholdOpt=3/5
#' 
#' # criteria to minimize or maximize
#' 
#' criteriaMinMax <- c("min","min","min","max","max")
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' #MRSortInterval
#' 
#' assignments<-MRSortInterval(performanceTable,categoriesLowerProfiles,
#'                             categoriesRanks,criteriaWeights,
#'                             criteriaMinMax,majorityThresholdPes,
#'                             majorityThresholdOpt)
#' 
#' @export MRSortInterval
MRSortInterval<-function(performanceTable,categoriesLowerProfiles,categoriesRanks,criteriaWeights,criteriaMinMax,majorityThresholdPes,majorityThresholdOpt){
  
  numcrit<-dim(performanceTable)[2]
  numalt<-dim(performanceTable)[1]
  #Definition of pessimistic and optimistic  versions of an alternative
  performanceTablepes=performanceTableopt<-as.list(rep(0,numalt*numcrit),c(numalt,numcrit))
  dim(performanceTablepes)=dim(performanceTableopt)<- c(numalt,numcrit)
  for (i in (1:numalt)){
    for (j in (1:numcrit)){
      if(is.numeric(performanceTable[[i,j]])) 
      {
        if (criteriaMinMax[j] == "max")
        {
          performanceTablepes[[i,j]]<-range(performanceTable[[i,j]])[1]
          performanceTableopt[[i,j]]<-range(performanceTable[[i,j]])[2]
        }
        else
        {
          performanceTablepes[[i,j]]<-range(performanceTable[[i,j]])[2]
          performanceTableopt[[i,j]]<-range(performanceTable[[i,j]])[1]
        }
      }
      
      else 
      {
        if (criteriaMinMax[j] == "max")
        {
          performanceTablepes[[i,j]]<- -Inf
          performanceTableopt[i,j]<- Inf
        }
        else
        {
          performanceTablepes[[i,j]]<- Inf
          performanceTableopt[i,j]<- -Inf
        }
      }
      
    }
  }
  performanceTableopt<-matrix(as.numeric(unlist(performanceTableopt)),numalt,numcrit)
  performanceTablepes<-matrix(as.numeric(unlist(performanceTablepes)),numalt,numcrit)
  rownames(performanceTablepes)=rownames(performanceTableopt)<-rownames(performanceTable)  
  colnames(performanceTablepes)=colnames(performanceTableopt)<-colnames(performanceTable)
  #The lower bound and the upper bound for an assignment using its pessimistic and its optimistic versions respectively  
  assignmentspes<-MRSort(performanceTablepes,categoriesLowerProfiles,categoriesRanks,criteriaWeights,criteriaMinMax,majorityThresholdPes)
  assignmentsopt<-MRSort(performanceTableopt,categoriesLowerProfiles,categoriesRanks,criteriaWeights,criteriaMinMax,majorityThresholdOpt)
  hpes<-NULL
  hopt<-NULL
  for (i in 1:length(assignmentsopt))
  {
    hpes[i]<-categoriesRanks[assignmentspes[i]]
    hopt[i]<-categoriesRanks[assignmentsopt[i]]
  }
  assignments<-mapply(seq,hpes,hopt)
  names(assignments)<-rownames(performanceTable)
  
  assignmentsfinal<-vector(mode = "list", length = length(assignments))
  
  for (i in 1:length(assignments))
  {
    for (j in 1:length(assignments[[i]]))
    {
      assignmentsfinal[[i]][j]<-names(categoriesRanks[assignments[[i]][j]])
    }
  }
  names(assignmentsfinal)<-rownames(performanceTable)
  return(assignmentsfinal)
}
