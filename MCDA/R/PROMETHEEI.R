#' PROMETHEE I
#' 
#' The PROMETHEE I constructs preference indices from the criteria evaluations
#' of alternatives and outputs three preference relations (P - preference, I -
#' indifference, R - incomparability) based on the outranking flows between the
#' alternatives.
#' 
#' 
#' @param performanceTable Matrix containing the evaluation table.  Each row
#' corresponds to an alternative, and each column to a criterion.  Rows (resp.
#' columns) must be named according to the IDs of the alternatives (resp.
#' criteria).
#' @param preferenceFunction A vector with preference
#' functions.preferenceFunction should be equal to
#' Usual,U-shape,V-shape,Level,V-shape-Indiff or Gaussian.  The elements are
#' named according to the IDs of the criteria.
#' @param preferenceThreshold A vector containing threshold of strict
#' preference. The elements are named according to the IDs of the criteria.
#' @param indifferenceThreshold A vector containing threshold of indifference.
#' The elements are named according to the IDs of the criteria.
#' @param gaussParameter A vector containing parameter of the Gaussian
#' preference function. The elements are named according to the IDs of the
#' criteria.
#' @param criteriaWeights Vector containing the weights of the criteria.  The
#' elements are named according to the IDs of the criteria.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @return The function returns three matrices: The first one contains the
#' preference relations between the alternatives, the second one contains the
#' indifference relations between the alternatives and the third one contains
#' the incomparability relations between the alternatives.
#' @examples
#' 
#' # The evaluation table
#' 
#' performanceTable <- rbind(
#'   c(1,10,1),
#'   c(4,20,2),
#'   c(2,20,0),
#'   c(6,40,0),
#'   c(30,30,3))
#' rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")
#' colnames(performanceTable) <- c("Price","Time","Comfort")
#' 
#' # The preference functions 
#' preferenceFunction<-c("Gaussian","Level","V-shape-Indiff")
#' 
#' #Preference threshold
#' preferenceThreshold<-c(5,15,3)
#' names(preferenceThreshold)<-colnames(performanceTable)
#' 
#' #Indifference threshold
#' indifferenceThreshold<-c(3,11,1)
#' names(indifferenceThreshold)<-colnames(performanceTable)
#' 
#' #Parameter of the Gaussian preference function
#' gaussParameter<-c(4,0,0)
#' names(gaussParameter)<-colnames(performanceTable)
#' 
#' #weights
#' 
#' criteriaWeights<-c(0.2,0.3,0.5)
#' names(criteriaWeights)<-colnames(performanceTable)
#' 
#' # criteria to minimize or maximize
#' 
#' criteriaMinMax<-c("min","min","max")
#' names(criteriaMinMax)<-colnames(performanceTable)
#' 
#' PROMETHEEI(performanceTable, preferenceFunction,preferenceThreshold,
#'           indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
#' 
#' 
#' @export PROMETHEEI
PROMETHEEI<-function(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
# This function is the  PROMETHEE I partial ranking which  is obtained from the positive
  #and the negative outranking flows. This function returns three matrices P (for Preference relations), I(for indifference relations) and  R(for incomparability relations).
  #Each matrix contains only 0 and 1. 1 (at the position (i,j) ) means that a_i P a_j (in the matrix P), or a_i I a_j (in the matrix I)
  # or a_i R a_j (in the matrix R)  and 0 else.
{ 
numAlt<-dim(performanceTable)[1] # number of alternatives
# Call of the function PROMETHEEOutrankingFlows
outranking<-PROMETHEEOutrankingFlows(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
outrankingflowspos<-outranking[[1]]
outrankingflowsneg<-outranking[[2]]
P<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt) #matrix containig the preference relations between alternatives
I<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt) #matrix containig the indifference relations between alternatives
R<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt) #matrix containig the incomparability relations between alternatives
for (i in (1:numAlt)){
  for (j in (1:numAlt)){
if (((outrankingflowspos[i]>outrankingflowspos[j])&(outrankingflowsneg[i]<outrankingflowsneg[j]))||((outrankingflowspos[i]==outrankingflowspos[j])&(outrankingflowsneg[i]<outrankingflowsneg[j]))||((outrankingflowspos[i]>outrankingflowspos[j])&(outrankingflowsneg[i]==outrankingflowsneg[j])))
      {
  #a_i P a_j
  P[i,j]=1
  }
else if ((outrankingflowspos[i]== outrankingflowspos[j])&(outrankingflowsneg[i]== outrankingflowsneg[j]))
  {
  # a_i I a_j
  I[i,j]=1
  }
  else if (((outrankingflowspos[i]>outrankingflowspos[j])&(outrankingflowsneg[i]>outrankingflowsneg[j]))||((outrankingflowspos[i]<outrankingflowspos[j])&(outrankingflowsneg[i]<outrankingflowsneg[j])))
  {
    #a_i R a_j
    R[i,j]=1
  }
  }
}
rownames(P) <- names(outrankingflowspos)
colnames(P) <- names(outrankingflowspos)
rownames(I) <- names(outrankingflowspos)
colnames(I) <- names(outrankingflowspos)
rownames(R) <- names(outrankingflowspos)
colnames(R) <- names(outrankingflowspos)
list(P=P,I=I,R=R)
}


