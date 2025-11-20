#' Preference indices for the PROMETHEE methods
#' 
#' This function computes the preference indices from a performance table based
#' on the given function types and parameters for each criterion.
#' 
#' 
#' @param performanceTable Matrix containing the performance table.  Each row
#' corresponds to an alternative, and each column to a criterion.  Rows (resp.
#' columns) must be named according to the IDs of the alternatives (resp.
#' criteria).
#' @param preferenceFunction A vector containing the names of the preference
#' functions to be used. preferenceFunction should be equal to Usual, U-shape,
#' V-shape, Level, V-shape-Indiff or Gaussian. The elements of the vector are
#' named according to the IDs of the criteria.
#' @param preferenceThreshold A vector containing thresholds of strict
#' preference. The elements are named according to the IDs of the criteria.
#' @param indifferenceThreshold A vector containing thresholds of indifference.
#' The elements are named according to the IDs of the criteria.
#' @param gaussParameter A vector containing parameters of the Gaussian
#' preference function. The elements are named according to the IDs of the
#' criteria.
#' @param criteriaWeights Vector containing the weights of the criteria.  The
#' elements are named according to the IDs of the criteria.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @return The function returns a matrix containing all the aggregated
#' preference indices.
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
#' 
#' #Preference indices
#' 
#' preferenceTable<-PROMETHEEPreferenceIndices(performanceTable, preferenceFunction,
#'                                             preferenceThreshold, indifferenceThreshold,
#'                                             gaussParameter, criteriaWeights,
#'                                             criteriaMinMax)
#' 
#' 
#' @export PROMETHEEPreferenceIndices
PROMETHEEPreferenceIndices<- function(performanceTable, preferenceFunction, preferenceThreshold, indifferenceThreshold, gaussParameter, criteriaWeights, criteriaMinMax)
  
{ 
  # check the input data
  
  numAlt<-dim(performanceTable)[1] # number of alternatives
  numCrit<-dim(performanceTable)[2] # number of criteria
  
  if (!(is.matrix(performanceTable)))
    stop("wrong performanceTable, should be a matrix")
  
  if (!(is.vector(preferenceFunction)))
    stop("preferenceFunction should be a vector")
  
  for (j in (1:numCrit))
  {
    if (!(preferenceFunction[j] %in% c("Usual","U-shape","V-shape","Level","V-shape-Indiff","Gaussian")))
    {
      stop("wrong preferenceFunction, should be equal to Usual,U-shape,V-shape,Level,V-shape-Indiff or Gaussian")
    }
  }
   
  
  if (!(is.vector(preferenceThreshold)))
    stop("preferenceThreshold should be a vector")
  
  if (!(is.vector(indifferenceThreshold)))
    stop("indifferenceThreshold should be a vector")
  
  if (!(is.vector(gaussParameter)))
    stop("gaussParameter should be a vector")
  
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  # -------------------------------------------------------
  

  preferenceTable<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt)
#Pairwise comparisons of evaluation criteria
  for(i in (1:numAlt)){
    for(j in (1:numAlt)){
      if (i==j)
        preferenceTable[i,j]=0
      else
      {
      for(l in (1:numCrit)){
        d<-performanceTable[i,l]-performanceTable[j,l]
          d1<- -d
#Definition of the six types of preference functions
          if (preferenceFunction[l]=='Usual' & criteriaMinMax[l]=='max'){
            if (d>0){
              Pl=1
#Definition of matrix (numAlt x numAlt) containing the aggregated preference indices 
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='Usual' & criteriaMinMax[l]=='min'){
            if (d1>0){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          else if (preferenceFunction[l]=='U-shape' & criteriaMinMax [l]=='max'){
            if (d>indifferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='U-shape' & criteriaMinMax[l]=='min'){
            if (d1>indifferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            
          }
          
          else if (preferenceFunction[l]=='V-shape' & criteriaMinMax[l]=='max'){
            if (d>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            
            else if ((d<=preferenceThreshold[l])&(d>=0)){
              Pl=d/(preferenceThreshold[l])
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          else if (preferenceFunction[l]=='V-shape' & criteriaMinMax[l]=='min'){
            if (d1>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            else if ((d1<=preferenceThreshold[l])&(d1>=0)){
              Pl=d1/(preferenceThreshold[l])
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='Level' & criteriaMinMax[l]=='max'){
            if (d>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            
            else if ((d<=preferenceThreshold[l])&(d>indifferenceThreshold[l])){
              Pl=0.5
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          else if (preferenceFunction[l]=='Level' & criteriaMinMax[l]=='min'){
            if (d1>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            else if ((d1<=preferenceThreshold[l])&(d1>indifferenceThreshold[l])){
              Pl=0.5
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='V-shape-Indiff' & criteriaMinMax[l]=='max'){
            if (d>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            
            else if ((d<=preferenceThreshold[l])&(d>indifferenceThreshold[l])){
              Pl=(d-indifferenceThreshold[l])/(preferenceThreshold[l]-indifferenceThreshold[l])
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='V-shape-Indiff' & criteriaMinMax[l]=='min'){
            if (d1>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            else if ((d1<=preferenceThreshold[l])&(d1>indifferenceThreshold[l])){
              Pl=(d1-indifferenceThreshold[l])/(preferenceThreshold[l]-indifferenceThreshold[l])
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='Gaussian' & criteriaMinMax[l]=='max'){
            if (d>0){
              Pl=1-exp(-((d^2)/(2*gaussParameter[l]^2)))
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          else if (preferenceFunction[l]=='Gaussian' & criteriaMinMax[l]=='min'){
            if (d1>0){
              Pl=1-exp(-(((d1)^2)/(2*gaussParameter[l]^2)))
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            
          }
    }
    }
    }
  }
  rownames(preferenceTable) <- rownames(performanceTable)
  colnames(preferenceTable) <- rownames(performanceTable)
  return(preferenceTable)
}
