#' This function preprocess the data for the Optimal Trees function.
#'
#'@param formula a description of the model to be fit. The format is \code{Y1 + \dots + Yn ~  X1 + \dots + Xn},
#'   where the variable before ~ represents the dependent variables and the variables after
#'   the ~ are the independent variables.
#' @param data Dataset to be analyzed. If data contains ordinal variables, order the factor levels
#' before using this function. Otherwise, the factors will be ordered alphabetically and the results will not be correct.
#' @param Names names of the variables in the data. If empty, it uses the column names of Data
#' @param YSelected index corresponding to the first Selected column.
#' @param XSelected Indices corresponding to the selected columns that will form X
#' @param map [ori vars] sets for each variable whether the tree will depict
#'                       thresholds based upon original values (var number), or recoded values (0).
#' @param original [TRUE/FALSE] sets for each variable whether the tree will depict
#'                       thresholds based upon original values (true), or recoded values (false).
#' @param round [factors] autorecoding based upon rounded categories; the categories will
#'                       be round after multiplying with the factor, and divided by the
#'                       factor after rounding. Set round=0 for variables that are not
#'                       rounded (e.g. discretized variables).
#' @param discretize [ncat] optimal discretization while minimizing SS within
#'                       a group, set ncat=0 for each variable that is to
#'                       be left unaltered
#' @param verbose logical, if TRUE, prints information about the progress of the algorithm.
#'
#' @details The function selects the Y and X variables according to the inputs and also returns
#' Desc, a list containing variable cutting points and variable names. These 3 elements are
#' required by the OptimalTrees function.
#'
#' @return Returns the following 3 elements:
#'  \item{Y}{dataset containing the Y variables.}
#'  \item{X}{dataset containing the X variables.}
#'  \item{Desc}{List containing the variable names and their corresponding cutting points.}
#'
#'
#' @seealso \code{\link{summary.ETree}}, \code{\link{ETree}},
#'   \code{\link{plot.ETree}}, \code{\link{predict.ETree}}
#'
#' @examples
#' data(mtcars)
#' dataSelection<-SelectVar( mpg ~ cyl + hp + wt, data = mtcars, discretize= c(0, 10, 10))
#'
#' @keywords tree
#' @keywords exact
#'
#' @importFrom pracma histc repmat
#' @importFrom formula.tools lhs.vars rhs.vars
#' @importFrom stats model.frame complete.cases
#'
#' @export
SelectVar<-function(formula=NULL, data, Names=NULL, YSelected=NULL, XSelected=NULL, map=NULL, original=NULL, round=NULL, discretize=NULL, verbose = TRUE){



  if(is.null(formula)&(is.null(YSelected) | is.null(XSelected))){
    stop("Fill in the formula or YSelected and XSelected")
  }


  if(!is.null(formula)){

    Lside<- lhs.vars(formula)
    Rside<- rhs.vars(formula, data=data)
    YSelected <- which(colnames(data) %in% Lside)
    XSelected <- which(colnames(data) %in% Rside)
  }

  isFactor<-c() #Change 11/08/2022
  for (i in 1:length(XSelected)) {
    isFactor[i]<-ifelse(is.factor(data[,XSelected[i]]), TRUE, FALSE)
  }

  if(any(isFactor)){
    warning("The method for nominal variables is still under development.")
  }

  XType<-as.integer(isFactor)


  if(is.null(map)+is.null(original)<1){
    stop("Use only one argument between map and original.")
  }



  if(length(round)==1){
    round<-rep(round,length(XSelected))
  }

  if(length(discretize)==1){
    discretize<-rep(discretize,length(XSelected))
  }



  Mapped <- FALSE
  Rounded <- FALSE
  RFactor<-rep(0, length(XSelected))
  Discretized <- FALSE
  XMap<-rep(0, length(XSelected))
  NCat<-rep(0, length(XSelected))
  if(is.null(Names)){
    Names <- colnames(data)
  }




  if (!is.null(map)){
    XMap <- map
    Mapped <- TRUE
  }

  if(!is.null(original)){
    Mapped <-TRUE
    XMap <- XSelected
    XMap[which(original==FALSE)]<-0
  }

  if(!is.null(round)){
    Rounded <- TRUE
    RFactor <- round
  }


  if(!is.null(discretize)){
    Discretized <- TRUE
    NCat <- discretize
  }


  DataOrig<-data

  #Deal with missing values
  data<-missingDataDealer(data, XSelected, YSelected)



  # start processing of data
  n<-dim(data)[1]
  m<-length(YSelected) #dim(YSelected)[2]
  Y<-matrix(0,nrow = n, ncol = m)

  for(y in 1:m){
    Y[,y]<-data[,YSelected[y]]
  }


  m<-length(XSelected) #dim(XSelected)[2]
  X<-matrix(0,nrow = n, ncol = m)
  Desc<-list() #matrix(nrow = m,ncol = 2)#cell(m,2); #it is more a list with 2 dimensions
  length(Desc)<-2*m




  for(x in 1:m){
    V<-data[,XSelected[x]] #predictor is selected

    if(isFactor[x]){

      #New change 17/03/2026: Order the factor based on the mean of Y.  #New change if factor 11/08/2022
      # Note: The algorithm is not ready for this.
      # mean_cat<-rep(0, length(levels(data[,XSelected[x]])))
      # for (cat in 1:length(levels(data[,XSelected[x]]))) {
      #   mean_cat[cat] <- mean(Y[data[,XSelected[x]]==(levels(data[,XSelected[x]])[cat])])
      # }
      # ordCat<-order(mean_cat)



      #X and Desc

      #Check 28/05/2026
      Desc[[x]]<- suppressWarnings(matrix(as.numeric(levels(data[,XSelected[x]])), ncol = 1))

      if(all(is.na(Desc[[x]]))){
        Desc[[x]]<- matrix(as.numeric(1:length(levels(data[,XSelected[x]]))), ncol = 1)
      }


      #Desc[[x]]<- matrix(levels(data[,XSelected[x]])[ordCat], ncol = 1)

      # New: Desc[[x]]<- matrix(num_levels, ncol = 1)
      Desc[[m+x]]<-Names[XSelected[x]]

      # old: X[,x]<- as.numeric(data[,XSelected[x]])

      #Not ready yet:
      #levels(data[,XSelected[x]])<-ordCat
      #X[,x]<- as.numeric(as.factor(data[,XSelected[x]]))

      #Not all can be transformed to numeric
      #If it cannot be transformed to numeric, we will use the order of the categories
      #Use a try-catch to fix it
      tryCatch({
        X[,x]<- as.numeric(data[,XSelected[x]])
      }, warning = function(w) {
        X[,x]<- as.numeric(as.factor(data[,XSelected[x]]))
      }, error = function(e) {
        levels(data[,XSelected[x]])<-1:length(levels(data[,XSelected[x]]))
        X[,x]<- as.numeric(as.factor(data[,XSelected[x]]))
      })




    }else{ #if not factor

      if(Rounded & RFactor[x]>0){
        V<-round(V*RFactor[x])/RFactor[x]
      }
      if(Discretized & NCat[x]>1){
        if(verbose){
          print(paste0('optimal discretization, N= ',n,' NCat= ',NCat[x]))
        }
        if(NCat[x]==2){
          DPOPout<-DPOP(V,3, verbose=verbose)
        }else{
          DPOPout<-DPOP(V,NCat[x], verbose=verbose)
        }

        H<-DPOPout$H
        P<-DPOPout$P
        I<-DPOPout$I
        measure<-DPOPout$measure
        Heap<-DPOPout$Heap
        if(NCat[x]==2){
          X[,x]<-GetOP(I,P[2,1])
        }else{
          X[,x]<-GetOP(I,P[NCat[x],])
        }
        M<-GetCodeMap(X[,x],data[,XSelected[x]])
      }else{
        AutoRecOut<-AutoRecode(V)
        X[,x]<-AutoRecOut$O
        M<-AutoRecOut$M
      }
      if(Mapped & (XMap[x]>0)){
        Desc[[x]]<-GetCodeMap(X[,x],data[,XMap[x]])
        Desc[[m+x]]<-Names[XMap[x]]
      }else{
        Desc[[x]]<-M
        Desc[[m+x]]<-Names[XSelected[x]]
      }

    } #end if not factor


  } #end for each predictor



  #show selection

  if(verbose){

    for(y in 1:NCOL(YSelected)){
      if(m==1){
        print(paste0('Y = ',Names[YSelected[y]]))
      }else{
        print(paste0('Y', y,' = ',Names[YSelected[y]]))
      }
    }

    for(x in 1:m){
      # if(isFactor[x]){
      #   Line <- paste0('X',x,' = ',Names[XSelected[x]],' NCat = ',length(levels(data[,XSelected[x]])))
      # }else{
      #   Line <- paste0('X',x,' = ',Names[XSelected[x]],' NCat = ',max(X[,x]))
      # }
      Line <- paste0('X',x,' = ',Names[XSelected[x]],' NCat = ',max(X[,x]))

      if (Mapped & (XMap[x]>0)){
        Line <- paste0(Line, ', Mapped on ',Names[XMap[x]])
      }
      if(Rounded & (RFactor[x]>0)){
        Line <- paste0(Line,', Rounded, factor = ',RFactor[x])
      }
      if(Discretized & (NCat[x]>0)){
        Line <- paste0(Line,', Optimally Discretized, max cat = ',NCat[x])
      }
      print(Line)
    }
  }

  colnames(Y)<-Names[YSelected]
  colnames(X)<-Names[XSelected]

  return(list(Y=Y, X=X, XType = XType, Desc=Desc, DataOrig=DataOrig))

}
