missingDataDealer<-function(data, XSelected, YSelected){

  Y<-data[,YSelected]
  X<-data[,XSelected]

  #Missing data in the outcome
  if(any(is.na(Y))){
    warning("There are missing values on your outcome variable. These cases are removed")
    index<-is.na(Y)
    data<-data[!index,]
  }

  #if(method == "Delete"){
  if(any(is.na(X))){
    warning("There are missing values on the predictors. The cases with missing values are removed. If you want those cases to be included in the model, deal with the missing values (e.g., imputation) before running Exact Trees.")
    indexNA<-!complete.cases(X)
    data<-data[!indexNA,]
  }
  #}

  # if(method == "Category"){
  #
  #
  #   nvars <- NCOL(X)
  #   for (i in 1:nvars) {
  #     if(any(is.na(X[i,]))){
  #       warning("There are missing values on your predictors variable.")
  #
  #       #How do we want to deal with them?
  #
  #
  #       #If X is numeric
  #
  #
  #
  #
  #       #If X is ordinal
  #
  #
  #
  #
  #       #If X is nominal
  #
  #
  #     }#End if
  #   }#End for
  #
  # }#End Category

  return(data)


}
