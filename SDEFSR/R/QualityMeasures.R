

#------------------------------------------------------------------------
# 
#        Returns the coverage ( N(Cond) / Ns )  of the rule
#        
#        Where:
#        ? N(Cond) is the number of examples that matches the antecedent part of the rule
#        ? Ns is the number of examples in the dataset
#        
#------------------------------------------------------------------------

# 'x' is a list of values obtained by the .getValuesForQualityMeasures() function
.coverage <- function(x){
  c <- x[[1]] / x[[4]]
  if(is.nan(c)){
    0
  } else {
    c
  }
}


#------------------------------------------------------------------------
# 
#        Return the support ( N(TargetValue ? Cond) / Ns )  of the rule
#        
#        Where:
#        ? N(TargetValue ? Cond)  is the number of examples correctly covered
#        ? Ns is the number of examples in the dataset
#        
#------------------------------------------------------------------------


.Csupport <- function(x){
  c <- x[[2]] / x[[4]]
  if(is.nan(c)){
    0
  } else {
    c
  }
}


# Fuzzy Support

.Fsupport <- function(x){
  c <- x[[11]] / x[[4]]
  if(is.nan(c)){
    0  
  } else {
    c
  }
}


#Fuzzy Local Support for SDIGA
#
.FLocalSupport <- function(x){
  c <- x[[12]] / x[[9]]
  if(is.nan(c)){
    0  
  } else {
    c
  }
}

# Confidence
# 
.confidence <- function(x){
  
  c <- x[[2]] / x[[1]]
  if(is.nan(c)){
    0
  } else {
    c
  }
  
}

#Fuzzy confidence
#
.fuzzyConfidence <- function(x){
  c <- x[[11]] / x[[10]]
  if(is.nan(c)){
    0
  } else {
    c
  }
}


# Unusualness
# 
.unusualness <- function(x){
  .coverage(x) * ( .confidence(x) - (x[[5]] / x[[4]]) )
}

# Normalized Unusualness
# 
.norm_unusualness <- function(x){
  .coverage(x) * .confidence(x)
  
}

# Sensitivity (TPR)
# 
.sensitivity <- function(x){
  c <- x[[2]] / x[[5]]
  if(is.nan(c)){
    0
  } else {
    c
  }
}

# Accuracy QM
# 
.accuracy <- function(x){
  c <-  (x[[2]] + 1) / (x[[1]] + NROW(x[[7]]))
  if(is.nan(c)){
    0
  } else {
    c
  }
}


# Significance
# 
.significance <- function(x){
  cove <- .coverage(x = x)
  if(cove > 0){
    SumsingClase <- 0
    for(i in 1:length(x[[6]])){
      if(x[[6]][i] > 0 && x[[7]][i] > 0){
        SumsingClase <- SumsingClase + x[[6]][i] * log10(x = (x[[6]][i] / (x[[7]][[i]] * cove)  ))
      }
    }
    
    2 * SumsingClase
  
  } else {
    0
  }
}


# Local Support for SDIGA
# 
.LocalSupport <- function(x){
  
  c <- x[[8]] / x[[9]]
  if(is.infinite(c)){
    0
  } else {
    c
  }
}





