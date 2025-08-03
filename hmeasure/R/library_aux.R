#' @export
relabel <- function(labels){
  
  
  if (length(levels(as.factor(labels)))==1){
    stop('Only one class is present in the dataset. Need both classes to be represented.')
  }
  if (length(levels(as.factor(labels)))>2){
    stop('More than two classes present, but code can only handle binary classification.')
  }
  
  labels <- as.factor(as.character(labels))
  input.labels <- levels(labels)
  
  
  cond.temp <- (	identical(input.labels,c('case','non-case')) |
                   identical(input.labels,c('Case','Non-case')) |
                   identical(input.labels,c('case','noncase'))  |
                   identical(input.labels,c('Case','Non-case')) )
  
  if (cond.temp) {
    levels(labels) <- c('1', '0')
    message('Class labels have been switched from (',
            paste(input.labels[1],input.labels[2], sep=','), ') to (',
            paste('1', '0', sep=','), ')')
    labels <- as.factor(labels)
    labels <- 2-as.numeric(labels) # turn into numeric array of 0s and 1s
  } else {
    levels(labels) <- c('0', '1')
    if (!(identical(input.labels,c('0', '1')))){
      message('Class labels have been switched from (',
              paste(input.labels[1],input.labels[2], sep=','), ') to (',
              paste('0', '1', sep=','), ')')
    }
    labels <- as.factor(labels)
    labels <- as.numeric(labels)-1 # turn into numeric array of 0s and 1s
    
  }
  
  return(labels)
}


release_questions <- function() {
  c(
    "Have you updated the bibliography in the vignette?"
  )
}
