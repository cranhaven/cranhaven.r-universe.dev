f.influence.total = function(io){
  # Checking if we have an InputOuput object
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  
  # Getting started
  n = dim(io$L)[1]
  output = matrix(0, nrow = n, ncol = n)
  if(n >= 150){
    progress = round(seq(1,n, length = 40))
    cat('|========================================|\n|')
    update = c('-','-','-','-','-','-','-','-','-','-','-','-','-','-',' ',
               'i','o','a','n','a','l','y','s','i','s',
               ' ','-','-','-','-','-','-','-','-','-','-','-','-','-','-')
    k = 0
    for(i in 1:n){
      for(j in 1:n){
        output = output + f.influence(io, i, j)
      }
      if(i %in% progress){
        k = k + 1
        cat(update[k])
      }
    }
    cat('|\n')
    cat('|========================================|\n|')
  } else {
    for(i in 1:n){
      for(j in 1:n){
        output = output + f.influence(io, i, j)
      }
    }
  }
  output
}
