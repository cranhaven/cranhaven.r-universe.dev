feedback.loop.matrix = function(fl, loop){
  if(class(fl) != 'FeedbackLoop'){stop('fl must be a "FeedbackLoop object. See ?feedback.loop')}
  if(missing(loop)){stop('Which loop do you want? Specify loop = ')}
  
  # Initiating the matrix
  n = length(fl$value)
  if(loop > n){stop(cat('There are fewer than ', loop, 
                        ' loops. Choose a number weakly less than ', n, '.', sep = ''))}
  fl.mat = matrix(0, nrow = n, ncol = n)
  
  cords = data.frame(row = NULL, col = NULL)
  for(j in 1:length(fl[[loop + 1]])){
    l = length(fl[[loop+1]][[j]]$index)
    rows = fl[[loop+1]][[j]]$index[1:(l-1)]
    cols = fl[[loop+1]][[j]]$index[2:l]
    cords = rbind(cords, cbind(rows, cols))
  }
  
  for(i in 1:n){
    fl.mat[cords$rows[i], cords$cols[i]] = 1
  }
  fl.mat
}
