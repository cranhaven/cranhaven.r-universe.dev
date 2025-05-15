###############################################################
###############################################################
normalizeData2 <- function(data){
  features <- apply(data, MARGIN = 2,
                    FUN = function(x){
                      resx <- (x - min(x)) / (max(x) - min(x))
                      return(resx)
                    })
  data_aux <- as.data.frame(features)
  return(data_aux)
}

###############################################################
###############################################################
forecast <- function(prediccion, real, NS){
  forest <- logical(length(prediccion))
  dif <- abs(prediccion-real)
  idx_NS <- which(dif > NS)
  # if are different: True
  forest[idx_NS] <- T
  return(forest)
}

###############################################################
###############################################################
sort_DROP2RT <- function(data, D = 0.1) {
  Y <- data[,ncol(data)]
  num_instances <- length(Y)
  distances <- matrix(0, nrow = num_instances, ncol = num_instances)
  
  # Calculate distances between instances
  for (i in 1:num_instances) {
    for (j in 1:num_instances) {
      # print(i , j)
      distances[i, j] <- abs(Y[i] - Y[j])
    }
  }
  # Find the closest enemies for each instance based on the specified condition
  closest_enemies <- vector("numeric", length = num_instances)
  for (i in 1:num_instances) {
    closest_enemy_distance <- Inf
    for (j in 1:num_instances) {
      if (i != j && abs(Y[i] - Y[j]) > D && distances[i, j] < closest_enemy_distance) {
        closest_enemy_distance <- distances[i, j]
        closest_enemies[i] <- j
      }
    }
  }
  
  # Sort instances based on the distance to their closest enemy
  sorted_indices <- order(closest_enemies)
  sorted_data <- data[sorted_indices, ]
  rownames(sorted_data) <- 1:nrow(sorted_data)
  return(sorted_data)
}

###############################################################
###############################################################
normalKFCV <- function(data_, k_){
  
  part_aux <- crossv_kfold(data = data_, k = k_)
  
  part <- list(train = list(NULL), test = list(NULL))
  for(i in 1:k_){
    part$test[[i]] <- part_aux$test[[i]]$idx
    part$train[[i]] <- part_aux$train[[i]]$idx
    
    if(length(part$test[[i]])+length(part$train[[i]]) != nrow(data_)){
      message("===============> ERROR!!")
      stop("===============> Error in SCV - Normal")
      return(NULL)
    }
  }
  return(part)
}

###############################################################
###############################################################
perform_kknn <- function(x, formu, k) {
  prediction <- sapply(1:nrow(x),function(i){
    kknn::kknn(formula = formu, train = x[-i,], test = x[i,], k = k,
               kernel = "rectangular")$fitted.values
  })
  return(prediction)
}