getTotalAltersByStatus = function(individualID, status, alters) {
  
  if(ncol(individualID) != 1 || ncol(status) != 1) {
    stop("Error: individualID and status must only have one column")
  }
  
  if(nrow(status) != nrow(individualID) || nrow(alters) != nrow(individualID) || nrow(alters) != nrow(status)) {
    stop("Error: The rows of the parameter inputs differ! They should all have the same number of rows!")
  }
  
  numberOfUniqueStatuses = nrow(unique(status))
  totalAltersByStatusColumnNames = c()
  
  for(i in 1:numberOfUniqueStatuses){
    totalAltersByStatusColumnNames[i] = as.character(unique(status)[i, 1])
  }
  
  if(NA %in% totalAltersByStatusColumnNames) {
    totalAltersByStatusColumnNames = totalAltersByStatusColumnNames[!is.na(totalAltersByStatusColumnNames)]
    numberOfUniqueStatuses = numberOfUniqueStatuses - 1
  }
  
  totalAltersByStatusColumnNames = c(totalAltersByStatusColumnNames, NA)
  
  totalAltersByStatus = data.frame(matrix(0, nrow(individualID), numberOfUniqueStatuses + 1, dimnames = list(c(), sprintf("numberOfAltersWithStatus%s", totalAltersByStatusColumnNames))))
  
  for(i in 1:nrow(individualID)) {
    for(j in 1:ncol(alters)) {
      currentAlter = alters[i, j]
      if (is.na(currentAlter)) {
        # Nothing should happen. The individual did not nominate someone in this instance.
      } else if(!(currentAlter %in% individualID[, 1])) {
        totalAltersByStatus[i, numberOfUniqueStatuses + 1] = totalAltersByStatus[i, numberOfUniqueStatuses + 1] + 1
      } else if(is.na(status[which(currentAlter == individualID), 1])) {
        totalAltersByStatus[i, numberOfUniqueStatuses + 1] = totalAltersByStatus[i, numberOfUniqueStatuses + 1] + 1
      } else {
        currentAlterStatuses = status[which(currentAlter == individualID), 1]
        columnOfAlterStatus = which(currentAlterStatuses == totalAltersByStatusColumnNames)
        totalAltersByStatus[i, columnOfAlterStatus] = totalAltersByStatus[i, columnOfAlterStatus] + 1
      }
    }
  }
  
  return(totalAltersByStatus)
}
