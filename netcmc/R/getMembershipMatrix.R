getMembershipMatrix = function(individualID, alters) {
  
  if(nrow(alters) != nrow(individualID)) {
    stop("Error: The rows of the parameter inputs differ! They should all have the same number of rows!")
  }
  
  individualIDList = individualID[, 1]
  altersList = c()
  for(i in 1:ncol(alters)){
    altersList = append(altersList, alters[, i])
  }
  altersAndNotEgos = unique(altersList[!altersList %in% individualIDList])
  altersAndNotEgos = altersAndNotEgos[! altersAndNotEgos %in% NA]
  altersInEgos = unique(altersList[altersList %in% individualIDList])
  
  for(i in 1:ncol(alters)){
    alters[which(altersAndNotEgos %in% alters[, i]), i] = NA
  }
  
  uniqueAlters = unique(altersList)
  numberOfUniqueAlters = length(altersInEgos)
  numberOfEgos = length(individualIDList)
  membershipMatrix = data.frame(matrix(0, numberOfEgos, numberOfUniqueAlters, dimnames = list(c(), sprintf("Alter%s", altersInEgos))))
  for(i in 1:nrow(alters)){
    for(j in 1:ncol(alters)) {
      alter = alters[i, j]
      if (is.na(alter)) {
        # Nothing should happen. The individual did not nominate someone in this instance.
      } else {
        membershipMatrix[i, which(altersInEgos == alter)] = 1
      }
    }
  }
  
  rowNormalizedMembershipMatrix = membershipMatrix
  for(i in 1:nrow(membershipMatrix)){
    rowSum = sum(membershipMatrix[i, ])
    if(rowSum != 0) {
      for(j in 1:ncol(membershipMatrix)) {
        rowNormalizedMembershipMatrix[i, j] = membershipMatrix[i, j] / rowSum
      }
    }
  }
  
  return(list(alters = alters, membershipMatrix = membershipMatrix, rowNormalizedMembershipMatrix = rowNormalizedMembershipMatrix))
  
}

