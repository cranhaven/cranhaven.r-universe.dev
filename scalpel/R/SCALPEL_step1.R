#The following functions are needed to perform Step 1 of SCALPEL

#create candidate spatial components for a frame
#based on finding connected components of size 25 or greater
getSpatialComponents = function(frameVec, videoHeight, connectivity=4, minSize, maxSize, maxWidth, maxHeight) {

  #use ConnCompLabel function from SDMTools package to find connected components
  #connComp = SDMTools::ConnCompLabel(Matrix(frameVec, nrow=videoHeight, sparse=T))
  #code from SDMTools is now incorporated directly into the package b/c SDMTools is no longer maintained on CRAN
  matForComp = Matrix(frameVec, nrow=videoHeight, sparse=T)
  attrib = attributes(matForComp)
  #run the connected component labelling
  connComp = .Call('ccl',as.matrix(matForComp),PACKAGE='scalpel')
  attributes(connComp) = attrib
  
  #remove connected components of size smaller than 25 pixels
  smallComp = as.numeric(names(which(table(as.vector(connComp))<25)))
  connComp[which(as.vector(connComp) %in% smallComp)] = 0

  #relabel components to be consecutive integers
  oldLabels = sort(unique(as.vector(connComp)))
  if (length(oldLabels)>1) {
    oldLabels = oldLabels[2:length(oldLabels)] #remove 0 label
    for (i in 1:length(oldLabels)) connComp[which(connComp==oldLabels[i])] = i
  }
  labelVec = as.vector(connComp)

  #create candidate spatial component matrix (for this frame)
  spatialComp = Matrix(0, nrow=length(frameVec), ncol=max(connComp), sparse=T)
  if (max(connComp)>0) {
    for (comp in 1:max(connComp)) {
      spatialComp[which(labelVec==comp),comp] = 1
    }

    #filter out candidate components that are smaller than minSize or larger than maxSize
    # or that have a width greater than maxWidth or height greater than maxHeight
    sizes = colSums(spatialComp)
    remove = which(sizes>maxSize | sizes<minSize)
    if (length(remove)>0) {
      spatialComp = spatialComp[,-remove,drop=F]
      sizes = sizes[-remove]
    }
    toCheck = which(sizes>min(c(maxWidth, maxHeight)))
    if (length(toCheck)>0) {
      sizeOK = apply(spatialComp[,toCheck,drop=F], 2, checkWidthHeight,
                     videoHeight=videoHeight, maxWidth=maxWidth, maxHeight=maxHeight)
      remove = toCheck[which(sizeOK==0)]
      if (length(remove)>0) {
        spatialComp = spatialComp[,-remove,drop=F]
      }
    }
  }
  return(spatialComp)
}

#checks whether the width and height of ROI are <= maxWidth and maxHeight pixels, respectively
#returns 1 if they are, 0 if not
checkWidthHeight = function(vec, videoHeight, maxWidth, maxHeight) {
  mat = Matrix(vec, nrow=videoHeight, sparse=T)
  colsFilled = colSums(mat)
  rowsFilled = rowSums(mat)
  width = max(which(colsFilled>0)) - min(which(colsFilled>0))
  height = max(which(rowsFilled>0)) - min(which(rowsFilled>0))
  return(ifelse(width<=maxWidth & height<=maxHeight, 1, 0))
}
