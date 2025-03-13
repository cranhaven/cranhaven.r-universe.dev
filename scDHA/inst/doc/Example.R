## ----eval=FALSE---------------------------------------------------------------
#  #Install devtools:
#  utils::install.packages('devtools')
#  
#  #Install the package from GitHub:
#  devtools::install_github('duct317/scDHA')
#  #With manual and vignette:
#  devtools::install_github('duct317/scDHA', build_manual = T, build_vignettes = T)
#  #Or from CRAN:
#  install.packages("scDHA")
#  
#  #When the package is loaded, it will check for C++ libtorch
#  library(scDHA)
#  #libtorch can be installed using:
#  torch::install_torch()

## ----eval=FALSE---------------------------------------------------------------
#  if (!requireNamespace("mclust", quietly = TRUE)) install.packages("mclust")

## ----eval=FALSE---------------------------------------------------------------
#  library(scDHA)
#  #Load example data (Goolam dataset)
#  data("Goolam")
#  
#  #Get data matrix and label
#  data <- t(Goolam$data); label <- as.character(Goolam$label)
#  
#  #Log transform the data
#  data <- log2(data + 1)

## ----eval=FALSE---------------------------------------------------------------
#  #Generate clustering result, the input matrix has rows as samples and columns as genes
#  result <- scDHA(data, seed = 1)
#  
#  #The clustering result can be found here
#  cluster <- result$cluster
#  
#  #Calculate adjusted Rand Index using mclust package
#  ari <- round(mclust::adjustedRandIndex(cluster,label), 2)
#  print(paste0("ARI = ", ari))

## ----eval=FALSE---------------------------------------------------------------
#  #Generate 2D representation, the input is the output from scDHA function
#  result <- scDHA.vis(result, seed = 1)
#  
#  #Plot the representation of the dataset, different colors represent different cell types
#  plot(result$pred, col=factor(label), xlab = "scDHA1", ylab = "scDHA2")

## ----eval=FALSE---------------------------------------------------------------
#  #Cell stage order in Goolam dataset
#  cell.stages <- c("2cell", "4cell", "8cell", "16cell", "blast")
#  
#  #Generate pseudo-time for each cell, the input is the output from scDHA function
#  result <- scDHA.pt(result, start.point = 1, seed = 1)
#  
#  #Calculate R-squared value representing correlation between inferred pseudo-time and cell stage order
#  r2 <- round(cor(result$pt, as.numeric(factor(label, levels = cell.stages)))^2, digits = 2)
#  
#  #Plot pseudo-temporal ordering of cells in Goolam dataset
#  plot(result$pt, factor(label, levels = cell.stages), xlab= "Pseudo Time", ylab = "Cell Stages", xaxt="n", yaxt="n")
#  axis(2, at=1:5,labels=cell.stages, las=2)
#  text(x = 1, y = 4.5, labels = paste0("R2 = ", r2))

## ----eval=FALSE---------------------------------------------------------------
#  #Split data into training and testing sets
#  set.seed(1)
#  idx <- sample.int(nrow(data), size = round(nrow(data)*0.75))
#  
#  train.x <- data[idx, ]; train.y <- label[idx]
#  test.x <- data[-idx, ]; test.y <- label[-idx]
#  
#  #Predict the labels of cells in testing set, the input matrices have rows as samples and columns as genes
#  prediction <- scDHA.class(train = train.x, train.label = train.y, test = test.x, seed = 1)
#  
#  #Calculate accuracy of the predictions
#  acc <- round(sum(test.y == prediction)/length(test.y), 2)
#  print(paste0("Accuracy = ", acc))

