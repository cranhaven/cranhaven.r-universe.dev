## ---- eval=FALSE--------------------------------------------------------------
#  library(devtools)
#  install_github("labinfo-lncc/mnmer", ref="main")

## ---- echo=FALSE--------------------------------------------------------------
score <- 30
paste0("Warning: Sequence has a proportion of N + IUPAC bases = ", score, "%")

## ---- eval=FALSE--------------------------------------------------------------
#  library("mnmer")
#  dir <-system.file("extdata", package="mnmer")

## ---- eval=FALSE--------------------------------------------------------------
#  human <-readNumFASTA((file.path(dir, "human_vir.fasta")), 10,TRUE,0.50)
#  plant <-readNumFASTA((file.path(dir, "plant_vir.fasta")), 10,TRUE,0.50)

## ---- eval=FALSE--------------------------------------------------------------
#  human_02mer <- mnmer(human,2,0)
#  plant_02mer <- mnmer(plant,2,0)

## ---- eval=FALSE--------------------------------------------------------------
#  human_21mer <- mnmer(human,2,1)
#  plant_21mer <- mnmer(plant,2,1)

## ---- eval=FALSE--------------------------------------------------------------
#  library(caret)
#  # Add class information
#  classes <- replicate(nrow(human_21mer), "human.vir")
#  featureMatrix_human_21mer <- cbind(human_21mer,classes)
#  classes <- replicate(nrow(plant_21mer), "plant.vir")
#  featureMatrix_plant_21mer <- cbind(plant_21mer,classes)
#  featureMatrix <- rbind(featureMatrix_human_21mer, featureMatrix_plant_21mer)
#  featureMatrix <- subset(featureMatrix, select = -c(seqid))
#  
#  # Machine Learning
#  train_index <- caret::createDataPartition(featureMatrix$classes, p=0.8, list=FALSE)
#  train <- featureMatrix[train_index, ]
#  test <- featureMatrix[-train_index, ]
#  control <- caret::trainControl(method="cv",
#                                 summaryFunction=twoClassSummary,
#                                 classProbs=TRUE,
#                                 savePredictions = TRUE)
#  roc <- caret::train(classes ~ .,
#                      data=train,
#                      method="rf",
#                      preProc=c("center"),
#                      trControl=control)

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

