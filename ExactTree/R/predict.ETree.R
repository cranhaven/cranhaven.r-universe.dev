#' Predictions for new data with a ETree object
#'
#' Predicts for (new) subjects the outcome variable based on a fitted
#'   \code{ETree} object.
#'
#' @param object an object of the class \dQuote{ETree}.
#' @param newdata a data frame with data on new subjects for whom predictions should be made.
#'   The data frame should contain at least the variables used in the splits of the fitted tree.
#'   It is not necessary to include the treatment variable.
#' @param type character string denoting the type of predicted object to be returned. The default is
#'   set to \code{type="pred"}: a vector with predicted treatment subgroup classes per subject
#'   is returned. If set to \code{"matrix"}, a matrix is returned with the leaf and
#'   corresponding node of the tree to which a subject is assigned.
#' @param depth If alltreesizes was set to TRUE in ETree, you need to specify the depth of the tree
#'   you want to use in the predict function. This parameter should be equal to the number of the tree
#'   you want to use in the TAll output of ETree. If NULL, the largest tree is used.
#' @param \dots optional additional arguments.
#'
#' @return One of the following objects is returned depending on output type specified
#'   in the function:
#'
#'   If \code{type="pred"}:
#'   vector of the predicted outcome for every individual in the data set. Returns NA
#'   for subjects with missing values on one or more of the splitting variables.
#'
#'   If \code{type="matrix"}:
#'   a matrix with predicted locations of subjects within the fitted tree. The leaf numbers are
#'   in the first column and the corresponding node numbers in the second column. Returns NA
#'   for subjects with missing values on one or more of the splitting variables.
#'
#'   If \code{type="prob"}:
#'   a matrix with probabilities.
#'
#' @seealso \code{\link{ETree}}, \code{\link{ETree.control}}
#'
#' @examples
#' \donttest{
#'   data(iris)
#'   trainingIris<-iris[1:100,]
#'   # Fit an Exact Tree model
#'   controlEtree <- ETree.control(measure=0, maxsize = 4, maxdepth = 3,
#'   minbucket = 5, ncv=5, alltreesizes = FALSE)
#'   tree<-ETree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'   control= controlEtree, data = trainingIris)
#'   testIris<-iris[101:150,]
#'   predictions<-predict(tree, newdata=testIris, type="pred", depth=1)
#' }
#'
#'
#' @export
predict.ETree<-function(object, newdata = NULL, type = 'pred', depth=NULL, ...){


  if(is.null(depth)){
    depth<-length(object$TAll)
  }

  Desc<-object$Desc
  Data<-object$Data
  Yvarnames<-object$Yvarnames
  Xvarnames<-object$Xvarnames

  if(length(object$Sizes)!=0){
    Sizes<-object$Sizes[[depth]]
    Prob<-object$ProbLeaves[[depth]]
  }

  object<-object$Transf_Trees[[depth]]

  parent_nodes<-object$si[,"Parent_node"]
  li<-object$li


  #Transform categorical variables of the test dataset to numeric. The split points are numeric.
  #Change 01/07/2026. Do not transform to numeric the categorical variables.
  #Do the same transformation as in SelectVar.
  isFactor<-c()
  for (i in 1:NCOL(newdata)) {
    isFactor[i]<-ifelse(is.factor(newdata[,i]), TRUE, FALSE)
  }

  whichFactor<-which(isFactor==TRUE)

  for (i in whichFactor) {
    tryCatch({
      newdata[,i]<- as.numeric(newdata[,i])
    }, warning = function(w) {
      newdata[,i]<- as.numeric(as.factor(newdata[,i]))
    }, error = function(e) {
      levels(newdata[,i])<-1:length(levels(newdata[,i]))
      newdata[,i]<- as.numeric(as.factor(newdata[,i]))
    })
  }



  if(type=="matrix") {
    predY<-matrix(rep(0,2*NROW(newdata)),NROW(newdata),2)

    for(i in 1:NROW(newdata)){

      # if(is.na(newdata[i,])){
      #
      #   predY[i,1]<-NA
      #   predY[i,2]<-NA
      #
      # }else{

      newroot<-1
      while(newroot %in% parent_nodes){

        oldroot<-newroot
        split_num<-which(object$si[,"Parent_node"]==oldroot)
        splt_var<-object$si[split_num,"Splitting_variable"]

        if(is.na(newdata[i,splt_var])){
          newroot<-NA
        }else{
          splt_point<-object$si[split_num,"Split_point"]

          if(is.numeric(splt_point)){

            if(newdata[i,splt_var]<splt_point){ #Condition for numeric Change 10/08/2022 Old: if(newdata[i,splt_var]<splt_point){
              newroot<-object$si[split_num,"Child_nodes_Y"]
            }else{
              newroot<-object$si[split_num,"Child_node_N"]
            }

          }else{

            #New 26/03/2026
            Desc_index1 <- which(Xvarnames==splt_var)
            index_splt_point <- which(Desc[[Desc_index1]]==splt_point)
            selected_categories <- Desc[[Desc_index1]][1:index_splt_point]

            if(newdata[i,splt_var] %in% selected_categories){ #Check this one!!!!!! (10/08/2022). Condition for characters. Maybe this is not needed if categorical variables are transformed into numbers
              newroot<-object$si[split_num,"Child_nodes_Y"]
            }else{
              newroot<-object$si[split_num,"Child_node_N"]
            }


            # Old
            # if(newdata[i,splt_var]==splt_point){ #Check this one!!!!!! (10/08/2022). Condition for characters. Maybe this is not needed if categorical variables are transformed into numbers
            #   newroot<-object$si[split_num,"Child_nodes_Y"]
            # }else{
            #   newroot<-object$si[split_num,"Child_node_N"]
            # }

          }

        }
        # if(is.na(newdata[i,splt_var])){
        #   newroot<-NA
        # }else{
        #   splt_point<-object$si[split_num,"Split_point"]
        #   if(newdata[i,splt_var]<splt_point){ #Check this condition: Change 10/08/2022 Old: if(newdata[i,splt_var]<=splt_point){
        #     newroot<-object$si[split_num,"Child_nodes_Y"]
        #   }else{
        #     newroot<-object$si[split_num,"Child_node_N"]
        #   }
        #
        # }
      }

      if(is.na(newroot)){
        predY[i,1]<-NA
        predY[i,2]<-NA
      }else{
        leaf<-which(li$Node==newroot)
        predY[i,1]<-leaf
        predY[i,2]<-newroot
        #predh[i]<-li[leaf,"h"]
      }

      #}

    }
    colnames(predY)<-c("Leaf","Node")

  }else if(type=="pred"){
    predY<-rep(0,NROW(newdata))

    for(i in 1:NROW(newdata)){


      newroot<-1
      while(newroot %in% parent_nodes){

        oldroot<-newroot
        split_num<-which(object$si[,"Parent_node"]==oldroot)
        splt_var<-object$si[split_num,"Splitting_variable"]

        if(is.na(newdata[i,splt_var])){
          newroot<-NA
        }else{
          splt_point<-object$si[split_num,"Split_point"]

          if(is.numeric(splt_point)){

            if(newdata[i,splt_var]<splt_point){ #Condition for numeric Change 10/08/2022 Old: if(newdata[i,splt_var]<splt_point){
              newroot<-object$si[split_num,"Child_nodes_Y"]
            }else{
              newroot<-object$si[split_num,"Child_node_N"]
            }

          }else{

            #New 26/03/2026
            Desc_index1 <- which(Xvarnames==splt_var)
            index_splt_point <- which(Desc[[Desc_index1]]==splt_point)
            selected_categories <- Desc[[Desc_index1]][1:index_splt_point]

            if(newdata[i,splt_var] %in% selected_categories){ #Check this one!!!!!! (10/08/2022). Condition for characters. Maybe this is not needed if categorical variables are transformed into numbers
              newroot<-object$si[split_num,"Child_nodes_Y"]
            }else{
              newroot<-object$si[split_num,"Child_node_N"]
            }


            # Old
            # if(newdata[i,splt_var]==splt_point){ #Check this one!!!!!! (10/08/2022). Condition for characters. Maybe this is not needed if categorical variables are transformed into numbers
            #   newroot<-object$si[split_num,"Child_nodes_Y"]
            # }else{
            #   newroot<-object$si[split_num,"Child_node_N"]
            # }

          }

        }

      }


      if(is.na(newroot)){
        predY[i]<-NA

      }else{
        if(is.numeric(Data[,Yvarnames])){
          leaf<-which(li$Node==newroot)
          predY[i]<-li[leaf,"y"]
          #predh[i]<-li[leaf,"h"]
        }else if(is.factor(Data[,Yvarnames])){
          # #New 26/03/2026
          # Desc_index1 <- which(Xvarnames==splt_var)
          # index_splt_point <- which(Desc[[Desc_index1]]==splt_point)
          # selected_categories <- Desc[[Desc_index1]][1:index_splt_point]
          #
          # if(!is.factor(newdata[,splt_var])){
          #
          #   if(newdata[i,splt_var] < splt_point){ #Check this one!!!!!! (10/08/2022). Condition for characters. Maybe this is not needed if categorical variables are transformed into numbers
          #     newroot<-object$si[split_num,"Child_nodes_Y"]
          #   }else{
          #     newroot<-object$si[split_num,"Child_node_N"]
          #   }
          #
          # }else{
          #
          #   if(newdata[i,splt_var] %in% selected_categories){ #Check this one!!!!!! (10/08/2022). Condition for characters. Maybe this is not needed if categorical variables are transformed into numbers
          #     newroot<-object$si[split_num,"Child_nodes_Y"]
          #   }else{
          #     newroot<-object$si[split_num,"Child_node_N"]
          #   }
          #
          # }

          levelsY<-levels(Data[,Yvarnames])
          leaf<-which(li$Node==newroot)
          predY[i]<-levelsY[li[leaf,"y"]]
        }else{
          stop("Y must be numeric or factor")
        }
      }

    }
    names(predY) <- 1:NROW(newdata)[1]

  }else if(type=="prob"){

    categories <- levels(Data[,Yvarnames])
    ncat <- length(categories)

    predY<-matrix(rep(0,ncat*NROW(newdata)),NROW(newdata),ncat)

    for(i in 1:NROW(newdata)){


      newroot<-1
      while(newroot %in% parent_nodes){

        oldroot<-newroot
        split_num<-which(object$si[,"Parent_node"]==oldroot)
        splt_var<-object$si[split_num,"Splitting_variable"]

        if(is.na(newdata[i,splt_var])){
          newroot<-NA
        }else{
          splt_point<-object$si[split_num,"Split_point"]

          if(is.numeric(splt_point)){

            if(newdata[i,splt_var]<splt_point){ #Condition for numeric Change 10/08/2022 Old: if(newdata[i,splt_var]<splt_point){
              newroot<-object$si[split_num,"Child_nodes_Y"]
            }else{
              newroot<-object$si[split_num,"Child_node_N"]
            }

          }else{

            #New 26/03/2026
            Desc_index1 <- which(Xvarnames==splt_var)
            index_splt_point <- which(Desc[[Desc_index1]]==splt_point)
            selected_categories <- Desc[[Desc_index1]][1:index_splt_point]

            if(newdata[i,splt_var] %in% selected_categories){ #Check this one!!!!!! (10/08/2022). Condition for characters. Maybe this is not needed if categorical variables are transformed into numbers
              newroot<-object$si[split_num,"Child_nodes_Y"]
            }else{
              newroot<-object$si[split_num,"Child_node_N"]
            }

            # if(newdata[i,splt_var]==splt_point){ #Check this one!!!!!! (10/08/2022). Condition for characters. Maybe this is not needed if categorical variables are transformed into numbers
            #   newroot<-object$si[split_num,"Child_nodes_Y"]
            # }else{
            #   newroot<-object$si[split_num,"Child_node_N"]
            # }

          }

        }

      }


      if(is.na(newroot)){
        predY[i]<-NA

      }else{
        if(is.factor(Data[,Yvarnames])){
          leaf<-which(li$Node==newroot)
          if(is.list(Prob)){
            predY[i,]<-Prob[[leaf]]
          }else{
            predY[i,]<-Prob
          }

        }else{
          stop("Y must be a factor")
        }
      }

    }
    colnames(predY) <- categories



  }





  return(predY)



}



# for(i in 1:NROW(newdata)){
#
#   newroot<-1
#   while(newroot %in% parent_nodes){
#
#     oldroot<-newroot
#     splt_var<-object$si[oldroot,"Splitting_variable"]
#     splt_point<-object$si[oldroot,"Split_point"]
#     if(newdata[i,splt_var]>=splt_point){ #Check this condition
#       newroot<-object$si[oldroot,"Child_nodes_Y"]
#     }else{
#       newroot<-object$si[oldroot,"Child_nodes_N"]
#     }
#
#   }
#
#
#
#   leaf<-which(li$Node==newroot)
#   predY[i]<-li[leaf,"y"]
#   #predh[i]<-li[leaf,"h"]
#
#
# }


# #Create variable to be returned
# if(type=="matrix") {
#   nodemat <- matrix(0, nrow=dim(newdata)[1], ncol=2)
#   for(i in 1:length(nnum)){
#     nodemat[index[which(Gmat[,i]==1)],1] <- which(object$li[,1]==nnum[i])
#     nodemat[index[which(Gmat[,i]==1)],2] <- nnum[i]
#   }
#   if(nmis!= 0){
#     nodemat[naindex,c(1:2)] <- NA
#   }
#   colnames(nodemat) <- c("Leaf", "Node")
#   #rownames(nodemat) <- as.numeric(rownames(ytxna)) # give subjects numbers of original dataset
#   return(nodemat)
# }
#
# if(type=="pred"){
#   classmat <- numeric(dim(newdata)[1])
#   for(i in 1:length(nnum)) {
#     classmat[index[which(Gmat[,i]==1)]] <- object$li[which(nnum[i]==object$li[,1]),10]
#   }
#   if(nmis!= 0){
#     classmat[naindex] <- NA
#   }
#   names(classmat) <- 1:dim(newdata)[1]
#   return(classmat)
# }
