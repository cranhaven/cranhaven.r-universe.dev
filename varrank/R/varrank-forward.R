###############################################################################
## varrank.forward.R ---
## Author          : Gilles Kratzer
## Last modified   : 23/11/2017
##                 : 24/11/2017
###############################################################################

varrank.forward <- function(data.df = NULL, variable.important = NULL, method = NULL, scheme=NULL, discretization.method = NULL,k=NULL, ratio=NULL, n.var=NULL,verbose=TRUE){

  nvar <- dim(data.df)[2]
  n.adjust <- nvar-length(variable.important)
  if(n.adjust==1) return(list(names(data.df)[!(names(data.df) %in% variable.important)],0))
  n.important <- nvar-n.adjust

  if(is.null(n.var)){n.var <- n.adjust}

  if(n.var==1 | n.var==2) verbose <- FALSE
  ##Progress bar
  if(verbose==TRUE) pbPrint <- txtProgressBar(min = 0, max = n.var-2, style = 3)

  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##Computing relevance (first variable is selected by maximizing relevance)
  ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  vect.relevance <- matrix(data = 0, nrow = n.adjust,ncol = 1)

  data.df.tmp <- data.df[,!(names(data.df) %in% variable.important)]

  names.to.order <- names(data.df.tmp)

  ##trick to keep the order in the final matrix
  names.index <- rep(TRUE, length(names(data.df.tmp)))

  ##discretization

  data.df <- varrank::discretization(data.df = data.df,discretization.method = discretization.method,frequency = FALSE)

  data.df.tmp <- varrank::discretization(data.df = data.df.tmp,discretization.method = discretization.method,frequency = FALSE)

  for(i in 1:n.adjust){
    vect.relevance[i] <- mi.data.discr(X = data.df.tmp[,i] ,Y = data.df[,variable.important])
  }

  first.add <- max.col(t(vect.relevance))

  ## scoring
  names.selected <- names(data.df.tmp)[first.add]
  score <- matrix(data = 0,nrow = n.adjust,ncol = 1)
  distance.matrix <- matrix(data = NA,nrow = n.adjust,ncol = n.adjust)
  distance.matrix[,1] <- vect.relevance

  if(n.var==1){
    distance.matrix <- distance.matrix[1]

    names.selected <- names(data.df)[!(names(data.df) %in% variable.important)]
    names.selected <- names.selected[first.add]
    names(distance.matrix) <- names.selected

    return(list(names.selected, distance.matrix))
  }

  names.selected <- names(data.df.tmp)[first.add]

  ##return for 3 variables
  if(dim(data.df.tmp)[2]<3){
    names.selected <- c(names.selected,names(data.df.tmp)[-first.add])
    names.index[first.add] <- FALSE
    colnames(distance.matrix) <- names.selected
    rownames(distance.matrix) <- names.to.order
    distance.matrix <- distance.matrix[names.selected,]

    return(list(names.selected, distance.matrix))
  }else{
    data.df.tmp <- data.df.tmp[,-c(first.add)]
    names.index[first.add] <- FALSE
    }

  vect.relevance <- as.matrix(vect.relevance[-first.add,])

  ##++++++++++++++++++++++++++++++++
  ##Computing relevance - redundancy
  ##++++++++++++++++++++++++++++++++

  ##Relevance

  ##precomputing entropy

  for(j in 1:(n.adjust-2)){

    vect.redundancy <- matrix(data = 0, nrow = n.adjust-j,ncol = 1)

    for(i in 1:(n.adjust-j)){
      tmp.sum <- 0
      for(z in 1:length(names.selected)){

        tmp <- mi.data.discr(X = data.df[,names.selected[z]],Y = data.df.tmp[,i])

        ##alpha computation
        switch(method,
               battiti={alpha=1},
               kwak={alpha=mi.data.discr(X = data.df[,variable.important],Y = data.df[,names.selected[z]])/(10^-6+entropy.data(freqs.table = table(data.df[,names.selected[z]])))},
               peng={alpha=1},
               estevez={alpha = 1/(min(entropy.data(freqs.table = table(data.df[,i])),entropy.data(freqs.table = table(data.df[,names.selected])))+10^-6)}
        )

        tmp.sum <- tmp.sum + alpha * tmp

      }
      #beta computation
      switch(method,
             battiti={beta=ratio},
             kwak={beta=ratio},
             peng={beta=1/length(names.selected)},
             estevez={beta=1/length(names.selected)}
      )

      vect.redundancy[i,] <- beta * tmp.sum
    }

    if(scheme=="mid"){score.tmp <- vect.relevance - vect.redundancy}
    if(scheme=="miq"){score.tmp <- vect.relevance / (vect.redundancy+10^-6)}

    tmp.name <- max.col(t(score.tmp))

    vect.relevance <- as.matrix(vect.relevance[-tmp.name,])

    names.selected <- c(names.selected,names(data.df.tmp)[tmp.name])

    distance.matrix[names.index,j+1] <- t(score.tmp)

    if(dim(data.df.tmp)[2]!=2) data.df.tmp <- data.df.tmp[,-tmp.name];names.index[which(names.index==TRUE)][tmp.name] <- FALSE

    if(length(names.selected)==n.var){
      distance.matrix <- distance.matrix[,1:n.var]
      colnames(distance.matrix) <- names.selected
      rownames(distance.matrix) <- names.to.order

      names.selected.tmp <- c(names.selected,names(data.df.tmp))
      distance.matrix <- distance.matrix[names.selected.tmp,]

      return(list(names.selected, distance.matrix))}

    if(verbose==TRUE){setTxtProgressBar(pb = pbPrint, value = j)}

  }

  names.selected <- c(names.selected,names(data.df.tmp)[-c(tmp.name)])
  colnames(distance.matrix) <- names.selected
  rownames(distance.matrix) <- names.to.order
  distance.matrix <- distance.matrix[names.selected,]

  return(list(names.selected, distance.matrix))
}
