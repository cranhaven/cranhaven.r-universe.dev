###############################################################################
## varrank.backward.R ---
## Author          : Gilles Kratzer
## Last modified   : 23/11/2017
###############################################################################

varrank.backward <- function(data.df = NULL, variable.important = NULL, method = NULL, scheme=NULL, discretization.method = NULL, k = NULL, ratio = NULL, n.var=NULL, verbose=TRUE){

  nvar <- dim(data.df)[2]
  n.adjust <- nvar-length(variable.important)
  if(n.adjust==1) return(list(names(data.df)[!(names(data.df) %in% variable.important)],0))
  n.important <- nvar-n.adjust

  if(is.null(n.var)){n.var <- n.adjust}

  ##Progress bar
  if(verbose==TRUE) pbPrint <- txtProgressBar(min = 0, max = n.var-1, style = 3)

  data.df.tmp.s <- data.df[,!(names(data.df) %in% variable.important)]
  data.df.tmp.i <- data.df[,(names(data.df) %in% variable.important)]

  data.df.tmp.s <- varrank::discretization(data.df = data.df.tmp.s,discretization.method = discretization.method,frequency = FALSE)
  data.df.tmp.i <- varrank::discretization(data.df = data.df.tmp.i,discretization.method = discretization.method,frequency = FALSE)


  score.out <- matrix(data = NA,nrow = n.var,ncol = n.var)
  distance.matrix <- matrix(data = NA,nrow = n.adjust,ncol = n.var)

  ##trick to keep the order in the final matrix
  names.index <- rep(TRUE, length(names(data.df.tmp.s)))

  names.selected <- names.to.order <- colnames(data.df.tmp.s)

  names.output <- NULL

  vect.relevance <- matrix(data = 0, nrow = n.adjust,ncol = 1)

  ##++++++++++++++++++++++++++++++++
  ##Computing relevance - redundancy
  ##++++++++++++++++++++++++++++++++

  ##Relevance
  for(i in 1:n.adjust){
    vect.relevance[i] <- mi.data.discr(X = data.df.tmp.s[,i],Y = (data.df.tmp.i))
  }

  for(j in 0:(n.var-1)){

    ##Redundancy
    vect.redundancy <- matrix(data = 0, nrow = n.adjust-j,ncol = 1)

    for(i in 1:(n.adjust-j)){
      tmp.sum <- 0
      for(z in 1:length(names.selected)){

        tmp <- mi.data.discr(X = data.df.tmp.s[,names.selected[z]],Y = data.df.tmp.s[,i])

        ##alpha computation
        switch(method,
               battiti={alpha=1},
               kwak={alpha=mi.data.discr(X = data.df.tmp.i,Y = data.df.tmp.s[,names.selected[z]])/(10^-6+entropy.data(freqs.table = table(data.df[,names.selected[z]])))},
               peng={alpha=1},
               estevez={alpha = 1/(min(entropy.data(freqs.table = table(data.df.tmp.s[,i])),entropy.data(freqs.table = table(data.df.tmp.s[,-i])))+10^-6)}
        )

        tmp.sum <- tmp.sum + alpha * tmp

      }
      #beta computation
      switch(method,
             battiti={beta=ratio},
             kwak={beta=ratio},
             peng={beta=1/(length(names.selected)+1)},
             estevez={beta=1/(length(names.selected)+1)}
      )

      vect.redundancy[i,] <- beta * tmp.sum
    }

    if(scheme=="mid"){score.tmp <- vect.relevance - vect.redundancy}
    if(scheme=="miq"){score.tmp <- vect.relevance / (vect.redundancy+10^-6)}

    tmp.name <- max.col(t(-score.tmp))

    vect.relevance <- as.matrix(vect.relevance[-tmp.name,])

    names.selected <- names(data.df.tmp.s)[-tmp.name]

    if(dim(data.df.tmp.s)[2]==1){names.output <- c(names.output,colnames(data.df.tmp.s)[tmp.name])
      }else{
        names.output <- c(names.output,names(data.df.tmp.s)[tmp.name])
        }

    if(j != (n.var-1)) distance.matrix[names.index,j+1] <- t(score.tmp)

    if(dim(data.df.tmp.s)[2]>2){data.df.tmp.s <- data.df.tmp.s[,-tmp.name];names.index[which(names.index==TRUE)][tmp.name] <- FALSE
    }else{

      #if(dim(data.df.tmp.s)[2]==1){}
      name.df.tmp <- colnames(data.df.tmp.s)

      data.df.tmp.s <- matrix(data = data.df.tmp.s[,-tmp.name],ncol = 1)
      colnames(data.df.tmp.s) <- name.df.tmp[-tmp.name]

      names.index[which(names.index==TRUE)][tmp.name] <- FALSE
    }

    if(verbose==TRUE){setTxtProgressBar(pb = pbPrint, value = j)}

  }

  colnames(distance.matrix) <- names.output
  rownames(distance.matrix) <- names.to.order

  distance.matrix <- distance.matrix[c(names.to.order[!(names.to.order %in% names.output)],rev(names.output)),]

  return(list(names.output, distance.matrix))
}
