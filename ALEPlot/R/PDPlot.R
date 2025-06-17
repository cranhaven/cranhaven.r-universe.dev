PDPlot <-
function(X, X.model, pred.fun, J, K) {

N = dim(X)[1]  #sample size
d = dim(X)[2]  #number of predictor variables

if (length(J) == 1) { #calculate main effects PD plot

  if (class(X[,J]) == "numeric" | class(X[,J]) == "integer") {#for numeric or integer X[,J], calculate the ALE plot

    fJ = numeric(K)
    fJ = numeric(K)
    xmin = min(X[,J])
    xmax = max(X[,J])
    x = seq(xmin, xmax, length.out=K)
    for (k in 1:K) {
      X.predict = X
      X.predict[,J] = x[k]
      y.hat = pred.fun(X.model=X.model, newdata = X.predict)
      fJ[k] = mean(y.hat)
    }  #end of for loop
    #now vertically translate fJ, by subtracting its average (averaged across X[,J])
    a<-cut(X[,J], breaks=c(xmin-(x[2]-x[1]),x), include.lowest=TRUE)
    b<- as.numeric(table(a)) #frequency count vector of X[,J] values falling into x intervals
    fJ = fJ - sum(fJ*b)/sum(b)
    plot(x, fJ, type="l", xlab = paste("x_",J, " (", names(X)[J], ")", sep=""), ylab = paste("f_",J,"(x_",J,")", sep=""))

  } #end of if (class(X[,J]) == "numeric" | class(X[,J]) == "integer") statement

  else if (class(X[,J]) == "factor") {#for factor X[,J], calculate the ALE plot

    #Get rid of any empty levels of x and tabulate level counts and probabilities
    X[,J] <- droplevels(X[,J])
    x.count <- as.numeric(table(X[,J])) #frequency count vector for levels of X[,J]
    x.prob <- x.count/sum(x.count) #probability vector for levels of X[,J]
    K <- nlevels(X[,J])  #reset K to the number of levels of X[,J]
    x <- levels(X[,J])  #as.character levels of X[,J] in original order
    fJ = numeric(K)

    for (k in 1:K) {
      X.predict = X
      X.predict[,J] = x[k]
      y.hat = pred.fun(X.model=X.model, newdata = X.predict)
      fJ[k] = mean(y.hat)
    }  #end of for loop
    #now vertically translate fJ, by subtracting its average (averaged across X[,J])
    fJ = fJ - sum(fJ*x.prob)
    barplot(fJ, names=x, xlab=paste("x_", J, " (", names(X)[J], ")", sep=""), ylab= paste("f_",J,"(x_",J,")", sep=""), las =3)

  }  #end of else if (class(X[,J]) == "factor") statement

  else print("error:  class(X[,J]) must be either factor or numeric or integer")

} #end of if (length(J) == 1) statement

else if (length(J) == 2) { #calculate second-order effects PD plot

  if (class(X[,J[2]]) != "numeric" & class(X[,J[2]]) != "integer") {
    print("error: X[,J[2]] must be numeric or integer. Only X[,J[1]] can be a factor")
  }

  if (class(X[,J[1]]) == "factor") {#for categorical X[,J[1]], calculate the PD plot

  #Get rid of any empty levels of x and tabulate level counts and probabilities
    X[,J[1]] <- droplevels(X[,J[1]])
    K1 <- nlevels(X[,J[1]])  #set K1 to the number of levels of X[,J[1]]
    fJ = matrix(0,K1,K)
    x1.char <- levels(X[,J[1]])  #as.character levels of X[,J[1]] in original order
    x1.num <- 1:K1  #numeric version of levels of X[,J[1]]
    xmin2 = min(X[,J[2]])
    xmax2 = max(X[,J[2]])
    x2 = seq(xmin2, xmax2, length.out=K)
    for (k1 in 1:K1) {
      for (k2 in 1:K) {
        X.predict = X
        X.predict[,J[1]] = x1.char[k1]
        X.predict[,J[2]] = x2[k2]
        y.hat = pred.fun(X.model=X.model, newdata = X.predict)
        fJ[k1,k2] = mean(y.hat)
      }  #end of k2 for loop
    }  #end of k1 for loop
    #now vertically translate fJ, by subtracting the averaged main effects
    b1=as.numeric(table(X[,J[1]]))  #K1-length frequency count vector of X[,J[1]] values falling into x1 levels
    a2=cut(X[,J[2]], breaks=c(xmin2-(x2[2]-x2[1]),x2), include.lowest=TRUE)
    b2=as.numeric(table(a2))  #K-length frequency count vector of X[,J[2]] values falling into x2 intervals
    b=as.matrix(table(X[,J[1]],a2))  #K1xK frequency count matrix (rows correspond to x1; columns to x2)
    fJ1=apply(t(fJ)*b2,2,sum)/sum(b2)  #main PD effect of x1 on fJ
    fJ2=apply(fJ*b1,2,sum)/sum(b1)  #main PD effect of x2 on fJ
    fJ = fJ - outer(fJ1,rep(1,K)) - outer(rep(1,K1),fJ2)
    fJ0=sum(fJ*b)/sum(b)  #average of fJ
    fJ=fJ - fJ0
    x <- list(x1.char, x2)
    K <- c(K1, K)
    image(x1.num, x2, fJ, xlab=paste("x_",J[1], " (", names(X)[J[1]], ")", sep=""), ylab= paste("x_",J[2], " (", names(X)[J[2]], ")", sep=""), ylim = range(x2), yaxs = "i")
    contour(x1.num, x2, fJ, add=TRUE, drawlabels=TRUE)
    axis(side=1, labels=x1.char, at=1:K1, las = 3, padj=1.2) #add level names to x-axis
  } #end of if (class(X[,J[1]]) == "factor") statement

  else if (class(X[,J[1]]) == "numeric" | class(X[,J[1]]) == "integer") {#for numerical/integer X[,J[1]], calculate the PD plot

    fJ = matrix(0,K,K)
    xmin1 = min(X[,J[1]])
    xmax1 = max(X[,J[1]])
    xmin2 = min(X[,J[2]])
    xmax2 = max(X[,J[2]])
    x1 = seq(xmin1, xmax1, length.out=K)
    x2 = seq(xmin2, xmax2, length.out=K)
    for (k1 in 1:K) {
      for (k2 in 1:K) {
        X.predict = X
        X.predict[,J[1]] = x1[k1]
        X.predict[,J[2]] = x2[k2]
        y.hat = pred.fun(X.model=X.model, newdata = X.predict)
        fJ[k1,k2] = mean(y.hat)
      }  #end of k2 for loop
    }  #end of k1 for loop
    #now vertically translate fJ, by subtracting the averaged main effects
    a1=cut(X[,J[1]], breaks=c(xmin1-(x1[2]-x1[1]),x1), include.lowest=TRUE)
    a2=cut(X[,J[2]], breaks=c(xmin2-(x2[2]-x2[1]),x2), include.lowest=TRUE)
    b1=as.numeric(table(a1))  #frequency count vector of X[,J[1]] values falling into x1 intervals
    b2=as.numeric(table(a2))  #frequency count vector of X[,J[2]] values falling into x2 intervals
    b=as.matrix(table(a1,a2))  #frequency count matrix (rows correspond to x1; columns to x2)
    fJ1=apply(t(fJ)*b2,2,sum)/sum(b2)  #main PD effect of x1 on fJ
    fJ2=apply(fJ*b1,2,sum)/sum(b1)  #main PD effect of x2 on fJ
    fJ = fJ - outer(fJ1,rep(1,K)) - outer(rep(1,K),fJ2)
    fJ0=sum(fJ*b)/sum(b)  #average of fJ
    fJ=fJ - fJ0
    x <- list(x1, x2)
    K <- c(K, K)
    image(x1, x2, fJ, xlab=paste("x_",J[1], " (", names(X)[J[1]], ")", sep=""), ylab= paste("x_",J[2], " (", names(X)[J[2]], ")", sep=""), xlim = range(x1), ylim = range(x2), xaxs = "i", yaxs = "i")
    contour(x1, x2, fJ, add=TRUE, drawlabels=TRUE)

  }  #end of else if (class(X[,J[1]]) == "numeric" | class(X[,J[1]]) == "integer") statement

  else print("error:  class(X[,J[1]]) must be either factor or numeric/integer")

} #end of if (length(J) == 2) statement

else print("error:  J must be a vector of length one or two")

list(x.values=x, f.values = fJ)
}
