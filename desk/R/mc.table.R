mc.table = function(x, intercept = TRUE, digits = 3){

if (intercept) a = 1 else a = 0
k = dim(x)[2] # Number of variables
if (k <= 2) stop("You need at least three variables, otherwise you can use cor().", call. = FALSE)
exo = 1:k # Index of variables
X.ind = list() # Init list
nreg = 0 # Init vector

for (i in 1:length(exo)){
  for (j in 1:(length(exo)-1)){
    X.ind[[((k-1)*(i-1)+j)]] = combn(exo[-i], j, simplify = TRUE)
    nreg = nreg + dim(X.ind[[((k-1)*(i-1)+j)]])[2]
  }
}
y.ind = rep(1:k, each = length(X.ind)/k)
R2.mat = NULL
R2.txt = NULL

for(i in 1:length(y.ind)){
  y = as.matrix(x[,y.ind[i]])
  colnames(y) = colnames(x)[y.ind[i]]
  for(j in (1:dim(X.ind[[i]])[2])){
    tmp = as.matrix(x[, X.ind[[i]][,j]])
    colnames(tmp) = colnames(x)[X.ind[[i]][,j]]
    X = as.matrix(cbind(a, tmp))
    aux.reg = lm.fit(X,y)
    R2 = sum((aux.reg$fitted.values - mean(aux.reg$fitted))^2)/sum((y - mean(y))^2)
    R2.mat = c(R2.mat, R2)
    R2.chars = paste(c(paste(X.ind[[i]][,j], sep = "", collapse = "+")), formatC( round(R2, digits), format='f', digits = digits ) )
    R2.txt = c(R2.txt, R2.chars)
    #R2.names = c(R2.names, paste(c(R2.names, round(R2, digits)), sep = "", collapse = " "))
  }
}

R2.mat = matrix(R2.mat, nreg/k, k)
R2.mat = R2.mat[nrow(R2.mat):1,]

R2.txt = matrix(R2.txt, nreg/k, k)
R2.txt = as.matrix(R2.txt[nrow(R2.txt):1,])

colnames(R2.txt) = paste(1:k, paste("(",colnames(x),")", sep = ""))
print(as.data.frame(R2.txt), right = TRUE, quote = FALSE)

return(invisible(R2.mat))
}
