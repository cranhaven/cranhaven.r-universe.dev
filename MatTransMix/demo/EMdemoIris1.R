########################
#### EMdemoIris #######
########################

set.seed(123)
data(iris)
data <- as.matrix(iris[,-5])
n <- nrow(data)
p <- 2
T <- 2
X1 <- array(NA, dim = c(p, T, n))

for(i in 1:n){
  X1[1,1,i] <- data[i,1]
  X1[1,2,i] <- data[i,2]
  X1[2,1,i] <- data[i,3]
  X1[2,2,i] <- data[i,4]
}

init <- MatTrans.init(X1, K = 3, n.start = 10)

M <- MatTrans.EM(X1, initial = init, 
                     row.skew = TRUE, col.skew = TRUE,
                     trans = "None", silent = TRUE, size.control = 3)

M$best.bic

M$best.model

table(M$best.result[[1]]$id, iris[,5])

MatTrans.plot(X1, M, rownames = c("Sepal", "Petal"), 
    colnames = c("Length", "Width"))
