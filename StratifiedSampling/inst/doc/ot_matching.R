## ---- include = FALSE---------------------------------------------------------
library(knitr)
library(prettydoc)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)



## ----contingency table--------------------------------------------------------
library(laeken)
library(sampling)
library(StratifiedSampling)

data("eusilc")
eusilc <- na.omit(eusilc)
N <- nrow(eusilc)


# Xm are the matching variables and id are identity of the units
Xm <- eusilc[,c("hsize","db040","age","rb090","pb220a")]
Xmcat <- do.call(cbind,apply(Xm[,c(2,4,5)],MARGIN = 2,FUN = disjunctive))
Xm <- cbind(Xmcat,Xm[,-c(2,4,5)])
id <- eusilc$rb030


# categorial income splitted by the percentile
c_income  <- eusilc$eqIncome
q <- quantile(eusilc$eqIncome, probs = seq(0, 1, 0.15))
c_income[which(eusilc$eqIncome <= q[2])] <- "(0,15]"
c_income[which(q[2] < eusilc$eqIncome & eusilc$eqIncome <= q[3])] <- "(15,30]"
c_income[which(q[3] < eusilc$eqIncome & eusilc$eqIncome <= q[4])] <- "(30,45]"
c_income[which(q[4] < eusilc$eqIncome & eusilc$eqIncome <= q[5])] <- "(45,60]"
c_income[which(q[5] < eusilc$eqIncome & eusilc$eqIncome <= q[6])] <- "(60,75]"
c_income[which(q[6] < eusilc$eqIncome & eusilc$eqIncome <= q[7])] <- "(75,90]"
c_income[which(  eusilc$eqIncome > q[7] )] <- "(90,100]"

# variable of interests
Y <- data.frame(ecostat = eusilc$pl030)
Z <- data.frame(c_income = c_income)

# put same rownames
rownames(Xm) <- rownames(Y) <- rownames(Z)<- id

YZ <- table(cbind(Y,Z))
addmargins(YZ)



## ----sample_scheme------------------------------------------------------------

# size of sample
n1 <- 1000
n2 <- 500

# samples
s1 <- srswor(n1,N)
s2 <- srswor(n2,N)
  
# extract matching units
X1 <- Xm[s1 == 1,]
X2 <- Xm[s2 == 1,]
  
# extract variable of interest
Y1 <- data.frame(Y[s1 == 1,])
colnames(Y1) <- colnames(Y)
Z2 <- as.data.frame(Z[s2 == 1,])
colnames(Z2) <- colnames(Z)
  
# extract correct identities
id1 <- id[s1 == 1]
id2 <- id[s2 == 1]
  
# put correct rownames
rownames(Y1) <- id1
rownames(Z2) <- id2
  
# here weights are inverse of inclusion probabilities
d1 <- rep(N/n1,n1)
d2 <- rep(N/n2,n2)
  
# disjunctive form
Y_dis <- sampling::disjunctive(as.matrix(Y))
Z_dis <- sampling::disjunctive(as.matrix(Z))
  
Y1_dis <- Y_dis[s1 ==1,]
Z2_dis <- Z_dis[s2 ==1,]



## ----harm---------------------------------------------------------------------


re <- harmonize(X1,d1,id1,X2,d2,id2)  

# if we want to use the population totals to harmonize we can use 
re <- harmonize(X1,d1,id1,X2,d2,id2,totals = c(N,colSums(Xm)))

w1 <- re$w1
w2 <- re$w2

colSums(Xm)
colSums(w1*X1)
colSums(w2*X2)



## ----OT-----------------------------------------------------------------------

# Optimal transport matching
object <- otmatch(X1,id1,X2,id2,w1,w2)
head(object[,1:3])

Y1_ot <- cbind(X1[as.character(object$id1),],y = Y1[as.character(object$id1),])
Z2_ot <- cbind(X2[as.character(object$id2),],z = Z2[as.character(object$id2),])
YZ_ot <- tapply(object$weight,list(Y1_ot$y,Z2_ot$z),sum)

# transform NA into 0
YZ_ot[is.na(YZ_ot)] <- 0

# result
round(addmargins(YZ_ot),3)



## ----BS-----------------------------------------------------------------------

# Balanced Sampling 
BS <- bsmatch(object)
head(BS$object[,1:3])


Y1_bs <- cbind(X1[as.character(BS$object$id1),],y = Y1[as.character(BS$object$id1),])
Z2_bs <- cbind(X2[as.character(BS$object$id2),],z = Z2[as.character(BS$object$id2),])
YZ_bs <- tapply(BS$object$weight/BS$q,list(Y1_bs$y,Z2_bs$z),sum)
YZ_bs[is.na(YZ_bs)] <- 0
round(addmargins(YZ_bs),3)

# With Z2 as auxiliary information for stratified balanced sampling.
BS <- bsmatch(object,Z2)

Y1_bs <- cbind(X1[as.character(BS$object$id1),],y = Y1[as.character(BS$object$id1),])
Z2_bs <- cbind(X2[as.character(BS$object$id2),],z = Z2[as.character(BS$object$id2),])
YZ_bs <- tapply(BS$object$weight/BS$q,list(Y1_bs$y,Z2_bs$z),sum)
YZ_bs[is.na(YZ_bs)] <- 0
round(addmargins(YZ_bs),3)






## -----------------------------------------------------------------------------

# split the weight by id1
q_l <- split(object$weight,f = object$id1)
# normalize in each id1
q_l <- lapply(q_l, function(x){x/sum(x)})
q <- as.numeric(do.call(c,q_l))
  
Z_pred <- t(q*disjunctive(object$id1))%*%disjunctive(Z2[as.character(object$id2),])
colnames(Z_pred) <- levels(factor(Z2$c_income))
head(Z_pred)


 
 

