PGini_RSV <-
function(y,s,w=rep(1,length(y)))
{
dataset<-cbind(y,s,w)
ord_y<-order(y)
dataset_ord<-dataset[ord_y, ]
y<-dataset_ord[,1]
s<-dataset_ord[,2]
w<-dataset_ord[,3]
N <- sum(w)
sw<-s*w
C_i<-cumsum(w)
num_1<-sum(sw*C_i)
num_2<-sum(sw)
num_3<-sum(sw*w)
CONC_num<-(2/N^2)*num_1-(1/N)*num_2-(1/N^2)*num_3
t_neg<-subset(sw,sw<=0)
T_neg<-sum(t_neg)
T_pos<-sum(sw)+abs(T_neg)
n_RSV<-(2*(T_pos+(abs(T_neg)))/N)
mean_RSV<-(n_RSV/2)
PGini_RSV<-(1/mean_RSV)*CONC_num
list(PGini_RSV=PGini_RSV)
}
