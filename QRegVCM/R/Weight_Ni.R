Weight_Ni <-
function(y, subj){
dim = length(y)
n = length(unique(subj))
N = rep(0,n)
w = rep(0,n)
for (i in 1:n){
N[i] = length(subj[which(subj==unique(subj)[i])])
w[i] = 1/N[i]
}
W = rep(w, N)
out = list(W=W)
return(out)
}
