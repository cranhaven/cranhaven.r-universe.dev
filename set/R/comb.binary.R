# To get the binary combination of several factors
#
# @param vector a vector of factors that you want to be combinated
# @param n the combination number. If n is missing, function comb.binary will return the whole possible combinations
#
# @return a matrix
#
# @examples comb.binary(1:4)
# @examples comb.binary(c(1,3,4),2)
# @examples comb.binary(c("a","b","c"))
# @examples comb.binary(c("a","b","c"),2)
# @author Jing Zhang
# @description You can use this package to get binary combinations of several factors.Great thanks to Marat Talipov

comb.binary<-function(vector,n){
  m=length(vector)
  m.rep=rep(0,m)
  m.binary=data.frame()
  if (missing(n)){
    if ((m %% 2)==0){
      m.int= m %/% 2
      for (m.i in 1:m.int){
        m.binary.i=t(apply(utils::combn(0:m,2*m.i),2,function(k){m.rep[k]=1;m.rep}))
        m.binary=rbind(m.binary,m.binary.i)
      }
    }
    if ((m %% 2)==1){
      m.int= (m+1) %/% 2
      for (m.i in 1:m.int){
        m.binary.i=t(apply(utils::combn(0:m,2*m.i),2,function(k){m.rep[k]=1;m.rep}))
        m.binary=rbind(m.binary,m.binary.i)
      }
    }
    colnames(m.binary)=vector
    return(rbind(rep(0,m),m.binary))
  }else{
    if (m==n){
      m.binary=t(data.frame(rep(1,m)))
      colnames(m.binary)=vector
      rownames(m.binary)=""
      return(m.binary)
    }else{
    m.binary=t(apply(utils::combn(0:m,n),2,function(k){m.rep[k]=1;m.rep}))
    m.n=m.binary[rowSums(m.binary)==n,]#delet zero line
    colnames(m.n)=vector
    rownames(m.n)=1:nrow(m.n)
    return(m.n)
    }
  }
}
