RVineMatrix <- function(help.env) {
   p <- get("p",help.env)
   VineMatrix <- matrix(get("VineMatrix",help.env)[,c(1:p)],(p*(p-1)/2),p)
   dimVM <- dim(VineMatrix)
   new.VineMatrix <- matrix(0,p,p)
   B <- c()
   for(i in 1:(p-1)) {
     D.len <-p-1-i
     ind <- c()
     for(l in 1:dimVM[1]) if(sum(!is.na(VineMatrix[l,c(3:dimVM[2])]))==D.len) ind <- c(ind,l)
     if(!is.null(B)) for(m in ind) if(any(B%in%VineMatrix[m,])) ind <- ind[-which(ind==m)]
     x1<-VineMatrix[ind,1]
     x2<-VineMatrix[ind,2]
     new.VineMatrix[i,i]<-x1
     new.VineMatrix[i+1,i]<-x2
     if(i<(p-1)) {
         for(k in (i+2):p) {
           D.len <- p-k
           ind <- c()
           for(l in 1:dimVM[1]) if((VineMatrix[l,1]==x1|VineMatrix[l,2]==x1)&sum(!is.na(VineMatrix[l,c(3:dimVM[2])]))==D.len) ind <- c(ind,l)
           if(!is.null(B)) for(m in ind) if(any(B%in%VineMatrix[m,])) ind <- ind[-which(ind==m)]
           ind.help <- !c(VineMatrix[ind,1]==x1,VineMatrix[ind,2]==x1)
           new.VineMatrix[k,i]<-VineMatrix[ind,c(1,2)[ind.help]]
         }
     }
     B <- c(B,x1)
   }
   new.VineMatrix[p,p]<-seq(1,p)[!seq(1,p)%in%B]
   assign("VineMatrix.help",get("VineMatrix",help.env),help.env)
   assign("VineMatrix",new.VineMatrix,help.env)
}
     
