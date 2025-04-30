mono.flag <-
function(data,id.col,x.col,y.col,min, max, data.r= NULL, tol=0, direction)
{
  data <-cbind(data[,id.col], data[,x.col], data[,y.col])
  data[,3][data[,3]< min]<-NA
  data[,3][data[,3]> max] <- NA #remove extreme outliers
  mono.1<- as.data.frame(subset(data, !(is.na(data[,2])))) #remove missing x values
  mono.1<-(cbind(mono.1,FALSE, FALSE))
  colnames(mono.1)<-c("ID", "X", "Y","Decreasing", "Outside.Range")


  U<- unique(mono.1[,1]) 
  mono.1 <-foreach( i = 1:length(U), .combine='rbind') %dopar%
  {
    p<- subset(mono.1, mono.1[,1] ==U[i])
    if(sum(is.na(p[,3]))!=nrow(p))
      {return(p)}
  }
  
  mono<- monotonic(data=mono.1, id.col=1, y.col,direction)
  U<- unique(mono.1[,1]) 
  #Remove duplicate entried and flag decreasing entires 
  mono.d <-foreach( i = 1:length(U), .combine='rbind') %dopar%
  {
    if(mono[i,2]==0  || is.na(mono[i,2])==TRUE){ 
        p<- subset(mono.1, mono.1[,1] ==U[i]) #subset of patient info
        if(nrow(unique(p[1:nrow(p),]))==1){
          g<-as.data.frame(unique(p[1:nrow(p),]))
        }else
          g<-as.data.frame(unique(p[1:nrow(p),]))
          g<-g[order(g[,2], g[,3]),] #subset of patient info
          g[,4][is.na(g[,3])] <- TRUE
          for(j in 1:(length(g[,1])-1))
          {
            m <- g[,3][j+1] - g[,3][j] #check if y values are increasing
            if(g[,4][j]==FALSE && g[,4][j+1]==FALSE)
            {
              if(m<0){
                g[,4][j+1]<-TRUE
                f<-g[,3][j]
              }else if(m>0){
                g[,4][j+1]<-FALSE
                f<-g[,3][j+1]}
            }else if(is.na(g[,3][j])==TRUE && is.na(g[,3][j+1])==FALSE){
              if(exists('f')){
                m <- g[,3][j+1] - f
                if(m<0){
                  g[,4][j+1]<-TRUE
                }else{
                  f<-g[,3][j+1]}
              }else{
                g[,4][j]<-TRUE
                f<-g[,3][j+1]
              }
            }else if(is.na(g[,3][j+1])==TRUE  && is.na(g[,3][j])==FALSE){
              g[,4][j+1]<-TRUE
              f<-g[,3][j]
            }else if(is.na(m)==TRUE && is.na(g[,3][j+1])==TRUE && is.na(g[,3][j])==TRUE){
              g[,4][j+1]<-TRUE
            }else if((g[,4][j]==TRUE && isTRUE(is.na(g[,3][j+1]))==FALSE)|| (g[,4][j]==TRUE && isTRUE(is.na(g[,3][j-1]))==FALSE)){ 
              m <- g[,3][j+1] - f #check if y values are increasing
              if(m<0){
                g[,4][j+1]<-TRUE
              }else{
                f<-g[,3][j+1]}}
          }
          g<-unique(g[1:nrow(g),])
    }else{
      p<- subset(mono.1, mono.1[,1] ==U[i]) 
      g<-as.data.frame(unique(p[1:nrow(p),]))
      g<-g[order(g[,2]),]} #subset of patient info
      g[,4][is.na(g[,3])] <- FALSE
      return(g)} 

  if(is.null(data.r)==FALSE){
    ids<-unique(mono.d[,1])
    #flag observations that do not fall in an interval given by user
    mono.d <- foreach(i=1:length(ids), .combine='rbind') %dopar%
    {
      x <- subset(mono.d, mono.d[,1]==ids[i])
      y <-data.r
      for(j in 1:nrow(x))
      {
        a <-x[,2][j]
        b<-match(a, data.r[,1])
        h <- x[,3][j]
        if(isTRUE(h >= y[b,2]-tol && h <= y[b,3]+tol)==T){  
          x[,5][j]<-FALSE
        }else{
          x[,5][j]<-TRUE
        } 
      }
      return(x)
    }
  }
  mono.d[,5][is.na(mono.d[,3])] <- TRUE
  i <- NULL; rm(i)
  return(mono.d)
}
