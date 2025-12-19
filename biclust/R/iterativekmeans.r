#-------------------- ITERATIVE K MEANS --------------------
#Iterative K means personal implementation
#NOTE: Good k estimation but time consuming
# x-vector to cluster
# minimum- Minimum number of clusters (default 2)
# maximum- Maximum number of clusters (default 10)
# returns -Optimal clustering
iterativeKmeans=function(x,minimum=2,maximum=10,choice=0.5)
  {
  n=length(x)
  
  #Input checking
  if(minimum<2)
    {
    stop("Error: clustering must divide data in at least 2 clusters (minimum must value at least 2)")
    }
  if(maximum>=n)
    {
    stop("Error: clustering must divide data in as much as n-1 clusters (maximum value at max is length(x)-1)")
    }
  if(maximum<minimum)
    {
    stop("Error: maximum value must be greater than minimum")
    }  
    
  numClusterings=maximum-minimum+1;
  k=minimum
  ss=c()
  while(k<=maximum)
    {
    ss=c(ss,mean(kmeans(x,k,iter.max=20)$withinss))
    k=k+1
    }
  diferencias=diff(ss)
  md=mean(diferencias)
  optimo=1
  for(i in 1:length(diferencias))
    {
    if(diferencias[i]>md) 
      {
      optimo=i
      break
      }
    }
  optimo=optimo+minimum-1
  kmeans(x,optimo,iter.max=20)
  }