variance_drv = function(data, years = 10) {

  n = years
  U = 1 + data
  u = mean(U)

  u2 = moment(U,
              central = FALSE,
              absolute = FALSE,
              order = 2)

  app1 = rep(NA, n - 1)

  for (i in 1:(n - 1)) app1[i]=moment(U,
                                      central = FALSE,
                                      absolute = FALSE,
                                      order = 2*i)

  p1 = sum(app1)

  app2 = matrix(NA, n,n)

 # for (r in 1:nrow(app2))  for (s in 1:ncol(app2)) app2[r,s]=sum(r+s)

  for (r in 1:nrow(app2))  for (s in 1:ncol(app2)) app2[r,s]=moment(U,
                                                                    central = FALSE,
                                                                    absolute = FALSE,
                                                                    order = sum(r+s))

  for (r in 1:nrow(app2))  for (s in 1:ncol(app2))  if (s<=r) app2[r,s]=0

  app2[,n]=0

  p2 = 2*sum(app2)

  app3=rep(NA, n - 1)

  for (i in 1:(n - 1)) app3[i]=moment(U,
                                      central = FALSE,
                                      absolute = FALSE,
                                      order = i)

  p3 =(sum(app3))^2


  p4 =moment(U,
             central = FALSE,
             absolute = FALSE,
             order = 2*n)

  un=moment(U,
         central = FALSE,
         absolute = FALSE,
         order = n)

  p5=un^2


  # vettore di appoggio per momenti da n+1 ad 2n-1

  app6=rep(NA,(2*n)-(n+1))

  # length(app6)

  for (i in 1:length(app6)) app6[i]=moment(U,
                                           central = FALSE,
                                           absolute = FALSE,
                                           order = n+i)

  p6=2*sum(app6)


  # vettore di appoggio per momenti da 1 ad n-1

  app7=rep(NA,n-1)

  for (i in 1:length(app7)) app7[i]=moment(U,
                                           central = FALSE,
                                           absolute = FALSE,
                                           order = i)


  p7=2*un*sum(app7)

var=p1+p2-p3+p4-p5+p6-p7

return(var)

}











