depthharm<- function(soildata, var.name, lam = 0.1, d = t(c(0,5,10,20,40,80,160,200))){
  if (is(soildata,"SoilProfileCollection") == TRUE){
    depthcols = soildata@depthcols
    idcol = soildata@idcol
    soildata@horizons = soildata@horizons[,c(idcol, depthcols, var.name)]
    sdata<- as.data.frame(soildata@horizons)}
  vlow = 0; vhigh = 1000; progress=TRUE
  if (is(soildata,"data.frame") == TRUE){
    sdata<- as.data.frame(soildata[,c(1:3,which(colnames(soildata)==var.name))])}
  soildata@horizons <- soildata@horizons[!soildata@horizons[,depthcols[1]]<0 & !soildata@horizons[,depthcols[2]]<0,]
  if (is(soildata,"data.frame") == FALSE & is(soildata,"SoilProfileCollection") == FALSE){
    stop("ERROR:: soildata must be either a data.frame or SoilProfileCollection")}
  maxdata<- max(d)
  s2<- (0.05*mean(sdata[,4]))^2
  sp_dat<-split(sdata,sdata[,1])
  nl<- length(lam)
  matrix_yfit<- matrix(NA,ncol=length(c(1:maxdata)),nrow=length(sp_dat))
  y.avg<- matrix(NA,ncol=length(d),nrow=length(sp_dat))
  sse<- matrix(NA,ncol=length(lam),nrow=1)
  sset<- matrix(NA,ncol=length(lam),nrow=length(sp_dat))
  mat_id<- sdata[0,]
  d.avg<- sdata[1,]
  d.avg$predicted<- 0
  d.avg$FID<- 0
  d.avg<- d.avg[0,]
  message("Harmonizing depth intervals...")
  if (progress) pb <- txtProgressBar(min=0, max=length(sp_dat), style=3)
  cnt<- 1
  for(st in 1:length(sp_dat)) {
    subs<-sp_dat[[st]]
    subs<-as.matrix(subs)
    mat_id[st,1]<- subs[1,1]
    ir<- c(1:nrow(subs))
    ir<-as.matrix(t(ir))
    u<- as.numeric(subs[ir,2])
    u<-as.matrix(t(u))
    v<- as.numeric(subs[ir,3])
    v<-as.matrix(t(v))
    y<- as.numeric(subs[ir,4])
    y<-as.matrix(t(y))
    n<- length(y);
    if (n == 1)
    {d.avg[cnt:(cnt-1+nrow(subs)),1:4]<- subs
    d.avg[cnt:(cnt-1+nrow(subs)),5]<- y
    d.avg[cnt:(cnt-1+nrow(subs)),6]<- st
    xfit<- as.matrix(t(c(1:maxdata)))
    nj<- max(v)
    if (nj > maxdata)
    {nj<- maxdata}
    yfit<- xfit
    yfit[,1:nj]<- y
    if (nj < maxdata)
    {yfit[,(nj+1):maxdata]=NA}
    matrix_yfit[st,]<- yfit
    nd<- length(d)-1
    dl<-d+1
    for (cj in 1:nd) {
      xd1<- dl[cj]
      xd2<- dl[cj+1]-1
      if (nj>=xd1 & nj<=xd2)
      {xd2<- nj-1
      y.avg[st,cj]<- mean(yfit[,xd1:xd2])}
      else
      {y.avg[st,cj]<- mean(yfit[,xd1:xd2])}
      y.avg[st,cj+1]<- max(v)}
    cnt<- cnt+nrow(subs)

    }
    else  {
      d.avg[cnt:(cnt-1+nrow(subs)),1:4]<- subs
      d.avg[cnt:(cnt-1+nrow(subs)),6]<- st
      np1 <- n+1
      nm1 <- n-1
      delta <- v-u
      del <- c(u[2:n],u[n])-v
      r <- matrix(0,ncol=nm1,nrow=nm1)
      for(dig in 1:nm1){
        r[dig,dig]<-1
      }
      for(udig in 1:nm1-1){
        r[udig,udig+1]<-1
      }
      d2 <- matrix(0, ncol=nm1, nrow=nm1)
      diag(d2) <- delta[2:n]
      r <- d2 %*% r
      r <- r + t(r)
      d1 <- matrix(0, ncol=nm1, nrow=nm1)
      diag(d1) <- delta[1:nm1]

      d3 <- matrix(0, ncol=nm1, nrow=nm1)
      diag(d3) <- del[1:nm1]
      r <- r+2*d1 + 6*d3
      q <- matrix(0,ncol=n,nrow=n)
      for (dig in 1:n){
        q[dig,dig]<- -1
      }
      for (udig in 1:n-1){
        q[udig,udig+1]<-1
      }
      q <- q[1:nm1,1:n]
      dim.mat <- matrix(q[],ncol=length(1:n),nrow=length(1:nm1))
      rinv <- try(solve(r), TRUE)
      if(is.matrix(rinv)){
        ind <- diag(n)
        pr.mat <- matrix(0,ncol=length(1:nm1),nrow=length(1:n))
        pr.mat[] <- 6*n*lam
        fdub <- pr.mat*t(dim.mat)%*%rinv
        z <- fdub%*%dim.mat+ind
        sbar <- solve(z,t(y))
        d.avg[cnt:(cnt-1+nrow(subs)),5]<- sbar
        cnt<- cnt+nrow(subs)
        b <- 6*rinv%*%dim.mat%*% sbar
        b0 <- rbind(0,b)
        b1 <- rbind(b,0)
        gamma <- (b1-b0) / t(2*delta)
        alfa <- sbar-b0 * t(delta) / 2-gamma * t(delta)^2/3
        xfit<- as.matrix(t(c(1:maxdata)))
        nj<- max(v)
        if (nj > maxdata)
        {nj<- maxdata}
        yfit<- xfit
        for (k in 1:nj){
          xd<-xfit[k]
          if (xd< u[1])
          {p<- alfa[1]} else
          {for (its in 1:n) {
            if(its < n)
            {tf2=as.numeric(xd>v[its] & xd<u[its+1])} else {tf2<-0}
            if (xd>=u[its] & xd<=v[its])
            {p=alfa[its]+b0[its]*(xd-u[its])+gamma[its]*(xd-u[its])^2} else if (tf2)
            {phi=alfa[its+1]-b1[its]*(u[its+1]-v[its])
            p=phi+b1[its]*(xd-v[its])}
          }}
          yfit[k]=p }
        if (nj < maxdata)
        {yfit[,(nj+1):maxdata]=NA}

        yfit[which(yfit > vhigh)] <- vhigh
        yfit[which(yfit < vlow)]  <-vlow
        matrix_yfit[st,]<- yfit
        nd<- length(d)-1
        dl<-d+1
        for (cj in 1:nd) {
          xd1<- dl[cj]
          xd2<- dl[cj+1]-1
          if (nj>=xd1 & nj<=xd2)
          {xd2<- nj-1
          y.avg[st,cj]<- mean(yfit[,xd1:xd2])}
          else
          {y.avg[st,cj]<- mean(yfit[,xd1:xd2])}
          y.avg[st,cj+1]<- max(v)}
         ssq <- sum((t(y)-sbar)^2)
        g <- solve(z)
        ei <- eigen(g)
        ei <- ei$values
        df <- n-sum(ei)
        sig2w <- ssq/df
        dfc <- n-2*sum(ei)+sum(ei^2)
        sig2c <- ssq/dfc
        tmse <- ssq/n-2*s2*df/n+s2
        sset[st] <- tmse

      }
    }

    if (progress) { setTxtProgressBar(pb, st)  }
  }
  if (progress) {
    close(pb)

  }
   y.avg<- as.data.frame(y.avg)
  jmat<- matrix(NA,ncol=1,nrow=length(d))
  for (i in 1:length(d)-1) {
    a1<-paste(d[i],d[i+1],sep="-")
    a1<-paste(a1,"cm",sep=" ")
    jmat[i]<- a1}
  jmat[length(d)]<- "soil depth"
  for (jj in 1:length(jmat)){
    names(y.avg)[jj]<- jmat[jj]
  }
  y.avg<- cbind(mat_id[,1],y.avg)
  names(y.avg)[1]<- "id"

  retval <- list(harmonized.d=y.avg, obs_n_preds=d.avg)

  return(retval)


}
