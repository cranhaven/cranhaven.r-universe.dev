start.valgrid <- function(penden.env) {
  p <- get("p",penden.env)
  X.knots <- matrix(NA,get("DD",penden.env),p)
  tilde.Psi.d.knots.start.r <-  array(NA, dim=c(dim(X.knots)[1],get("ddb",penden.env),p))
    
  for(j in 1:p)  X.knots[,j]  <- get("knots.t",penden.env)[get("Index.basis.D",penden.env)[,j]]
  env.extend <- list()
  for(j in 1:p) {
    name <- c(paste("y",j,sep=""))
    if(!get("cond",penden.env)|get("cond",penden.env)&p==3) env.extend[[noquote(name)]]<-get("knots.help",penden.env)
    if(get("cond",penden.env)&p==4&get("d",penden.env)==2) env.extend[[noquote(name)]]<-get("knots",penden.env)
  }
  assign("X.knots.g.all",expand.grid(env.extend),penden.env)
  tilde.Psi.d.knots.start.g.all <- array(NA, dim=c(dim(get("X.knots.g.all",penden.env))[1],get("ddb",penden.env),p))
  
  assign("X.knots",X.knots,penden.env)
  for (j in 1:p)
    {
      tilde.Psi.d.knots.start.r[,,j] <-  hierarch.bs(X.knots[,j], d = get("d",penden.env), plot.bsp = FALSE,typ=3,penden.env,int=FALSE)$B.tilde
      tilde.Psi.d.knots.start.g.all[,,j] <-  hierarch.bs(get("X.knots.g.all",penden.env)[,j], d = get("d",penden.env), plot.bsp =FALSE,typ=3,penden.env,int=FALSE)$B.tilde
    }

  assign("tilde.PSI.d.D.knots.start.r",tilde.Psi.d.knots.start.r[,get("Index.basis.D",penden.env)[,1],1],penden.env)
  assign("tilde.PSI.d.D.knots.start.g.all",tilde.Psi.d.knots.start.g.all[,get("Index.basis.D",penden.env)[,1],1],penden.env)
  
  for (j in 2:p)
    {
      assign("tilde.PSI.d.D.knots.start.r",get("tilde.PSI.d.D.knots.start.r",penden.env) * tilde.Psi.d.knots.start.r[,get("Index.basis.D",penden.env)[,j],j],penden.env)
      assign("tilde.PSI.d.D.knots.start.g.all",get("tilde.PSI.d.D.knots.start.g.all",penden.env) * tilde.Psi.d.knots.start.g.all[,get("Index.basis.D",penden.env)[,j],j],penden.env)
    }
  if(get("base",penden.env)=="B-spline") {
    ck.val<-solve(get("tilde.PSI.d.D.knots.start.r",penden.env),rep(1,get("DD",penden.env)))
    if(any(get("tilde.PSI.d.D",penden.env)%*%ck.val<0)) print("Startwertproblem")
    assign("ck.val",ck.val,penden.env)
    assign("ck.val.start",ck.val,penden.env)
    assign(x="ck.val.temp",value=get("ck.val",penden.env),envir=penden.env)
  }
}
