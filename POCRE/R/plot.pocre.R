plot.pocre<-function(x, x.id=NA, which=1:2, cex=.5, ...)
{
  if(is.na(x.id)) x.id <- 1:dim(x$beta)[1]

  # Plot x.id vs. beta
  if( any(which==1) )
    plotxbeta(pobj=x,x.id=x.id,...)

  # Plot x.id vs. loading of each component
  if( any(which==2) )
    plotcomponents(pobj=x,x.id=x.id,...)
}



# Plot x.id vs. beta
plotxbeta<-function(pobj, x.id=NA, cex=.5, ...)
{
  pl<-function(i){
    ggplot(...)+
      geom_point(aes(x=x.id,y=pobj$beta[,i]),color=4,shape=4,size=cex) +
      labs(title=paste('Y_',i,sep=""),x="index of predictor",y=paste('beta_',i,sep=""))+
      theme(plot.title = element_text(hjust=0.5))
  }

  ny <- dim(pobj$beta)[2]
  pp<-lapply(1:ny,pl)
  for(i in 1:ny) print(pp[[i]])
}



# Plot x.id vs. loading of each component
plotcomponents<-function(pobj, x.id=NA, cex=.5, ...)
{
  pl<-function(i){
    ggplot(...)+
      geom_point(aes(x=x.id,y=pobj$varpi[,i]),color=4,shape=4,size=cex) +
      labs(title=paste("vartheta_",i,"=",round(pobj$vartheta[i],digits=4),sep=""),
           x="index of predictor",y=paste('loading of component',i))+
      theme(plot.title = element_text(hjust=0.5))
  }

  pp<-lapply(1:pobj$nCmp,pl)
  for(i in 1:pobj$nCmp) print(pp[[i]])
}
