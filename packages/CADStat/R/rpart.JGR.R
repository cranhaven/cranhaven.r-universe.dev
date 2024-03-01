#' @export
rpart.JGR = function(formula, my.data, subset1.name=NULL, subset1.val=NULL, 
                      subset2.name=NULL, subset2.val=NULL,
                      minsplit=20, minbucket=round(minsplit/3),
                      cp=0.01, browserResults=FALSE)
{
  #get a place to store results
  if (browserResults) resultLocation = genResultSpace()

  #data subsetting
  my.data = gisdt.subset(my.data, 
                         subset1.name=subset1.name, subset1.val=subset1.val, 
                         subset2.name=subset2.name, subset2.val=subset2.val)
  #fit
  rpart.fit = rpart(formula=as.formula(formula),data=my.data, minsplit=minsplit,minbucket=minbucket,cp=cp)
  print(summary(rpart.fit))
  
  #plot of the fit
  if (browserResults) {
    #CairoSVG(file=file.path(resultLocation,"TreeRegression.svg"))
    png(filename=file.path(resultLocation,"Tree Regression.png"),width=650,height=800)
  } else {
    JavaGD(height=650,width=800)
  }
  
  par(col="gray",fg="gray")
  plot(rpart.fit, uniform=TRUE, branch=.2, compress=TRUE, margin=.1)
  text(rpart.fit, all=TRUE, fancy=TRUE, pretty=TRUE, fwidth=.8, fheight=0.99, FUN=text.rpart.JGR)
  
  #output results to the browser if necessary
  #output to the console handled previously
  if (browserResults) { 
    dev.off()
    buildresultsXML(object=list(rpart.fit$frame), location=resultLocation, title="Tree Regression Summary")
  }
  par(col="black",fg="black")
  
  return(invisible())
}

#' @export
text.rpart.JGR = function(x,y,label,...){
   if(length(grep(">|<",label))>0){
     label = gsub("< "," < ",label)
     label = gsub(">="," >= ",label)
     rect(x-0.1,y-0.03,x+0.1,y+0.03,col="white",border=FALSE)
     text(x,y,label=label,col="darkblue",...)
   } else {
     text(x,y,label=label,adj=c(0.5,0.9),col="darkred",...)
   }
}
