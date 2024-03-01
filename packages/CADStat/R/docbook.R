# Generic function starts method dispatch
# Add all S3 methods to namespace

# A description of docbook
# @title docbook: General function for preparing output
# @param x numeric number
# @examples
# a <- 1
# class(a) <- "try"
# docbook(a)
#' @export


docbook <- function (x,...) {
  UseMethod("docbook")
}

#' @export
docbook.try <- function(x,...){
  sectNode <- xmlNode("sect1")
  append.xmlNode(sectNode,xmlNode("para","error"))}

#' @export
docbook.default <- function (x,numTable=NULL,...) 
{
  sectNode <- xmlNode("sect1")
  if(!is.null(x$method) & !is.null(numTable))
    sectNode <- append.xmlNode(sectNode,xmlNode("title",paste("Table ",numTable,".  ",x$method,sep="")))
  else if(!is.null(x$method))
    sectNode <- append.xmlNode(sectNode,xmlNode("title",x$method))
  x <- unlist(x)
  x <- x[!names(x) %in% c("method","data.name")]
  append.xmlNode(sectNode,
                 lapply(1:length(x),
                        function(i,x){
                          xmlNode("para",paste(names(x[i])," = ",round(as.numeric(x[i]),2),sep=""))
                          },x)
                 )
}

#' @export
docbook.data.frame <- function (x,title=NULL,file=NULL,rowNames=TRUE,digits=4,numTable=NULL,...){
  tableNode <- xmlNode("table",attrs=c("role"="results"))
###  title <- attributes(x)$title
  if(!is.null(title)){
    x <- x[,!(names(x) %in% c("title"))]
###    tableNode <-  append.xmlNode(tableNode,xmlNode("title",title))

  }else if(!is.null(attr(x,"title"))){
    title <- attr(x,"title")
###    tableNode <-  append.xmlNode(tableNode,xmlNode("title",attr(x,"title")))
  }else{
    title <- ""
  }
  if(!is.null(numTable))
    title <- paste("Table ",numTable,".  ",title,sep="")

  tableNode <-  append.xmlNode(tableNode,xmlNode("caption",title))
  
  tgNode <- xmlNode("tgroup",attrs=c(cols=length(names(x))+1))
  thNode <- xmlNode("thead")
  thrNode <- xmlNode("row")
  colNames <- names(x)
  rowNames <- any(grep("[a-z]",row.names(x),ignore.case=TRUE))
  if(rowNames){
    colNames <- c("",names(x))
  }
  for(colName in colNames){
    thrNode <- append.xmlNode(thrNode,xmlNode("entry",colName))
  }
  thNode <-  append.xmlNode(thNode,thrNode)
  tgNode <-  append.xmlNode(tgNode,thNode)
  tbNode <- xmlNode("tbody")
  for(rowName in row.names(x)){
    tbrNode <- xmlNode("row")
    if(rowNames){
      tbrNode <- append.xmlNode(tbrNode,xmlNode("entry",rowName))
    }
    tmp <- options()$warn
    options(warn=-1)
    for(j in 1:length(x[rowName,])){
      result <- as.character(x[rowName,j])
      if(is.na(as.numeric(result)))
        tbrNode <- append.xmlNode(tbrNode,xmlNode("entry",result))
      else
        tbrNode <- append.xmlNode(tbrNode,xmlNode("entry",signif(as.numeric(result),digits)))
    }
    options(warn=tmp)
    tbNode <- append.xmlNode(tbNode,tbrNode)
  }
  tgNode <-  append.xmlNode(tgNode,tbNode)
  tableNode <-  append.xmlNode(tableNode,tgNode)
  sectNode <- append.xmlNode(xmlNode("sect1"),tableNode)
  
  if(!is.null(file)){
   saveXML(tableNode,file)
  }else{
   return(tableNode)
  }
}

#' @export
docbook.htest <- function (x,file=NULL,digits=4,numTable=NULL,...){
  method <- x$method
  x <- unlist(x)
  x <- x[!names(x) %in% c("method","data.name")]
  tableNode <- xmlNode("table",attrs=c("role"="results"))
  if(!is.null(numTable))
    tableNode <-  append.xmlNode(tableNode,xmlNode("title",paste("Table ",numTable,".  ",method,sep="")))
  else
    tableNode <-  append.xmlNode(tableNode,xmlNode("title",method))    
  tgNode <- xmlNode("tgroup",attrs=c(cols=length(x)))
  thrNode <- xmlNode("row")
  tbNode <- xmlNode("tbody")
  for(colName in names(x)){
    tbrNode <- xmlNode("row")
    tbrNode <- append.xmlNode(tbrNode,xmlNode("entry",colName))
    result <- x[[colName]]
    if(is.na(as.numeric(result)))
      tbrNode <- append.xmlNode(tbrNode,xmlNode("entry",result))
    else
      tbrNode <- append.xmlNode(tbrNode,xmlNode("entry",signif(as.numeric(result),digits)))
    tbNode <-  append.xmlNode(tbNode,tbrNode)
  }
  tgNode <-  append.xmlNode(tgNode,tbNode)
  tableNode <-  append.xmlNode(tableNode,tgNode)
  sectNode <- append.xmlNode(xmlNode("sect1"),tableNode)
  
  if(!is.null(file)){
		saveXML(sectNode,file)  
  }else{
   return(sectNode)
  }
}

#' @export
docbook.glm = function(x,...){
  if(x$family$family=="gaussian"){
    return(docbook.lm.summary(summary.lm(x),...))
  } else {
    return(docbook.glm.summary(summary(x),...))
  }
}

#' @export
docbook.glm.summary = function(x,file=NULL,...){
  sectNode = xmlNode("sect1")
  coefs = data.frame(coef(x),check.names=FALSE)
  coefNode = docbook(coefs)
  dev1Node = xmlNode("para",
    "Null Deviance: ",
    xmlNode("span",attrs=c(class="highlight"),signif(x$null.deviance,4)))
  dev2Node = xmlNode("para",
    "Residual Deviance: ",
    xmlNode("span",attrs=c(class="highlight"),signif(x$deviance,4)))
  chist = x$null.deviance-x$deviance
  chidf = x$df.null-x$df.residual
  chistNode = xmlNode("para",
    "Chi-squared statistic: ",
    xmlNode("span",attrs=c(class="highlight"),signif(chist,4)), " on ",
    xmlNode("span",attrs=c(class="highlight"),signif(chidf,4)), " degrees of freedom.  Pr(>|Chi|) = ",
    xmlNode("span",attrs=c(class="highlight"),signif(pchisq(chist,chidf,lower.tail=FALSE),4)))
  sectNode = append.xmlNode(sectNode,coefNode)
  sectNode = append.xmlNode(sectNode,dev1Node)
  sectNode = append.xmlNode(sectNode,dev2Node)
  sectNode = append.xmlNode(sectNode,chistNode)
}

#' @export
docbook.lm <- function (x,...){
  return(docbook.lm.summary(summary(x),...))
}

#' @export
docbook.lm.summary <- function (x,file=NULL,...){
  sectNode <- xmlNode("sect1")
#  callNode <- xmlNode("para",paste("Call: ",print(x$call),sep=""))
#  sectNode <- append.xmlNode(sectNode,callNode)
  coefs <- data.frame(coef(x),check.names=FALSE)
  coefNode <- docbook(coefs)
  rseNode <- xmlNode("para",
    "Residual standard error: ",
    xmlNode("span",attrs=c(class="highlight"),signif(x$sigma,4)),
    " on ",
    xmlNode("span",attrs=c(class="highlight"),x$df[1]),
    " degrees of freedom"
    )
  r2Node <- xmlNode("para",
    xmlNode("tip",attrs=c(title="The fraction of the variance explained by the model"),"Multiple R-Squared"),
    ": ",
    xmlNode("span",attrs=c(class="highlight"),paste(signif(x$r.squared,3)," ")),
    xmlNode("tip",attrs=c(title="The fraction of the variance explained by the model penalized by the number of parameters")," Adjusted R-squared"),
    ": ",
    xmlNode("span",attrs=c(class="highlight"),signif(x$adj.r.squared,3))
    )
  fstatNode <- xmlNode("para",
    "F-statistic: ",
    xmlNode("span",attrs=c(class="highlight"),signif(x$fstatistic[1],3)),
    " on ",
    xmlNode("span",attrs=c(class="highlight"),signif(x$fstatistic[2],0)),
    " and ",
    xmlNode("span",attrs=c(class="highlight"),signif(x$fstatistic[3],0)),
    " degrees of freedom"
    )

  sectNode <- append.xmlNode(sectNode,coefNode)
  sectNode <- append.xmlNode(sectNode,rseNode)
  sectNode <- append.xmlNode(sectNode,r2Node)  
  #sectNode <- append.xmlNode(sectNode,fstatNode)
  if(!is.null(file)){
		saveXML(sectNode,file)  
  }else{
    return(sectNode)
  }
}

#' @export
docbook.hmtestp <- function (x,file=NULL,...){
  if (x$asympt)
    method <- paste("Asymptotic simultaneous tests:", x$ctype)
  else
    method <- paste("Simultaneous tests:", x$ctype)
  ps <- data.frame(x$p.value.adj)
  names(ps) <- "p.value.adj"
  pNode <- docbook.data.frame(ps)
  sectNode <- append.xmlNode(xmlNode("sect1"),pNode)

  if(!is.null(file)){
    handle <- file(file,open="w")
    sink(handle)
    print(sectNode)
    sink()
    close(handle)
  }else{
    return(sectNode)
  }
}
