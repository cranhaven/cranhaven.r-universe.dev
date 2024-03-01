# Get place to store results
# require outdated, new standard uses requireNamespace()
#require(lattice)
# Better practice remove lattice from import and add sparingly lattice:: within module
# need to come  back and refine  <--------------------------- ultimately remove lattice from full import
requireNamespace("lattice", quietly = TRUE)
#' @export
bxplot.JGR <- function(my.data, result, group.name=NULL, group.val=NULL,
                        subset1.name=NULL, subset1.val=NULL, subset2.name=NULL,
                        subset2.val=NULL, main="", result.lab="",
                        plot.type="both", cex.main=1, cex.lab=1, cex=1,
                        cex.axis=1, horizontal=FALSE,iSampleSize=TRUE, names.rot=FALSE, ...)
{
  # draws boxplots using the JGR dialog box ui input
  
  # my.data			data.frame
  
  #these are no longer part of the UI
  # group.name    name of the data set
  # subset1.name	column name of the first subsetting variable (panel)
  # subset1.val		values of the first subsetting variable
  # subset2.name	column name of the second subsetting variable (group)
  # subset2.val		values of the second subsetting variable
  
  # result			column name containing the data to be plotted
  # main			  plot title
  # ylab			  y-axis label
  # plot.type		indicates the desired scale - "original", "log", or "both"
  # cex.main		magnification of the plot title
  # cex.lab			magnification of the axes labels
  # cex				  magnification of the plotted points and lines
  # cex.axis		magnification of the axes
  # names.rot		indicates whether y-axis labels should be rotated 90 degrees
  


  
  #no longer outputting to the browser - DAB, 01/13/2010
  #resultLocation = genResultSpace()
  
  # find proper data subset
  my.data = gisdt.subset(my.data,
							subset1.name=subset1.name, subset1.val=subset1.val, 
							subset2.name=subset2.name, subset2.val=subset2.val,
							na.check=c(result,group.name))

  my.data$scale = "Original"
  if (plot.type=="both" | plot.type == "log") {
    if(all(my.data[,result]>0)){
      #If were plotting both , create log and regular version of data.
      #Each variable has a scale type assigned to it. Works for horizontal as well.
      if (plot.type =="both") {
        tmp = my.data
        tmp$scale = "Log10"
        tmp[,result] = log10(my.data[,result])
        my.data = rbind(my.data,tmp)
        rm(tmp)
      }
      else {
        my.data$scale = "Log10"
        my.data[,result] = log10(my.data[,result])
      }
    }
    else {
      plot(c(0,1),c(0,1),axes=FALSE,type="n",ann=FALSE);
      text(c(0.5,0.5),c(0.5,0.5),"The Result values all \n need to be positive \n to log transform the data",cex=2)
      return()
    }
  }
 
  #subset group.name if necessary and build the plotting formula
  if(!is.null(group.name)) 
  {
    my.data[,group.name] =  as.character(my.data[,group.name])
    if(length(group.val)>0) my.data = my.data[my.data[,group.name] %in% group.val,]
    #print(my.data, quote = TRUE, row.names = TRUE)
  	if (horizontal) {
      	bw.formula = formula(paste(group.name, "~", result, "| scale")) 
    } else {
      	bw.formula = formula(paste(result, "~", group.name, "| scale"))
    }
  } else {
    my.data$bw_x = result
    if (horizontal) {
    	  bw.formula = formula(paste("bw_x ~", result, "| scale"))
    } else {
    	  bw.formula = formula(paste(result, "~ bw_x | scale"))
    }
  }

  bw.obj = bwplot(bw.formula,data=my.data, panel = function(x,y,...){
    panel.bwplot(x,y,...)
    if(iSampleSize){
      if(!horizontal){
        #the text that goes on the panel
        panel.text(sort(unique(x)),current.panel.limits()$ylim[1],
                   label=paste("n=",rowSums(table(x,y)),sep=""),
                   pos=3,cex=.75)#,adj = c(-0.5, -0.5))    
      }else{
        panel.text(current.panel.limits()$xlim[1],sort(unique(y)),
                   label=paste("n=",rowSums(table(y,x)),sep=""),
                   cex=.75,adj = c(-0.5,7.5))
      }
    }
  },
    cex=cex,
    strip=!(length(unique(my.data$scale))==1 & unique(my.data$scale)=="Original"),
    #Direction of labels, tick marks, scale to use, etc.
    ylab=ifelse(horizontal,"",list(result.lab,cex.lab=cex.lab)), 
    xlab=ifelse(horizontal,list(result.lab,cex.lab=cex.lab)," "),
    main=list(label=main,cex=cex.main),
    horizontal=horizontal,
    scales=list(y=list(relation='free',rot=0,cex=cex.axis),  
      x=list(relation='free', rot=as.numeric(names.rot)*90,cex=cex.axis)))
   
  JavaGD(height=500,width=600)
  print(bw.obj)
  return(invisible())
}