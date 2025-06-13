# created on Apr 29, 2019 by Weiliang Qiu
#  (1) stacked barplots
#  (2) based on the code at #http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
#  and https://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2

stackedBarPlot.default = function(dat, x, y, group, xlab=x, ylab=y, group.lab=group, title="Stacked barplots",
                          xLevel=NULL, groupLevel=NULL)
{
  myx = NULL
  myy = NULL
  mygroup = NULL

  # Order tbl rows by an expression involving its variables.
  ttdat=dat
  ttdat$myx=dat[, c(x)]
  ttdat$myy=dat[, c(y)]
  ttdat$mygroup=dat[, c(group)]

  if(!is.null(xLevel))
  {
    ttdat$myx = factor(ttdat$myx,
                             levels = xLevel, 
                             ordered = TRUE)
  }
  

  if(!is.null(groupLevel))
  {
    ttdat$mygroup = factor(ttdat$mygroup,
                       levels = groupLevel, 
                       ordered = TRUE)
  }
  
  
  #dat.s <- plyr::arrange(ttdat, myx, mygroup) 
  dat.s = ttdat[order(ttdat$myx, ttdat$mygroup),]
  dat.s$mygroup=as.factor(dat.s$mygroup)
  
  
  # Calculate the cumulative sum of len for each dose
  #dat.cumsum <- plyr::ddply(dat.s, x, transform, label_ypos=myy)
  
  
  # Create the barplot
  g = ggplot(data=dat.s, aes(x=myx, y=myy, fill=mygroup)) +
    geom_bar(stat="identity")+
    geom_text(aes(label=myy), #vjust=1.6, 
              #color="white", size=3.5, position = position_stack()) +
    color="white", size=3.5, position = position_stack(vjust = 0.5)) +
    #color="white", size=3.5, position = "stack") + 
    scale_fill_discrete(name = group.lab) +
    xlab(label=xlab) + ylab(label=ylab) +
    labs(title=title)
  
  g
}



stackedBarPlot = function(dat, catVar, group, 
                          xlab=catVar, ylab="Count", group.lab=group,
                          title="Stacked barplots of counts",
                                  catVarLevel=NULL, groupLevel=NULL,
                          addThemeFlag = TRUE)
{
  x1=dat[, c(catVar)]
  x2=dat[, c(group)]
  tt=table(x1, x2)
  
  freqDat = as.data.frame(tt)
  colnames(freqDat)=c(catVar, group, "Freq")
  
  g = stackedBarPlot.default(dat=freqDat, x=catVar, y="Freq", group=group, 
                             xlab=xlab, ylab=ylab, group.lab=group.lab, title=title,
                                    xLevel=catVarLevel, groupLevel=groupLevel)
  if(addThemeFlag)
  {
    g = addTheme(g)
  }
  g
}

