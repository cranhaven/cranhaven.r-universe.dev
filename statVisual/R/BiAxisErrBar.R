# modified on May 19, 2019
#  (1) automatically set dodge width if 'delta=NULL'.
#      we will set delta=minD/5 , where minD is the minimum of
#      difference between visit[i+1] - visit[i]
#  (2) set default value of addTheme as FALSE
#  (3) fixed a bug when cv1 < cvThresh
#
# modified on May 16, 2019
#  (1) use new algorithm
#
# modified on May 11, 2019
#  (1) added input 'addTheme'
#
# modified on May 9, 2019
#  (1) change seFlag to semFlag
#  (2) change function name to BiAxisErrBar_ggplot_v10
#
# modified on May 8, 2019
#  (1) do transformation first, then calculate mean and SEM
#      by following the document: BiAxisErrBarAlgorithms_v5.docx
#  (2) use same input names as BiAxisErrBar in statVisual v1.0.9
#  (3) set y-limit on left-side y-axis as the 
#      LB=min(min(yleft), min(LBErrBar)))
#      UB=min(max(yleft), max(UBErrBar)))
#
# created by Jun Luo on May 8, 2019
BiAxisErrBar<- function(
  dat,
  group,
  y.left,
  y.right,
  title = "Bi-Axis Error Bar Plot",
  xlab = group,
  ylab.left = y.left,
  ylab.right = y.right,
  legendLabel = "y axis variables",
  delta=NULL, 
  cvThresh = 0.01,
  Ntick = 5,
  semFlag = TRUE, #semFlag = FALSE if SE is required
  GroupLevel=NULL,
  addThemeFlag = FALSE
  ) {

  if(is.null(delta))
  {
    myT = unique(dat[, c(group)])
    if(!is.numeric(myT) && !is.integer(myT))
    {
      myT=as.numeric(as.factor(myT))
    }
    myT.s=sort(myT)
    nT=length(myT.s)
    myT.s2=c(myT.s[2:nT], myT.s[nT])
    diff=myT.s2-myT.s
    diff=diff[-nT]
    minD=min(diff, na.rm=TRUE)
    delta=minD/5
  }

  mu=NULL
  mu1=NULL
  se1=NULL
  mu2=NULL
  se2=NULL
  mu2star=NULL
  se2star=NULL
  Visit=NULL
  Group=NULL
  se=NULL

  if(!is.null(GroupLevel))
  {
    dat[, c(group)] = factor(dat[, c(group)],
			     levels = GroupLevel, 
			     ordered = TRUE)
  }
  
  if(semFlag)
 # SEM
  {
    seFunc = function(s, na.rm = TRUE) {
      if (na.rm) {
        s = s[!is.na(s)]
        sd(s, na.rm=TRUE)/sqrt(length(s))
      } else sd(s, na.rm=TRUE)/sqrt(length(s))
    }
  } else { # SE
    seFunc = function(s, na.rm = TRUE) {
      if (na.rm) {
        s = s[!is.na(s)]
        sd(s, na.rm=TRUE)
      } else sd(s, na.rm=TRUE)
    }
  }
  
  cvFunc = function(s) {
      s = s[!is.na(s)]
      ret = sd(s)/mean(s)
  }

  #  library(tidyverse)
  lty1 = "solid"
  lty2 = "dashed"
  ltype = c(lty1,lty2)
  

  ####
  y1value = dat[,y.left]
  y2value = dat[,y.right]
  
  y1value = y1value[!is.na(y1value)]
  y2value = y2value[!is.na(y2value)]

  y1max = max(y1value)
  y1min = min(y1value)
  y2max = max(y2value)
  y2min = min(y2value)

  # calculate cv=sigma/mu
  y1value_2 = y1value - y1min + 1
  y2value_2 = y2value - y2min + 1
  
  cv1 = cvFunc(y1value_2)
  cv2 = cvFunc(y2value_2)

  #######
  # calculate mu1, se1, mu2, se2
  df1 <- group_by_(dat, group) %>%
    summarise_at(c(y.left,y.right),funs(mean,seFunc),na.rm = TRUE) %>%
    magrittr::set_colnames(c("Visit",'mu1','mu2','se1','se2')) %>%
    mutate(LB1 = mu1 - se1, UB1 = mu1 + se1,LB2 = mu2 - se2, 
	   UB2 = mu2 + se2)
 
  maxUB1 = max(df1$UB1)
  minLB1 = min(df1$LB1)
  maxUB2 = max(df1$UB2)
  minLB2 = min(df1$LB2)

  r2=maxUB2-minLB2
  r1=maxUB1-minLB1


  if (cv1 < cvThresh || cv2 < cvThresh) 
  {
    b = 1  # when CV is small, force b to be 1
  } else {
    b = r2/r1
  }

  a = maxUB2 - b*maxUB1

  # do transformation: yright*=(yright-a)/b
  df1$LB2star=(df1$LB2-a)/b
  df1$UB2star=(df1$UB2-a)/b
  yrightstar = (y2value - a) /b

  y2star=paste(y.right, "star", sep="")
  dat[, c(y2star)]=yrightstar

 
  #ylim_1 = c(min(df1$LB1),max(df1$UB1))

  # calculate mu1, se1, mu2*, se2*
  df1star <- group_by_(dat, group) %>%
    summarise_at(c(y.left,y2star),funs(mean,seFunc),na.rm = TRUE) %>%
    magrittr::set_colnames(c("Visit",'mu1','mu2star','se1','se2star')) %>%
    mutate(LB1 = mu1 - se1, UB1 = mu1 + se1,LB2star = mu2star - se2star, 
	   UB2star = mu2star + se2star)
  
  df = data.frame(Visit = rep(df1star$Visit,2),
		  mu = c(df1star$mu1, df1star$mu2star),
		  se = c(df1star$se1, df1star$se2star),
                  Group = factor(rep(c(ylab.left,ylab.right),
				     each = nrow(df1star))))
  
  df$LB = df$mu - df$se
  df$UB = df$mu + df$se

  maxUB = max(df$UB)
  minLB = min(df$LB)
  step = (maxUB - minLB)/Ntick 

  y1breaks=seq(from=minLB, to=maxUB, by = step)
  y2breaks=a + b * y1breaks

  lab1breaks = round(y1breaks, digits= 2)
  lab2breaks = round(y2breaks, digits = 2)
  
  g1 = ggplot(df, aes(x = Visit, y = mu, group = Group)) + 
       geom_errorbar(aes(linetype = Group,
			 color = Group,
			 ymin = mu - se, 
			 ymax = mu + se),
                     position = position_dodge(delta),
		     width = 0.2) + 
    geom_line(aes(linetype = Group,color = Group), 
	      position = position_dodge(delta))+
    scale_linetype_manual(values = ltype) +
    geom_point(aes(color = Group,shape = Group),
	       position = position_dodge(delta)) 

  g1 <- g1 + scale_y_continuous(breaks = y1breaks,
				labels = lab1breaks, 
				sec.axis = sec_axis(~.*b+a, 
						    breaks = y2breaks,
						    name = ylab.right,
						    labels = lab2breaks))
  g1 = g1 + theme(plot.title = element_text(hjust = 0.5))
  g1 = g1 + labs(x = xlab,y = ylab.left,title = title)
  
  if (is.null(legendLabel)) {legendLabel = group}
  g1 = g1 + scale_color_discrete(name = legendLabel) + scale_linetype_discrete(name = legendLabel) 
  g1 = g1 + scale_shape_discrete(name = legendLabel)

    if(addThemeFlag)
  {
    g1 = addTheme(g1)
  }

  g1
}

