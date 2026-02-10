## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8, fig.height=4
)

## ----setup--------------------------------------------------------------------
library(lwqs)


## ---- echo=TRUE---------------------------------------------------------------
library(lwqs)
data(lwqs_data)

## ---- echo=FALSE, R.options = list(width = 200)-------------------------------------------------------------------------------------------------------------------------------------------------------
head(lwqs_data)


## ---- echo=F------------------------------------------------------------------
library(data.table)
library(ggplot2)
time=seq(1, 30, 1)    #time vector

#beta for predictor 1
beta1=data.frame(seq(1,length(time),1))
beta1[1:10,]<-0
beta1[11:20,]<-0.35
beta1[21:30,]<-0
beta1$time<-seq(1,length(time),1)
names(beta1)[1]<-"beta1"

#beta2
beta2=data.frame(seq(1,length(time),1))
beta2[1:10,]<-0
beta2[11:20,]<-0.25
beta2[21:30,]<-0
beta2$time<-seq(1,length(time),1)
names(beta2)[1]<-"beta2"

#beta3
beta3=data.frame(seq(1,length(time),1))
beta3[1:10,]<--0.25
beta3[11:20,]<-0
beta3[21:30,]<-0
beta3$time<-seq(1,length(time),1)
names(beta3)[1]<-"beta3"

#beta 4
beta4=data.frame(seq(1,length(time),1))
beta4[1:10,]<--0.35
beta4[11:20,]<-0
beta4[21:30,]<-0
beta4$time<-seq(1,length(time),1)
names(beta4)[1]<-"beta4"

#beta 5
beta5=data.frame(seq(1,length(time),1))
beta5[1:30,]<- -0
beta5$time<-seq(1,length(time),1)
names(beta5)[1]<-"beta5"

#merge betas
betas=cbind(beta1, beta2, beta3, beta4, beta5)
betas=betas[,-c(2,4,6,8)]
blong=melt(setDT(betas), measure=names(betas)[1:5], id.vars="time")
ggplot(blong, aes(x=time, y=value, group=variable, color=variable)) + geom_line(size=1)+
  theme_bw() + ylab("Simulated Association with Outcome")



## ---- echo=T------------------------------------------------------------------
mixvars=names(lwqs_data)[5:9]

## ---- echo=T, fig.align='center'----------------------------------------------
posmod=lwqs(data=lwqs_data,                       #specifies the dataframe containing study data
            timevar="time",                       #specifies the variable that denotes temporal intervals
            wqs_parms=list(formula=out ~ wqs,     #formula for the WQS component of the model, applied at each timepoint
                           data = lwqs_data,      #data frame, as above, for reference by the gWQS algorithm     
                           mix_name=mixvars,      #mixture variables, identified previously
                           b1_constr = T,         #specificies use of a directionality constraint
                           b1_pos=T,              #specifies directionality of constraint, in this case positive
                           b = 5,                 #specifies number of bootstraps used at each temporal interval
                           q = 5,                 #specifies quantiling, in this case to quintiles. 
                           validation = 0,        #specifies that no validation split is done on the data
                           family = "gaussian",   #indicates identity link appropriate for gaussian outcomes
                           seed = 1),           #specifies seed for reproducibility
            outcome="out",                        #specifies outcome variable
            ID="ID")                              #specifies ID variable that identifies each subject




## ---- echo=T------------------------------------------------------------------
negmod=lwqs(data=lwqs_data,                       
            timevar="time",                       
            wqs_parms=list(formula=out ~ wqs,     
                           data = lwqs_data,           
                           mix_name=mixvars,      
                           b1_constr = T,         
                           b1_pos=F,              #Parameter to specificy directionality. Specify "F" for negative. 
                           b = 5,                 
                           q = 5,                 
                           validation = 0,        
                           family = "gaussian",   
                           seed = 1),          
            outcome="out",                        
            ID="ID")                              




## ---- echo=T------------------------------------------------------------------
negcovmod=lwqs(data=lwqs_data,                       
            timevar="time",                       
            wqs_parms=list(formula=out ~ wqs + as.factor(sex),   #specifies covariate adjustment for sex variable     
                           data = lwqs_data,           
                           mix_name=mixvars,      
                           b1_constr = T,         
                           b1_pos=F,              
                           b = 5,                 
                           q = 5,                 
                           validation = 0,        
                           family = "gaussian",   
                           seed = 1),          
            outcome="out",                        
            ID="ID")                              




## ---- echo=T, eval=FALSE------------------------------------------------------
#  
#  #gWQS (and covariate) effect estimates at time point 1
#  summary(negcovmod$parameters$res$`1`)
#  
#  

## ---- echo=T------------------------------------------------------------------

#extract time-varying wqs index
timewqs=extract_mixture(negcovmod)


## ---- echo=T, warning=F-------------------------------------------------------
library(gamm4)

#merge covariate data
timewqs=merge(timewqs, unique(lwqs_data[,1:2]))

#reconstruct gam with fixed effect of sex
sexgam=gamm4(wqs ~ s(time, by=y, bs="cr") + sex,   #model formula
             data=timewqs,                         #dataset
             random = ~ (1 | ID))                 #random term for within-subject correlation

plot(sexgam$gam)

