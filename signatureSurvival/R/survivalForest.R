survivalForest <-
function(sdata,xtick){

colname<-colnames(sdata)

ind<-is.element(colname,c("variate","Variate","variable","Variable","category",
                          "Catergory","factor","Factor"))
jj<-which(ind==TRUE)
if(length(jj)==1){
  colname[jj]<-"variable"
}else{
  stop("no variable or variate or factor column in the inputing data")
}
ind<-is.element(colname,c("beta","Beta","coefficient","HR","hazard risk"))
jj<-which(ind==TRUE)
  if(length(jj)==1){
    colname[jj]<-"HR"
  }else{
    stop("no beta or HR or coefficient column in the inputing data")
  }

ind<-is.element(colname,c("se","SE","standard error"))
jj<-which(ind==TRUE)
if(length(jj)==1){
  colname[jj]<-"SE"
}else{
  stop("no SE or se or standard error column in the inputing data")
}

ind<-is.element(colname,c("pvalue","p_value","p-value","p value","pv","PV"))
jj<-which(ind==TRUE)
if(length(jj)==1){
  colname[jj]<-"pvalue"
}else{
  stop("no p_value or pvalue or pv column in the inputing data")
}
colnames(sdata)<-colname
group<-c("variable",as.character(sdata$variable))
beta<-c("HR",round(as.numeric(sdata$HR),4))
se<-c("SE",round(as.numeric(sdata$SE),4))
pvalue<-c("pvalue",round(as.numeric(sdata$pvalue),6))


ind1<-is.element(colname,c("stage","Stage"))
ind2<-is.element(colname,c("model","Model"))
j1<-which(ind1==TRUE)
j2<-which(ind2==TRUE)

if(length(j1)==1&&length(j2)==0){
  colname[jj]<-"stage"
  colnames(sdata)<-colname
  stage<-c("stage",as.character(sdata$stage))
  tabletext<-cbind(stage,group,beta,se,pvalue)
 }else if(length(j1)==0&&length(j2)==1){
   colname[jj]<-"model"
   colnames(sdata)<-colname
   mod<-c("model",as.character(sdata$model))
   tabletext<-cbind(mod,group,beta,se,pvalue)
  }else if(length(j1)==1&&length(j2)==1){
  colname[j1]<-"stage"
  colname[j2]<-"model"
  colnames(sdata)<-colname
  stage<-c("stage",as.character(sdata$stage))
  mod<-c("model",as.character(sdata$model))
  tabletext<-cbind(mod,stage, group,beta,se,pvalue)
  }else{
  tabletext<-cbind(group,beta,se,pvalue)
}

m<-c(as.numeric(sdata$HR))
l<-m-c(sdata$SE)*1.96
u<-m+c(sdata$SE)*1.96
m<-c(NA,m)
l<-c(NA,l)
u<-c(NA,u)
#length(m)

forestplot(labeltext=tabletext,
           mean=m,
           lower=l,
           upper=u,
           align = NULL,
           is.summary = FALSE,
           graph.pos = "right",
           hrzl_lines = NULL,
           clip = c(-Inf, Inf),
           xlab = NULL,
           zero =  0,
           graphwidth = "auto",
           colgap = NULL,
           lineheight = "auto",
           line.margin = NULL,
           col =  fpColors(lines = "blue", box = "blue", zero = "blue"),
           txt_gp = fpTxtGp(),
           xlog = FALSE,
           xticks = xtick,
           xticks.digits = 2,
           grid = FALSE,
           lwd.xaxis = 3,
           lwd.zero = 1,
           lwd.ci =  NULL,
           lty.ci = 1,
           ci.vertices = NULL,
           ci.vertices.height = 1,
           boxsize = 0.5,
           mar = unit(rep(5, times = 4), "mm"),
           title = NULL,
           legend = NULL,
           legend_args = fpLegend(),
           new_page = getOption("forestplot_new_page", FALSE),
           fn.ci_norm = fpDrawNormalCI,
           fn.ci_sum = fpDrawSummaryCI,
           fn.legend = NULL,
           shapes_gp = fpShapesGp()
          )
#|>
#        fp_set_style(box = "royalblue",
#                    line = "darkblue",
#                    summary = "royalblue")

}
