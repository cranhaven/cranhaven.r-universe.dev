scRNAtools_PEA <-
function(DEGs,number){
TPEA::AUEC
TPEA::TPEA
DEG<-as.matrix(DEGs[,1]);
##Set the times of perturbation
##number<-1000 or 5000;
##Calculate the observed statistic
scores<-AUEC(DEG);
##Significant computational
FDR_method<-"fdr";
results<-TPEA(DEG,scores,number,FDR_method);
results<-as.data.frame(results)
results<-as.matrix(results)
sig_re<-results[which(results[,4]<"0.05"),c(2,4)]
sig_re<-as.matrix(sig_re)
if(ncol(sig_re)==1){
df2<-cbind(sig_re[1,],"TPEA",sig_re[2,])
colnames(df2)<-c("KEGG_ID","Categories","P_TPEA")
}else if(length(sig_re)==0){
df2<-matrix(0,1,3)
df2[1,1]<-"No result"
df2[1,2]<-"TPEA"
df2[1,3]<-0
colnames(df2)<-c("KEGG_ID","Categories","P_TPEA")
}else
{
df2<-cbind(sig_re[,1],rep("TPEA",nrow(sig_re)),sig_re[,2])
colnames(df2)<-c("KEGG_ID","Categories","P_TPEA")
}
return(df2)


####Plot the results.
ggplot2::ggplot
ggplot2::geom_bar
ggplot2::theme
ggplot2::guides
ggplot2::guide_legend
ggplot2::unit
ggplot2::ggtitle
ggplot2::element_blank
ggplot2::aes
ggplot2::facet_grid
ggplot2::coord_flip
Rmisc::multiplot
ggthemes::theme_wsj
ggthemes::scale_colour_wsj
ggthemes::scale_fill_wsj


#####methods
mydata2<-as.data.frame(df2)
if(nrow(mydata2)>10){
p2<-ggplot(mydata2[1:10,],aes('KEGG_ID','P_TPEA',fill='Categories'))+
geom_bar(stat="identity",position="stack",na.rm=TRUE)+
theme_wsj()+
scale_fill_wsj(palette = "rgby", "")+
theme(axis.ticks.length=unit(0.5,'cm'))+
guides(fill=guide_legend(title=NULL))+
theme(axis.title = element_blank(),legend.position='none')+ facet_grid(Categories~.)+coord_flip()
}else
{
p2<-ggplot(mydata2,aes('KEGG_ID','P_TPEA',fill='Categories'))+
geom_bar(stat="identity",position="stack",na.rm=TRUE)+
theme_wsj()+
scale_fill_wsj(palette = "rgby", "")+
theme(axis.ticks.length=unit(0.5,'cm'))+
guides(fill=guide_legend(title=NULL))+
theme(axis.title = element_blank(),legend.position='none')+ facet_grid(Categories~.)+coord_flip()
}

multiplot(p2,cols = 1)
}
