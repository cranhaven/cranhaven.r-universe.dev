scRNAtools_pca <-
function(example1,types){
pca<- prcomp (t(example1[,-1]), scale=F)
summary (pca)
group<-factor(t(types))
ggplot2::ggplot
group2<-data.frame (group)
pca_reuslt<-as.data.frame (pca$x)
pca_reuslt<-cbind (pca_reuslt,group2)
p<-ggplot (pca_reuslt)+geom_point(aes (x=pca_reuslt[,1],y=pca_reuslt[,2], color=pca_reuslt$group,shape = pca_reuslt$group),size=2)
p<-p+theme (legend.title =element_blank())+labs (x="PCA1" , y="PCA2" )
return(p)
print("PCA has done !")
}
