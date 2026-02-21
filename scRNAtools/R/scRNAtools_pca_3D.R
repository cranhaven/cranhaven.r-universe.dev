scRNAtools_pca_3D <-
function(example1,types){
scatterplot3d::scatterplot3d
grDevices::rainbow
grDevices::pdf
pdf(file=file.path(tempdir(), "PCA_result_3D.pdf"))
par (mar=c (5.1, 4.1, 4.1, 8.1), xpd=TRUE )
group<-factor(t(types))
group2<-data.frame (group)
colour_group<-rainbow (length (unique (group)))#choose colors
colour<-colour_group[as.numeric (factor (group))]#set color vector
pca<- prcomp (t(example1[,-1]), scale=F)
pca_reuslt<-as.data.frame (pca$x)
pca_reuslt<-cbind (pca_reuslt,group2)
scatterplot3d (pca_reuslt[,1:3],pch=20 ,color=colour, angle=45, main ="PCA analysis_3D", cex.symbols =1, mar=c (5.1, 4.1, 4.1, 8.1))
legend ("right" , legend = group,col = colour,pch =20, bg="white" , xpd=TRUE , inset= -0.5) ##set the location to right and adjust "insert" to move the location of legend.
dev.off()
}
