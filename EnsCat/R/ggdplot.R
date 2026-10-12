ggdplot<-function(hc,lab=NULL,ptype=1,title=NULL, ...){
	hcdata<-dendro_data(hc)
	if(!is.null(lab)){
		hcdata$labels[,3]<-lab
		}
	if(ptype==1){
		#	horizontal plot with labels
		if(!is.null(title)){
		 	gg<-ggdendrogram(hcdata, rotate=TRUE, size=2) + labs(title=title)
		}
		if(is.null(title)){
		 	gg<-ggdendrogram(hcdata, rotate=TRUE, size=2) + labs(title="Dendrogram in ggplot2")
		 }
	}
	if(ptype!=1){
# 	horizontal plot with labels and grid
 		gg<-ggdendrogram(hcdata, rotate = TRUE, theme_dendro = FALSE)
	} 
	gg
}
