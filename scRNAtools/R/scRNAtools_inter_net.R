scRNAtools_inter_net <-
function(corr_re,p,r,size,color){
igraph::graph.data.frame
net<-corr_re[which(corr_re[,4]<p),]
net1<-net[which(net[,3]>r),]
d<-data.frame(from_id=net1[,1],to_id=net1[,2])
g<-graph.data.frame(d,directed=FALSE)
plot(g,vertex.size=size,vertex.color=color,vertex.label.cex=0.5,edge.arrow.size=0.2,layout=layout.kamada.kawai)
}
