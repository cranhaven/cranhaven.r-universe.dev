
GetNextGene_Z<-function(Sub_geneID,pathway,zz,Or_z){
	v1 <- adjacent_vertices(pathway,Sub_geneID);
	v2<- V(pathway)[unlist(v1)]$name
	v2<-unique(v2[!v2%in%Sub_geneID])
	v2_index<-na.omit(match(v2,rownames(zz)))
	Add<-zz[v2_index,"z_score"]
	Add<-as.numeric(Add)
	Add2<-Add+Or_z
	names(Add2) <- rownames(zz)[v2_index]
	Add_z<-Add2/sqrt(length(Sub_geneID)+1)
	return(list(Add2,Add_z))
}


#' @title FindSubPath
#' @description `FindSubPath` uses a greedy search algorithm to search for key subpathways in each entire pathway.
#' @param zz A numeric vector of z_scores.
#' @param Pathway The name of the pathway database.
#' @param delta Diffusion coefficient in each step of searching subpath.
#' @param seed_p Define gene whose p-value smaller than seed_p as seed gene.
#' @param  min.size The smallest size of subpathways.
#' @param out.F Logical,tell if output subpathways.
#' @param out.file file name of subpathways.
#' @return Key dysfunctional subpathways in each pathway, in which the risk score of the genes were significantly higher.
#' @importFrom graphite pathways
#' @importFrom graphite edges
#' @importFrom igraph V
#' @importFrom stats  na.omit
#' @importFrom utils  write.table
#' @importFrom igraph induced_subgraph
#' @importFrom igraph distances
#' @importFrom igraph adjacent_vertices
#' @importFrom igraph graph.data.frame
#' @importFrom igraph simplify
#' @importFrom graphite convertIdentifiers
#' @export
#' @examples
#' require(graphite)
#' zz<-GetExampleData("zzz")
#' \donttest{k<-FindSubPath(zz)}
FindSubPath<-function(zz,Pathway="kegg",delta=0.05,seed_p=0.05,min.size=5,out.F=FALSE,out.file="Subpath.txt"){
seed_z<-zz[,"z_score"]
seedgene<-rownames(zz)[which(as.numeric(zz[,"padjust"])<seed_p)]
Result2<-c()
for(p in 1:length(pathways("hsapiens", Pathway))){
print(p)
mm<-1
edges(convertIdentifiers(pathways("hsapiens", Pathway)[[p]],"symbol"))[,c("src","dest")]->Pairs;
graph.data.frame(unique(Pairs), directed=FALSE, vertices=NULL)->graph;
simpleGraph<-simplify(graph);
seed_index <- which(seedgene%in%V(simpleGraph)$name==TRUE);
if(length(seed_index)!=0){
	Result1<-matrix("",length(seed_index),5)
	for(i in 1:length(seed_index)){
		Sub_geneID <- seedgene[seed_index[i]]
	 	Or_z<- seed_z[match(seedgene[seed_index[i]],rownames(zz))]
	 	Or_z<-as.numeric(Or_z)
		Subpath_z<-Or_z
		nn<-1
		while(length(Sub_geneID)==nn){
			nn<-nn+1;
			AddGENE<-GetNextGene_Z(Sub_geneID,pathway=simpleGraph,zz=zz,Or_z=Or_z)
			if(all(is.na(AddGENE[[2]]))){
				break
			}else{
				max_z<-which.max( AddGENE[[2]])
				Add_Z <- AddGENE[[2]][max_z]
				if(Add_Z/Subpath_z>(1+delta)){
					G<-induced_subgraph(simpleGraph, c(Sub_geneID, names(AddGENE[[2]])[max_z]))
					if(all( distances(G)<=3)==TRUE){
						Subpath_z<-Add_Z

						Sub_geneID <- c(Sub_geneID, names(AddGENE[[2]])[max_z])
						Or_z<-AddGENE[[1]][max_z]
						#print(paste("join the gene",subgene,"into subpathway"))
					}
				}else{
					break
				}

			}
		}
		pathway.name<-names(pathways("hsapiens", Pathway))[p]
		size<-length(Sub_geneID)

		if(size>=min.size){
			id<-pathways("hsapiens", Pathway)[[p]]@id
			substr(id,1,4)<-"path"
			pathway<-cbind(paste0(id,"_",mm),pathway.name)
			Subgene<-Sub_geneID[1]
			for(i in 2:length(Sub_geneID)){
				Subgene<-paste(Subgene,Sub_geneID[i],sep="/")
			}
			Result1[mm,]<-cbind(pathway,Subgene,size,Subpath_z)
			mm<-mm+1
		}
	}

	if(all(Result1=="")==FALSE){
		Result1<-Result1[which(apply(Result1,1,function(x) all(x==""))==FALSE), ,drop=FALSE]
		Result2<-rbind(Result2,Result1)
	}

}

}
if(is.null(Result2)){
  print("We find 0 key pathway")
}else if(is.matrix(Result2)){
  colnames(Result2)<-c("SubpathwayID","pathway","Subgene","Size","SubpathwayZScore")
}else{
  Result2<-data.frame(Result2[1],Result2[2],Result2[3],Result2[4],Result2[5])
  colnames(Result2)<-c("SubpathwayID","pathway","Subgene","Size","SubpathwayZScore")
}
if(out.F==TRUE){
write.table(Result2,out.file,row.names=FALSE,sep="\t")
}
return(Result2)
}


