pathway_fgsea<-function(db="c1",number_of_k,clusters_data,topPaths=5){

############################# read gene to pathway lists

  #load("R/sysdata.rda")


  d<-if(db=="c1") msigdbr::msigdbr(species = "Homo sapiens", category = "C1",subcategory = NULL)
  else  d<-if(db=="c2") msigdbr::msigdbr(species = "Homo sapiens", category = "C2",subcategory = NULL)
  else  d<-if(db=="c3") msigdbr::msigdbr(species = "Homo sapiens", category = "C3",subcategory = NULL)
  else  d<-if(db=="c4") msigdbr::msigdbr(species = "Homo sapiens", category = "C4",subcategory = NULL)
  else  d<-if(db=="c5") msigdbr::msigdbr(species = "Homo sapiens", category = "C5",subcategory = NULL)
  else  d<-if(db=="c6") msigdbr::msigdbr(species = "Homo sapiens", category = "C6",subcategory = NULL)
  else  d<-if(db=="c7") msigdbr::msigdbr(species = "Homo sapiens", category = "C7",subcategory = NULL)
  else  d<-if(db=="h")  msigdbr::msigdbr(species = "Homo sapiens", category = "H",subcategory = NULL)
####################################### change format to accepted format by fgsea
  unq<-unique(d$gs_name)
  i=1
  ds<-lapply(1:length(unq),FUN = function(i){
    tt<-as.list(d[d$gs_name==unq[i],"entrez_gene"])
    tt<-as.character(tt$entrez_gene)
  })
  names(ds)<-unq
  d<-ds
####################################### fgsea

  top_paths<-lapply(1:number_of_k, function(i){
    cluster_stats<-as.data.frame(clusters_data[[i]])
    stats<-(cluster_stats[,1])
    names(stats)<-rownames(cluster_stats)
    fgseaRes<-fgsea::fgsea(pathways = d,stats = stats,
                           minSize=15,
                           maxSize=500,
                           nperm=1000)

    topPathwaysUp <- fgseaRes[fgseaRes$ES > 0,]
    topPathways <-topPathwaysUp[utils::head(order(topPathwaysUp$pval), n=topPaths), c("pathway","ES","pval")]
    topPathways<-topPathways[order(topPathways$ES),]

  })

  return(top_paths)
}
