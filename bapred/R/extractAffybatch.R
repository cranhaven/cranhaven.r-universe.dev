extractAffybatch <-
function(inds,abo){
    exprs<-Biobase::exprs(abo[,inds])
    phenoData<-Biobase::phenoData(abo)[inds,]
    description<-new('MIAME')
    description@title<-'Extracted Affy Batch'
    Biobase::notes(description)<-'do not read this'
    Biobase::exprs(abo)<-exprs
    Biobase::phenoData(abo)<-phenoData
    Biobase::experimentData(abo)<-description
    #abo@description<-description
    Biobase::protocolData(abo)<-Biobase::protocolData(abo)[inds,]
    Biobase::featureData(abo)<-Biobase::featureData(abo)[inds,]
    abo
}
