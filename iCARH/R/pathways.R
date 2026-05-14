#' @title Builds pathways adjacency matrices
#'
#' @description Builds pathways adjacency matrices from specified KEGG identifiers.
#' Returns a list of pathway adjacency matrices based on shortest paths.
#'
#' @param keggid KEGG identifiers as specified in KEGG.
#' keggid is list that might contain multiple identifiers per metabolite.
#' @param org organism
#'
#' @return list of pathway matrices based on shortest paths between two metabolites
#'
#' @examples keggid = list("C08363")
#' iCARH.getPathwaysMat(keggid, "rno")
#' \donttest{keggid = list("Unk1", "C00350",c("C08363", "C01245"))
#' iCARH.getPathwaysMat(keggid, "rno")}
#'
#' @export iCARH.getPathwaysMat

iCARH.getPathwaysMat = function(keggid, org){
  if(length(keggid)<1) stop("Two compounds or more are needed to build the adjacency matrix.")
  compoundPath = FindMetabolitePathways(unlist(keggid), org)
  if(is.null(compoundPath)){
    return(NULL)
  }
  membmat = MembershipFromPathways(compoundPath)
  #keggid : for unknowns id="Unk[number]"
  membmat=do.call(rbind,lapply(keggid, function(x) {
    if(grepl("Unk",paste(x, collapse="|")) | length(grep(paste(x, collapse="|"),rownames(membmat)))==0 ) rep(0,ncol(membmat))
    else colSums((membmat[grep(paste(x, collapse="|"), rownames(membmat)),, drop=F]))>0}))
  rownames(membmat)=sapply(keggid, function(x) paste(x, collapse="|"))
  # remove global pathway map01100
  membmat = membmat[,-which(grepl("01100",colnames(membmat))), drop=FALSE]
  if(ncol(membmat)==0) {
    print("No pathway.")
    return(NULL)
  }
  pathways = GetDistanceMat(membmat)
  return(pathways)
}

## Helper functions

################### convertTable ####################
# modified MetaboSignal function : http://bioconductor.org/packages/release/bioc/html/MetaboSignal.html
convertTable = function(res) {
  if (nchar(res) == 0 || res == "\n") {
    print("no result")
    result = NULL
  } else {
    rows = strsplit(res, "\n")
    rows.len = length(rows[[1]])
    result = matrix(unlist(lapply(rows, strsplit, "\t")), nrow = rows.len,
                    byrow = TRUE)
  }
  return(result)
}

#################### FindMetabolitePathways ####################
FindMetabolitePathways = function(compoundM=NULL, organism_code=NULL){
  compounds.string = paste0(compoundM,collapse="+")
  file = paste0("http://rest.kegg.jp/link/pathway/cpd:",compounds.string)
  response = RCurl::getURL(file)
  compoundPath = convertTable(response)
  if(!is.null(compoundPath)){
    colnames(compoundPath) = c("KEGG compound", "pathway")
    compoundPath[,2] = as.character(gsub("path:map", paste0("path:",organism_code), compoundPath[,2]))
    aux = RCurl::getURL(paste0("http://rest.kegg.jp/link/pathway/",organism_code))
    path.org = unique(convertTable(aux)[,2]) # check pathways in the organism
    compoundPath = compoundPath[compoundPath[,2]%in%path.org,, drop=F]
  }
  return(compoundPath)
}

#################### MembershipFromPathway ####################
MembershipFromPathways = function(compoundPath=NULL){
  compoundPath = compoundPath[order(compoundPath[,"KEGG compound"]),, drop=F]
  pathways = unique(compoundPath[,"pathway"])
  compounds = unique(compoundPath[,"KEGG compound"])
  compoundsOcc = sapply(compounds,function(x)sum(compoundPath[,"KEGG compound"]==x)) #table funtion sorts the output
  compoundsInd = match(compounds, compoundPath[,"KEGG compound"])
  compoundsNb = length(compounds)
  pathwaysNb = length(pathways)
  membershipMat = matrix(0, nrow=compoundsNb, ncol=pathwaysNb)
  colnames(membershipMat) = pathways
  row_names = compounds
  sum_repl = 0
  for(i in 1:compoundsNb){
    ind = compoundsInd[i]:(compoundsInd[i]+compoundsOcc[i]-1)
    ipathways = which(pathways%in%compoundPath[ind,"pathway"])
    membershipMat[i+sum_repl,ipathways] = 1
    row_names[i+sum_repl] = compounds[i]
    # replicate row for metabolites with same KEGG identifier
    repl = unique(table(compoundPath[ind,"pathway"]))
    if(length(repl)>1) stop("Problem occured: Non consistent data! Please check KEGG query.")
    else if(repl > 1){
      membershipMat = rbind(membershipMat, matrix(0, nrow=repl-1, ncol=pathwaysNb))
      row_names = c(row_names, rep(0,repl-1))
      rangei = (i+sum_repl+1):(i+sum_repl+repl-1)
      membershipMat[rangei,ipathways] = 1
      row_names[rangei] = compounds[i]
      sum_repl = sum_repl + repl-1;
    }
  }
  rownames(membershipMat) = row_names
  return(membershipMat)
}

#################### GetGraphPathway ####################
GetGraphPathway = function(pathId){
  file = RCurl::getURL(paste("http://rest.kegg.jp/get/", pathId, "/kgml", sep = ""))
  pathkgml = KEGGgraph::parseKGML(file)
  reactions = suppressWarnings(KEGGgraph::KEGGpathway2reactionGraph(pathkgml))
  if(is.null(reactions)) return(NULL)
  inet = igraph::igraph.from.graphNEL(reactions)
  return(inet)
}

#################### GetDistanceMatrix ####################
GetDistanceMat = function(membmat){
  dmat = list()
  inet.list = lapply(colnames(membmat), GetGraphPathway)
  names(inet.list) = colnames(membmat)
  inet.list = inet.list[!sapply(inet.list, is.null)]
  # deal with non unique kegg ids
  metset = strsplit(rownames(membmat), split="|", fixed=T)
  inet.list = lapply(inet.list, function(x) {
    vname = which(!is.element(unique(paste0("cpd:",unlist(metset))), igraph::get.vertex.attribute(x,"name")))
    x = igraph::add.vertices(x, length(vname), attr = list(name=unique(paste0("cpd:",unlist(metset)))[vname]))})
  #get distance mat,
  dmat = lapply(inet.list, igraph::distances, mode="all")
  dmat = lapply(dmat, function(x) x[paste0("cpd:",unlist(metset)),paste0("cpd:",unlist(metset))])
  #get min shortest distance for non unique identifiers
  idx = unlist(sapply(seq_along(metset), function(i,x) {
    if(length(metset[[i]])==1) i else rep(i,length(metset[[i]]))}, x=metset))
  #deal with non unique kegg ids
  dmat_uq = lapply(dmat, function(x) as.matrix(aggregate(x, by=list(id=idx), FUN=min))[,-1, drop=F]) #try mean or other functions as well
  dmat_uq = lapply(dmat_uq, function(x) {rownames(x) = rownames(membmat); x})
  dmat_uq = lapply(dmat_uq, function(x) as.matrix(aggregate(t(x), by=list(id=idx), FUN=min))[,-1, drop=F])
  dmat_uq = lapply(dmat_uq, function(x) {rownames(x) = rownames(membmat); t(x)})
  #remove identical distance matrices
  nodup = which(!duplicated(dmat_uq))
  nn = sapply(nodup, function(i,x) paste(names(dmat_uq)[sapply(x, identical, dmat_uq[[i]]), drop=F],collapse="|") ,x=dmat_uq)
  dmat_uq = dmat_uq[nodup, drop=F]
  #remove distance matrices with elements that are all infinite
  nn = nn[sapply(dmat_uq, function(x) sum(1/x[lower.tri(x)])>0), drop=F]
  dmat_uq = dmat_uq[sapply(dmat_uq, function(x) sum(1/x[lower.tri(x)])>0), drop=F]
  names(dmat_uq) = nn
  return(dmat_uq)
}
