
clusters.detection = 
function (smiles, k = 50, seed = 12345, max_nc = 30, 
          dissimilarity.parameters = list(), kodama.matrix.parameters = list(), 
          kodama.visualization.parameters = list(), hclust.parameters = list(method = "ward.D"), 
          verbose = TRUE) 
{
  min_nc = 2
  if (length(smiles) <= 25) {
    stop("The number of SMILEs must be higher than 25")
  }
  if (max_nc > (length(smiles)/2)) {
    stop("The maximum number of cluster is too high")
  }
  k_data = length(smiles) - 1
  if (k > k_data) {
    k = k_data
    warnings("Since k exceed the number of entries, the number of components will be reduced to", 
             k_data)
  }
  dissimilarity.parameters$smiles = smiles
  if (verbose) {
    print("Generation of chemical structural dissimilarity matrix.")
  }
  di = do.call("chemical.dissimilarity", dissimilarity.parameters)
  cc = cmdscale(as.dist(di), k = k)
  kodama.matrix.parameters$data = cc
  kodama.matrix.parameters$landmarks = 20000
  kodama.matrix.parameters$seed= seed
  kodama.matrix.parameters$FUN="simpls"
  res.kodama.matrix = do.call("KODAMA.matrix", kodama.matrix.parameters)
  
  kodama.visualization.parameters$kk = res.kodama.matrix
  res.kodama.visualization = do.call("KODAMA.visualization", 
                                      kodama.visualization.parameters)
  
  hclust.parameters$d = dist(res.kodama.visualization)
  res.hclust = do.call("hclust", hclust.parameters)
  res = list(kodama = list(visualization = res.kodama.visualization), 
             hclust = res.hclust)
  clusters = tree.cutting(res, max_nc = max_nc)$clusters

  main_cluster = which.max(clusters) + 1

          
  rownames(res.kodama.visualization)=names(smiles)
  return(list(visualization = res.kodama.visualization, clusters = clusters, 
              silhouette = clusters,
              max_nc = max_nc, min_nc = min_nc, hclust = res.hclust, 
              main_cluster = main_cluster)) 
}
  

  

allbranches = function(hh,minlen=5){
  nr=length(hh$order)
  cl=list()
  h=0
  for(i in 2:floor(nr/3)){
    cc=cutree(hh,i)
    for(j in 1:i){
      nn=names(which(cc==j))  
      if(length(nn)>=minlen){
        h=h+1
        cl[[h]]=nn
        registered=0
        for(k in 1:h){
          if(length(cl[[k]])==length(nn)){
            if(all(cl[[k]]==nn)){
              registered=registered+1
            }
          }
        }
        if(registered>1){
          cl[[h]]=NULL
          h=h-1
          
        }
      }
    }
  }
  cl
}


  
  
  
  
  
  



chemical.dissimilarity = function(smiles,method="tanimoto",type="extended"){
  mol <- parse.smiles(smiles)
  fps <- lapply(mol, get.fingerprint, type=type)
  ls=length(smiles)
  
  ma=1-fp.sim.matrix(fps,method=method)
  
  return(ma)
}


KODAMA.chem.sim = function (smiles,
                            d=NULL,
                            k=50,
                            dissimilarity.parameters=list(),
                            kodama.matrix.parameters=list(),
                            kodama.visualization.parameters=list(),
                            hclust.parameters=list(method="ward.D")){
  if(length(smiles)<=25){
    stop("The number of SMILEs must be higher than 25")
  }
  if(is.null(d)){
    k_data=length(smiles)-1
  }else{
    k_data=nrow(d)-1
  }
  if(k>k_data){
    k=k_data
    warnings("Since k exceed the number of entries, the number of components will be reduced to",k_data)
  }
  
  if(is.null(d)){
    dissimilarity.parameters$smiles=smiles
  }else{
    if (anyNA(d)) 
      stop("NA values not allowed in 'd'")
    if ((n <- nrow(d)) != ncol(d)) {
      stop("distances must be result of 'dist' or a square matrix")
    }
  }
  di=do.call("chemical.dissimilarity",dissimilarity.parameters)

  cc=cmdscale(as.dist(di),k = k)
  rownames(cc)=names(smiles)
  kodama.matrix.parameters$data=cc
  res.kodama.matrix=do.call("KODAMA.matrix",kodama.matrix.parameters)
  kodama.visualization.parameters$kk=res.kodama.matrix
  res.kodama.visualization=do.call("KODAMA.visualization",kodama.visualization.parameters)
  hclust.parameters$d=dist(res.kodama.visualization)
  res.hclust=do.call("hclust",hclust.parameters)
  
  return(list(kodama=list(matrix=res.kodama.matrix,visualization=res.kodama.visualization),hclust=res.hclust))
}



tree.cutting = function(res,max_nc=20){
  min_nc=2
  if(max_nc<=min_nc){
    stop("The maximum cluster number is equal or less the minimum cluster number.")
  }  
  if(min_nc<2){
    stop("The minimun cluster number cannot be lower than 2.")
  }
  nn=max_nc-min_nc+1
  name_clu=paste("Clusters",min_nc:max_nc)
  res.S=rep(NA,nn)
  names(res.S)=name_clu
  
  dd=as.matrix(dist(res$kodama$visualization))
  clusters <- NULL
  

  for (nc in min_nc:max_nc)
  {
    cl <- cutree(res$hclust, k=nc)
    clusters=cbind(clusters,cl)
    Si <- 0
    for (k in 1:max(cl)) {
      Sil = 0
      if ((sum(cl == k)) > 1)  {
        for (i in 1:length(cl)) {
          if (cl[i] == k) {
            ai <- sum(dd[i, cl == k])/(sum(cl == k) - 1)
            dips <- NULL
            for (j in 1:max(cl)) {
              if (cl[i] != j) {
                if (sum(cl == j) != 1) {
                  dips <- cbind(dips, c((sum(dd[i, cl == j]))/(sum(cl == j))))
                }
                else{
                  dips <- cbind(dips, c((sum(dd[i, cl == j]))))
                }
              }
            }
            bi <- min(dips)
            Sil <- Sil + (bi - ai)/max(c(ai, bi))
          }
        }
      }
      Si <- Si + Sil
    }
    res.S[nc-1]=Si/length(cl)
  }  
 
  colnames(clusters)=name_clu
  return(list(clusters=clusters, 
              res.S=res.S,
              max_nc=max_nc,
              min_nc=min_nc))
}


# Weighted Metabolite Chemical Structural Analysis 

WMCSA =
  function (data, cl) 
  {
    
    eigenmetab <- as.data.frame(seq(1:ncol(data)))
    for (i in 1:length(cl)) {
      cluster <- data[cl[[i]], ]
      cluster=scale(t(cluster))
      cm=rowMeans(cluster)
      temp <- prcomp(cluster)
      temp = temp$x[, 1]
      temp = temp*sign(cor(temp,cm))
      eigenmetab <- cbind(eigenmetab, temp)
    }
    eigenmetab <- eigenmetab[, -1]
    colnames(eigenmetab) <- paste0("Module", 1:length(cl))
    eigenmetab = t(eigenmetab)
    return(eigenmetab)
  }


readMet = function (ID, address = c("http://www.hmdb.ca/metabolites/"),remove=TRUE) 
{
  ww=names(which(table(ID)>1))
  if(length(ww)>0){
    text="Your ID list contains duplicates:"
    text=paste(text,paste(ww))
    stop(text)
  }
  doc=list()
  n = length(ID)
  doc$n = n
  doc$HMDB = ID
  doc$ID = 1:n
  pb <- txtProgressBar(min = 1, max = n, style = 1)
  doc$error.HMDB=NULL
  doc$old.HMDB=NULL
  doc$empty.HMDB=NULL
  doc$remove=rep(FALSE,n)
  httr::set_config(config(ssl_verifypeer = 0L))
  for (i in 1:n) {
    setTxtProgressBar(pb, i)
    doc$met[[i]]=list()
    error=0
    if(is.na(ID[i]) | ID[i]==""){
      error=3
      doc$met[[i]]$description = "Empty"  
      doc$met[[i]]$exist = FALSE
      doc$empty.HMDB=c(doc$empty.HMDB,ID[i])
      doc$remove[i]=TRUE
    }else{
    
    file = paste(address, ID[i], ".xml", sep = "")

    res <- try(httr::RETRY("GET",
                           file,
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if(res$status_code==404){
      error=1
      doc$met[[i]]$description = "Not found"  
      doc$met[[i]]$exist = FALSE
      doc$error.HMDB=c(doc$error.HMDB,ID[i])
      doc$remove[i]=TRUE
    }else{
      raw=rawToChar(res$content)
      xml=try(xmlParse(raw,error=NULL), silent = TRUE)
      
      
      if(attr(xml,"class")[1]=="try-error"){
        error=2
        doc$met[[i]]$description = "Old HMDB access ID"  
        doc$met[[i]]$exist = FALSE
        doc$old.HMDB=c(doc$old.HMDB,ID[i])
        doc$remove[i]=TRUE
      }else{
        doc$met[[i]] = xmlToList(xml)
        doc$met[[i]]$exist = TRUE
      }
    }
    }
  }
    

  
  close(pb)
  if(remove==TRUE){
    sel=which(doc$remove)
    l=length(sel)
    if(l!=0){
      doc$met=doc$met[-sel]	
      doc$ID=doc$ID[-sel]
      doc$HMDB=doc$HMDB[-sel]
      doc$n=doc$n-l
    }    
  }

  names(doc$remove)=names(ID)
  doc
}




selectionMet = function(doc,sel){
  sel=sel[sel!=""]
  sel=unique(sel)
  
  sel=match(sel,doc$HMDB)
  sel=sel[!is.na(sel)]
  l=length(sel)	
  
  doc$met=doc$met[sel]	
  doc$ID=doc$ID[sel]
  doc$HMDB=doc$HMDB[sel]
  doc$n=l
  doc
}



nameMet = function(doc){
  m = as.data.frame(matrix(ncol=1,nrow=doc$n))
  rownames(m)=doc$ID
  colnames(m)="Name"
  for(i in 1:doc$n)
    m[i,1]=doc$met[[i]]$name
  return(m)
}



MWMet = function(doc){
  m = as.data.frame(matrix(ncol=1,nrow=doc$n))
  rownames(m)=doc$ID
  colnames(m)="Molecular Weight"
  for(i in 1:doc$n){
    m[i,1]=doc$met[[i]]$average_molecular_weight
  }
  return(m)
}


substituentsMet = function(doc){
  vi=list()
  for(i in 1:doc$n)
    if(!is.null(doc$met[[1]]$taxonomy))
      if((doc$met[[i]]$taxonomy!="\n  ")[1])
        vi[[i]]=doc$met[[i]]$taxonomy$substituents
  u=unique(unlist(vi))
  lu=length(u)
  m=as.data.frame(matrix(nrow=doc$n,ncol=lu,FALSE))
  rownames(m)=as.vector(doc$HMDB)
  colnames(m)=u
  for(i in 1:doc$n)
    if(!is.null(unique(unlist(vi[[i]]))))
      if (!is.na(vi[[i]][1]))
        m[i,unique(unlist(vi[[i]]))]=TRUE
  m
}



propertiesMet = function(doc){
  m = as.data.frame(matrix(ncol=21,nrow=doc$n,NA))
  rownames(m)=doc$ID
  colnames(m)=c("logp_ALOGPS",
                "logs_ALOGPS",
                "solubility_ALOGPS",
                "logp_ChemAxon",
                "pka_strongest_acidic_ChemAxon",
                "pka_strongest_basic_ChemAxon",
                "iupac_ChemAxon",
                "average_mass_ChemAxon",
                "mono_mass_ChemAxon",
                "smiles_ChemAxon",
                "formula_ChemAxon",
                "inchi_ChemAxon",
                "inchikey_ChemAxon",
                "polar_surface_area_ChemAxon",
                "refractivity_ChemAxon",
                "polarizability_ChemAxon",
                "rotatable_bond_count_ChemAxon",
                "acceptor_count_ChemAxon",
                "donor_count_ChemAxon",
                "physiological_charge_ChemAxon",
                "formal_charge_ChemAxon")
  
    for(i in 1:doc$n){
    if(is.na(pmatch("\n",doc$met[[i]]$predicted_properties))){
      for(j in 1:length(doc$met[[i]]$predicted_properties))
        m[i,paste(doc$met[[i]]$predicted_properties[[j]]$kind,doc$met[[i]]$predicted_properties[[j]]$source,sep="_")]=(doc$met[[i]]$predicted_properties[[j]]$value)
      
    }else{
      m[i,]=rep(NA,21)
    }
  }
  rownames(m)=as.vector(doc$HMDB)
  return(m)
}




taxonomyMet = 
  function (doc) 
  {
    m = as.data.frame(matrix(ncol = 5, nrow = doc$n))
    rownames(m) = as.vector(doc$HMDB)
    colnames(m) = c("Kingdom", "Super Class", "Class","Sub-class", "Direct Parent")
    for (i in 1:doc$n) {
      if (!is.null(doc$met[[i]]$taxonomy$kingdom)) 
        m[i, 1] = doc$met[[i]]$taxonomy$kingdom
      if (!is.null(doc$met[[i]]$taxonomy$super_class)) 
        m[i, 2] = doc$met[[i]]$taxonomy$super_class
      if (!is.null(doc$met[[i]]$taxonomy$class)) 
        m[i, 3] = doc$met[[i]]$taxonomy$class
      if (!is.null(doc$met[[i]]$taxonomy$sub_class)) 
        m[i, 4] = doc$met[[i]]$taxonomy$sub_class
      if (!is.null(doc$met[[i]]$taxonomy$direct_parent)) 
        m[i, 5] = doc$met[[i]]$taxonomy$direct_parent
    }
    return(m)
  }




enzymesMet = function(doc){
  vi=list()
  for(i in 1:doc$n){
    if(is.na(pmatch("\n",doc$met[[i]]$protein_associations))){
      v=NULL
      for(j in 1:length(doc$met[[i]]$protein_associations)){
        v=c(v,doc$met[[i]]$protein_associations[j]$protein$gene_name)
      }
      vi[[i]]=v	
    }else{
      vi[[i]]=NA
    }
  }
  u=unique(unlist(vi))
  u=u[!is.na(u)]
  lu=length(u)
  m=as.data.frame(matrix(nrow=doc$n,ncol=lu,FALSE))
  rownames(m)=as.vector(doc$HMDB)
  colnames(m)=u
  for(i in 1:doc$n)
    if(!is.na(vi[[i]][1]))
      m[i,unique(unlist(vi[[i]]))]=TRUE
  m
}



pathwaysMet = function(doc){
  vi=list()
  for(i in 1:doc$n){
    if(is.na(pmatch("\n",doc$met[[i]]$biological_properties$pathways))){
      v=NULL
      for(j in 1:length(doc$met[[i]]$biological_properties$pathways)){
        v=c(v,doc$met[[i]]$biological_properties$pathways[j]$pathway$name)
      }
      vi[[i]]=v	
    }else{
      vi[[i]]="Not Available"
    }
  }
  
  u=unique(unlist(vi))
  lu=length(u)
  m=as.data.frame(matrix(nrow=doc$n,ncol=lu,FALSE))
  rownames(m)=as.vector(doc$HMDB)
  colnames(m)=u
  for(i in 1:doc$n)
    m[i,unique(unlist(vi[[i]]))]=TRUE
  m
}



diseasesMet = function(doc){
   vi=list()
   for(i in 1:doc$n){
      if(is.na(pmatch("\n",doc$met[[i]]$diseases))){
         v=NULL
         for(j in 1:length(doc$met[[i]]$diseases)){
            v=c(v,doc$met[[i]]$diseases[[j]]$name)
            }
         vi[[i]]=v	
      }else{
        vi[[i]]=NA
      }
   }

   u=unique(unlist(vi))
   u=u[!is.na(u)]
   lu=length(u)
   m=as.data.frame(matrix(nrow=doc$n,ncol=lu,FALSE))
   rownames(m)=as.vector(doc$HMDB)
   colnames(m)=u
   for(i in 1:doc$n)
    if(!is.na(vi[[i]][1]))
      m[i,unique(unlist(vi[[i]]))]=TRUE
   m
}



write.gmt = function(sub,address,min_entry=2,max_entry=50){
  cona=colnames(sub)
  rona=rownames(sub)
  text_lines=NULL
  for(i in 1:ncol(sub)){
    sel=sub[,i]
    sumsel=sum(sel)
    if(sumsel>min_entry & sumsel<max_entry)
      text_lines[i]=paste(cona[i],"\tNA\t",paste(rona[sel],collapse = "\t"),sep="")
  }
  text_lines=text_lines[!is.na(text_lines)]
  writeLines(text_lines, address) 
}


write.gct <- function(es, address) {
  con=file(address)
  open(con, open="w")
  writeLines("#1.2", con)
  ann.col <- ncol(es)
  ann.row <- nrow(es)
  writeLines(sprintf("%s\t%s", ann.row, ann.col), con)
  writeLines(paste0(c("NAME", "Description", colnames(es)), collapse="\t"), con)
  ann.col.table <- cbind(rownames(es),rep(NA,ann.row),es)
  write.table(ann.col.table, file=con, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
  close(con)
}

write.cls <- function(es, address) {
  con=file(address)
  open(con, open="w")
  writeLines(paste0(c(length(es),2,1), collapse="\t"), con)
  writeLines(paste0("# ",paste0(unique(es), collapse="\t"), collapse=""), con)
  writeLines(paste0(es, collapse="\t"), con)
  close(con)
}


features =
  function (doc, cla, cl,HMDB_ID) 
  {
    lc = length(cl)
    nam=names(which(doc$remove))
    for(i in 1:lc){
      m=match(cl[[i]],nam)
      cl[[i]]=HMDB_ID[cl[[i]][is.na(m)]]
    }
    
    res = list()
    for (i in 1:lc) {
      sel = !is.na(match(rownames(cla),cl[[i]]))
      if (sum(sel) >= 3) {
        res[[i]] = sort(apply(cla, 2, function(x) fisher.test(table(sel, 
                                                                    x), alternative = "greater")$p.value))
        res[[i]] = res[[i]][res[[i]] < 0.05]
      }
      else {
        res[[i]] = "Insufficient number of metabolites"
      }
    }
    res
  }





