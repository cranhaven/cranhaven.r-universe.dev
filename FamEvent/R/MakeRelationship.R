MakeRelationship <- function(ped, counseland.id){
	## Feb, 2019
	## 1=proband, 2=sib, 3=child, 4=parent, 5=niece/nephew, 6=spouse, 7=brother/sister-in-law (sib's spouse), 8=grand-parent (father-side), 9=uncle/aunt (father-side), 10=cousin (father-side), 11=grand-parent (mother-side), 12=uncle/aunt (mother-side), 13=cousin (mother-side), 14=son/daughter-in-law, 15=grand-child, 16=uncle/aunt's spouse
  iid <- ped[,"indID"]
  fid <- ped[,"fatherID"]
  mid <- ped[,"motherID"]
  gender <- ped[,"gender"]

  relationship <- rep(0,nrow(ped))
  
  current.id <- counseland.id
  indiv <- iid == current.id
  relationship[indiv] <- 1
  
  #identify parents (4), brothers or sisters (2), nephews or nieces (5)
  sibs <- NULL
  nep <- NULL
  spouse.id <- NULL
  potent.spouse.id <- NULL

# print(counseland.id)

  if (sum(indiv)!=0){
    if (fid[indiv] !=0 ){
      relationship[iid == fid[indiv]] <- 4
      sibs <- c(sibs, which(fid == fid[indiv] & !indiv))
    }
    if (mid[indiv]!=0 ){
      relationship[iid == mid[indiv]] <- 4
      sibs <- c(sibs, which(mid == mid[indiv] & !indiv))
    }
    if (length(unique(sibs))!=0){
      relationship[unique(sibs)] <- 2
      for (i in 1:length(unique(sibs))){
        nep <- c(nep,which(fid==iid[unique(sibs)[i]]|mid==iid[unique(sibs)[i]]))
      }
      if (sum(nep)!=0)
      relationship[unique(nep)] <- 5
    }  
  }
  
#  print("a")

  #identify proband's spouse (6), son or daughter (3)
  if(sum(indiv)!=0){
    if (gender[indiv]==0){
      relationship[mid==iid[indiv]] <- 3
      child <- which(mid==iid[indiv])
      if (sum(child)!=0){
        spouse.id <- unique(fid[child])
        if (sum(spouse.id)!=0){
          for (i in 1:length(spouse.id)){
             relationship[iid==spouse.id[i]] <- 6
           }
        }
      } 
    }else {
      relationship[fid==iid[indiv]] <- 3
      child <- which(fid==iid[indiv])
      if (sum(child)!=0){
        spouse.id <- unique(mid[child])
        if (sum(spouse.id)!=0){
          for ( i in 1:length(spouse.id)){
            relationship[iid==spouse.id[i]] <- 6
          }
        }
      } 
    }
  }
     
 # grand-child (15)    
 gcid <- is.element(fid, iid[relationship==3]) | is.element(mid, iid[relationship==3]) # grand-child (15)
 relationship[gcid] <- 15 

 # son/daughter-in-law (14) 
  if (sum(gcid)!=0) { 
    child.spouse.id <- unique(c(fid[gcid],mid[gcid]))
    # print( uncle.spouse.id)
    if (sum(child.spouse.id)!=0){
    for ( i in 1:length(child.spouse.id)){
      indiv <- iid==child.spouse.id[i]
      if (sum(indiv)!=0){
        if (relationship[indiv]==0) relationship[indiv] <- 14
        }
      }
     }
    }
  

  current.fid <- fid[indiv]
  current.mid <- mid[indiv]
  
  #Paternal side identify grandparents(8),aunts or uncles (9), cousin (10), aunt or uncle's spouse (16)
  indiv <- iid==current.fid
  
  if(sum(indiv, na.rm=TRUE)!=0) {
    if (fid[indiv]!=0){
      relationship[iid==fid[indiv]] <- 8
      relationship[fid==fid[indiv] & !indiv] <- 9  
    }
    if (mid[indiv]!=0){
      relationship[iid==mid[indiv]] <- 8
      relationship[mid==mid[indiv] & !indiv] <- 9
    }
    
    cid <- is.element(fid, iid[relationship==9]) | is.element(mid, iid[relationship==9]) # cousin 
    relationship[cid] <- 10 
    
  }
  
  #Maternal side identify grandparents(11), aunts or uncles (12), cousin (13), aunt or uncle's spouse (16)
  indiv <- iid==current.mid
  
  if(sum(indiv, na.rm=TRUE)!=0){
    if (fid[indiv] != 0){
      relationship[iid==fid[indiv]] <- 11
      relationship[fid==fid[indiv] & !indiv] <- 12 
    }
    if (mid[indiv] !=0){
      relationship[iid==mid[indiv]] <- 11
      relationship[mid==mid[indiv] & !indiv] <- 12
    }
     cid <- is.element(fid, iid[relationship==12]) | is.element(mid, iid[relationship==12])   
     relationship[cid] <- 13 
     

  }

#  print("d")
#  Identify spouse, brother or sister in law (15->7)
#  print(spouse.id)
#  if (sum(spouse.id)!=0){
#  		spouse.id <- spouse.id[spouse.id > 0]
#      for (i in 1:length(spouse.id)){
#        indiv <- iid == spouse.id[i]
#        if ( length(fid[indiv])!=0 ){
#          relationship[fid==fid[indiv] & !indiv] <- 7
#        }
#        if ( length(mid[indiv])!=0 ){
#          relationship[mid==mid[indiv] & !indiv] <- 7
#      	}
#      }
#  }
 
  #Identify bother or sister's spouse (7)
  if (sum(nep)!=0)
  potent.spouse.id <- unique(c(fid[nep],mid[nep]))
  if (sum(potent.spouse.id)!=0){
    for ( i in 1:length(potent.spouse.id)){
      indiv <- iid==potent.spouse.id[i]
      if (sum(indiv)!=0){
        if (relationship[indiv]==0){
          relationship[indiv] <- 7
        }
      }
    }
  }

  #Identify uncle or aunt's spouse (16)
  cousin <-  which(relationship==10 | relationship == 13)
 
  if (sum(cousin)!=0) { 
    uncle.spouse.id <- unique(c(fid[cousin],mid[cousin]))
    # print( uncle.spouse.id)
    if (sum(uncle.spouse.id)!=0){
    for ( i in 1:length(uncle.spouse.id)){
      indiv <- iid==uncle.spouse.id[i]
      if (sum(indiv)!=0){
        if (relationship[indiv]==0){
          relationship[indiv] <- 16
        }
      }
    }
  }
  }
    
  return(relationship)
}
  
