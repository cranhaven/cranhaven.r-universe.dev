 # carrier probability

carrierprobgeno <-function(method="data", data, mode="dominant", q=0.02){
	
## relation code: 1=proband, 2=sib, 3=child, 4=parent, 5=niece/nephew, 6=spouse, 7=brother/sister-in-law (sib's spouse), 8=grand-parent (father-side), 9=uncle/aunt (father-side), 10=cousin (father-side), 11=grand-parent (mother-side), 12=uncle/aunt (mother-side), 13=cousin (mother-side), 14=grand-child, 15=uncle/aunt's spouse

  carrp <- data$mgene
  id.na <- data$indID[is.na(data$mgene)]
  mut.ca <- data$relation[data$mgene==1 & !is.na(data$mgene)]

  majorgene <- data$majorgene
  pAA <- ifelse(is.na(carrp), NA, ifelse(majorgene==1, 1, 0)) # P(1=AA), A is disease gene
  pAa <- ifelse(is.na(carrp), NA, ifelse(majorgene==2, 1, 0)) # P(2=Aa)
  paa <- ifelse(is.na(carrp), NA, ifelse(majorgene==3, 1, 0)) # P(3=aa)
#  cfam.id <- data$famID[data$proband==1 & data$mgene==1]
#  nfam.id <- data$famID[data$proband==1 & data$mgene==0]
#  i.cfam <- is.element(data$famID,cfam.id)
#  i.nfam <- is.element(data$famID,nfam.id)
  
  if(method=="data"){
    for(g in unique(data$relation)){
      for(s in c(0,1)){ #gender
        pAA[is.na(data$mgene) & data$relation == g & data$gender == s] <- mean(data$majorgene[!is.na(data$mgene) & data$relation == g & data$gender == s]==1)
        pAa[is.na(data$mgene) & data$relation == g & data$gender == s] <- mean(data$majorgene[!is.na(data$mgene) & data$relation == g & data$gender == s]==2)
        paa[is.na(data$mgene) & data$relation == g & data$gender == s] <- mean(data$majorgene[!is.na(data$mgene) & data$relation == g & data$gender == s]==3)
      }
    }
    
    if(mode == "dominant") carrp <- pAA+pAa
    else carrp <- pAA
    carrp[is.na(carrp)] <- 0
    data$carrp.geno <- carrp
    data$pAA <- pAA
    data$pAa <- pAa
    data$paa <- paa
    
  }
  else if (method=="mendelian"){
  	
  	if(is.null(q)) {
  	  q <- ifelse(mode=="recessive", sqrt(mean(data$mgene[data$generation==0], na.rm=T)), 1-sqrt(1-mean(data$mgene[data$generation==0], na.rm=T)) )
  	  cat("Estimate allele frequency = ", q, "\n")
  	}
  	else if(q>1 | q<0) stop("The allele frequency (q) should lie between 0 and 1.")

  	  #G1=mutation status of proband
  	  #G2=mutation status of sib
  	  #G3=mutation status of child
  	  #G4=mutation status of parent
  	  #G5=mutation status of sib's child
  	  
  	  #G8=mutation status of grand-parent
  	  #G9=mutation status of uncle/aunt 


      # P1.s1=P(1S0=(AA,Aa,aa) | 0S0=1) = P(parent/offsprint=(AA,Aa,aa) |proband is carrier); 
      # P1.s0=P(1S0=(AA,Aa,aa) | 0S0=0)
      P1.s1 <- c(q/(2-q), (1-q^2)/(2-q), (1-q)^2/(2-q))
      P1.s0 <- c(0, q, 1-q)
      
      # P(0F0=(AA,Aa,aa) | 0S0=1) = P(sib =(AA,Aa,aa) | proband is carrier)
      # P(0F0=(AA,Aa,aa) | 0S0=0)
      P2.s1 <- c((3+2*q-q^2)*q/4/(2-q), (2+3*q-q^2)*(1-q)/2/(2-q), (1-q)^2*(4-q)/4/(2-q) )
      P2.s0 <- c(q^2/4, q*(2-q)/2, (2-q)^2/4 )
      
      # P(2S0=(AA,Aa,aa)|0S0=1) = P(grandparent/grandchild =(AA,Aa,aa) | proband is carrier)
      # this prob is same for niece/nephew, aunt/uncle
      P3.s1 <- c((1+2*q-q^2)*q/2/(2-q), (1+5*q-2*q^2)*(1-q)/2/(2-q), (1-q)^2*(3-q)/2/(2-q) )
      P3.s0 <- c(q^2/2, q*(3-2*q)/2, (1-q)*(2-q)/2 )
      
      GP.AA <- c(q^2+(1-q)*q/2, q^2/2+q/4, q^2/2)             #P(Grandparent/grandchild=AA|0S0=(AA,Aa,aa))
      GP.Aa <- c(q*(1-q)+(1-q)/2, (1-q)*q+1/4, q/2 + q*(1-q)) #P(Grandparent/grandchild=Aa|0S0=(AA,Aa,aa))
      GP.aa <- c((1-q)^2/2, (3-2*q)*(1-q)/4, (1-q)*(2-q)/2)   #P(Grandparent/grandchild=aa|0S0=(AA,Aa,aa))
      
      P4.s1 <- c(sum(GP.AA*P1.s1), sum(GP.Aa*P1.s1), sum(GP.aa*P1.s1)) # P(cousin=(AA,Aa,aa) | 0S0=1)
      P4.s0 <- c(sum(GP.AA*P1.s0), sum(GP.Aa*P1.s0), sum(GP.aa*P1.s0)) # P(cousin=(AA,Aa,aa) | 0S0=0)

P0 <- c(q^2, 2*q*(1-q), (1-q)^2) 

n <- ifelse(mode=="recessive", 1,2)

p1 <- sum(P1.s1[1:n]) # parent, child
p2 <- sum(P2.s1[1:n]) # sib
p3 <- sum(P3.s1[1:n]) # uncle,aunt, niece, nephew, grandparent, grandchild all have same carrier prob
p4 <- sum(P4.s1[1:n]) # cousin
p0 <- sum(P0[1:n]) # founders (spouses)
# pp is P(f=1|m=1,p=1) = prob of a parent being carrier when proband and the other parent are both carriers
pp <- c(q, 3*q*(1-q^2)/(1+q-q^2))[n] #recessive, dominant 

p1.0 <- sum(P1.s0[1:n])
p2.0 <- sum(P2.s0[1:n])
p3.0 <- sum(P3.s0[1:n])
p4.0 <- sum(P4.s0[1:n])

#  mode=="dominant"
#      p1 <- (1+q-q^2)/(2-q) #p1=sum(P1.s1[1:2]) # parent
#      p2 <- (4+5*q-6*q^2+q^3)/(8-4*q) #p2=sum(P2.s1[1:2]) # sib
#      p3 <- (1+5*q-5*q^2+q^3)/2/(2-q) #p4=sum(P3.s1[1:2]) # uncle,aunt, niece, nephew, grandparent, grandchild all have same carrier prob
#    (i.rel==5 | i.rel=="1F0" | i.rel == "2S0" | i.rel == "0S2")]  <- p3 #gparent/gchild,uncle/aunt,niece/nephew

#      p4 <- sum(P4.s1[1:2]) # cousin (1F1)   	    	

	fid <- data$fatherID
	mid <- data$motherID
	iid <- data$indID
#print(id.na)
      for(i in id.na){
        p.rel <- data$relation[iid==i] # relation to proband
        i.fam <- data$famID[iid==i]
        fam <- data[data$famID==i.fam, c("indID","fatherID","motherID","gender","mgene")]
        fam[is.na(fam)] <- 0
		    i.rel <- MakeRelationshipExtended(fam, i)
		
		pid <- iid[data$famID==i.fam & data$proband==1] # proband's id
		
		gp <- data$mgene[iid==pid] #proband's genotype
		g1 <- sum(fam$mgene[is.element(i.rel, c(3,4))], na.rm=TRUE) 
		gs <- sum(fam$mgene[is.element(i.rel, c(2))], na.rm=TRUE)
		g2 <- sum(fam$mgene[is.element(i.rel, c(5,8,9,11,12,15))], na.rm=TRUE)
		g3 <- sum(fam$mgene[is.element(i.rel, c(10,13))], na.rm=TRUE)
		g6 <- sum(fam$mgene[is.element(i.rel, c(6))], na.rm=TRUE) # spouse
		
		if(gp == 1) { # proband is carrier
		  if(g1 > 0 & g6 == 1 ) p <- pp
			else if(g1 > 0 & g6 == 0) p <- p1      # parent or kid is a carrier
			else if(gs > 0) p <- p2 # sib is carrier
			else if(g2 > 0) p <- p3 # 2nd degree relatives are carriers
			else if(g3 > 0) p <- p4 # 3rd degree relatives (cousin) are carriers 
			else p <- p0 # founders (spouses) are carriers
			carrp[data$indID==i] <- p	
		}
		else { # proband is non-carrier
			if( g1 > 0) p <- p1 # parent or kid is a carrier
          	else if(gs > 0) p <- p2
          	else if(g2 > 0) p <- p3
          	else if(g3 > 0) p <- p4
          	else if( is.element(p.rel, c(3,4)) ) p <- p1.0
          	else if( is.element(p.rel, 2) ) p <- p2.0
          	else if( is.element(p.rel, c(5,8,9,11,12,14)) ) p <- p3.0
          	else if( is.element(p.rel, c(10, 13)) ) p <- p4.0
          	else if( is.element(p.rel, c(6,7,15)) ) p <- p0
        	carrp[data$indID==i] <- p

		}

	}#close for for(i in id.na)
  
	carrp[is.na(carrp)] <- 0
	data$carrp.geno <- carrp
#	data$pAA <- pAA
#	data$pAa <- pAa
#	data$paa <- paa
	
	}# close for else if(method=="mendelian")
  else stop("Unrecognized method")

return(data)
}