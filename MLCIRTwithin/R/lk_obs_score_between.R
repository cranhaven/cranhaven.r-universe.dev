lk_obs_score_between <- function(part_comp,lde,lpart,lgat,S,R,yv,k,rm,lv,J,fv,disc,
	glob,refitem,miss,ltype,XXdis,Xlabel,ZZ0,fort,Zpar,zpar,Zga,zga,items){

# preliminaries
	lm = max(lv)
	ncov = dim(XXdis)[2]
	ns = length(Xlabel)
	cov = TRUE
	if(glob) logit_cov = "g" else logit_cov = "m"
# separate parameters
	de = part_comp[1:lde]
	part = part_comp[lde+(1:lpart)]
	par = Zpar%*%part+zpar	
	if(disc==1) gat = part_comp[lde+lpart+(1:lgat)]
# Compute log-likelihood
	if(k>1) Piv = prob_multi_glob(XXdis,logit_cov,de,Xlabel)$P
	if(disc==0) ZZ = ZZ0
	if(disc==1){
		ga = Zga%*%gat+zga
		gac = rep(1,J); gac[items] = ga
		ZZ = ZZ0
		for(j in 1:J){
			ind = (refitem==j)
   			ZZ[,1:(k*rm),ind] = ZZ[,1:(k*rm),ind]*gac[j]
		}
	}
	P = prob_multi_glob_gen(ZZ,ltype,par)$P
	Phi = array(t(P),c(lm,J,k))
	Psi = matrix(1,ns,k)
	if(miss){
		for(j in 1:J) for(c in 1:k) Psi[,c] = Psi[,c]*(Phi[S[,j]+1,j,c]*R[,j]+(1-R[,j]))	
	}else{
		# if(fort){
			# o = .Fortran("lk_obs",J,as.integer(k1*k2),as.integer(ns),as.integer(S),as.integer(l),Phi,Psi=Psi)
           # Psi = o$Psi
		# }else{
	    for(j in 1:J) for(c in 1:k) Psi[,c] = Psi[,c]*Phi[S[,j]+1,j,c]
   		# }            	
	}
	if(k==1) Pj=Psi else Pj = Psi*Piv
	pm = rowSums(Pj)
	lk = sum(yv*log(pm))
#	print(lk)
# ---- E-step ----
	V = ((yv/pm)%o%rep(1,k))*Piv*Psi; sV = colSums(V)
	YY = matrix(NA,J*k,lm)
	count = 0
	for(c in 1:k) for(j in 1:J){
		count = count+1
		for(y in 1:lv[j]){
			ind = (S[,j]==(y-1))
			if(miss) YY[count,y] = sum(V[ind,c]*R[ind,j]) else YY[count,y] = sum(V[ind,c])			
		}
	}
# ---- M-step ----
	if(disc==0){
		sc_gat = NULL
	}else{
		if(rm<J){
			ZZ = array(NA,c(lm-1,J,J*k))
			count = 0
			for(c in 1:k) for(j in 1:J){
				count = count+1
				ZZ[1:(lv[j]-1),,count] = 0
				ZZ[1:(lv[j]-1),j,count] = ZZ0[1:(lv[j]-1),1:(k*rm),count]%*%par[1:(k*rm)]
			}
			ind = (k*rm+1):dim(ZZ0)[2]
			ZZInt = array(NA,c(lm-1,J*k))
			count = 0
			for(c in 1:k) for(j in 1:J){
				count = count+1
				ZZInt[1:(lv[j]-1),count] = ZZ0[1:(lv[j]-1),ind,count]%*%par[ind]
			}
			ZZ = ZZ[,items,]
			sc_gat = est_multi_glob_genZ(YY,ZZ,ltype,de=gat,Int=ZZInt,only_sc=TRUE,Z=Zga,z=zga)$scde
		}
		ZZ = ZZ0
		for(j in 1:J){
			ind = (refitem==j)
			ZZ[,1:(k*rm),ind] = ZZ[,1:(k*rm),ind]*gac[j]
		}
	}
	sc_part = est_multi_glob_genZ(YY,ZZ,ltype,de=part,only_sc=TRUE,Z=Zpar,z=zpar)$scde
# Update piv
	if(k==1) sc_de = NULL
	else sc_de = est_multi_glob(V,XXdis,logit_cov,Xlabel,de,only_sc=TRUE)$sc
# output
	sc = c(sc_de,sc_part,sc_gat)
	out = list(lk=lk,sc=sc)
	return(out)
	
}