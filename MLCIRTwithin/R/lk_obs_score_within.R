lk_obs_score_within <- function(part_comp,lde1,lde2,lpart,lga1t,lga2t,S,R,yv,k1,k2,rm1,rm2,lv,J,fv,disc,
	glob,refitem,miss,ltype,XX1dis,XX2dis,Xlabel,ZZ0,fort,Zpar,zpar,Zga1,zga1,Zga2,zga2,items1,items2){

# preliminaries
	lm = max(lv)
	ncov1 = dim(XX1dis)[2]
	ncov2 = dim(XX2dis)[2]
	ns = length(Xlabel)
	cov = TRUE
	if(glob) logit_cov = "g" else logit_cov = "m"
	Aggr1 = diag(k1)%x%matrix(1,1,k2)
	Aggr2 = matrix(1,1,k1)%x%diag(k2)	
	rm = rm1*rm2
# separate parameters
	de1 = part_comp[1:lde1]
	de2 = part_comp[lde1+(1:lde2)]
	part = part_comp[lde1+lde2+(1:lpart)]
	par = Zpar%*%part+zpar	
	if(disc){
		ga1t = part_comp[lde1+lde2+lpart+(1:lga1t)]			
		ga2t = part_comp[lde1+lde2+lpart+lga1t+(1:lga2t)]
		ga1 = Zga1%*%ga1t+zga1
		ga2 = Zga2%*%ga2t+zga2
	}	
# Compute log-likelihood
	if(k1>1) Piv1 = prob_multi_glob(XX1dis,logit_cov,de1,Xlabel)$P
	if(k2>1) Piv2 = prob_multi_glob(XX2dis,logit_cov,de2,Xlabel)$P
	if(k1==1) Piv = Piv2
	if(k2==1) Piv = Piv1
	if(k1>1 & k2>1){
		Piv = matrix(0,ns,k1*k2)
		for(i in 1:ns) Piv[i,] = Piv1[i,]%x%Piv2[i,]
	}
	if(disc==0) ZZ = ZZ0
	if(disc==1){
		ga1 = Zga1%*%ga1t+zga1
		ga2 = Zga2%*%ga2t+zga2
		ga1c = rep(1,J); ga1c[items1] = ga1
		ga2c = rep(1,J); ga2c[items2] = ga2
		ZZ = ZZ0
		for(j in 1:J){
			ind = (refitem==j)
			ZZ[,1:(k1*rm1),ind] = ZZ[,1:(k1*rm1),ind]*ga1c[j]
			ZZ[,k1*rm1+1:(k2*rm2),ind] = ZZ[,k1*rm1+1:(k2*rm2),ind]*ga2c[j]
		}
	}
	P = prob_multi_glob_gen(ZZ,ltype,par)$P
	Phi = array(t(P),c(lm,J,k1*k2))
	Psi = matrix(1,ns,k1*k2)
	if(miss) for(j in 1:J) for(c in 1:(k1*k2)) Psi[,c] = Psi[,c]*(Phi[S[,j]+1,j,c]*R[,j]+(1-R[,j]))	
	else for(j in 1:J) for(c in 1:(k1*k2)) Psi[,c] = Psi[,c]*Phi[S[,j]+1,j,c]
	if(k1==1 & k2==1) Pj=Psi else Pj = Psi*Piv
	pm = rowSums(Pj)
	lk = sum(yv*log(pm))
# ---- E-step ----
	V = ((yv/pm)%o%rep(1,k1*k2))*Piv*Psi; sV = colSums(V)
	YY = matrix(NA,J*k1*k2,lm)
	count = 0
	for(c in 1:(k1*k2)) for(j in 1:J){
		count = count+1
		for(y in 1:lv[j]){
			ind = (S[,j]==(y-1))
			if(miss) YY[count,y] = sum(V[ind,c]*R[ind,j]) else YY[count,y] = sum(V[ind,c])			
		}
	}
# ---- M-step ----
	if(disc){
		ZZ1 = ZZ2 = array(NA,c(lm-1,J,J*k1*k2))
		count = 0
		for(c1 in 1:k1) for(c2 in 1:k2) for(j in 1:J){
			count = count+1
			ZZ1[1:(lv[j]-1),,count] = 0
			ZZ1[1:(lv[j]-1),j,count] = ZZ0[1:(lv[j]-1),1:(k1*rm1),count]%*%par[1:(k1*rm1)]
			ZZ2[1:(lv[j]-1),,count] = 0
			ZZ2[1:(lv[j]-1),j,count] = ZZ0[1:(lv[j]-1),k1*rm1+1:(k2*rm2),count]%*%par[k1*rm1+1:(k2*rm2)]
		}
		ind = (k1*rm1+k2*rm2+1):dim(ZZ0)[2]
		ZZ1Int = ZZ2Int = array(NA,c(lm-1,J*k1*k2))
		count = 0
		for(c1 in 1:k1) for(c2 in 1:k2) for(j in 1:J){
			count = count+1
			ZZ1Int[1:(lv[j]-1),count] = ga2c[j]*ZZ0[1:(lv[j]-1),k1*rm1+1:(k2*rm2),count]%*%par[k1*rm1+1:(k2*rm2)]+ZZ0[1:(lv[j]-1),ind,count]%*%par[ind]
			ZZ2Int[1:(lv[j]-1),count] = ga1c[j]*ZZ0[1:(lv[j]-1),1:(k1*rm1),count]%*%par[1:(k1*rm1)]+ZZ0[1:(lv[j]-1),ind,count]%*%par[ind]
		}
		ZZ1 = ZZ1[,items1,]; ZZ2 = ZZ2[,items2,]
		sc_ga1t = est_multi_glob_genZ(YY,ZZ1,ltype,de=ga1t,Int=ZZ1Int,only_sc=TRUE,Z=Zga1,z=zga1)$scde
		sc_ga2t = est_multi_glob_genZ(YY,ZZ2,ltype,de=ga2t,Int=ZZ2Int,only_sc=TRUE,Z=Zga2,z=zga2)$scde
		ZZ = ZZ0
		for(j in 1:J){
			ind = (refitem==j)
			ZZ[,1:(k1*rm1),ind] = ZZ[,1:(k1*rm1),ind]*ga1c[j]
			ZZ[,k1*rm1+1:(k2*rm2),ind] = ZZ[,k1*rm1+1:(k2*rm2),ind]*ga2c[j]
		}
	}
	else sc_ga1t = sc_ga2t = NULL
	sc_part = est_multi_glob_genZ(YY,ZZ,ltype,de=part,only_sc=TRUE,Z=Zpar,z=zpar)$scde
# Update piv
	if(k1==1) sc_de1 = NULL
	else sc_de1 = est_multi_glob(V%*%t(Aggr1),XX1dis,logit_cov,Xlabel,de1,only_sc=TRUE)$sc
	if(k2==1) sc_de2 = NULL
	else sc_de2 = est_multi_glob(V%*%t(Aggr2),XX2dis,logit_cov,Xlabel,de2,only_sc=TRUE)$sc
# output
	sc = c(sc_de1,sc_de2,sc_part,sc_ga1t,sc_ga2t)
	out = list(lk=lk,sc=sc)
	return(out)
	
}