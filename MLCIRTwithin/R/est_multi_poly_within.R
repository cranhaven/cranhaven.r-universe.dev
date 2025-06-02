est_multi_poly_within <- function (S, yv = rep(1, ns), k1, k2, X = NULL, 
	start = c("deterministic","random","external"), link = c("global","local"), 
	disc = FALSE, difl = FALSE, multi1, multi2, Phi = NULL, ga1t = NULL, ga2t = NULL,
	De1 = NULL, De2 = NULL, fort = FALSE, tol = 10^-10, maxitc = 10^4, disp = FALSE, 
	output = FALSE, out_se = FALSE, glob = FALSE, Zth1=NULL, zth1=NULL, Zth2=NULL, zth2=NULL,
	Zbe=NULL, zbe=NULL, Zga1=NULL, zga1=NULL, Zga2=NULL, zga2=NULL){
    	
# check input
	if (k1 == 1 & k2 == 1) stop("--> for LC models use est_multi_poly (package MultiLCIRT)")
	if (k1 == 1 | k2 == 1) stop("--> for k1=1 or k2=1 use est_multi_poly_between")
	if (max(S, na.rm = TRUE) == 1 & difl) {
		warning("with binary data use difl=FALSE\n")
		difl = FALSE
	}
	link = match.arg(link)
	start = match.arg(start)
	if(is.vector(multi1)) multi1 = t(multi1)    
	if(is.vector(multi2)) multi2 = t(multi2)    
	multi1v = as.vector(multi1)
	if(any(multi1v==0)) multi1v = sort(multi1v[-which(multi1v==0)])
	multi2v = as.vector(multi2)
	if(any(multi2v==0)) multi2v = sort(multi2v[-which(multi2v==0)])
	multiv = union(multi1v,multi2v)
	if(length(multi1v)==length(multi2v)) if(all(multi1v==multi2v)) warning("multi1 and multi2 cannot be equal")
	cons1 = multi1[,1]
	cons2 = multi2[,1]
	if(length(intersect(cons1,cons2))>0) warning("There are repetitions in the constrained items")
	if(!is.null(Zth1)){
		if(is.vector(Zth1)) Zth1 = t(Zth1)
		if(is.null(zth1)) zth1 = rep(0,nrow(Zth1))
		constr.th1 = TRUE
	}else constr.th1 = FALSE
	if(!is.null(Zth2)){
		if(is.vector(Zth2)) Zth2 = t(Zth2)
		if(is.null(zth2)) zth2 = rep(0,nrow(Zth2))
		constr.th2 = TRUE
	}else constr.th2 = FALSE
	if(!is.null(Zbe)){
		if(is.vector(Zbe)) Zbe = t(Zbe)
		if(is.null(zbe)) zbe = rep(0,nrow(Zbe))
		constr.be = TRUE
	}else constr.be = FALSE
	if(!is.null(Zga1)){
		if(is.vector(Zga1)) Zga1 = t(Zga1)
		if(is.null(zga1)) zga1 = rep(0,nrow(Zga1))
		constr.ga1 = TRUE
	}else constr.ga1 = FALSE
	if(!is.null(Zga2)){
		if(is.vector(Zga2)) Zga2 = t(Zga2)
		if(is.null(zga2)) zga2 = rep(0,nrow(Zga2))
		constr.ga2 = TRUE
	}else constr.ga2 = FALSE
# adjust covariates	
	cov = !is.null(X)
	if (cov) {
		X = as.matrix(X)
		namesX = colnames(X)
		if (glob) logit_cov = "g" else logit_cov = "m"
	}else logit_cov = "m"
# adjust S for missing data	
	miss = any(is.na(S))
	ns = nrow(S)
	J = ncol(S)
	if (miss) {
		cat("Missing data in the dataset, units and items without responses are removed\n")
		ind = which(apply(is.na(S), 1, all))
		if (length(ind) > 0) {
			S = S[-ind, ]
			yv = yv[-ind]
			if (!is.null(X)) X = as.matrix(X[-ind, ])
			ind = which(apply(is.na(S), 2, all))
			if (length(ind) > 0) {
				S = S[, -ind]
				miss = any(is.na(S))
			}
		}
	}
	if (miss) {
		R = 1 * (!is.na(S))
		S[is.na(S)] = 0
	}
	lv = apply(S, 2, max) + 1
	if(difl & max(lv)>min(lv))
    		stop("Option difl=TRUE not allowed in the presence of items with a different number of response categories within the same dimension")
	lm = max(lv)
	ns = nrow(S)
	J = ncol(S)
	n = sum(yv)
# design matrix for the covariates	
	if (cov) {
		ncov = ncol(X)
		out = aggr_data(X, fort = fort)
		Xdis = as.matrix(out$data_dis)
		Xlabel = out$label
		Xndis = max(out$label)
		if (glob) {
			XX1dis = array(0, c(k1 - 1, k1 - 1 + ncov, Xndis))
			for (i in 1:Xndis) XX1dis[, , i] = cbind(diag(k1-1), rep(1, k1-1) %o% Xdis[i, ])
		} else {
			XX1dis = array(0, c(k1 - 1, (k1 - 1) * (ncov + 1), Xndis))
			if (k1 == 2) II = 1
			else II = diag(k1 - 1)
			for (i in 1:Xndis) XX1dis[, , i] = II %x% t(c(1, Xdis[i, ]))
		}
		if (glob) {
			XX2dis = array(0, c(k2 - 1, k2 - 1 + ncov, Xndis))
			for (i in 1:Xndis) XX2dis[, , i] = cbind(diag(k2 - 1), rep(1, k2 - 1) %o% Xdis[i, ])
		} else {
			XX2dis = array(0, c(k2 - 1, (k2 - 1) * (ncov + 1),Xndis))
			if (k2 == 2) II = 1
			else II = diag(k2 - 1)
			for (i in 1:Xndis) XX2dis[, , i] = II %x% t(c(1,Xdis[i, ]))
		}
	}else{
		ncov = 0
		XX1dis = array(diag(k1 - 1), c(k1 - 1, k1 - 1, 1))
		XX2dis = array(diag(k2 - 1), c(k2 - 1, k2 - 1, 1))
		Xlabel = rep(1, ns)
	}
# design matrices for the responses
	Aggr1 = diag(k1) %x% matrix(1, 1, k2)
	Aggr2 = matrix(1, 1, k1) %x% diag(k2)
	if (link == "global") ltype = "g"
	else if (link == "local") ltype = "l"
	items1 = sort(unique(as.vector(multi1))) # items affected by first latent variable
	if (any(items1 == 0)) items1 = items1[items1 > 0]
	Jitems1 = length(items1)
	rm1 = nrow(multi1)
	items2 = sort(unique(as.vector(multi2))) # items affected by second latent variable
	if (any(items2 == 0)) items2 = items2[items2 > 0]
	Jitems2 = length(items2) 
	rm2 = nrow(multi2)
# index of the discriminant parameters that are constrained (1st LV)	
	fv1 = multi1[, 1]
	fv1e = NULL
	count = 0
	for (j in 1:J){
		if (j %in% fv1) fv1e = c(fv1e, count + 1)
		count = count + lv[j] - 1
	}
# index of the discriminant parameters that are constrained (2nd LV)	
	fv2 = multi2[, 1]
	fv2e = NULL
	count = 0
	for (j in 1:J) {
		if (j %in% fv2) fv2e = c(fv2e, count + 1)
		count = count + lv[j] - 1
	}
# index of the discriminant parameters that are constrained (both LVs)	
	fv = union(fv1, fv2)
	fve = union(fv1e, fv2e)
	rm = length(fve)
# index of free discriminat parameters	
	indga1t = setdiff(items1, fv1) # index of ga1t in ga1c
	indga2t = setdiff(items2, fv2)
# index of the free ability parameters	
	if(constr.th1){
		if(ncol(Zth1)==0) indth1t = NULL # index of th1t in part
		else indth1t = 1:ncol(Zth1)
	}else indth1t = 1:(k1*rm1)
	indth1 = 1:(k1*rm1) # index of th1 in par
	if(constr.th2){
		if(ncol(Zth2)==0) indth2t = NULL
		else{
			if(is.null(indth1t)) tmp = 0
			else max = max(indth1t)
			indth2t = tmp+(1:ncol(Zth2))
		}
	}else{
		if(is.null(indth1t)) tmp = 0
		else tmp = max(indth1t)
		indth2t = tmp+(1:(k2*rm2))
	}
	indth2 = k1*rm1+(1:(k2*rm2))
# index of free difficulty parameters	
	if(constr.be){
		if(ncol(Zbe)==0) indbet = NULL
		else{
			if(is.null(indth1t)) tmp = 0
			else tmp = max(indth1t)
			if(!is.null(indth2t)) tmp = max(indth2t)
			indbet = tmp+(1:ncol(Zbe)) # index of bet in part
   		}
    	}else{
		if(is.null(indth1t)) tmp = 0
		else tmp = max(indth1t)
		if(!is.null(indth2t)) tmp = max(indth2t)
		if(difl) indbet = tmp+(1:(J-rm+lm-2))
		else indbet = tmp+(1:(sum(lv - 1))-rm)
	}
	indbe = k1 * rm1 + k2 * rm2 + (1:sum(lv - 1))
	abils1 = rep(0, J)  # items affects by the first type of ability
	if (rm1 == 1) abils1[multi1] = 1
	else {
		for (h in 1:rm1) {
			ind = multi1[h, ]
			ind = ind[ind > 0]
			abils1[ind] = h
		}
	}
	abils2 = rep(0, J)
	if (rm2 == 1) abils2[multi2] = 1
	else {
		for (h in 1:rm2) {
			ind = multi2[h, ]
			ind = ind[ind > 0]
			abils2[ind] = h
		}
	}
	abils = rep(0, J)
	for(j in 1:J) for(h in 1:rm) if(abils1[j] == abils1[fv[h]] & abils2[j] == abils2[fv[h]]) abils[j] = h
# design matrix for logit model	
	ZZ = array(NA, c(lm - 1, k1 * rm1 + k2 * rm2 + sum(lv - 1), J * k1 * k2))
	cont = 0
	refitem = matrix(0, J * k1 * k2, 1)
	for (c1 in 1:k1){	
		u11 = matrix(0, 1, k1)
		u11[c1] = 1
		for(c2 in 1:k2){
			u12 = matrix(0, 1, k2)
			u12[c2] = 1
			for(j in 1:J){
				u21 = matrix(0, 1, rm1)
				u21[abils1[j]] = 1
				u22 = matrix(0, 1, rm2)
				u22[abils2[j]] = 1
				v = matrix(0, 1, J)
				v[j] = 1
				cont = cont + 1
				Te = matrix(0, lv[j] - 1, sum(lv - 1))
				if (j == 1) Te[, 1:(lv[j] - 1)] = diag(lv[j] - 1)
				else Te[, sum(lv[1:(j - 1)] - 1) + (1:(lv[j] - 1))] = diag(lv[j] - 1)
				ZZ[1:(lv[j]-1),,cont] = cbind(rep(1,lv[j]-1)%*%(u11%x%u21),rep(1,lv[j]-1)%*%(u12%x%u22),-Te)
				refitem[cont] = j
			}
		}
	}
	ZZ0 = ZZ
# matrix of constraints for glob
	if (glob) {
		II1 = diag(k1 - 1)
		II1 = cbind(0, II1) - cbind(II1, 0)
		if (rm1 > 1) II1 = II1 %x% matrix(c(1, rep(0, rm1 - 1)), 1, rm1)
		II2 = diag(k2 - 1)
		II2 = cbind(0, II2) - cbind(II2, 0)
		if (rm2 > 1) II2 = II2 %x% matrix(c(1, rep(0, rm1 - 1)), 1, rm1)
		II = rbind(cbind(II1, matrix(0, k1 - 1, dim(II2)[2])), cbind(matrix(0, k2 - 1, dim(II1)[2]), II2))
		Dis = cbind(II, matrix(0, k1 + k2 - 2, dim(ZZ)[2] - (k1*rm1+k2*rm2)))
	}else Dis = NULL
# design matrices for theta, beta and gamma	
	if(!constr.th1){
		Zth1 = diag(k1*rm1); zth1 = rep(0,k1*rm1)
	}
	if(!constr.th2){
		Zth2 = diag(k2*rm2); zth2 = rep(0,k2*rm2)
	}
	if(!constr.be){
		if(difl){
			if(lm-1==2) Tmp = matrix(c(0,1),2,1)
			else Tmp = diag(lm-1)[,-1]
			Zbe = cbind(diag(J)[,-fv]%x%matrix(1,lm-1,1),matrix(1,J,1)%x%Tmp)
		}else{
			Zbe = diag(sum(lv - 1))[,-fve] 
		}
		zbe = rep(0,sum(lv - 1))
	}
	Zpar = blkdiag(blkdiag(Zth1,Zth2),Zbe)
	zpar = c(zth1,zth2,zbe)
	if(!constr.ga1){
		Zga1 = diag(Jitems1)[,-which(items1%in%fv1)]
		zga1 = rep(0,Jitems1); zga1[which(items1%in%fv1)] = 1
	}
	if(!constr.ga2){
		Zga2 = diag(Jitems2)[,-which(items2%in%fv2)]
		zga2 = rep(0,Jitems2); zga2[which(items2%in%fv2)] = 1
	}
# starting values
    if (start == "deterministic") {
        if(cov){
            de1 = de2 = NULL
            Piv = matrix(1/(k1 * k2), ns, k1 * k2)
            piv1 = piv2 = NULL
        }else{
            de1 = de2 = NULL
	        piv1 = rep(1, k1)/k1
    	    piv2 = rep(1, k2)/k2
        }
        if (k1 == 1) grid1 = 0
        else grid1 = seq(-k1, k1, 2 * k1/(k1 - 1))
        if (k2 == 1) grid2 = 0
        else grid2 = seq(-k2, k2, 2 * k2/(k2 - 1))
        Phi = array(NA, c(lm, J, k1 * k2))
        for (j in 1:J) {
            dist = rep(0, lv[j])
            for (y in 0:(lv[j] - 1)) dist[y + 1] = (sum(yv[S[,j] == y]) + 0.5)/n
            out = matr_glob(lv[j])
            Co = out$Co
            Ma = out$Ma
            eta = Co %*% log(Ma %*% dist)
            count = 0
            for (c1 in 1:k1) for (c2 in 1:k2) {
                count = count + 1
                Phi[1:lv[j], j, count] = inv_glob(eta + grid1[c1] + grid2[c2])$p
            }
        }
    }
	if (start == "random") {
		if (cov) {
			if (glob){
				de1 = de2 = NULL
				Piv = matrix(runif(ns * k1 * k2), ns, k1 * k2)
				Piv = Piv * (1/rowSums(Piv))
				piv1 = piv2 = NULL
			}else {
				de1 = rnorm((k1 - 1) * (ncov + 1))/rep(c(1, apply(X, 2, sd)), (k1 - 1))
				de2 = rnorm((k2 - 1) * (ncov + 1))/rep(c(1, apply(X, 2, sd)), (k2 - 1))
				if (k1 > 1) Piv1 = prob_multi_glob(XX1dis, logit_cov, de1, Xlabel)$P
				if (k2 > 1) Piv2 = prob_multi_glob(XX2dis, logit_cov, de2, Xlabel)$P
				if (k1 == 1) Piv = Piv2
				if (k2 == 1) Piv = Piv1
				if (k1 > 1 & k2 > 1) {
					Piv = matrix(0, ns, k1 * k2)
					for (i in 1:ns) Piv[i,] = Piv1[i,]%x%Piv2[i,]
				}
				piv1 = piv2 = NULL
			}
		} else {
			de1 = de2 = NULL
			piv1 = runif(k1)
			piv1 = piv1/sum(piv1)
			piv2 = runif(k2)
			piv2 = piv2/sum(piv2)
		}
        Phi = array(runif(lm * J * k1 * k2), c(lm, J, k1 * k2))
        for (c in 1:(k1 * k2)) for (j in 1:J) {
            Phi[1:lv[j], j, c] = Phi[1:lv[j], j, c]/sum(Phi[1:lv[j],j, c])
            if (lv[j] < lm) Phi[(lv[j] + 1):lm, j, c] = NA
        }
        if(glob){
        	if (runif(1) > 0.5) for (j in 1:J) {
            	mPhi = (0:(lv[j] - 1)) %*% Phi[1:lv[j], j, ]
                ind = order(mPhi)
                Phi[, j, ] = Phi[, j, ind]
            }
        }
    }
	if (start == "external"){
		de1 = as.vector(De1)
		de2 = as.vector(De2)
		if (cov) {
			if (k1 > 1) Piv1 = prob_multi_glob(XX1dis, logit_cov, de1, Xlabel)$P
			if (k2 > 1) Piv2 = prob_multi_glob(XX2dis, logit_cov, de2, Xlabel)$P
			if (k1 == 1) Piv = Piv2
			if (k2 == 1) Piv = Piv1
			if (k1 > 1 & k2 > 1) {
				Piv = matrix(0, ns, k1 * k2)
				for (i in 1:ns) Piv[i,] = Piv1[i,]%x%Piv2[i,]
			}
			piv1 = piv2 = NULL
		} else {
			piv1 = c(1,exp(de1))
			piv1 = piv1/sum(piv1)
			piv2 = c(1,exp(de2))
			piv2 = piv2/sum(piv2)
		}
	}
	if (start == "deterministic" || start == "random" || (start=="external" & is.null(ga1t))){
		if(is.null(Zga1)) ga1t = rep(1,0)
		else ga1t = rep(1, ncol(Zga1))
	}
	if (start == "deterministic" || start == "random" || (start=="external" & is.null(ga2t))){
		if(is.null(Zga2)) ga2t = rep(1,0)
		else ga2t = rep(1, ncol(Zga2))
	}
	ga1 = Zga1%*%ga1t+zga1 
	ga1c = rep(0,J); ga1c[items1] = ga1
	ga2 = Zga2%*%ga2t+zga2 
	ga2c = rep(0,J); ga2c[items2] = ga2
# compute log-likelihood
	Psi = matrix(1, ns, k1 * k2)
	if (miss) for (j in 1:J) for (c in 1:(k1 * k2)) Psi[, c] = Psi[, c] * (Phi[S[, j] + 1, j, c] * R[, j] + (1 - R[, j]))
	else for (j in 1:J) for (c in 1:(k1 * k2)) Psi[, c] = Psi[, c] * Phi[S[, j] + 1, j, c]
	if(cov){
		if (start == "external") {
			if (k1 > 1) Piv1 = prob_multi_glob(XX1dis, logit_cov, de1, Xlabel)$P
			if (k2 > 1) Piv2 = prob_multi_glob(XX2dis, logit_cov, de2, Xlabel)$P
			if (k1 == 1) Piv = Piv2
			if (k2 == 1) Piv = Piv1
			if (k1 > 1 & k2 > 1) {
				Piv = matrix(0, ns, k1 * k2)
				for (i in 1:ns) Piv[i, ] = Piv1[i, ] %x% Piv2[i,]
			}
		}
	}else Piv = rep(1, ns) %o% (piv1 %x% piv2)
	if (k1 * k2 == 1) Pj = Psi
	else Pj = Psi * Piv
	pm = rowSums(Pj)
	lk = sum(yv * log(pm))
# iterate untile convergence
	cat("*-------------------------------------------------------------------------------*\n")
	cat(c("Model with multidimensional structure\n\n"))
	cat("* First latent variable\n")
	names11 = NULL
	for(j in 1:rm1) names11 = c(names11, paste("Dimension", j))
	rownames(multi1) = names11
	print(multi1)
	cat("\n * Second latent variable\n")
	names12 = NULL
	for (j in 1:rm2) names12 = c(names12, paste("Dimension", j))
	rownames(multi2) = names12
	print(multi2)
	cat("\n")
	cat(c("Number of classes for 1st LV = ", k1, "\n"))
	cat(c("Number of classes for 2nd LV = ", k2, "\n"))
	cat(c("Link of type =                 ", link, "\n"))
	cat(c("Discrimination index =         ", disc, "\n"))
	cat(c("Constraints on the difficulty =", difl, "\n"))
	cat(c("Type of initialization =       ", start, "\n"))
	cat("*-------------------------------------------------------------------------------*\n")
	cat("\n")
	if (disp) {
		if (!disc || (length(ga1) == 0 & length(ga2) == 0)) {
			cat("------------|-------------|-------------|-------------|-------------|-------------|\n")
			cat("  iteration |      lk     |    lk-lko   |     dis     |   min(par)  |   max(par)  |\n")
			cat("------------|-------------|-------------|-------------|-------------|-------------|\n")
		}
		if (disc) {
			cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|\n")
			cat("  iteration |    lk       |    lk-lko   |      dis    |   min(ga)   |   max(ga)   |   min(par)  |   max(par)  |\n")
			cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|\n")
		}
		cat(sprintf("%11g", c(0, lk)), "\n", sep = " | ")
	}
	it = 0
	lko = lk - 10^10
	dis = 0
	part = par = dga = lkv = NULL
	while (((abs(lk - lko)/abs(lko) > tol) && it < maxitc) || it < 2) {
		it = it + 1
		paro = par
		ga1o = ga1
		ga2o = ga2
		piv1o = piv1
		piv2o = piv2
		de1o = de1
		de2o = de2
		lko = lk
# E-step
		V = ((yv/pm) %o% rep(1, k1 * k2)) * Piv * Psi
		sV = colSums(V)
		YY = matrix(NA, J * k1 * k2, lm)
		count = 0
		for (c in 1:(k1 * k2)) for (j in 1:J) {
			count = count + 1
			for (y in 1:lv[j]) {
				ind = (S[, j] == (y - 1))
				if (miss) YY[count, y] = sum(V[ind, c] * R[ind, j])
				else YY[count, y] = sum(V[ind, c])
			}
		}
# M-step        
		if(disc){
			if(it > 1){
				ZZ1 = ZZ2 = array(NA, c(lm - 1, J, J * k1 * k2))
				count = 0
				for (c1 in 1:k1) for (c2 in 1:k2) for (j in 1:J) {
					count = count + 1
					ZZ1[1:(lv[j]-1),,count] = 0
					ZZ1[1:(lv[j]-1),j,count] = ZZ0[1:(lv[j]-1),1:(k1*rm1),count]%*%par[1:(k1*rm1)]
					ZZ2[1:(lv[j]-1),,count] = 0
					ZZ2[1:(lv[j]-1),j,count] = ZZ0[1:(lv[j] -1),k1*rm1+1:(k2*rm2),count]%*%par[k1*rm1+1:(k2*rm2)]
				}
				ZZ1 = ZZ1[,items1,]; ZZ2 = ZZ2[,items2,]
				ind = k1 * rm1 + k2 * rm2 + 1:(sum(lv - 1))
				ZZ1Int = ZZ2Int = array(NA, c(lm-1, J*k1*k2))
				count = 0
				for (c1 in 1:k1) for (c2 in 1:k2) for (j in 1:J){
					count = count + 1
					ZZ1Int[1:(lv[j]-1),count] = ga2c[j]*ZZ0[1:(lv[j]-1),k1*rm1+1:(k2*rm2),count]%*%
						par[k1 * rm1 + 1:(k2 * rm2)] + ZZ0[1:(lv[j] - 1), ind, count] %*% par[ind]
					ZZ2Int[1:(lv[j] - 1), count] = ga1c[j]*ZZ0[1:(lv[j]-1),1:(k1*rm1),count] %*%
						par[1:(k1 * rm1)] + ZZ0[1:(lv[j] - 1), ind, count] %*% par[ind]
				}
# update gamma				
				if(constr.ga1 || constr.th1){
					ga1t = est_multi_glob_genZ(YY, ZZ1, ltype, de = ga1t, Int = ZZ1Int,Z=Zga1,z=zga1)$de
					ga1 = Zga1%*%ga1t+zga1; ga1c = rep(1,Jitems1); ga1c[items1] = ga1
				}else{
					ga1c[items1] = est_multi_glob_genZ(YY,ZZ1, ltype, de = ga1c[items1], Int = ZZ1Int)$be
					for(j in 1:rm1){
						tmp = ga1c[multi1[j,1]]
						ga1c[multi1[j,]] = ga1c[multi1[j,]]/tmp
						ind = seq(j,k1*rm1,rm1) 
						part[ind] = part[ind]*tmp
					}
					ga1 = ga1c[items1]; ga1t = ga1c[indga1t]
				}
				if(constr.ga2 || constr.th2){
					ga2t = est_multi_glob_genZ(YY, ZZ2,ltype,de = ga2t,Int = ZZ2Int,Z=Zga2,z=zga2)$de
					ga2 = Zga2%*%ga2t+zga2; ga2c = rep(1,Jitems2); ga2c[items2] = ga2
				}else{
					ga2c[items2] = est_multi_glob_genZ(YY,ZZ2,ltype,de = ga2c[items2],Int = ZZ2Int)$be
					for(j in 1:rm2){
						tmp = ga2c[multi2[j,1]]
						ga2c[multi2[j,]] = ga2c[multi2[j,]]/tmp
						ind = k1*rm1+seq(j,k2*rm2,rm2)
						part[ind] = part[ind]*tmp
					}
					ga2 = ga2c[items2]; ga2t = ga2c[indga2t]
				}
				par = Zpar%*%part+zpar
			}
			ZZ = ZZ0
			for (j in 1:J) {
				ind = (refitem == j)
				ZZ[,1:(k1 * rm1), ind] = ZZ[, 1:(k1 * rm1), ind] * ga1c[j]
				ZZ[,k1 * rm1 + 1:(k2 * rm2), ind] = ZZ[, k1 * rm1 + 1:(k2 * rm2), ind] * ga2c[j]
			}
		}
# update part		
		if(start==2 & it==1) maxit = 250 else maxit = 10
		out = est_multi_glob_genZ(YY,ZZ,ltype,de = part,Z=Zpar,z=zpar,Dis=Dis,maxit=maxit)
		part = out$de; par = Zpar%*%part+zpar
		P = out$P
		Phi = array(t(P), c(lm, J, k1 * k2))
# update de	
		if (cov) {
			if (k1 > 1) {
				out = est_multi_glob_genZ(V %*% t(Aggr1), XX1dis, logit_cov, Xlabel, de1)
				de1 = out$be
				P1dis = out$Pdis
				Piv1 = out$P
			}
			if (k2 > 1) {
				out = est_multi_glob_genZ(V %*% t(Aggr2), XX2dis, logit_cov, Xlabel, de2)
				de2 = out$be
				P2dis = out$Pdis
				Piv2 = out$P
			}
			if (k1 == 1) Piv = Piv2
			if (k2 == 1) Piv = Piv1
			if (k1 > 1 & k2 > 1) for (i in 1:ns) Piv[i, ] = Piv1[i, ] %x% Piv2[i,]
		} else {
			piv1 = as.vector(Aggr1 %*% sV)/n
			piv2 = as.vector(Aggr2 %*% sV)/n
			Piv = rep(1, ns) %o% (piv1 %x% piv2)
		}
# compute log-likelihood		
		Psi = matrix(1, ns, k1 * k2)
		if (miss) for (j in 1:J) for (c in 1:(k1 * k2)) Psi[, c] = Psi[, c] * (Phi[S[, j] + 1, j, c] * R[, j] + (1 - R[, j]))
		else for (j in 1:J) for (c in 1:(k1 * k2)) Psi[, c] = Psi[, c] * Phi[S[, j] + 1, j, c]
		if (k1 * k2 == 1) Pj = Psi
		else Pj = Psi * Piv
		pm = rowSums(Pj)
		lk = sum(yv * log(pm))
		if (it > 1) dis = max(c(abs(par - paro), abs(ga1 - ga1o), abs(ga2 - ga2o), abs(piv1 - piv1o), abs(piv2 - piv2o)))
		if (disp)  if (it/10 == floor(it/10)) {
			if (!disc || (length(ga1) == 0 & length(ga2) == 0)) cat(sprintf("%11g", c(it, lk, lk - lko, dis, min(par), max(par))), "\n", sep = " | ")
			if (disc) cat(sprintf("%11g", c(it, lk, lk - lko, dis, min(c(ga1, ga2)), max(c(ga1, ga2)), min(par), max(par))), "\n", sep = " | ")
		}
		lkv = c(lkv, lk)
	}
	if (disp) {
		if (it/10 > floor(it/10)) {
			if (!disc || (length(ga1) == 0 & length(ga2) == 0)) cat(sprintf("%11g", c(it, lk, lk - lko, dis, min(par), max(par))), "\n", sep = " | ")
			if (disc) cat(sprintf("%11g", c(it, lk, lk - lko, dis, min(c(ga1, ga2)), max(c(ga1, ga2)), min(par), max(par))), "\n", sep = " | ")
		}
		if (!disc || (length(ga1) == 0 & length(ga2) == 0)) cat("------------|-------------|-------------|-------------|-------------|-------------|\n")
		if (disc) cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|\n")
	}
# final output	
	np = length(part)	
	if (disc) np=np+length(ga1t)+length(ga2t)
	if (cov) {
		if (glob) np = np + k1 + k2 - 2 + 2 * ncov
		else np = np + (k1 + k2 - 2) * (ncov + 1)
	} else np = np + k1 - 1 + k2 - 1
	aic = -2 * lk + 2 * np
	bic = -2 * lk + np * log(n)
	th1t = part[indth1t]
	th2t = part[indth2t]
	bet = part[indbet]
	be = par[indbe]
	if(difl){
		bec1 = rep(0, J)
		bec1[(1:J)[-fv]] = bet[1:(J - rm)]
		bec2 = rep(0, lm - 1)
		bec2[2:(lm - 1)] = bet[J - rm + (1:(lm - 2))]
       	Bec = list(difficulties = bec1, cutoffs = bec2)
	}else{
		Bec = matrix(NA, J, lm - 1)
		count = 0
		for(j in 1:J){
			Bec[j, (1:lv[j] - 1)] = be[count + (1:(lv[j] - 1))]
			count = count + lv[j] - 1
		}
		dimnames(Bec) = list(item=1:J,cutoff=1:(lm-1))
	}
	ga1c[setdiff(1:J,multi1v)] = NA
	ga2c[setdiff(1:J,multi2v)] = NA
	th1 = Zth1%*%th1t+zth1
	Th1 = matrix(th1, rm1, k1)
	dimnames(Th1) = list(dimension=1:rm1,class=1:k1)
	th2 = Zth2%*%th2t+zth2
	Th2 = matrix(th2, rm2, k2)
	dimnames(Th2) = list(dimension=1:rm2,class=1:k2)
	Pp = ((1/pm) %o% rep(1, k1 * k2)) * Piv * Psi
	Pp1 = Pp %*% t(Aggr1)
	Pp2 = Pp %*% t(Aggr2)
	ent = -sum(V * log(pmax(Pp, 10^-100)))
	dimnames(Phi) = list(category = 0:(lm - 1), item = 1:J, class = 1:(k1*k2))
	if (cov){
		if (glob) {
			if (k1 == 1) De1 = NULL
			else{
				De1 = matrix(de1, ncov + k1 - 1, 1)
				names_cutoff = paste("cutoff", 1:(k1 - 1), sep = "")
				if (is.null(namesX)) namesX1 = c(names_cutoff, paste("X", 1:ncov, sep = ""))
				else namesX1 = c(names_cutoff, namesX)
				rownames(De1) = namesX1
			}
			if (k2 == 1) De2 = NULL
			else {
				De2 = matrix(de2, ncov + k2 - 1, 1)
				names_cutoff = paste("cutoff", 1:(k2 - 1), sep = "")
				if (is.null(namesX)) namesX2 = c(names_cutoff, paste("X", 1:ncov, sep = ""))
				else namesX2 = c(names_cutoff, namesX)
				rownames(De2) = namesX2
			}
		}else {
			if (is.null(namesX)) namesX = c("intercept", paste("X", 1:ncov, sep = ""))
			else namesX = c("intercept", namesX)
			if (k1 == 1) De1 = NULL
			else {
				De1 = matrix(de1, ncov + 1, k1 - 1)
				dimnames(De1) = list(namesX, logit = 2:k1)
			}
			if (k2 == 1) De2 = NULL
			else {
				De2 = matrix(de2, ncov + 1, k2 - 1)
				dimnames(De2) = list(namesX, logit = 2:k2)
			}
		}
		piv1 = t(Piv1)%*%yv/n
		piv2 = t(Piv2)%*%yv/n
	}else{
		de1 = log(piv1[-1]/piv1[1])
		de2 = log(piv2[-1]/piv2[1])
		De1 = t(de1); dimnames(De1) = list("intercept", logit = 2:k1)
		De2 = t(de2); dimnames(De2) = list("intercept", logit = 2:k2)
	}
# compute standard errors    
	if(out_se){
		lde1 = length(de1)
		lde2 = length(de2)
		lpar = length(par); lpart = length(part)
		lga = 0
		part_comp = c(de1, de2, part)
		if (disc) {	
			lga1 = length(ga1); lga1t = length(ga1t)
			lga2 = length(ga2); lga2t = length(ga2t)
			part_comp = c(part_comp, ga1t, ga2t)
		}
		if (disp) {
			cat("computation of derivatives\n")
			cat(length(part_comp), "parameters\n")
		}
		out = lk_obs_score_within(part_comp, lde1, lde2, lpart,
			lga1t,lga2t,S, R, yv, k1, k2, rm1, rm2, lv, J, fv, disc,
			glob, refitem, miss, ltype, XX1dis, XX2dis,
			Xlabel, ZZ0, fort, Zpar,zpar, Zga1,zga1,Zga2,zga2,items1,items2)
		scn = rep(0, length(part_comp))
		Jn = NULL
		for (j in 1:length(part_comp)) {
			part_comp1 = part_comp
			part_comp1[j] = part_comp1[j] + 10^-6
			out1 = lk_obs_score_within(part_comp1, lde1, lde2, lpart,
				lga1t,lga2t, S, R, yv, k1, k2, rm1, rm2, lv, J, fv, disc, 
				glob, refitem, miss, ltype, XX1dis, XX2dis,
				Xlabel, ZZ0, fort, Zpar, zpar,Zga1,zga1,Zga2,zga2,items1,items2)
			scn[j] = (out1$lk - lk) * 10^6
			Jn = cbind(Jn, (out1$sc - out$sc) * 10^6)
			if (disp){
				if (j/10 > floor(j/10)) cat(".") else cat(round(j/10))
				if (j/100 == floor(j/100)) cat("\n")
			}
		}
		if(disp) cat("\n")
		Jn = -(Jn + t(Jn))/2
		Vnt = ginv(Jn)
		ZZZ = blkdiag(diag(lde1+lde2),Zpar)
		if(disc) ZZZ = blkdiag(blkdiag(ZZZ,Zga1),Zga2)
		Vn = ZZZ%*%Vnt%*%t(ZZZ)
		set = sqrt(abs(diag(Vnt)))
		se = sqrt(abs(diag(Vn)))
		if (k1 > 1) sede1 = se[1:lde1]
		if (k2 > 1) sede2 = se[lde1 + 1:lde2]
		separt = set[lde1 + lde2 + (1:lpart)]
		separ = se[lde1 + lde2 + (1:lpar)]
		if(disc){
			sega1t = set[lde1 + lde2 + lpart + (1:lga1t)]
			sega2t = set[lde1 + lde2 + lpart + lga1t + (1:lga2t)]
			sega1 = se[lde1 + lde2 + lpar + (1:lga1)]
			sega2 = se[lde1 + lde2 + lpar + lga1 + (1:lga2)]
			sega1c = rep(NA, J); sega1c[items1] = sega1
			sega2c = rep(NA, J); sega2c[items2] = sega2
		}else{
			sega1t = sega1 = sega1c = NULL
			sega2t = sega2 = sega2c = NULL
		}
		seth1t = separt[indth1t]
 		seth2t = separt[indth2t]
		seth1 = separ[indth1]
 		seth2 = separ[indth2]
 		seTh1 = matrix(seth1, rm1, k1)
 		seTh2 = matrix(seth2, rm2, k2)
 		dimnames(seTh1) = list(dimension=1:rm1,class=1:k1)
		dimnames(seTh2) = list(dimension=1:rm2,class=1:k2)
 		sebet = separt[indbet]
 		sebe = separ[indbe]
		if (difl){
			sebec1 = rep(0, J)
			sebec1[(1:J)[-fv]] = sebe[1:(J - rm)]
			sebec2 = rep(0, lm - 1)
			sebec2[2:(lm - 1)] = sebe[J - rm + (1:(lm - 2))]
        		seBec = list(difficulties = sebec1, cutoffs = sebec2)
    		}else{
			seBec = matrix(NA, J, lm - 1)
			count = 0
			for (j in 1:J) {
				seBec[j, (1:lv[j] - 1)] = sebe[count + (1:(lv[j] - 1))]
				count = count + lv[j] - 1
			}
			dimnames(seBec) = list(item=1:J,cutoff=1:(lm-1))
		}
		if (cov){
			if (glob) {
				if (k1 == 1) seDe1 = NULL
				else{
					seDe1 = matrix(sede1, ncov + k1 - 1, 1)
					rownames(seDe1) = namesX1
				}
				if (k2 == 1) seDe2 = NULL
				else{
					seDe2 = matrix(sede2, ncov + k2 - 1, 1)
					rownames(seDe2) = namesX2
				}
			}else{
				if (k1 == 1) seDe1 = NULL
				else{
					seDe1 = matrix(sede1, ncov + 1, k1 - 1)
					dimnames(seDe1) = list(namesX, logit = 2:k1)
				}
				if (k2 == 1) seDe2 = NULL
				else{
					seDe2 = matrix(sede2, ncov + 1, k2 - 1)
					dimnames(seDe2) = list(namesX, logit = 2:k2)
				}
			}
        }else{
        	seDe1 = t(sede1); dimnames(seDe1) = list("intercept", logit = 2:k1)
        	seDe2 = t(sede2); dimnames(seDe2) = list("intercept", logit = 2:k2)
        }
	}
# order and standardize
	mu1 = drop(Th1%*%piv1)
	sd1 = drop(sqrt((Th1-mu1)^2%*%piv1))
	ind1 = order(Th1[1,])
	Th1s = (Th1[,ind1]-mu1)/sd1
	piv1s = piv1[ind1]
	mu2 = drop(Th2%*%piv2)
	sd2 = drop(sqrt((Th2-mu2)^2%*%piv2))
	ind2 = order(Th2[1,])
	Th2s = (Th2[,ind2]-mu2)/sd2
	piv2s = piv2[ind2]
	Becs = Bec
	ga1cs = ga1c
	for(j in 1:rm1){
		tmp = multi1[j,]; tmp = tmp[tmp>0]
		ga1cs[tmp] = ga1c[tmp]*sd1[j]
		if(difl) Becs$difficulties[tmp] = Becs$difficulties[tmp]-ga1c[tmp]*mu1[j] 
		else Becs[tmp,] = Becs[tmp,]-ga1c[tmp]*mu1[j]				
	}
	ga2cs = ga2c
	for(j in 1:rm2){
		tmp = multi2[j,]; tmp = tmp[tmp>0]
		ga2cs[tmp] = ga2c[tmp]*sd2[j]
		if(difl) Becs$difficulties[tmp] = Becs$difficulties[tmp]-ga2c[tmp]*mu2[j]
		else Becs[tmp,] = Becs[tmp,]-ga2c[tmp]*mu2[j]
	}
# output
	out = list(piv1 = piv1, piv2 = piv2, fv1 = fv1, fv2 = fv2, th1t=th1t, th2t=th2t, Th1=Th1, Th2=Th2,
		bet=bet,Bec=Bec,ga1t=ga1t,ga2t=ga2t,ga1c=ga1c,ga2c=ga2c,De1=De1,De2=De2,Phi=Phi,lk = lk,
		np = np, aic = aic, bic = bic, ent = ent, piv1s=piv1s, piv2s=piv2s, Th1s=Th1s, Th2s=Th2s,
		Becs=Becs, ga1cs = ga1cs, ga2cs=ga2cs,call = match.call())
	if (output){
		out$Pp1 = Pp1
		out$Pp2 = Pp2
		out$lkv = lkv
		if(cov){
			out$Xlabel = Xlabel
			out$XX1dis = XX1dis
			out$XX2dis = XX2dis
			out$Piv1 = Piv1
			out$Piv2 = Piv2
		}
	}
	if(out_se){
		out$seth1t = seth1t
		out$seth2t = seth2t
		out$seTh1 = seTh1
		out$seTh2 = seTh2
		out$sebet = sebet
		out$seBec = seBec
		out$sega1t = sega1t  
		out$sega2t = sega2t 		
		out$sega1c = sega1c
		out$sega2c = sega2c
		out$seDe1 = seDe1
		out$seDe2 = seDe2
		out$Vnt = Vnt
		out$Vn = Vn
	}
	class(out) = "est_multi_poly_within"
	return(out)
	
}