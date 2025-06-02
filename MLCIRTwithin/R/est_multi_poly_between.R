est_multi_poly_between <- function (S, yv = rep(1, ns), k, X = NULL, start = c("deterministic","random","external"),
    link = c("global","local"), disc = FALSE, difl = FALSE, multi = 1:J, Phi = NULL, gat = NULL, De = NULL,
    fort = FALSE, tol = 10^-10, maxitc = 10^4, disp = FALSE, output = FALSE, out_se = FALSE, glob = FALSE, 
    Zth=NULL,zth=NULL, Zbe=NULL,zbe=NULL,Zga=NULL,zga=NULL){

# check input    	
	if (k == 1) stop("--> use est_multi_poly")
	if (max(S, na.rm = TRUE) == 1 & difl) {
		warning("with binary data put difl=FALSE\n")
		difl = FALSE
	}
	link = match.arg(link)
	start = match.arg(start)
    J = ncol(S)
	if(is.vector(multi)) multi = t(multi)
	if(!is.null(Zth)){
		if(is.vector(Zth)) Zth = t(Zth)
		if(is.null(zth)) zth = rep(0,nrow(Zth))
		constr.th = TRUE
	}else{
		constr.th = FALSE
	}
	if(!is.null(Zbe)){
		if(is.vector(Zbe)) Zbe = t(Zbe)
		if(is.null(zbe)) zbe = rep(0,nrow(Zbe))
		constr.be = TRUE
	}else{
		constr.be = FALSE
	}
	if(!is.null(Zga)){
		if(is.vector(Zga)) Zga = t(Zga)
		if(is.null(zga)) zga = rep(0,nrow(Zga))
		constr.ga = TRUE
	}else{
		constr.ga = FALSE
	}
# adjust covariates	
    cov = !is.null(X)
    if(cov){
        X = as.matrix(X)
        namesX = colnames(X)
        if (glob) logit_cov = "g"
        else logit_cov = "m"
    }else logit_cov = "m" 
# adjust S for missing data
    miss = any(is.na(S))
	ns = nrow(S)
	if(miss) {
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
    if (miss){
        R = 1*(!is.na(S))
        S[is.na(S)] = 0
    }
    lv = apply(S,2,max) + 1
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
            XXdis = array(0, c(k - 1, k - 1 + ncov, Xndis))
            for (i in 1:Xndis) XXdis[, , i] = cbind(diag(k - 1), rep(1, k - 1) %o% Xdis[i, ])
        }else{
            XXdis = array(0, c(k - 1, (k - 1) * (ncov + 1),Xndis))
            if (k == 2) II = 1
            else II = diag(k - 1)
            for (i in 1:Xndis) XXdis[, , i] = II %x% t(c(1,Xdis[i, ]))
        }
    }else{
        ncov = 0
        XXdis = array(diag(k - 1), c(k - 1, k - 1, 1))
        Xlabel = rep(1, ns)
    }
# design matrices for the responses
	if(link=="global") ltype = "g"
	else if (link == "local") ltype = "l"
	items = sort(as.vector(multi)) # items depending on one of the abilities
	if(any(items == 0)) items = items[items > 0]
	Jitems = length(items)
	rm = nrow(multi)
# index of the discriminant parameters that are constrained
	fv = multi[, 1]
# index of the difficulty parameters that are constrained
	fve = NULL
	count = 0
	for(j in 1:J){
		if (j %in% fv) fve = c(fve, count + 1)
		count = count + lv[j] - 1
	}
# index of free discriminant prameters
	indgat = setdiff(items,fv) # index of gat in gac
# index of free ability parameters
	if(constr.th){
		if(ncol(Zth)==0) indtht = NULL     # index of tht in part
		else indtht = 1:ncol(Zth)
	}else indtht = 1:(k*rm)
	indth = 1:(k*rm)   # index of th in par
# index of free difficulty parameters
	if(constr.be){
		if(ncol(Zbe)==0) indbet = NULL
		else{
			if(is.null(indth)) tmp = 0
			else tmp = max(indth)
			indbet = tmp+(1:ncol(Zbe))     # index of bet in part
		}
	}else{
		if(is.null(indth)) tmp = 0
		else tmp = max(indth)
		if(difl) indbet = tmp+(1:(J-rm+lm-2))
		else indbet = tmp+1:(sum(lv - 1)-rm)  #FB
    }
    indbe = k*rm + (1:sum(lv - 1))          # index of be in par
    abils = rep(0, J)
    if (rm == 1) abils[multi] = 1
	else{
		for(h in 1:rm){
			ind = multi[h, ]
			ind = ind[ind > 0]
			abils[ind] = h
		}
	}
# design matrix for logit model
	ZZ = array(NA, c(lm - 1, k * rm + sum(lv - 1), J * k))
	cont = 0
	refitem = matrix(0, J * k, 1)
	for (c in 1:k){
		u1 = matrix(0, 1, k)
		u1[c] = 1
		for(j in 1:J){
			u2 = matrix(0, 1, rm)
			u2[abils[j]] = 1
			v = matrix(0, 1, J)
			v[j] = 1
			cont = cont + 1
			Te = matrix(0, lv[j] - 1, sum(lv - 1))
			if (j == 1) Te[, 1:(lv[j] - 1)] = diag(lv[j] - 1)
			else Te[, sum(lv[1:(j - 1)] - 1) + (1:(lv[j] - 1))] = diag(lv[j] - 1)
			ZZ[1:(lv[j] - 1), , cont] = cbind(rep(1, lv[j] - 1) %*% (u1 %x% u2), -Te)
			refitem[cont] = j
		}
	}
	ZZ0 = ZZ
# matrix of constraints for glob	
	if (glob) {
		II = diag(k - 1)
		II = cbind(0, II) - cbind(II, 0)
		if (rm > 1) II = II %x% matrix(c(1, rep(0, rm - 1)), 1, rm)
		Dis = cbind(II, matrix(0, k - 1, dim(ZZ)[2] - (k * rm)))
	}else Dis = NULL
# design matrices for theta, beta and gamma	
	if(!constr.th){
		Zth = diag(k*rm); zth = rep(0,k*rm)
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
    Zpar = blkdiag(Zth,Zbe)
    zpar = c(zth,zbe)
	if(!constr.ga){
		Zga = diag(Jitems)[,-which(items%in%fv)]
		zga = rep(0,Jitems); zga[which(items%in%fv)] = 1
	}
# starting values	    
    if(start=="deterministic") {
        if(cov){
            de = NULL
            Piv = matrix(1/k, ns, k)
            piv = NULL
        }else{
            de = NULL
            piv = rep(1, k)/k
        }
        if (k == 1) grid = 0
        else grid = seq(-k, k, length.out = k)
        Phi = array(NA, c(lm, J, k))
        for (j in 1:J) {
            dist = rep(0, lv[j])
            for (y in 0:(lv[j] - 1)) dist[y + 1] = (sum(yv[S[,j] == y]) + 0.5)/n
            out = matr_glob(lv[j])
            Co = out$Co
            Ma = out$Ma
            eta = Co %*% log(Ma %*% dist)
            count = 0
            for (c in 1:k){
                count = count + 1
                Phi[1:lv[j], j, count] = inv_glob(eta + grid[c])$p
            }
        }
    }
    if(start=="random") {
        if (cov) {
            if (glob){
                de = NULL
                Piv = matrix(runif(ns * k), ns, k)
                Piv = Piv * (1/rowSums(Piv))
                piv = NULL
            }else {
                de = rnorm((k - 1) * (ncov + 1))/rep(c(1, apply(X, 2, sd)), (k - 1))
                if (k > 1) Piv = prob_multi_glob(XXdis, logit_cov, de, Xlabel)$P
                piv = NULL
            }
        }else {
        	de = NULL
            piv = runif(k)
            piv = piv/sum(piv)
        }
        Phi = array(runif(lm * J * k), c(lm, J, k))
        for (c in 1:k) for (j in 1:J) {
            Phi[1:lv[j], j, c] = Phi[1:lv[j], j, c]/sum(Phi[1:lv[j], j, c])
            if (lv[j] < lm) Phi[(lv[j] + 1):lm, j, c] = NA
        }
		if(glob){
			if(runif(1) > 0.5) for (j in 1:J){
				mPhi = (0:(lv[j] - 1)) %*% Phi[1:lv[j], j,]
				ind = order(mPhi)
				Phi[, j, ] = Phi[, j, ind]
			}
		}
	}
	if(start=="external"){
		de = as.vector(De)
		if(cov){
			Piv = prob_multi_glob(XXdis, logit_cov, de, Xlabel)$P
			piv = NULL
		}else{
			piv = c(1,exp(de))
			piv = piv/sum(piv)
		}
	}
	if (start=="deterministic" || start=="random" || (start=="external" & is.null(gat))){
		if(is.null(Zga)) gat = rep(1,0)
		else gat = rep(1, ncol(Zga))
	}
	ga = Zga%*%gat+zga
	gac = rep(0,J); gac[items] = ga
# compute log-likelihood		   	
   	Psi = matrix(1, ns, k)
	if(miss) for (j in 1:J) for (c in 1:k) Psi[, c] = Psi[, c] * (Phi[S[, j] + 1, j, c] * R[, j] + (1 - R[, j]))
	else for (j in 1:J) for (c in 1:k) Psi[, c] = Psi[,c] * Phi[S[, j] + 1, j, c]
	if(cov){
		if (start=="external") {
			if (k > 1) Piv = prob_multi_glob(XXdis, logit_cov, de, Xlabel)$P
		}
	}else Piv = rep(1, ns) %o% piv
	if (k == 1) Pj = Psi
	else Pj = Psi * Piv
	pm = rowSums(Pj)
	lk = sum(yv * log(pm))
# iterate until convergence	
	cat("*-------------------------------------------------------------------------------*\n")
	cat(c("Model with multidimensional structure\n"))
	names1 = NULL
	for (j in 1:rm) names1 = c(names1, paste("Dimension", j))
	rownames(multi) = names1
	print(multi)
	cat("\n")
	cat(c("Number of classes =            ", k, "\n"))	
	cat(c("Link of type =                 ", link, "\n"))
	cat(c("Discrimination index =         ", disc, "\n"))
	cat(c("Constraints on the difficulty =", difl, "\n"))
	cat(c("Type of initialization =       ", start, "\n"))
	cat("*-------------------------------------------------------------------------------*\n\n")
	if (disp) {
		if(!disc || length(ga) == 0){
			cat("------------|-------------|-------------|-------------|-------------|-------------|\n")
			cat("  iteration |      lk     |    lk-lko   |     dis     |   min(par)  |   max(par)  |\n")
			cat("------------|-------------|-------------|-------------|-------------|-------------|\n")
		}
		if (disc) {
			cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|\n")
			cat("  iteration |      lk     |    lk-lko   |      dis    |   min(ga)   |   max(ga)   |   min(par)  |   max(par)  |\n")
			cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|\n")
		}
		cat(sprintf("%11g", c(0, lk)), "\n", sep = " | ")
	}
	it = 0
	lko = lk - 10^10
	dis = 0
	part = par = dga = lkv = NULL
	while (((abs(lk - lko)/abs(lko) > tol) && it < maxitc) || it < 2){
		it = it + 1
		paro = par
		gao = ga
		pivo = piv
		deo = de
		lko = lk
# E-step		
		V = ((yv/pm) %o% rep(1, k)) * Piv * Psi
		sV = colSums(V)
		YY = matrix(NA, J * k, lm)
		count = 0
		for (c in 1:k) for (j in 1:J) {
			count = count + 1
			for (y in 1:lv[j]) {
				ind = (S[, j] == (y - 1))
				if (miss)
				YY[count, y] = sum(V[ind, c] * R[ind, j])
				else YY[count, y] = sum(V[ind, c])
			}
		}
# M-step
		if(disc){
			if(it > 1 & rm < J){
				ZZ = array(NA, c(lm - 1, J, J * k))
				count = 0
				for(c in 1:k) for(j in 1:J){
					count = count + 1
					ZZ[1:(lv[j] - 1), , count] = 0
					ZZ[1:(lv[j] - 1), j, count] = ZZ0[1:(lv[j] - 1), 1:(k * rm), count] %*% par[1:(k*rm)]
				}
				ZZ = ZZ[, items, ]
				ind = k * rm + 1:sum(lv - 1)
				ZZInt = array(NA, c(lm - 1, J * k))
				count = 0
				for (c in 1:k) for (j in 1:J) {
					count = count + 1
					ZZInt[1:(lv[j] - 1), count] = ZZ0[1:(lv[j] - 1), ind, count] %*% par[ind]
					ZZ0[1:(lv[j] - 1), ind, count] %*% par[ind]
				}
# update ga				
				if(constr.ga || constr.th){
					gat = est_multi_glob_genZ(YY, ZZ, ltype, de = gat, Int = ZZInt,Z=Zga,z=zga)$de
					ga = Zga%*%gat+zga; gac = rep(1,J); gac[items] = ga
				}else{
					gac[items] = est_multi_glob_genZ(YY,ZZ, ltype, de = gac[items], Int = ZZInt)$be
					for(j in 1:rm){
						tmp = gac[multi[j,1]]
						gac[multi[j,]] = gac[multi[j,]]/tmp
						# ind = (j-1)*rm+seq(j,k*rm,rm)  # FB
						ind = seq(j,k*rm,rm)
						part[ind] = part[ind]*tmp
					}
					ga = gac[items]; gat = gac[indgat]
				}
				par = Zpar%*%part+zpar
			}
			ZZ = ZZ0
			for (j in 1:J) {
				ind = (refitem == j)
				ZZ[, 1:(k * rm), ind] = ZZ[, 1:(k * rm), ind] * gac[j]
			}
		}
# update part	
		if(start=="external" & it==1) maxit = 250 else maxit = 10
		out = est_multi_glob_genZ(YY,ZZ,ltype,de=part,Z=Zpar,z=zpar,Dis=Dis,maxit=maxit)
		part = out$de; par = Zpar%*%part+zpar
		P = out$P
		Phi = array(t(P), c(lm, J, k))
# updat de
		if (cov) {
			if (k > 1) {
				out = est_multi_glob(V, XXdis, logit_cov, Xlabel, de)
				de = out$be
				Pdis = out$Pdis
				Piv = out$P
			}
		}else {
			piv = sV/n
			Piv = rep(1, ns) %o% piv
		}
# compute log-likelihood		
		Psi = matrix(1, ns, k)
		if (miss) for (j in 1:J) for (c in 1:k) Psi[, c] = Psi[,c] * (Phi[S[, j] + 1, j, c] * R[, j] + (1 - R[,j]))
        	else for (j in 1:J) for (c in 1:k) Psi[, c] = Psi[,c] * Phi[S[, j] + 1, j, c]
		if (k == 1) Pj = Psi
		else Pj = Psi * Piv
		pm = rowSums(Pj)
		lk = sum(yv * log(pm))
		if (it > 1) dis = max(c(abs(par - paro), abs(ga - ga), abs(piv - pivo)))
		if (disp) {
			if (it/10 == floor(it/10)) {
				if (!disc || length(ga) == 0 ) cat(sprintf("%11g", c(it, lk, lk - lko, dis, min(par), max(par))), "\n", sep = " | ")
				if (disc) cat(sprintf("%11g", c(it, lk, lk - lko, dis, min(ga), max(ga), min(par), max(par))), "\n", sep = " | ")
			}
		}
		lkv = c(lkv, lk)
	}
	if (disp) {
		if (it/10 > floor(it/10)) {
			if (!disc || length(ga) == 0) cat(sprintf("%11g", c(it, lk, lk - lko, dis, min(par), max(par))), "\n", sep = " | ")
			if (disc) cat(sprintf("%11g", c(it, lk, lk - lko, dis, min(ga), max(ga), min(par), max(par))), "\n", sep = " | ")
        }
		if (!disc || length(ga) == 0) cat("------------|-------------|-------------|-------------|-------------|-------------|\n")
		if (disc) cat("------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|\n")
	}
# final output
	np = length(part)
	if (disc==1) np=np+length(gat)
	if (cov) {
		if (glob) np = np + k - 1 + ncov
		else np = np + (k-1) * (ncov + 1)
	}else np = np + k - 1
	aic = -2 * lk + 2 * np
	bic = -2 * lk + np * log(n)
	tht = part[indtht]
	th = par[indth]
	bet = part[indbet]	
	be = par[indbe]
	if (difl){
		bec1 = rep(0, J)
		bec1[(1:J)[-fv]] = bet[1:(J - rm)]
		bec2 = rep(0, lm - 1)
		bec2[2:(lm - 1)] = bet[J - rm + (1:(lm - 2))]
		Bec = list(difficulties = bec1, cutoffs = bec2)
	}else{
		Bec = matrix(NA, J, lm - 1)
		count = 0
		for (j in 1:J) {
			Bec[j, (1:lv[j] - 1)] = be[count + (1:(lv[j] - 1))]
			count = count + lv[j] - 1
		}
		dimnames(Bec) = list(item = 1:J, cutoff = 1:(lm-1))
	}
	gac[setdiff(1:J,items)] = NA
	Th = matrix(th, rm, k)
	dimnames(Th) = list(dimension = 1:rm,class=1:k)
	Pp = ((1/pm) %o% rep(1, k)) * Piv * Psi
	ent = -sum(V * log(pmax(Pp, 10^-100)))
	dimnames(Phi) = list(category = 0:(lm - 1), item = 1:J, class = 1:k)
	if (cov) {
		if (glob) {
			if (k == 1) De = NULL
			else {
				De = matrix(de, ncov + k - 1, 1)
				names_cutoff = paste("cutoff", 1:(k - 1), sep = "")
				if (is.null(namesX))  namesX1 = c(names_cutoff, paste("X", 1:ncov, sep = ""))
				else namesX1 = c(names_cutoff, namesX)
				rownames(De) = namesX
			}
		}else {
			if (is.null(namesX)) namesX = c("intercept", paste("X", 1:ncov, sep = ""))
			else namesX = c("intercept", namesX)
			if (k == 1) De = NULL
			else {
				De = matrix(de, ncov + 1, k - 1)
				dimnames(De) = list(namesX, logit = 2:k)
			}
		}
		piv = drop(t(Piv)%*%yv/n)
	}else{
		de = log(piv[-1]/piv[1])
		De = t(de); dimnames(De) = list("intercept",logit = 2:k)
	}
# compute standard errors
	if (out_se) {
		lde = length(de)
		lpar = length(par); lpart = length(part)
		lga = 0
		part_comp = c(de, part)
		if (disc) {
			lga = length(ga); lgat = length(gat)
			part_comp = c(part_comp, gat)
		}
		if (disp) {
			cat("computation of derivatives\n")
			cat(length(part_comp), "parameters\n")
		}
		out = lk_obs_score_between(part_comp, lde, lpart, lgat, S, R, yv, k, rm, lv, J, fv, disc, glob,
			refitem, miss, ltype, XXdis, Xlabel, ZZ0, fort, Zpar, zpar,Zga,zga,items)
		scn = rep(0, length(part_comp))
		Jn = NULL
		for (j in 1:length(part_comp)) {
			part_comp1 = part_comp
			part_comp1[j] = part_comp1[j] + 10^-6
			out1 = lk_obs_score_between(part_comp1, lde, lpart, lgat, S, R, yv, k, rm, lv, J, fv, disc, glob, 
				refitem, miss, ltype, XXdis, Xlabel, ZZ0, fort, Zpar, zpar, Zga,zga,items)
			scn[j] = (out1$lk - lk) * 10^6
			Jn = cbind(Jn, (out1$sc - out$sc) * 10^6)
			if (disp){
				if (j/10 > floor(j/10)) cat(".")
				else cat(round(j/10))
			}
			if (disp) if (j/100 == floor(j/100)) cat("\n")
		}
		if (disp) cat("\n")
		Jn = -(Jn + t(Jn))/2
		Vnt = ginv(Jn)
		ZZZ = blkdiag(diag(lde),Zpar)
		if(disc) ZZZ = blkdiag(ZZZ,Zga)
		Vn = ZZZ%*%Vnt%*%t(ZZZ)
		set = sqrt(abs(diag(Vnt)))
		se = sqrt(abs(diag(Vn)))
		if (k > 1) sede = se[1:lde]
		separt = se[lde + (1:lpart)]
		setht = separt[indtht]
		sebet = separt[indbet]
		separ = se[lde + (1:lpar)]
		if(disc){
			segat = set[lde + lpart + (1:lgat)]
			sega = se[lde + lpar + (1:lga)]
			segac = rep(NA, J); segac[items] = sega
		}else{
			segat = sega = segac = NULL
		}
		setht = separ[indtht]
		seth = separ[indth]
		seTh = matrix(seth, rm, k)
		dimnames(seTh) = list(dimension = 1:rm,class=1:k)
		sebe = separ[indbe]
		if (difl) {
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
			dimnames(seBec) = list(item = 1:J, cutoff = 1:(lm-1))
		}
		if (glob){
			if (k == 1) seDe = NULL
			else{
				if(cov){
					seDe = matrix(sede, ncov + k - 1, 1)
					rownames(seDe) = namesX
				}else seDe = sede
			}
		}else{
			if (k == 1) seDe = NULL
			else{
				if(cov){
					seDe = matrix(sede, ncov + 1, k - 1)
					rownames(seDe) = namesX
				}else seDe = sede
			}
		}
	}
# order and standardize
	mu = drop(Th%*%piv)
	sd = drop(sqrt((Th-mu)^2%*%piv))
	ind = order(Th[1,])
	Ths = (Th[,ind]-mu)/sd
	pivs = piv[ind]
	Becs = Bec
	gacs = gac
	for(j in 1:rm){
		tmp = multi[j,]; tmp = tmp[tmp>0]
		gacs[tmp] = gac[tmp]*sd[j]
		if(difl) Becs$difficulties[tmp] = Becs$difficulties[tmp]-gac[tmp]*mu[j] 
		else Becs[tmp,] = Becs[tmp,]-gac[tmp]*mu[j]
	}
# output
	out = list(piv = piv, fv = fv, tht = tht, Th = Th, bet=bet, Bec = Bec, gat=gat, gac = gac, De = De,
		Phi = Phi, lk = lk, np = np, aic = aic, bic = bic, ent = ent, pivs=pivs, Ths=Ths, Becs=Becs,
		gacs = gacs, call = match.call())
	if (output) {
		out$Pp = Pp
		out$lkv = lkv
		if(cov){
			out$Xlabel = Xlabel
			out$XXdis = XXdis
			out$Piv = Piv
        }
	}
	if (out_se) {
		out$setht = setht
		out$seTh = seTh
		out$sebet = sebet
		out$seBec = seBec
		out$segat = segat
		out$segac = segac
		out$seDe = seDe
		out$Vnt = Vnt
		out$Vn = Vn
	}
	class(out) = "est_multi_poly_between"
	return(out)
	
}