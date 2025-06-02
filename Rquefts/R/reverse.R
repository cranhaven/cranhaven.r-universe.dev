
#REVERSE QUEFTS. 
#Estimate the apparent soil nutrient supply based on a set of observed crop responses

.qmo <- function(supply, obs, qmod){    
	qmod$soil$N_base_supply <- supply[1]
	qmod$soil$P_base_supply <- supply[2]
	qmod$soil$K_base_supply <- supply[3]    
	Yq <- sapply(1:nrow(obs), function(i) {
			qmod$N <- obs$N[i] 
			qmod$P <- obs$P[i]
			qmod$K <- obs$K[i]
			run(qmod)["store_lim"] })
    # sum of squared errors (SSE) (computed and observed yield)
    sum((Yq - obs$Y)^2)    
}


revSupply <- function(obs, crop, soil, Ya, leaf_ratio, stem_ratio, SeasonLength=120, ...){
  # minimise SSE to get soil NPK supply
	obs <- obs[, c("N", "P", "K", "Y")]
	init <- c(soil$N_base_supply, soil$P_base_supply, soil$K_base_supply)
	qYa <- list(leaf_att  = Ya * leaf_ratio, 
              stem_att  = Ya * stem_ratio, 
              store_att = Ya, SeasonLength=SeasonLength)  
    qmod <- quefts(soil, crop, list(N=0,K=0,P=0), qYa)
	stats::optim(init, .qmo, obs=obs, qmod=qmod, ...)$par
}

