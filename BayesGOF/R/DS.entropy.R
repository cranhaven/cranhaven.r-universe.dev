DS.entropy <- 
function(DS.GF.obj){
##########################
# INPUT: A DS object with LP.type = "L2"
# OUTPUT: ent: entropy
	out <- list()
	#Check for parametric
	if(sum(DS.GF.obj$LP.par^2) == 0){
		a <- min(DS.GF.obj$prior.fit$theta.vals)
		b <- max(DS.GF.obj$prior.fit$theta.vals)
		out$qLP <- 0
		out$ent <- sintegral(DS.GF.obj$prior.fit$theta.vals,
								(DS.GF.obj$prior.fit$parm.prior - 1/(b-a))^2)$int
		} else {
		#Check for Max Entropy
		if(DS.GF.obj$LP.type == "L2"){out$qLP <- sum(DS.GF.obj$LP.par^2)}
		#Check for Normal; want to adjust domain such that it fits DS not parametric
		if(DS.GF.obj$fam == "Normal"){
			dens.diff <- abs(diff(DS.GF.obj$prior.fit$ds.prior))
			diff.ind <- which(dens.diff > 0)
			a.ind <- max(1, min(diff.ind[-c(1,length(diff.ind))])-1)
			b.ind <- min(length(DS.GF.obj$prior.fit$theta.vals), 
						 max(diff.ind[-c(1,length(diff.ind))])+1)
			a <- DS.GF.obj$prior.fit$theta.vals[a.ind]
			b <- DS.GF.obj$prior.fit$theta.vals[b.ind]		
			} else {
				a <- min(DS.GF.obj$prior.fit$theta.vals)
				b <- max(DS.GF.obj$prior.fit$theta.vals)
			}
		out$ent <- sintegral(DS.GF.obj$prior.fit$theta.vals,
							(DS.GF.obj$prior.fit$ds.prior - 1/(b-a))^2)$int
			}
				
	names(out$ent) <- NULL
	return(out)
	}