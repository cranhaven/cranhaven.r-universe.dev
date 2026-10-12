.addNode <-
function(ID,parentID,data=NULL,ccstat=NULL){
		# add an internal node
	
	    tempenv <- parent.env(environment())
	    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
		ind <- length(tempenv$private$IDs) + 1
		parent <- match(parentID,tempenv$private$IDs)
		if (is.na(parent)){
			stop("Error: parent not found")
			}
		tempenv$private$IDs[ind] <- ID
		tempenv$private$ps[ind] <- parent
		tempenv$private$os[[parent]] <- c(tempenv$private$os[[parent]],ind)
		tempenv$private$os[[ind]] <- vector("numeric",length=0)
		tempenv$private$data[[ind]] <- data
		if (!is.null(ccstat)) tempenv$private$Naff <- tempenv$private$Naff + ccstat
	}

.addPed <-
function(ped,ORs){
		#read a pedigree/odds ratio file into a Mangrove tree
		#print("start")

	    tempenv <- parent.env(environment())
	    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")

		tempenv$private$Naff <- sum(ped[,6] == 2)
	
		# extract risk allele counts
		SNPnames <- paste(rep(ORs[,1],rep(2,length(ORs[,1]))),rep(c(".1",".2"),length(ORs[,1])),sep="")
		RiskAlleles <- rep(ORs[,2],rep(2,length(ORs[,2])))
		temp <- t(apply(ped[,SNPnames],1,function(x) x == RiskAlleles))
		temp[ped[,SNPnames] == 0] <- NA
		RAcounts <- sapply(ORs[,1],function(x) temp[,paste(x,".1",sep="")] + temp[,paste(x,".2",sep="")])
		
		#print("1")
		
		# extract founders, couples and leaves
		seen_founders <- ped[ped[,3] == 0 & ped[,4] == 0,2]
		couples <- unique(ped[!ped[,2] %in% seen_founders,3:4],MARGIN=1)
		coup_f1 <- couples[,1] %in% seen_founders
		coup_f2 <- couples[,2] %in% seen_founders
		leaves <- ped[,2][!(ped[,2] %in% c(couples[,1],couples[,2]))]
		#print("2")
	
		# so some error checking
		if (any(!coup_f1 & !coup_f2)) stop("Error: Cannot handle non-founder non-founder matings")
		if (sum(coup_f1 & coup_f2) > 1) stop("Error: Cannot handle multiple founder-founder matings")
		if (sum(coup_f1 & coup_f2) > 1) stop("Error: Cannot handle multiple founder-founder matings")
		if (any(duplicated(couples[,1])) | any(duplicated(couples[,2]))) stop("Error: cannot handle half-siblings")

		# find which couple members are founders and which are not, and generate couple IDs
		coup_founders <- sapply(1:length(coup_f1),function(i )paste(couples[i,2 - coup_f1[i]]))
		coup_nonfounders <- sapply(1:length(coup_f1),function(i )paste(couples[i,2 - !coup_f1[i]]))
		coup_ids <- paste(coup_nonfounders,coup_founders,sep="-")

		#print("3")

		# find parent for individuals and couples
		parentage <- apply(ped,1,function(x) coup_ids[x[3] == couples[,1] & x[4] == couples[,2]])
		parentage <- sapply(parentage,function(x) ifelse(length(x) == 0,NA,x))
		coup_parentage <- parentage[coup_nonfounders]
		
		# add founders to tree
		tempenv$public$addRoot(coup_ids[coup_f1 & coup_f2],RAcounts[as.character(couples[coup_f1 & coup_f2,]),])
	
		#print("4")
	
		# add parent couple nodes
		toAdd <- which(coup_parentage %in% tempenv$private$IDs & !(coup_ids %in% tempenv$private$IDs))
		while (length(toAdd) > 0){
			sapply(toAdd,function(i) tempenv$public$addNode(coup_ids[i],coup_parentage[i], RAcounts[c(coup_nonfounders[i]	,coup_founders[i]),]))
			toAdd <- which(coup_parentage %in% tempenv$private$IDs & !(coup_ids %in% tempenv$private$IDs))
			}
	
		# add leaves
		toAdd <- which(parentage %in% tempenv$private$IDs & !(names(parentage) %in% tempenv$private$IDs) & !(names(parentage) %in% c(couples[,1],couples[,2])))
		while (length(toAdd) > 0){
		
			sapply(toAdd,function(i) tempenv$public$addNode(names(parentage)[i],parentage[i],RAcounts[i,]))
			toAdd <- which(parentage %in% tempenv$private$IDs & !(names(parentage) %in% tempenv$private$IDs) & !(names(parentage) %in% c(couples[,1],couples[,2])))
		
			}
}

.addRoot <-
function(ID,data=NULL,ccstat=NULL){
		#add a root node
		
	    tempenv <- parent.env(environment())
	    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
		
		
		ind <- length(tempenv$private$IDs) + 1
		tempenv$private$ps[ind] <- NA
		tempenv$private$IDs[ind] <- ID
		tempenv$private$os[[ind]] <-  vector("numeric",length=0)
		tempenv$private$data[[ind]] <- data
		if (!is.null(ccstat)) tempenv$private$Naff <- tempenv$private$Naff + ccstat
}

.calcAlpha <-
function(ORs,i=1,progress=FALSE){
	# calculate inside probabilities
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	if (progress & i %% floor(length(tempenv$private$IDs)/5) == 0){
		
		cat('.')
		
	}
	
	freqs <- ORs[,5]

	# if the node is a leaf, alpha = L
	if (length(tempenv$private$os[[i]]) == 0){
		tempenv$private$alpha[[i]] <- tempenv$private$Ls[[i]]
	# if the node is not a leaf
	} else {
		for (j in tempenv$private$os[[i]]) tempenv$public$calcAlpha(ORs,j, progress=progress)
		
		tempalpha <- tempenv$private$Ls[[i]]
		hw_freqs <- cbind((1 - freqs)^2,2*freqs*(1 - freqs),freqs^2)

		for (a in 0:2){
			for (b in 0:2){
				Mtrans <- sapply(0:2,function(c) .getTrans(c(a,b),c))

				for (child in tempenv$private$os[[i]]){

					if (length(tempenv$private$os[[child]]) == 0){
						temp <- apply(Mtrans* tempenv$private$alpha[[child]],2,sum)
						tempalpha[a+1,b+1,] <- tempalpha[a+1,b+1,] * temp
					} else {
						temp <- sapply(1:length(hw_freqs[,1]),function(k) sum(Mtrans %*% t(hw_freqs[k,]) * tempenv$private$alpha[[child]][,,k]))
						tempalpha[a+1,b+1,] <- tempalpha[a+1,b+1,] * temp
					}
			
				}
		
			}
		}
	
		tempenv$private$alpha[[i]] <- tempalpha
	}
}

.calcBeta <-
function(ORs,i=1,progress=FALSE){
	#calculate inside probabilities, given frequencies in OR object
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	
	if (progress & i %% floor(length(tempenv$private$IDs)/5) == 0){
		
		cat('.')
		
		}
	
	freqs <- ORs[,5]
	
	tempbeta <- tempenv$private$Ls[[i]]*0
	hw_freqs <- cbind((1 - freqs)^2,2*freqs*(1 - freqs),freqs^2)
			
	parent <- tempenv$private$ps[i]
		
	
	# deal with the root
	if (i == 1){

		for (a in 0:2){
			for (b in 0:2){
				
					tempbeta[a+1,b+1,] <- hw_freqs[,a+1]*hw_freqs[,b+1]
				
				}
			}
		
	# then with internal nodes
	} else if (length(tempenv$private$os[[i]]) > 0){
		
		tempenv$private$betap[[i]] <- array(dim=c(3,3,3,3,length(hw_freqs[,1])))

		for (a in 0:2){
			for (b in 0:2){

				tempbeta_ab <- 0
				
				for (c in 0:2){
					for (d in 0:2){
						
						tempbeta_abcd <- tempenv$private$beta[[parent]][c+1,d+1,]*tempenv$private$Ls[[parent]][c+1,d+1,]*.getTrans(c(c,d),a) * hw_freqs[,b+1]
						Mtrans <- sapply(0:2,function(e) .getTrans(c(c,d),e))
						
						for (sib in setdiff(tempenv$private$os[[parent]],i)){
						
							if (length(tempenv$private$os[[sib]]) == 0){
								temp <- apply(Mtrans*tempenv$private$alpha[[sib]],2,sum)
								tempbeta_abcd <- tempbeta_abcd * temp
							} else {
								temp <- sapply(1:length(hw_freqs[,1]),function(k) sum(Mtrans %*% t(hw_freqs[k,]) * tempenv$private$alpha[[sib]][,,k]))
								tempbeta_abcd <- tempbeta_abcd * temp
							}
							
						}

						tempenv$private$betap[[i]][a+1,b+1,c+1,d+1,] <- tempbeta_abcd				
						tempbeta_ab <- tempbeta_ab + tempbeta_abcd 
						
					}
				}
				
				tempbeta[a+1,b+1,] <- tempbeta_ab
				
			}
		}
		
	# then with the leaves
	} else {
		
		tempenv$private$betap[[i]] <- array(dim=c(3,3,3,length(hw_freqs[,1])))
		
		for (a in 0:2){
			
			tempbeta_ab <- 0
				
			for (c in 0:2){
				for (d in 0:2){
					
					tempbeta_abcd <- tempenv$private$beta[[parent]][c+1,d+1,]*tempenv$private$Ls[[parent]][c+1,d+1,]*.getTrans(c(c,d),a)
					Mtrans <- sapply(0:2,function(e) .getTrans(c(c,d),e))
					
					for (sib in setdiff(tempenv$private$os[[parent]],i)){
					
						if (length(tempenv$private$os[[sib]]) == 0){
							temp <- apply(Mtrans*tempenv$private$alpha[[sib]],2,sum)
							tempbeta_abcd <- tempbeta_abcd * temp
						} else {
							temp <- sapply(1:length(hw_freqs[,1]),function(k) sum(Mtrans %*% t(hw_freqs[k,]) * tempenv$private$alpha[[sib]][,,k]))
							tempbeta_abcd <- tempbeta_abcd * temp
						}
							
					}
					tempenv$private$betap[[i]][a+1,c+1,d+1,] <- tempbeta_abcd
					tempbeta_ab <- tempbeta_ab + tempbeta_abcd 
						
				}
			}
			
			tempbeta[a+1,] <- tempbeta_ab
				
		}

		
		
	}
	
	tempenv$private$beta[[i]] <- tempbeta
	
	if (length(tempenv$private$os[[i]]) > 0){
		
		for (i in tempenv$private$os[[i]]){
			tempenv$public$calcBeta(ORs,i,progress)
		}
		
	}
	
}

.calcLikelihoods <-
function(data){
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	# calculates the likelihoods given genotype data private$data
	tempenv$private$Ls <- lapply(tempenv$private$data,.getLikelihood)
		
	}

.calcParams <-
function(ORs,progress=TRUE){
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	cat("Running Inside-Outside Algorithm\n")
	cat("Calculating likelihoods.....")	
	tempenv$public$calcLikelihoods()
	cat("done\nCalculating inside parameters")
	tempenv$public$calcAlpha(ORs,progress=progress)
	cat("done\nCalculating outside parameters")
	tempenv$public$calcBeta(ORs,progress=progress)
	cat("done\nCalculating posteriors.....")
	tempenv$public$calcPost()
	cat("done\n")
	}

.calcPost <-
function(i=1){

    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	

	if (i == 1){

		unnorm <- tempenv$private$beta[[i]]*tempenv$private$alpha[[i]]
		Pds <- apply(unnorm,3,sum)
		tempenv$private$post[[i]] <- unnorm/rep(Pds,rep(9,length(Pds)))
	
	} else {
		# calculate and normalize conditional posteriors
		tempenv$private$condpost[[i]] <- tempenv$private$betap[[i]]*0
		
		if (length(tempenv$private$os[[i]]) == 0){
			
			unnorm <- tempenv$private$beta[[i]]*tempenv$private$alpha[[i]]
			Pds <- apply(unnorm,2,sum)
			tempenv$private$post[[i]] <- unnorm/rep(Pds,rep(3,length(Pds)))
			
			for (c in 0:2){	
				for (d in 0:2){				
					unnorm <- tempenv$private$betap[[i]][,c+1,d+1,]*tempenv$private$alpha[[i]]
					Pds <- apply(unnorm,2,sum)
					tempenv$private$condpost[[i]][,c+1,d+1,] <- unnorm/rep(Pds,rep(3,length(Pds)))
				}
			}
		} else {
			
			unnorm <- tempenv$private$beta[[i]]*tempenv$private$alpha[[i]]
			Pds <- apply(unnorm,3,sum)
			tempenv$private$post[[i]] <- unnorm/rep(Pds,rep(9,length(Pds)))
			
			for (c in 0:2){
				for (d in 0:2){
					unnorm <- tempenv$private$betap[[i]][,,c+1,d+1,]* tempenv$private$alpha[[i]]
					Pds <- apply(unnorm,3,sum)
					tempenv$private$condpost[[i]][,,c+1,d+1,] <- unnorm/rep(Pds,rep(9,length(Pds)))
				}
			
			
			}
		
		}
	}
	
	for (child in tempenv$private$os[[i]])	tempenv$public$calcPost(child)

}

.getCoupleLikelihood <-
function(a){
	
	# calculates the likelihood for an internal node given genotypes a	
	out <- array(dim=c(3,3,length(a[1,])))
	
	L0_nf <- (a[1,] == 0)
	L0_nf[is.na(a[1,])] <- 1
	L1_nf <- (a[1,] == 1)
	L1_nf[is.na(a[1,])] <- 1
	L2_nf <- (a[1,] == 2)
	L2_nf[is.na(a[1,])] <- 1

	L0_f <- (a[2,] == 0)
	L0_f[is.na(a[2,])] <- 1
	L1_f <- (a[2,] == 1)
	L1_f[is.na(a[2,])] <- 1
	L2_f <- (a[2,] == 2)
	L2_f[is.na(a[2,])] <- 1
	
	#00
	out[1,1,] <- L0_nf * L0_f
	#01
	out[1,2,] <- L0_nf * L1_f
	#02
	out[1,3,] <- L0_nf * L2_f
	#10
	out[2,1,] <- L1_nf * L0_f
	#11
	out[2,2,] <- L1_nf * L1_f
	#12
	out[2,3,] <- L1_nf * L2_f
	#20
	out[3,1,] <- L2_nf * L0_f
	#21
	out[3,2,] <- L2_nf * L1_f
	#22
	out[3,3,] <- L2_nf * L2_f
	
	return(out)
}

.getData <-
function(){
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
		# returns the private data
		return(tempenv$private)
	}

.getLikelihood <-
function(a){
	# gets the likelihood for any node given genotype a
	if (is.array(a)){
		return(.getCoupleLikelihood(a))
	} else {
		return(.getSingleLikelihood(a))
	}
}

.getMaxDepth <-
function(ID=NULL,depth=0){
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
		#returns the maximum depth of the tree
		temp <- depth
		if (tempenv$public$getNNodes() == 0) return(0)		
		if (is.null(ID)){
			
			for (x in tempenv$public$getRoot()){
				temp <- c(temp,tempenv$public$getMaxDepth(x,depth=depth+1))
			}
			
		} else {
			#print(c(ID,depth))

			for (x in tempenv$public$getOffspring(ID)){
				temp <- c(temp,tempenv$public$getMaxDepth(x,depth=depth+1))
			}
			
		}

		return(max(temp))
	}

.getNinds <-
function(){
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	return(sum(sapply(tempenv$private$os,length) != 0)*2 + sum(sapply(tempenv$private$os,length) == 0))
	}

.getNNodes <-
function(){
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	return(length(tempenv$private$IDs))
	}

.getOffspring <-
function(ID){
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
		# returns offspring of a node
		tempenv$private$IDs[tempenv$private$os[[which(tempenv$private$IDs == ID)]]]
	}

.getPrevs <-
function(ORs = NULL,K = NULL,overwrite=FALSE,iter=1000){
	# fit models and sample variants and cases, and return samples of number of cases
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	
	progress=TRUE
	if (tempenv$public$getNNodes() < 5) progress=FALSE
	if (tempenv$public$getNNodes() == 0) stop("Error: Cannot perform sampling on an empty tree")
	if (!is.null(K)) tempenv$private$K <- K
	if (overwrite | length(tempenv$private$Ls) == 0 | length(tempenv$private$alpha) == 0 | length(tempenv$private$beta) == 0 | length(tempenv$private$betap) == 0){
		if (is.null(ORs)) stop('Error: Need to provide OR object to fit model parameters')
		tempenv$public$calcParams(ORs,progress)
	}

	if (overwrite | length(tempenv$private$simVars) == 0){
		cat("Sampling variants from posterior")
		tempenv$public$sampleVariants(iter= iter,report=T)	
		cat('done\n')
	}
	
	if (overwrite | length(tempenv$private$simCases) == 0){
		if (is.null(ORs)) stop('Error: Need to provide odds ratio object ORs to sample cases')
		if (is.null(K)) stop('Error: Need to provide prevalence value K to sample cases')
		cat('Sampling cases given sampled variants')
		tempenv$public$sampleCases(ORs,K,report=T,iter=iter)
		cat('done\n')
	}	
	
	output <- list()
	output$samples <- tempenv$private$simCases
	output$K <- tempenv$private$K
	output$Ncases <- tempenv$private$Naff
	output$N <- tempenv$public$getNinds()
	class(output) <- "MangroveSample"
	return(output)
}

.getRoot <-
function(){
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
		# returns IDs of founders
		tempenv$private$IDs[which(is.na(tempenv$private$ps))]
	}

.getSingleLikelihood <-
function(a){
	# calculates the likelihood for a leaf node given genotype a	
	L0 <- (a == 0)
	L0[is.na(a)] <- 1
	L1 <- (a == 1)
	L1[is.na(a)] <- 1
	L2 <- (a == 2)
	L2[is.na(a)] <- 1
	
	return(rbind(L0,L1,L2))
	}

.getTrans <-
function(a,b,f=NULL){
	mendT <- array(NA,c(3,3,3))
	mendT[c(3,6,7,8,9,10,18,19,20,21,22,25)] <- 0
	mendT[c(5,23)] <- 0.25
	mendT[c(2,4,11,13,14,15,17,24,26)] <- 0.5
	mendT[c(1,12,16,27)] <- 1
	
	# calculate transmission probabilities between two nodes	
	if (length(b) > 1 & is.null(f))	stop("Error: Need to supply f for couple->couple trans")
	if (length(b) == 1 & !is.null(f)) stop("Error: f supplied but only one b state given")
	
	if (length(b) == 1) return(mendT[a[1]+1,a[2]+1,b+1])
	else return(mendT[a[1]+1,a[2]+1,b[1]+1]*dbinom(b[2],2,f))
	
}

.getVarExp_loci <-
function(K,OR1,OR2,f){
	
	meanor <- (1-f)^2 + 2*f*(1-f)*OR1 + f^2*OR2

	P_aa <- applyORs(1/meanor,K)
	P_Aa <- applyORs(OR1/meanor,K)
	P_AA <- applyORs(OR2/meanor,K)
	T <- qnorm(1 - K)
	
	mu_aa <- T - qnorm(1 - P_aa)
	mu_Aa <- T - qnorm(1 - P_Aa)
	mu_AA <- T - qnorm(1 - P_AA)

	Vstar <- mu_aa^2*(1-f)^2 + mu_Aa^2*2*f*(1-f) + mu_AA^2*f^2
	return(Vstar)

	}

.onLoad <-
function (libname, pkgname) 
{
    op <- options()
    op.utils <- list(help.try.all.packages = FALSE, internet.info = 2, 
        pkgType = .Platform$pkgType, str = list(strict.width = "no", 
            digits.d = 3, vec.len = 4), demo.ask = "default", 
        example.ask = "default", menu.graphics = TRUE, mailer = "mailto")
    extra <- if (.Platform$OS.type == "windows") {
        list(unzip = "internal", editor = if (length(grep("Rgui", 
            commandArgs(), TRUE))) "internal" else "notepad", 
            repos = c(CRAN = "@CRAN@", CRANextra = "http://www.stats.ox.ac.uk/pub/RWin"))
    }
    else list(unzip = Sys.getenv("R_UNZIPCMD"), editor = Sys.getenv("EDITOR"), 
        repos = c(CRAN = "@CRAN@"))
    op.utils <- c(op.utils, extra)
    toset <- !(names(op.utils) %in% names(op))
    if (any(toset)) 
        options(op.utils[toset])
}
.printSummary <-
function(){
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	#prints some data about the tree
	 
	cat("A mangrove tree\n")
	cat("Number of nodes: ")
	cat(tempenv$public$getNNodes())
	cat("\n")
	cat("Max depth: ")
	cat(tempenv$public$getMaxDepth())
	cat("\n")

	if (length(tempenv$private$data) > 0){
		cat("Genotype data: loaded\n")
	} else {
		cat("Genotype data: Not loaded\n")
	}
	
	if (length(tempenv$private$Ls) > 0 & length(tempenv$private$alpha) > 0 & length(tempenv$private$beta) > 0 & length(tempenv$private$betap)){
		cat("Model parameters: calculated\n")
	} else {
		cat("Model parameters: not calculated\n")
	}
	
	if (length(tempenv$private$simVars) > 0 | length(tempenv$private$simCases)){
		cat("Sampling: performed\n")
	} else {
		cat("Sampling: not performed\n")
	}
}

.printTree <-
function(ID=NULL,depth=0){
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	
		#prints the tree structure
		if (tempenv$public$getNNodes() == 0){
			 cat("The tree is empty\n")		
			return(0)
		}
		if (is.null(ID)){	
			tempenv$public$printTree(tempenv$public$getRoot(),depth=depth+1)
		
		} else {
			#print(c(ID,depth))
			cat(paste(rep("-",depth),collapse=""))
			cat(paste(ID,"\n",sep=""))
			for (x in tempenv$public$getOffspring(ID)){
				tempenv$public$printTree(x,depth=depth+1)
			}
			
		}
		
	}

.sampleCases <-
function(ORs,K,report=FALSE,iter=1000){
	# samples cases given sampled variants
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	
	mix2founder <- c(1,2,3,1,2,3,1,2,3)
	mix2nfounder <- c(1,1,1,2,2,2,3,3,3)

	N <- sum(sapply(tempenv$private$Ls,function(x) length(dim(x))-1))
	iters <- iter
	out <- matrix(NA,ncol=N,nrow=iters)

	pos <- 1

	meanOR <- prod(ORs[,6])

	for (node in 1:length(tempenv$private$IDs)){
	
		if (report & (node %% floor(N/5)) == 0) cat(".")
	
		if (length(dim(tempenv$private$Ls[[node]])) == 3){
			k1 <- apply(tempenv$private$simVars[,node,],1,function(i) prod((mix2founder[i] == 1) + (mix2founder[i] == 2)*ORs[,3] + (mix2founder[i] == 3)*ORs[,4]))
			k2 <- apply(tempenv$private$simVars[,node,],1,function(i) prod((mix2nfounder[i] == 1) + (mix2nfounder[i] == 2)*ORs[,3] + (mix2nfounder[i] == 3)*ORs[,4]))
	
			out[,pos] <- k1
			out[,pos + 1] <- k2
			pos <- pos + 2
		} else {
	
			k <- apply(tempenv$private$simVars[,node,],1,function(i) prod((i == 1) + (i == 2)*ORs[,3] + (i == 3)*ORs[,4]))
			out[,pos] <- k
			pos <- pos + 1
		}
	
	}

	preodds <- K/(1 - K)
	postodds <- preodds*out/meanOR
	post <- postodds/(1 + postodds)
	tempenv$private$simCases <- apply(post > runif(post),1,sum)
	
}

.sampleVariant <-
function(variant,i=1,p=NULL,parentsamples=NULL,iter=1000){
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	transM <- cbind(rep(0:2,3),rep(0:2,c(3,3,3)))

	parents_nz <- (1:9)[parentsamples != 0]
	parent_counts <- parentsamples[parentsamples != 0]
	
	if (i == 1) {
		if (all(is.na(as.vector(tempenv$private$post[[1]][,,variant])))){
			stop(paste("Impossible genetic structure for variant number ",variant,". Likely a Mendelian error.",sep=""))
		}
		outtemp <- rmultinom(1,iter,as.vector(tempenv$private$post[[1]][,,variant]))
		tempenv$private$simVars[,1,variant] <- rep(1:9, outtemp)
		
	} else if (length(tempenv$private$os[[i]]) == 0){

		for (x in 1:length(parents_nz)){
		if (all(is.na(as.vector(tempenv$private$post[[1]][,,variant])))){
			stop(paste("Impossible genetic structure for variant number ",variant," . Likely a Mendelian error.",sep=""))
		}
			temp_sample <- rmultinom(1,parent_counts[x], tempenv$private$condpost[[i]][,transM[parents_nz[x],1]+1,transM[parents_nz[x],2]+1,variant])
			temp_sample_data <- rep(1:3,temp_sample)
			if (length(temp_sample_data) > 1) tempenv$private$simVars[tempenv$private$simVars[,p,variant] == parents_nz[x],i,variant] <- sample(temp_sample_data)
			else tempenv$private$simVars[tempenv$private$simVars[,p,variant] == parents_nz[x],i,variant] <- temp_sample_data
		}
	
	} else {
	
		outtemp <- parentsamples*0
	
		for (x in 1:length(parents_nz)){
			temp_sample <- rmultinom(1:9,parent_counts[x],tempenv$private$condpost[[i]][,,transM[parents_nz[x],1]+1,transM[parents_nz[x],2]+1,variant])
			temp_sample_data <- rep(1:9,temp_sample)
			if (length(temp_sample_data) > 1) tempenv$private$simVars[tempenv$private$simVars[,p,variant] == parents_nz[x],i,variant] <- sample(temp_sample_data)
			else tempenv$private$simVars[tempenv$private$simVars[,p,variant] == parents_nz[x],i,variant] <- temp_sample_data
			outtemp <- outtemp + temp_sample
		}
	} 
	
	for (child in tempenv$private$os[[i]]) tempenv$public$sampleVariant(variant,child,i,outtemp,iter)
	
}

.sampleVariants <-
function(iter=1000,report=FALSE){
	
    tempenv <- parent.env(environment())
    if (identical(globalenv(), tempenv)) stop("Error: Attempted to write to global environment")
	
	tempenv$private$simVars <- array(dim=c(iter,length(tempenv$private$IDs),length(tempenv$private$Ls[[1]][1,1,])))

	for (i in 1:length(tempenv$private$Ls[[1]][1,1,])){
		if (report & i %% (max(floor(length(tempenv$private$Ls[[1]][1,1,])/5),1)) == 0) cat('.')
		tempenv$public$sampleVariant(i,iter=iter)	
		}
	}

