#The following functions are needed to perform Step 3 of SCALPEL

#######################################################################################
# FUNCTIONS FOR CALCULATING MAXIMUM LAMBDA TO CONSIDER
#######################################################################################

#function that calculates positive part of a vector
positivePart = function(vec) {
	return(apply(cbind(vec,0), 1, max))
}

#function that computes a sufficiently large lambda for given A, Y, and alpha
sglMaxLambda = function(A, Y, alpha) {
	AtY = crossprod(A, Y)
	lambdaSet = sapply(1:ncol(A), sglMaxLambdaVec, AtY=AtY, alpha=alpha)
	return(max(lambdaSet))
}

#function that computes a sufficiently large lambda for each neuron
#call this K times and take the max over the values
sglMaxLambdaVec = function(AtY, k, alpha) {
	lam1 = max(positivePart(AtY[k,])) / alpha
	lam2 = sqrt(sum((positivePart(AtY[k,]))^2)) / (1-alpha)
	return(min(lam1, lam2))
}

#######################################################################################
# FUNCTIONS FOR SOLVING SPARSE GROUP LASSO WITH NON-NEGATIVITY CONSTRAINT
#######################################################################################

#GGD algorithm to solve our sparse group lasso problem with non-negativity constraint
#problem is:
# 1/2 * sum(( Y - AZ )^2) + lambda*alpha*sum(Z) + lambda*(1-alpha)*sum(sqrt(rowSums(Z^2))) such that Z>=0
#solve by separating into groups of overlapping spatial components

sgl = function(Y, A, videoHeight, totalPixels, pixelsUse, lambdaMinRatio = 0.1, nLambda = 20, lambdaSeq = NULL, alpha = 0.9, tolerance = 10e-10, l2Scale="sq") {

  call = match.call()
  fit = list()

  #check function arguments
  if (nrow(Y)!=nrow(A)) stop("The number of rows of 'Y' must equal the number of rows of 'A'")
  if (length(lambdaSeq)==1) stop("Provide a sequence of decreasing values for 'lambdaSeq'")
  if (!is.null(lambdaSeq)) if(min(lambdaSeq)<=0) stop("Values in 'lambdaSeq' must be positive")
  if (min(alpha)<0 | max(alpha)>1) stop("Value for 'alpha' must be in [0,1]")

  #scale columns of A to have l1 norm of 1
  fit$initialA = A
  if (l2Scale=="sq") {
    A = t(t(A) / colSums(A))
  } else if (l2Scale=="yes") {
    A = t(t(A) / sqrt(colSums(A)))
  }
  fit$A = A

  #calculate appropriate sequence of lambdas if user-specified not provided
  if (is.null(lambdaSeq)) {
    maxLam = sglMaxLambda(A = A, Y = Y, alpha = alpha)
    lambdaSeq = exp(seq(log(maxLam), log(maxLam*lambdaMinRatio), len=nLambda))
  }

  #make sure lambdaSeq is decreasing
  lambdaSeq = sort(lambdaSeq, decreasing=TRUE)

  #matrices and lists to store results
  fit$alpha = alpha
  fit$lambdaSeq = lambdaSeq
  nTuning = length(fit$lambdaSeq)
  fit$ZList = vector("list", nTuning)
  for (l in 1:nTuning) fit$ZList[[l]] = matrix(NA, nrow=ncol(A), ncol=ncol(Y))

  #find groups of overlapping neurons, construct adjacency matrix & find the connected components
  adjMatrix = crossprod(A)
  pos = adjMatrix@x > 0
  adjMatrix@x[pos] = 1
  graph = igraph::graph.adjacency(adjMatrix, mode="undirected")
  setVec = igraph::clusters(graph)$membership

  #get fit for each set of connected components
  for (set in 1:max(setVec)) {

    message("Connected component: ", set, " of ", max(setVec))

    members = which(setVec==set)

    if (length(members)>1) { #use generalized gradient descent
      pixels = which(rowSums(A[,members])!=0)
      fit$ZList = sglConnected(Y=Y[pixels,], A=A[pixels,members], ZList=fit$ZList, nTuning=nTuning, lambdaSeq=lambdaSeq, alpha=fit$alpha, tolerance=tolerance, members=members, l2Scale=l2Scale)
    } else { #there exists a closed-form solution
      pixels = which(A[,members]!=0)
      fit$ZList = sglSingle(Y=Y[pixels,], A=A[pixels,members,drop=F], ZList=fit$ZList, nTuning=nTuning, lambdaSeq=lambdaSeq, alpha=fit$alpha, members=members)
    }
  }

  fit$numNeurons = sapply(fit$ZList, function(Z) nrow(Z[which(apply(Z, 1, function(col) sum(col^2))!=0),,drop=F]), simplify=T)

  fit$videoHeight = videoHeight; fit$pixelsUse = pixelsUse; fit$totalPixels = totalPixels
  fit$Y = Y; fit$call = call; fit$tolerance = tolerance
  class(fit) = "sgl"

  return(fit)
}

#performs Step 2(a) of Algorithm 1
#calculates closed-form solution for zhat when spatial component doesn't overlap with any other spatial components
sglSingle = function(Y, A, ZList, nTuning, lambdaSeq, alpha, members) {

  AtY = crossprod(A,Y)
  AtAinv = 1/as.vector(crossprod(A))

  for (l in 1:nTuning) {

    ZList[[l]][members,] = AtAinv * fitEst(ytilde=as.vector(AtY-lambdaSeq[l]*alpha), alpha=alpha, lambda=lambdaSeq[l], t=1)

  }
  return(ZList)
}

#performs Step 2(b) of Algorithm 1
#solves for solution for zhat using GGD when there is a group of overlapping spatial components
sglConnected = function(Y, A, ZList, nTuning, lambdaSeq, alpha, tolerance, members, l2Scale) {

	for (l in 1:nTuning) {

		#initialize Z
		if (l==1) {
			initialZ = NULL
		} else {
			initialZ = ZList[[l-1]][members,,drop=F]
		}

		oneFit = sglHelper(Y=Y, A=A, lambda=lambdaSeq[l], alpha=alpha, initialZ=initialZ, tolerance=tolerance, l2Scale=l2Scale)

		ZList[[l]][members,] = matrix(oneFit$Z, nrow=length(members))

	}
  return(ZList)
}

#calculate z update in Step 2(b)ii of Algorithm 1
sglHelper = function(Y, A, lambda, alpha, initialZ=NULL, tolerance, l2Scale) {

	#intialize matrix for storing Z estimate if not provided
	if (!is.null(initialZ)) ZOld = Matrix(initialZ, sparse=T) else ZOld = Matrix(0, nrow=ncol(A), ncol=ncol(Y), sparse=T)

	#step size
	t = 1/max(rowSums(crossprod(A)))

	#number of iterations counter
	nIter = 0

	#some matrices to store for use later
	part_of_g = -crossprod(A, Y) + lambda*alpha #part of gradient
	AtA = crossprod(A) #used repeatedly in calculating gradient
	K = nrow(ZOld)

	#do first gradient update
	g = part_of_g + crossprod(AtA, ZOld) #update gradient

	converge = FALSE

	while (converge==FALSE & nIter<1000) {

		nIter = nIter + 1

		Z = Matrix(t(apply(ZOld - t * g, 1, fitEst, alpha=alpha, lambda=lambda, t=t)), sparse=T)
		g = part_of_g + crossprod(AtA, Z) #update gradient

		#check for convergence
		if (nIter>1) {
		  ZChange = sum((Z-ZOld)^2)
		  if (ZChange < max(c(tolerance, tolerance*sum(ZOld^2)))) converge = TRUE
		}

		ZOld = Z
	}

	if (nIter==1000) warning("Convergence issue")

	return(list(Z=Z))
}

#function to perform soft-scaling on vector
softScaleEst = function(vec, tuningParameter) {

	norm = sqrt(sum(vec^2))
	if (norm <= tuningParameter) {
		vec = rep(0, length(vec))
	} else {
		vec = (1 - tuningParameter/norm) * vec
	}
	return(vec)
}

fitEst = function(ytilde, alpha, lambda, t) {

	#soft-scale the positive part of the residual vector
	ytilde = apply(cbind(0,ytilde), 1, max)
	est = softScaleEst(ytilde, t*(1-alpha)*lambda)

	return(est)
}

#create training and test sets of pixels by sampling 60% of pixels from each overlapping group of neurons
samplePixels = function(A, columns, pctTrain=0.6, seed=100) {
  set.seed(seed)
  pixels = which(rowSums(A[,columns,drop=F])!=0)
  sample(pixels, round(pctTrain*length(pixels)))
}
