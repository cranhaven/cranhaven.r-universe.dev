EM_REML_MM <-
function(Mat_K_inv, Y, X, Z, init_sigma2K, init_sigma2E, convergence_precision, nb_iter, display)
{
	###======================###
	### Build MME components ###
	###======================###	
	n = length(Y)
	XpX = t(X)%*%X
	XpZ = t(X)%*%Z
	ZpX = t(Z)%*%X
	ZpZ = t(Z)%*%Z
	XpY = t(X)%*%Y
	ZpY = t(Z)%*%Y
	RHS = c(XpY, ZpY)
	
	rankX = qr(X,LAPACK=TRUE)$rank
	rankK = qr(Mat_K_inv,LAPACK=TRUE)$rank		## The rank of a matrix is equal to the rank of its inverse (i.e. ranK=ranK_inv) 
	
	Nb_rows_MME = length(RHS)
	l = Nb_rows_MME - dim(ZpZ)[1] + 1			## Cuu extraction starts at index "l" up to "Nb_rows_MME"
	
	###=======================###
	### Initialize parameters ###
	###=======================###	
	old_sigma2E = init_sigma2E
	old_sigma2K = init_sigma2K	
	
	precision1 = 1
	precision2 = 1
	i = 0

	###===================================================###
	### Iterate over variance components through EM steps ###
	###===================================================###	
	while (precision1 > convergence_precision & precision2 > convergence_precision)
	{
		i = i+1
		
		lambda = as.vector((old_sigma2E/old_sigma2K))
		LHS = rbind( cbind(XpX, XpZ), cbind(ZpX, ZpZ+Mat_K_inv*lambda ) )	
		
		ginv_LHS=ginv(LHS)
		
		Gamma=ginv_LHS%*%RHS
		E =  Y - cbind(X,Z)%*%Gamma
	
		Cuu = ginv_LHS[l:Nb_rows_MME, l:Nb_rows_MME]
		U = Gamma[l:length(Gamma)]
		
		new_sigma2E = (t(E)%*%Y)/(n-rankX)
		new_sigma2K = (t(U)%*%Mat_K_inv%*%U + sum(Mat_K_inv*Cuu)*old_sigma2E )/(rankK)
		
		precision1 = abs(new_sigma2E-old_sigma2E)
		precision2 = abs(new_sigma2K-old_sigma2K)
		
		old_sigma2E = new_sigma2E
		old_sigma2K = new_sigma2K
		
		if ( i >= nb_iter ){ break }
	}
	
	if (display == TRUE)
	{
		cat('\n')
		cat("iteration ", i, '\n')
		cat("Sigma2E_hat", new_sigma2E, '\n')
		cat("Sigma2K_hat", new_sigma2K, '\n')				
	}
	
	cat('\n')
	return(list( "Beta_hat"=Gamma[1:(l-1)], "U_hat"=U, "Sigma2E_hat"=new_sigma2E, "Sigma2K_hat"=new_sigma2K ))

}
