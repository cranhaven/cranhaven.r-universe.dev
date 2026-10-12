Kernel_Ridge_MM <-
function( Y_train, X_train=as.vector(rep(1,length(Y_train))), Z_train=diag(1,length(Y_train)), 

	Matrix_covariates_train, method="RKHS", kernel="Gaussian", rate_decay_kernel=0.1, degree_poly=2, scale_poly=1, 
	
	offset_poly=1, degree_anova=3, init_sigma2K=2, init_sigma2E=3, convergence_precision=1e-8, nb_iter=1000, display="FALSE" )
{


	###===============================================###
	### kernel Ridge regression (aka RKHS regression) ###
	###===============================================###
	
	if ( identical( method, "RKHS" ) ) 
	{ 
		
		###-----------------###
		### Gaussian kernel ###
		###-----------------###

		if ( identical( kernel, "Gaussian" ) )
		{
		
			p=dim(Matrix_covariates_train)[2]
			rbf=rbfdot(sigma = (1/p)*rate_decay_kernel)
			
			K_train=kernelMatrix(rbf, Matrix_covariates_train)
			K_train_inv=ginv(K_train)
			n_train=length(Y_train)
	
			MM_components_solved=EM_REML_MM( K_train_inv, Y_train, X_train, Z_train, init_sigma2K, init_sigma2E, convergence_precision, nb_iter, display )

			Beta_hat_train = as.vector(MM_components_solved$Beta_hat)
			Sigma2K_hat_train = as.vector(MM_components_solved$Sigma2K_hat)
			Sigma2E_hat_train = as.vector(MM_components_solved$Sigma2E_hat)
	
			lambda=(Sigma2E_hat_train/Sigma2K_hat_train)
	
			Var_Y_train_div_sig2_alpha = Z_train%*%K_train%*%t(Z_train) + lambda*diag(1, n_train)
	
			if ( length(Beta_hat_train) > 1 )
			{	
				Vect_alpha = t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train%*%Beta_hat_train)
		
			}else{
	
				Vect_alpha = t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train*Beta_hat_train)
		
			}
	
			return( list( "Matrix_covariates_train"=Matrix_covariates_train, "Beta_hat"=Beta_hat_train,

			"Sigma2K_hat"=Sigma2K_hat_train, "Sigma2E_hat"=Sigma2E_hat_train, "Vect_alpha"=Vect_alpha, 
			
			"method"=method, "kernel"=kernel, "rate_decay_kernel"=rate_decay_kernel ) )

			
		###------------------###
		### Laplacian kernel ###
		###------------------###
		
		} else if ( identical( kernel, "Laplacian" ) ){
		
		
			p=dim(Matrix_covariates_train)[2]
			rbf=laplacedot(sigma = (1/p)*rate_decay_kernel)
			
			K_train=kernelMatrix(rbf, Matrix_covariates_train)					
			K_train_inv=ginv(K_train)
			n_train=length(Y_train)
	
			MM_components_solved=EM_REML_MM( K_train_inv, Y_train, X_train, Z_train, init_sigma2K, init_sigma2E, convergence_precision, nb_iter, display )

			Beta_hat_train = as.vector(MM_components_solved$Beta_hat)
			Sigma2K_hat_train = as.vector(MM_components_solved$Sigma2K_hat)
			Sigma2E_hat_train = as.vector(MM_components_solved$Sigma2E_hat)
	
			lambda=(Sigma2E_hat_train/Sigma2K_hat_train)
	
			Var_Y_train_div_sig2_alpha = Z_train%*%K_train%*%t(Z_train) + lambda*diag(1, n_train)
	
			if ( length(Beta_hat_train) > 1 )
			{	
				Vect_alpha = t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train%*%Beta_hat_train)
		
			}else{
	
				Vect_alpha = t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train*Beta_hat_train)
		
			}	

			return( list( "Matrix_covariates_train"=Matrix_covariates_train, "Beta_hat"=Beta_hat_train,

			"Sigma2K_hat"=Sigma2K_hat_train, "Sigma2E_hat"=Sigma2E_hat_train, "Vect_alpha"=Vect_alpha, 
			
			"method"=method, "kernel"=kernel, "rate_decay_kernel"=rate_decay_kernel ) )


		###-------------------###
		### Polynomial kernel ###
		###-------------------###		
		
		} else if ( identical( kernel, "Polynomial" ) ){
		
			rbf=polydot(degree = degree_poly, scale = scale_poly, offset = offset_poly)
			
			K_train=kernelMatrix(rbf, Matrix_covariates_train)		
			K_train_inv=ginv(K_train)
			n_train=length(Y_train)
	
			MM_components_solved=EM_REML_MM( K_train_inv, Y_train, X_train, Z_train, init_sigma2K, init_sigma2E, convergence_precision, nb_iter, display )

			Beta_hat_train = as.vector(MM_components_solved$Beta_hat)
			Sigma2K_hat_train = as.vector(MM_components_solved$Sigma2K_hat)
			Sigma2E_hat_train = as.vector(MM_components_solved$Sigma2E_hat)
	
			lambda=(Sigma2E_hat_train/Sigma2K_hat_train)
	
			Var_Y_train_div_sig2_alpha = Z_train%*%K_train%*%t(Z_train) + lambda*diag(1, n_train)
	
			if ( length(Beta_hat_train) > 1 )
			{	
				Vect_alpha = t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train%*%Beta_hat_train)
		
			}else{
	
				Vect_alpha = t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train*Beta_hat_train)
		
			}
	
			return( list( "Matrix_covariates_train"=Matrix_covariates_train, "Beta_hat"=Beta_hat_train, 
			
			"Sigma2K_hat"=Sigma2K_hat_train, "Sigma2E_hat"=Sigma2E_hat_train, "Vect_alpha"=Vect_alpha, "method"=method, 
			
			"kernel"=kernel, "degree_poly"=degree_poly, "scale_poly"=scale_poly, "offset_poly"=offset_poly ) )

		
		###--------------###
		### ANOVA kernel ###
		###--------------###
	
		} else if ( identical( kernel, "ANOVA" ) ){		
		
			p=dim(Matrix_covariates_train)[2]
			rbf=anovadot(sigma = (1/p)*rate_decay_kernel, degree = degree_anova)
			
			K_train=kernelMatrix(rbf, Matrix_covariates_train)			
			K_train_inv=ginv(K_train)
			n_train=length(Y_train)
	
			MM_components_solved=EM_REML_MM( K_train_inv, Y_train, X_train, Z_train, init_sigma2K, init_sigma2E, convergence_precision, nb_iter, display )

			Beta_hat_train = as.vector(MM_components_solved$Beta_hat)
			Sigma2K_hat_train = as.vector(MM_components_solved$Sigma2K_hat)
			Sigma2E_hat_train = as.vector(MM_components_solved$Sigma2E_hat)
	
			lambda=(Sigma2E_hat_train/Sigma2K_hat_train)
	
			Var_Y_train_div_sig2_alpha = Z_train%*%K_train%*%t(Z_train) + lambda*diag(1, n_train)
	
			if ( length(Beta_hat_train) > 1 )
			{	
				Vect_alpha = t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train%*%Beta_hat_train)
		
			}else{
	
				Vect_alpha = t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train*Beta_hat_train)
		
			}
	
			return( list( "Matrix_covariates_train"=Matrix_covariates_train, "Beta_hat"=Beta_hat_train, 
			
			"Sigma2K_hat"=Sigma2K_hat_train, "Sigma2E_hat"=Sigma2E_hat_train, "Vect_alpha"=Vect_alpha, "method"=method, 
			
			"kernel"=kernel, "rate_decay_kernel"=rate_decay_kernel, "degree_anova"=degree_anova ) )
		
		}
		
	###==========================================================================================###
	### Ridge regression with estimated marker effects (aka RR-BLUP or GBLUP with linear kernel) ###
	###==========================================================================================###		
	 
	} else if ( identical( method, "RR-BLUP" ) ){
	
		Matrix_covariates_train=scale(Matrix_covariates_train, center=TRUE, scale=FALSE)
	
		K_train=Matrix_covariates_train%*%t(Matrix_covariates_train)
		K_train_inv=ginv(K_train)
		n_train=length(Y_train)
	
		MM_components_solved=EM_REML_MM( K_train_inv, Y_train, X_train, Z_train, init_sigma2K, init_sigma2E, convergence_precision, nb_iter, display )

		Beta_hat_train = as.vector(MM_components_solved$Beta_hat)
		Sigma2K_hat_train = as.vector(MM_components_solved$Sigma2K_hat)
		Sigma2E_hat_train = as.vector(MM_components_solved$Sigma2E_hat)
	
		lambda=(Sigma2E_hat_train/Sigma2K_hat_train)
	
		Var_Y_train_div_sig2_alpha = Z_train%*%K_train%*%t(Z_train) + lambda*diag(1, n_train)
	
		if ( length(Beta_hat_train) > 1 )
		{	
			Vect_alpha=t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train%*%Beta_hat_train)
			Gamma_hat=t(Matrix_covariates_train)%*%Vect_alpha
			
		
		}else{
	
			Vect_alpha=t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train*Beta_hat_train)
			Gamma_hat=t(Matrix_covariates_train)%*%Vect_alpha
		}
		
		return( list( "Matrix_covariates_train"=Matrix_covariates_train, "Beta_hat"=Beta_hat_train, 
		
		"Sigma2K_hat"=Sigma2K_hat_train, "Sigma2E_hat"=Sigma2E_hat_train, "Vect_alpha"=Vect_alpha, "Gamma_hat"=Gamma_hat, 

		"method"=method ) )
		
		
	###================###
	### GBLUP directly ###
	###================###
	
	} else if ( identical( method, "GBLUP" ) ){

		###----------------------------###
		### Linear kernel (i.e. GBLUP) ###
		###----------------------------###

		Matrix_covariates_train=scale(Matrix_covariates_train, center=TRUE, scale=FALSE)
		
		K_train=Matrix_covariates_train%*%t(Matrix_covariates_train)
		K_train_inv=ginv(K_train)
		n_train=length(Y_train)
	
		MM_components_solved=EM_REML_MM( K_train_inv, Y_train, X_train, Z_train, init_sigma2K, init_sigma2E, convergence_precision, nb_iter, display )

		Beta_hat_train = as.vector(MM_components_solved$Beta_hat)
		Sigma2K_hat_train = as.vector(MM_components_solved$Sigma2K_hat)
		Sigma2E_hat_train = as.vector(MM_components_solved$Sigma2E_hat)
	
		lambda=(Sigma2E_hat_train/Sigma2K_hat_train)
	
		Var_Y_train_div_sig2_alpha = Z_train%*%K_train%*%t(Z_train) + lambda*diag(1, n_train)

		if ( length(Beta_hat_train) > 1 )
		{	
			Vect_alpha = t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train%*%Beta_hat_train)
		
		}else{
	
			Vect_alpha = t(Z_train)%*%ginv(Var_Y_train_div_sig2_alpha)%*%(Y_train - X_train*Beta_hat_train)
		
		}
	
		return( list( "Matrix_covariates_train"=Matrix_covariates_train, "Beta_hat"=Beta_hat_train,

		"Sigma2K_hat"=Sigma2K_hat_train, "Sigma2E_hat"=Sigma2E_hat_train, "Vect_alpha"=Vect_alpha, 
		
		"method"=method ) )
	
	}

	
}
