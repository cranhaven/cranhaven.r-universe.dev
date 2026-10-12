Predict_kernel_Ridge_MM <-
function( Model_kernel_Ridge_MM, Matrix_covariates_target, 

	X_target=as.vector(rep(1,dim(Matrix_covariates_target)[1])),

	Z_target=diag(1,dim(Matrix_covariates_target)[1]) )  

{
	###=====================================###
	### Get fixed effects from model object ###
	###=====================================###

	Beta_hat=Model_kernel_Ridge_MM$Beta_hat 
	
	
	###===============================================###
	### kernel Ridge regression (aka RKHS regression) ###
	###===============================================###
	
	if ( identical( Model_kernel_Ridge_MM$method, "RKHS" ) ) 
	{ 
		
		
		###-----------------###
		### Gaussian kernel ###
		###-----------------###

		if ( identical( Model_kernel_Ridge_MM$kernel, "Gaussian" ) )
		{
			
			p=dim(Model_kernel_Ridge_MM$Matrix_covariates_train)[2]
			
			rbf=rbfdot(sigma = (1/p)*Model_kernel_Ridge_MM$rate_decay_kernel)
	
			K_target_train=kernelMatrix(rbf, Matrix_covariates_target, Model_kernel_Ridge_MM$Matrix_covariates_train)			
			
			if ( length(Beta_hat) > 1 )
			{					
				f_hat = X_target%*%Beta_hat + Z_target%*%K_target_train%*%Model_kernel_Ridge_MM$Vect_alpha

			}else{
	
				f_hat = X_target*Beta_hat + Z_target%*%K_target_train%*%Model_kernel_Ridge_MM$Vect_alpha		
			}	
	
			return( f_hat )
			
			
		###------------------###
		### Laplacian kernel ###
		###------------------###
		
		} else if ( identical( Model_kernel_Ridge_MM$kernel, "Laplacian" ) )	{
			
			p=dim(Model_kernel_Ridge_MM$Matrix_covariates_train)[2]
			
			rbf=laplacedot(sigma = (1/p)*Model_kernel_Ridge_MM$rate_decay_kernel)
	
			K_target_train=kernelMatrix(rbf, Matrix_covariates_target, Model_kernel_Ridge_MM$Matrix_covariates_train)
	
			if ( length(Beta_hat) > 1 )
			{					
				f_hat = X_target%*%Beta_hat + Z_target%*%K_target_train%*%Model_kernel_Ridge_MM$Vect_alpha

			}else{
	
				f_hat = X_target*Beta_hat + Z_target%*%K_target_train%*%Model_kernel_Ridge_MM$Vect_alpha		
			}	
	
			return( f_hat )
			
			
		###-------------------###
		### Polynomial kernel ###
		###-------------------###		
		
		} else if ( identical( Model_kernel_Ridge_MM$kernel, "Polynomial" ) )	{
			
			rbf=polydot(degree = Model_kernel_Ridge_MM$degree_poly, scale = Model_kernel_Ridge_MM$scale_poly,

			offset = Model_kernel_Ridge_MM$offset_poly )
			
			K_target_train=kernelMatrix(rbf, Matrix_covariates_target, Model_kernel_Ridge_MM$Matrix_covariates_train)
	
			if ( length(Beta_hat) > 1 )
			{					
				f_hat = X_target%*%Beta_hat + Z_target%*%K_target_train%*%Model_kernel_Ridge_MM$Vect_alpha

			}else{
	
				f_hat = X_target*Beta_hat + Z_target%*%K_target_train%*%Model_kernel_Ridge_MM$Vect_alpha		
			}	
	
			return( f_hat )
		
			
		###--------------###
		### ANOVA kernel ###
		###--------------###
	
		} else if ( identical( Model_kernel_Ridge_MM$kernel, "ANOVA" ) ){		
		
			p=dim(Model_kernel_Ridge_MM$Matrix_covariates_train)[2]
			
			rbf=anovadot(sigma = (1/p)*Model_kernel_Ridge_MM$rate_decay_kernel, degree = Model_kernel_Ridge_MM$degree_anova)
	
			K_target_train=kernelMatrix(rbf, Matrix_covariates_target, Model_kernel_Ridge_MM$Matrix_covariates_train)
	
			if ( length(Beta_hat) > 1 )
			{					
				f_hat = X_target%*%Beta_hat + Z_target%*%K_target_train%*%Model_kernel_Ridge_MM$Vect_alpha

			}else{
	
				f_hat = X_target*Beta_hat + Z_target%*%K_target_train%*%Model_kernel_Ridge_MM$Vect_alpha		
			}	
	
			return( f_hat )
			
		}	
		
		
	###==========================================================================================###
	### Ridge regression with estimated marker effects (aka RR-BLUP or GBLUP with linear kernel) ###
	###==========================================================================================###		
	 
	} else if ( identical( Model_kernel_Ridge_MM$method, "RR-BLUP" ) ){
	
		Matrix_covariates_target=scale(Matrix_covariates_target, center=TRUE, scale=FALSE)
		
		if ( length(Beta_hat) > 1 )
		{					
			f_hat = X_target%*%Beta_hat + Z_target%*%Matrix_covariates_target%*%Model_kernel_Ridge_MM$Gamma_hat

		}else{
	
			f_hat = X_target*Beta_hat + Z_target%*%Matrix_covariates_target%*%Model_kernel_Ridge_MM$Gamma_hat		
		}	
	
		return( f_hat )
		
		
	###================###
	### GBLUP directly ###
	###================###
	
	} else if ( identical( Model_kernel_Ridge_MM$method, "GBLUP" ) ){

		###----------------------------###
		### Linear kernel (i.e. GBLUP) ###
		###----------------------------###

		Matrix_covariates_target=scale(Matrix_covariates_target, center=TRUE, scale=FALSE)
		
		K_target_train=Matrix_covariates_target%*%t(Model_kernel_Ridge_MM$Matrix_covariates_train)

		if ( length(Beta_hat) > 1 )
		{					
			f_hat = X_target%*%Beta_hat + Z_target%*%K_target_train%*%Model_kernel_Ridge_MM$Vect_alpha

		}else{
	
			f_hat = X_target*Beta_hat + Z_target%*%K_target_train%*%Model_kernel_Ridge_MM$Vect_alpha		
		}	
	
		return( f_hat )
	
	}    

}
