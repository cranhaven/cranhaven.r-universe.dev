Tune_kernel_Ridge_MM <-
function( Y_train, X_train=as.vector(rep(1,length(Y_train))), Z_train=diag(1,length(Y_train)), 

	Matrix_covariates_train, method="RKHS", kernel="Gaussian", rate_decay_kernel=0.1, degree_poly=2, scale_poly=1, 
	
	offset_poly=1, degree_anova=3, init_sigma2K=2, init_sigma2E=3, convergence_precision=1e-8, nb_iter=1000, display="FALSE",

	rate_decay_grid=seq(0.1,1.0,length.out=10), nb_folds=5, loss="mse")
{

	N=length(Y_train)
	Index_whole=1:N
	Folds=cvFolds(N, nb_folds, type="consecutive")
	Vect_expected_loss_grid=rep(0, length(rate_decay_grid))
	
	method_defined=method
	kernel_defined=kernel
	rate_decay_defined=rate_decay_kernel
	degree_poly_defined=degree_poly
	scale_poly_defined=scale_poly
	offset_poly_defined=offset_poly
	degree_anova_defined=degree_anova
	init_sigma2K_defined=init_sigma2K
	init_sigma2E_defined=init_sigma2E
	convergence_precision_defined=convergence_precision
	nb_iter_defined=nb_iter
	display_defined=display

	if( identical(loss, "mse") )
	{
	
	
		l=1
		for ( h in rate_decay_grid )
		{	
			
			
			Vect_loss_folds=rep(0,nb_folds)
		
			for ( fold_i in 1:nb_folds )
			{	
				
				### ------------------------------###
				###	Target subset in training set ###		
				### ------------------------------###
			
				Index_fold_i=which(Folds$which==fold_i)							

				Y_fold_i=Y_train[Index_fold_i]
				Matrix_covariates_fold_i=Matrix_covariates_train[Index_fold_i, ]
			
				### -----------------------------###
				###	Train subset in training set ###		
				### -----------------------------###			
			
				Index_minus_fold_i=Index_whole[-Index_fold_i]					

				Y_minus_fold_i=Y_train[Index_minus_fold_i]
				X_minus_fold_i=as.vector(rep(1,length(Y_minus_fold_i)))
				Z_minus_fold_i=diag(1,length(Y_minus_fold_i))
			
				Matrix_covariates_minus_fold_i=Matrix_covariates_train[Index_minus_fold_i, ]
			
				###-----------------------------------------------------------------------###
				### Build Kernel Ridge regression (KRR) model on train subset in training ###
				###-----------------------------------------------------------------------###
			
				Model_KRR_minus_fold_i = Kernel_Ridge_MM( Y_train=Y_minus_fold_i, X_train=X_minus_fold_i, 
				
				Z_train=Z_minus_fold_i, Matrix_covariates_train=Matrix_covariates_minus_fold_i,

				method=method_defined, kernel=kernel_defined, rate_decay_kernel=h, degree_poly=degree_poly_defined,

				scale_poly=scale_poly_defined, offset_poly=offset_poly_defined, degree_anova=degree_anova_defined,

				init_sigma2K=init_sigma2K_defined, init_sigma2E=init_sigma2E_defined, 
				
				convergence_precision=convergence_precision_defined, nb_iter=nb_iter_defined, display=display_defined )

				f_hat_fold_i = Predict_kernel_Ridge_MM( Model_KRR_minus_fold_i, Matrix_covariates_fold_i )
	
				### Loss measure for fold i:
				
				Vect_loss_folds[fold_i]= mean((f_hat_fold_i-Y_fold_i)^2)
		
			}
		
			Vect_expected_loss_grid[l]=mean(Vect_loss_folds)
			l=l+1	
		}
	
		### The optimal h:
		
		optimal_h=rate_decay_grid[which.min(Vect_expected_loss_grid)]
		
		
		### The tuned model for optimal h:
		
		tuned_model=Kernel_Ridge_MM ( Y_train, X_train, Z_train, Matrix_covariates_train, method, kernel,

		rate_decay_kernel=optimal_h, degree_poly, scale_poly, offset_poly, degree_anova, init_sigma2K, init_sigma2E,

		convergence_precision, nb_iter, display)

		return( list( "tuned_model"=tuned_model, "optimal_h"=optimal_h, "loss"=loss,

		"expected_loss_grid"=Vect_expected_loss_grid, "rate_decay_grid"=rate_decay_grid ) )
		
	
	} else if ( identical( loss, "cor" ) ){
	
	
		l=1
		for ( h in rate_decay_grid )
		{	
			
			
			Vect_loss_folds=rep(0,nb_folds)
		
			for ( fold_i in 1:nb_folds )
			{	
				
				### ------------------------------###
				###	Target subset in training set ###		
				### ------------------------------###
			
				Index_fold_i=which(Folds$which==fold_i)							

				Y_fold_i=Y_train[Index_fold_i]
				Matrix_covariates_fold_i=Matrix_covariates_train[Index_fold_i, ]
			
				### -----------------------------###
				###	Train subset in training set ###		
				### -----------------------------###			
			
				Index_minus_fold_i=Index_whole[-Index_fold_i]					

				Y_minus_fold_i=Y_train[Index_minus_fold_i]
				X_minus_fold_i=as.vector(rep(1,length(Y_minus_fold_i)))
				Z_minus_fold_i=diag(1,length(Y_minus_fold_i))
			
				Matrix_covariates_minus_fold_i=Matrix_covariates_train[Index_minus_fold_i, ]
			
				###-----------------------------------------------------------------------###
				### Build Kernel Ridge regression (KRR) model on train subset in training ###
				###-----------------------------------------------------------------------###
			
				Model_KRR_minus_fold_i = Kernel_Ridge_MM( Y_train=Y_minus_fold_i, X_train=X_minus_fold_i, 
				
				Z_train=Z_minus_fold_i, Matrix_covariates_train=Matrix_covariates_minus_fold_i,

				method=method_defined, kernel=kernel_defined, rate_decay_kernel=h, degree_poly=degree_poly_defined,

				scale_poly=scale_poly_defined, offset_poly=offset_poly_defined, degree_anova=degree_anova_defined,

				init_sigma2K=init_sigma2K_defined, init_sigma2E=init_sigma2E_defined, 
				
				convergence_precision=convergence_precision_defined, nb_iter=nb_iter_defined, display=display_defined )

				f_hat_fold_i = Predict_kernel_Ridge_MM( Model_KRR_minus_fold_i, Matrix_covariates_fold_i )
	
				### Loss measure for fold i:
				
				Vect_loss_folds[fold_i]= 0.5*(1-cor(f_hat_fold_i, Y_fold_i))
		
			}
		
			Vect_expected_loss_grid[l]=mean(Vect_loss_folds)
			l=l+1	
		}
	
		### The optimal h:
		
		optimal_h=rate_decay_grid[which.min(Vect_expected_loss_grid)]
		
		
		### The tuned model for optimal h:
		
		tuned_model=Kernel_Ridge_MM ( Y_train, X_train, Z_train, Matrix_covariates_train, method, kernel,

		rate_decay_kernel=optimal_h, degree_poly, scale_poly, offset_poly, degree_anova, init_sigma2K, init_sigma2E,

		convergence_precision, nb_iter, display)

		return( list( "tuned_model"=tuned_model, "optimal_h"=optimal_h, "loss"=loss,

		"expected_loss_grid"=Vect_expected_loss_grid, "rate_decay_grid"=rate_decay_grid ) )
	
	
	}
	
	
	
	
}
