#include <RcppArmadillo.h>
#ifdef _OPENMP
    #include <omp.h>
    // [[Rcpp::plugins(openmp)]]
#endif
// [[Rcpp::depends(RcppArmadillo)]]


using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
arma::mat soft_full(arma::mat L, double lambda){
	int height=L.n_rows;
	int width=L.n_cols;
 	for(int y = 0; y < height; y++){
 		for(int x=0; x < width; x++) {
            if (L(y, x) > lambda)
            	L(y, x) = L(y, x) - lambda;
            else if (L(y, x) <  - lambda)
                L(y, x) = L(y, x) + lambda;
            else
                L(y, x) = 0.0;
        }

 	}
 	return L;
}

// [[Rcpp::export]]
List var_break_fit_block_cpp(NumericMatrix data, double lambda, double lambda2, int q, int max_iteration, double tol , NumericMatrix initial_phi, NumericVector blocks, NumericVector cv_index ){

    int k = data.ncol(); int T = data.nrow(); int n_new = blocks.size() - 1;
    int n = T - q;


    arma::mat data_m(data.begin(), T, k);
    List Y_b(n_new);
    List y_b(n_new);

    for(int i = 0; i < n_new; i++) {
    	y_b[i] = data_m( span(blocks[i],  blocks[i+1]-1 ), span::all) ;
    }
    y_b[0] = data_m(  span(q, (blocks[1]-1) ), span::all );


    arma::mat Y( k*q, n);
    for(int i = (q-1); i < (T-1); i++) {
    	for(int j = 1; j <= q; j++){
    		Y.submat( (j-1)*k,(i-q+1), j*k-1 ,(i-q+1) ) = data_m.submat(i-j+1, 0, i-j+1,  k-1 ).t();
    	}

    }

    Y_b[0] = Y.submat( 0 ,0, k*q-1,blocks[1]-q-1  );
    for(int i = 1; i < n_new; i++) {
    	Y_b[i] = Y.submat( 0, blocks[i]-q ,k*q-1 , blocks[i+1]-q-1  ) ;
    }

    int cv_l = cv_index.size();
    if( cv_l >0){
    	for(int t_1 = 0; t_1 < cv_l; t_1 ++){
    		arma::mat yb_temp = y_b[cv_index[t_1]-1];
    		arma::mat Yb_temp = Y_b[cv_index[t_1]-1];
    		int tt =  yb_temp.n_rows;
            y_b[cv_index[t_1]-1] = yb_temp(span(0,tt-2), span::all );
            Y_b[cv_index[t_1]-1] = Yb_temp( span::all ,span(0,tt-2));

    	}
    }

    List C(n_new);
    for(int j = 0; j < n_new; j++){
    	arma::mat yb_temp = y_b[j];
    	arma::mat Yb_temp = Y_b[j];
    	C[j] = Yb_temp * yb_temp;
    }

    arma::mat C_sum (k*q*n_new, k, fill::zeros);
    arma::mat C_temp = C[0];
    C_sum(span(0, k*q - 1) , span::all) = C_temp;
    for(int i = 2; i <= n_new ; i++){
    	arma::mat C_temp = C[i-1];
    	C_sum(  span((i-1)*k*q, (i*k*q) -1  ), span::all ) = C_sum( span( (i-2)*k*q,(i-1)*k*q -1 ), span::all) + C_temp;
    }
    arma::mat C_sum_new (k*q*n_new,k, fill::zeros);
    C_sum_new( span(   0, k*q - 1 ), span::all) =  C_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all );
    for(int i = 2; i <= n_new ; i++){
    	C_sum_new(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = C_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all ) - C_sum( span ((i-2)*k*q, (i-1)*k*q -1 ), span::all);
    }


    List D(n_new);
    for(int j = 0; j < n_new; j++){
    	arma::mat Yb_temp = Y_b[j];
    	D[j] = Yb_temp * Yb_temp.t();
    }

    arma::mat D_sum (k*q*n_new,k*q, fill::zeros);
    arma::mat D_temp = D[0];
    D_sum(span(0, k*q - 1) , span::all) = D_temp;
    for(int i = 2; i <= n_new ; i++){
    	arma::mat D_temp = D[i-1];
    	D_sum(  span((i-1)*k*q, (i*k*q) -1  ), span::all ) = D_sum( span( (i-2)*k*q,(i-1)*k*q -1 ), span::all) + D_temp;
    }
    arma::mat D_sum_new (k*q*n_new,k*q, fill::zeros);
    D_sum_new( span(  0, k*q - 1 ), span::all) =  D_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all );
    for(int i = 2; i <= n_new ; i++){
    	D_sum_new(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = D_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all ) - D_sum( span ((i-2)*k*q, (i-1)*k*q -1 ), span::all);
    }

    arma::mat D_sum_new_inv (k*q*n_new,k*q, fill::zeros);

    for(int i = 1; i <= n_new ; i++){
        vec eigval =  eig_sym(D_sum_new ( span((i-1)*k*q, (i*k*q)-1), span::all)  );
        double add_pd =0.0;
        double min_eigen = eigval(0);
        if(min_eigen <= 0){
            // Rprintf("Invertiable! adding noise!");
            add_pd = (10)*fabs( min_eigen);
        }
        arma::mat noise(k*q,k*q, fill::eye);
        D_sum_new_inv(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = inv( D_sum_new( span((i-1)*k*q, (i*k*q) -1  ), span::all  ) +  add_pd*noise );
    }


    arma::mat phi_hat(initial_phi.begin(), k, k*q*n_new);
    arma::mat phi_new (k, k*q*n_new, fill::zeros);

    int flag = 0;
    int l = 2;

    while( l < max_iteration){
    	if(  l == floor(0.5 * max_iteration)  ){
    		tol = (2)*tol;
    	}
    	if( l  == floor( 0.75 * max_iteration )) {
    		tol = (4/2)*tol;
    	}
        l = l+1;
        arma::mat phi_compare = phi_hat;

        for(int i = 1; i <= n_new ; i++){
        	List E(n_new);
        	for(int j = 1; j <= n_new ; j++){

        		E[j-1] = D_sum_new( span(  ( std::max( j,i )-1)*k*q  , std::max(j,i )*k*q  -1 ), span::all)  * phi_hat(span::all , span( (j-1)*k*q, j*k*q -1  )).t() ;
        	}




        	arma::mat E_new = E[0];
    		for ( int g = 1; g < n_new; g++ ) {
    			arma::mat E_temp = E[g];
    			E_new = E_new + E_temp;
    		}




    		E_new =  E_new - D_sum_new(  span( (i-1)*k*q,  i*k*q -1  ), span::all)  * phi_hat( span::all, span( (i-1)*k*q, i*k*q -1) ).t();

    		arma::mat S =  C_sum_new(  span(  (i-1)*k*q , i*k*q -1 ), span::all )  - E_new;


    		S = soft_full(S, lambda);


    		arma::mat phi_temp = D_sum_new_inv( span ((i-1)*k*q , i*k*q-1), span::all )  *  S;

    		phi_temp = phi_temp.t();


    		phi_hat( span::all ,   span( (i-1)*k*q, i*k*q -1)  ) = phi_temp;
            phi_new( span::all ,   span( (i-1)*k*q, i*k*q -1)  ) = phi_temp;


        }

        arma::mat phi_temp_soft(k,k*q*n_new, fill::zeros);
        phi_temp_soft( span::all , span(0, k*q-1) ) = soft_full(phi_hat( span::all ,  span(0, k*q-1) ),lambda2);
        arma::mat temp_1 = phi_hat( span::all ,  span(0, k*q-1) );
        for(int z_1 = 2; z_1 <= n_new; z_1 ++){
            arma::mat temp_2 = temp_1 + phi_hat( span::all ,  span((z_1-1)*k*q, z_1*k*q-1) );
            arma::mat temp_1_soft = soft_full(temp_1,lambda2);
            arma::mat temp_2_soft = soft_full(temp_2,lambda2);
            phi_temp_soft(   span::all ,  span( (z_1-1)*k*q , z_1*k*q  -1 ) )= temp_2_soft - temp_1_soft;
            temp_1 = temp_2;
        }
        phi_new  = phi_temp_soft;



        arma::mat abs_temp = abs(phi_new - phi_compare);
        double max_temp = abs_temp.max();
        if ( max_temp < tol) {
            break;
        }
        if ( max_temp > tol ) {
        	phi_hat = phi_new;
        }
        if (  max_temp > pow(10.0, 5.0)) {
            flag = 1;
            break;
        }


	}


    return List::create(Named("phi.hat")= phi_hat,Named("flag")= flag);
}


// [[Rcpp::export]]
List var_break_fit_block_group_cpp(NumericMatrix data, double lambda, double lambda2, int q, int max_iteration, double tol , NumericMatrix initial_phi, NumericVector blocks, NumericVector cv_index, List group_index ){

    int k = data.ncol(); int T = data.nrow(); int n_new = blocks.size() - 1;
    int n = T - q;


    arma::mat data_m(data.begin(), T, k);
    List Y_b(n_new);
    List y_b(n_new);

    for(int i = 0; i < n_new; i++) {
    	y_b[i] = data_m( span(blocks[i],  blocks[i+1]-1 ), span::all) ;
    }
    y_b[0] = data_m(  span(q, (blocks[1]-1) ), span::all );


    arma::mat Y( k*q, n);
    for(int i = (q-1); i < (T-1); i++) {
    	for(int j = 1; j <= q; j++){
    		Y.submat( (j-1)*k,(i-q+1), j*k-1 ,(i-q+1) ) = data_m.submat(i-j+1, 0, i-j+1,  k-1 ).t();
    	}

    }

    Y_b[0] = Y.submat( 0 ,0, k*q-1,blocks[1]-q-1  );
    for(int i = 1; i < n_new; i++) {
    	Y_b[i] = Y.submat( 0, blocks[i]-q ,k*q-1 , blocks[i+1]-q-1  ) ;
    }

    //NumericMatrix temp = y_b[0];
    //Rcout << temp.nrow();

    int cv_l = cv_index.size();
    if( cv_l >0){
    	for(int t_1 = 0; t_1 < cv_l; t_1 ++){
    		arma::mat yb_temp = y_b[cv_index[t_1]-1];
    		arma::mat Yb_temp = Y_b[cv_index[t_1]-1];
    		int tt =  yb_temp.n_rows;
    		//Rcout << tt1;
            y_b[cv_index[t_1]-1] = yb_temp(span(0,tt-2), span::all );
            Y_b[cv_index[t_1]-1] = Yb_temp( span::all ,span(0,tt-2));

    	}
    }

    List C(n_new);
    for(int j = 0; j < n_new; j++){
    	arma::mat yb_temp = y_b[j];
    	arma::mat Yb_temp = Y_b[j];
    	C[j] = Yb_temp * yb_temp;
    }

    arma::mat C_sum (k*q*n_new, k, fill::zeros);
    //Rcout << C_sum(1,1);
    arma::mat C_temp = C[0];
    C_sum(span(0, k*q - 1) , span::all) = C_temp;
    for(int i = 2; i <= n_new ; i++){
    	arma::mat C_temp = C[i-1];
    	//Rcout <<  size(C_temp);
    	C_sum(  span((i-1)*k*q, (i*k*q) -1  ), span::all ) = C_sum( span( (i-2)*k*q,(i-1)*k*q -1 ), span::all) + C_temp;
    }
    arma::mat C_sum_new (k*q*n_new,k, fill::zeros);
    C_sum_new( span(   0, k*q - 1 ), span::all) =  C_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all );
    for(int i = 2; i <= n_new ; i++){
    	C_sum_new(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = C_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all ) - C_sum( span ((i-2)*k*q, (i-1)*k*q -1 ), span::all);
    }


    List D(n_new);
    for(int j = 0; j < n_new; j++){
    	arma::mat Yb_temp = Y_b[j];
    	D[j] = Yb_temp * Yb_temp.t();
    }

    arma::mat D_sum (k*q*n_new,k*q, fill::zeros);
    arma::mat D_temp = D[0];
    D_sum(span(0, k*q - 1) , span::all) = D_temp;
    for(int i = 2; i <= n_new ; i++){
    	arma::mat D_temp = D[i-1];
    	D_sum(  span((i-1)*k*q, (i*k*q) -1  ), span::all ) = D_sum( span( (i-2)*k*q,(i-1)*k*q -1 ), span::all) + D_temp;
    }
    arma::mat D_sum_new (k*q*n_new,k*q, fill::zeros);
    D_sum_new( span(  0, k*q - 1 ), span::all) =  D_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all );
    for(int i = 2; i <= n_new ; i++){
    	D_sum_new(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = D_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all ) - D_sum( span ((i-2)*k*q, (i-1)*k*q -1 ), span::all);
    }

    arma::mat D_sum_new_inv (k*q*n_new,k*q, fill::zeros);

    for(int i = 1; i <= n_new ; i++){
        vec eigval =  eig_sym(D_sum_new ( span((i-1)*k*q, (i*k*q)-1), span::all)  );
        double add_pd =0.0;
        double min_eigen = eigval(0);
        if(min_eigen <= 0){
            // Rprintf("Invertiable! adding noise!");
            add_pd = (10)*fabs( min_eigen);
        }
        arma::mat noise(k*q,k*q, fill::eye);
        D_sum_new_inv(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = inv( D_sum_new( span((i-1)*k*q, (i*k*q) -1  ), span::all  ) +  add_pd*noise );
    }


    arma::mat phi_hat(initial_phi.begin(), k, k*q*n_new);
    arma::mat phi_new (k, k*q*n_new, fill::zeros);

    int flag = 0;
    int l = 2;

    while( l < max_iteration){
    	if(  l == floor(0.5 * max_iteration)  ){
    		tol = (2)*tol;
    	}
    	if( l  == floor( 0.75 * max_iteration )) {
    		tol = (4/2)*tol;
    	}
        l = l+1;
        arma::mat phi_compare = phi_hat;

        for(int i = 1; i <= n_new ; i++){
        	List E(n_new);
        	for(int j = 1; j <= n_new ; j++){

        		E[j-1] = D_sum_new( span(  ( std::max( j,i )-1)*k*q  , std::max(j,i )*k*q  -1 ), span::all)  * phi_hat(span::all , span( (j-1)*k*q, j*k*q -1  )).t() ;
        	}




        	arma::mat E_new = E[0];
    		for ( int g = 1; g < n_new; g++ ) {
    			arma::mat E_temp = E[g];
    			E_new = E_new + E_temp;
    		}




    		E_new =  E_new - D_sum_new(  span( (i-1)*k*q,  i*k*q -1  ), span::all)  * phi_hat( span::all, span( (i-1)*k*q, i*k*q -1) ).t();

    		arma::mat S =  C_sum_new(  span(  (i-1)*k*q , i*k*q -1 ), span::all )  - E_new;


    		S = soft_full(S, lambda);


    		arma::mat phi_temp = D_sum_new_inv( span ((i-1)*k*q , i*k*q-1), span::all )  *  S;

    		phi_temp = phi_temp.t();


    		phi_hat( span::all ,   span( (i-1)*k*q, i*k*q -1)  ) = phi_temp;
            phi_new( span::all ,   span( (i-1)*k*q, i*k*q -1)  ) = phi_temp;


        }

        arma::mat phi_temp_soft(k,k*q*n_new, fill::zeros);
        int g = group_index.size();


        vec s = svd( Y);
        double step_size =1/pow(max(s) ,2.0);

        // Rcout << g;
        // Rcout <<phi_hat( span::all ,  span(0, k*q-1) ) <<'\n';


        for(int i = 0; i < g; i ++){
            uvec ll = group_index[i];
            arma::mat temp_3 = phi_hat( span::all ,  span(0, k*q-1) );
            vec v = temp_3.cols(ll);
            double tt = 1.0 - step_size * lambda2 / norm(v, "fro");
            // Rcout << tt;
            if(tt < 0 ){
                tt = 0;
            }
            phi_temp_soft.cols(ll) =  tt * v;
        }
        // Rcout << phi_temp_soft( span::all , span(0, k*q-1) );
        // phi_temp_soft( span::all , span(0, k*q-1) ) = soft_full(phi_hat( span::all ,  span(0, k*q-1) ),lambda2);
        arma::mat temp_1 = phi_hat( span::all ,  span(0, k*q-1) );
        arma::mat temp_1_soft = phi_temp_soft( span::all ,  span(0, k*q-1) );
        for(int z_1 = 2; z_1 <= n_new; z_1 ++){
            arma::mat temp_2_soft(k,k*q, fill::zeros);
            arma::mat temp_2 = temp_1 + phi_hat( span::all ,  span((z_1-1)*k*q, z_1*k*q-1) );
            // Rcout << temp_1_soft << '\n';
            // Rcout << temp_2 << '\n';

            for(int i = 0; i < g; i ++){
                // umat ll = group_index[i];
                 uvec ll = group_index[i];
                // uvec eids = sub2ind(size(temp_2), ll.t());
                // Rcout << eids;
                // vec v = temp_2.elem(eids);
                vec v = temp_2.cols(ll);
                double tt = 1.0 - step_size * lambda2 / norm(v, "fro");
                if(tt < 0 ){
                    tt = 0;
                }
                temp_2_soft.cols(ll) =  tt * v;
                // temp_2_soft.cols(ll) = soft_full(v, lambda2);
            }
            // mat temp_1_soft = soft_full(temp_1,lambda2);
            // mat temp_2_soft = soft_full(temp_2,lambda2);
            phi_temp_soft(   span::all ,  span( (z_1-1)*k*q , z_1*k*q  -1 ) )= temp_2_soft - temp_1_soft;
            temp_1_soft = temp_2_soft;
            temp_1 = temp_2;

        }
        phi_new  = phi_temp_soft;

        // Rcout <<phi_new <<'\n';



        arma::mat abs_temp = abs(phi_new - phi_compare);
        double max_temp = abs_temp.max();
        if ( max_temp < tol) {
            break;
        }
        if ( max_temp > tol ) {
        	phi_hat = phi_new;
        }
        if (  max_temp > pow(10.0, 5.0)) {
            flag = 1;
            break;
        }


	}


    return List::create(Named("phi.hat")= phi_hat, Named("flag")= flag);
}







// [[Rcpp::export]]
List var_break_fit_block_grouprow_cpp(NumericMatrix data, double lambda, double lambda2, int q, int max_iteration, double tol , NumericMatrix initial_phi, NumericVector blocks, NumericVector cv_index, List group_index ){

    int k = data.ncol(); int T = data.nrow(); int n_new = blocks.size() - 1;
    int n = T - q;

    // Rcout << n;

    arma::mat data_m(data.begin(), T, k);
    List Y_b(n_new);
    List y_b(n_new);

    for(int i = 0; i < n_new; i++) {
    	y_b[i] = data_m( span(blocks[i],  blocks[i+1]-1 ), span::all) ;
    }
    y_b[0] = data_m(  span(q, (blocks[1]-1) ), span::all );


    arma::mat Y( k*q, n);
    for(int i = (q-1); i < (T-1); i++) {
    	for(int j = 1; j <= q; j++){
    		Y.submat( (j-1)*k,(i-q+1), j*k-1 ,(i-q+1) ) = data_m.submat(i-j+1, 0, i-j+1,  k-1 ).t();
    	}

    }

    Y_b[0] = Y.submat( 0 ,0, k*q-1,blocks[1]-q-1  );
    for(int i = 1; i < n_new; i++) {
    	Y_b[i] = Y.submat( 0, blocks[i]-q ,k*q-1 , blocks[i+1]-q-1  ) ;
    }



    int cv_l = cv_index.size();
    if( cv_l >0){
    	for(int t_1 = 0; t_1 < cv_l; t_1 ++){
    		arma::mat yb_temp = y_b[cv_index[t_1]-1];
    		arma::mat Yb_temp = Y_b[cv_index[t_1]-1];
    		int tt =  yb_temp.n_rows;
    		//Rcout << tt1;
            y_b[cv_index[t_1]-1] = yb_temp(span(0,tt-2), span::all );
            Y_b[cv_index[t_1]-1] = Yb_temp( span::all ,span(0,tt-2));

    	}
    }

    List C(n_new);
    for(int j = 0; j < n_new; j++){
    	arma::mat yb_temp = y_b[j];
    	arma::mat Yb_temp = Y_b[j];
    	C[j] = Yb_temp * yb_temp;
    }

    arma::mat C_sum (k*q*n_new, k, fill::zeros);
    arma::mat C_temp = C[0];
    C_sum(span(0, k*q - 1) , span::all) = C_temp;
    for(int i = 2; i <= n_new ; i++){
    	arma::mat C_temp = C[i-1];
    	//Rcout <<  size(C_temp);
    	C_sum(  span((i-1)*k*q, (i*k*q) -1  ), span::all ) = C_sum( span( (i-2)*k*q,(i-1)*k*q -1 ), span::all) + C_temp;
    }
    arma::mat C_sum_new (k*q*n_new,k, fill::zeros);
    C_sum_new( span(   0, k*q - 1 ), span::all) =  C_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all );
    for(int i = 2; i <= n_new ; i++){
    	C_sum_new(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = C_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all ) - C_sum( span ((i-2)*k*q, (i-1)*k*q -1 ), span::all);
    }


    List D(n_new);
    for(int j = 0; j < n_new; j++){
    	arma::mat Yb_temp = Y_b[j];
    	D[j] = Yb_temp * Yb_temp.t();
    }

    arma::mat D_sum (k*q*n_new,k*q, fill::zeros);
    arma::mat D_temp = D[0];
    D_sum(span(0, k*q - 1) , span::all) = D_temp;
    for(int i = 2; i <= n_new ; i++){
    	arma::mat D_temp = D[i-1];
    	D_sum(  span((i-1)*k*q, (i*k*q) -1  ), span::all ) = D_sum( span( (i-2)*k*q,(i-1)*k*q -1 ), span::all) + D_temp;
    }
    arma::mat D_sum_new (k*q*n_new,k*q, fill::zeros);
    D_sum_new( span(  0, k*q - 1 ), span::all) =  D_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all );
    for(int i = 2; i <= n_new ; i++){
    	D_sum_new(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = D_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all ) - D_sum( span ((i-2)*k*q, (i-1)*k*q -1 ), span::all);
    }

    arma::mat D_sum_new_inv (k*q*n_new,k*q, fill::zeros);

    for(int i = 1; i <= n_new ; i++){
        vec eigval =  eig_sym(D_sum_new ( span((i-1)*k*q, (i*k*q)-1), span::all)  );
        double add_pd =0.0;
        double min_eigen = eigval(0);
        if(min_eigen <= 0){
            // Rprintf("Invertiable! adding noise!");
            add_pd = (10)*fabs( min_eigen);
        }
        arma::mat noise(k*q,k*q, fill::eye);
        D_sum_new_inv(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = inv( D_sum_new( span((i-1)*k*q, (i*k*q) -1  ), span::all  ) +  add_pd*noise );
    }


    arma::mat phi_hat(initial_phi.begin(), k, k*q*n_new);
    arma::mat phi_new (k, k*q*n_new, fill::zeros);

    int flag = 0;
    int l = 2;

    while( l < max_iteration){
    	if(  l == floor(0.5 * max_iteration)  ){
    		tol = (2)*tol;
    	}
    	if( l  == floor( 0.75 * max_iteration )) {
    		tol = (4/2)*tol;
    	}
        l = l+1;
        arma::mat phi_compare = phi_hat;

        for(int i = 1; i <= n_new ; i++){
        	List E(n_new);
        	for(int j = 1; j <= n_new ; j++){

        		E[j-1] = D_sum_new( span(  ( std::max( j,i )-1)*k*q  , std::max(j,i )*k*q  -1 ), span::all)  * phi_hat(span::all , span( (j-1)*k*q, j*k*q -1  )).t() ;
        	}


        	arma::mat E_new = E[0];
    		for ( int g = 1; g < n_new; g++ ) {
    			arma::mat E_temp = E[g];
    			E_new = E_new + E_temp;
    		}


    		E_new =  E_new - D_sum_new(  span( (i-1)*k*q,  i*k*q -1  ), span::all)  * phi_hat( span::all, span( (i-1)*k*q, i*k*q -1) ).t();

    		arma::mat S =  C_sum_new(  span(  (i-1)*k*q , i*k*q -1 ), span::all )  - E_new;


    		S = soft_full(S, lambda);


    		arma::mat phi_temp = D_sum_new_inv( span ((i-1)*k*q , i*k*q-1), span::all )  *  S;

    		phi_temp = phi_temp.t();


    		phi_hat( span::all ,   span( (i-1)*k*q, i*k*q -1)  ) = phi_temp;
            phi_new( span::all ,   span( (i-1)*k*q, i*k*q -1)  ) = phi_temp;


        }

        arma::mat phi_temp_soft(k,k*q*n_new, fill::zeros);
        int g = group_index.size();


        vec s = svd( Y);
        double step_size =1/pow(max(s) ,2.0);

        // Rcout << g;
        // Rcout <<phi_hat( span::all ,  span(0, k*q-1) ) <<'\n';


        for(int i = 0; i < g; i ++){
            uvec ll = group_index[i];
            // Rcout << ll;
            arma::mat temp_3 = phi_hat( span::all ,  span(0, k*q-1) );
            // Rcout << temp_3;
            // rowvec v = temp_3.rows(ll);
            arma::mat v = temp_3.rows(ll);
            // Rcout << v;
            double tt = 1.0 - step_size * lambda2 / norm(v, "fro");
            // Rcout << tt;
            if(tt < 0 ){
                tt = 0;
            }
            uvec lll = linspace<uvec>( 0 , k*q -1, k*q );
            // Rcout << lll;
            phi_temp_soft.submat(ll, lll ) =  tt * v;
        }
        // Rcout << phi_temp_soft( span::all , span(0, k*q-1) );
        // phi_temp_soft( span::all , span(0, k*q-1) ) = soft_full(phi_hat( span::all ,  span(0, k*q-1) ),lambda2);
        arma::mat temp_1 = phi_hat( span::all ,  span(0, k*q-1) );
        arma::mat temp_1_soft = phi_temp_soft( span::all ,  span(0, k*q-1) );
        for(int z_1 = 2; z_1 <= n_new; z_1 ++){
            arma::mat temp_2_soft(k,k*q, fill::zeros);
            arma::mat temp_2 = temp_1 + phi_hat( span::all ,  span((z_1-1)*k*q, z_1*k*q-1) );
            // Rcout << temp_1_soft << '\n';
            // Rcout << temp_2 << '\n';

            for(int i = 0; i < g; i ++){
                uvec ll = group_index[i];
                // rowvec v = temp_2.rows(ll);
                arma::mat v = temp_2.rows(ll);
                double tt = 1.0 - step_size * lambda2 / norm(v, "fro");
                if(tt < 0 ){
                    tt = 0;
                }
                // uvec lll = linspace<uvec>( (z_1-1)*k*q , z_1*k*q-1, k*q );
                // temp_2_soft.submat(ll, lll  ) =  tt * v;
                temp_2_soft.rows(ll ) =  tt * v;
                // temp_2_soft.cols(ll) = soft_full(v, lambda2);
            }
            // mat temp_1_soft = soft_full(temp_1,lambda2);
            // mat temp_2_soft = soft_full(temp_2,lambda2);
            phi_temp_soft(   span::all ,  span( (z_1-1)*k*q , z_1*k*q  -1 ) )= temp_2_soft - temp_1_soft;
            temp_1_soft = temp_2_soft;
            temp_1 = temp_2;

        }
        phi_new  = phi_temp_soft;

        // Rcout <<phi_new <<'\n';



        arma::mat abs_temp = abs(phi_new - phi_compare);
        double max_temp = abs_temp.max();
        if ( max_temp < tol) {
            break;
        }
        if ( max_temp > tol ) {
        	phi_hat = phi_new;
        }
        if (  max_temp > pow(10.0, 5.0)) {
            flag = 1;
            break;
        }


	}


    return List::create(Named("phi.hat")= phi_hat, Named("flag")= flag);
}




// [[Rcpp::export]]
List var_break_fit_block_groupidx_cpp(NumericMatrix data, double lambda, double lambda2, int q, int max_iteration, double tol , NumericMatrix initial_phi, NumericVector blocks, NumericVector cv_index, List group_index ){

    int k = data.ncol(); int T = data.nrow(); int n_new = blocks.size() - 1;
    int n = T - q;

    // Rcout << n;

    arma::mat data_m(data.begin(), T, k);
    List Y_b(n_new);
    List y_b(n_new);

    for(int i = 0; i < n_new; i++) {
        y_b[i] = data_m( span(blocks[i],  blocks[i+1]-1 ), span::all) ;
    }
    y_b[0] = data_m(  span(q, (blocks[1]-1) ), span::all );


    arma::mat Y( k*q, n);
    for(int i = (q-1); i < (T-1); i++) {
        for(int j = 1; j <= q; j++){
            Y.submat( (j-1)*k,(i-q+1), j*k-1 ,(i-q+1) ) = data_m.submat(i-j+1, 0, i-j+1,  k-1 ).t();
        }

    }

    Y_b[0] = Y.submat( 0 ,0, k*q-1,blocks[1]-q-1  );
    for(int i = 1; i < n_new; i++) {
        Y_b[i] = Y.submat( 0, blocks[i]-q ,k*q-1 , blocks[i+1]-q-1  ) ;
    }



    int cv_l = cv_index.size();
    if( cv_l >0){
        for(int t_1 = 0; t_1 < cv_l; t_1 ++){
            arma::mat yb_temp = y_b[cv_index[t_1]-1];
            arma::mat Yb_temp = Y_b[cv_index[t_1]-1];
            int tt =  yb_temp.n_rows;
            //Rcout << tt1;
            y_b[cv_index[t_1]-1] = yb_temp(span(0,tt-2), span::all );
            Y_b[cv_index[t_1]-1] = Yb_temp( span::all ,span(0,tt-2));

        }
    }

    List C(n_new);
    for(int j = 0; j < n_new; j++){
        arma::mat yb_temp = y_b[j];
        arma::mat Yb_temp = Y_b[j];
        C[j] = Yb_temp * yb_temp;
    }

    arma::mat C_sum (k*q*n_new, k, fill::zeros);
    arma::mat C_temp = C[0];
    C_sum(span(0, k*q - 1) , span::all) = C_temp;
    for(int i = 2; i <= n_new ; i++){
        arma::mat C_temp = C[i-1];
        //Rcout <<  size(C_temp);
        C_sum(  span((i-1)*k*q, (i*k*q) -1  ), span::all ) = C_sum( span( (i-2)*k*q,(i-1)*k*q -1 ), span::all) + C_temp;
    }
    arma::mat C_sum_new (k*q*n_new,k, fill::zeros);
    C_sum_new( span(   0, k*q - 1 ), span::all) =  C_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all );
    for(int i = 2; i <= n_new ; i++){
        C_sum_new(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = C_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all ) - C_sum( span ((i-2)*k*q, (i-1)*k*q -1 ), span::all);
    }


    List D(n_new);
    for(int j = 0; j < n_new; j++){
        arma::mat Yb_temp = Y_b[j];
        D[j] = Yb_temp * Yb_temp.t();
    }

    arma::mat D_sum (k*q*n_new,k*q, fill::zeros);
    arma::mat D_temp = D[0];
    D_sum(span(0, k*q - 1) , span::all) = D_temp;
    for(int i = 2; i <= n_new ; i++){
        arma::mat D_temp = D[i-1];
        D_sum(  span((i-1)*k*q, (i*k*q) -1  ), span::all ) = D_sum( span( (i-2)*k*q,(i-1)*k*q -1 ), span::all) + D_temp;
    }
    arma::mat D_sum_new (k*q*n_new,k*q, fill::zeros);
    D_sum_new( span(  0, k*q - 1 ), span::all) =  D_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all );
    for(int i = 2; i <= n_new ; i++){
        D_sum_new(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = D_sum(   span(  (n_new-1)*k*q,  n_new*k*q - 1  ), span::all ) - D_sum( span ((i-2)*k*q, (i-1)*k*q -1 ), span::all);
    }

    arma::mat D_sum_new_inv (k*q*n_new,k*q, fill::zeros);

    for(int i = 1; i <= n_new ; i++){
        vec eigval =  eig_sym(D_sum_new ( span((i-1)*k*q, (i*k*q)-1), span::all)  );
        double add_pd =0.0;
        double min_eigen = eigval(0);
        if(min_eigen <= 0){
            // Rprintf("Invertiable! adding noise!");
            add_pd = (10)*fabs( min_eigen);
        }
        arma::mat noise(k*q,k*q, fill::eye);
        D_sum_new_inv(  span((i-1)*k*q, (i*k*q) -1  ), span::all )  = inv( D_sum_new( span((i-1)*k*q, (i*k*q) -1  ), span::all  ) +  add_pd*noise );
    }


    arma::mat phi_hat(initial_phi.begin(), k, k*q*n_new);
    arma::mat phi_new (k, k*q*n_new, fill::zeros);

    int flag = 0;
    int l = 2;

    while( l < max_iteration){
        if(  l == floor(0.5 * max_iteration)  ){
            tol = (2)*tol;
        }
        if( l  == floor( 0.75 * max_iteration )) {
            tol = (4/2)*tol;
        }
        l = l+1;
        arma::mat phi_compare = phi_hat;

        for(int i = 1; i <= n_new ; i++){
            List E(n_new);
            for(int j = 1; j <= n_new ; j++){

                E[j-1] = D_sum_new( span(  ( std::max( j,i )-1)*k*q  , std::max(j,i )*k*q  -1 ), span::all)  * phi_hat(span::all , span( (j-1)*k*q, j*k*q -1  )).t() ;
            }


            arma::mat E_new = E[0];
            for ( int g = 1; g < n_new; g++ ) {
                arma::mat E_temp = E[g];
                E_new = E_new + E_temp;
            }


            E_new =  E_new - D_sum_new(  span( (i-1)*k*q,  i*k*q -1  ), span::all)  * phi_hat( span::all, span( (i-1)*k*q, i*k*q -1) ).t();

            arma::mat S =  C_sum_new(  span(  (i-1)*k*q , i*k*q -1 ), span::all )  - E_new;


            S = soft_full(S, lambda);


            arma::mat phi_temp = D_sum_new_inv( span ((i-1)*k*q , i*k*q-1), span::all )  *  S;

            phi_temp = phi_temp.t();


            phi_hat( span::all ,   span( (i-1)*k*q, i*k*q -1)  ) = phi_temp;
            phi_new( span::all ,   span( (i-1)*k*q, i*k*q -1)  ) = phi_temp;


        }

        arma::mat phi_temp_soft(k,k*q*n_new, fill::zeros);
        int g = group_index.size();


        vec s = svd( Y);
        double step_size =1/pow(max(s), 2.0);

        // Rcout << g;
        // Rcout <<phi_hat( span::all ,  span(0, k*q-1) ) <<'\n';


        for(int i = 0; i < g; i ++){
            uvec ll = group_index[i];
            // Rcout << "ll";
            // Rcout << ll;
            arma::mat temp_3 = phi_hat( span::all ,  span(0, k*q-1) );
            uvec lll = linspace<uvec>( 0 , 0, 1 );

            arma::mat v = temp_3.elem(ll);
            // if(i == 0){
            //     Rcout << temp_3.rows(lll);
            //     Rcout << "v";
            //     Rcout << v;

            // }

            double tt = 1.0 - step_size * lambda2 / norm(v, "fro");
            // Rcout << tt;
            if(tt < 0 ){
                tt = 0;
            }
            // uvec lll = linspace<uvec>( 0 , k*q -1, k*q );
            // Rcout << lll;
            // phi_temp_soft.submat(ll, lll ) =  tt * v;
            phi_temp_soft.elem(ll) =  tt * v;
        }
        // Rcout << phi_temp_soft( span::all , span(0, k*q-1) );
        // phi_temp_soft( span::all , span(0, k*q-1) ) = soft_full(phi_hat( span::all ,  span(0, k*q-1) ),lambda2);
        arma::mat temp_1 = phi_hat( span::all ,  span(0, k*q-1) );
        arma::mat temp_1_soft = phi_temp_soft( span::all ,  span(0, k*q-1) );
        for(int z_1 = 2; z_1 <= n_new; z_1 ++){
            arma::mat temp_2_soft(k,k*q, fill::zeros);
            arma::mat temp_2 = temp_1 + phi_hat( span::all ,  span((z_1-1)*k*q, z_1*k*q-1) );
            // Rcout << temp_1_soft << '\n';
            // Rcout << temp_2 << '\n';

            for(int i = 0; i < g; i ++){
                uvec ll = group_index[i];
                // rowvec v = temp_2.rows(ll);
                // arma::mat v = temp_2.rows(ll);
                arma::mat v = temp_2.elem(ll);
                double tt = 1.0 - step_size * lambda2 / norm(v, "fro");
                if(tt < 0 ){
                    tt = 0;
                }
                // uvec lll = linspace<uvec>( (z_1-1)*k*q , z_1*k*q-1, k*q );
                // temp_2_soft.submat(ll, lll  ) =  tt * v;
                // temp_2_soft.rows(ll ) =  tt * v;
                temp_2_soft.elem(ll ) =  tt * v;

                // temp_2_soft.cols(ll) = soft_full(v, lambda2);
            }
            // mat temp_1_soft = soft_full(temp_1,lambda2);
            // mat temp_2_soft = soft_full(temp_2,lambda2);
            phi_temp_soft(   span::all ,  span( (z_1-1)*k*q , z_1*k*q  -1 ) )= temp_2_soft - temp_1_soft;
            temp_1_soft = temp_2_soft;
            temp_1 = temp_2;

        }
        phi_new  = phi_temp_soft;

        // Rcout <<phi_new <<'\n';



        arma::mat abs_temp = abs(phi_new - phi_compare);
        double max_temp = abs_temp.max();
        if ( max_temp < tol) {
            break;
        }
        if ( max_temp > tol ) {
            phi_hat = phi_new;
        }
        if (  max_temp > pow(10.0, 5.0)) {
            flag = 1;
            break;
        }


    }


    return List::create(Named("phi.hat")= phi_hat, Named("flag")= flag);
}












//[[Rcpp::export]]
arma::mat soft_cpp(arma::mat L, arma::vec weight, double lambda){
    int width=L.n_cols;
    for(int x=0; x < width; x++) {
        double lambda_w = lambda*(1+ weight(x));
        if (L(0, x) > lambda_w)
            L(0, x) = L(0, x) - lambda_w;
        else if (L(0, x) <  - lambda_w)
            L(0, x) = L(0, x) + lambda_w;
        else
            L(0,x) = 0.0;
    }
    return L;
}

//[[Rcpp::export]]
arma::mat pred_cpp(arma::mat Y, arma::mat phi, int q, int T, int k, int h){
    arma::mat concat_Y (k,q+h, fill::zeros);
    concat_Y(span::all , span(0, q-1)) = Y(span::all , span( T-q, T-1) );
    for( int j = 0; j < h; j++){
        arma::mat temp (k, 1,fill::zeros );
        for(int i = 0; i < q; i ++){
            temp = temp + phi(span::all ,  span( i*k, (i+1)*k -1 )) * concat_Y(span::all , q+j-i-1);
        }
        concat_Y(span::all, q + j) = temp;
    }
    return concat_Y(span::all , q + h -1 );

}



// [[Rcpp::export]]
List var_lasso_brk(NumericMatrix data, NumericVector lambda, int q, int max_iteration, double tol ){
    int k = data.ncol(); int T = data.nrow(); int T_1 = T;
    int lambda_len = lambda.length();

    arma::mat iter (k, lambda_len, fill::zeros);
    arma::mat phi_hat (k,k*q, fill::zeros);
    arma::mat phi_hat_fista (max_iteration, k*q, fill::zeros);
    arma::mat phi_hat_temp (k, k*q*lambda_len, fill::zeros);
    vec pred_error =  zeros<vec>(lambda_len);

    arma::mat data_m(data.begin(), T, k);
    arma::mat Y = data_m.t();
    Y = Y(span::all, span(q, T_1-1));
    arma::mat Z(k*q,T_1-q, fill::zeros);
    for(int i = 0; i < T_1 - q; i++) {
        for(int j = 1; j <= q; j++){
            Z(  span( (j-1)*k, j*k -1 ), i )= data_m( i+q-j, span::all).t();
        }

    }

    arma::vec s = svd( Z );
    double step_size =1/pow(max(s) ,2.0);

    arma::vec weight = zeros<vec>(k*q);
    arma::mat forecast_full (k, T_1-q, fill::zeros);
    for (int ll = 0; ll < lambda_len; ll++ ){
        for (int ii = 0; ii < k; ii ++ ){
            int l = 2;
            while( l < max_iteration){
                l  = l + 1;
                // Rcout << phi_hat_fista(l-2,span::all);
                arma::mat phi_temp = phi_hat_fista(l-2,span::all) + ((l-2.0)/(l+1.0)) * (phi_hat_fista(l-2,span::all)  - phi_hat_fista(l-3,span::all) );
                //Rcout << phi_temp;
                arma::mat phi_new =  phi_temp + step_size *  (Z  * (   Y( ii, span::all) - phi_temp*Z ).t() ).t();

                phi_new  =  soft_cpp(phi_new, weight , lambda(ll) );
                arma::mat abs_temp = abs(phi_new - phi_temp);
                double max_temp = abs_temp.max();
                //Rprintf( "%f \n", max_temp);
                if ( max_temp < tol) {
                    phi_hat_temp( ii, span(  ll*k*q, (ll+1)*k*q -1 )) = phi_new;
                    break;
                }
                if ( max_temp > tol ) {
                    // Rprintf( "%i \n", l);
                    // Rprintf( "%f \n", max_temp);
                    phi_hat_fista(l-1,span::all) =  phi_new;
                    // Rcout << phi_new;
                }
            }
            iter(ii,ll)  =  l;

        }


        arma::mat forecast (k, T_1-q, fill::zeros);
        for(int j = q-1; j < T_1 - 1; j++){
            forecast(span::all, (j-q+1)) = pred_cpp( data_m.t(), phi_hat_temp(span::all ,  span( ll*k*q,(ll+1)*k*q-1) ) , q , j+1, k, 1);
            //Rcout <<  forecast(span::all, j);

        }

        forecast_full =forecast;
        pred_error(ll) = accu( pow(  data_m( span(q ,T_1-1), span::all).t() - forecast , 2.0) );
    }
    int ll_final = 1;
    phi_hat  = phi_hat_temp(span::all , span( (ll_final-1)*k*q, ll_final*k*q -1 ));


    return List::create(Named("phi.hat")= phi_hat,Named("iter")= iter, Named("pred.error")= pred_error, Named("data_m")= data_m( span(q,T_1-1), span::all).t(), Named("forecast_full")=  forecast_full );

}



// [[Rcpp::export]]
List var_lasso_brk_group(NumericMatrix data, NumericVector lambda, int q, int max_iteration, double tol, List group_index ){
    int k = data.ncol(); int T = data.nrow(); int T_1 = T;
    int lambda_len = lambda.length();

    arma::mat iter (1, lambda_len, fill::zeros);
    arma::mat phi_hat (k, k*q, fill::zeros);
    arma::mat phi_hat_old (k, k*q, fill::zeros);
    arma::mat phi_hat_temp (k, k*q*lambda_len, fill::zeros);
    vec pred_error =  zeros<vec>(lambda_len);

    arma::mat data_m(data.begin(), T, k);
    arma::mat Y = data_m.t();
    Y = Y(span::all, span(q, T_1-1));
    arma::mat Z(k*q,T_1-q, fill::zeros);
    for(int i = 0; i < T_1 - q; i++) {
        for(int j = 1; j <= q; j++){
            Z(  span( (j-1)*k, j*k -1 ), i )= data_m( i+q-j, span::all).t();
        }

    }

    arma::vec s = svd( Z );
    double step_size =1/pow(max(s) ,2.0);
    int g = group_index.size();
    arma::vec weight = zeros<vec>(k*q);
    arma::mat forecast_full (k, T_1-q, fill::zeros);
    for (int ll = 0; ll < lambda_len; ll++ ){
        int l = 2;
        arma::mat phi_new (k, k*q, fill::zeros);
        while( l < max_iteration){
            l  = l + 1;
            // update for each group
            arma::mat phi_temp = phi_hat_old;
            for(int i = 0; i < g; i ++){
                uvec gg = group_index[i];
                arma::mat phi_i = phi_temp.cols(gg);
                arma::mat Z_i = Z.rows( gg);
                // compute the residual by other groups
                // Rcout << phi_i << '\n';
                arma::mat omega = phi_i + step_size* ( Z_i * ( Y - phi_temp*Z ).t() ).t();
                // Rcout << step_size;
                // Rcout << norm(omega, "fro") ;
                double tt = 1.0 - step_size * lambda(ll) / norm(omega, "fro");
                // Rcout << tt<< '\n';
                if(tt < 0 ){
                    tt = 0;
                }
                // Rcout << tt;
                phi_new.cols(gg) =  tt * omega;

            }

            arma::mat abs_temp = abs(phi_new - phi_temp);
            double max_temp = abs_temp.max();
            // Rprintf( "%f \n", max_temp);
            if ( max_temp < tol) {
                phi_hat_temp( span::all, span(  ll*k*q, (ll+1)*k*q -1 )) = phi_new;
                break;
            }
            if ( max_temp > tol ) {
                // Rprintf( "%i \n", l);
                // Rprintf( "%f \n", max_temp);
                phi_hat_old =  phi_new;
                // Rcout << phi_new;
            }
        }
        iter(0, ll)  =  l;



        arma::mat forecast (k, T_1-q, fill::zeros);
        for(int j = q-1; j < T_1 - 1; j++){
            forecast(span::all, (j-q+1)) = pred_cpp( data_m.t(), phi_hat_temp(span::all ,  span( ll*k*q,(ll+1)*k*q-1) ) , q , j+1, k, 1);
            //Rcout <<  forecast(span::all, j);

        }

        forecast_full =forecast;
        pred_error(ll) = accu( pow(  data_m( span(q ,T_1-1), span::all).t() - forecast , 2.0) );
    }
    int ll_final = 1;
    phi_hat  = phi_hat_temp(span::all , span( (ll_final-1)*k*q, ll_final*k*q -1 ));


    return List::create(Named("phi.hat")= phi_hat,Named("iter")= iter, Named("pred.error")= pred_error, Named("data_m")= data_m( span(q,T_1-1), span::all).t(), Named("forecast_full")=  forecast_full );

}





// [[Rcpp::export]]
List var_lasso_brk_group_idx(NumericMatrix data, NumericVector lambda, int q, int max_iteration, double tol, List group_index ){
    int k = data.ncol(); int T = data.nrow(); int T_1 = T;
    int lambda_len = lambda.length();

    arma::mat iter (1, lambda_len, fill::zeros);
    arma::mat phi_hat (1, k*k*q, fill::zeros);
    arma::mat phi_hat_old (1, k*k*q, fill::zeros);
    arma::mat phi_hat_temp (1, k*k*q*lambda_len, fill::zeros);
    vec pred_error =  zeros<vec>(lambda_len);

    arma::mat data_m(data.begin(), T, k);
    arma::mat Y = data_m;
    Y = Y(span(q, T_1-1), span::all);
    // np * 1
    // Rcout << Y;
    vec y = vectorise(Y);
    // Rcout << y;
    // mat Z(k*q, T_1-q, fill::zeros);
    // for(int i = 0; i < T_1 - q; i++) {
    //     for(int j = 1; j <= q; j++){
    //         Z(  span( (j-1)*k, j*k -1 ), i ) = data_m( i+q-j, span::all).t();
    //     }
    // }
    // // nk * k^2q
    // mat Z_large = kron( Z, eye(k, k)).t();

    arma::mat Z(T_1-q, k*q, fill::zeros);
    for(int i = 0; i < T_1 - q; i++) {
        for(int j = 1; j <= q; j++){
            Z( i, span( (j-1)*k, j*k -1 )) = data_m( i+q-j, span::all);
        }
    }
    // nk * k^2q
    arma::mat Z_large = kron( eye(k, k), Z);



    vec s = svd( Z );
    double step_size = 1/pow(max(s) ,2.0);
    int g = group_index.size();
    arma::mat forecast_full (k, (T_1-q), fill::zeros);
    for (int ll = 0; ll < lambda_len; ll++ ){
        int l = 2;
        arma::mat phi_new (1 , k*k*q, fill::zeros);
        while( l < max_iteration){
            l  = l + 1;
            // update for each group (by index)
            arma::mat phi_temp = phi_hat_old;
            for(int i = 0; i < g; i ++){
                uvec gg = group_index[i];
                vec phi_i = phi_temp.elem(gg);
                arma::mat Z_i = Z_large.cols( gg);

                // compute the residual by other groups
                arma::mat omega = phi_i + step_size* Z_i.t()*( y - Z_large*phi_temp.t() );

                double tt = 1.0 - step_size * lambda(ll) / norm(omega, "fro");
                // Rcout << tt<< '\n';
                if(tt < 0 ){
                    tt = 0;
                }
                phi_new.elem(gg) =  tt * omega;
                // Rcout << tt * omega;

            }

            arma::mat abs_temp = abs(phi_new - phi_temp);
            double max_temp = abs_temp.max();
            // Rcout << 'error:';
            // Rprintf( "%f \n", max_temp);
            if ( max_temp < tol) {
                phi_hat_temp( span::all, span(  ll*k*k*q, (ll+1)*k*k*q -1 )) = phi_new;
                break;
            }
            if ( max_temp > tol ) {
                // Rprintf( "%i \n", l);
                // Rprintf( "%f \n", max_temp);
                phi_hat_old =  phi_new;
                // Rcout << phi_new;
            }
        }
        iter(0, ll)  =  l;



        arma::mat forecast (k, (T_1-q), fill::zeros);
        arma::mat tmp = reshape( phi_hat_temp(span::all ,  span( ll*k*k*q,(ll+1)*k*k*q-1) ), k*q, k ).t();
        // Rcout << phi_hat_temp(span::all ,  span( ll*k*k*q,(ll+1)*k*k*q-1) );
        // Rcout << tmp;
        for(int j = q-1; j < T_1 - 1; j++){
            forecast(span::all, (j-q+1)) = pred_cpp( data_m.t(), tmp , q , j+1, k, 1);
            //Rcout <<  forecast(span::all, j);

        }

        forecast_full = forecast;
        pred_error(ll) = accu( pow(  data_m( span(q ,T_1-1), span::all).t() - forecast , 2.0) );
    }
    int ll_final = 1;
    phi_hat  =  reshape( phi_hat_temp(span::all , span( (ll_final-1)*k*k*q, ll_final*k*k*q -1 )), k*q, k ).t();


    return List::create(Named("phi.hat")= phi_hat, Named("pred.error")= pred_error,  Named("forecast_full")=  forecast_full );

}


// [[Rcpp::export]]
List lambda_warm_up(NumericMatrix data, int q, NumericVector blocks, NumericVector cv_index ){

    int p = data.ncol(); int T = data.nrow(); int n_new = blocks.size() - 1;
    int n = T - q;


    arma::mat data_m(data.begin(), T, p);
    List Y_b(n_new);
    List y_b(n_new);

    for(int i = 0; i < n_new; i++) {
        y_b[i] = data_m( span(blocks[i],  blocks[i+1]-1 ), span::all) ;
    }
    y_b[0] = data_m(  span(q, (blocks[1]-1) ), span::all );


    arma::mat Y( p*q, n);
    for(int i = (q-1); i < (T-1); i++) {
        for(int j = 1; j <= q; j++){
            Y.submat( (j-1)*p,(i-q+1), j*p-1 ,(i-q+1) ) = data_m.submat(i-j+1, 0, i-j+1,  p-1 ).t();
        }

    }

    Y_b[0] = Y.submat( 0 ,0, p*q-1,blocks[1]-q-1  );
    for(int i = 1; i < n_new; i++) {
        Y_b[i] = Y.submat( 0, blocks[i]-q ,p*q-1 , blocks[i+1]-q-1  ) ;
    }


    int cv_l = cv_index.size();
    if( cv_l >0){
        for(int t_1 = 0; t_1 < cv_l; t_1 ++){
            arma::mat yb_temp = y_b[cv_index[t_1]-1];
            arma::mat Yb_temp = Y_b[cv_index[t_1]-1];
            int tt =  yb_temp.n_rows;
            y_b[cv_index[t_1]-1] = yb_temp(span(0,tt-2), span::all );
            Y_b[cv_index[t_1]-1] = Yb_temp( span::all ,span(0,tt-2));

        }
    }

    List C(n_new);
    for(int j = 0; j < n_new; j++){
        arma::mat yb_temp = y_b[j];
        arma::mat Yb_temp = Y_b[j];
        C[j] = Yb_temp * yb_temp;
    }

    arma::mat C_sum (p*q*n_new, p, fill::zeros);
    arma::mat C_temp = C[0];
    C_sum(span(0, p*q - 1) , span::all) = C_temp;
    for(int i = 2; i <= n_new ; i++){
        arma::mat C_temp = C[i-1];
        C_sum(  span((i-1)*p*q, (i*p*q) -1  ), span::all ) = C_sum( span( (i-2)*p*q,(i-1)*p*q -1 ), span::all) + C_temp;
    }
    arma::mat C_sum_new (p*q*n_new,p, fill::zeros);
    C_sum_new( span(   0, p*q - 1 ), span::all) =  C_sum(   span(  (n_new-1)*p*q,  n_new*p*q - 1  ), span::all );
    for(int i = 2; i <= n_new ; i++){
        C_sum_new(  span((i-1)*p*q, (i*p*q) -1  ), span::all )  = C_sum(   span(  (n_new-1)*p*q,  n_new*p*q - 1  ), span::all ) - C_sum( span ((i-2)*p*q, (i-1)*p*q -1 ), span::all);
    }

    double lambda_max = 0;
    for(int i = 1; i <= n_new; i++){
        double lambda_temp = abs(C_sum_new(  span((i-1)*p*q, (i*p*q) -1  ), span::all )).max();
        lambda_max = std::max(lambda_max,lambda_temp);

    }


    return List::create(Named("lambda_1_max")= lambda_max);


}



// [[Rcpp::export]]
List local_refine(NumericMatrix data, int q, NumericVector blocks,
                    NumericVector nums, int lb1, int ub2, NumericMatrix phi_hat_1, NumericMatrix phi_hat_2 ){

    int num_len = nums.length();
    int k = data.ncol(); int T = data.nrow();
    arma::mat data_m(data.begin(), T, k);
    arma::mat phi_hat_1_m(phi_hat_1.begin(), k, k*q);
    arma::mat phi_hat_2_m(phi_hat_2.begin(), k, k*q);
    vec sse_full =  zeros<vec>(num_len);
    #ifdef _OPENMP
        omp_set_num_threads(4);
        #pragma omp parallel for
            for (int ll = 0; ll < num_len; ll++ ){
                int num = nums(ll);
                int ub1 = num - 1;
                int len1 =  ub1 - lb1 + 1;
                arma::mat forecast1 (k, len1, fill::zeros);
                for(int j = 0; j < len1; j++){
                    int jjj = lb1 + j;
                    forecast1(span::all, j) = pred_cpp( data_m.t(), phi_hat_1_m , q , jjj - 1, k, 1);
                    // Rcout <<  forecast1(span::all, j);

                }

                double pred_error_1 = accu( pow(  data_m( span( lb1-1 , ub1-1), span::all).t() - forecast1 , 2.0) );
                // Rcout << pred_error_1;

                int lb2 = num ;
                int len2 = ub2 - lb2 + 1;

                arma::mat forecast2 (k, len2, fill::zeros);
                for(int j = 0; j < len2; j++){
                    int jjj = lb2 + j;
                    forecast2(span::all, j) = pred_cpp( data_m.t(), phi_hat_2_m , q , jjj - 1, k, 1);
                    // Rcout <<  forecast2(span::all, j);

                }

                double pred_error_2 = accu( pow(  data_m( span( lb2-1 , ub2-1), span::all).t() - forecast2 , 2.0) );
                // Rcout << pred_error_2;

                sse_full(ll) = pred_error_1 + pred_error_2;

            }
    #else
    // single-threaded version of code
        for (int ll = 0; ll < num_len; ll++ ){
                int num = nums(ll);
                int ub1 = num - 1;
                int len1 =  ub1 - lb1 + 1;
                arma::mat forecast1 (k, len1, fill::zeros);
                for(int j = 0; j < len1; j++){
                    int jjj = lb1 + j;
                    forecast1(span::all, j) = pred_cpp( data_m.t(), phi_hat_1_m , q , jjj - 1, k, 1);
                    // Rcout <<  forecast1(span::all, j);

                }

                double pred_error_1 = accu( pow(  data_m( span( lb1-1 , ub1-1), span::all).t() - forecast1 , 2.0) );
                // Rcout << pred_error_1;

                int lb2 = num ;
                int len2 = ub2 - lb2 + 1;

                arma::mat forecast2 (k, len2, fill::zeros);
                for(int j = 0; j < len2; j++){
                    int jjj = lb2 + j;
                    forecast2(span::all, j) = pred_cpp( data_m.t(), phi_hat_2_m , q , jjj - 1, k, 1);
                    // Rcout <<  forecast2(span::all, j);

                }

                double pred_error_2 = accu( pow(  data_m( span( lb2-1 , ub2-1), span::all).t() - forecast2 , 2.0) );
                // Rcout << pred_error_2;

                sse_full(ll) = pred_error_1 + pred_error_2;

            }
    #endif
    // for (int ll = 0; ll < num_len; ll++ ){
    //     int num = nums(ll);
    //     int ub1 = num - 1;
    //     int len1 =  ub1 - lb1 + 1;
    //     mat forecast1 (k, len1, fill::zeros);
    //     for(int j = 0; j < len1; j++){
    //         int jjj = lb1 + j;
    //         forecast1(span::all, j) = pred_cpp( data_m.t(), phi_hat_1_m , q , jjj - 1, k, 1);
    //         // Rcout <<  forecast1(span::all, j);

    //     }

    //     double pred_error_1 = accu( pow(  data_m( span( lb1-1 , ub1-1), span::all).t() - forecast1 , 2) );
    //     // Rcout << pred_error_1;

    //     int lb2 = num ;
    //     int len2 = ub2 - lb2 + 1;

    //     mat forecast2 (k, len2, fill::zeros);
    //     for(int j = 0; j < len2; j++){
    //         int jjj = lb2 + j;
    //         forecast2(span::all, j) = pred_cpp( data_m.t(), phi_hat_2_m , q , jjj - 1, k, 1);
    //         // Rcout <<  forecast2(span::all, j);

    //     }

    //     double pred_error_2 = accu( pow(  data_m( span( lb2-1 , ub2-1), span::all).t() - forecast2 , 2) );
    //     // Rcout << pred_error_2;

    //     sse_full(ll) = pred_error_1 + pred_error_2;

    // }


    return List::create(Named("sse_full")= sse_full);

}
