#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// simulate from U(a,b)
double unifrnd(double a,double b) {
	double rand = R::runif(0,1);
	return ((b-a)*rand + a);
}

// sample from categorical distribution with equal probabilities
int sample_equal(int n) {
    double cumsum = 0.0;
    double inc;
    int i;
    double p = unifrnd(0.0, 1.0);
    inc = 1.0/(1.0*n);
    for (i = 0; i < n; i++) {
        cumsum += inc;
        if (p <= cumsum) {
            return(i);
        }
    }
	throw(Rcpp::exception("sample_equal finished without a successful sample."));
    //Rcpp::Rcout << "sample_equal finished without a successful sample" << std::endl;
    return n;
}

// the model simulation
int simulate(int *x, int *rows, int *cols, double Pm, double Pp, int N, int nrow, int ncol, int sim_iters){
    int i, r, row, col, row_prop, col_prop, K, s;
	double u;
    int num_motility_prop = 0, num_prolif_prop = 0;

    K=N;

    // simulate over discrete time steps
    for (s=0; s<sim_iters; s++) {
        // potential motility event for each cell
        for (i=0; i<K; i++) {
            if (unifrnd(0.0,1.0)<Pm) { //attempt a move with some probability (inputted probability of motility)
                num_motility_prop++;
                r = sample_equal(K);
                row = rows[r];
                col = cols[r];
                u = unifrnd(0.0,1.0); //randomly choose location
                if (u < 0.25) {
                    row_prop = row-1;
                    col_prop = col;
                }
                else if (u > 0.25 && u < 0.5) {
                    row_prop = row+1;
                    col_prop = col;
                }
                else if (u > 0.5 && u < 0.75) {
                    row_prop = row;
                    col_prop = col-1;
                }
				else {
                    row_prop = row;
                    col_prop = col+1;
                }

				if (row_prop >= 0 && row_prop <= (nrow-1) && col_prop >= 0 && col_prop <= (ncol-1)) { //if chosen location is empty and within bounds then move is successful
                    if (x[row_prop + nrow*col_prop] == 0) {
                        // move
                        x[row_prop + nrow*col_prop] = 1;
                        x[row + nrow*col] = 0;
                        rows[r] = row_prop;
                        cols[r] = col_prop;
                    }
                }
            }
        }

        // potential proliferation event for each cell
        for (i=0; i<K; i++) {
            if (unifrnd(0.0,1.0) < Pp) { //attempt a proliferation event with some probability (inputted probability of proliferation)
                num_prolif_prop++;
                r = sample_equal(K);
                row = rows[r];
                col = cols[r];
                u = unifrnd(0.0,1.0);  //randomly choose location and if empty then move is successful
                if (u < 0.25) {
                    row_prop = row-1;
                    col_prop = col;
                }
                else if (u > 0.25 && u < 0.5) {
                    row_prop = row+1;
                    col_prop = col;
                }
                else if (u > 0.5 && u < 0.75) {
                    row_prop = row;
                    col_prop = col-1;
                }else {
                    row_prop = row;
					col_prop = col+1;
                }

                if (row_prop >= 0 && row_prop <= (nrow-1) && col_prop >= 0 && col_prop <= (ncol-1)) { //if chosen location is empty and within bounds then proliferation is successful
                    if (x[row_prop + nrow*col_prop]==0) {
                        // proliferate
                        x[row_prop + nrow*col_prop] = 1;
                        rows[K] = row_prop;
                        cols[K] = col_prop;
                        K++;
                        if (K >= nrow*ncol) {
                            // then too many birthing of cells to store in array (this parameter will end up being rejected)
                            return(K);
                        }
                    }
                }
            }
        }
    }
    return(K);
}

//' Simulation function of the cell biology example
//'
//' @description Simulation function of the cell biology example.
//' @param x The initial matrix of cell presences of size \code{rows}
//' \eqn{\times} \code{cols}.
//' @param Pm Parameter \eqn{P_m},
//' the probability of cell movement.
//' @param Pp Parameter \eqn{P_p},
//' the probability of cell proliferation.
//' @inheritParams cell
//' @return  A \code{rows} \eqn{\times} \code{cols}
//'  \eqn{\times} \code{num_obs} array
//'   of the cell presences at times \code{1:num_obs} (not time 0).
//' @export
// [[Rcpp::export]]
arma::ucube simulate_cell(LogicalMatrix x, IntegerVector rows, IntegerVector cols, double Pm, double Pp, int sim_iters, int num_obs) {
    int *xc, *colsc, *rowsc;
    int nrow, ncol, N, i, j, k, cap;

    // read in the data
    nrow = x.nrow();
    ncol = x.ncol();
    N = rows.length();
	cap = nrow*ncol;

    // allocate double the space required
    rowsc = (int*)malloc(cap*sizeof(int));
    colsc = (int*)malloc(cap*sizeof(int));

    for (i = 0; i<N; i++) {
        rowsc[i] = rows[i];
        colsc[i] = cols[i];
    }

    // output (collection of binary matrices)
    arma::ucube xr(nrow, ncol, num_obs);
    xr.zeros();

    xc = (int*)malloc(cap*sizeof(int));

    for (i=0; i<nrow; i++) {
        for (j=0; j<ncol; j++) {
            xc[i + nrow*j] = x(i, j);
        }
    }

    // perform simulation over num_obs time points
    for (k=0; k<num_obs; k++) {
		if (N < cap){
			N = simulate(xc, rowsc, colsc, Pm, Pp, N, nrow, ncol, sim_iters);
		}
        // store result in output object
        for (i=0; i<nrow; i++) {
            for (j=0; j<ncol; j++) {
                xr(i ,j ,k) = xc[i + nrow*j];
            }
        }
    }

    // clean memory
    free(rowsc);
    free(colsc);
    free(xc);

    return (xr);
}


///////////////////////


