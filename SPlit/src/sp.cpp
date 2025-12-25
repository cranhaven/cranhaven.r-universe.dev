// [[Rcpp::plugins("cpp11")]]
#include <RcppArmadillo.h>
#include <iostream>
#include <cmath>
#include <vector>
#include <float.h>
#include <random>
#include <chrono>

int parallel_threads = 1;
#ifdef _OPENMP
	#include <omp.h>
#endif


void printProgress(int percent)
{
	if(parallel_threads == 1)
		Rcpp::Rcout << "\rOptimizing <1 thread> [" << std::string(percent / 5, '+') << std::string(100 / 5 - percent / 5, ' ') << "] " << percent << "%";
	else
		Rcpp::Rcout << "\rOptimizing <" << parallel_threads << " threads> [" << std::string(percent / 5, '+') << std::string(100 / 5 - percent / 5, ' ') << "] " << percent << "%";
}


// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
Rcpp::NumericMatrix sp_cpp(std::size_t& des_num, int dim_num, Rcpp::NumericMatrix& ini, Rcpp::NumericMatrix& distsamp, bool thin, 
	Rcpp::NumericMatrix& bd, std::size_t& point_num, int it_max, double tol, int num_proc, double n0, Rcpp::NumericVector& wts, bool rnd_flg)
{
	#ifdef _OPENMP
		omp_set_num_threads(num_proc);
		parallel_threads = num_proc;
	#endif

	int it_num = 0; 
	bool cont = true;  
	arma::vec curconst(des_num); 
	curconst.fill(0.0);
	arma::vec runconst(des_num); 
	runconst.fill(0.0);
	arma::vec runconst_up(des_num); 
	runconst_up.fill(0.0);
	std::vector<double> prevdes(des_num * dim_num);
	std::default_random_engine generator(std::chrono::high_resolution_clock::now().time_since_epoch().count());
	std::vector<double> des(des_num * dim_num);
	std::vector<double> des_up(des_num * dim_num);
	for(std::size_t i = 0; i < des_num; i++)
		for(int j = 0; j < dim_num; j++)
			des[j + i * dim_num] = ini(i, j);
  
	double nug = 0.0;
	int percent_complete = 0;
	while(cont)
	{
		int percent = (100 * (it_num + 1)) / it_max;
		if(percent > percent_complete)
		{
			printProgress(percent);
			percent_complete = percent;
		}
		
    	curconst.fill(0.0);
    	bool nanflg = false;
	    for(std::size_t i = 0; i < des_num; i++)
			for(int j = 0; j < dim_num; j++)
				prevdes[j + i * dim_num] = des[j + i * dim_num];
    
	    arma::mat rnd(point_num, dim_num); 
	    arma::vec rnd_wts(point_num); 
	    std::uniform_int_distribution<int> uddist(0, distsamp.nrow() - 1);
	    for(std::size_t i = 0; i < point_num; i++)
	    {
			std::size_t ss;
			if(rnd_flg)
				ss = uddist(generator);
			else
				ss = i;

			for(int j = 0; j < dim_num; j++)
				rnd(i, j) = distsamp(ss, j);
			
			rnd_wts(i) = wts(ss);
	    }
  
		#pragma omp parallel for
		for(std::size_t m = 0; m < des_num; m++)
		{
			arma::vec xprime(dim_num);
			xprime.fill(0.0);
			arma::vec tmpvec(dim_num);
			tmpvec.fill(0.0);
			for(std::size_t o = 0; o < des_num; o++)
				if(o != m)
				{
					double tmptol = 0.0;
					for(int n = 0; n < dim_num; n++)
					{
						tmpvec(n) = prevdes[n + m * dim_num] - prevdes[n + o * dim_num];
						tmptol += pow(tmpvec(n), 2.0);
					}

					tmptol = sqrt(tmptol);
					for(int n = 0; n < dim_num; n++)
						xprime(n) += tmpvec(n) / (tmptol + nug * DBL_MIN);

				}

			for(int n = 0; n < dim_num; n++)
				xprime(n) = xprime(n) * ((double)point_num / (double)des_num);

			for(std::size_t o = 0; o < point_num; o++)
			{
				double tmptol = 0.0; 
				for(int n = 0; n < dim_num; n++)
					tmptol += pow(rnd(o, n) - prevdes[n + m * dim_num], 2.0);
				
				tmptol = sqrt(tmptol);
				curconst(m) += rnd_wts(o) / (tmptol + (nug * DBL_MIN));
				for(int n = 0; n < dim_num; n++)
					xprime(n) += rnd_wts(o) * rnd(o, n) / (tmptol + (nug * DBL_MIN));
				
			}

			double denom = (1.0 - (n0 / (it_num + n0))) * runconst(m) + (n0 / (it_num + n0)) * curconst(m);
			for (int n = 0; n < dim_num; n++)
				xprime(n) = ((1.0 - (n0 / (it_num + n0))) * runconst(m) * prevdes[n + m * dim_num] + (n0 / (it_num + n0)) * xprime(n) ) / denom;

			for(int n = 0; n < dim_num; n++)
				xprime(n) = std::min(std::max(xprime(n), bd(n, 0)), bd(n, 1));

			for(int n = 0; n < dim_num; n++)
			{
				des_up[n + m * dim_num] = xprime(n);
				if(std::isnan(xprime(n)))
			  		nanflg = true;
			}

	    	runconst_up(m) = (1 - (n0 / (it_num + n0))) * runconst(m) + (n0 / (it_num + n0)) * curconst(m);
    	}
    
		if(nanflg)
		{
			nug += 1.0;
			runconst.fill(0.0);
			Rcpp::Rcout << "\nNumerical instablities encountered; resetting optimization.\n";
		}
		else
		{
			des = des_up; 
			runconst = runconst_up;
		}
    
		it_num++;
		double maxdiff = 0.0;
		double rundiff = 0.0;
		for(std::size_t n = 0; n < des_num; n++)
		{
			rundiff = 0.0;
			for (int o = 0; o < dim_num; o++)
				rundiff += std::pow(des[o + n * dim_num] - prevdes[o + n * dim_num], 2.0);

			maxdiff = std::max(maxdiff, rundiff);
		}

		if((maxdiff < tol) && (!nanflg))
		{
			cont = false;
			Rcpp::Rcout << "\nTolerance level reached.";
		}

		if ((it_num >= it_max) && (!nanflg) )
		{
			cont = false;
		}
    
	}

	Rcpp::Rcout << "\n";
	Rcpp::NumericMatrix retdes(des_num, dim_num);
	for(int j = 0; j < dim_num; j++)
		for(std::size_t i = 0; i < des_num; i++)
			retdes(i, j) = des[j + i * dim_num];

	return(retdes);
}





