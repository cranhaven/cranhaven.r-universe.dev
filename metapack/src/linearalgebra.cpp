#include <stdio.h>
#include <cmath>
#include <RcppArmadillo.h>
#include <Rdefines.h>
#include "linearalgebra.h"

arma::vec vecr(const arma::mat& X) {
	const int J = X.n_cols;
	arma::vec vphi((J*(J-1))/2, arma::fill::zeros);
	for (int i = 0; i < J-1; ++i) {
		for (int j = i+1; j < J; ++j) {
			int k = (J*(J-1)/2) - (J-i)*((J-i)-1)/2 + j - i - 1;
			vphi(k) = X(i,j);
		}
	}
	return vphi;
}

/******************************************************************************************
 * vecrinv
 * DESCRIPTION: Restore a vector of upper-diagonal elements to a matrix
                Does not fill in the diagonal elements; the diagonal entries will be zeros
******************************************************************************************/
arma::mat vecrinv(const arma::vec& X, const int& J) {
	const int vdim = X.n_elem;
	arma::mat R(J, J, arma::fill::zeros);
	for (int kk = 0; kk < vdim; ++kk) {
		int iR = J - 2 - static_cast<int>(std::sqrt(-8.0*static_cast<double>(kk) + 4.0*static_cast<double>(J*(J-1))-7.0)/2.0 - 0.5); // row index
		int iC = kk + iR + 1 - (J*(J-1))/2 + ((J-iR)*((J-iR)-1))/2; // column index
		R(iR,iC) = X(kk);
		R(iC,iR) = X(kk);
	}
	return R;
}

arma::vec vecl(const arma::mat& X) {
	int n = X.n_rows;
	arma::vec out(n*(n-1)/2, arma::fill::zeros);
	for (int j = 0; j < n-1; ++j) {
		for (int i = j+1; i < n; ++i) {
			out((n-1)*j - (j-1)*j/2 + i - 1 - j) = X(i, j);
		}
	}
	return out;
}

/*
Row-wise half-vectorization
stored by rows in lower triangular form as a one dimensional array, 
//    in the sequence
//    A(1,1),
//    A(2,1), A(2,2),
//    A(3,1), A(3,2), A(3,3), and so on.
*/
arma::vec vechr(const arma::mat& X) {
	int n = X.n_rows;
	arma::vec out(n*(n+1)/2, arma::fill::zeros);
	out(0) = X(0,0);
	int index = 1;
	for (int i = 1; i < n; ++i) {
		for (int j = 0; j < i+1; ++j) {
			out(index + j) = X(i,j);
		}
		index +=  i+1;
	}
	return out;
}

arma::vec vech(const arma::mat& X) {
	int n = X.n_rows;
	arma::vec out(n*(n+1)/2, arma::fill::zeros);
	for (int j = 0; j < n; ++j) {
		for (int i = j; i < n; ++i) {
			out(n*j - (j-1)*j/2 + i - j) = X(i, j);
		}
	}
	return out;
}

arma::mat vechinv(const arma::vec& v, const int& n) {
	arma::mat out(n, n, arma::fill::zeros);
	int count1 = 0;
	int count2 = n-1;
	for (int i = 0; i < n-1; ++i) {
		arma::vec vv = v(arma::span(count1, count2));
		out(arma::span(i+1, n-1),i) = vv.tail(n-1-i);
		out(i, arma::span(i, n-1)) = vv.t();
		count1 = count2 + 1;
		count2 += n-1-i;
	}
	out(n-1,n-1) = v(n*(n+1)/2-1);
	return out;
}

/*****************************************
Convert partial correlation to correlation
 * pRho must have unit diagonal elements
*****************************************/
arma::mat pRho_to_Rho(arma::mat& pRho) {
	const int nT = pRho.n_rows;
	arma::mat Rho = pRho;

	for (int iL=2; iL < nT; ++iL) {
		arma::mat rmat(iL-1,iL-1, arma::fill::zeros);

		for (int iR=1; iR < nT-iL+1; ++iR) {
			arma::vec rvec1(nT-2, arma::fill::zeros);
			arma::vec rvec3(nT-2, arma::fill::zeros);

			for (int i1=1; i1 < iL; ++i1) {
				rvec1(i1-1) = Rho(iR-1, iR+i1-1);
				rvec3(i1-1) = Rho(iR+i1-1, iR+iL-1);
			}
			double rr11 = 0.0;
			double rr13 = 0.0;
			double rr33 = 0.0;

			for (int i1=1; i1 < iL; ++i1) {
				for (int j1=1; j1 < iL; ++j1) {
					rmat(i1-1,j1-1) = Rho(iR+i1-1, iR+j1-1);
				}
			}
			arma::mat rmatinv = rmat.i();
			for (int i1=1; i1 < iL; ++i1) {
				for (int j1=1; j1 < iL; ++j1) {
					rr11 += rvec1(i1-1) * rmatinv(i1-1,j1-1) * rvec1(j1-1);
					rr13 += rvec1(i1-1) * rmatinv(i1-1,j1-1) * rvec3(j1-1);
					rr33 += rvec3(i1-1) * rmatinv(i1-1,j1-1) * rvec3(j1-1);
				}
			}
			Rho(iR-1, iR+iL-1) = rr13 + pRho(iR-1,iR+iL-1) * std::sqrt((1.0 - rr11) * (1.0 - rr33));
			Rho(iR+iL-1,iR-1) = Rho(iR-1, iR+iL-1);
		}
	}
	return Rho;
}

arma::mat Rho_to_pRho(arma::mat& Rho) {
	const int nT = Rho.n_rows;
	arma::mat pRho = Rho;

	for (int iL=nT-1; iL > 1; --iL) {
		arma::mat rmat(iL-1,iL-1, arma::fill::zeros);

		for (int iR=1; iR < nT-iL+1; ++iR) {
			arma::vec rvec1(nT-2, arma::fill::zeros);
			arma::vec rvec3(nT-2, arma::fill::zeros);

			for (int i1=1; i1 < iL; ++i1) {
				rvec1(i1-1) = pRho(iR-1, iR+i1-1);
				rvec3(i1-1) = pRho(iR+i1-1, iR+iL-1);
			}
			double rr11 = 0.0;
			double rr13 = 0.0;
			double rr33 = 0.0;

			for (int i1=1; i1 < iL; ++i1) {
				for (int j1=1; j1 < iL; ++j1) {
					rmat(i1-1,j1-1) = pRho(iR+i1-1, iR+j1-1);
				}
			}
			arma::mat rmatinv = rmat.i();
			for (int i1=1; i1 < iL; ++i1) {
				for (int j1=1; j1 < iL; ++j1) {
					rr11 += rvec1(i1-1) * rmatinv(i1-1,j1-1) * rvec1(j1-1);
					rr13 += rvec1(i1-1) * rmatinv(i1-1,j1-1) * rvec3(j1-1);
					rr33 += rvec3(i1-1) * rmatinv(i1-1,j1-1) * rvec3(j1-1);
				}
			}
			pRho(iR-1, iR+iL-1) = (Rho(iR-1, iR+iL-1) - rr13) / std::sqrt((1.0 - rr11) * (1.0 - rr33));
			pRho(iR+iL-1,iR-1) = pRho(iR-1, iR+iL-1);
		}
	}
	return pRho;
}

arma::mat find_diag(const arma::vec x, const double &TOL)
{
	int d = x.n_elem;
	int n = static_cast<int>(1.0 + std::sqrt(1.0 + 8.0 * static_cast<double>(d))) / 2; // dimension of corresponding matrix
	arma::mat A = vecrinv(x, n);															   // = log(rho)
	arma::vec diag = arma::diagvec(A);
	double m = std::sqrt(static_cast<double>(n));
	double dist = m;
	while (dist > m * TOL)
	{
		arma::vec ld = arma::log(arma::diagvec(arma::expmat(A)));
		diag -= ld;
		A.diag() = diag;
		dist = arma::norm(ld);
	}
	return A;
}

// hyperspherical reparameterization: Rho = B * B'
arma::mat constructB(const arma::mat &Rangle)
{
	const int J = Rangle.n_cols;
	arma::mat B(J, J, arma::fill::zeros);
	for (int j = 0; j < J; ++j)
	{
		for (int i = j; i < J; ++i)
		{
			if (i == 0 && i == j)
			{
				B(i, i) = 1.0;
			}
			else if (i != j)
			{
				B(i, j) = std::cos(Rangle(i, j));
				for (int l = 0; l < j; ++l)
				{
					B(i, j) *= std::sin(Rangle(i, l));
				}
			}
			else
			{
				B(i, i) = 1.0;
				for (int l = 0; l < i; ++l)
				{
					B(i, i) *= std::sin(Rangle(i, l));
				}
			}
		}
	}
	return B;
}
