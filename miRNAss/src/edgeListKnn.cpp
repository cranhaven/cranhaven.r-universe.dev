#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <cmath>

#if defined _OPENMP && !defined __APPLE__
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#endif

using namespace Rcpp;

#define matrix std::vector< std::vector<double> >
#define edge_iter std::vector<edge>::iterator
NumericMatrix edge_list_knn(const matrix& x, const std::vector<double>& y,
                            const int nn);

// [[Rcpp::export(.edgeListKnn)]]
NumericMatrix edgeListKnn(const NumericMatrix& x, const NumericVector& y,
                          const int nn, const int threadNumber) {
	matrix X(x.nrow());
	for(unsigned int i=0; i<X.size(); i++) {
		X[i].resize(x.ncol());
		for(unsigned int j=0; j<X[i].size(); j++) {
			X[i][j] = x(i,j);
		}
	}
	std::vector<double> Y = as< std::vector<double> >(y);

#if defined _OPENMP && !defined __APPLE__
	if(threadNumber > 0)
	    omp_set_num_threads(threadNumber);
#endif
	return edge_list_knn(X, Y, nn);
}


struct edge {
	double dist;
	int i, j;
	edge() {};
	edge(double d, int i, int j) : dist(d), i(i), j(j) {}
	void swap(edge& b) {
		double tmpd=dist;
		dist = b.dist;
		b.dist =tmpd;

		int tmpo=i;
		i=b.i;
		b.i=tmpo;

		tmpo=j;
		j=b.j;
		b.j=tmpo;
	}
	void assign(double d_, int i_, int j_) {
		dist=d_;
		i=i_;
		j=j_;
	}
	bool operator<(const edge& b) const {
		return dist < b.dist;
	};
	bool operator<=(const edge& b) const {
		return dist <= b.dist;
	};
	bool operator==(const edge& b) const {
		return i==b.i && j==b.j;
	};
};

inline bool edge_order(const edge& a, const edge& b) {
	if(a.i != b.i)
		return a.i < b.i;
	else if(a.j != b.j)
		return a.j < b.j;
	else
		return false;
}

inline edge_iter partition(edge_iter start, edge_iter end) {
	if(start==end)
		return start;
	edge_iter pos=end+1,j;
	for(j=start+1; j<=end; j++) {
		if((*j) <= (*start) && pos!=end+1)
			j->swap(*(pos++));
		else if(pos==end+1 && (*start) <= (*j))
			pos=j;
	}
	pos--;
	start->swap(*pos);
	return pos;
}

void order_statistic(edge_iter start, edge_iter end, int k) {
	edge_iter pivot= start + distance(start,end)/2, p;
	pivot->swap(*start);
	p = partition(start, end);
	int position = distance(start, p)+1;
	if(k==position)
		return;
	else if(k<position)
		order_statistic(start, p-1, k);
	else
		order_statistic(p+1, end, k-position);
}

NumericMatrix edge_list_knn(const matrix& x, const std::vector<double>& y,
                            const int nn) {
	const unsigned int nrow = x.size(), ncol=x[0].size();
	std::vector<edge> all;
	all.reserve(nn*nrow);

#pragma omp parallel shared(all)
{

#if defined _OPENMP && !defined __APPLE__
	std::vector<edge> e(nn*nrow/omp_get_num_threads() + nn*nrow);
#else
	std::vector<edge> e(2*nn*nrow);
#endif


	edge_iter ee=e.begin(), eb;
#pragma omp for schedule(static)
	for(unsigned int i=0; i<nrow; i++) {
		eb=ee;
		for(unsigned int j=0; j<i; j++) {
			if(y[i]*y[j] >= 0) {
				double d=0;
				for(int k=0;k<ncol;k++)
					d += pow(x[i][k] - x[j][k],2);
				(ee++)->assign(d, j, i);
			}
		}
		for(unsigned int j=i+1; j<nrow; j++) {
			if(y[i]*y[j] >= 0) {
				double d=0;
				for(unsigned int k=0;k<ncol;k++)
					d += pow(x[i][k] - x[j][k],2);
				(ee++)->assign(d, i, j);
			}
		}
		order_statistic(eb, ee-1, nn);
		ee = eb+nn;
	}
#pragma omp critical
	all.insert(all.end(), e.begin(), ee);
}
sort(all.begin(), all.end(), edge_order);
std::vector<edge>::iterator it = unique(all.begin(), all.end());
all.resize(distance(all.begin(),it));

double mean=0;
for(int i=0; i<all.size(); i++)
	mean += all[i].dist;
mean /= all.size();

NumericMatrix el(all.size(), 3);
for(int i=0; i<all.size(); i++) {
	el(i, 0) = all[i].i + 1;
	el(i, 1) = all[i].j + 1;
	el(i, 2) = 1 / (1 + all[i].dist/mean);
}
return el;
}


