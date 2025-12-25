// [[Rcpp::plugins("cpp11")]]
#include <memory>
#include <vector>
#include <Rcpp.h>
#include <cmath>
#include <iomanip>
#include <iostream>
#include "nanoflann.hpp"

class DF
{
private:
	std::shared_ptr<Rcpp::NumericMatrix> df_;

public:
	void import_data(Rcpp::NumericMatrix& df)
	{
		df_ = std::make_shared<Rcpp::NumericMatrix>(Rcpp::transpose(df));
	}

	std::size_t kdtree_get_point_count() const
	{
		return df_->cols();
	}

	double kdtree_get_pt(const std::size_t idx, const std::size_t dim) const 
	{
		return (*df_)(dim, idx);
	}

	const double* get_row(const std::size_t idx) const
	{
		return &(*df_)(0, idx);
	}

	template <class BBOX>
	bool kdtree_get_bbox(BBOX&) const 
	{ 
		return false; 
	}
};


typedef nanoflann::KDTreeSingleIndexDynamicAdaptor<nanoflann::L2_Adaptor<double, DF>, DF, -1, std::size_t> kdTree;


class KDTree
{
private:
	const std::size_t dim_;
	const std::size_t N_;
	const std::size_t n_;
	DF data_;
	DF sp_;

public:
	KDTree(Rcpp::NumericMatrix& data, Rcpp::NumericMatrix& sp) : 
	dim_(data.cols()), N_(data.rows()), n_(sp.rows())
	{
		if(static_cast<unsigned int>(sp.cols()) != dim_)
			Rcpp::Rcerr << "\nDimensions do not match.\n";
		else
		{
			data_.import_data(data);
			sp_.import_data(sp);
		}
	}

	std::vector<std::size_t> subsample_indices_sequential()
	{
		kdTree tree(dim_, data_, nanoflann::KDTreeSingleIndexAdaptorParams(8));
		
		nanoflann::KNNResultSet<double> resultSet(1);
		std::size_t index;
		double distance;
		
		std::vector<std::size_t> indices;
		indices.reserve(n_);

		for(std::size_t i = 0; i < n_; i++)
		{
			resultSet.init(&index, &distance);
			tree.findNeighbors(resultSet, sp_.get_row(i), nanoflann::SearchParams());
			indices.push_back(index + 1);
			tree.removePoint(index);
		}

		return indices;
	}
};


// [[Rcpp::export]]
std::vector<std::size_t> subsample(Rcpp::NumericMatrix& data, Rcpp::NumericMatrix& points)
{
	KDTree kdt(data, points);
	return kdt.subsample_indices_sequential();
}

