#include <Rcpp.h>
#include <unistd.h>
using namespace Rcpp;

//' max_heapify: Part of DCEM package.
//'
//' Implements the creation of max heap. Internally called by the \code{dcem_star_train}.
//'
//' @param data (NumericMatrix): The dataset provided by the user.
//' @param index (int): The index of the data point.
//' @param num_data (numeric): The total number of observations in the data.
//'
//' @return A NumericMatrix with the max heap property.
//'
//' @usage
//' max_heapify(data, index, num_data)
//'
//' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mehmet Dalkilic
//'
// [[Rcpp::export]]
NumericMatrix max_heapify(NumericMatrix data, int index, int num_data){

  int left = 2*index + 1;
  int right = (2*index) + 2;
  int largest = index;
  int key=0, val=0 ;


  if ( (left <= num_data) && (data(left, 0) > data(largest, 0)) ){
    largest = left;
  }

  if ( (right <= num_data) && (data(right, 0) > data(largest, 0)) ){
    largest = right;
  }

  if (largest != index)
  {
    key  = data(largest, 0);
    val  = data(largest, 1);

    data(largest, 0) = data(index, 0);
    data(largest, 1) = data(index, 1);

    data(index, 0) = key;
    data(index, 1) = val;

    data = max_heapify(data, largest, num_data);
  }

  return data;
}

//' build_heap: Part of DCEM package.
//'
//' Implements the creation of heap. Internally called by the \code{dcem_star_train}.
//'
//' @param data (NumericMatrix): The dataset provided by the user.
//'
//' @return A NumericMatrix with the max heap property.
//'
//' @usage
//' build_heap(data)
//'
//' @author Parichit Sharma \email{parishar@iu.edu}, Hasan Kurban, Mehmet Dalkilic
//'
// [[Rcpp::export]]
NumericMatrix build_heap(NumericMatrix data){

  if (data.nrow() == 1){
    Rcout << "only 1 element in the heap" << std::endl;
    return data;
  }
  int num_data = data.nrow()/2;
  while (num_data>=0){
    data = max_heapify(data, num_data, data.nrow()-1);
    num_data = num_data-1 ;
  }
  return data;
}
