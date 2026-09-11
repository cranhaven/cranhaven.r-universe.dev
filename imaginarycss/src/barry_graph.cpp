#include <Rcpp.h>
#include <barry/barry.hpp>
using namespace Rcpp;

// #include "geese-utils.h"

// [[Rcpp::export(rng = false, name = "new_barry_graph_cpp")]]
SEXP new_barry_graph(
    int n,
    const IntegerVector & source,
    const IntegerVector & target,
    int netsize,
    const IntegerVector & endpoints
    ) {
  
  // Turning source, target, and endpoint to vector< size_t >
  std::vector< size_t > source_(source.begin(), source.end());
  std::vector< size_t > target_(target.begin(), target.end());

  // Creating network of size six with five ties
  Rcpp::XPtr< netcounters::Network > dat(
      new netcounters::Network(n, n, source_, target_)
      );
  
  // Adding data
  dat->set_data(new netcounters::NetworkData(), true);
  dat->D().directed = true;

  dat.attr("class")     = "barry_graph";
  dat.attr("netsize")   = netsize;
  dat.attr("endpoints") = endpoints;
  
  return dat;
  
}

// [[Rcpp::export(name = "print_barry_graph_cpp", invisible = true, rng = false)]]
int print_barry_graph(SEXP x, int n) {
  
  Rcpp::XPtr< netcounters::Network >ptr(x);
  
  
  // Retrieving an attribute from x
  IntegerVector end = ptr.attr("endpoints");
  auto nnets = end.size();

  int netsize = ptr.attr("netsize");
  
  Rprintf(
    "A barry_graph with %i networks of size %i\n.",
    static_cast<int>(nnets) + 1,
    netsize
  ); 

  ptr->print_n(
    static_cast<size_t>(n),
    static_cast<size_t>(n),
    ""
  );
  
  return 0;
  
}

#define APPEND_COUNTER(a) netcounters:: a <netcounters::Network>(\
  counter.get_counters(), n, end, counter_type );

//' Add a counter for reciprocity errors
//' @param x An object of class [barry_graph].
//' @param counter_type An integer indicating the type of census to compute (see details).
//' @details
//' We can also separate the counts as a function of whether the perceiver is looking
//' into all ties, only ties including them, or only ties not including them.
//' This is controlled by the \code{counter_type} argument:
//' 
//' - 0: All ties
//' - 1: Only ties including the perceiver
//' - 2: Only ties not including the perceiver
//' @return A data frame with columns `id` (integer perceiver identifier),
//'   `name` (character label for the reciprocity error type), and `value`
//'   (numeric count).
//' @examples
//' data(krackhardt_advice)
//' data(krackhardt_advice_perceptions)
//'
//' n_people <- 21
//' advice_matrix <- matrix(0L, nrow = n_people, ncol = n_people)
//' advice_matrix[cbind(krackhardt_advice$from, krackhardt_advice$to)] <-
//'   krackhardt_advice$value
//'
//' krack_graph <- new_barry_graph(
//'   c(list(advice_matrix), krackhardt_advice_perceptions)
//' )
//' count_recip_errors(krack_graph)
//' @export
// [[Rcpp::export(rng = false)]]
DataFrame count_recip_errors(
    SEXP x,
    int counter_type = 0
  ) {
  
  Rcpp::XPtr< netcounters::Network >ptr(x);
  netcounters::NetStatsCounter< netcounters::Network> counter(ptr);
  
  std::vector< size_t > end = ptr.attr("endpoints");
  int                     n = ptr.attr("netsize");
  
  APPEND_COUNTER(counter_css_partially_false_recip_omiss)
  APPEND_COUNTER(counter_css_partially_false_recip_commi)
  APPEND_COUNTER(counter_css_completely_false_recip_omiss)
  APPEND_COUNTER(counter_css_completely_false_recip_comiss)
  APPEND_COUNTER(counter_css_mixed_recip)
    
  APPEND_COUNTER(counter_css_census01)
  APPEND_COUNTER(counter_css_census02)
  APPEND_COUNTER(counter_css_census03)
  APPEND_COUNTER(counter_css_census04)
  APPEND_COUNTER(counter_css_census05)
  APPEND_COUNTER(counter_css_census06)
  APPEND_COUNTER(counter_css_census07)
  APPEND_COUNTER(counter_css_census08)
  APPEND_COUNTER(counter_css_census09)
  APPEND_COUNTER(counter_css_census10)
  
  IntegerVector id(end.size());
  for (int i = 0; i < static_cast<int>(id.size()); ++i)
    id[i] = i;
  
  // print(Rcpp::wrap(counter.get_names()));
  
  return DataFrame::create(
    _["id"]    = id,
    _["name"]  = wrap(counter.get_names()),
    _["value"] = counter.count_all()
  );
  
}

//' Computes census of imaginary errors
//' @param x An object of class [barry_graph].
//' @param counter_type An integer indicating the type of census to compute (see details).
//' @details
//' We can also separate the counts as a function of whether the perceiver is looking
//' into all ties, only ties including them, or only ties not including them.
//' This is controlled by the \code{counter_type} argument:
//' 
//' - 0: All ties
//' - 1: Only ties including the perceiver
//' - 2: Only ties not including the perceiver
//' 
//' There are ten (10) values:
//' - (01) Accurate null
//' - (02) Partial false positive (null)
//' - (03) Complete false positive (null)
//' - (04) Partial false negative (assym)
//' - (05) Accurate assym
//' - (06) Mixed assym
//' - (07) Partial false positive (assym)
//' - (08) Complete false negative (full)
//' - (09) Partial false negative (full)
//' - (10) Accurate full
//' @noRd
// [[Rcpp::export(rng = false)]]
DataFrame count_imaginary_census_cpp(
    SEXP x,
    int counter_type = 0
) {
  
  Rcpp::XPtr< netcounters::Network >ptr(x);
  netcounters::NetStatsCounter< netcounters::Network> counter(ptr);
  
  std::vector< size_t > end = ptr.attr("endpoints");
  int n = ptr.attr("netsize");
  
  APPEND_COUNTER(counter_css_census01)
  APPEND_COUNTER(counter_css_census02)
  APPEND_COUNTER(counter_css_census03)
  APPEND_COUNTER(counter_css_census04)
  APPEND_COUNTER(counter_css_census05)
  APPEND_COUNTER(counter_css_census06)
  APPEND_COUNTER(counter_css_census07)
  APPEND_COUNTER(counter_css_census08)
  APPEND_COUNTER(counter_css_census09)
  APPEND_COUNTER(counter_css_census10)
    
  IntegerVector id(end.size());
  for (int i = 0; i < static_cast<int>(id.size()); ++i)
    id[i] = i;
  
  // print(Rcpp::wrap(counter.get_names()));
  return DataFrame::create(
    _["id"]    = id,
    _["name"]  = wrap(counter.get_names()),
    _["value"] = counter.count_all()
  );
  
}

//' Retrieves the edgelist of a barry_graph
//' @param x An object of class barry_graph.
//' @return A matrix with two columns, the first one with the source and the
//'   second one with the target.
//' @examples
//' data(krackhardt_advice)
//' data(krackhardt_advice_perceptions)
//'
//' n_people <- 21
//' advice_matrix <- matrix(0L, nrow = n_people, ncol = n_people)
//' advice_matrix[cbind(krackhardt_advice$from, krackhardt_advice$to)] <-
//'   krackhardt_advice$value
//'
//' krack_graph <- new_barry_graph(
//'   c(list(advice_matrix), krackhardt_advice_perceptions)
//' )
//' head(barray_to_edgelist(krack_graph))
//' @export
// [[Rcpp::export(rng = false)]]
IntegerMatrix barray_to_edgelist(SEXP x) {
    
  // checking the class attribute of x
  if (Rf_inherits(x, "barry_graph") == false)
    stop("x must be of class barry_graph");

  Rcpp::XPtr< netcounters::Network >ptr(x);
  
  barry::Entries< double > entries = ptr->get_entries();
  size_t nentries = entries.source.size();

  IntegerMatrix out(nentries, 2);
  
  for (size_t i = 0u; i < nentries; ++i)
  {
      out(i, 0) = static_cast<int>(entries.source[i]) + 1;
      out(i, 1) = static_cast<int>(entries.target[i]) + 1;
  }

  // Adding names
  out.attr("dimnames") = List::create(
      R_NilValue,
      CharacterVector::create("source", "target")
  );

  return out;

}


#undef APPEND_COUNTER


