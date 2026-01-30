/*******************************************************************************
 *
 * Group the columns of a numeric matrix into clusters
 *
 *******************************************************************************
 * Initialization
 ******************************************************************************/
#include "distance.h"
#include <Rcpp.h>
#include <algorithm>  // for std::sort, std::min, std::max
using namespace Rcpp;
/*******************************************************************************
 * Classes
 ******************************************************************************/
struct Centroid {
  std::vector<int> rows;
  std::vector<double> values;
  double total = 0.0;
  
  // Hash map for fast lookup of rows
  std::unordered_map<int, double> row_value_map;
  
  SparseVec view() const {
    return SparseVec{rows.data(),values.data(),rows.size(),total};
  }
  
  // Initialize map
  void build_map() {
    row_value_map.clear();
    for (size_t i = 0; i < rows.size(); ++i)
      row_value_map[rows[i]] = values[i];
  }
  
  bool has_map() const {
    return !row_value_map.empty();
  }
};
/*******************************************************************************
 * Forward Declarations
 ******************************************************************************/
void print_time_cpp(const std::string &txt);
/*******************************************************************************
* Internal Functions
*******************************************************************************/
/*******************************************************************************
 * Create a text distance matrix using a SparseVec vector
 ******************************************************************************/
List textDistMatrixV_cpp(const std::vector<SparseVec>& docs,
                              const std::vector<int>& sel,
                              double zeroes) {
  size_t ncol = sel.size();
  int ncomb = (ncol * (ncol - 1)) / 2;
  NumericVector out(ncomb);
  IntegerMatrix comb(2, ncomb);
  
  int idx = 0;
  for (size_t i = 0; i < ncol; ++i) {
    for (size_t j = i + 1; j < ncol; ++j) {
      out[idx] = dist_sparse_cpp(docs[sel[i]], docs[sel[j]], zeroes);
      comb(0, idx) = i; // these are indices in sel
      comb(1, idx) = j;
      ++idx;
    }
  }
  
  return List::create(
    Named("out") = out,
    Named("comb") = comb
  );
}
/*******************************************************************************
 * Obtain text distance between a column and a centroid
 ******************************************************************************/
double dist_to_centroid_cpp(const SparseVec& doc, const Centroid& centroid, double zeroes) {
  double denom = doc.total + centroid.total;
  if (denom == 0.0) return zeroes;  // both vectors are empty
  
  //If no map or doc has more rows than centroid, run the normal method
  if (!centroid.has_map()) {
    return dist_sparse_cpp(doc, centroid.view(), zeroes);
  }

  double num = denom;
  const auto& cmap = centroid.row_value_map;
  
  for (size_t i = 0; i < doc.size; ++i) {
    int r = doc.rows[i];
    auto it = cmap.find(r);
    if (it != cmap.end()) {
      double v1 = it->second;
      double v2 = doc.values[i];
      if (v1 < v2) num -= (2*v1); else num -= (2*v2);
    }
  }
  
  return num / denom;
}
/*******************************************************************************
 * Compute cluster centroids from sparse columns
 * docs: Vector of SparseVec columns
 * doc_cl: Cluster assignment for each column
 * k: Number of clusters
 ******************************************************************************/
std::vector<Centroid> centroids_cpp(const std::vector<SparseVec>& docs,
                                    const std::vector<int>& doc_cl,
                                    int k,
                                    std::vector<int>& cl_sz) {
  //Unordered map for each cluster with row -> value
  std::vector<std::unordered_map<int, double>> sums(k);
  cl_sz.assign(k, 0); // Reset sizes
  
  // Store information from the documents by cluster
  for (size_t j = 0; j < docs.size(); ++j) {
    int cl = doc_cl[j];
    if (cl < 0 || cl >= k) continue;
    cl_sz[cl]++;
    const auto& col = docs[j];
    for (size_t i = 0; i < col.size; ++i) {
      sums[cl][col.rows[i]] += col.values[i];
    }
  }
  
  // Build centroids
  std::vector<Centroid> centroids;
  centroids.reserve(k);
  
  for (int c = 0; c < k; ++c) {
    Centroid centroid;
    if (cl_sz[c] == 0) {
      centroids.emplace_back(std::move(centroid));
      continue;
    }
    
    std::vector<std::pair<int, double>> pairs;
    centroid.rows.reserve(sums[c].size());
    centroid.values.reserve(sums[c].size());
    double total = 0.0;
    
    //Fill the pairs
    for (const auto& [r, val] : sums[c]) {
      double avg = val / cl_sz[c];
      pairs.emplace_back(r, avg);
      total += avg;
    }
    
    // Sort by row index
    std::sort(pairs.begin(), pairs.end());
    
    //Fill the centroid
    for (size_t i = 0; i < pairs.size(); ++i) {
      centroid.rows.push_back(pairs[i].first);
      centroid.values.push_back(pairs[i].second);
    }
    
    centroid.total = total;
    if (centroid.rows.size() > 10) centroid.build_map();
    centroids.push_back(std::move(centroid));  }
  
  return centroids;
}
/*******************************************************************************
 * Reassign columns to their nearest cluster centroids
 * docs: Vector of SparseVec columns
 * centroids: Vector of SparseVec cluster centroids
 * doc_cl: Cluster assignment for each column (modified in place)
 * k: Number of clusters
 * zero_cols: Whether the last cluster is reserved for zero columns
 ******************************************************************************/
void reassign_clusters_cpp(const std::vector<SparseVec>& docs,
                           const std::vector<Centroid>& centroids,
                           std::vector<int>& doc_cl,
                           int k,
                           bool zero_cols) {
  size_t nc = docs.size();
  int maxk = zero_cols ? k - 1 : k;
  
  for (size_t j = 0; j < nc; ++j) {
    // Skip zero-column cluster if applicable
    if (zero_cols && doc_cl[j] == k - 1) continue;
    
    double best_dist = 1.0, prior_dist = 1.0;
    int best_cl = doc_cl[j];
    
    const auto& col = docs[j];
    
    for (int c = 0; c < maxk; ++c) {
      const auto& cent = centroids[c];
      double d = dist_to_centroid_cpp(col, cent, 0.5);
//      double d = dist_sparse_cpp(col, cent.view(), 0.5);
      if (doc_cl[j] == c) prior_dist = d;
      if (d < best_dist) {
        best_dist = d;
        best_cl = c;
      }
    }
    if (best_dist < prior_dist) doc_cl[j] = best_cl;
  }
}
/*******************************************************************************
* Main Function textCluster_cpp
*******************************************************************************/
// [[Rcpp::export]]
List textCluster_cpp(
    const std::vector<int>& Mi,     // row indices (terms)
    const std::vector<double>& Mx,  // values
    const std::vector<int>& Mp,     // First indices of each column
    int k,                          // number of clusters
    int mx,                         // max iterations
    int md,                         // Max cols to use in distance matrix
    int nr,                         // number of terms
    bool silent) {                  // Produce timing messages if false
  
  //Initialize
  if (!silent) print_time_cpp("Initialization:");
  int nc = Mp.size() - 1;           //Number of columns
  std::vector<int> doc_cl(nc,-1);   //Assignment of columns to clusters
  std::vector<int> cl_sz(k,0);      //Number of columns in each cluster
  bool zero_cols = false;           //True if there are all-zero columns
  int fill = 0;                     //Number of columns assigned to a cluster
  std::vector<SparseVec> docs;      //Vector of Sparse columns
  docs.reserve(nc);
  //Use the larger of md, which by default is 10*k, and 3*k as the number of 
  //columns to use for the initial assignment
  md = std::max(md, 3 * k);
  std::vector<double> col_sz(nc, 0.0); //Total values by column

  // -------------------------------------------------
  // Store all columns in docs and handle zero columns
  // -------------------------------------------------
  if (!silent) print_time_cpp("Store columns:");
  for (int j = 0; j < nc; ++j) {
    int start = Mp[j];
    int end = Mp[j + 1];
    size_t len = end - start;
    
    // Handle zero-column case
    if (len == 0) {
      doc_cl[j] = k - 1;
      zero_cols = true;
      ++fill;
    }
    
    //Mx.data() is the same as &Mx[0]
    double total = sum_doubles(Mx.data() + start, len);
    
    SparseVec vec{Mi.data() + start,Mx.data() + start,len,total};
    docs.push_back(vec);
    
    col_sz[j] = len;
  }

  // -------------------------------------------------
  // Get distances for selected columns
  // -------------------------------------------------
  // Determine columns to use for initial distances
  if (!silent) print_time_cpp("Determine columns to use:");
  std::vector<int> sm; // indices of selected columns

  if (nc > md) {
    // Order by descending size
    std::vector<int> order_cols(nc);
    std::iota(order_cols.begin(), order_cols.end(), 0);
    std::sort(order_cols.begin(), order_cols.end(),
              [&](int a, int b){ return col_sz[a] > col_sz[b]; });
    //order_cols now contains indices ordered by largest size (most phrases)
    
    // Pick top-md columns
    sm.assign(order_cols.begin(), order_cols.begin() + md);
  } else {
    // Use all columns
    sm.resize(nc);
    std::iota(sm.begin(), sm.end(), 0);
  } 
  
  // Compute pairwise distances only for selected columns
  if (!silent) print_time_cpp("DistMatrix:");
  List dist = textDistMatrixV_cpp(docs,sm, 0.5);  
  
  // Map comb indices back to original document indices
  if (!silent) print_time_cpp("Map indices back to original documents:");
  NumericVector out = dist["out"];
  IntegerMatrix comb = dist["comb"];
  for (int i = 0; i < comb.ncol(); ++i) {
    comb(0,i) = sm[comb(0,i)];
    comb(1,i) = sm[comb(1,i)];
  }

  // -------------------------------------------
  // Initialize clusters using closest pairs
  // -------------------------------------------
  if (!silent) print_time_cpp("Initialize clusters:");
  int ncomb = out.size();
  std::vector<int> order(ncomb);
  std::iota(order.begin(), order.end(), 0); // fill with 0..n-1
  
  // Sort indices based on values in `out`
  std::sort(order.begin(), order.end(),
            [&](int a, int b) {
              if (out[a] != out[b]) return out[a] < out[b];
              if (comb(0, a) != comb(0, b)) return comb(0, a) < comb(0, b);
              return comb(1, a) < comb(1, b);
            });
  //order now contains the indices of the pairs in ascending order of distance,
  //then within that by pairs in the combinations.
  
  //Put a pair with small distance in each cluster
  int cl = 0, pair_idx = 0;
  while (cl < k && pair_idx < ncomb) { //For each cluster
    if (cl==(k-1) && zero_cols) break; //Last cluster has the zero columns
    
    //Find the first pair where neither column has been assigned a cluster yet
    int p1 = comb(0,order[pair_idx]);
    int p2 = comb(1,order[pair_idx]);
    
    if (doc_cl[p1] == -1 && doc_cl[p2] == -1) { //Found an unassigned pair
      doc_cl[p1] = cl;
      doc_cl[p2] = cl;
      fill+=2;
      ++cl;
    } else if (out[order[pair_idx]]==0) { //special case for identical columns
      if (doc_cl[p1] == -1 && doc_cl[p2] != -1) {doc_cl[p1] = doc_cl[p2];++fill;}
      else if (doc_cl[p1] != -1 && doc_cl[p2] == -1) {doc_cl[p2] = doc_cl[p1];++fill;}
    }
    ++pair_idx;
  }
  
  // Check that we found at least one distinct pair for each cluster
  if ((cl < (k - 1)) || (!zero_cols && cl < k)) {
    stop("Could not find at least one distinct pair of documents for each cluster. Try reducing k.");}
  
  // Assign some of the remaining columns using the closest pairs
  pair_idx = 1; //First closest pair (pair 0) will have been assigned
  while (fill < nc && pair_idx < std::min(3 * k, ncomb)) {
    int p1 = comb(0,order[pair_idx]);
    int p2 = comb(1,order[pair_idx]);
    //For each pair where only one column has been assigned, assign the other to
    //the same cluster
    if (doc_cl[p1] == -1 && doc_cl[p2] != -1) {doc_cl[p1] = doc_cl[p2];++fill;} 
    else if (doc_cl[p1] != -1 && doc_cl[p2] == -1) {doc_cl[p2] = doc_cl[p1];++fill;}
    
    ++pair_idx;
  }
  
  // -------------------------------------------
  // Compare to centroids
  // -------------------------------------------
  if (!silent) print_time_cpp("Compare to centroids:");
  
  // Keep a copy of previous cluster assignments
  std::vector<int> doc_cl_old = doc_cl;
  int q;
  for (q = 0; q < mx; ++q) { 
    // Number of centroids: exclude zero-column cluster if present
    int nk = zero_cols ? k - 1 : k;

//    if (!silent && q < 5) print_time_cpp("Build centroids:");
    // Compute centroids for non-zero clusters
    std::vector<Centroid> centroids = centroids_cpp(docs, doc_cl, nk, cl_sz);
    
    // Reassign columns to the nearest centroid
//    if (!silent && q < 5) print_time_cpp("Reassign clusters:");
    reassign_clusters_cpp(docs, centroids, doc_cl, k, zero_cols);
//    if (!silent && q < 5) print_time_cpp("Reassigned clusters:");
    
    // Check for convergence
    if (doc_cl_old == doc_cl) break;
    doc_cl_old = doc_cl;
  } 
  
  if (!silent) {
    print_time_cpp("Centroid comparison done:");
    Rcpp::Rcout << "Number of times compared = " << q <<std::endl;
  }
  
  // -------------------------------------------
  // Prepare for output
  // -------------------------------------------
  if (!silent) print_time_cpp("Prepare for output:");
  
  // Recalculate centroids for all clusters, including zero-column cluster
  std::vector<Centroid> centroids = centroids_cpp(docs, doc_cl, k, cl_sz);
  
  if (!silent) print_time_cpp("Centroids done:");
  // Convert centroids to dense matrix
  NumericMatrix centroid_out(nr, k);
  for (int c = 0; c < k; ++c) {
    for (size_t idx = 0; idx < centroids[c].rows.size(); ++idx) {
      int r = centroids[c].rows[idx];
      centroid_out(r, c) = centroids[c].values[idx];
    }
  }
  
  if (!silent) print_time_cpp("Returning:");
  return List::create(
    Named("cluster")   = wrap(doc_cl),
    Named("centroids") = centroid_out,
    Named("size")      = wrap(cl_sz)
  );
}

