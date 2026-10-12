
#include <Rcpp.h>
using namespace Rcpp;

//function to convert 3 counters to key
std::vector<int> N3_to_vec(int N, int N_0, int N_1, int N_2){
  int keys[] = {N_0,N_1,N_2};
  std::vector<int> res (keys, keys + sizeof(keys) / sizeof(int) );
  return(res);
  //return(std::string(N_0) + "/" + std::string(N_1) + "/" +  std::string(N_2) + "/" +std::string(N_3));
}



//function to convert key to 3 counters
void vec_to_N3(int N,std::vector<int> key, int &N_0,int &N_1,int &N_2){
  
  N_0 = key.at(0);
  N_1 = key.at(1); 
  N_2 = key.at(2);
}

//function for computing outer product of two hash tables
std::map <std::vector<int>,double> hashtable_outer_product(std::map <std::vector<int>,double> d_l,
                                                    std::map <std::vector<int>,double> d_r,
                                                    int N){
  
  //these will be used for iterating over the keys and values of the two hash tables
  int cur_N0_l,cur_N1_l,cur_N2_l;
  int cur_N0_r,cur_N1_r,cur_N2_r;
  std::map<std::vector<int>, double> next_dist;
  double cur_prob_l=1.0;
  double cur_prob_r=1.0;
  double temp_prob=1.0;
  
  std::vector<int> cur_key_l;
  std::vector<int> cur_key_r;
  std::vector<int> temp_key;
  
  std::map<std::vector<int>,double>::iterator iter_l;
  std::map<std::vector<int> ,double>::iterator iter_r;
  //iterate over items of the left distribution
  //double _thres = (10^(-10));
  for(iter_l = d_l.begin(); iter_l != d_l.end(); iter_l++)
  {
    cur_key_l =  (iter_l->first);
    cur_prob_l = (iter_l->second);
    vec_to_N3(N,cur_key_l,cur_N0_l,cur_N1_l,cur_N2_l);
    
    //iterate over items of the right distribution
    for(iter_r = d_r.begin(); iter_r != d_r.end(); iter_r++){
      cur_key_r =  (iter_r->first);
      cur_prob_r = (iter_r->second);
      vec_to_N3(N,cur_key_r,cur_N0_r,cur_N1_r,cur_N2_r);
      
      //this is the item to add
      temp_prob = cur_prob_l * cur_prob_r;
      temp_key =  N3_to_vec(N,
                             cur_N0_l + cur_N0_r,
                             cur_N1_l + cur_N1_r,
                             cur_N2_l + cur_N2_r);
      //we add the item to the next distribution
      
      // for large groups I need to check if(temp_prob > _thres) // this is used for filtering out cases which are rare
      
        if( next_dist.count(temp_key)>0 ){
          next_dist[temp_key] = next_dist[temp_key] + temp_prob;
        }else{
          next_dist[temp_key] = temp_prob;
        }  
      
      
    }//end of iterate over right
  } // end of iterate over left
  return(next_dist);
}

//this is the object returned by Cluster_to_HashTable
struct Cluster_to_HashTable_Return{
  std::map <std::vector<int>,double> dist;
  std::map <std::vector<int>,double> dist_PI;
} ;

//function for converting cluster data to hashtable
Cluster_to_HashTable_Return Cluster_to_HashTable(
    NumericMatrix incluster_pdf_index_0,
    NumericMatrix incluster_pdf_index_1,
    NumericMatrix incluster_pdf_index_2,
    NumericMatrix incluster_Pi,
    NumericMatrix incluster_binned_z_mat,
    NumericVector incluster_studies_ind,
    int n_association,
    int nr_studies,
    int current_SNP,
    bool compute_PI
    ){
  
  std::map<std::vector<int>, double> next_dist; // this if for the Local FDR
  std::map<std::vector<int>, double> next_dist_PI; // this is for the PI matrix
  
  int current_0 = 0;
  int current_1 = 0;
  int current_2 = 0;
  
  double current_prob = 1.0; // cummulative produt for the in row probability
  double temp_prob = 1.0; // probabilty per study, in specific row
  double current_cell_value =0.0;
  int nr_studies_in_cluster = incluster_studies_ind.length();
  int Pi_col = incluster_Pi.ncol()-1; // this column holds the probability of the PI mat
  
  std::vector<int> current_key;
  
  //we first find on what bin is the SNP in each study for this cluster
  NumericVector bin_for_snp_in_study(nr_studies_in_cluster);
  for(int i=0;i<nr_studies_in_cluster;i++){
    bin_for_snp_in_study(i) = incluster_binned_z_mat(current_SNP,i) - 1 ;
    // we shift by one, because indices are given in R, but used in C++
  }
  
  //we iterate over the rows of incluster_Pi, and add them to next_dist
  for(int i=0; i<incluster_Pi.nrow(); i++){
    
    current_0 = 0;
    current_1 = 0;
    current_2 = 0;
    current_prob = 1.0;
    for(int j = 0 ; j<nr_studies_in_cluster;j++){
      current_cell_value =  incluster_Pi(i,j);
      //we need to find the density of the Z score measured, under the correct hypothesis, for the specific study
      // by the current row we are in
      if(n_association == 3){
        if(current_cell_value == -1){ current_0++  ; temp_prob = incluster_pdf_index_0( j , bin_for_snp_in_study(j) ); }
        if(current_cell_value == 0){  current_1++  ; temp_prob = incluster_pdf_index_1( j , bin_for_snp_in_study(j) ); }
        if(current_cell_value == 1){  current_2++  ; temp_prob = incluster_pdf_index_2( j , bin_for_snp_in_study(j) ); }
      }else if(n_association == 2){
        if(current_cell_value == 0){  current_0++  ; temp_prob = incluster_pdf_index_0( j , bin_for_snp_in_study(j) ); }
        if(current_cell_value == 1){  current_1++  ; temp_prob = incluster_pdf_index_1( j , bin_for_snp_in_study(j) ); }
      }
      //we add the cell density (for the study), to the row cummulative product
      current_prob = current_prob * temp_prob;
    } // done iterating over studies in row
    
    //we multiply by the joint probability of the row in PI
    current_prob = current_prob * incluster_Pi(i,Pi_col);
    
    //we now have finished computing a row.
    //we add it to the null table.
    //we need to check if it is found or not
    current_key = N3_to_vec(nr_studies,current_0,current_1,current_2);
    
    if( next_dist.count(current_key)>0 ){
      next_dist[current_key] = next_dist[current_key] + current_prob;
      if(compute_PI)
        next_dist_PI[current_key] = next_dist_PI[current_key] + incluster_Pi(i,Pi_col);
    }else{
      next_dist[current_key] = current_prob;
      if(compute_PI)
        next_dist_PI[current_key] = incluster_Pi(i,Pi_col);
    } 
  }// done iterating over rows
  
  //building the returned struct
  Cluster_to_HashTable_Return ret;
  ret.dist = next_dist;
  ret.dist_PI = next_dist_PI;
  return(ret);
} // done Cluster_to_HashTable


// [[Rcpp::export]]
List rcpp_main(NumericVector Sizes,
               List pdf_binned_z_list_index0,
               List pdf_binned_z_list_index1,
               List pdf_binned_z_list_index2,
               List binned_z_mat_list,
               List cluster_ind_list,
               List repfdr_Pi_list){
  
    //extracting parameters
    int nr_studies = Sizes[0];
    int nr_bins = Sizes[1];
    int n_association = Sizes[2];
    int nr_clusters = Sizes[3];
    int non_null_code = Sizes[4]; // currently not used
    int non_null_u = Sizes[5]; // currently not used
    int current_SNP = Sizes[6] - 1; // we move from 1 based R index of SNP to zero based C index of SNP
    int debug = Sizes[7]; // currently all debug messages are disabled in code (via && false)
    int Compute_PI = Sizes[8]; // PI is not SNP dependent , threfore the R code calls the computation only for the last SNP
    int N0,N1,N2; // used for handling keys
    
    // DEBUG: making sure that the parameters arrived correctly, currently disabled
    if(debug>0 && false){
      REprintf("\n\r nr_studies: %d nr_bins: %d n_assoc: %d  nr_clusters:%d non_null_code:%d non_null_u:%d \n\r",
               nr_studies, nr_bins, n_association,nr_clusters,non_null_code,non_null_u);  
    }
    
    //generate base table
    std::map<std::vector<int>, double> dist; // the base distribution has no values measured , and has prob 1
    dist[N3_to_vec(nr_studies,0,0,0)] = 1.0;
    
    std::map<std::vector<int>, double> dist_PI; //This holds PI
    dist_PI[N3_to_vec(nr_studies,0,0,0)] = 1.0;
    
    //These will be used to extract the per cluster information
    NumericMatrix incluster_pdf_index_0 ;
    NumericMatrix incluster_pdf_index_1 ;
    NumericMatrix incluster_pdf_index_2 ;
    NumericMatrix incluster_Pi;
    NumericMatrix incluster_binned_z_mat;
    NumericVector incluster_studies_ind;
    
    
    
    //iterating over clusters
    for(int i=0 ; i<nr_clusters ; i++){
      //we extract the cluster data from the lists we got
      incluster_pdf_index_0 =  wrap(pdf_binned_z_list_index0(i));
      incluster_pdf_index_1 =   wrap(pdf_binned_z_list_index1(i));
      if(n_association == 3){
        incluster_pdf_index_2 =  wrap(pdf_binned_z_list_index2(i));
      }
      incluster_Pi =  wrap(repfdr_Pi_list(i));
      incluster_binned_z_mat = wrap(binned_z_mat_list(i));
      incluster_studies_ind = wrap(cluster_ind_list(i));
       
      //we convert the cluster to joint probability held in distribution
      Cluster_to_HashTable_Return current_cluster_ret = Cluster_to_HashTable(
        incluster_pdf_index_0,
        incluster_pdf_index_1,
        incluster_pdf_index_2,
        incluster_Pi,
        incluster_binned_z_mat,
        incluster_studies_ind,
        n_association,
        nr_studies,
        current_SNP,
        (Compute_PI >0.5)
      );
      
      std::map<std::vector<int>, double> current_cluster = current_cluster_ret.dist;
      std::map<std::vector<int>, double> current_cluster_PI = current_cluster_ret.dist_PI;
      
      // DEBUG we also look at the per iteration table added to the current distribution
      if(false && debug>0){
        REprintf("Printing table to add \n\r");
        std::map<std::vector<int>,double>::iterator current_cluster_iter;
        for(current_cluster_iter = current_cluster.begin(); current_cluster_iter != current_cluster.end(); current_cluster_iter++)
        {
          vec_to_N3(nr_studies,current_cluster_iter->first, N0, N1, N2);
          REprintf("N0:%d N1:%d N2:%d P:%f \n\r",N0,N1,N2,(current_cluster_iter->second));
        }
        
      }
      
      // we taken an outer product between the current cluster and all previous ones
      dist = hashtable_outer_product(dist,current_cluster,nr_studies);
      if(Compute_PI >0.5)
        dist_PI = hashtable_outer_product(dist_PI,current_cluster_PI,nr_studies);
      
    }//done iterating over studies
    
    
    //CharacterVector x = CharacterVector::create( "foo", "bar" );
    //NumericVector y   = NumericVector::create( 0.0, 1.0 );
    
    //***************************
    //Extract results
    //***************************
    
    //Rcpp::NumericVector keys(dist.size());
    //extract keys and values
    Rcpp::NumericVector values(dist.size());
    Rcpp::NumericVector values_PI(dist.size());
    Rcpp::NumericMatrix keys_counts_arr(dist.size(), 3);
    //REprintf('%ld',result.size());
    long pointer=0;
    std::map<std::vector<int>,double>::iterator iter;
    for(iter = dist.begin(); iter != dist.end(); iter++)
    {
      
      //keys[pointer]=double(iter->first); // old implementation form the time the keys where long long and not std::vector<int>
      vec_to_N3(nr_studies,iter->first, N0, N1, N2);
      keys_counts_arr(pointer,0) = double(N0);
      keys_counts_arr(pointer,1) = double(N1);
      keys_counts_arr(pointer,2) = double(N2);
      values[pointer]=double(iter->second);
      if(Compute_PI >0.5)
        values_PI[pointer] = dist_PI[iter->first]; // we have the same keys for the two hashmaps
      pointer=pointer+1;
      
    }
    
    Rcpp::NumericVector size_ans(1);
    size_ans[0] = dist.size();
    
    List res = List::create(keys_counts_arr,values,size_ans,values_PI); //send response
    return res;
}
