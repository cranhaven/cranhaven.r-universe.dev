// [[Rcpp::depends(RcppArmadillo)]]

//maximum likelihood algorithm
#include <RcppArmadillo.h>
using namespace Rcpp;


long long combnnum(int nrow, int p){
  int i;
  long long combnnum =1;
  for (i = 0; i < p; i++){
    combnnum = combnnum * (long long)(nrow- i);
  }
  for (i = 0; i < p; i++){
    combnnum = combnnum/((long long)(i+1));
  }
  return(combnnum);
}

arma::colvec Next( arma::colvec combn){
  
  int length = combn.n_rows;
  int target_position = 0, target_order = 0;
  
  int i,j;
  
  for (i = length-2 ; i> -1; i--){
    if ((1 == combn[i]) && (0 == combn[i+1]) ){
      target_position = i;
      target_order = 0;
      for (j = i+2; j< length; j++) target_order = target_order + combn[j];
      break;
    }
  }
  
  combn[target_position] = 0;
  combn[target_position + 1] = 1;
  for(i = target_position +2 ; i < length; i++) combn[i] = 0;
  
  if(target_order > 0){
    for( i = target_position+2; i<= target_position + 1 + target_order; i++ ){
      combn[i] = 1;
    }
  }
  
  
  return(combn);
}

// [[Rcpp::export]]

arma::mat pivot_cpp(int row_ind, int col_ind, arma::mat table_simpl_tran){
  
  
  int table_nrow = table_simpl_tran.n_rows;
  
  table_simpl_tran.row(row_ind) = table_simpl_tran.row(row_ind)/table_simpl_tran(row_ind,col_ind);
  for (int row = 0; row < table_nrow; row ++){
    if( row != row_ind){
      table_simpl_tran.row(row) = table_simpl_tran.row(row) + table_simpl_tran.row(row_ind)*(-1)*table_simpl_tran(row,col_ind);
    }
  }
  return (table_simpl_tran);
  
  
}


// [[Rcpp::export]]
NumericVector mle_simpl_FStep_Cpp(int time_indicator, NumericMatrix InputData, NumericMatrix Domain){
  
  //InputData: time_grid + Covariates; p+1+1 * Sample_Size
  //Domain: (p+1) * nrow_dom, the row is the normal vector of the corresponding boundaryplane.
  
  int p = Domain.ncol()-1;
  int nrow_dom = Domain.nrow();
  int nrow_inputdata = InputData.nrow();
  int i,j;
  double epsilon = 0.0000001;
  
  arma::mat X(InputData.begin(), InputData.nrow(), InputData.ncol(), false);
  arma::mat dom(Domain.begin(), Domain.nrow(), Domain.ncol(), false);
  arma::colvec domain_check(nrow_dom);
  
  arma::mat coefficient_matrix;
  coefficient_matrix.zeros(p + 1, p + 1);
  arma::colvec constant_vector(p+1);
  arma::colvec solution(p+1);
  
  arma::mat table_simpl_initial;
  table_simpl_initial.zeros(nrow_dom +2, 1 + p+1 + nrow_dom);
  // constructing the first simplex table;
  // first row, the expression of ml;
  // second row, the linear expression of u, the linear part inside log;
  // the following dom_nrow rows are the conditions given by matrix_domain;
  // the first column is for u, the followint p+1 columns is for the beta, 
  // the following dom_nrow columns for the conditions.
  
  // first row:
  table_simpl_initial(0,0) = 0;
  for (i =1; i<p+1+1; i++){
    for(j = time_indicator-1-1; j< nrow_inputdata; j++ ){
      table_simpl_initial(0,i) = table_simpl_initial(0,i)- InputData(j,i);
    }
  } 
  
  // second row:
  table_simpl_initial(1,0) = 1;
  for (i =1; i< (p+1+1); i++){
    table_simpl_initial(1,i) = -X((time_indicator-1-1),i);
  }
  
  // the follwoing dom_nrow  rows:
  for (i=0; i<nrow_dom; i++) {
    for(j =0; j< p+1; j++){
      table_simpl_initial(i+2,j+1) = dom(i,j);
    }
    table_simpl_initial(i+2,1+p+1+i) = -1;
  }
  
  //Rcout<<table_simpl_initial;
  
  // substitute beta_i by beta_i1-beta_i0:
  
  
  arma::mat table_simpl_tran;
  table_simpl_tran.zeros(nrow_dom +2, 1 + p+1 + nrow_dom); 
  // declare simplex table for transforming
  arma::mat table_inout;
  table_inout.zeros(2,p);
  // indicate which beta is eliminated (first row)
  // which boundary adopted (corresponds to the beta eliminated) (second row)
  arma::rowvec expression_temp(1+p+1+nrow_dom);
  // used for check the expression   
  
  
  int table_nrow = table_simpl_initial.n_rows;
  int table_ncol = table_simpl_initial.n_cols;
  
  table_simpl_tran = arma::mat(table_simpl_initial);
  //Rcout<<table_simpl_tran;
  
  table_simpl_tran =  pivot_cpp(1,1,table_simpl_tran);
  //Rcout<<table_simpl_tran;
  // ===== Construct the first expression
  
  bool IsMove = FALSE;
  int Move_level = p-1;
  int in_loop,out_loop;
  int in_temp;
  
  
  
  while(Move_level> 0 ){
    IsMove= FALSE;
    for(in_loop = table_inout(1,p-Move_level-1)+1; in_loop <= (nrow_dom-Move_level);in_loop++){
      for(out_loop =1;out_loop <= (p+1); out_loop++)
      {
        if(table_simpl_tran(2+in_loop-1,1+out_loop-1)> epsilon || table_simpl_tran(2+in_loop-1,1+out_loop-1)< (-1)*epsilon){
          table_simpl_tran = pivot_cpp(2+in_loop-1,1+out_loop-1,table_simpl_tran);
          table_inout(0,p-Move_level-1) = out_loop;
          for (i =(p-Move_level); i<=(p); i++) table_inout(1,i-1) = in_loop;
          IsMove = TRUE;
          Move_level= Move_level-1;
          break;
        }
      }
      if(IsMove) break;
    }
  }
  
  //Rcout<<table_simpl_tran;
  //Rcout<<table_inout;
  
  
  // ===== Main loop   
  bool IsFind = FALSE;
  bool IsCheck = FALSE;
  bool out_check = FALSE;
  double min_mme = 0;
  
  while(!IsFind){
    
    // Start Check
    // Check means to find the last hyperplane bounndary which suits the maximum point
    
    IsCheck = FALSE;
    if((table_inout(1,p-1)+1)<=(nrow_dom)){
      out_check = FALSE;          
      for(i = (2+table_inout(1,p-1)+1-1); i< table_nrow;i++){
        for(j =1; j< p+1+1; j++){
          if(table_simpl_tran(i,j)!= 0) {
            out_loop = j;
            out_check= TRUE;
            break;
          }
        }
        if(out_check) break;
      }
      //Rcout<<out_loop<<std::endl;
      for(in_loop = table_inout(1,p-1)+1; in_loop<=(nrow_dom);in_loop ++){
        if(table_simpl_tran(2+in_loop-1,out_loop +1-1) > epsilon || table_simpl_tran(2+in_loop-1,out_loop +1-1) < (-1)*epsilon){
          
          expression_temp = table_simpl_tran.row(0) + table_simpl_tran.row(2+in_loop-1)*(-1)*table_simpl_tran(0,out_loop+1-1)/table_simpl_tran(2+in_loop-1,out_loop+1-1);
          //Rcout<<expression_temp;
          if(arma::all(expression_temp < epsilon) && expression_temp(0) < -epsilon && expression_temp(0) < min_mme){
            //print("find solution")
            min_mme= expression_temp(0);
            IsCheck= TRUE;
            table_simpl_tran = pivot_cpp(2+in_loop-1,1+out_loop-1,table_simpl_tran);
            table_inout(0,p-1) = out_loop;
            table_inout(1,p-1) = in_loop;
            
            //Rcout<<"expression:\n";
            //Rcout<<expression_temp<<std::endl;
            //Rcout<<"table_inout: \n";
            //Rcout<<table_inout<<std::endl;
            
            return(NumericVector(table_inout.row(1).begin(), table_inout.row(1).end()));
            
            
            // Rcout  << loop_index << std::endl;
            // Rcout  << solution << std::endl;
            // Rcout  << beta_time << std::endl;
            
            // ======================================================
            
            break;
          }
        }
      }
    }
    if(IsCheck) {
      //print(table_inout)
      in_temp = table_inout(1,p-1);
      table_simpl_tran = pivot_cpp(2+in_temp-1, 1+p+1+in_temp-1, table_simpl_tran);
      continue;
    }
    
    // Start Move
    //Rcout<<table_simpl_tran;
    Move_level = 1;
    in_temp = table_inout(1,p-Move_level-1);
    table_simpl_tran = pivot_cpp(2+in_temp-1, 1+p+1+in_temp-1, table_simpl_tran);
    
    
    while(Move_level> 0 ){
      IsMove= FALSE;
      if(table_inout(1,p-Move_level-1)+1 <= nrow_dom-Move_level){
        for(in_loop =(table_inout(1,p-Move_level-1)+1); in_loop <=(nrow_dom-Move_level);in_loop++){
          for(out_loop = 1; out_loop <=(p+1); out_loop ++)
          {
            if(table_simpl_tran(2+in_loop-1,1+out_loop-1) >epsilon || table_simpl_tran(2+in_loop-1,1+out_loop-1) <- epsilon){
              table_simpl_tran = pivot_cpp(2+in_loop-1,1+out_loop-1,table_simpl_tran);
              
              table_inout(0,p-Move_level-1) = out_loop;
              for (i = p-Move_level-1; i<p; i++) table_inout(1,i) = in_loop;
              
              IsMove = TRUE;
              Move_level= Move_level-1;
              break;
            }
            
          }
          if(IsMove) {break;}
        }
      }
      if(IsMove) {continue;}
      Move_level = Move_level +1;
      if(Move_level> p-1) {return(NumericVector(p+1));}
      in_temp = table_inout(1,p-Move_level-1);
      table_simpl_tran = pivot_cpp(2+in_temp-1, 1+p+1+in_temp-1, table_simpl_tran);
      
      
    }  
    
  }
  return(NumericVector(p+1));
}




// [[Rcpp::export]]
NumericVector mle_loop_Cpp_sub(int time_indicator, NumericMatrix InputData, NumericMatrix Domain){
  
  //InputData: time_grid + Covariates; p+1+1 * Sample_Size
  //Domain: (p+1) * nrow_dom, the row is the normal vector of the corresponding boundaryplane.
  
  int p = Domain.ncol()-1;
  int nrow_dom = Domain.nrow();
  int nrow_inputdata = InputData.nrow();
  int i,j,count;
  //long long  size_index = combnnum(nrow_dom, p);
  
  arma::mat X(InputData.begin(), InputData.nrow(), InputData.ncol(), false);
  arma::mat dom(Domain.begin(), Domain.nrow(), Domain.ncol(), false);
  arma::colvec domain_check(nrow_dom);
  arma::colvec history(nrow_dom);
  history.zeros();
  int length_history = 0;
  
  arma::mat coefficient_matrix;
  double det_coefficient_matrix;
  coefficient_matrix.zeros(p + 1, p + 1);
  arma::colvec constant_vector(p+1);
  arma::colvec solution(p+1);
  // prepare the constant vector  
  constant_vector.zeros();

  for (i = 0; i < p + 1; i++){
    for(j = time_indicator-1-1; j < nrow_inputdata; j++ ){
      constant_vector[i] = constant_vector[i] - X(j,i+1);
    }
  }  

  
  arma::mat coefficient_matrix_beta;
  coefficient_matrix_beta.zeros(p + 1, p + 1);
  arma::colvec constant_vector_beta(p+1);
  arma::colvec beta_time(p+1);
  
  // reuses memory and avoids extra copy

  arma::colvec combn(p);
  combn.zeros();

  NumericVector nosolution(p+1);


  int length, target_position, target_order;
  bool Isfullrank_coefficient_matrix;
  bool IsFind = FALSE;
  
  int target;
  
  bool in_history;
  bool valid_solution;
  int element_history;
  
   //Rcout   << time_indicator<< std::endl;
   //Rcout   << nrow_dom <<' ' << p <<std::endl;

  
  
  
  // loop over all the possible cases of the index picked  
  while(!IsFind){
    
    in_history = FALSE;
    valid_solution = FALSE;
    
    if( nrow_dom - p + 1 == combn(0) ){
      return nosolution;
    }else{
      if( 0 == combn(p-1)){
        for(i=0; i< p; i++) combn(i) = i+1;
      }else{
        for(i = 0; i< p ; i++){
          if(combn(p-i-1) < nrow_dom - i){
           target = p-i-1; break;
          }
        }
        combn(target) = combn(target) + 1;
        for(i = target + 1; i< p; i++){
          combn(i) = combn(i-1)+1;
        }
      }
    }
    
    if(length_history > 0){
      for(i = 0; i< length_history+1; i++){
        if(arma::any(combn == history(i))){
          in_history = TRUE;
          break;
        }
      }
    }
    
    if(in_history) continue;
    
    //
    
    //if (loop_index < 695 or loop_index>705) continue;
    //Rcout << combn << std::endl;
    
    //beta_time = Solve_mme_analy(time_indicator, order[loop_index])
    
    //coefficient_matrix[,1:p] = t(matrix_domain[index_picked[,combi_indicator],])

    for (i = 0; i < p; i++ ){
        coefficient_matrix.col(i) = dom.row(combn(i)-1).t();
    }
    
    //coefficient_matrix[,p+1] = t(as.matrix(CP[time_indicator-1,(2:(p+2))]))
    for (i = 0; i< p+1; i++){
      coefficient_matrix(i,p) = X(time_indicator-1-1,i+1);
    }
    
    //Isfullrank_coefficient_matrix = abs(det(coefficient_matrix))> 0.000001
    det_coefficient_matrix = arma::det(coefficient_matrix);
    Isfullrank_coefficient_matrix = (det_coefficient_matrix > 0.0000001) or (det_coefficient_matrix< -0.0000001);
    
    
    //Rcout   << coefficient_matrix << std::endl;
    //Rcout   << Isfullrank_coefficient_matrix <<std::endl;
    
    // check the linear system has a unique solution or not
    if(!Isfullrank_coefficient_matrix){
      continue;
    }
    

    
    // find the solution
    
    solution = arma::solve(coefficient_matrix,constant_vector);
    
    
    //   << constant_vector <<std::endl;
     // Rcout   << solution << std::endl;
    // Rcout  << arma::any(solution>-0.0000001) << std::endl;
    // Rcout  << (abs(solution(p))< 0.0000001) << std::endl;
    
    // check solution, all the entries must be negative
    if (arma::all(solution<0.0000001)){
      valid_solution = TRUE;
    }else{
      if( solution(p) > -0.0000001) continue;
      count = 0;
      for(i=0; i< p; i++){
        if(solution(i)>0.0000001) count++;
      }
      if(count  > 1) continue;
      for(i=0; i< p; i++){
        if(solution(i)>0.0000001) element_history = combn(i);
      }
      
    }
    
    // check solution, the last entry can not be zero
    if( solution(p)< 0.0000001 and solution(p) > -0.0000001){
      continue;
    }
    
    
    // build the coefficient matrix for beta
    
    coefficient_matrix_beta = coefficient_matrix.t();
    
    
    
    // build constant vector for beta
    
    constant_vector_beta = constant_vector_beta.zeros();
    constant_vector_beta(p) = -1/solution(p);
    
    
    //find the solution
    
    beta_time = arma::solve(coefficient_matrix_beta,constant_vector_beta);
    
    // Rcout   << coefficient_matrix_beta << std::endl;
    // Rcout   << constant_vector_beta <<std::endl;
    // Rcout   << beta_time << std::endl;
    
    // Rcout  << arma::all(arma::abs(beta_time) < 0.0000001) <<std::endl;
    
    if(arma::all(arma::abs(beta_time) < 0.0000001)){
      continue;
    }
    
    
    
    
    domain_check = dom * beta_time;
    
    // Rcout   << domain_check <<std::endl;
    
    // Rcout   << arma::any(domain_check< -0.0000001) << std::endl;
    
    if(arma::any(domain_check< -0.0000001)){
      continue;
    }
    
    // Rcout  << loop_index << std::endl;
    // Rcout  << solution << std::endl;
    // Rcout  << beta_time << std::endl;
    
    if(valid_solution){
      arma::colvec result(p+1+p);
      for (i =0; i< p+1; i++){
        result(i)= beta_time(i);
      }
      for (i = 0; i< p; i++){
        result(p+1+i) = combn(i);
      }
      
      return(NumericVector(result.begin(), result.end()));
    }else{

      history(length_history) = element_history;
      length_history++;
      
     //Rcout << history << std::endl;
      //Rcout << length_history <<std::endl;
    }

    
    
    
  }
  
  
  return(NumericVector(p+1+p));
}

