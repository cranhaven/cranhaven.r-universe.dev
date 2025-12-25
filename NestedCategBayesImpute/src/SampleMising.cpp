#include "sampleW.h"

IntegerVector CheckSZ_batch(IntegerMatrix X_house, IntegerMatrix X_indiv) {
  if (X_house.ncol() != 6) {
    Rcout << "Household matrix must have 6 columns" << std::endl;
    return R_NilValue;
  }
  if (X_indiv.ncol() != 5) {
    Rcout << "Household matrix must have 6 columns" << std::endl;
    return R_NilValue;
  }
  int batch = X_house.nrow();
  int household_size = X_indiv.nrow() / batch + 1;
  int nvaraibles = X_indiv.ncol();
  IntegerMatrix comb_to_check(batch, household_size * nvaraibles);
  for (int i = 0; i < batch; i++) {
    int count = 0;
    for (int j = 1; j < nvaraibles; j++) { //skip first household variables
      comb_to_check(i,count++) = X_house(i,j);
    }
    comb_to_check(i,count++) = 1; //Set relate to 1
    int indiv_start_row = i * (household_size -1); //household_size -1 is the number of other household members
    for (int j = indiv_start_row; j < indiv_start_row + (household_size -1); j++) {
      for (int k = 0; k < nvaraibles - 1; k++) {
        comb_to_check(i,count++) = X_indiv(j,k);
      }
      comb_to_check(i,count++) = X_indiv(j,nvaraibles -1) + 1; //recode relate
    }
  }
  return checkSZ2(comb_to_check,household_size);
}

List SampleMissingForOneHousehold_batch(IntegerVector another_index,
                                        IntegerMatrix X_house_s_prop, IntegerMatrix X_indiv_s_prop,
                                        IntegerVector house_szv_index, LogicalMatrix NA_house_missing_status,
                                        IntegerVector indiv_szv_index, LogicalMatrix NA_indiv_missing_status,
                                        List lambda,NumericMatrix phi,int G_household_G_s, IntegerVector index,
                                        IntegerVector orig_d, int orig_maxd,
                                        int batch) {
  int n_0_reject = 0;
  int first_valid = 0;
  int nvidiv = another_index.length();
  while(first_valid == 0){
    for(int i = 0; i < house_szv_index.length(); i++) {
      int real_k =  house_szv_index[i] - 1; //to zero-based index
      if(NA_house_missing_status(another_index[0] - 1,real_k) != 0) {
        NumericMatrix lambda_real_k = lambda[real_k];
        NumericVector lambda_row = lambda_real_k.row(G_household_G_s - 1);
        X_house_s_prop.column(real_k) = sampleW_multi(lambda_row,runif(batch));
      }
    }
    for(int i = 0; i < indiv_szv_index.length(); i++) {
      int real_k =  indiv_szv_index[i] - 1; //to zero-based index
      int base = real_k*orig_maxd;
      for (int j = 0; j < nvidiv;j++) {
        int index_j = index[j] - 1;
        if (NA_indiv_missing_status(another_index[j]-1, real_k) != 0) {
          int n = orig_d[real_k];
          NumericVector p(n);
          for (int s = 0; s <  n; s++) {
            p[s] = phi(base+s,index_j);
          }
          IntegerVector samples = sampleW_multi(p,runif(batch));
          for (int s = 0; s < batch; s++) {
            X_indiv_s_prop(j + s * nvidiv, real_k) = samples[s];
          }
        }
      }
    }

    //Check edit rules; Need to make this part more general, very specific for
    //this data and assumes head is at the household level
    IntegerVector nv_first_valid = CheckSZ_batch(X_house_s_prop, X_indiv_s_prop);
    first_valid = nv_first_valid[0];
    if (first_valid > 0) {
      n_0_reject = n_0_reject + first_valid - 1;
    } else {
      n_0_reject = n_0_reject + batch;
    }
  }

  return List::create(Named("n_0_reject", n_0_reject),
                      Named("first_valid", first_valid),
                      Named("X_house_s_prop", X_house_s_prop),
                      Named("X_indiv_s_prop", X_indiv_s_prop));
}


IntegerMatrix SampleNonStructureZerosHouseC(IntegerMatrix household,
                                            LogicalMatrix NA_house_missing_status,
                                            IntegerVector house_non_szv_index_raw,
                                            IntegerVector house_non_szv_index,
                                            List para_lambda,
                                            IntegerVector G_household_G,
                                            IntegerVector orig_n_i
                                            ) {
  for (int count = 0; count < house_non_szv_index_raw.length(); count++) {
    int k = house_non_szv_index_raw[count] - 1; //index into raw data
    int real_k = house_non_szv_index[count] - 1; //index into invividual level variables
    NumericMatrix lambda_k = para_lambda[real_k];
    int cumcount = 0;
    int nhouseholds = G_household_G.length();
    NumericVector r = runif(nhouseholds);
    for (int i = 0; i < nhouseholds; i++) {
      if (NA_house_missing_status(cumcount,real_k)) {
        NumericVector pr_X_miss_p(lambda_k.row(G_household_G[i] - 1));
        int sample = samplew(pr_X_miss_p.begin(), pr_X_miss_p.length(), r[i]);
        for (int pos = cumcount; pos < cumcount + orig_n_i[i]; pos++) {
          household(pos,k) = sample;
        }
      }
      cumcount += orig_n_i[i];
    }

  }
  return(household);
}

IntegerMatrix SampleNonStructureZerosIndivC(IntegerMatrix household,
                                            LogicalMatrix NA_indiv_missing_status,
                                            IntegerVector indiv_non_szv_index_raw,
                                            IntegerVector phi_m_g_index,
                                            IntegerVector indiv_non_szv_index,
                                            NumericMatrix para_phi,
                                            IntegerVector orig_d,
                                            int orig_maxd) {
  for (int count = 0; count < indiv_non_szv_index_raw.length(); count++) {
    int k = indiv_non_szv_index_raw[count] - 1; //index into raw data
    int real_k = indiv_non_szv_index[count] - 1; //index into invividual level variables
    NumericVector r = runif(NA_indiv_missing_status.rows());
    for (int i = 0; i < NA_indiv_missing_status.rows(); i++) {
      if (NA_indiv_missing_status(i,real_k)) {
        int col = phi_m_g_index[i];
        int offset = real_k* orig_maxd;
        NumericVector c = para_phi.column(col -1);
        household(i,k) = samplew(c.begin() + offset, orig_d[real_k], r[i]);
      }
    }
  }

  return(household);
}

// [[Rcpp::export]]
List SampleMissing(List MissData, List para, List orig,List G_household,
                   IntegerVector M, List hyper) {
  IntegerVector G_Individuals =  G_household["G_Individuals"];
  IntegerVector G = G_household["G"];
  int hyper_SS = hyper["SS"];
  List lambda = para["lambda"];
  NumericMatrix phi = para["phi"];
  IntegerVector d = orig["d"];
  IntegerVector maxd = orig["maxd"];
  IntegerMatrix household = MissData["household"];
  LogicalMatrix NA_house_missing_status = MissData["NA_house_missing_status"];
  LogicalMatrix NA_indiv_missing_status = MissData["NA_indiv_missing_status"];
  IntegerVector phi_m_g_index(M.length());
  for (int i = 0; i < M.length(); i++) {
    phi_m_g_index[i] = M[i] + (G_Individuals[i] -1) * hyper_SS;
  }

  household = SampleNonStructureZerosIndivC(household,
                                            NA_indiv_missing_status,
                                            as<IntegerVector>(MissData["indiv_non_szv_index_raw"]),
                                            phi_m_g_index,
                                            as<IntegerVector>(MissData["indiv_non_szv_index"]),
                                            phi,d,maxd[0]);
  household = SampleNonStructureZerosHouseC(household,
                                            NA_house_missing_status,
                                            as<IntegerVector>(MissData["house_non_szv_index_raw"]),
                                            as<IntegerVector>(MissData["house_non_szv_index"]),
                                            lambda,G,
                                            as<IntegerVector>(orig["n_i"]));

  IntegerVector miss_Hhindex = MissData["miss_Hhindex"];
  List miss_Hh_invidual_index = MissData["miss_Hh_invidual_index"];
  IntegerVector batches = MissData["n_batch_imp"];
  IntegerVector household_variable_index = MissData["household_variable_index"];
  IntegerVector individual_variable_index = MissData["individual_variable_index"];
  IntegerVector house_szv_index = MissData["house_szv_index"];
  IntegerVector indiv_szv_index = MissData["indiv_szv_index"];

  IntegerVector n_0_reject = MissData["n_0_reject"];

  for (int i = 0; i < miss_Hhindex.length(); i++) {
    int s = miss_Hhindex[i] - 1;
    IntegerVector another_index = miss_Hh_invidual_index[s]; //the row index for all other family members
    int n_indiv = another_index.length();
    IntegerMatrix X_house(batches[s], household_variable_index.length());
    for (int b = 0; b < batches[s]; b++) {
        int house_row =  another_index[0] -1;
        for (int v = 0; v < household_variable_index.length();v++) {
          X_house(b,v) = household(house_row,household_variable_index[v] - 1);
        }
    }
    IntegerMatrix X_indiv(batches[s] * n_indiv, individual_variable_index.length());
    for (int b = 0; b < batches[s]; b++) {
      for (int ind = 0; ind < another_index.length(); ind++ ) {
        int indiv_row =  another_index[ind] -1;
        for (int v = 0; v < individual_variable_index.length();v++) {
          X_indiv( b*n_indiv + ind,v) = household(indiv_row,individual_variable_index[v] - 1);
        }
      }
    }

    IntegerVector index(another_index.length());
    for (int j = 0; j < index.length(); j++) {
      index[j] = M[another_index[j] - 1] + (G[s] -1) * hyper_SS;
    }

    List OneHousehold = SampleMissingForOneHousehold_batch(another_index, X_house, X_indiv,house_szv_index,
                                                           NA_house_missing_status,
                                                           indiv_szv_index, NA_indiv_missing_status,
                                                           lambda,phi, G[s], index,
                                                           d,maxd[0],batches[s]);

    n_0_reject[s] = n_0_reject[s] + as<int>(OneHousehold["n_0_reject"]);
    int first_valid = as<int>(OneHousehold["first_valid"]) - 1;
    IntegerMatrix House = OneHousehold["X_house_s_prop"];
    IntegerMatrix Indivs = OneHousehold["X_indiv_s_prop"];

    for (int ind = 0; ind < n_indiv; ind++) {
      int indiv_row =  another_index[ind] -1;
      for (int v = 0; v < household_variable_index.length();v++) {
        household(indiv_row,household_variable_index[v] - 1) = House(first_valid,v);
      }

      for (int v = 0; v < individual_variable_index.length();v++) {
        household(indiv_row,individual_variable_index[v] - 1) = Indivs( first_valid*n_indiv + ind,v);
      }
    }
  }
  MissData["household"] = household; //maybe not need as R/C++ all IntegerMatrix
  MissData["n_0_reject"] = n_0_reject; //numeric vector in R, integer vectort in Rcpp
  return(MissData);
}

IntegerVector sampleW_multi(NumericVector p, NumericVector d) {
  int howmany = d.length();
  IntegerVector samples(howmany);
  int n = p.length();
  samplew_multi2(p.begin(), n, d.begin(), samples.begin(), howmany);
  return samples;
}

