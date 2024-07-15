#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

int my_rand_int(int floor, int ceil){
    double x1 = runif(1, floor, ceil)[0];
    int result = (int) (x1 + 0.5);
    return result;
}

// [[Rcpp::export]]
NumericVector pmin3(NumericVector vec1, NumericVector vec2){
    int n = vec1.size();
    NumericVector out(n);
    for (int i = 0; i < n; i++) {
        out[i] = std::min(vec1[i], vec2[i]);
    }
    return out;
}


// [[Rcpp::export]]
void put_val(NumericMatrix mtx, int xpos, int ypos, double val){
    mtx(xpos - 1, ypos - 1) = val;
}

double get_contributions_cpp(NumericMatrix F, NumericMatrix ND, NumericMatrix DM, int idx){
    double A1 = 0.0;
    NumericVector contrib_rows(F.nrow());
    NumericVector contrib_cols(F.ncol());
    for (int i = 0; i < F.ncol(); ++i) {
        double val1 = F(idx, i);
        double val2 = DM(idx, i);
        contrib_cols[i] = ND(idx,i)*(val1 / val2);
       
    }
    for (int i = 0; i < F.nrow(); ++i) {
        double val1 = F(i, idx);
        double val2 = DM(i, idx);
        contrib_rows[i] = ND(i, idx)*(val1 / val2);
    }
    for (int i = 0; i < F.ncol(); ++i) {
        A1+= contrib_cols[i];
    }
    for (int i = 0; i < F.nrow(); ++i) {
        A1 += contrib_rows[i];
    }
    return A1;
}

void update_DegreeMinima(NumericMatrix DM, NumericVector mt, double val, int idx){
    for (int i = 0; i < DM.ncol(); ++i) {
        double myval = std::min(val, mt[i]);
        DM(idx, i) = myval;
        DM(i, idx) = myval;
    }
}

void update_NegativeDeltas(NumericMatrix ND, NumericVector mt, int val, int idx){
    for (int i = 0; i < ND.ncol(); ++i) {
        // Update the row
        double myval = (val > mt[i]);
        ND(idx, i) = myval;
        // Now update the column
        myval = (val < mt[i]);
        ND(i, idx) = myval;
    }
    //Rcout << ND << std::endl;
}

void update_FillFactors_rem0(NumericMatrix Fill, NumericMatrix mtx, int pos1, int pos2){
    for (int i = 0; i < Fill.ncol(); ++i) {
        // Update the row
        Fill(pos1, i) -= mtx(i, pos2);
        Fill(i, pos1) -= mtx(i, pos2);
    }
    Fill(pos1, pos1) -= 1;
}

void update_FillFactors_remt(NumericMatrix Fill, NumericMatrix mtx, int pos1, int pos2){

    for (int i = 0; i < Fill.ncol(); ++i) {
        // Update the row
        Fill(pos1, i) -= mtx(pos2, i);
        Fill(i, pos1) -= mtx(pos2, i);
    }
    Fill(pos1, pos1) -= 1;
}

void update_FillFactors_add0(NumericMatrix Fill, NumericMatrix mtx, int pos1, int pos2){
    for (int i = 0; i < Fill.ncol(); ++i) {
        // Update the row
        Fill(pos1, i) += mtx(i, pos2);
        Fill(i, pos1) += mtx(i, pos2);
    }
    Fill(pos1, pos1) -= 1;
}
void update_FillFactors_addt(NumericMatrix Fill, NumericMatrix mtx, int pos1, int pos2){
    for (int i = 0; i < Fill.ncol(); ++i) {
        // Update the row
        Fill(pos1, i) += mtx(pos2, i);
        Fill(i, pos1) += mtx(pos2, i);
    }
    Fill(pos1, pos1) -= 1;
}

NumericMatrix mmult(const NumericMatrix& m1, const NumericMatrix& m2){
    if (m1.ncol() != m2.nrow()) stop ("Incompatible matrix dimensions");
    NumericMatrix out(m1.nrow(),m2.ncol());
    NumericVector rm1, cm2;

    for (size_t i = 0; i < m1.nrow(); ++i) {
        rm1 = m1(i,_);
        for (size_t j = 0; j < m2.ncol(); ++j) {
            cm2 = m2(_,j);
            out(i,j) = std::inner_product(rm1.begin(), rm1.end(), cm2.begin(), 0.);
        }
    }
    return out;
}

// [[Rcpp::export]]
NumericVector computeMT0(NumericMatrix mtx){
    int N = mtx.nrow();
    NumericVector mt0(N);
    // make this for loop run in parallel :)
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < mtx.ncol(); ++j) {
            mt0(i) += mtx(i, j);
        }
    }
    return mt0;
}

// [[Rcpp::export]]
NumericVector computeMTt(NumericMatrix mtx){
    int N = mtx.ncol();
    NumericVector mtt(N);
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < mtx.nrow(); ++j) {
            mtt(i) += mtx(j,i);
        }
    }
    return mtt;
}

// [[Rcpp::export]]
NumericMatrix computeFill0(NumericMatrix mtx){
    NumericMatrix mtx_transpose(mtx.ncol(), mtx.nrow());
    for (int i = 0; i < mtx.nrow(); ++i) {
        for (int j = 0; j < mtx.ncol(); ++j) {
            mtx_transpose(j,i) = mtx(i,j);
        }
    }
    return mmult(mtx, mtx_transpose);
}

// [[Rcpp::export]]
NumericMatrix computeFillt(NumericMatrix mtx){
    NumericMatrix mtx_transpose(mtx.ncol(), mtx.nrow());
    for (int i = 0; i < mtx.nrow(); ++i) {
        for (int j = 0; j < mtx.ncol(); ++j) {
            mtx_transpose(j,i) = mtx(i,j);
        }
    }
    return mmult(mtx_transpose, mtx);
}

// [[Rcpp::export]]
NumericMatrix computeDM0(NumericMatrix mtx){
    NumericVector mt0 = computeMT0(mtx);
    NumericMatrix DM0(mtx.nrow(), mtx.nrow());
    for (int i = 0; i < mtx.nrow(); ++i) {
        for (int j = 0; j < mtx.nrow(); ++j) {
            DM0(i,j) = std::min(mt0[i], mt0[j]);
        }
    }
    return DM0;
}

// [[Rcpp::export]]
NumericMatrix computeDMt(NumericMatrix mtx){
    NumericVector mtt = computeMTt(mtx);
    NumericMatrix DMt(mtx.ncol(), mtx.ncol());
    for (int i = 0; i < mtx.ncol(); ++i) {
        for (int j = 0; j < mtx.ncol(); ++j) {
            DMt(i,j) = std::min(mtt[i], mtt[j]);
        }
    }
    return DMt;
}

// [[Rcpp::export]]
NumericMatrix computeND0(NumericMatrix mtx){
    NumericVector mt0 = computeMT0(mtx);
    NumericMatrix ND0(mtx.nrow(), mtx.nrow());
    for (int i = 0; i < mtx.nrow(); ++i) {
        for (int j = 0; j < mtx.nrow(); ++j) {
            ND0(i,j) = mt0[i] > mt0[j];
        }
    }
    return ND0;
}

// [[Rcpp::export]]
NumericMatrix computeNDt(NumericMatrix mtx){
    NumericVector mtt = computeMTt(mtx);
    NumericMatrix NDt(mtx.ncol(), mtx.ncol());
    for (int i = 0; i < mtx.ncol(); ++i) {
        for (int j = 0; j < mtx.ncol(); ++j) {
            NDt(i,j) = mtt[i] > mtt[j];
        }
    }
    return NDt;
}

// [[Rcpp::export]]
NumericVector computeS(NumericMatrix mtx){
    NumericVector S(2);
    NumericMatrix F0 = computeFill0(mtx);
    NumericMatrix Ft = computeFillt(mtx);
    NumericMatrix DM0 = computeDM0(mtx);
    NumericMatrix DMt = computeDMt(mtx);
    NumericMatrix ND0 = computeND0(mtx);
    NumericMatrix NDt = computeNDt(mtx);

    double s0 = 0.0;
    double s1 = 0.0;
    for (int i = 0; i < mtx.nrow(); ++i) {
        for (int j = 0; j < mtx.nrow(); ++j) {
            s0 += ND0(i,j) * (F0(i,j) / DM0(i,j));
        }
    }
    for (int i = 0; i < mtx.ncol(); ++i) {
        for (int j = 0; j < mtx.ncol(); ++j) {
            s1 += NDt(i,j) * (Ft(i,j) / DMt(i,j));
        }
    }
    S(0) = s0;
    S(1) = s1;
    return S;
}

// [[Rcpp::export]]
double nodf_one_link_removed_cpp(NumericMatrix mtx, int xpos, int ypos, NumericVector mt_0, NumericVector mt_t, NumericMatrix F0, NumericMatrix Ft, NumericMatrix DM0, NumericMatrix DMt, NumericMatrix ND0, NumericMatrix NDt, NumericVector S){
    int NodesA = mtx.ncol();
    int NodesB = mtx.nrow();
    // Internal xPos and yPos as Cpp and R indexing differs
    int xPos = xpos - 1;
    int yPos = ypos - 1;

    double my_norm = 0.5*((NodesA *(NodesA-1)) + (NodesB *(NodesB-1)));

    // Modify the matrix appropriately:
    mtx(xPos, yPos) = 0.0;

    // Compute old contributions:
    double old_contrib0 = get_contributions_cpp(F0, ND0, DM0, xPos);
    double old_contribt = get_contributions_cpp(Ft, NDt, DMt, yPos);

    // Modify the marginal totals:
    mt_0(xPos) -= 1;
    mt_t(yPos) -= 1;

    // Update the degree minimia matrix:
    update_DegreeMinima(DM0, mt_0, mt_0[xPos], xPos);
    update_DegreeMinima(DMt, mt_t, mt_t[yPos], yPos);

    // Update the negative deltas:
    update_NegativeDeltas(ND0, mt_0, mt_0[xPos], xPos);
    update_NegativeDeltas(NDt, mt_t, mt_t[yPos], yPos);

    // Update the Fill Factors:
    update_FillFactors_rem0(F0, mtx, xPos, yPos);
    update_FillFactors_remt(Ft, mtx, yPos, xPos);
    // Compute new contributions:
    double new_contrib0 = get_contributions_cpp(F0, ND0, DM0, xPos);
    double new_contribt = get_contributions_cpp(Ft, NDt, DMt, yPos);
    // Update the sums:
    S[0] = S[0] - old_contrib0 + new_contrib0;
    S[1] = S[1] - old_contribt + new_contribt;

    // Compute the new nodf value:
    double nodf = (S[0] + S[1]) / my_norm;
    return nodf;
}


// [[Rcpp::export]]
double nodf_one_link_added_cpp(NumericMatrix mtx, int xpos, int ypos, NumericVector mt_0, NumericVector mt_t, NumericMatrix F0, NumericMatrix Ft, NumericMatrix DM0, NumericMatrix DMt, NumericMatrix ND0, NumericMatrix NDt, NumericVector S){
    int NodesA = mtx.ncol();
    int NodesB = mtx.nrow();
    // Internal xPos and yPos as Cpp and R indexing differs
    int xPos = xpos - 1;
    int yPos = ypos - 1;

    double my_norm = 0.5*((NodesA *(NodesA-1)) + (NodesB *(NodesB-1)));

    // Modify the matrix appropriately:
    mtx(xPos, yPos) = 1.0;

    // Compute old contributions:
    double old_contrib0 = get_contributions_cpp(F0, ND0, DM0, xPos);
    double old_contribt = get_contributions_cpp(Ft, NDt, DMt, yPos);

    // Modify the marginal totals:
    mt_0(xPos) += 1;
    mt_t(yPos) += 1;

    // Update the degree minimia matrix:
    update_DegreeMinima(DM0, mt_0, mt_0[xPos], xPos);
    update_DegreeMinima(DMt, mt_t, mt_t[yPos], yPos);

    // Update the negative deltas:
    update_NegativeDeltas(ND0, mt_0, mt_0[xPos], xPos);
    update_NegativeDeltas(NDt, mt_t, mt_t[yPos], yPos);

    // Update the Fill Factors:
    update_FillFactors_add0(F0, mtx, xPos, yPos);
    update_FillFactors_addt(Ft, mtx, yPos, xPos);

    // Compute new contributions:
    double new_contrib0 = get_contributions_cpp(F0, ND0, DM0, xPos);
    double new_contribt = get_contributions_cpp(Ft, NDt, DMt, yPos);

    // Update the sums:
    S[0] = S[0] - old_contrib0 + new_contrib0;
    S[1] = S[1] - old_contribt + new_contribt;

    // Compute the new nodf value:
    double nodf = (S[0] + S[1]) / my_norm;
    return nodf;
}

// [[Rcpp::export]]
NumericMatrix get_valid_ones_cpp(NumericMatrix mtx){
    int NodesA = mtx.nrow();
    int NodesB = mtx.ncol();
    // First count the number of ones in the submatrix so we know
    // how much memory to allocate:
    int numOnes = 0;

    for (int i = 1; i < NodesA; ++i) {
        for (int j = 1; j < NodesB; ++j) {
            numOnes += mtx(i,j);
        }
    }
    NumericMatrix pos_list(numOnes, 2);
    int myCounter = 0;
    for (int i = 1; i < NodesA; ++i) {
        for (int j = 1; j < NodesB; ++j) {
            if(mtx(i,j) == 1){
                pos_list(myCounter, 0) = i + 1;
                pos_list(myCounter, 1) = j + 1;
                myCounter += 1;
            }
        }
    }
    return pos_list;
}

// [[Rcpp::export]]
NumericMatrix websearch_cpp(NumericMatrix mtx, int threshold = 2){
    int NodesA = mtx.nrow();
    int NodesB = mtx.ncol();

    int nRows = 0;
    for (int i = 1; i < NodesA; ++i) {
        for (int j = 1; j < NodesB; ++j) {
            if(mtx(i,j) == 0){
                int nbhd_val = 0;
                nbhd_val += mtx(i-1, j);
                nbhd_val += mtx(i, j-1);
                if(i < NodesA - 1){
                    nbhd_val += mtx(i+1, j);
                }
                if(j < NodesB - 1){
                    nbhd_val += mtx(i, j+1);
                }
                if(nbhd_val >= threshold){
                    nRows += 1;
                }
            }
        }
    }
    // Create the result matrix:
    NumericMatrix pos_list(nRows, 2);
    int my_counter = 0;
    for (int i = 1; i < NodesA; ++i) {
        for (int j = 1; j < NodesB; ++j) {
            if(mtx(i,j) == 0){
                int nbhd_val = 0;
                nbhd_val += mtx(i-1, j);
                nbhd_val += mtx(i, j-1);
                if(i < NodesA - 1){
                    nbhd_val += mtx(i+1, j);
                }
                if(j < NodesB - 1){
                    nbhd_val += mtx(i, j+1);
                }
                if(nbhd_val >= threshold){
                    pos_list(my_counter, 0) = i + 1;
                    pos_list(my_counter, 1) = j + 1;
                    my_counter += 1;
                }
            }
        }
    }
    return pos_list;
}

// [[Rcpp::export]]
NumericMatrix get_zeros_cpp(NumericMatrix mtx){
    int NodesA = mtx.nrow();
    int NodesB = mtx.ncol();

    int nRows = 0;
    for (int i = 1; i < NodesA; ++i) {
        for (int j = 1; j < NodesB; ++j) {
            nRows += (1.0 - mtx(i,j));
        }
    }
    // Create the result matrix:
    NumericMatrix pos_list(nRows, 2);
    int my_counter = 0;
    for (int i = 1; i < NodesA; ++i) {
        for (int j = 1; j < NodesB; ++j) {
            if(mtx(i,j) == 0){
                pos_list(my_counter, 0) = i + 1;
                pos_list(my_counter, 1) = j + 1;
                my_counter += 1;
            }
        }
    }
    return pos_list;
}

NumericMatrix copy_mtx(NumericMatrix mtx){
    NumericMatrix my_mtx(mtx.nrow(), mtx.ncol());
    for (int i = 0; i < mtx.nrow(); ++i) {
        for (int j = 0; j < mtx.ncol(); ++j) {
            my_mtx(i, j) = mtx(i,j);
        }
    }
    return my_mtx;
}

double get_paired_nestedness(NumericMatrix mtx, bool rows){
    double res = 0.0;
    if(rows){
        NumericMatrix F0 = computeFill0(mtx);
        NumericMatrix DM0 = computeDM0(mtx);
        NumericMatrix ND0 = computeND0(mtx);

        for (int i = 0; i < mtx.nrow(); ++i) {
            for (int j = 0; j < mtx.nrow(); ++j) {
                res += ND0(i,j) * F0(i,j) / DM0(i,j);
            }
        }
    }else{
        NumericMatrix Ft = computeFillt(mtx);
        NumericMatrix DMt = computeDMt(mtx);
        NumericMatrix NDt = computeNDt(mtx);

        for (int i = 0; i < mtx.ncol(); ++i) {
            for (int j = 0; j < mtx.ncol(); ++j) {
                res += NDt(i,j) * Ft(i,j) / DMt(i,j);
            }
        }
    }
    return res;
}

//' Raw NODF calculation
//'
//' Calculates the raw NODF of a bipartite incidence matrix
//' @param mtx A numeric matrix describing a bipartite network (a bipartite incidence matrix where elements are positive numbers if nodes interact, and 0 otherwise).
//' @details For a given network, \code{nodf_cpp} calculates the raw NODF value. Calculation is fast as the code is implemented in C++.
//' @return Returns the NODF of the network.
//' @examples
//' set.seed(123)
//' nodf_cpp(matrix(sample(x = 0:1, size = 100, replace = TRUE),10,10))
//' @useDynLib maxnodf
//' @import Rcpp
//' @export
// [[Rcpp::export]]
double nodf_cpp(NumericMatrix mtx){
    int NodesA = mtx.nrow();
    int NodesB = mtx.ncol();
    double fac = ((NodesA - 1) * NodesA + (NodesB-1)*NodesB) * 0.5;
    double nodf1 = get_paired_nestedness(mtx, true);
    double nodf2 = get_paired_nestedness(mtx, false);
    return (nodf1 + nodf2) / fac;
}

NumericVector get_row_from_mtx(NumericMatrix mtx, int rowidx){
    NumericVector vec(mtx.ncol());
    for (int i = 0; i < mtx.ncol(); ++i) {
        vec[i] = mtx(rowidx, i);
    }
    return vec;
}

double accept_probability_cpp(double old_nodf, double new_nodf, double temp){
    double old_cost = 1.0 - old_nodf;
    double new_cost = 1.0 - new_nodf;
    double res = -1.0;

    if(old_cost > new_cost){
        res = 1.0;
    }else{
        double my_exp = -1.0*(new_cost - old_cost) / temp;
        res = exp(my_exp);
    }
    return res;
}

void display_txt_pbar(double progress){
    int barWidth = 70;

    Rcout << "[";
    int pos = barWidth * progress;
    for (int i = 0; i < barWidth; ++i) {
        if (i < pos) Rcout << "=";
        else if (i == pos) Rcout << ">";
        else Rcout << " ";
    }
    Rcout << "] " << int(progress * 100.0) << " %\r";
    Rcout.flush();
}

// [[Rcpp::export]]
NumericMatrix hill_climb_step_cpp(NumericMatrix mtx, int R = 2){
    NumericMatrix opt_mtx = copy_mtx(mtx);
    double opt_nodf = nodf_cpp(mtx);
    // initialise NODF data
    NumericVector mt_0 = computeMT0(mtx);
    NumericVector mt_t = computeMTt(mtx);
    NumericMatrix F0 = computeFill0(mtx);
    NumericMatrix Ft = computeFillt(mtx);
    NumericMatrix DM0 = computeDM0(mtx);
    NumericMatrix DMt = computeDMt(mtx);
    NumericMatrix ND0 = computeND0(mtx);
    NumericMatrix NDt = computeNDt(mtx);
    NumericVector S = computeS(mtx);
    double rem_nodf = 0.0;
    double new_nodf = 0.0;

    // Compute valid ones:
    NumericMatrix oPosList = get_valid_ones_cpp(mtx);
    for (int i = 0; i < oPosList.nrow(); ++i) {
        NumericVector oPos(2);
        oPos[0] = oPosList(i,0);
        oPos[1] = oPosList(i,1);
        // Iterate over neighbor zeros:
        for (int xshift = -R;  xshift<= R; ++xshift) {
            for (int yshift = -R;  yshift<= R; ++yshift) {
                int xnew = oPos[0] + xshift;
                int ynew = oPos[1] + yshift;
                if(1 <= xnew && xnew <= mtx.nrow() and 1 <= ynew and ynew <= mtx.ncol()){
                    // Test if we found a zeros position:
                    if(mtx(xnew - 1, ynew - 1) == 0){
                        NumericVector zPos(2);
                        zPos[0] = xnew;
                        zPos[1] = ynew;
                        // Perform the swap:
                        rem_nodf = nodf_one_link_added_cpp(mtx, zPos[0], zPos[1],
                                mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
                        new_nodf = nodf_one_link_removed_cpp(mtx, oPos[0], oPos[1],
                                mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
                        // Found a new optimal solution!
                        if(new_nodf > opt_nodf){
                            opt_mtx = copy_mtx(mtx);
                            opt_nodf = new_nodf;
                        }
                        // Reverse the swap:
                        rem_nodf = nodf_one_link_added_cpp(mtx, oPos[0], oPos[1],
                                mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
                        new_nodf = nodf_one_link_removed_cpp(mtx, zPos[0], zPos[1],
                                mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
                    }

                }
            }
        }
    }
    return opt_mtx;
}

// [[Rcpp::export]]
NumericMatrix full_hill_climb_cpp(NumericMatrix mtx, int R = 2){
    double old_nodf = -100.0;
    double eps = 0.00000000001;
    int counter = 0;
    while(old_nodf + eps < nodf_cpp(mtx)){
        counter = counter + 1;
        old_nodf = nodf_cpp(mtx);
        Rcout << "Hillclimb round: "<< counter << ". Current NODF= "<<old_nodf<< std::endl;
        mtx = hill_climb_step_cpp(mtx, R);
    }
    return mtx;
}

// [[Rcpp::export]]
NumericMatrix sim_anneal_opt_cpp(NumericMatrix mtx, double alpha = 0.998, int
        iters = 8, double init_temp = 0.25, double min_temp = 0.0001){
    Rcout << std::endl;
    // Outer frame of the simulated annealing simulation
    int cool_steps = round(log(min_temp / init_temp) /log(alpha));

    NumericMatrix opt_mtx = copy_mtx(mtx);

    // initialise NODF data
    NumericVector mt_0 = computeMT0(mtx);
    NumericVector mt_t = computeMTt(mtx);
    NumericMatrix F0 = computeFill0(mtx);
    NumericMatrix Ft = computeFillt(mtx);
    NumericMatrix DM0 = computeDM0(mtx);
    NumericMatrix DMt = computeDMt(mtx);
    NumericMatrix ND0 = computeND0(mtx);
    NumericMatrix NDt = computeNDt(mtx);
    NumericVector S = computeS(mtx);

    // Backup for the optimal data:
    NumericVector mt0_opt = computeMT0(opt_mtx);
    NumericVector mtt_opt = computeMTt(opt_mtx);
    NumericMatrix F0_opt = computeFill0(opt_mtx);
    NumericMatrix Ft_opt = computeFillt(opt_mtx);
    NumericMatrix DM0_opt = computeDM0(opt_mtx);
    NumericMatrix DMt_opt = computeDMt(opt_mtx);
    NumericMatrix ND0_opt = computeND0(opt_mtx);
    NumericMatrix NDt_opt = computeNDt(opt_mtx);
    NumericVector S_opt = computeS(opt_mtx);

    double temp = init_temp;
    NumericMatrix oPosList = get_valid_ones_cpp(mtx);
    NumericMatrix zPosList = get_zeros_cpp(mtx);
    double opt_nodf = nodf_cpp(opt_mtx);
    double curr_nodf = nodf_cpp(mtx);
    double old_nodf = curr_nodf;
    double rem_nodf = 0.0;
    double new_nodf = 0.0;
    if(zPosList.nrow() < 1){
        return opt_mtx;
    }
    if(oPosList.nrow() < 1){
        return opt_mtx;
    }
    for (int i = 0; i < cool_steps; ++i) {
        double progress = ((double) i + 1.0) / ((double) cool_steps);
        display_txt_pbar(progress);
        for (int j = 0; j < iters; ++j) {
            // Choose random positions to swap:
            int oIdx = my_rand_int(0, oPosList.nrow() - 1);
            int zIdx = my_rand_int(0, zPosList.nrow() - 1);

            NumericVector oPos = get_row_from_mtx(oPosList, oIdx);
            NumericVector zPos = get_row_from_mtx(zPosList, zIdx);

            // Perform the swap:
            //Rcout << oPos[0]<< "," << oPos[1] <<": "<< mtx(oPos[0]-1, oPos[1]-1)<< std::endl;
            rem_nodf = nodf_one_link_added_cpp(mtx, zPos[0], zPos[1], mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
            // Rcout << rem_nodf << ", " << nodf_cpp(mtx) << std::endl;
            new_nodf = nodf_one_link_removed_cpp(mtx, oPos[0], oPos[1], mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
            //Rcout <<  new_nodf << ", " << nodf_cpp(mtx) << std::endl;
            //Rcout << oPos[0]<< "," << oPos[1] <<": "<< mtx(oPos[0]-1, oPos[1]-1)<< std::endl;

            if(new_nodf > opt_nodf){
                opt_mtx = copy_mtx(mtx);
                opt_nodf = new_nodf;
                Rcout << "Hill climbing!" << std::endl;
                opt_mtx = full_hill_climb_cpp(opt_mtx);
                Rcout << "Found opt mtx with nodf: "<<opt_nodf<<" ("<<nodf_cpp(opt_mtx)<<")"<< std::endl;
                // Backup for the optimal data:
                mt0_opt = computeMT0(opt_mtx);
                mtt_opt = computeMTt(opt_mtx);
                F0_opt = computeFill0(opt_mtx);
                Ft_opt = computeFillt(opt_mtx);
                DM0_opt = computeDM0(opt_mtx);
                DMt_opt = computeDMt(opt_mtx);
                ND0_opt = computeND0(opt_mtx);
                NDt_opt = computeNDt(opt_mtx);
                S_opt = computeS(opt_mtx);

            }
            double rand_number = runif(0, 0.0 , 1.0)[0];
            double acc_prob= accept_probability_cpp(old_nodf, new_nodf, temp);
            if(rand_number <= acc_prob){
                // Accept the move by not changing the matrix back
                // and modifying the oPosList and zPosList accordingly
                oPosList(oIdx, 0) = zPos[0];
                oPosList(oIdx, 1) = zPos[1];
                zPosList(zIdx, 0) = oPos[0];
                zPosList(zIdx, 1) = oPos[1];
                old_nodf = curr_nodf;
                curr_nodf = new_nodf;
            }
            else{
                // Reject the move by changing back and not modifying
                // the oPosList and zPosList
                // Perform the swap:
                nodf_one_link_added_cpp(mtx, oPos[0], oPos[1], mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
                nodf_one_link_removed_cpp(mtx, zPos[0], zPos[1], mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
            }
        }
        temp = alpha * temp;
        // End of the inner loop:
        // Test if we should go back to the optimal solution:
        if(curr_nodf < 0.95 * opt_nodf){
            // Revert to the optimal solution:
            // Rcout << "Back to the optimum." << std::endl;
            curr_nodf = opt_nodf;
            mtx = copy_mtx(opt_mtx);
            mt_0 = computeMT0(mtx);
            mt_t = computeMTt(mtx);
            F0 = copy_mtx(F0_opt);
            Ft = copy_mtx(Ft_opt);
            DM0 = copy_mtx(DM0_opt);
            DMt = copy_mtx(DMt_opt);
            ND0 = copy_mtx(ND0_opt);
            NDt = copy_mtx(NDt_opt);
            S = NumericVector(2);
            S[0] = S_opt[0];
            S[1] = S_opt[1];
            // Update the position lists
            oPosList = get_valid_ones_cpp(mtx);
            zPosList = get_zeros_cpp(mtx);
        }
        //Rcout << "Curr nodf: " << curr_nodf << " Opt nodf: " << opt_nodf << std::endl;
    }
    Rcout << std::endl;
    return opt_mtx;
}
