#include <cstring>
#include <Rcpp.h>
using namespace Rcpp;
#include "checkconstraints.h"

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

void checkconstraints_imp_HHhead_at_group_level(int *data, int *isPossible,int hh_size, int DIM, int nHouseholds, int begin, int end) {
  int realsize = hh_size + 1;
  //column 2, 5, 6 = sex, age and relte //zero-based here
  int column[COL]; column[0] = 0; column[1] = 3; column[2] = 4;

  int *datah = new int[realsize * COL + 1];
  for (int m = begin; m < end; m++){
    //for each household member
    //need to check the logic here later. It works, but very strange.
    for (int j = 1; j < realsize; j++) {
      for (int k = 0; k < COL; k++) { //0 sex, sex,sex,...age,age,age,...relte,relte,relate...
        datah[k * realsize + j] = data[((j-1) * DIM + column[k]+2) * nHouseholds + m];
        if (k+1 == COL) { //relate column
          datah[k * realsize + j] = datah[k * realsize + j] + 1; //addjust by adding 1
        }
      }
    }
    datah[realsize] = data[(column[0]+8) * nHouseholds + m];
    datah[2 * realsize] = data[(column[1]+8) * nHouseholds + m];
    datah[3 * realsize] = 1;
    isPossible[m] = isValid(datah, realsize);
  }
  delete [] datah;
}

struct HHheadConstraintChecker : public Worker {
  //input
  int *data;
  int hh_size;
  int DIM;
  int nHouseholds;

  //output
  int *isPossible;

  HHheadConstraintChecker(int *data, int hh_size, int DIM, int nHouseholds, int *isPossible)
    :data(data), hh_size(hh_size), DIM(DIM), nHouseholds(nHouseholds), isPossible(isPossible) {
  }
  void operator()(std::size_t begin, std::size_t end) {
    ::checkconstraints_imp_HHhead_at_group_level(data, isPossible, hh_size, DIM, nHouseholds, begin, end);
  }
};

struct Sum : public Worker {
  int* input;

  // accumulated value
  int value;

  // constructors
  Sum(int* input) : input(input), value(0) {}
  Sum(const Sum& sum, Split) : input(sum.input),  value(0) {}

  // accumulate just the element of the range I've been asked to
  void operator()(std::size_t begin, std::size_t end) {
    value += std::accumulate(input + begin, input + end, 0);
  }

  // join my value with that of another Sum
  void join(const Sum& rhs) {
    value += rhs.value;
  }
};


int checkconstraints_imp_HHhead_at_group_level(int *data, int *isPossible,int hh_size, int DIM, int nHouseholds, int parallel) {
  int totalpossible = 0;

  if (parallel != 0) {
    HHheadConstraintChecker cc(data, hh_size, DIM, nHouseholds, isPossible);
    parallelFor(0, nHouseholds, cc,GRAINSIZE);

    Sum sum(isPossible);
    parallelReduce(0, nHouseholds, sum, GRAINSIZE);
    totalpossible = sum.value;
  } else {
    checkconstraints_imp_HHhead_at_group_level(data, isPossible, hh_size, DIM, nHouseholds, 0, nHouseholds);
    totalpossible = std::accumulate(isPossible, isPossible + nHouseholds, 0);
  }
  return totalpossible;
}


// [[Rcpp::export]]
List checkconstraints_HHhead_at_group_level(IntegerMatrix data,int neededpossiblehh, int hh_size, int parallel) {
  int nHouseholds = data.nrow(); //data item in rows. !!

  //use the raw data instead, which has hh_size * DIM + 1 + hh_size
  int columns = data.ncol();
  int DIM = (columns -1) / hh_size -1;

  IntegerVector isPossible(nHouseholds);
  int totalpossible = checkconstraints_imp_HHhead_at_group_level(data.begin(), isPossible.begin(), hh_size, DIM, nHouseholds, parallel);

  int rows = nHouseholds-totalpossible;
  IntegerMatrix newdata(columns,rows);
  IntegerMatrix syndata(columns,totalpossible);
  IntegerVector impossible_counts(totalpossible);

  int count1 = 0;
  int count2 = 0;
  for (int i = 0; i < nHouseholds && count2 < neededpossiblehh; i++) {
    if (isPossible[i] == 0) { //found an impossible household
      for (int j = 0; j < columns; j++) { //impossible ones
        newdata[count1*columns+j] = data[j*nHouseholds+i];
      }
      count1++;
    } else {
      impossible_counts[count2] = count1;
      for (int j = 0; j < columns; j++) { //possible ones (syndata)
        syndata[count2*columns+j] = data[j*nHouseholds+i];
      }
      count2++;
    }
  }

  if (count1 < rows) { //rows in C are the columns in the return matrix
    //need to resize the output matrix
    if (count1 >= 1) {
      newdata = newdata(Range(0,columns-1), Range(0,count1-1));
    } else {
      newdata = R_NilValue;
    }
  }

  if (count2 < totalpossible) { //truncate possible households if too many
    //need to resize the output matrix
    if (count2 >= 1) {
      syndata = syndata(Range(0,columns-1), Range(0,count2-1));
    } else {
      syndata = R_NilValue;
    }
  }

  return List::create(Named("outcome", isPossible),
                      Named("Households", newdata),
                      Named("Index", impossible_counts),
                      Named("synHouseholds", syndata),
                      Named("possible", count2));
}


// [[Rcpp::export]]
List checkconstraints(IntegerMatrix data,int neededpossiblehh, int hh_size) {
  int nHouseholds = data.nrow(); //data item in rows. !!

  //use the raw data instead, which has hh_size * DIM + 1 + hh_size
  int columns = data.ncol();
  int DIM = (columns -1) / hh_size -1;
  //int hh_size = (columns -1) / (DIM+1);

  IntegerVector isPossible(nHouseholds);
  int totalpossible = checkconstraints_imp(data.begin(), isPossible.begin(), hh_size, DIM, nHouseholds);

  int rows = nHouseholds-totalpossible;
  IntegerMatrix newdata(columns,rows);
  IntegerMatrix syndata(columns,totalpossible);
  IntegerVector impossible_counts(totalpossible);

  int count1 = 0;
  int count2 = 0;
  for (int i = 0; i < nHouseholds && count2 < neededpossiblehh; i++) {
    if (isPossible[i] == 0) { //found an impossible household
      for (int j = 0; j < columns; j++) { //impossible ones
        newdata[count1*columns+j] = data[j*nHouseholds+i];
      }
      count1++;
    } else {
      impossible_counts[count2] = count1;
      for (int j = 0; j < columns; j++) { //possible ones (syndata)
        syndata[count2*columns+j] = data[j*nHouseholds+i];
      }
      count2++;
    }
  }

  if (count1 < rows) { //rows in C are the columns in the return matrix
    //need to resize the output matrix
    if (count1 >= 1) {
      newdata = newdata(Range(0,columns-1), Range(0,count1-1));
    } else {
      newdata = R_NilValue;
    }
  }

  if (count2 < totalpossible) { //truncate possible households if too many
    //need to resize the output matrix
    if (count2 >= 1) {
      syndata = syndata(Range(0,columns-1), Range(0,count2-1));
    } else {
      syndata = R_NilValue;
    }
  }

  return List::create(Named("outcome", isPossible),
                      Named("Households", newdata),
                      Named("Index", impossible_counts),
                      Named("synHouseholds", syndata),
                      Named("possible", count2));
}
