//============================================================================
// Name        : BET2nd.cpp
// Author      : wanzhang
// Version     :
// Copyright   : Your copyright notice
// Description : BET 2nd version in C++, Ansi-style
//============================================================================

#include "BET.h"
#include <bitset>
#include <iostream>
#include <math.h>
#include <ctime>
#include <cstdlib>
#include <string>
#include <vector>
#include <numeric>
#include <algorithm>
#include <limits>
#include <fstream>
#include <sstream>
#include <cassert>
#include <iterator>
#include <random>
#include <chrono>
#include <map>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;
using namespace N;

inline int randWrapper(const int n) { return floor(unif_rand()*n); }

// vector<vector<double> > BETfunction::imp(NumericMatrix& X)
// {
//   int r = X.nrow(), c = X.ncol();
//   vector<vector<double>> v(r, vector<double>(c));
//   for (int i = 0; i < r; i++){
//     for (int j = 0; j < c; j++)
//       v[i][j] = X(i, j);
//   }
//   return v;
// }

vector<vector<int> > BETfunction::interactions()
{
	// all interactions on depth = d
	vector<vector<int>> res;
	vector<int> nums(d);
	for (int i = 0; i < d; i++){
		nums[i] = i + 1;
	}
	for(int num: nums){
	  int n = res.size();
	  res.push_back({num});

	  for (int i = 0; i < n; ++i){
		  auto temp = res[i];
		  temp.push_back(num);
		  res.push_back(temp);
	  }
	}
	return res;
}

vector<vector<int>> BETfunction::interaction_mat()
{
  // matrix interaction for BEAST
  vector<vector<int>> itr = interactions();
  size_t len = itr.size();
  vector<vector<int>> res(len + 1, vector<int>(d));

  for(size_t i = 0; i < len; i++){
    for(size_t j = 0; j < itr[i].size(); j++){
      res[i+1][itr[i][j]-1] = 1;
    }
  }

  return res;
}

vector<string> BETfunction::interaction_index()
{
	// interactions in string: 1-d and binary
	vector<vector<int>> itr = interactions();
	vector<string> res;
	string str;
	string str0(d, '0');
	res.push_back(str0);
	for (size_t i = 0; i < itr.size(); i++)
	{
		// binary
		str = str0;
		for (size_t j = 0; j < itr[i].size(); j++){
			str.replace(itr[i][j]-1, 1, "1");
		}
		res.push_back(str);
	}
	return res;
}

vector<long long> BETfunction::BIDs()
{
  int n = (int)round(pow(2, d-1));
  vector<long long> b;
  string str1(n, '1');
  string str2(n, '0');
  //	basic configurations
  b.push_back(stoll(str2 + str1, nullptr, 2)); // A0
  for (int i = 1; i < d; i++)
  {
    //	recursively: A_i = A_{i-1} XOR (A_{i-1} << (1 << (d-i)))
    b.push_back(b[i-1] ^ (b[i-1] << (1 << (d-i-1))));
  }

  vector<vector<long long>> subs;
  //	all subsets of basic configurations
  for (long long num: b){
    int n = subs.size();
    subs.push_back({num});

    for (int i = 0; i < n; ++i){
      auto temp = subs[i];
      temp.push_back(num);
      subs.push_back(temp);
    }
  }

  double len = (int)round(pow(2, d));
  string str((int)len, '1');
  unsigned long long a0 = stoull(str, nullptr, 2);
  vector<long long> res;
  long long x;
  for (size_t i = 0; i < subs.size(); i++)
  {
    //  NOT XOR
    x = a0;
    for (size_t j = 0; j < subs[i].size(); j++){
      x = ~(x ^ subs[i][j]);
    }

    x = x & a0; // keep only 2^d digits
    res.push_back(x);
  }
  return res; // BIDs on one dimension
}

vector<vector<int>> BETfunction::ecdf_loc(vector<vector<double>>& X)
{
  int n = X.size(), p = X[0].size();
  vector<vector<int>> C(n, vector<int>(p));
  double u = 0;
  vector<double> sorted(n);
  if (p == 1)
  {
    for (int i = 0; i < n; i++){
      // throw error in R
      //			if (X[i][0] > 1 or X[i][0] < 0){
      //				// test whether unif[0,1]
      //				throw "Data out of range [0, 1]!";
      //			}
      C[i][0] = (int) ceil(X[i][0] * (int)round(pow(2, d)));
    }
  }
  else{
    if (unifMargin)
      // already uniformed
      for (int j = 0; j < p; j++){
        for (int i = 0; i < n; i++){
          C[i][j] = (int) ceil(X[i][j] * (int)round(pow(2, d)));
        }
      }
      else
      {
        for (int j = 0; j < p; j++){
          for (int i = 0; i < n; i++){
            sorted[i] = X[i][j];
          }
          // store sorted data
          sort(sorted.begin(), sorted.end(), greater<double>());
          
          unordered_map<double, int> value_to_position;  // Unordered map to store value and its original index
          
          // Store in unordered_map
          for (int rank = 0; rank < n; rank++) {
            value_to_position[sorted[rank]] = rank;
          }
          
          for (int i = 0; i < n; i++){
            //	empirical cdf
            // vector<double>::iterator ans = find(sorted.begin(), sorted.end(), X[i][j]);
            // int index = distance(sorted.begin(), ans);
            int index = value_to_position[X[i][j]];
            u = n - index;
            C[i][j] = (int) ceil(u/n * (int)round(pow(2, d)));
          }
        }
      }

  }
  return C; // Cij
}

map<vector<int>, int> BETfunction::groupC(vector<vector<int>>& c)
{
  map<vector<int>, int> count;
  for (auto& v: c){
    count[v]++;
  }
  return count;
}

int BETfunction::locate(int c, long long b)
{
  // the c th bits of b (BID)
  int move = (int)round(pow(2, d)) - c;
  int res = (b >> move) & 1;
  return res;
}

vector<vector<vector<int>>> BETfunction::CBIDs(map<vector<int>, int>& count)
{
  size_t numCount = count.size(), numBIDs = (int)round(pow(2, d));
  // store all Cij * BIDs in to a matrix: p * #BIDs * #Cijs
  vector<vector<vector<int>>> res(p, vector<vector<int>>(numBIDs, vector<int>(numCount)));

  for (int i = 0; i < (int)p; i++){
    size_t j = 0;
    vector<vector<int>> temp(numBIDs, vector<int>(numCount, 1));
    // go over all BIDs:
    for (map<vector<int>, int>::iterator it=count.begin(); it!=count.end()&&(j < numCount); ++it, j++){
      // go over all p dimensions:
      for (size_t k = 0; k < numBIDs - 1; k++){
        temp[k+1][j] = locate(it->first[i], bid[k]);
      }
    }
    res[i] = temp;
  }
  return res;
}

vector<vector<int>> BETfunction::allColor(vector<vector<int>>& Cij)
{
  size_t numCount = Cij.size(), numBIDs = (int)round(pow(2, d)), total = (int)round(pow(2, d*p));
  vector<vector<int>> res(total, vector<int>(numCount));
  // store all npoints * BIDs in to a matrix: p * #BIDs * n
  vector<vector<vector<int>>> store(p, vector<vector<int>>(numBIDs, vector<int>(numCount)));

  for (int i = 0; i < (int)p; i++){
    vector<vector<int>> temp(numBIDs, vector<int>(numCount, 1));
    // go over all BIDs:
    for (size_t j = 0; j < numCount; j++){
      // go over all p dimensions:
      for (size_t k = 0; k < numBIDs - 1; k++){
        temp[k+1][j] = locate(Cij[j][i], bid[k]);
      }
    }
    store[i] = temp;
  }

#ifdef _OPENMP
  omp_set_num_threads(numThread);
#pragma omp parallel for
#endif

  for (size_t th = 1; th < numThread + 1; th++){
    //		double wtime = omp_get_wtime();
    for (size_t i = thread[th - 1]; i < thread[th]; i++){
      int loc0 = 1;
      for (size_t j = 0; j < numCount; j++){
        for (size_t k = 0; k < p; k++){
          loc0 = (~(loc0 ^ store[k][allidx[i][k]][j])) & 1;
        }
        //store color of point j for interaction i
        res[i][j] = 2*loc0 - 1;
        loc0 = 1;
      }
    }
  }

  return res;
}

vector<vector<size_t>> BETfunction::allComb(size_t p, int dep)
{
  // # of BIDs
  size_t b = (int)round(pow(2, dep));

  // # of combinations = 2^d^p
  size_t rows = (int)round(pow(b, p));

  // store all combinations
  vector<vector<size_t>> res(rows, vector<size_t>(p));

  vector<size_t> indices(p);
  vector<size_t> choose(p);

  // p columns, each column from 0 to 2^d-1
  vector<vector<size_t>> comb(p, vector<size_t>(b));
  for (size_t i = 0; i < p; i++){
    for (size_t j = 0; j < b; j++){
      comb[i][j] = j;
    }
  }

  size_t i = 0;
  size_t k = 0;
  while (i < p){
    for (size_t j = 0; j < p; j++){
      choose[j] = comb[j][indices[j]];
    }

    // store into res
    res[k] = choose;

    i = 0;
    while (i < comb.size() && ++indices[i] == comb[i].size())
      indices[i++] = 0;

    k++;
  }
  return res;
}

static bool abs_compare(int a, int b)
{
  return (std::abs(a) < std::abs(b));
}

bool BETfunction::isIndex(vector<size_t>& idx)
{
  bool res = 1;
  if(!testUnif){
    for(size_t i = 0; i < indepIndex.size(); i++){
      bool nonZero = 0;
      for(size_t j = 0; j < indepIndex[i].size(); j++){
        nonZero = nonZero || (idx[indepIndex[i][j]-1] != 0);
      }
      res = res && nonZero;
    }
  }

  return res;
}

vector<int> BETfunction::symmstats(vector<int>& countValues, vector<vector<vector<int>>>& matrix, size_t sampleSize)
{
  size_t total = (int)round(pow(2, p*d));
  vector<int> symmstats(total, 0);
  size_t numCount = countValues.size();
  // vector<vector<int>> inter_idx(total);

#ifdef _OPENMP
  omp_set_num_threads(numThread);
#pragma omp parallel for
#endif

  for (size_t th = 1; th < numThread + 1; th++){
    //		double wtime = omp_get_wtime();
    for (size_t i = thread[th - 1]; i < thread[th]; i++){
      string bi = "";
      // vector<int> iidx(p);
      for (size_t v = 0; v < p; v++){
        // store interaction
        out_symminter[v][i] = inter[allidx[i][v]];
        // add a - for divide
        if(v < p-1){
          bi += (inter[allidx[i][v]] + "-");
        }else{
          bi += inter[allidx[i][v]];
        }
        // iidx[v] = allidx[i][v];
      }
      binary_inter[i] = bi;
      // inter_idx[i] = iidx;
      if ( (testUnif && i > 0) || ( (count(allidx[i].begin(), allidx[i].end(), 0) <= (int)(p - 2)) && isIndex(allidx[i]) ) || (p == 1 && i > 0) ){
        int loc0 = 1, sum = 0;
        for (size_t j = 0; j < numCount; j++){
          for (size_t k = 0; k < p; k++){
            loc0 = (~(loc0 ^ matrix[k][allidx[i][k]][j])) & 1;
          }
          sum += loc0 * countValues[j];
          loc0 = 1;
        }
        //store symmetry statistics
        symmstats[i] = 2 * sum - (int)sampleSize;
      }
    }
  }

  // find the extreme stat
  vector<int>::iterator findMax = max_element(symmstats.begin(), symmstats.end(), abs_compare);
  size_t max_idx = distance(symmstats.begin(), findMax);

  // max interaction index
  // vector<int> max_interaction = inter_idx[max_idx];
  string max_interaction = binary_inter[max_idx];

  // count most frequent max interaction
  countInteraction[max_interaction]++;

  return symmstats;
}


// ----------------add new version for symmstats: iterBET----------------------------------
map<vector<int>, int> BETfunction::allPosition(int dep){
  //X:n by p (p&d is already defined in class privately, delete!)
  //find all 2^pd grid, a map only with key for this function, used for ecdf_loc and getPattern

  map<vector<int>, int> count;

  // 2^d: # grid on each edge
  size_t b = (int)round(pow(2, dep));

  vector<size_t> indices(p);
  vector<int> choose(p);

  // p columns, each column from 0 to 2^d-1
  vector<vector<int>> comb(p, vector<int>(b));
  for (size_t i = 0; i < p; i++){
    for (int j = 0; j < b; j++){
      comb[i][j] = j;
    }
  }

  size_t i = 0;
  while (i < p){
    for (size_t j = 0; j < p; j++){
      choose[j] = comb[j][indices[j]];
    }

    // store into targeted map
    count[choose] = 0;

    i = 0;
    while (i < comb.size() && ++indices[i] == comb[i].size())
      indices[i++] = 0;
  }

  return count;
}

vector<vector<int>> BETfunction::getPattern(){
  // a matrix 2^p by 2^p: signs for basic config
  size_t col = (size_t)(int)round(pow(2, (int)p));
  // matrix 2^p by 2^p: +- for each grid
  vector<vector<int>> pat(col, vector<int>(col));
  // get indicators of BID 00 or 01
  vector<vector<size_t>> idx = allComb(p, 1);
  // fit all 2^p (d=1) grid with 2^p
  // all grid:
  map<vector<int>, int> posi = allPosition(1);

  // 2 BIDs for d=1:
  vector<int> bid1 = {0, 1};
  for(size_t i=0; i<idx.size(); i++){
    size_t nposi = 0;
    for (map<vector<int>,int>::iterator it=posi.begin(); it!=posi.end(); ++it){
      // each grid:
      int sign = 1;
      for(size_t j=0; j<p; j++){
        //				sign = ~(sign ^ locate(it->first[j]+1, bid1[idx[i][j]], 1)) & 1;
        if((it->first[j] == 1) && (idx[i][j] == 1)){
          // sign of j-dim with i-bid : 1
          sign = (~(sign ^ 1)) & 1;
        }else if((it->first[j] == 0) && (idx[i][j] == 1)){
          // 0
          sign = (~(sign ^ 0)) & 1;
        }
        			// cout << idx[i][j] << " ";
      }
      		// cout << ": " << sign << endl;

      pat[i][nposi] = 2*sign-1;
      nposi++;
    }

  }

  return pat;
}

string BETfunction::convert(size_t idx){
  string res(p*d + p-1, '0');

  long a = (long)idx;
  int base = (int)round(pow(2, (int)p));

  int b[1000] = {0};
  int num = 0;
  int i = 0;
  while (a > 0)
  {
    num = a % base;
    a /= base;
    b[i] = num;
    int num0 = 0;
    int j = p*d +p - 1 - i - 1;
    while (num > 0)
    {
      num0 = num % 2;
      num /= 2;
      res.replace(j, 1, to_string(num0));

      j = j - d - 1;
    }

    ++i;
  }
  for(size_t i = d; i < p*d + p - 1; i = i+d+1){
    res.replace(i, 1, "-");
  }


  return res;
}

vector<string> BETfunction::idxFilter(){
  size_t total = (size_t)round(pow(2, (int)p*d));
  vector<string> res(total);
  for(size_t idx = 0; idx < total; idx++){

    long a = (long)idx;
    int base = (int)round(pow(2, (int)p));

    vector<int> b(d);
    int num = 0;
    int i = 0;
    while (a > 0)
    {
      num = a % base;
      a /= base;
      b[i] = num;
      ++i;
    }

    bool isidx = 1;
    //see whether in indep index
    if(!testUnif){
      for(size_t i = 0; i < indepIndex.size(); i++){
        bool nonZero = 0;
        // whether indep[i][j]-1 -th digit i=0
        for(size_t j = 0; j < indepIndex[i].size(); j++){
          for(auto& num: b){
            nonZero = nonZero || (num >> (p - indepIndex[i][j])&1);
          }

        }
        isidx = isidx && nonZero;

      }
    }


    if(isidx){
      string inter(p*d + p-1, '0');
      for(size_t i = 0; i < d; ++i){
        int num0 = 0;
        int j = p*d +p - 1 - i - 1;
        while (b[i] > 0)
        {
          num0 = b[i] % 2;
          b[i] /= 2;
          inter.replace(j, 1, to_string(num0));

          j = j - d - 1;
        }
      }


      for(size_t i = d; i < p*d + p - 1; i = i+d+1){
        inter.replace(i, 1, "-");
      }
      res[idx] = inter;

    }


  }

  return res;
}


vector<int> BETfunction::iterBET(map<vector<int>, int>& posi){
  //posi: original grid

  size_t col = (size_t)(int)round(pow(2, (int)p));


  // layer0: first layer with d-1
  vector<vector<size_t>> layer0 = allComb(p, d-1);
  // for all combinations (0,0) -> (2^d-1, 2^d-1) (p-dim): toner of +0 or +1
  vector<vector<size_t>> toner0 = allComb(p, 1);

  // store first operation results: key all comb of p*2^d-1; value 2^p pattern results
  //	map<vector<int>, vector<int>> layer1;
  // store separately: map for (pair) -> index; vector of coefficients
  map<vector<size_t>, size_t> layer1;
  size_t coeff_r = layer0.size();
  vector<vector<int>> aCoeff(coeff_r, vector<int>(col));

#ifdef _OPENMP
  #pragma omp parallel for if(coeff_r > 1)
#endif
  for(size_t idx = 0; idx < coeff_r; idx++){
    // k: 2^p pattern

    for(size_t k = 0; k < col; k++){
      //get one vector xij: 2^p (times with pattern matrix)
      int sum = 0;
      // i: each row 2^p entry
      for(size_t i = 0; i < col; i++){
        vector<int> grid(p);
        for(size_t j = 0; j < p; j++){
          grid[j] = 2*layer0[idx][j] + toner0[i][j] + 1;
        }
        // get large grid with 2^p grid
        if(posi.count(grid) > 0){
          sum += pat[k][i] * posi[grid];
        }
      }
      aCoeff[idx][k] = sum;
    }
#ifdef _OPENMP
  #pragma omp critical
#endif
    {layer1[layer0[idx]] = idx;}
  }


  int d0 = d-1;
  // store next operation results: key all comb of p*2^d0-1; value 2^p pattern results
  while (d0 > 0){
    map<vector<size_t>, size_t> layer2;
    d0 = d0 - 1;

    // layer0: k-th layer with d-k, same toner
    layer0 = allComb(p, d0);
    size_t coeff_r = layer0.size();
    // store coefficients
    size_t coeff_c2 = (size_t)(int)round(pow(2, (int)p*(d - d0)));
    vector<vector<int>> bCoeff(coeff_r, vector<int>(coeff_c2) );

    // 2^(d-d0) coefficients for now
    size_t coeff_c = (size_t)(int)round(pow(2, (int)p*(d - d0 - 1)));

#ifdef _OPENMP
#pragma omp parallel for if(coeff_r > 1)
#endif
    for(size_t idx = 0; idx < coeff_r; idx++){
#ifdef _OPENMP
      #pragma omp parallel for if(coeff_r == 1)
#endif
      for(size_t k = 0; k < col; k++){
        //get one vector xij: 2^p (times with pattern matrix)
        int sum;
        for(size_t ci = 0; ci < coeff_c; ci++){
          sum = 0;
          size_t f = k*coeff_c + ci;
          if(d0 == 0){
            if(binary_inter[f] != ""){
              for(size_t i = 0; i < col; i++){

                vector<size_t> grid(p);
                for(size_t j = 0; j < p; j++){
                  grid[j] = 2*layer0[idx][j] + toner0[i][j];

                  //							cout << " idx: " << grid[j] << " ";
                }

                // go over all coeff
                // get large grid with 2^p grid
                if(layer1.count(grid) > 0){
                  sum += pat[k][i] * aCoeff[layer1[grid]][ci];
                }
              }

      #ifdef _OPENMP
      #pragma omp critical
      #endif
          {bCoeff[idx][f] = sum;}
            }
          }else{
            for(size_t i = 0; i < col; i++){

              vector<size_t> grid(p);
              for(size_t j = 0; j < p; j++){
                grid[j] = 2*layer0[idx][j] + toner0[i][j];
              }

              // go over all coeff
              // get large grid with 2^p grid
              if(layer1.count(grid) > 0){
                sum += pat[k][i] * aCoeff[layer1[grid]][ci];
              }
            }
      #ifdef _OPENMP
      #pragma omp critical
      #endif
      {bCoeff[idx][f] = sum;}
          }


        }

      }
#ifdef _OPENMP
    #pragma omp critical
#endif
      {layer2[layer0[idx]] = idx;}
    }

    //refresh layer1 & 2; refresh a/bCoeff
    layer1 = layer2;
    aCoeff = bCoeff;

  }

  // find the extreme stat
  aCoeff[0][0] = 0;
  vector<int>::iterator findMax = max_element(aCoeff[0].begin(), aCoeff[0].end(), abs_compare);
  size_t max_idx = distance(aCoeff[0].begin(), findMax);

  // max interaction index
  // vector<int> max_interaction = inter_idx[max_idx];
  string max_interaction = convert(max_idx);

  // count most frequent max interaction
  countInteraction[max_interaction]++;

  return aCoeff[0];
}




// ----------------end of iterBET----------------------------------------

vector<size_t> BETfunction::unreplaceShuffle(size_t size, size_t max_size)
{
  assert(size <= max_size);
  vector<size_t> b(size);
  // vector<size_t> t(max_size);
  IntegerVector t(max_size);
  IntegerVector x(max_size);

  for(size_t i = 0; i < max_size; i++){
    x[i] = i;
  }

  // shuffle 0~max_size
  // std::random_device rd;
  // std::mt19937 gen(rd());
  // shuffle(t.begin(), t.end(), gen);
  // shuffle(t.begin(), t.end(), randWrapper);
  t = sample(x, x.size());

  // get first size entry
  for(size_t i = 0; i < size; i++){
    b[i] = t[i];
  }

  return b;
}

vector<double> BETfunction::subsample(size_t m, size_t B)
{
  size_t total = (int)round(pow(2, p*d));
  vector<double> res(total);

  // bool debug = 0;

  // subsample B times
  // cout << ifIter << endl;
  for(size_t b = 0; b < B; b++){
    vector<size_t> idx = unreplaceShuffle(m, n);

    // if(debug){
      // for(size_t i = 0; i < m; i++){
      //   cout << idx[i] << " ";
      // }
      // cout << " The " << b+1 << " time index " << endl;
    // }

    //	new data
    vector<vector<double> > X_m(m, vector<double>(p));
    for(size_t i = 0; i < m; i++){
      X_m[i] = X[idx[i]];
    }

    vector<vector<int>> c = ecdf_loc(X_m);

    // create a map that count for observations in the same location
    map<vector<int>, int> mapC = groupC(c);
    size_t numCount = mapC.size();
    vector<int> S;
    
    // consist symm and beast
    if(b == 0 && ifIter && (numCount < total*2/3)){
      // if iter for symm but not iter for now
      ifIter = 0;
      // regenerate symm with notiter
      vector<int> countValues;
      for (map<vector<int>, int>::iterator it=countGrid.begin(); it!=countGrid.end(); ++it){
        countValues.push_back(it->second);
      }

      // 3-dimension matrix: Xij ~ BIDs
      vector<vector<vector<int>>> matrix = CBIDs(countGrid);

      // binary expansion on X
      out_symmstats = symmstats(countValues, matrix, n);
    }else if(b == 0 && !ifIter && (numCount >= total*2/3) && p*d >= 12){
      // if notiter for symm but iter for now
      ifIter = 1;
      // iter for symm
      out_symmstats = iterBET(countGrid);
    }
    
    if(!ifIter){
      // number of each location
      vector<int> countValues;
      for (map<vector<int>, int>::iterator it=mapC.begin(); it!=mapC.end(); ++it){
        countValues.push_back(it->second);
      }

      // 3-dimension matrix: Xij ~ BIDs
      vector<vector<vector<int>>> matrix = CBIDs(mapC);

      // binary expansion on X_m
      S = symmstats(countValues, matrix, m);

    }else{
      S = iterBET(mapC);

    }

    // if(debug){
      // for(size_t i = 0; i < S.size(); i++){
      //   cout << S[i] << " ";
      // }
      // cout << " The " << b+1 << " time sample " << endl;
    // }


    // sum S1 to SB
    for(size_t i = 0; i < total; i++){
      res[i] += (double)S[i]/(double)m;
    }
  }
  // cout << ifIter << endl;

  // take mean
  for(size_t i = 0; i < total; i++){
    res[i] = res[i]/(double)B;
  }

  return res;
}

vector<double> BETfunction::softthreshold(vector<double>& S, double lambda)
{
  size_t length = S.size();
  vector<double> res(length);
  for(size_t i = 0; i < length; i++){
    if(S[i] >= 0){
      if((double)S[i] - lambda > 0){
        res[i] = (double)S[i] - lambda;
      }
    }else{
      if((double)S[i] + lambda < 0){
        res[i] = (double)S[i] + lambda;
      }
    }
  }

  return res;
}

double BETfunction::logHypergeometricProb(double* logFacs, int a, int b, int c, int d)
{
  return logFacs[a+b] + logFacs[c+d] + logFacs[a+c] + logFacs[b+d] - logFacs[a] - logFacs[b] - logFacs[c] - logFacs[d] - logFacs[a+b+c+d];
}

double BETfunction::fisherExact(int a, int b, int c, int d)
{
  int n = a + b + c + d;
  //	vector<double> logFacs(n+1);
  double* logFacs = new double[n+1] ();

  for(int i=1; i < n+1; ++i) {
    logFacs[i] = logFacs[i-1] + log((double)i); //store factorization
  }
  double logpCutoff = logHypergeometricProb(logFacs,a,b,c,d); // *** logFacs added
  double pFraction = 0;
  for(int x=0; x <= n; ++x) {
    if ( a+b-x >= 0 && a+c-x >= 0 && d-a+x >=0 ) {
      double l = logHypergeometricProb(logFacs, x,a+b-x,a+c-x,d-a+x);
      if ( l <= logpCutoff )
        pFraction += exp(l - logpCutoff);
    }
  }
  double logpValue = logpCutoff + log(pFraction);
  double hp = exp(logpCutoff)/2;
  delete [] logFacs;
  return exp(logpValue) - hp;
}

double BETfunction::binomial(int n, int k, double p)
{
  // binomial distribution with dp
  // store previous results
  double **ret = new double*[n + 1];
  for (int i = 0; i < n + 1; ++i)
    ret[i] = new double[k + 1] ();

  ret[0][0] = 1.0;
  for (int i = 1; i < n + 1; ++i)
    ret[i][0] = (1.0 - p) * ret[i - 1][0];
  for (int j = 1; j < k + 1; ++j)
    ret[0][j] = 0.0;
  for (int i = 1; i < n + 1; ++i){
    for (int j = 1; j < k + 1; ++j){
      ret[i][j] = (1.0 - p) * ret[i - 1][j] + p * ret[i - 1][j - 1];
    }
  }

  double res = ret[n][k];
  
  for (int i = 0; i < n + 1; ++i)
    delete [] ret[i];
  delete [] ret;

  return res;
}

double BETfunction::pbinom(int n, int k, double p)
{
  // cumulative: P(Binom(n, p) >= k)
  if (n < 0 || k < 0)
    return 0;
  // mid p value correction
  double pb = binomial(n, k, p) / 2;
  for (int i = k + 1; i <= n; i++){
    pb += binomial(n, i, p);
  }
  return pb;
}

int BETfunction::getStats()
{
  return Stats;
}

double BETfunction::getPvalue()
{
  return pvalue;
}

string BETfunction::getInteraction()
{
  return interaction_str;
}

vector<int> BETfunction::getSymmStats()
{
  return out_symmstats;
}

map<string, int, std::greater<std::string>> BETfunction::getGrids(){
  map<string, int, std::greater<std::string>> res;
  map<vector<int>, int> posi = allPosition(d);
  for(auto& c: cell){
    vector<int> c1(p);
    for(size_t i = 0; i < p; i++){
      c1[i] = c[i] - 1;
    }
    posi[c1]++;
  }
  for (map<vector<int>, int>::iterator it=posi.begin(); it!=posi.end(); ++it){
    string g = "";
    for(size_t i = 0; i < it->first.size(); i++){
      bitset<6> b(it->first[i]);
      g += b.to_string().substr(6-d, d);
    }
    res[g] = it->second;
  }

  return res;
}

vector<vector<int>> BETfunction::getAllColor()
{
  // empirical cdf
  vector<vector<int>> c = ecdf_loc(X);
  return allColor(c);
}

vector<vector<string>> BETfunction::getSymmInteraction()
{
  return out_symminter;
}

vector<string> BETfunction::getBinary()
{
  return binary_inter;
}

double BETfunction::getBeastStat()
{
  return BeastStat;
}

// vector<vector<int>> BETfunction::getBeastInteraction()
// {
//   return BeastInter;
// }

string BETfunction::getBeastInteraction()
{
  return freqInter;
}


void BETfunction::Beast(size_t m, size_t B, double lambda, bool test_uniformity, bool test_independence, vector<vector<size_t>>& independence_index)
{
  // bool debug = 0;

  testUnif = test_uniformity;
  testIndep = test_independence;

  if(testIndep){
    indepIndex = independence_index;
  }else{
    indepIndex = {{}};
    for(size_t i = 0; i < p; i++){
      indepIndex[0].push_back(i);
    }
  }

  // first subsample
  vector<double> S1 = subsample(m, B);

  // if(debug){
    // for(size_t i = 0; i < S1.size(); i++){
    //   cout << S1[i] << " ";
    // }
    // cout << "  S1" <<endl;
  // }

  // soft-thresholding lambda = sqrt(2log(2^pd)/n)
  //		lambda = sqrt(2 * log(pow(2, p*d)) / n) / 2;

  //soft-threshold
  vector<double> T1 = softthreshold(S1, lambda);
  // if(debug){
    // for(size_t i = 0; i < S1.size(); i++){
    //   cout << T1[i] << " ";
    // }
    // cout << "  T1" <<endl;
  // }

  vector<double> symm_double(out_symmstats.size());
  // (out_symmstats.size())
  // for(size_t i = 0; i < S1.size(); i++){
  //   cout << out_symmstats[i] << " ";
  // }
  // cout << "  symm" <<endl;
  
  for(size_t i = 0; i < out_symmstats.size(); i++){
    symm_double[i] = (double)out_symmstats[i] / (double)n;
  }
  // copy(out_symmstats.begin(), out_symmstatss.end(), back_inserter(symm_double));
  // for(size_t i = 0; i < S1.size(); i++){
  //   cout << symm_double[i] << " ";
  // }
  // cout << "  S" <<endl;


  vector<double> T = softthreshold(symm_double, lambda);
  // if(debug){
    // for(size_t i = 0; i < S1.size(); i++){
    //   cout << T[i] << " ";
    // }
    // cout << "  T" <<endl;
  // }

  double T_module = 0;
  for(size_t i = 0; i < T.size(); i++){
    T_module += T[i] * T[i];
  }

  for(size_t i = 0; i < S1.size(); i++){
    BeastStat += T1[i]*T[i];
  }

  BeastStat = BeastStat/sqrt(T_module);

  using pair_type = decltype(countInteraction)::value_type;
  auto mostFreq = max_element(countInteraction.begin(), countInteraction.end(), [] (const pair_type & p1, const pair_type & p2) {
    return p1.second < p2.second;
  });



  // if(debug){
  //   cout <<"interaction : " << mostFreq->first << endl;
  // }

  freqInter = mostFreq->first;

  // for(size_t i = 0; i < p; i++){
  //   BeastInter.push_back(inter_mat[freqInter[i]]);
  // }

}


BETfunction:: BETfunction(vector<vector<double>>& X_R, int depth, bool unif, bool asymptotic, bool test_uniformity, bool test_independence, vector<vector<size_t>>& independence_index)
{
	unifMargin = unif;
  asympt = asymptotic;
  X = X_R;
	d = depth;
	n = X.size();
	p = X[0].size();

	testUnif = test_uniformity;
	testIndep = test_independence;

	if(testIndep){
	  indepIndex = independence_index;
	}else{
	  indepIndex = {{}};
	  for(size_t i = 0; i < p; i++){
	    indepIndex[0].push_back(i);
	  }
	}

	int bonf = 1;
	for (size_t i = 0; i < indepIndex.size(); i++){
	  int l = (int)round(pow(2, (int)indepIndex[i].size()*d)) - 1;
	  bonf = bonf * l;
	}

	bid = BIDs();

	inter = interaction_index();
	inter_mat = interaction_mat();

	// all variables go over all bids:
	allidx = allComb(p, d);
	size_t total = (int)round(pow(2, p*d));


	// store symmetry statistics
	out_symmstats.resize(total, 0);

	// store interactions
	vector<string> temp(total);
	out_symminter.resize(p, temp);

	// store binary interactions
	binary_inter.resize(total);

	// start multiprocessing
#ifdef _OPENMP
	if(p == 2){
	  numThread = 4;
	}else if(p >= 3){
	  numThread = omp_get_num_procs();
	}
#endif

	// loops: 2^pd
	// steps of multi-processing
	size_t remainder = total % numThread;
	size_t step = total / numThread;

	// # of assignments each thread
	vector<size_t> interval(numThread, step);
	for (size_t i = 0; i < remainder; i++){
	  interval[i] += 1;
	}

	thread.resize(numThread + 1);
	for (size_t i = 1; i < numThread + 1; i++){
	  thread[i] = thread[i - 1] + interval[i - 1];
	}


	// empirical cdf
	cell = ecdf_loc(X);

	// create a map that count for observations in the same location
	countGrid = groupC(cell);
	size_t numCount = countGrid.size();





	// -----------------decide iterBET or old BET------------------------

	int Sa = 0, Sb = 0;
	// numCount < total*2/3 || d==1
	if(numCount >= total*2/3 && (p*d) >= 12){
	  ifIter = 1;
	}

  if(!ifIter){
    // number of each location
    vector<int> countValues;
    for (map<vector<int>, int>::iterator it=countGrid.begin(); it!=countGrid.end(); ++it){
      countValues.push_back(it->second);
    }

    // 3-dimension matrix: Xij ~ BIDs
    vector<vector<vector<int>>> matrix = CBIDs(countGrid);

    // binary expansion on X
    out_symmstats = symmstats(countValues, matrix, n);
    // find the extreme stat
    vector<int>::iterator findMax = max_element(out_symmstats.begin(), out_symmstats.end(), abs_compare);
    size_t max_idx = distance(out_symmstats.begin(), findMax);

    // maxStat
    Stats = out_symmstats[max_idx];
    // max interaction: interaction_str
    interaction_str = binary_inter[max_idx];

    // store marginal
    for (size_t i = 0; i < p; i++){
      vector<string>::iterator ans = find(inter.begin(), inter.end(), out_symminter[i][max_idx]);
      int index = distance(inter.begin(), ans);
      if (p == 2 && i == 0){
        // count marginal
        for (size_t j = 0; j < numCount; j++){
          Sa += matrix[i][index][j] * countValues[j];
        }
      }
      if (p == 2 && i == 1){
        // count marginal
        for (size_t j = 0; j < numCount; j++){
          Sb += matrix[i][index][j] * countValues[j];
        }
      }
    }

  }else{
    // get all interaction
    binary_inter = idxFilter();
    // pattern for every 2^p by 2^p grid
    pat = getPattern();

    out_symmstats = iterBET(countGrid);
    


    // find the extreme stat
    vector<int>::iterator findMax = max_element(out_symmstats.begin(), out_symmstats.end(), abs_compare);
    size_t max_idx = distance(out_symmstats.begin(), findMax);

    // maxStat
    Stats = out_symmstats[max_idx];
    // max interaction
    interaction_str = binary_inter[max_idx];

    // store marginal
    long a = (long)max_idx;
    int base = (int)round(pow(2, (int)p));

    int num = 0;
    int i = 0;
    while (a > 0)
    {
      num = a % base;
      a /= base;
      Sa += ((num>>1)<<1) * (int)round(pow(4, i));
      Sb += (((num<<1)&3)>>1) * (int)round(pow(4, i));

      ++i;
    }

    Sa = (out_symmstats[Sa] + (int)n)/2;
    Sb = (out_symmstats[Sb] + (int)n)/2;

  }

	if (p > 2 || asympt){
	  // asymptotic p value: normal N (0, n)
	  double z = abs(Stats)/sqrt((double)n);
	  if (p == 1){
	    pvalue = (1-0.5 * erfc(-z * sqrt(0.5))) * ((int)round(pow(2, (int)d)) - 1) * 2;
	  }else{
	    // pvalue = (1-0.5 * erfc(-z * sqrt(0.5))) * ((int)round(pow(2, (int)p*d)) - (int)p*((int)round(pow(2, d)) - 1) - 1) * 2;
	    pvalue = (1-0.5 * erfc(-z * sqrt(0.5))) * bonf * 2;
	    // pvalue = (1-0.5 * erfc(-z * sqrt(0.5))) * out * 2;
	  }
	}else if (unifMargin){
	  // p value: if unif >> binomial
	  // bonferroni
	  // pvalue = ((int)round(pow(2, (int)p*d)) - (int)p*((int)round(pow(2, d)) - 1) - 1) * pbinom(n, (abs(Stats) + n) / 2, 0.5);
	  pvalue = bonf * pbinom(n, (abs(Stats) + n) / 2, 0.5);
	}else if (p == 1){
	  // p value: p = 1 >> binomial
	  // bonferroni
	    pvalue = ((int)round(pow(2, (int)d)) - 1) * pbinom(n, (abs(Stats) + n) / 2, 0.5);
	}else if (p == 2){
	  // p = 2: fisher exact
	  int n11 = ((n + Stats) / 2 + Sb - (n - Sa))/2;
	  int n00 = (n + Stats) / 2 - n11;
	  int n10 = Sa - n11;
	  int n01 = Sb - n11;
	  // pvalue = ((int)round(pow(2, (int)p*d)) - (int)p*((int)round(pow(2, d)) - 1) - 1) * fisherExact(n11, n01, n10, n00);
	  pvalue = bonf * fisherExact(n11, n01, n10, n00);
	}

	if (pvalue > 1){
	  pvalue = 1;
	}
	if (pvalue < 0){
	  pvalue = 0;
	}

}

vector<vector<double>> imp(NumericMatrix& X)
{
  int r = X.nrow(), c = X.ncol();
  vector<vector<double>> v(r, vector<double>(c));
  for (int i = 0; i < r; i++){
    for (int j = 0; j < c; j++)
      v[i][j] = X(i, j);
  }
  return v;
}

//[[Rcpp::export]]
List symmCpp(NumericMatrix& X_R, int d, bool unif)
{
  vector<vector<double>> X = imp(X_R);

  vector<vector<size_t>> idx = {{}};;
  BETfunction bet(X, d, unif, 1, 1, 0, idx);

  // size_t length = (int)round(pow(2, (int)p*d));

  DataFrame symm = DataFrame::create(
    Named("Statistics") = bet.getSymmStats()
  );

  // for (size_t i = p; i > 1; i--){
  //   vector<string>::const_iterator first = bet.getSymmInteraction().end() - length*(p-i+1);
  //   vector<string>::const_iterator last = bet.getSymmInteraction().end() - length*(p-i);
  //   vector<string> temp(first, last);
  //   symm.push_front(temp, "X" + to_string(i));
  // }
  //
  // vector<string> temp1(length);
  // for (size_t i = 0; i < length; i++){
  //   temp1[i] = bet.getSymmInteraction()[i];
  // }
  // symm.push_front(temp1, "X1");

  // for (size_t i = p; i > 0; i--){
  //   symm.push_front(bet.getSymmInteraction()[i-1], "X" + to_string(i));
  // }

  symm.push_front(bet.getBinary(), "BinaryIndex");

  // List L = List::create(Named("SymmetryStatistics") = DataFrame(symm));

  return DataFrame(symm);

}

//[[Rcpp::export]]
DataFrame colorCpp(NumericMatrix& X_R, int d, bool unif)
{
  vector<vector<double>> X = imp(X_R);
  vector<vector<size_t>> idx = {{}};
  BETfunction bet(X, d, unif, 1, 1, 0, idx);
  vector<vector<int>> c = bet.getAllColor();
  DataFrame df = DataFrame(c);
  df.names() = bet.getBinary();

  return df;
}

//[[Rcpp::export]]
DataFrame cellCpp(NumericMatrix& X_R, int d, bool unif)
{
  vector<vector<double>> X = imp(X_R);
  vector<vector<size_t>> idx = {{}};
  BETfunction bet(X, d, unif, 1, 1, 0, idx);
  map<string, int, std::greater<std::string>> cell = bet.getGrids();
  vector<string> p(cell.size());
  vector<int> v(cell.size());
  size_t i = 0;
  for(map<string, int>::iterator it=cell.begin(); it!=cell.end(); ++it, i++){
    p[i] = it->first;
    v[i] = it->second;
  }
  DataFrame df = DataFrame::create( Named("Cell") = p , Named("Count") = v );

  return df;
}

//[[Rcpp::export]]
List BETCpp(NumericMatrix& X_R, int d, bool unif, bool asymptotic, bool test_uniformity, bool test_independence, List& independence_index)
{
  vector<vector<double>> X = imp(X_R);
  vector<vector<size_t>> idx;
  if(test_independence){
    for(auto& v: independence_index){
      idx.push_back(v);
    }
  }else{
    idx = {{}};
  }

  BETfunction bet(X, d, unif, asymptotic, test_uniformity, test_independence, idx);
  size_t n = X.size();
  double zstats = abs(bet.getStats())/sqrt(n);

  DataFrame df = DataFrame::create();

  // size_t p = X[0].size();
  // for (size_t i = p; i > 0; i--){
  //   // CharacterVector vi = {bet.getInteraction()[i-1]};
  //   df.push_front(bet.getInteraction()[i-1], "X" + to_string(i));
  // }

  List L = List::create(Named("Interaction") = bet.getInteraction(), Named("Extreme.Asymmetry") = bet.getStats(), Named("p.value.bonf") = bet.getPvalue(), Named("z.statistic") = zstats);

  return L;

}

//[[Rcpp::export]]
List BeastCpp(NumericMatrix& X_R, int d, size_t m, size_t B, bool unif, double lambda, bool test_uniformity, bool test_independence, List& independence_index, String method, int numPerm)
{
  vector<vector<double>> X = imp(X_R);
  // independence index
  vector<vector<size_t>> idx;
  if(test_independence){
    for(auto& v: independence_index){
      idx.push_back(v);
    }
  }else{
    idx = {{}};
  }

  size_t n = X.size();
  size_t p = X[0].size();

  BETfunction bet(X, d, unif, 1, test_uniformity, test_independence, idx);

  bet.Beast(m, B, lambda, test_uniformity, test_independence, idx);

  double beastStat = bet.getBeastStat();

  //null distribution simulation
  vector<double> nullD(numPerm);

  vector<vector<double>> indp;
  NumericMatrix indp_R(n, p);

  // unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  // std::default_random_engine gen(seed);
  // std::normal_distribution<double> dis(0,1);

  IntegerVector x(n);
  IntegerVector t(n);
  for(size_t i = 0; i < n; i++){
    x[i] = i;
  }

  double pv;

  if(method == "NA"){
    List L = List::create(Named("Interaction") = bet.getBeastInteraction(), Named("BEAST.Statistic") = beastStat);
    return L;
  }else{
    // 1-dim: only simulation
    if(p == 1){
      method = "s";
    }

    if(method == "p"){
      // permutation
      vector<vector<double>> newdata = X;
      for(int isample = 0; isample < numPerm; isample++){
        // uniformity:
        if(test_uniformity){
          for(size_t col = 1; col < p; col++){

            // shuffle 0~n
            // random_device rd;
            // mt19937 gen(rd());
            // shuffle(t.begin(), t.end(), gen);
            // shuffle(t.begin(), t.end(), randWrapper);
            t = sample(x, x.size());

            for(size_t i = 0; i < n; i++){
              newdata[i][col] = X[t[i]][col];
            }
          }
          BETfunction bet0(newdata, d, unif, 1, test_uniformity, test_independence, idx);
          bet0.Beast(m, B, lambda, test_uniformity, test_independence, idx);
          nullD[isample] = bet0.getBeastStat();
        }else{
          // independence:
          for(size_t g = 1; g < idx.size(); g++){

            // shuffle 0~n
            // random_device rd;
            // mt19937 gen(rd());
            // shuffle(t.begin(), t.end(), gen);
            // shuffle(t.begin(), t.end(), randWrapper);
            t = sample(x, x.size());

            for(size_t v = 0; v < idx[g].size(); v++){
              for(size_t i = 0; i < n; i++){
                newdata[i][idx[g][v]-1] = X[t[i]][idx[g][v]-1];
              }
            }
          }
        }
        // null distribution:
        BETfunction bet0(newdata, d, unif, 1, test_uniformity, test_independence, idx);
        bet0.Beast(m, B, lambda, test_uniformity, test_independence, idx);
        nullD[isample] = bet0.getBeastStat();
      }
    }else if(method == "s"){
      // simulation
      for(int sample = 0; sample < numPerm; sample++){
        // for(size_t j = 0; j < p; j++){
        //   for(size_t i = 0; i < n; i++){
        //     indp[i][j] = dis(gen);
        //   }
        // }
        if(p == 1){
          // p=1: uniform 0, 1
          indp_R(_, 0) = runif(n);
        }else{
          for(size_t j = 0; j < p; j++){
            indp_R(_, j) = runif(n);
          }
        }
        indp = imp(indp_R);

        BETfunction bet0(indp, d, unif, 1, test_uniformity, test_independence, idx);
        bet0.Beast(m, B, lambda, test_uniformity, test_independence, idx);
        nullD[sample] = bet0.getBeastStat();
      }
    }

    pv = (double)count_if(nullD.begin(), nullD.end(), [&beastStat](double x) { return (x >= beastStat); }) / (double)numPerm;

    List L = List::create(Named("Interaction") = bet.getBeastInteraction(), Named("BEAST.Statistic") = beastStat, Named("Null.Distribution") = nullD, Named("p.value") = pv);

    return L;
  }


}

// //[[Rcpp::export]]
// NumericVector nullCpp(size_t n, size_t p, int d, size_t m, size_t B, double lambda, bool test_uniformity, bool test_independence, List& independence_index, String method, int numPerm)
// {
//   // independence index
//   vector<vector<size_t>> idx;
//   if(test_independence){
//     for(auto& v: independence_index){
//       idx.push_back(v);
//     }
//   }else{
//     idx = {{}};
//   }
// 
//   //null distribution simulation
//   NumericVector nullD (numPerm);
// 
//   vector<vector<double>> indp;
//   NumericMatrix indp_R(n, p);
// 
//   IntegerVector t(n);
//   IntegerVector x(n);
//   for(size_t i = 0; i < n; i++){
//     x[i] = i;
//   }
// 
//   // 1-dim: only simulation
//   if(p == 1){
//     method = "s";
//   }
// 
//   if(method == "p"){
//     // permutation
//     for(size_t j = 0; j < p; j++){
//       indp_R(_, j) = runif(n);
//     }
//     indp  = imp(indp_R);
//     vector<vector<double>> newdata = indp;
//     for(int isample = 0; isample < numPerm; isample++){
//       // uniformity:
//       if(test_uniformity){
//         for(size_t col = 1; col < p; col++){
// 
//           // shuffle 0~n
//           // random_device rd;
//           // mt19937 gen(rd());
//           // shuffle(t.begin(), t.end(), gen);
//           // shuffle(t.begin(), t.end(), randWrapper);
//           t = sample(x, x.size());
// 
//           for(size_t i = 0; i < n; i++){
//             newdata[i][col] = indp[t[i]][col];
//           }
//         }
//         BETfunction bet0(newdata, d, 1, 1, test_uniformity, test_independence, idx);
//         bet0.Beast(m, B, lambda, test_uniformity, test_independence, idx);
//         nullD[isample] = bet0.getBeastStat();
//       }else{
//         // independence:
//         for(size_t g = 1; g < idx.size(); g++){
// 
//           // shuffle 0~n
//           // random_device rd;
//           // mt19937 gen(rd());
//           // shuffle(t.begin(), t.end(), gen);
//           // shuffle(t.begin(), t.end(), randWrapper);
//           t = sample(x, x.size());
// 
//           for(size_t v = 0; v < idx[g].size(); v++){
//             for(size_t i = 0; i < n; i++){
//               newdata[i][idx[g][v]-1] = indp[t[i]][idx[g][v]-1];
//             }
//           }
//         }
//       }
//       // null distribution:
//       BETfunction bet0(newdata, d, 0, 1, test_uniformity, test_independence, idx);
//       bet0.Beast(m, B, lambda, test_uniformity, test_independence, idx);
//       nullD[isample] = bet0.getBeastStat();
//     }
//   }else if(method == "s"){
//     // simulation
//     for(int sample = 0; sample < numPerm; sample++){
//       // for(size_t j = 0; j < p; j++){
//       //   for(size_t i = 0; i < n; i++){
//       //     indp[i][j] = dis(gen);
//       //   }
//       // }
//       if(p == 1){
//         // p=1: uniform 0, 1
//         indp_R(_, 0) = runif(n);
//       }else{
//         for(size_t j = 0; j < p; j++){
//           indp_R(_, j) = runif(n);
//         }
//       }
// 
//       bool unif;
//       if(test_uniformity){
//         unif = 1;
//       }else{
//         unif = 0;
//       }
// 
//       indp = imp(indp_R);
//       BETfunction bet0(indp, d, unif, 1, test_uniformity, test_independence, idx);
//       bet0.Beast(m, B, lambda, test_uniformity, test_independence, idx);
//       nullD[sample] = bet0.getBeastStat();
//     }
//   }
// 
//   return nullD;
// 
// }

