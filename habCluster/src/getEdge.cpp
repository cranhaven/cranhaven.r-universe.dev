#include <Rcpp.h>
using namespace Rcpp;
using namespace std;


long getCellID(int &r, int &c, int &nrow){
  return nrow*(c)+r+1;
}

bool hasEdge(int &from, int &to, map<int,set<int>> &mp){
  map<int,set<int>>::iterator it=mp.find(from);
  if(it==mp.end()){
    return false;
  }else{
    return it->second.find(to) != it->second.end();
  }
}

// [[Rcpp::export]]
DataFrame getEdgeDF(NumericMatrix &m){
  map <int, set<int>> oldEdges;
  vector<int> fromV;
  vector<int> toV;
  vector<double>weightV;

  int nrow = m.rows();
  int ncol = m.cols();

  for(int r0 = 0; r0 < nrow; r0++){
    for(int c0 = 0; c0 < ncol; c0++){
      if(NumericVector::is_na(m(r0,c0))){
        continue;
      }

      for(int r1 = r0-1; r1<=r0+1; r1++){
        if(r1 < 0 || r1 == nrow){
          continue;
        }

        for(int c1 = c0-1; c1<c0+1; c1++){
          if(NumericVector::is_na(m(r1,c1))){
            continue;
          }
          if(c1< 0 || c1 == ncol){
            continue;
          }
          if(c0==c1&&r0==r1){
            continue;
          }

          int from = getCellID(r0,c0,nrow);
          // cout<< r0 <<","<< c0 <<": "<<from<<endl;
          int to = getCellID(r1,c1,nrow);

          // check whether the edge is duplicated as undirected;
          if(hasEdge(to,from,oldEdges)){
            continue;
          }


          double w = 0.0;

          if (r0 == r1 || c0 == c1) {
            w = 0.5 * (m(r0,c0) + m(r1,c1));
          } else{
            w =  0.35355339059 * (m(r0,c0) + m(r1,c1));
          }

          oldEdges[from].insert(to);
          fromV.push_back(from);
          toV.push_back(to);
          weightV.push_back(w);
        }

      }
    }
  }

  DataFrame df = DataFrame::create( Named("v1") = fromV,
                                    Named("v2") = toV,
                                    Named("weight") = weightV);
  return df;
}
