//  ===============================================================================
//
//  Developers:
//
//  Tiago de Conto - tdc.florestal@gmail.com -  https://github.com/tiagodc/
//
//  COPYRIGHT: Tiago de Conto, 2020
//
//  This piece of software is open and free to use, redistribution and modifications
//  should be done in accordance to the GNU General Public License >= 3
//
//  Use this software as you wish, but no warranty is provided whatsoever. For any
//  comments or questions on TreeLS, please contact the developer (prefereably through my github account)
//
//  If publishing any work/study/research that used the tools in TreeLS,
//  please don't forget to cite the proper sources!
//
//  Enjoy!
//
//  ===============================================================================

#include <Rcpp.h>
#include "methods.h"
#include "utils.h"
#include "classes.h"
#include "utils.h"

using namespace Rcpp;

// conversions
vector<vector<double> > rmatrix2cpp(NumericMatrix& cloud){

  unsigned int ncol = cloud.ncol();
  vector<vector<double> > xyz(ncol);

  for(unsigned int i = 0; i < ncol; ++i){
    NumericMatrix::Column tempcol = cloud( _, i);
    xyz[i].insert(xyz[i].begin(), tempcol.begin(), tempcol.end());
  }

  return xyz;
}


//// split point cloud according to some criterion
vector<vector<vector<double> > > getChunks(vector<vector<double> >& cloud, vector<unsigned int>& identifier){

  vector<double>& xcol = cloud[0];
  vector<double>& ycol = cloud[1];
  vector<double>& zcol = cloud[2];

  unsigned int minIndex = *min_element(identifier.begin(), identifier.end());
  unsigned int maxIndex = *max_element(identifier.begin(), identifier.end());
  unsigned int nlayers =  maxIndex - minIndex + 1;

  vector<vector<vector<double> > > store(nlayers, vector<vector<double> >(3));

  for(unsigned int i = 0; i < xcol.size(); ++i){

    unsigned int n = identifier[i] - minIndex;

    store[n][0].push_back(xcol[i]);
    store[n][1].push_back(ycol[i]);
    store[n][2].push_back(zcol[i]);
  }

  return store;

}
vector<vector<unsigned int> > splitVector(vector<unsigned int>& to_split, vector<unsigned int>& split_by){

  set<unsigned int> usp(split_by.begin(), split_by.end());
  vector<vector<unsigned int> > parts(usp.size());

  for(unsigned int i = 0; i < to_split.size(); ++i){
    unsigned int val = to_split[i];
    unsigned int idx = split_by[i];

    set<unsigned int>::iterator it = find(usp.begin(), usp.end(), idx);
    unsigned int dst = distance(usp.begin(), it);

    parts[dst].push_back(val);
  }

  return parts;

}

//// split point cloud into horizontal slices
vector<vector<vector<double> > > getSlices(NumericMatrix& cloud, double zmin, double zmax, double zstep){
  vector<vector<double> > las = rmatrix2cpp(cloud);
  return getSlices(las, zmin, zmax, zstep);
}

vector<vector<vector<double> > > getSlices(vector<vector<double> >& cloud, double zmin, double zmax, double zstep){

  vector<double>& xcol = cloud[0];
  vector<double>& ycol = cloud[1];
  vector<double>& zcol = cloud[2];

  unsigned int nlayers = ceil((zmax - zmin) / zstep);
  vector<vector<vector<double> > > store(nlayers, vector<vector<double> >(3));

  for(unsigned int i = 0; i < xcol.size(); ++i){

    double z = zcol[i];
    if(z < zmin || z >= zmax)
      continue;

    unsigned int n = floor((z - zmin) / zstep);

    store[n][0].push_back(xcol[i]);
    store[n][1].push_back(ycol[i]);
    store[n][2].push_back(zcol[i]);
  }

  return store;

}


// utilities
vector<double> getMinMax(vector<vector<double> >& xyz){

  vector<double> minmax(6);
  minmax[0] = minmax[1] = xyz[0][0];
  minmax[2] = minmax[3] = xyz[1][0];
  minmax[4] = minmax[5] = xyz[2][0];

  for(unsigned int i = 1; i < xyz[0].size(); ++i){

    double x = xyz[0][i];
    double y = xyz[1][i];
    double z = xyz[2][i];

    if(x < minmax[0]) minmax[0] = x; else if(x > minmax[1]) minmax[1] = x;
    if(y < minmax[2]) minmax[2] = y; else if(y > minmax[3]) minmax[3] = y;
    if(z < minmax[4]) minmax[4] = z; else if(z > minmax[5]) minmax[5] = z;
  }

  return minmax;

}
