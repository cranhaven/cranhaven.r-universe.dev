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

using namespace Rcpp;

// [[Rcpp::export]]
List houghStemPoints(NumericMatrix& las, double h1 = 1, double h2 = 3, double hstep=0.5, double radius=0.25, double pixel=0.025, double density=0.1, unsigned int votes=3){

  vector<vector<double> > cppCloud = rmatrix2cpp(las);
  vector<HoughCenters> treeEstimates = treeHough(cppCloud, h1, h2, hstep, radius, pixel, density, votes);

  if(treeEstimates.empty()){
    List noTree;
    return noTree;
  }

  tempContainer isStem(cppCloud[0].size());
  for(unsigned int i = 0; i < cppCloud[0].size(); ++i){

    double& x = cppCloud[0][i];
    double& y = cppCloud[1][i];
    double& z = cppCloud[2][i];

    if(z < 0) continue;

    unsigned int ptLayer = floor(z / hstep);

    HoughCircle* alias = &treeEstimates[ptLayer].main_circle;

    if(alias->n_votes < votes) continue;

    double dist = sqrt( pow(x - alias->x_center, 2) + pow(y - alias->y_center, 2) );

    if(dist < alias->radius + pixel*2 /* && dist > alias->radius - pixel*2 */){
      isStem.filter[i] = true;
      isStem.values[i] = alias->radius;
      isStem.counts[i] = alias->n_votes;
      isStem.sections[i] = ptLayer + 1;
    }
  }

  cppCloud.clear();
  cppCloud.shrink_to_fit();

  List output;
  output["Stem"] = isStem.filter;
  output["Segment"] = isStem.sections;
  output["Radius"] = isStem.values;
  output["Votes"] = isStem.counts;
  isStem.clear();

  return output;
}

// [[Rcpp::export]]
List houghStemPlot(NumericMatrix& las, NumericVector& ptIds, double h1 = 1, double h2 = 3, double hstep=0.5, double radius=0.25, double pixel=0.025, double density=0.1, unsigned int votes=3){

  // unordered_set<unsigned int> treeIds(pointIds.begin(), pointIds.end());
  // vector<unsigned int> treeIdsVec(treeIds.begin(), treeIds.end());

  vector<vector<double> > cloud = rmatrix2cpp(las);
  vector<unsigned int> pointIds = Rcpp::as< vector<unsigned int> >( ptIds );
  vector<vector<vector<double> > > treeList = getChunks(cloud, pointIds);
  // unordered_map<unsigned int, vector<HoughCenters> > denoisedTrees;
  vector<vector<HoughCenters>> denoisedTrees;

  for(auto& tree : treeList){

    // vector<vector<double> >& tree = treeList[i];
    if(tree[0].empty()) continue;

    vector<HoughCenters> denoised = treeHough(tree, h1, h2, hstep, radius, pixel, density, votes);

    if(denoised.empty()) continue;

    denoisedTrees.push_back(denoised);
  }

  tempContainer plotInfo( cloud[0].size() );
  for(unsigned int i = 0; i < cloud[0].size(); ++i){

    double& x = cloud[0][i];
    double& y = cloud[1][i];
    double& z = cloud[2][i];

    if(z < 0) continue;

    unsigned int ptLayer = floor(z / hstep);
    for(auto& tree : denoisedTrees){

      if(tree.size() <= ptLayer) continue;

      HoughCircle* tempCircle = &tree[ptLayer].main_circle;

      if(tempCircle->n_votes < votes) continue;

      double dist = sqrt( pow(x - tempCircle->x_center, 2) + pow(y - tempCircle->y_center, 2) );

      if(dist < tempCircle->radius + pixel*2 /* && dist > tempCircle->radius - pixel*2 */){
        plotInfo.filter[i] = true;
        plotInfo.values[i] = tempCircle->radius;
        plotInfo.counts[i] = tempCircle->n_votes;
        // plotInfo.ids[i] = tree.first;
        plotInfo.sections[i] = ptLayer + 1;
        break;
      }
    }
  }

  cloud.clear();
  cloud.shrink_to_fit();

  List output;
  output["Stem"]   = plotInfo.filter;
  // output["TreeID"] = plotInfo.ids;
  output["Segment"] = plotInfo.sections;
  output["Radius"] = plotInfo.values;
  output["Votes"]  = plotInfo.counts;
  plotInfo.clear();

  return output;

}
