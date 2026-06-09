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
// convert point cloud slice to raster of point count
Raster getCounts(vector<vector<double> >& slice, double pixel_size){

  vector<double>& xcol = slice[0];
  vector<double>& ycol = slice[1];
  vector<double> minmax = getMinMax(slice);

  Raster ras;
  ras.min_x = minmax[0];
  ras.max_x = minmax[1];
  ras.min_y = minmax[2];
  ras.max_y = minmax[3];
  ras.min_z = minmax[4];
  ras.max_z = minmax[5];

  ras.pixel_size = pixel_size;

  ras.setDims();
  ras.setMatrixSize();
  ras.max_count = 0;

  for(unsigned int i = 0; i < xcol.size(); ++i){
    ras.updateMatrix(xcol[i], ycol[i]);
  }

  return ras;

}


// single tree stem points detection
vector<HoughCenters> treeHough(vector<vector<double> >& cppCloud, double h1, double h2, double hstep, double radius, double pixel, double density, unsigned int votes){

  vector<double> bbox = getMinMax(cppCloud);
  vector<vector<double> > cloudSegment = getSlices(cppCloud, h1, h2, h2-h1)[0];

  if(cloudSegment[0].empty()){
    vector<HoughCenters> noPoints;
    return noPoints;
  }

  Raster raster = getCounts(cloudSegment, pixel);

  HoughCircle circle = getSingleCenter(&raster, radius, density, votes).main_circle;

  cloudSegment.clear();
  cloudSegment.shrink_to_fit();

  Raster startLayer;
  startLayer.min_x = bbox[0];
  startLayer.max_x = bbox[1];
  startLayer.min_y = bbox[2];
  startLayer.max_y = bbox[3];
  startLayer.pixel_size = pixel;
  startLayer.max_count = 0;
  startLayer.setDims();
  startLayer.setMatrixSize();

  unsigned int nLayers = ceil(bbox[5] / hstep);
  vector<Raster> treeRasters(nLayers, startLayer);

  for(unsigned int i = 0; i < cppCloud[0].size(); ++i){

    double x = cppCloud[0][i];
    double y = cppCloud[1][i];
    double z = cppCloud[2][i];

    if(z < 0) continue;

    unsigned int ptLayer = floor(z / hstep);
    Raster* alias = &treeRasters[ptLayer];

    if(alias->max_count == 0)
      alias->min_z = alias->max_z = z;
    else if(z < alias->min_x)
      alias->min_z = z;
    else if(z > alias->max_z)
      alias->max_z = z;

    alias->updateMatrix(x,y);
  }

  vector<HoughCenters> treeEstimates( treeRasters.size() );
  for(unsigned int i = 0; i < treeRasters.size(); ++i){

    if(circle.n_votes < votes) break;

    Raster* alias = &treeRasters[i];

    alias->cleanRadius(circle.x_center, circle.y_center, circle.radius + pixel*2);

    if(alias->max_count <= votes){
      if(i == 0)
        treeEstimates[i].main_circle = circle;
      else
        treeEstimates[i] = treeEstimates[i-1];
    }else
      treeEstimates[i] = getSingleCenter(alias, circle.radius + pixel*2, density , votes);

    if(alias->min_z > h2)
      circle = treeEstimates[i].main_circle;

  }

  return treeEstimates;

}

// hough transform for a single cylinder's point cloud
HoughCenters getSingleCenter(Raster* raster, double max_radius, double min_den, unsigned int min_votes){

  //count raster properties (&raster)
  unsigned int& x_len = raster->x_dim;
  unsigned int& y_len = raster->y_dim;
  unsigned int min_count = ceil( (raster->max_count)*min_den );

  //empty raster properties (votes)
  Raster empty_raster;
  empty_raster.min_x = raster->min_x - max_radius;
  empty_raster.min_y = raster->min_y - max_radius;
  empty_raster.max_x = raster->max_x + max_radius;
  empty_raster.max_y = raster->max_y + max_radius;
  empty_raster.pixel_size = raster->pixel_size;
  empty_raster.setDims();
  empty_raster.setMatrixSize();

  //get valid pixels
  vector< array<unsigned int,2> > pixels;

  for(unsigned i = 0; i < x_len; ++i){
    for(unsigned j = 0; j < y_len; ++j){
      if(raster->matrix[i][j] >= min_count )
        pixels.push_back( {i,j} );
    }
  }

  //make circles centered in every valid pixel
  HoughCenters circle_candidates;
  circle_candidates.low_z = raster->min_z;
  circle_candidates.up_z = raster->max_z;

  for(double rad = raster->pixel_size; rad <= max_radius; rad+=raster->pixel_size){

    vector< vector<unsigned int> > votes = empty_raster.matrix;
    PixelSet pixel_set;

    for(auto& p : pixels){

      vector<double> center = raster->absCenter(p[0], p[1]);
      PixelSet h_circle = empty_raster.rasterCircle(rad, center[0], center[1]);

      for(auto& h : h_circle){
        unsigned int vx = h[0];
        unsigned int vy = h[1];

        if( vx >= votes.size() || vy >= votes[0].size() ) continue;

        votes[ vx ][ vy ]++;

        if(votes[ vx ][ vy ] >= min_votes){
          array<unsigned int, 2> vxy = {vx, vy};
          pixel_set.insert( vxy );
        }
      }
    }

    for(auto& k : pixel_set){
      unsigned int vx = k[0];
      unsigned int vy = k[1];
      vector<double> coor = empty_raster.absCenter(vx, vy);

      HoughCircle hc;
      hc.radius = rad;
      hc.x_center = coor[0];
      hc.y_center = coor[1];
      hc.n_votes = votes[vx][vy];

      circle_candidates.circles.push_back(hc);
    }
  }

  circle_candidates.getCenters();

  return circle_candidates;

}

// apply hough transform on Raster object
vector<HoughCenters> getCenters(Raster* raster, double max_radius, double min_den, unsigned int min_votes){

  //count raster properties (&raster)
  unsigned int& x_len = raster->x_dim;
  unsigned int& y_len = raster->y_dim;
  unsigned int min_count = ceil( (raster->max_count)*min_den );

  //empty raster properties (votes)
  Raster empty_raster;
  empty_raster.min_x = raster->min_x - max_radius;
  empty_raster.min_y = raster->min_y - max_radius;
  empty_raster.max_x = raster->max_x + max_radius;
  empty_raster.max_y = raster->max_y + max_radius;
  empty_raster.pixel_size = raster->pixel_size;
  empty_raster.setDims();
  empty_raster.setMatrixSize();

  //get valid pixels
  vector< array<unsigned int,2> > pixels;

  for(unsigned i = 0; i < x_len; ++i){
    for(unsigned j = 0; j < y_len; ++j){
      if(raster->matrix[i][j] >= min_count )
        pixels.push_back( {i,j} );
    }
  }

  //make circles centered in every valid pixel
  vector<HoughCenters> g_circles;
  double inclusionRadius = max_radius * 3;

  for(double rad = raster->pixel_size; rad <= max_radius; rad+=raster->pixel_size){

    vector< vector<unsigned int> > votes = empty_raster.matrix;
    PixelSet pixel_set;

    for(auto& p : pixels){

      vector<double> center = raster->absCenter(p[0], p[1]);
      PixelSet h_circle = empty_raster.rasterCircle(rad, center[0], center[1]);

      for(auto& h : h_circle){
        unsigned int vx = h[0];
        unsigned int vy = h[1];

        if( vx >= votes.size() || vy >= votes[0].size() ) continue;

        votes[ vx ][ vy ]++;

        if(votes[ vx ][ vy ] >= min_votes){
          array<unsigned int, 2> vxy = {vx, vy};
          pixel_set.insert( vxy );
        }
      }
    }

    for(auto& k : pixel_set){
      unsigned int vx = k[0];
      unsigned int vy = k[1];
      vector<double> coor = empty_raster.absCenter(vx, vy);

      HoughCircle hc;
      hc.radius = rad;
      hc.x_center = coor[0];
      hc.y_center = coor[1];
      hc.n_votes = votes[vx][vy];

      // disposable when looking at single circle
      bool newCircle = true;
      for(auto& gc : g_circles){

        double dist = sqrt( pow(hc.x_center - gc.avg_x, 2) + pow(hc.y_center - gc.avg_y, 2) );

        if(dist < inclusionRadius){
          gc.circles.push_back(hc);
          newCircle = false;
          break;
        }
      }

      if(newCircle){
        HoughCenters g_cen;
        g_cen.circles = {hc};
        g_cen.avg_x = hc.x_center;
        g_cen.avg_y = hc.y_center;
        g_cen.low_z = raster->min_z;
        g_cen.up_z = raster->max_z;
        g_cen.aggregate_radius = inclusionRadius;
        g_circles.push_back(g_cen);
      }
    }
  }

  for(auto& i : g_circles) i.getCenters();

  return g_circles;

}

