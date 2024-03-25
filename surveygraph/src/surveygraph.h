#ifndef SURVEYGRAPH_H_
#define SURVEYGRAPH_H_

#include "graph.h"

#include <vector>
#include <map>
#include <set>

using namespace std;

class surveygraph
{
  public :
    surveygraph(vector<vector<double>> &a){
      survey = a;
      nrow = survey.size();
      ncol = survey[0].size();  // will have verified dimensions in R routines
    }

    surveygraph(vector<vector<double>> &a, int b, double c, int d){
      survey = a;
      nrow = survey.size();
      ncol = survey[0].size();  // verify dimensions in R routines

      // b is threshold method flag
      if(b == 0){        // method is target lcc
        target_lcc = c;     
      }else if(b == 1){  // method is target avg degree
        target_ad = c;      
      }else if(b == 2){  // method is input raw similarity threshold
        raw_similarity = c;        
      }

      // d is similarity metric flag
      if(d == 0){        // similarity metric is Manhattan distance
        metric = 0;
      }
    }

    double target_lcc, target_ad, raw_similarity;
    int metric;

    int nrow, ncol;  // number of agent, symbolic

    vector<vector<double>> survey, surveysample;  // survey, small sample of survey

    graph g_agent, g_symbolic;

    vector<vector<double>> profile_agent;     // agent threshold data
    vector<vector<double>> profile_symbolic;  // symbolic threshold data
    
    void search_threshold_agent_lcc();
    void search_threshold_agent_ad();
    void search_threshold_symbolic_lcc();
    void search_threshold_symbolic_ad();

    void make_proj_agent_lcc();      // builds agent projection graph with target largest component size
    void make_proj_agent_ad();       // builds agent projection graph with target average degree
    void make_proj_agent_similar();  // builds agent projection graph with desired threshold

    void make_proj_symbolic_lcc();      // builds symbolic projection graph with target largest component size
    void make_proj_symbolic_ad();       // builds symbolic projection graph with target average degree
    void make_proj_symbolic_similar();  // builds symbolic projection graph with desired threshold

    void max_threshold_agent(double, int);

    void make_threshold_profile_agent();     // sweeps through a range of radii and studies 
    void make_threshold_profile_symbolic();  // sweeps through a range of radii and studies 
};
#endif
