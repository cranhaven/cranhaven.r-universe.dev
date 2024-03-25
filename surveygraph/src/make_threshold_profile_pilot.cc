#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

void surveygraph::make_threshold_profile_agent()
{
  profile_agent = vector<vector<double>>{};

  target_lcc = 1.00;
  search_threshold_agent_lcc();    // finds optimal threshold

  double optimal_threshold_agent = g_agent.threshold;

  int count = 200;

  double dt_agent = 1.0 / double(count);
  for(int i = 0; i < count; ++i){
    double threshold = i * dt_agent;

    g_agent = graph(0, threshold, survey);

    vector<double> dummy;
    dummy.push_back(threshold);
    dummy.push_back(g_agent.avg_degree / double(g_agent.n));
    dummy.push_back(g_agent.lcc / double(g_agent.n));
    dummy.push_back(double(g_agent.isols));
    dummy.push_back(double(g_agent.comps));

    profile_agent.push_back(dummy);

    if(!(g_agent.avg_degree / double(g_agent.n) >= 0 && g_agent.avg_degree / double(g_agent.n) <= 1)){
      error("an internal test has failed, please report to package creators\n");
    }
  }
}

// pilots the process of constructing respondent and item graphs
void surveygraph::make_threshold_profile_symbolic()
{
  profile_symbolic = vector<vector<double>>{};

  target_lcc = 0.95;
  search_threshold_symbolic_lcc(); // sets threshold_symbolic to optimal threshold

  double optimal_threshold_symbolic = g_symbolic.threshold;

  int count = 200;

  double dt_symbolic = 1.0 / double(count);
  for(int i = 0; i < count; ++i){
    double threshold = i * dt_symbolic;

    g_symbolic = graph(1, threshold, survey);

    vector<double> dummy;
    dummy.push_back(threshold);
    dummy.push_back(g_symbolic.avg_degree / double(g_symbolic.n));
    dummy.push_back(g_symbolic.lcc / double(g_symbolic.n));
    dummy.push_back(double(g_symbolic.isols));
    dummy.push_back(double(g_symbolic.comps));

    profile_symbolic.push_back(dummy);

    if(!(g_symbolic.avg_degree / double(g_symbolic.n) >= 0 && g_symbolic.avg_degree / double(g_symbolic.n) <= 1)){
      error("an internal test has failed, please report to package creators\n");
    }
  }
}
