#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <cmath>

using namespace std;

void surveygraph::make_proj_symbolic_lcc()
{
  search_threshold_symbolic_lcc();  // finds threshold_symbolic with desired lcc
}

void surveygraph::make_proj_symbolic_ad()
{
  search_threshold_symbolic_ad();  // finds threshold_symbolic with desired avg degree
}

void surveygraph::make_proj_symbolic_similar()
{
  double threshold = raw_similarity;
  g_symbolic = graph(1, threshold, survey);
}

void surveygraph::search_threshold_symbolic_lcc()
{
  double tlower = -1;
  double tupper = 1;
  double threshold = 0;

  bool tfound = false;
  int i = 0;
  while(!tfound && i < 20){  // bisection method
    threshold = (tlower + tupper) / 2.0;
    g_symbolic = graph(1, threshold, survey);

    double lccdummy = g_symbolic.lcc / double(g_symbolic.network.size());

    // not exploring all cases?
    if(lccdummy > target_lcc){
      tlower = g_symbolic.threshold;
    }else if(lccdummy < target_lcc){
      tupper = g_symbolic.threshold;
    }else if(lccdummy == target_lcc){
      tfound = true;
    }
    i += 1;
  }
}

void surveygraph::search_threshold_symbolic_ad()
{
  double tlower = -1;
  double tupper = 1;
  double threshold = 0;

  bool tfound = false;
  int i = 0;
  while(!tfound && i < 20){  // bisection method
    threshold = (tlower + tupper) / 2.0;
    g_symbolic = graph(1, threshold, survey);

    double addummy = g_symbolic.avg_degree / double(g_symbolic.network.size());

    // hello Ana
    if(addummy > target_ad){
      tlower = g_symbolic.threshold;
    }else if(addummy < target_ad){
      tupper = g_symbolic.threshold;
    }else if(addummy == target_ad){
      tfound = true;
    }
    i += 1;
  }
}
