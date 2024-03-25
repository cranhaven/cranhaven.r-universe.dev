#include "surveygraph.h"
#include "graph.h"

#include "R.h"
#include "Rdefines.h"

#include <cmath>

using namespace std;

// build graph using Euclidean distance or cosine similarity
void graph::build_graph(const surveydef &S)
{
  network = map<int, set<neighbour>>{};
  for(int i = 0; i < n; ++i) network[i] = set<neighbour>{};

  e = 0;
  avg_degree = 0;
  for(unsigned int i = 0; i < n; ++i){
    for(unsigned int j = i + 1; j < n; ++j){
      double w = 0.0;
      man_distance(S, int(i), int(j), w);
      if(w > threshold){
        network[i].insert(neighbour{int(j), w});
        network[j].insert(neighbour{int(i), w});
        avg_degree += 2;
        e += 1;
      }
    }
  }
  // want average degree to be between 0 and 1
  avg_degree /= double(n);
}

// Manhattan distance between rows or columns u and v
void graph::man_distance(const surveydef &S, const int &u, const int &v, double &w)
{
  switch(layer){
    case Layer::agent:
      w = 0;
      for(int i = 0; i < m; ++i){
        w += abs(S[u][i] - S[v][i]);
      }
      w = (double(m) - w) / double(m);
      break;
    case Layer::symbolic:
      w = 0;
      for(int i = 0; i < m; ++i)
        w += abs(S[i][u] - S[i][v]);
      w = (double(m) - w) / double(m);
      break;
  }
}
