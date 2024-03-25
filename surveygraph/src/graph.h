#ifndef GRAPH_H_
#define GRAPH_H_

#include <vector>
#include <map>
#include <set>

using namespace std;

typedef vector<vector<double>> surveydef;
enum class Layer{agent, symbolic};

struct neighbour
{
  neighbour(int a, double b) { u = a; w = b; }

  int u;     // neighbour index
  double w;  // edge weight, corresponding to agent or symbolic similarity

  bool operator<(const neighbour& rhs) const { return u < rhs.u; }
};

class graph
{
  public :
    graph(){}

    // provide flag for projection type, threshold, and survey data
    graph(const int &a, const double &b, const surveydef &S){
      switch(a){
        case 0:
          f = 0;
          layer = Layer::agent;
          n = S.size();          
          m = S[0].size();          
          break;
        case 1:
          f = 1;
          layer = Layer::symbolic;
          m = S.size();          
          n = S[0].size();          
          break;
        default:
          f = 0;
          layer = Layer::agent;
          n = S.size();          
          m = S[0].size();          
      }
      threshold = b;

      build_graph(S);
      build_partition();
    }

    Layer layer;
    int f;  // 0 for agent, 1 for symbolic, projection flag
    int n;  // number of graph nodes (nrow if agent, ncol if symbolic)
    int e;  // number of graph edges
    int m;  // complements n (ncol if agent, nrow if symbolic)
    double avg_degree;
    double threshold;

    map<int, set<neighbour>> network;  // neighbour list

    int lcc, isols, comps;
    set<vector<int>> partition;

    void build_graph(const surveydef&);
    void build_partition();  // computes distribution of component sizes
    void bfs(const int&, vector<int>&);   // breadth-first search

    void man_distance(const surveydef&, const int&, const int&, double&);
    void euclid_distance(const surveydef&, const int&, const int&, double&);
};
#endif
