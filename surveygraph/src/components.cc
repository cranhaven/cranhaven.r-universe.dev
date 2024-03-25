#include "R.h"
#include "Rdefines.h"

#include "surveygraph.h"

#include <queue>

using namespace std;

// partitions network by connected components
void graph::build_partition() 
{
  partition = set<vector<int>>{};

  set<int> sorted;
  for(auto &it : network) sorted.insert(it.first);

  lcc = 0;
  isols = 0;
  comps = 0;
  vector<int> comp;
  while(sorted.size() > 0) {
    int u = *sorted.begin();
    bfs(u, comp);
    for(auto &it : comp){
      sorted.erase(it);
    }
    partition.insert(comp);
    if(comp.size() > lcc) lcc = comp.size();
    if(comp.size() == 1) isols += 1;
    comps += 1;
  }

  int norm = 0;
  for(auto it : partition) norm += it.size();
  if(norm != network.size()){
    error("an internal test has failed, please report to package creators\n");
  }
}

// get all nodes in connected component of u
void graph::bfs(const int &u, vector<int> &occupied) 
{
  occupied = vector<int>{};

  set<int> visited;
  queue<int> adjacent;

  occupied.push_back(u);
  visited.insert(u);

  for(auto &it : network[u]){
    adjacent.push(it.u);
    visited.insert(it.u);
  }

  while(adjacent.size() > 0) {
    int v = adjacent.front();
    for(auto &it : network[v]){
      if(visited.find(it.u) == visited.end()){
        adjacent.push(it.u);  
        visited.insert(it.u);  
      }
    }
    adjacent.pop();
    occupied.push_back(v);
  }
}
