//define guards, so headers are declare only once.
#ifndef EDGE_H
#define EDGE_H

#include "Headers.h"

class edge {

 public:
  edge();
  ~edge();
  
  int target;        // Index in the vertex[] array of neighboring vertex.
                     // (Note that this is not necessarily equal to the GML
                     // ID of the neighbor if IDs are nonconsecutive or do
                     // not start at zero.)
  double K;          //Community edge belongs too.
  double weight;     // Weight of edge.  1 if no weight is specified.
  
};

#endif
