//define guards, so headers are declare only once.
#ifndef VERTEX_H
#define VERTEX_H

#include "edge.h"

class vertex : edge {

 public:
  vertex();
  ~vertex();
  void copy( vertex* );
  void assignNsamples( int );
  void assignKprobs  ( int );
  void assignE       ( int );
  void freeNsamples  ();
  void freeKprobs    ();  
  void freeE         ();

  void freeSpace();
  // void printV();
  // void setPrint( bool );
  
  int id;            // GML ID number of vertex
  int degree;        // Degree of vertex (out-degree for directed nets)
  int K;             // Concrete community;
  double bridge;     // Vertex bridgeness
  char *label;       // GML label of vertex.  NULL if no label specified
  edge   *E;         // Array of EDGE structs, one for each neighbor
  double *Kprobs;    // Community (K) probabilities
  double *Nsamples;  // N gene expression samples
  
  int Nk;            // Size of Kprobs
  int Ns;            // Size of Nsamples;

 // private:  
 //  bool PRINT;
  
};

#endif
