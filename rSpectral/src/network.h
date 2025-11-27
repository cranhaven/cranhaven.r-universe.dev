//define guards, so headers are declare only once.
#ifndef NETWORK_H
#define NETWORK_H

#include "Headers.h"
#include "vertex.h"

// struct to hold an edge
struct edgelist{
   int source;     // gml vertex id
   int target;     // gml vertex id
   double K;
   double weight;
 } ;

// Function to compare the IDs of two vertices
struct sortIDs{
    bool operator()(const vertex &v1p, const vertex &v2p) const {
      return v1p.id < v2p.id;
    }
  };


class network : vertex {

 public:
  network();
  ~network();
  void       removeVertices( int[], int, int );
  int        buildNetworkReps( bool=false, bool=true );
  void       buildA ( int );
  void       assignA();
  void       checkA();
  void       removeLoopsA();
  void       countEdges();
  int        countDegree();
  void       freeV();
  void       freeA();
  double *   getA();
  edgelist*  getEdgeList();
  int        getN();
  int        getM();
  int        getM2();
  int        getLOOPS();
  void       setN();
  void       setM();
  void       setM( int );
  
  // void printA();
  // void printEdgeList();
  // void printVertices();
  int  getMaxK();
  int  getMinK();
  void reorderK();
  void offSetK( int );
  // void setPrint( bool );
  
  int nvertices;     // Number of vertices in network
  int directed;      // 1 = directed network, 0 = undirected
  vertex *V;         // Array of VERTEX structs, one for each vertex

/*
  //Print _Nr x _Nc matrix _M
  static inline void printM( double *_M, int _Nr, int _Nc, const char* _Name ){

    int i,j,k,KK;
    
    cout << "Printing Matrix: " << _Name << endl;

    KK=_Nr * _Nc;
    for(k=0; k<KK; k++){
    i = floor(k/_Nr);
    j = k % _Nr;
    if( _M[(i*_Nr)+j] != 0 ) cout << " " << _M[(i*_Nr)+j] << " ";
    else                     cout << " . ";

    if( j == (_Nr-1) )       cout << "" << endl;
    }
    
 };
*/
 
 private:
  int   AKK;//Adjanceny matrix size

  double *A;//Adjacency matrix
  int     N;
  int     M;
  int     M2;
  int     LOOPS;

  int     maxTYPES;

  // bool PRINT;
  
  edgelist *el;

  void checkVertexDegree( double * );

};

#endif

