#include "network.h"
#include <Rcpp.h>
//using namespace Rcpp;

network::network() : vertex() {

  nvertices=0;
  directed =0; 
  V        =0;

  AKK      =0;
  A        =0;
  el       =0;
  N        =0;
  M        =0;
  M2       =0;
  LOOPS    =0;
  maxTYPES =0;
  // PRINT    = false;
  
}

network::~network(){

  //---free up memory of vertices
  freeV();
   
  //---free up memory of Adjacency matrix
  freeA();
  
  //---delete a 1d array in C++
  if( el != 0 ){ delete[] el; }
  
}

void      network::freeV(){

  if( V!=0 && nvertices!=0 ){ delete[] V; }

  nvertices = 0;
  N         = 0;
  
}

void      network::freeA(){

  if( A!=0 && AKK != 0 ){ delete[] A; }

  AKK = 0;

}

void      network::buildA( int _AKK ){

  int k,KK;

  N  = nvertices;
  KK = N * N;
  
  if( _AKK >=0 && _AKK == KK ){

    freeA();
    AKK = _AKK;

    A = new double[AKK];
    for(k=0; k<AKK; k++){ A[k] = 0.0; }
    
  }
  
}

void      network::assignA(){

  int i,j,I,J;

  N = nvertices;
  
  for(i=0; i<N; ++i){
    for(j=0; j<V[i].degree; j++){
      I = (int)V[i].id;
      J = (int)V[i].E[j].target;

      A[(I*N)+J] = (double)V[i].E[j].weight;

    }
  }


}

void network::checkA(){

  //--- check A if network is undirected
  if( directed == 0 ){

    int i,j,k,KK;
    double wei, wej;
    
    N  = nvertices;

    KK = N*(N+1)/2;
    
    for(k=0; k<KK; k++){

      //--- linear indexing for symmetric matrix
      i = floor( (2*N+1 - sqrt( (2*N+1)*(2*N+1) - 8*k ))/2 );
      j = k - N*i + i*(i-1)/2;

      wei = (double)A[(i*N)+j];
      wej = (double)A[(j*N)+i];
		
      if( wei != wej ){

	if( wei != 0 && wej == 0 ){ A[(j*N)+i] = wei; }
	if( wej != 0 && wei == 0 ){ A[(i*N)+j] = wej; }
	
      }

    }
  }

  
}
  
void      network::removeLoopsA(){

  int i,j,I,J;

  N = nvertices;
  
  for(i=0; i<N; ++i){
    for(j=0; j<V[i].degree; j++){
      I = (int)V[i].id;
      J = (int)V[i].E[j].target;

      //A[(I*N)+J] = (double)V[i].E[j].weight;

      if( I==J ) A[(I*N)+J] = 0.0;
      
    }
  }


}


void network::countEdges(){

  int i,j,k,KK;

  N = nvertices;

  KK = N * N ;
  
  //---test, counting number of edges in A
  for(k=0, M2=0, M=0, LOOPS=0; k<KK; k++){
    if( A[k] != 0 ) M++;
    
    i = floor(k/N);
    j = k % N;

    if( j>=i && A[(i*N)+j] != 0 ){
      M2++;
      if(j==i){ LOOPS++; }
    }

  }
       
  // if( PRINT ){ cout << "TEST: M " << M << " M/2 " << M2 << " LOOPS " << LOOPS << " sum(D) " << countDegree() << endl; }
  
}

int network::countDegree(){

  int i,sum;

  N = nvertices;
  
  for(i=0,sum=0; i<N; ++i){ sum += (int)V[i].degree; }

  return sum;
  
}


int       network::getN()        { return nvertices; }

int       network::getM()        { return M; }

int       network::getM2()       { return M2; }

int       network::getLOOPS()    { return LOOPS; }

void      network::setN()        { if( V!=0 ) N = nvertices; }

void      network::setM( int Mm ){ if( V!=0 ) M = Mm; }

void      network::setM(){

  // int i;
  
  if( V!=0 ){
    
    N = nvertices;
    M = 0;  
    M = countDegree();

  }
    
}



double*  network::getA()        { return A; }

edgelist* network::getEdgeList() { return el; }

//build the network repersentations, Adjacency Matrix & edge list
int network::buildNetworkReps( bool useLOOPS, bool Check ){

  // int i,j,m,k,I,J,KK;
  // int status;
  // double sum;
  int KK;
  
  if( V!= 0 ){

    N = nvertices;
    
    //linear indexing, size K is rows (N) x cols (N).
    KK=N*N;

    //---
    //---Create Adjacency matrix
    buildA  ( KK );
    assignA ();

    //--- check A if network is undirected
    checkA();
    
    
    //---test, counting number of edges in A
    countEdges();

    //Check vertex degree from .gml file and our Adjacency matrix,
    //always true if we're not reading in networks from .gml file. 
    if( Check == true ){

      checkVertexDegree( A );    

      //---test, counting number of edges in A
      countEdges();
    
    }
      
    //remove loops
    if( !useLOOPS ){

      removeLoopsA();
    
      //---test, counting number of edges in A
      countEdges();
    
    }
         

  } else { return -1; }

  return 0;

}

void network::checkVertexDegree( double *AA ){

  // int i,j,k,K,sum;
  int i,j,k,sum;
  
  N = nvertices;
  
  // if( PRINT ){cout << "checking vertex degrees..."; }
   
  for(i=0; i<N; ++i){
    sum=0;
    k=0;
    for(j=0; j<N; ++j){ if(AA[(i*N)+j] != 0) sum++; }
    V[i].assignE( sum );
    for(j=0; j<N; ++j){ 
      if(AA[(i*N)+j] != 0){
	V[i].E[k].target = j; 
	V[i].E[k].weight = (double)AA[(i*N)+j];
	k++; 
      }//if
    }
  }

  // if( PRINT ){cout << "done." << endl; }

 }

void network::removeVertices( int keys[], int length, int dummy ){

  int i,j,p,k,KK,KKin,Ng;

  N = nvertices;

  //find size of remaining vertices
  Ng = 0;
  for(i=0; i<N; i++){
    if( keys[i] !=  dummy ) Ng++;
  }
  
  if( (length == N) && (Ng != 0) && (Ng < N) ){

    // if( PRINT ){cout << "Resizing V from " << N << " to " << Ng << "..." << endl; }
    
    //Set current size of the Adjaceny matrix
    KKin = N * N;

    //make sure A exists
    if( A==0 || AKK == 0 ){
      buildA ( KKin );
      assignA();
    }

    //construct temp A
    KK = Ng * Ng;
    //double tA[KK];
    double* tA = (double*)malloc(KK*sizeof(double));
    
    for(k=0, p=0; k<KKin; k++){

      i = floor(k/N);
      j = k % N;

      if( keys[i] != dummy && keys[j] != dummy ){
	tA[p] = 0.0;
	tA[p] = A[(i*N)+j]; p++; }
      
    }
   
    //free A
    freeA();

    //copy V to temp V
    vertex *tV = new vertex[Ng];
    for(i=0, p=0; i<N; ++i){

      if( keys[i] != dummy ){
	tV[p].copy(&V[i]); p++;
      }

    }

    //free V
    freeV();

    //resize V to tV size Ng
    V = new vertex[Ng];
    nvertices = Ng;
    for(i=0; i<Ng; ++i){
      V[i].copy(&tV[i]);
      V[i].id = i;
    }

    //delete temp V
    if( tV!=0 && Ng!=0 ){ delete[] tV; Ng=0; }

    //Now assign edges to V.E using the Adjaceny matrix
    checkVertexDegree( tA  );    
    
    // if( PRINT ){cout << "done." << endl; }
    free(tA);
  }
  
  
}
/*
void network::printA(){
  
  int i,j,k,K;

  if( A!=0 && AKK!=0 ){
  
    // if( PRINT ){ cout << "Adjancency Matrix: " << endl; }
  N=nvertices;
  K=N*N;
  for(k=0; k<K; k++){
    i = floor(k/N);
    j = k % N;
    // if( A[(i*N)+j] != 0 ) if( PRINT ){ cout << " " << A[(i*N)+j] << " "; }
    // else                  if( PRINT ){ cout << " . "; }
    // 
    // if( j == (N-1) )      if( PRINT ){ cout << "" << endl; }
  }

  // if( PRINT ){ cout << "" << endl; }

  } else {
    // if( PRINT ){ cout << "Empty container." << endl; }
  }
  
  
}

void network::printEdgeList(){

  
  // int m;
  // 
  // if( el != 0 ){
  // 
  //   if( PRINT ){ cout << "Edge List" << endl; }
  //   for(m=0; m<M; ++m){
  //     if( PRINT ){ cout << "el.source: " << el[m].source << " el.target " << el[m].target << " el.weight " << el[m].weight << endl; }
  //   }
  // } else {
  //   if( PRINT ){ cout << "Empty container." << endl; }
  // }
  // 

}


void network::printVertices(){

  
  // int v;
  // 
  // if( V!=0 && nvertices!=0 ){
  // 
  //   N    = nvertices;
  //   
  //   if( PRINT ){ cout << "Vertex Properties" << endl; }
  // 
  //   for(v=0; v<N; ++v){
  //     if( PRINT ){ cout << "id " << V[v].id << " label " << V[v].label << " degree " << V[v].degree << " K " << V[v].K << endl; }
  //   }
  // 
  // } else {
  //   if( PRINT ){ cout << "Empty container." << endl; }
  // }
  // 

}
*/

int network::getMaxK(){

  int v, Ncom;

  Ncom = -1;
  
  if( V!=0 && nvertices!=0 ){

    N    = nvertices;
    Ncom = 0;
    Ncom = V[0].K;
  
    for(v=0; v<N; ++v){
      if( V[v].K > Ncom ) Ncom = V[v].K; 
    }
  
  }  

  return Ncom;
  
}

int network::getMinK(){

  int v, Ncom;

  Ncom = -1;
  
  if( V!=0 && nvertices!=0 ){

    N    = nvertices;
    Ncom = 0;
    Ncom = V[0].K;
  
    for(v=0; v<N; ++v){
      if( V[v].K < Ncom ) Ncom = V[v].K; 
    }
  
  }  

  return Ncom;
  
}

void network::reorderK(){

  int v,counter,Knew,Kmax;

  if( V!=0 && nvertices!=0 ){

    N = nvertices;
    
    //int temp[N];
    Rcpp::IntegerVector  temp(N);
    counter = getMinK();
    Knew    = 1;
    Kmax    = getMaxK();

    while( counter <= Kmax ){

      bool found=false;
      
      for(v=0; v<N; v++){
	if( V[v].K == counter ){
	  temp[v] = Knew;
	  found=true;
	}
      }

      if(found) Knew++;
      
      counter++;
    }

    for(v=0; v<N; v++){ V[v].K = temp[v]; }
    
  }
  
}

void network::offSetK( int offset ){

  int v;

  if( V!=0 && nvertices!=0 && offset >= 0 ){

    N    = nvertices;
  
    for(v=0; v<N; ++v)
      V[v].K = (V[v].K - offset) + 1;

  }
    
}

/*
void network::setPrint( bool status ){
  PRINT = status;
}
*/
