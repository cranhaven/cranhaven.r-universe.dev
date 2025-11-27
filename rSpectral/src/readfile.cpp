// Functions to read a network stored in a GML file into a NETWORK struct
//
// Written by Mark Newman  11 AUG 06
// Changed to allow node labels containing the word "node", which previously
//   confused the (rather simple) code for counting network nodes  3 DEC 14
//
// To use this package, #include "readgml.h" at the head of your program
// and then call the following:
//
// Function calls:
//   int read_network(NETWORK *network, FILE *stream)
//     -- Reads a network from the FILE pointed to by "stream" into the
//        structure "network".  For the format of NETWORK structs see file
//        "network.h".  Returns 0 if read was successful.
//   void free_network(NETWORK *network)
//     -- Destroys a NETWORK struct again, freeing up the memory
// Inclusions, found in Headers.h
#include "readfile.h"

// Function to establish whether the network read from a given stream is
// directed or not.  Returns 1 for a directed network, and 0 otherwise.  If
// the GML file contains no "directed" line then the graph is assumed to be
// undirected, which is the GML default behavior.



// Function to count the vertices in file.  Returns number of vertices.
// removeDuplicates first sorts the string/int vector in ascending order
int readfile::count_vertices()
{
  int result=0;

  if( isLabel == 1 ){  
    removeDuplicates( labels );
    result = labels.size();
  } else {    
    removeDuplicates( labelsI );  
    result = labelsI.size();
  }
    
  return result;
}




// Function to allocate space for a network structure stored in a GML file
// and determine the parameters (id, label) of each of the vertices.

void readfile::create_network()
{
  int i, length;  
  char lab[LINELENGTH];
  
  // Determine whether the network is directed
  net->directed = 0;

  // Count the vertices
  net->nvertices = count_vertices();

    
  // Make space for the vertices
  ////net->V = (vertex*)calloc(net->nvertices,sizeof(vertex));
  net->V = new vertex[net->nvertices];
   
  // Go through the file reading the details of each vertex one by one
  if( isLabel == 1 ){
    for(i=0; i<net->nvertices; ++i){    
      net->V[i].id = i;
      //sprintf(lab,"%s",labels[i].c_str());
      //length = strlen(lab);
      length = snprintf(lab,sizeof(lab),"%s",labels[i].c_str());
      lab[length] = '\0';
      ////net->V[i].label = (char*)malloc((length+1)*sizeof(char));
      ////net->V[i].assignLabel( (length+1), lab );
      net->V[i].label = new char[(length+1)];
      strcpy(net->V[i].label,lab);
    }
  } else {
    for(i=0; i<net->nvertices; ++i){    
      net->V[i].id = i;
      //sprintf(lab,"%d",labelsI[i]);
      //length = strlen(lab);
      length = snprintf(lab,sizeof(lab),"%d",labelsI[i]);
      lab[length] = '\0';
      ////net->V[i].assignLabel( (length+1), lab );
      ////net->V[i].label = (char*)malloc((length+1)*sizeof(char));
      net->V[i].label = new char[(length+1)];
      strcpy(net->V[i].label,lab);
    }
  }
  
  
}


   // Function:
   //   Searches sortedVector[first]..sortedVector[last] for key.  
   // returns: index of the matching element if it finds key, 
   //         otherwise  -(index where it could be inserted)-1.
   // parameters:
   //   sortedVector in  array of sorted (ascending) values.
   //   first, last in  lower and upper subscript bounds
   //   key         in  value to search for.
   // returns:
   //   index of key, or -insertion_position -1 
   //                 if key is not in the array.
int readfile::find_vertex(int first, int last, string key)
{
    
  if (first <= last) {
       int mid = (first + last) / 2;  // compute mid point.
       if ( key.compare(labels[mid]) == 0 ) 
           return mid;   // found it.
       else if (key.compare(labels[mid]) < 0 ) 
           // Call ourself for the lower part of the array
           return find_vertex(first, mid-1, key);
       else
           // Call ourself for the upper part of the array
           return find_vertex(mid+1, last, key);
   }
   return -(first + 1);    // failed to find key
  
}

int readfile::find_vertex(int first, int last, int key)
{
    
  if (first <= last) {
       int mid = (first + last) / 2;  // compute mid point.
       if ( key == labelsI[mid] ) 
           return mid;   // found it.
       else if ( key < labelsI[mid] ) 
           // Call ourself for the lower part of the array
           return find_vertex(first, mid-1, key);
       else
           // Call ourself for the upper part of the array
           return find_vertex(mid+1, last, key);
   }
   return -(first + 1);    // failed to find key
  
}


// Function to determine the degrees of all the vertices by going through
// the edge data

void readfile::get_degrees()
{
  int i,j,k,KK,vs,vt;
    
  KK = NROWS * NCOLS;

  if( isLabel == 1 ){

    string s,t;  
    s = t = "";   
  
    for( k=0; k<KK; k++ ){

      i = floor(k/NCOLS);
      j = k % NCOLS;

      // Read the source of the edge    
      if( j == 0 )
	s = dataset[(i*NCOLS)+j];

      // Read the target of the edge    
      if( j == 1 )
	t = dataset[(i*NCOLS)+j];

      if ((s!="")&&(t!="")) {
	vs = find_vertex(0, net->nvertices, s);
	net->V[vs].degree++;
	if (net->directed==0) {
	  vt = find_vertex(0, net->nvertices, t);
	  net->V[vt].degree++;
	}
	//reset s & t
	s = t = "";   
      }
         
    }

  } else {

    int s,t;  
    s = t = -1;   
  
    for( k=0; k<KK; k++ ){

      i = floor(k/NCOLS);
      j = k % NCOLS;

      // Read the source of the edge    
      if( j == 0 )
	s = stoi(dataset[(i*NCOLS)+j]);

      // Read the target of the edge    
      if( j == 1 )
	t = stoi(dataset[(i*NCOLS)+j]);

      if ((s>=0)&&(t>=0)) {
	vs = find_vertex(0, net->nvertices, s);
	net->V[vs].degree++;
	if (net->directed==0) {
	  vt = find_vertex(0, net->nvertices, t);
	  net->V[vt].degree++;
	}
	//reset s & t
	s = t = -1;   
      }
         
    }

  }
    
  return;

}


// Function to read in the edges

void readfile::read_edges()
{
  int i,j,k,KK;
  int vs,vt;
  int *count;
  double w, sum;
  
  // Malloc space for the edges and temporary space for the edge counts
  // at each vertex

  sum = 0;
  for (i=0; i<net->nvertices; ++i) {
    net->V[i].E  = new edge[(int)net->V[i].degree];
    ////net->V[i].E = (edge*)malloc(net->V[i].degree*sizeof(edge));    
    sum        += net->V[i].degree; 
  }

  net->setM((int)sum);
  
  count = (int*)calloc(net->nvertices,sizeof(int));

  KK = NROWS * NCOLS;

  if( isLabel == 1 ){

    string s,t;
    s = t = "";
    w = 1.0;
    
    for(k=0; k<KK; k++){

      i = floor(k/NCOLS);
      j = k % NCOLS;
      
      // Read the source of the edge    
      if( j == 0 )
	s = dataset[(i*NCOLS)+j];

      // Read the target of the edge    
      if( j == 1 )
	t = dataset[(i*NCOLS)+j];


      // Add these edges to the appropriate vertices
      if ((s!="")&&(t!="")) {

	if( NCOLS == 3 )
	  w = stod(dataset[(i*NCOLS)+2]);  
	
	vs = find_vertex(0, net->nvertices, s);
	vt = find_vertex(0, net->nvertices, t);

	net->V[vs].E[count[vs]].target = vt;
	net->V[vs].E[count[vs]].weight = w;
	count[vs]++;
	if (net->directed==0) {
	  net->V[vt].E[count[vt]].target = vs;
	  net->V[vt].E[count[vt]].weight = w;
	  count[vt]++;
	}      
	//reset s & t
	s = t = "";
	w = 1.0;
      }
    }

    
  } else {

    int s,t;
    s = t = -1;
    w = 1.0;

    for(k=0; k<KK; k++){

      i = floor( k/NCOLS );
      j = k % NCOLS;

      // Read the source of the edge    
      if( j == 0 )
	s = stoi(dataset[(i*NCOLS)+j]);

      // Read the target of the edge    
      if( j == 1 )
	t = stoi(dataset[(i*NCOLS)+j]);

      // Add these edges to the appropriate vertices
      if ((s>=0)&&(t>=0)) {

	if( NCOLS == 3 )
	  w = stod(dataset[(i*NCOLS)+2]);  
	
	vs = find_vertex(0, net->nvertices, s);
	vt = find_vertex(0, net->nvertices, t);
	net->V[vs].E[count[vs]].target = vt;
	net->V[vs].E[count[vs]].weight = w;
	count[vs]++;
	if (net->directed==0) {
	  net->V[vt].E[count[vt]].target = vs;
	  net->V[vt].E[count[vt]].weight = w;
	  count[vt]++;
	}
	//reset s & t
	s = t = -1;
	w = 1.0; 
      }

    }

  }

  free(count);
  return;
}


readfile::readfile(){ 
  
  this->net       = 0;
  this->NROWS     = 0;
  this->NCOLS     = 0;
  this->isLabel   = 0;
  this->OFFSET    = 0;
  this->Header    = false;
  this->Skip      = 0;
  this->dataset   = 0;
  
}


// Function to read a complete network file
readfile::readfile(network *NET, string *DATAIN, int nCOLS, int nROWS, int LABEL)
{ 
  int i,j,k,KK;

  this->net       = 0;
  this->NROWS     = 0;
  this->NCOLS     = 0;
  this->isLabel   = 0;
  this->OFFSET    = 0;
  this->Header    = false;
  this->Skip      = 0;
  this->dataset   = 0;
  
  this->net       = NET;
  this->NROWS     = nROWS;
  this->NCOLS     = nCOLS;
  this->isLabel   = LABEL;//are we dealing with alphaNumeric (0) or numeric (!0) ids
  this->OFFSET    = 0;
  this->Header    = false;
  this->Skip      = 0;
  this->dataset   = DATAIN;


  KK = NROWS * NCOLS;

  if( isLabel == 1 ){
  
    for(k=0; k<KK; k++){
      i = floor(k/NCOLS);
      j = k % NCOLS;

      //if weighted, store two labels of edge
      if( j <= 1 ){ labels.push_back( dataset[k] ); }

    }
  } else {

    for(k=0; k<KK; k++){
      i = floor(k/NCOLS);
      j = k % NCOLS;
      
      if ( j <= 1 ){ labelsI.push_back( stoi(dataset[k]) ); }
	
    }
      
  }

  create_network();
  get_degrees();
  read_edges();
 
}


string* readfile::getDataSet(){

  return dataset;
  
}

readfile::~readfile(){}
