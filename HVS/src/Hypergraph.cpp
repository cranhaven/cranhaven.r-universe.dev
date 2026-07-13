/* Method for counting independent sets of a hypergraph.

 */

#include <cstdlib>
#include <iostream>
#include <cmath>
#include <R.h>

using namespace std;

inline unsigned int random(unsigned int n){
  //generates a random number in the range 0 to n-1.

  double ru=unif_rand();
  return(floor(ru*n));
};


class Permutation{
  unsigned int n;
  unsigned int *pi;
  unsigned int *inverse;
  unsigned int pos;
  char* skip; 
  
public:
  Permutation(unsigned int sz);
  ~Permutation();
  void random();
  friend std::ostream& operator <<(std::ostream &out,const Permutation &p);
  unsigned int next();
  void set_skip(unsigned int i);
};

Permutation::~Permutation(){
  delete[] pi;
  delete[] inverse;
  delete[] skip;
};

Permutation::Permutation(unsigned int sz):n(sz),pos(0){
  pi=new unsigned int[n];
  inverse=new unsigned int[n];
  skip=new char[n];
  for(unsigned int i=0;i<n;i++){
    pi[i]=i;
    inverse[i]=i;
    skip[i]=0;
  };   
};


inline void Permutation::set_skip(unsigned int i){
  *(skip+inverse[i])=1;
  for(;pos<n&&skip[pos];pos++);
};

void Permutation::random(){
  // randomly switch value in each position i with a later value, or
  // the same value.
  for(unsigned int i=0;i<n-1;i++){
    unsigned int s=::random(n-i);
    unsigned int temp=*(pi+i);
    *(pi+i)=*(pi+i+s);
    *(pi+i+s)=temp;
  };
  for(unsigned int i=0;i<n;i++){
    *(inverse+*(pi+i))=i;
    *(skip+i)=0;
  };
  pos=0;
};

unsigned int Permutation::next(){
  if(pos<n){
    unsigned int ans=pi[pos];
    for(pos++;pos<n&&*(skip+pos);pos++);
    return ans;
  }else{
    pos=0;
    return n; // If finished, return n.
  };
};

std::ostream& operator <<(std::ostream &out,const Permutation &p){
  for(unsigned int i=0;i<p.n;i++){
    out<<*(p.pi+i)<<" ";     
  };
  return out;
};

class list{
public:
  unsigned int size;
  unsigned int* elements;

  void setsize(unsigned int n);
  void setelement(unsigned int pos,unsigned int val);
  void set(unsigned int n,const bool *selected);
  void set_list(unsigned int n,const unsigned int *entries);
  void set_list(unsigned int n,const int *entries);
  list();
  ~list();
};

void list::setsize(unsigned int n){
  if(elements){
    delete[] elements;
  };
  size=n;
  elements=new unsigned int[size];
};

void list::setelement(unsigned int pos,unsigned int val){
  if(pos<size){
    elements[pos]=val;
  };
};


void list::set(unsigned int n,const bool *selected){
  if(elements){
    delete[] elements;
  };
  size=0;
  for(unsigned int i=0;i<n;i++){
    if(selected[i]){
      size++;
    };
  };
  elements=new unsigned int[size];
  unsigned int j=0;
  for(unsigned int i=0;i<n;i++){
    if(selected[i]){
      elements[j++]=i;
    };
  };
};


void list::set_list(unsigned int n,const unsigned int *entries){
  if(elements){
    delete[] elements;
  };
  size=n;
  elements=new unsigned int[size];  
  for(unsigned int i=0;i<size;i++){
    elements[i]=entries[i];
  };
};

void list::set_list(unsigned int n,const int *entries){
  if(elements){
    delete[] elements;
  };
  size=n;
  elements=new unsigned int[size];  
  for(unsigned int i=0;i<size;i++){
    elements[i]=entries[i];
  };
};


list::list(){
  size=0;
  elements=NULL;
};

list::~list(){
  if(elements){
    delete[] elements;
  };
};


class hypergraph{
  unsigned int nv; //number of vertices
  unsigned int ne; //number of edges
  list *vertices; //each vertex is a list of the edges that contain it.
  list *edges; //each edge is a list of the vertices it contains.

  //Temporary calculation variables
  unsigned char* used; //Indicates whether a vertex is already included or excluded.
  unsigned int* order; //The order of each edge in the reduced hypergraph.
  //Set to zero if the edge is removed.

public:
  
  hypergraph(unsigned int edgenum,unsigned int vertnum);
  ~hypergraph();
  void random(double p); //generates a random hypergraph with the
			 //specified number of edges, where each edge
			 //contains each vertex with probability p.
  void read(const unsigned int *ord,const unsigned int *eds);
  void read(const int *ord,const int *eds);
  double countindep(unsigned int N); //performs the subsampling   
  long unsigned int countindep_exact();//Uses a BK-type approach to count all independent sets.
  friend std::ostream& operator <<(std::ostream &out,const hypergraph &h);
};

std::ostream& operator <<(std::ostream &out,const list &l){
  out<<"{";
  for(unsigned int i=0;i+1<l.size;i++){
    out<<l.elements[i]<<",";
  };
  if(l.size){
    out<<l.elements[l.size-1];
  };
  out<<"}";
  return out;
};

std::ostream& operator <<(std::ostream &out,const hypergraph &h){
  for(unsigned int i=0;i<h.ne;i++){
    out<<*(h.edges+i)<<"\n";     
  };
  return out;
};

hypergraph::hypergraph(unsigned int edgenum,unsigned int vertnum):nv(vertnum),ne(edgenum){
  vertices=new list[nv]; 
  edges=new list[ne]; 
  used=new  unsigned char[nv];
  order=new unsigned int[ne];
};

hypergraph::~hypergraph(){
  delete[] vertices;
  delete[] edges;
  delete[] used;
  delete[] order;
};


void hypergraph::random(double p){
  //generates a random hypergraph with the specified numb
  bool *sel=new bool[nv];
  unsigned int* degree=new unsigned int[nv];
  for(unsigned int i=0;i<nv;i++){
    degree[i]=0;
  };
  for(unsigned int i=0;i<ne;i++){
    for(unsigned int j=0;j<nv;j++){
      sel[j]=unif_rand()<p;
      if(sel[j]){
	degree[j]++;
      };
    };
    (edges+i)->set(nv,sel);
  };
  for(unsigned int i=0;i<nv;i++){
    (vertices+i)->setsize(degree[i]);
    degree[i]=0; //use degree to count position.
  };
  for(unsigned int i=0;i<ne;i++){
    for(unsigned int j=0;j<(edges+i)->size;j++){
      unsigned int k=(edges+i)->elements[j];
      (vertices+k)->setelement(degree[k]++,i);
    };
  };
  delete[] sel;
  delete[] degree;
};

void hypergraph::read(const unsigned int *ord,const unsigned int *eds){
  //Reads in a hypergraph from a list of orders and the concatenated
  //list of vertices.
  const unsigned int* current=eds;
  for(unsigned int i=0;i<ne;i++){
    (edges+i)->set_list(ord[i],current);
    current+=ord[i];
  };
    

  unsigned int* degree=new unsigned int[nv];
  for(unsigned int i=0;i<nv;i++){
    degree[i]=0;
  };
  for(unsigned int i=0;i<ne;i++){
    for(unsigned int j=0;j<(edges+i)->size;j++){
      unsigned int k=(edges+i)->elements[j];
      degree[k]++;
    };
  };

  for(unsigned int i=0;i<nv;i++){
    (vertices+i)->setsize(degree[i]);
    degree[i]=0; //use degree to count position.
  };
  for(unsigned int i=0;i<ne;i++){
    for(unsigned int j=0;j<(edges+i)->size;j++){
      unsigned int k=(edges+i)->elements[j];
      (vertices+k)->setelement(degree[k]++,i);
    };
  };

  delete[] degree;  
};


void hypergraph::read(const int *ord,const int *eds){
  //Reads in a hypergraph from a list of orders and the concatenated
  //list of vertices.

  const int* current=eds;
  for(unsigned int i=0;i<ne;i++){
    (edges+i)->set_list(ord[i],current);
    current+=ord[i];
  };


  unsigned int* degree=new unsigned int[nv];
  for(unsigned int i=0;i<nv;i++){
    degree[i]=0;
  };
  for(unsigned int i=0;i<ne;i++){
    for(unsigned int j=0;j<(edges+i)->size;j++){
      unsigned int k=(edges+i)->elements[j];
      degree[k]++;
    };
  };

  for(unsigned int i=0;i<nv;i++){
    (vertices+i)->setsize(degree[i]);
    degree[i]=0; //use degree to count position.
  };
  for(unsigned int i=0;i<ne;i++){
    for(unsigned int j=0;j<(edges+i)->size;j++){
      unsigned int k=(edges+i)->elements[j];
      (vertices+k)->setelement(degree[k]++,i);
    };
  };

  delete[] degree;  
};




double hypergraph::countindep(unsigned int N){
  //Counts the number of independent sets, using an importance
  //sampling approach based on randomly constructing an independent
  //set.
  double ans=0;
  Permutation perm(nv);
  for(unsigned int sample=0;sample<N;sample++){
    //initialise used and order variables
    for(unsigned int i=0;i<nv;i++){
      used[i]=0;
    };
    for(unsigned int i=0;i<ne;i++){
      order[i]=(edges+i)->size;
    };
    perm.random();
    unsigned int avail=nv;
    // remove all loops
    // could do this once for all permutations.
    for(unsigned int e=0;e<ne;e++){
      if(order[e]==1){
	unsigned int l=(edges+e)->elements[0];
	used[l]=1;
	avail--;
	perm.set_skip(l); // vertex l cannot be added to the independent set	
	// Remove all edges that contain vertex l, as they can
	// never be completed.
	for(unsigned int m=0;m<(vertices+l)->size;m++){
	  order[(vertices+l)->elements[m]]=0;
	};
      };
    };
    
    double w=1;
    double tot=1;
    unsigned int iter=1;
    for(unsigned int i=perm.next();i<nv;i=perm.next()){
      //i is added to the independent set.
      w*=((double)avail)/(iter++);
      tot+=w;
      used[i]=1;
      avail--;
      //remove i from all edges that contain it.
      for(unsigned int j=0;j<(vertices+i)->size;j++){
	unsigned int e=(vertices+i)->elements[j];
	if(order[e]){
	  order[e]--;
	  if(order[e]==1){
	    //This edge has become a loop.
	    //Remove the remaining vertex.
	    unsigned int k=0;
	    for(;k<(edges+e)->size&&used[(edges+e)->elements[k]];k++);
	    unsigned int l=(edges+e)->elements[k];
	    used[l]=1;
	    avail--;
	    perm.set_skip(l); // vertex l cannot be added to the independent set. 
	    // Remove all edges that contain vertex l, as they can
	    // never be completed.
	    for(unsigned int m=0;m<(vertices+l)->size;m++){
	      order[(vertices+l)->elements[m]]=0;
	    };
	  };
	};
      };
    };
    ans+=tot;
  };
  return ans/N;
};

long unsigned int hypergraph::countindep_exact(){
  //calculates the exact number of independent sets.
  bool *in=new bool[nv];
  long unsigned int ans=0;
  unsigned int pos=0;
  for(unsigned int i=0;i<ne;i++){
    order[i]=(edges+i)->size;
  };  
  while(true){
    //Try including vertex pos;
    in[pos]=true;
    for(unsigned int j=0;j<(vertices+pos)->size;j++){
      unsigned int e=(vertices+pos)->elements[j];
      if(order[e]==1){
	//Edge e is a loop containing just this index;
	in[pos]=false;
	j=(vertices+pos)->size;
	//don't bother testing remaining edges.
      };
    };
    if(in[pos]){
      //can add vertex pos
      if(pos+1>=nv){
	//reached end of vertices.
	ans=ans+2;
	//could include or exclude vertex n.
	for(pos--;pos>0&&!in[pos];pos--);
	//skip past all impossible positions.
	if(!in[pos]){
	  //Finished.
	  return ans;
	};
	//remove vertex pos, and resume.
	in[pos]=false;
	for(unsigned int j=0;j<(vertices+pos)->size;j++){
	  unsigned int e=(vertices+pos)->elements[j];
	    order[e]++;
	};	
	pos++;
      }else{
	for(unsigned int j=0;j<(vertices+pos)->size;j++){
	  unsigned int e=(vertices+pos)->elements[j];
	  order[e]--;
	};
	pos++;
      };
    }else{
      //can't add vertex pos
      if(pos+1>=nv){
	//reached end of vertices.
	ans=ans+1;
	for(pos--;pos>0&&!in[pos];pos--);
	//skip past all impossible positions.
	if(!in[pos]){
	  //Finished.
	  return ans;
	};
	//remove vertex pos, and resume.
	in[pos]=false;
	for(unsigned int j=0;j<(vertices+pos)->size;j++){
	  unsigned int e=(vertices+pos)->elements[j];
	    order[e]++;
	};	
	pos++;
      }else{
		pos++;
      };      
    };
  };
};

extern "C"{

  void count_indep_hyper_exact(double *answer,const int* nv,const int* ne,const int* orders,const int *edges){
    hypergraph h(*ne,*nv);
    h.read(orders,edges);
    *answer=h.countindep_exact();
  };


  void count_indep_hyper_approx(double *answer,const int* nv,const int* ne,const int* orders,const int *edges,const int *nsamp){
    GetRNGstate();
    hypergraph h(*ne,*nv);
    h.read(orders,edges);
    *answer=h.countindep(*nsamp);
    PutRNGstate();
};

  
};
  
