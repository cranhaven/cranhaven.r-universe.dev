/* Method for counting independent sets of a graph.

 */


#include <cstdlib>
#include <iostream>
#include <cmath>
#include "Set.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>




using namespace HVS;
//using namespace std;

using set=HVS::set;

unsigned int npowers=0;
double *powers;

inline unsigned int random(unsigned int n){
  //generates a random number in the range 0 to n-1.

  double ru=unif_rand();
  return(floor(ru*n));
};

class graph{ //a graph labelled by real numbers
  unsigned int n;
  double *edges;
public:
  graph(){n=0;edges=NULL;}
  graph(unsigned int no):n(no){edges=new double[n*n];}
  graph(unsigned int no,const double *ed):n(no){edges=new double[n*n];for(unsigned int i=0;i<n*n;i++){edges[i]=ed[i];};};
  void set_size(unsigned int no){n=no;edges=new double[n*n];};
  void random();
  double indep_prob(double c);
  double indep_inc_sample2(double c,unsigned int nsamp);
  double select_cutoff(double thresh,unsigned int batch_size=100,unsigned int nbatch=100,double *cvals=NULL,double *siz=NULL,double* is=NULL,double* ir=NULL);
  graph sort(double c);
  friend class setgraph;
};

void quicksort(unsigned int n,unsigned int* vals,unsigned int* to,unsigned int *indices,unsigned int* indicesto){
  //n is the length of the list
  //vals is the values
  //indices are the indices
  if(n==0){
    return;
  }else if(n==1){
    *to=*vals;
    *indicesto=*indices;
    return;
  };
  
  unsigned int separator=vals[0];
  unsigned int* lowind=to;
  unsigned int* lowindind=indicesto;
  unsigned int* highind=to+n-1;
  unsigned int* highindind=indicesto+n-1;
  for(unsigned int i=1;i<n;i++){
    if(vals[i]<separator){
      *(lowind++)=vals[i];
      *(lowindind++)=indices[i];
    }else{
      *(highind--)=vals[i];
      *(highindind--)=indices[i];
    };
  };
  unsigned int nlow=lowind-to;
  *lowind=separator;
  *lowindind=indices[0];
  quicksort(nlow,to,vals,indicesto,indices);
  quicksort(n-1-nlow,lowind+1,vals+nlow+1,indicesto+nlow+1,indices+nlow+1);  
};


void quicksort(unsigned int n,double * vals,double* to){
  //n is the length of the list
  //vals is the values
  if(n==0){
    return;
  }else if(n==1){
    *to=*vals;
    return;
  };
  
  double separator=vals[0];
  double* lowind=to;
  double* highind=to+n-1;
  for(unsigned int i=1;i<n;i++){
    if(vals[i]<separator){
      *(lowind++)=vals[i];
    }else{
      *(highind--)=vals[i];
    };
  };
  unsigned int nlow=lowind-to;
  *lowind=separator;
  quicksort(nlow,to,vals);  
  quicksort(n-1-nlow,lowind+1,vals+nlow+1);
  vals[nlow]=*lowind;
};




graph graph::sort(double c){
  // count vertex degrees
  unsigned int *degree=new unsigned int[n];
  for(unsigned int i=0;i<n;i++){
    degree[i]=0;
    for(unsigned int j=0;j<n;j++){
      degree[i]+=((*(edges+i*n+j))>=c);
    };
  };

  //Sort degree sequence

  unsigned int *index=new unsigned int[n];
  for(unsigned int i=0;i<n;i++){
    index[i]=i;
  };
  unsigned int *temp=new unsigned int[n];
  unsigned int *tempind=new unsigned int[n];

  quicksort(n,degree,temp,index,tempind);

  graph ans(n);

  
  for(unsigned int i=0;i<n;i++){
    for(unsigned int j=0;j<n;j++){
      *(ans.edges+i*n+j)=*(edges+tempind[i]*n+tempind[j]);
    };
  };	

  return ans;
  
};

class setgraph{
  unsigned int n;
  set *edges;
public:
  setgraph(const graph&G,double c);
  double indep_prob();    
  double indep_prob(const set &remain);   
  double indep_prob(const set & remain,set *temp);
  double indep_inc_sample2(unsigned int nsamp);
};

setgraph::setgraph(const graph&G,double c){
  n=G.n;
  set::setmaxcard(n);
  edges=new set[n];
  unsigned int *temp=new unsigned int[n];
  for(unsigned int i=0;i<n;i++){
    for(unsigned int j=0;j<n;j++){
      *(temp+j)=*(G.edges+i*n+j)<c;
    };
    edges[i].assign(n,temp);
  };
};


double setgraph::indep_prob(){
  set empty(n);
  set all=empty.complement();

  // remove all singletons 
  for(unsigned int i=0;i<n;i++){
    if(edges[i].ni(i)){
      all.remove(i);
    };
  };

  if(all.empty()){
    return 1;
  };

  unsigned int ac=all.card();
  if(npowers<ac){
    if(powers){
      delete[] powers;
    };
    
    powers=new double[ac+1];
    powers[0]=1;
    for(unsigned int i=0;i<ac;i++){
      powers[i+1]=2*powers[i];
    };    
    npowers=ac;
  };
  
  set *temp=new set[n];
  for(unsigned int i=0;i<n;i++){
    temp[i].assign(n);
  };
  double ans=this->indep_prob(all,temp);
  for(unsigned int i=0;i<n;i++){
    temp[i].clearmem();
  };
  delete[] temp;
  
  return ans;
};    

double setgraph::indep_prob(const set & remain,set *temp){
  //takes a vector of sets as temporary working space to avoid
  //allocating and deleting memory.
  
  double ans1;
  unsigned int i=remain.getfirst();
  remain.writeto(*temp);
  temp->remove(i);
  temp->setminus(edges[i]);
  unsigned int j=temp->getfirst();

  //If the graph is empty, then there are 2^m independent subsets
  for(;j<set::maxcard&&temp->disjoint(edges[j]);temp->getnext(j));
  if(j==set::maxcard){
    ans1=powers[temp->card()];
    //    ans1=ldexp((double)1,temp->card());
  }else{
    ans1=this->indep_prob(*temp,temp+1);
  };
  // ans1 is the conditional probability given that the first element is included.
  
  remain.writeto(*temp);

  temp->remove(i);

  //If the graph is empty, then there are 2^m independent subsets
  for(j=temp->getfirst();j<set::maxcard&&temp->disjoint(edges[j]);temp->getnext(j));
    if(j==set::maxcard){
    ans1+=powers[temp->card()];
    //    ans1+=ldexp((double)1,temp->card());
  }else{
    ans1+=this->indep_prob(*temp,temp+1); 
  };
  return ans1;
};    




double setgraph::indep_prob(const set & remain){   
  //Slower original verion. (Not used)

  double ans1;
  unsigned int i=remain.getfirst();
  if(i==set::maxcard){// shouldn't happen
    return 0;
  };
  set testrem(remain);
  testrem.remove(i);
  testrem.setminus(edges[i]);
  unsigned int j=testrem.getfirst();
  for(;j<set::maxcard&&testrem.intersect(edges[j]).empty();testrem.getnext(j));
  if(j==set::maxcard){
    ans1=pow(2,testrem.card());
  }else{
    ans1=this->indep_prob(testrem);
  };
  // ans1 is the conditional probability given that the first element is included.
  
  remain.writeto(testrem);

  testrem.remove(i);

  for(j=testrem.getfirst();j<set::maxcard&&testrem.intersect(edges[j]).empty();testrem.getnext(j));
  
  if(j==set::maxcard){
    ans1+=pow(2,testrem.card());
  }else{
    ans1+=this->indep_prob(testrem); 
  };
  
  testrem.clearmem();
  return ans1;
};    



void graph::random(){
  for(unsigned int i=0;i<n;i++){
    *(edges+i*(n+1))=2;
    for(unsigned int j=i+1;j<n;j++){
      *(edges+i*n+j)=unif_rand();//   ((double)rand())/RAND_MAX;
      *(edges+j*n+i)=*(edges+i*n+j);
    };
  };
}

double graph::indep_prob(double c){
  return(setgraph(*this,c).indep_prob());
};


inline unsigned int randn(unsigned int max){ //repeat of random function defined above.
  unsigned int limit=RAND_MAX-RAND_MAX%max;
  unsigned int ans;
  for(ans=rand();ans>limit;ans=rand());
  return ans%max;
};

 
double setgraph::indep_inc_sample2(unsigned int nsamp){
  //uses random incremental sampling to estimate the number of independent sets.
  
  //Create a list of sets to sample and set then all full.
  set current(n);
  double ans=0;
  set start(n);
  for(unsigned int j=0;j<n;j++){
    if(!(edges[j].ni(j))){
      start.flip(j);
    };
  };
  unsigned int startchoice=start.card();
  if(startchoice==0){
    return 1;
  };
  for(unsigned int j=0;j<nsamp;j++){
    current=start;
    double weight=1;
    ans+=weight;
    unsigned int nchoice=startchoice;
    for(unsigned int i=0;i<n&&nchoice;i++){
      weight*=((double)nchoice)/(i+1);
      if(nchoice){
	ans+=weight;
	unsigned int r=::random(nchoice);
	unsigned int addelt=current.getelement(r);
	current.remove(addelt);
	current.setminus(edges[addelt]);
      };
      nchoice=current.card();
    };
  };
  return ans/nsamp;
};

double graph::indep_inc_sample2(double c,unsigned int nsamp){
  return(setgraph(*this,c).indep_inc_sample2(nsamp));
};


inline bool test_convex(const double* x,const double *y,const unsigned int *m,unsigned int a,unsigned int b,unsigned int c){
  //determines whether the points (x1,y1), (x2,y2) and (x3,y3) are convex up
  // assuming x1<x2<x3
  return (y[m[b]]-y[m[a]])*(x[m[c]]-x[m[b]])>(x[m[b]]-x[m[a]])*(y[m[c]]-y[m[b]]);
};



unsigned int convex_hull(unsigned int n,unsigned int *m,const double *x,const double *y){
  //Computes the indices of the convex hull.
  //return value is number of blocks.
  //Seems to take slightly under 3 seconds for 100,000,000 points.
  
  if(n<3){ // base case - if two or fewer points, then all points on
	   // convex hull.
    for(unsigned int i=0;i<n;i++){
      m[i]=i;
    };
    return n;
  };

  
  //Find the convex hulls of each half.
  unsigned int mid=(n)/2;
  unsigned int f=convex_hull(mid+1,m,x,y);
  unsigned int s=convex_hull(n-mid,m+f-1,x+mid,y+mid);
  for(unsigned int j=0;j<s;j++){
    m[f-1+j]+=mid;
  };  
  
  if(test_convex(x,y,m,f-2,f-1,f)){
    return(f+s-1);
  };

  //Now remove all points not in convex hull
  unsigned int fl=f-2;
  unsigned int sf=f;
  for(bool changed=true;changed;){    
    changed=false;
    for(;fl>0&&!test_convex(x,y,m,fl-1,fl,sf);fl--){
      changed=true;
    };
    for(;sf+2<f+s&&!test_convex(x,y,m,fl,sf,sf+1);sf++){
      changed=true;
    };
  };
  
  if(sf+2==s+f){
    for(;fl>0&&!test_convex(x,y,m,fl-1,fl,sf);fl--);
  };
  for(unsigned int i=0;i<f+s-sf;i++){
    m[i+fl+1]=m[sf+i];
  };
  
  return f+s+fl-sf;    
};

inline bool test_convex_even(const double *y,const unsigned int *m,unsigned int a,unsigned int b,unsigned int c){
  //determines whether the points (m1,y1), (m2,y2) and (m3,y3) are convex up
  // assuming m1<m2<m3
  return (y[m[b]]-y[m[a]])*(m[c]-m[b])>(m[b]-m[a])*(y[m[c]]-y[m[b]]);
};


unsigned int convex_hull(unsigned int n,unsigned int *m,const double *y){
  //Convex hull for evenly spaced x coordinates.
  //Computes the indices of the convex hull.
  //return value is number of blocks.
  //Seems to take slightly under 3 seconds for 100,000,000 points.

  if(n<3){ // base case - if two or fewer points, then all points on
	   // convex hull.
    for(unsigned int i=0;i<n;i++){
      m[i]=i;
    };
    return n;
  };

  
  //Find the convex hulls of each half.
  unsigned int mid=(n)/2;
  unsigned int f=convex_hull(mid+1,m,y);
  unsigned int s=convex_hull(n-mid,m+f-1,y+mid);
  for(unsigned int j=0;j<s;j++){
    m[f-1+j]+=mid;
  };  

  if(test_convex_even(y,m,f-2,f-1,f)){
    return(f+s-1);
  };

  //Now remove all points not in convex hull
  unsigned int fl=f-2;
  unsigned int sf=f;
  for(bool changed=true;changed;){    

    changed=false;
    for(;fl>0&&!test_convex_even(y,m,fl-1,fl,sf);fl--){
      changed=true;
    };
    for(;sf+2<f+s&&!test_convex_even(y,m,fl,sf,sf+1);sf++){
      changed=true;
    };
  };
  
  if(sf+2==s+f){
    for(;fl>0&&!test_convex_even(y,m,fl-1,fl,sf);fl--);
  };
  for(unsigned int i=0;i<f+s-sf;i++){
    m[i+fl+1]=m[sf+i];
  };
  
  return f+s+fl-sf;    
};




void isotonic(double *x,double *y,unsigned int n1,unsigned int n2,double *to){
  //x, y are divided into two blocks. x is sorted within each block.

  //merge two blocks.
  double *xtmp=new double[n1+n2];
  double *ytmp=new double[n1+n2];
  unsigned int i=0;
  unsigned int j=0;
  for(;i<n1&&j<n2;){
    if(x[i]<x[n1+j]){
      xtmp[i+j]=x[i];
      ytmp[i+j]=y[i];
      i++;
    }else{
      xtmp[i+j]=x[n1+j];
      ytmp[i+j]=y[n1+j];
      j++;
    };
  }
  for(;i<n1;i++){
    xtmp[i+j]=x[i];
    ytmp[i+j]=y[i];
  };
  for(;j<n2;j++){
    xtmp[i+j]=x[n1+j];
    ytmp[i+j]=y[n1+j];
  };
  for(i=0;i<n1+n2;i++){
    x[i]=xtmp[i];
    y[i]=ytmp[i];
  };

  //Now blocks are sorted.
  //calculate cumulative sums.
  //Take cumulative sums backwards for numerical stability.
  double *cs=new double[n1+n2];
  cs[n1+n2-1]=-y[n1+n2-1];
  for(unsigned int i=n1+n2-2;i>0;i--){
    cs[i]=cs[i+1]-y[i]; 
  };
  cs[0]=cs[1]-y[0];
  
  
  unsigned int *m=new unsigned int[n1+n2]; //locations of cumulative maxima
  unsigned int nbl=convex_hull(n1+n2,m,cs);
  
  for(unsigned int i=0;i+1<nbl;i++){
    double value=(cs[m[i+1]]-cs[m[i]])/(m[i+1]-m[i]);
    for(unsigned int j=m[i];j<=m[i+1];j++){
      to[j]=value;
    };
  };
};

unsigned int findindex(double *x,unsigned int n,double v){
  //Finds the index where the sorted list x is approximately equal to v.
  if(n<=1){
    return 0;
  };
  if(v>x[n/2]){
    return(n/2+findindex(x+n/2,n-n/2,v));
  }else{
    return(findindex(x,n/2,v));    
  };
};


double graph::select_cutoff(double thresh,unsigned int batch_size,unsigned int nbatch,double *cvals,double *siz,double* is,double* ir){
  //Finds the largest c such that the number of independent sets at
  //cut-off c is more than 2^tc.

  //n is the size of the graph.

  double *pvs=new double[n*n]; //sorted p-values
  double *pvstemp=new double[n*n];

  for(unsigned int i=0;i<n*n;i++){
    pvstemp[i]=edges[i];
  };

  quicksort(n*n,pvstemp,pvs);
  delete[] pvstemp;

  
  unsigned int ns=nbatch*batch_size;
  bool cvtmp=!cvals;
  bool sztmp=!siz;
  bool irtmp=!ir;
  bool istmp=!is;

  if(!cvals){
    cvals=new double[ns];
  };
  if(!siz){
    siz=new double[ns];
  };
  if(!ir){
    ir=new double[ns];
  };
  if(!is){
    is=new double[ns];
  };
  
  
  unsigned int min=0;
  unsigned int width=n*n;

  unsigned int *current=new unsigned int[n];
  unsigned int jj;

  for(unsigned int i=0;i<nbatch;i++){
    for(unsigned int j=0;j<batch_size;j++){
      if((unsigned int)min+(j*width)/batch_size){
	cvals[i*batch_size+j]=pvs[(unsigned int)min+(j*width)/batch_size];
      }else{
	cvals[i*batch_size+j]=0;
      };
      double c=cvals[i*batch_size+j];
      unsigned int ns=0;
      for(unsigned int k=0;k<n;k++){
	current[k]=(*(edges+k*(n+1))<c)?0:1;
	ns+=current[k];
      };
      
      unsigned int nchoice=ns;

    //    };
      double weight=1;
      is[i*batch_size+j]=1;
      for(unsigned int k=0;k<ns&&nchoice;k++){
	weight*=((double)nchoice)/(k+1);
	if(nchoice){
	  is[i*batch_size+j]+=weight;
	  unsigned int r=::random(nchoice); // Choose a random element
	  // to add to the set
	  //select the r th element from the available choices.
	  unsigned int addelt=0;
	  for(unsigned int count=0;count<=r;addelt++){
	    if(current[addelt]){
	      count++;
	    };
	  };
	  addelt--;
	  //remove this element
	  current[addelt]=0; 
	  nchoice--;
	  //remove all choices connected to the new element.
	  for(unsigned int l=0;l<n;l++){
	    if(current[l]&&*(edges+addelt*n+l)<c){
	      current[l]=0;
	      nchoice--;
	    };
	  };
	};
      };
      
      
    };
    //Now we have approximately counted the number of independent sets
    //for various cut-offs.
    isotonic(cvals,is,i*batch_size,batch_size,ir);

    unsigned int mx=0;
    jj=(i+1)*batch_size-1;
    for(double resid=-10; resid<0&&jj>0;jj--){
      siz[jj]=n-log2(ir[jj]); //sizing function
      resid=siz[jj]/(thresh*cvals[jj])-1;
      if(resid>-0.3&&!mx){
	mx=jj; //First index 
      };
    };
    jj++;
    //Now jj should be the last point at which the sizing function
    //exceeds the cut-off. mx is the point at which the estimates get
    //reasonably close.
    
    
    //Find where pvs equals cvals[j] and cvals[mx]
    unsigned int max=findindex(pvs,n*n,cvals[mx]);
    unsigned int eq=findindex(pvs,n*n,cvals[jj]);
    if(max<2*eq){
      min=0;
    }else{
      min=max-2*eq;
    };

    //Add some randomisation of points to ensure sufficient density of points.
    width=(max-min)*2/3+::random((max-min)*2/3);
    if(min+width>n*n){
      width=n*n-min;
    };
    if(width==0){
      width=batch_size;// Don't set width 0
    };
    
  };
  for(unsigned int jj=0; jj<ns;jj++){
    siz[jj]=n-log2(ir[jj]); //sizing function
  };
  delete[] current; 
  if(irtmp){
    delete[] ir;
  };
  if(istmp){
    delete[] is;
  };
  if(sztmp){
    delete[] siz;
  };
  double ans=cvals[jj];  

  if(cvtmp){
    delete[] cvals;
  };
  return ans;  
};

extern "C"{

  void count_indep(double *answer,const int *n, const double *G, const double *cutoff){
    GetRNGstate();
    unsigned int sz=*n;
    graph Gr(*n,G);
    *answer=Gr.indep_prob(*cutoff);
    PutRNGstate();
  };
  
  void count_indep_import(double *answer,const int *n, const double *G, const double *cutoff,const int *nsamp){
    GetRNGstate();
    unsigned int sz=*n;
    graph Gr(*n,G);
    *answer=Gr.indep_inc_sample2(*cutoff,(unsigned)*nsamp);
    PutRNGstate();
  };
  
  void select_cutoff(double *answer,const int *n, const double *G,const int *batchsize,const int *nbat,const double *threshold){
    GetRNGstate();
    graph Gr(*n,G);
    //    target_n=*n;
    unsigned int ns=(*nbat)*(*batchsize);
    *answer=Gr.select_cutoff(*threshold,*batchsize,*nbat,answer+1,answer+ns,answer+2*ns,answer+3*ns);
    PutRNGstate();
  };
  
  void count_indep_hyper_exact(double *answer,const int* nv,const int* ne,const int* orders,const int *edges);

  void count_indep_hyper_approx(double *answer,const int* nv,const int* ne,const int* orders,const int *edges,const int *nsamp);


  void find_convex_hull(int *answer,const int* n,const double *x,const double *y){
    unsigned int *ans=new unsigned int[(*n)+1];
    *answer=convex_hull(*n,ans, x, y);
    for(unsigned int i=0;i<(unsigned) *answer;i++){
      answer[i+1]=ans[i];
    };
    delete[] ans;
  };

  void isotonic_regression(double *answer,const int* n1,const int* n2,const double *x,const double *y){
    double* xcp=new double[*n1+*n2];
    double* ycp=new double[*n1+*n2];

    for(unsigned int i=0;i<(unsigned)(*n1+*n2);i++){
      xcp[i]=x[i];
      ycp[i]=y[i];
    };
    
    isotonic(xcp, ycp,*n1,*n2,answer);

    delete[] xcp;
    delete[] ycp;
  };



  
static const R_CMethodDef CEntries[] = {
    {"count_indep",              (DL_FUNC) &count_indep,              4},
    {"count_indep_hyper_approx", (DL_FUNC) &count_indep_hyper_approx, 6},
    {"count_indep_hyper_exact",  (DL_FUNC) &count_indep_hyper_exact,  5},
    {"count_indep_import",       (DL_FUNC) &count_indep_import,       5},
    {"select_cutoff",            (DL_FUNC) &select_cutoff,            6},
    //Functions for testing, delete in final package version
    {"find_convex_hull",         (DL_FUNC) &find_convex_hull,         4},
    {"isotonic_regression",         (DL_FUNC) &isotonic_regression,         5},
    {NULL, NULL, 0}
};

void R_init_HVS(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

};
