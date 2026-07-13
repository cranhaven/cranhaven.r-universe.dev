#include <limits.h>
#include <string.h>
#include <iosfwd>

#include "config.h"

typedef unsigned long int weighttype;

#define SUL (sizeof(unsigned long)*CHAR_BIT)

#ifndef LOOKUPMODULUS

#define LOOKUPMODULUS 131

#endif
namespace HVS{
  class set;
};

std::ostream& operator <<(std::ostream &out,const HVS::set &s);

namespace HVS{

static const unsigned int Mod131BitPos[131]={131,0,1,72,2,46,73,96,3,14,47,56,74,18,97,118,4,43,15,35,48,38,57,23,75,92,19,86,98,51,119,29,5,128,44,12,16,41,36,90,49,126,39,124,58,60,24,105,76,62,93,115,20,26,87,102,99,107,52,82,120,78,30,110,6,64,0,71,45,95,13,55,17,117,42,34,37,22,91,85,50,28,127,11,40,89,125,123,59,104,61,114,25,101,106,81,77,109,63,70,94,54,116,33,21,84,27,10,88,122,103,113,100,80,108,69,53,32,83,9,121,112,79,68,31,8,111,67,7,66,65};



#define CONCATENATE(a,b,c) CONCATENATE2(a,b,c)
#define CONCATENATE2(a,b,c) a##b##c
#define LOOKUPARRAY CONCATENATE(Mod,LOOKUPMODULUS,BitPos)

inline unsigned int getuniquepos(unsigned long v){
  return LOOKUPARRAY[v%LOOKUPMODULUS];
};

inline unsigned int getleastpos(unsigned long v){
  return getuniquepos(v&-v);
};

class setlist;
class setlistelt;

class set{
//Use bitfields for maximal efficiency.
  unsigned long *v;
public:
  static unsigned int numulst;
  static unsigned int maxcard;
  static unsigned int setmaxcard(unsigned int mc){unsigned int ans=maxcard;maxcard=mc;numulst=mc/SUL+1;return(ans);};
  void operator =(const set &x){if(v==NULL){v=new unsigned long[numulst];};for(unsigned int i=0;i<numulst;i++){*(v+i)=*(x.v+i);};};
  bool operator ==(const set &x){for(unsigned int i=0;i<numulst;i++){if(*(v+i)!=*(x.v+i)){return false;};};return true;};
  bool increment(){unsigned int i=0;for(;*(v+i)==(unsigned)-1;*(v+i++)=0);(*(v+i))++;return(i<(numulst-1)||((*(v+i))&((((unsigned long)1)<<(maxcard-(numulst-1)*SUL))-1)));};
  set(const set &x){v=new unsigned long[numulst];for(unsigned int i=0;i<numulst;i++){*(v+i)=*(x.v+i);};};
  set():v(NULL){};
  set(unsigned int m);
  //  set(unsigned int m,unsigned int *mem);
  set(unsigned int m,unsigned long *&mem);
  ~set(){if(v!=NULL){delete[] v;};v=NULL;};
  void assign(unsigned int m,unsigned int *mem);
  void assign(unsigned int m,unsigned int *mem,unsigned int step);
  void assign(unsigned int m);
  void copy_upto(unsigned int i,const set &s);
  void use(set s){v=s.v;};
  inline bool is_null(){return v==NULL;};
  int ni(unsigned int i) const;
  void setunion(const set &s);
  set unionwith(const set &s) const;
  set intersect(const set &s)const;
  set minus(const set &s)const;
  set complement()const;
  void setminus(const set &s);
  void intersectto(const set &s,set &o)const;
  unsigned long intersecttotest(const set &s,set &o)const;
  unsigned long intersecttotest(const set &s,set &o,unsigned int first)const;
  void minusto(const set &s,set &o)const;
  void minusto(const set &s,set &o,unsigned int first)const;
  void insert(unsigned int i);
  void writeto(set &to) const;
  void writeto(unsigned long *&mem) const;
  void writetoandinsert(unsigned long *&mem,unsigned int ins) const;
  unsigned int card() const ;
  unsigned int getfirst() const;
  unsigned int getfirstcomp() const;
  unsigned int getfirst(unsigned int &j) const;
  unsigned int getelement(unsigned int i) const;
  void getnext(unsigned int &i) const;
  unsigned int listSize() const;
  void toList(unsigned int *&l) const;
  bool empty() const;
  bool disjoint(const set &s) const;
  unsigned int is_subset(const set &A) const;
  bool containscomp(const set &A) const;
  void setempty();
  void setall();
  void remove(unsigned int ind);
  void flip(unsigned int ind);
  void clearmem();
  //  friend setlist BronKerbosh(set among,set *neighbours,long unsigned int *mem);
  friend void BronKerbosh(set &among,weighttype tot,unsigned int pivot);
  //  friend std::ostream& std::operator <<(std::ostream &out,const HVS::set &s);
  friend bool modlexless(const set& a,const set & b);
  friend unsigned int hash(const set &A);
};

inline set set::complement()const{
  set ans(maxcard);
  for(unsigned int i=0;i<numulst-1;i++){
    *(ans.v+i)=~*(v+i);
  };
  unsigned long spare=(((unsigned long)1)<<(set::maxcard%SUL))-1;
  *(ans.v+numulst-1)=~*(v+numulst-1)&spare;
  return ans;
};


//extern unsigned int set::numulst;
//extern unsigned int set::maxcard;

inline bool set::containscomp(const set &A) const{
  //returns true if union is whole set;
  for(unsigned int i=0;i<set::numulst-1;i++){
    if(!~(*(v+i)|*(A.v+i))){
      return false;
    };
  };
  unsigned long spare=(((unsigned long)1)<<(set::maxcard%SUL))-1;
  return((*(v+(numulst-1))|*(A.v+(numulst-1)))==spare);
};

inline unsigned int set::card() const {
  unsigned int ans=0;
  for(unsigned long *p=v;p<v+numulst;p++){
    for(unsigned long t=*p;t;t^=(t&-t)){
      ans++;
    };
  };  
  return ans;
};

inline unsigned int set::getelement(unsigned int i) const {
  unsigned int pos=0;
  for(unsigned int b=0;b<numulst;b++){
    unsigned long *p=v+b;
    for(unsigned long t=*p;t;t^=(t&-t)){
      pos++;
      if(pos>i){
	return(b*SUL+getleastpos(t));
      };
    };
  };  
  return maxcard+1;//Not enough elements in set.
};



inline unsigned int getcard(unsigned long *v){
  unsigned int ans=0;
  for(unsigned long *p=v;p<v+set::numulst;p++){
    for(unsigned long t=*p;t;t^=(t&-t)){
      ans++;
    };
  };  
  return ans;
};


inline bool set::disjoint(const set &s) const{
  for(unsigned int i=0;i<numulst;i++){
    if(*(v+i)&*(s.v+i)){
      return 0;
    };
  };
  return 1;
};


inline void set::setunion(const set &s){
  //forms the union of the current set with s
  for(unsigned int i=0;i<numulst;i++){
    *(v+i)|=*(s.v+i);
  };
};

inline set set::unionwith(const set &s) const{
  //forms the union of the current set with s
  set ans(maxcard);
  for(unsigned int i=0;i<numulst;i++){
    *(ans.v+i)=*(v+i)|*(s.v+i);
  };
  return ans;
};


inline unsigned int set::is_subset(const set &A) const{
  unsigned int ans=0;
  for(unsigned int i=0;i<numulst;i++){
    ans|=*(v+i)&~(*(A.v+i));
  };
  return !ans;
};


inline void set::writeto(set &s) const {
  unsigned long* t=s.v;
  this->writeto(t); // dangerous. Assumes s is already created.
};


inline void set::writeto(unsigned long *&mem) const {
  for(unsigned int i=0;i<numulst;i++){
    *(mem++)=*(v+i);
  };
};

inline void set::writetoandinsert(unsigned long *&mem,unsigned int ins) const {
  unsigned int a=ins/SUL;
  unsigned int b=((unsigned long int)1)<<(ins%SUL);
  for(unsigned int i=0;i<a;i++){
    *(mem++)=*(v+i);
  };
  *(mem++)=*(v+a)|b;
  for(unsigned int i=a+1;i<numulst;i++){
    *(mem++)=*(v+i);
  };
};

//
inline set::set(unsigned int m,unsigned long *&mem):v(mem){//use previously allocated memory to create a new set.
  //  maxcard=m;
  //v=mem;
  mem+=numulst;
};

inline void set::clearmem(){
  if(v!=NULL){
    delete[] v;
    v=NULL;
  };
};


inline void set::remove(unsigned int i){
  *(v+i/SUL)&=~(((long unsigned int)1)<<(i%SUL));
};

inline void set::flip(unsigned int i){
  *(v+i/SUL)^=(((long unsigned int)1)<<(i%SUL));
};

inline bool set::empty() const {
  unsigned long e=*v;
  for(unsigned int i=1;e==0&&i<numulst;i++){
    e|=*(v+i);
  };
  return !e;
};

inline void set::setempty(){
  for(unsigned int i=0;i<numulst;i++){
    *(v+i)=0;
  };
};

inline void set::setall(){
  for(unsigned int i=0;i<numulst;i++){
    *(v+i)=-1;
  };
  (*(v+maxcard/SUL))=((((long unsigned int)1)<<maxcard%SUL)-1);
};

inline void set::assign(unsigned int m){
  if(v!=NULL){
    delete[] v;    
  };
  v=new unsigned long[numulst];
  //  maxcard=m;
  memset(v,0,numulst*sizeof(unsigned long));
};


inline void set::assign(unsigned int m,unsigned int *mem){
  if(v!=NULL){
    delete[] v;    
  };
  v=new unsigned long[numulst];
  //  maxcard=m;
  unsigned int *pos=mem;
  unsigned long *w=v;
  for(unsigned int i=0;i<m/SUL;i++){
    *w=0;
    /*    for(unsigned int j=0;j<SUL;j++){
      (*w)|=((unsigned long int)*(pos++))<<j;
    };*/
    /*    unsigned long int p=1;
    for(;p;p<<=1){
      (*w)|=p*((long unsigned int)((*(pos++))>0));
      };*/
    unsigned long int p=1;
    for(;p;p<<=1){
      (*w)|=p*(*(pos++));
    };
    w++;
  };
  *w=0;
  unsigned long int p=1;
  unsigned long int msk=((((long unsigned int)1)<<(maxcard%SUL))-1);
  for(;(p&msk);p=p<<1){
    (*w)|=p*((long unsigned int)((*(pos++))>0));
  };
};

inline void set::assign(unsigned int m,unsigned int *mem,unsigned int step){
  if(v!=NULL){
    delete[] v;    
  };
  v=new unsigned long[numulst];
  //  maxcard=m;
  unsigned int *pos=mem;
  unsigned long *w=v;
  for(unsigned int i=0;i<m/SUL;i++){
    *w=0;
    /*    for(unsigned int j=0;j<SUL;j++){
      (*w)|=((unsigned long int)*(pos++))<<j;
    };*/
    /*    unsigned long int p=1;
    for(;p;p<<=1){
      (*w)|=p*((long unsigned int)((*(pos++))>0));
      };*/
    unsigned long int p=1;
    for(;p;p<<=1){
      (*w)|=p*(*(pos));
      pos+=step;
    };
    w++;
  };
  *w=0;
  unsigned long int p=1;
  unsigned long int msk=((((long unsigned int)1)<<(maxcard%SUL))-1);
  for(;(p&msk);p=p<<1){
    (*w)|=p*((long unsigned int)((*(pos))>0));
    pos+=step;
  };
};



inline unsigned int set::getfirst() const {
  unsigned int j=0;
  for(;*(v+j)==0&&j<numulst-1;j++);
  if(*(v+j)){
    /*    unsigned int k=0;
    for(unsigned long p=1;(p&*(v+j))==0;p=p<<1){
      k++;
      };*/
    return j*SUL+getleastpos(*(v+j));
  }else{
    return maxcard;
  };
};

inline void set::copy_upto(unsigned int i,const set &s){
  unsigned int p=0;
  for(;p<i/SUL;p++){
    *(v+p)=*(s.v+p);
  };
  *(v+p)=(*(v+p)&~((((long unsigned int)1)<<i%SUL)-1))|*(s.v+p);
};

inline unsigned int set::getfirstcomp() const {
  unsigned int j=0;
  for(;*(v+j)==(unsigned)-1&&j<numulst-1;j++);
  if(j<numulst-1||~*(v+j)&((((long unsigned int)1)<<maxcard%SUL)-1)){
    return j*SUL+getuniquepos((*(v+j)^(*(v+j)+1))&(*(v+j)+1));
  }else{
    return maxcard;
  };
};


inline unsigned int set::getfirst(unsigned int &j) const {
  for(;*(v+j)==0&&j<numulst-1;j++);
  if(*(v+j)){ //This test isn't necessary, but seems to speed it up
    /*    unsigned int k=0;
    for(unsigned long p=1;(p&*(v+j))==0;p=p<<1){
      k++;
      };*/
    return j*SUL+getleastpos(*(v+j));
  }else{
    return maxcard; //This statement almost never gets executed, but seems to speed up code???
  };
};

inline void set::getnext(unsigned int &i) const{
  unsigned int l=i/SUL;
  unsigned long int b=(((long unsigned )1<<i%SUL)<<1)-1;
  if(*(v+l)&~b){
    i=getleastpos(*(v+l)&~b)+l*SUL;
    return;
  }else{
    unsigned int j;
    for(j=l+1;j<numulst&&!(*(v+j));j++);
    if(j<numulst){
      i=getleastpos(*(v+j))+j*SUL;
      return;
    }else{
      i=maxcard;
      return;
    };
  };
};

inline set::set(unsigned int m){
  //  maxcard=m;
  if(m>0){
    v=new unsigned long[numulst];
    for(unsigned int i=0;i<numulst;i++){
      *(v+i)=0;
    };
  }else{
    v=NULL;
  };
};


inline int set::ni(unsigned int i) const {
  return (*(v+i/SUL)>>(i%SUL))&1;
};

inline set set::intersect(const set &s)const{
  unsigned int min=maxcard;
  set ans(min);
  for(unsigned int i=0;i<numulst;i++){
    *(ans.v+i)=*(v+i)&*(s.v+i);
  };
  return ans;
};

inline set set::minus(const set &s)const{
  set ans(maxcard);
  unsigned int i=0;
  for(;i<numulst;i++){
    *(ans.v+i)=*(v+i)&(~*(s.v+i));
  };
  return ans;
};

inline void set::setminus(const set &s){
  unsigned int i=0;
  for(;i<numulst;i++){
    *(v+i)&=(~*(s.v+i));
  };
};

inline void set::intersectto(const set &s,set &o)const{
  //  o.maxcard=maxcard;
  for(unsigned int i=0;i<numulst;i++){
    *(o.v+i)=*(v+i)&*(s.v+i);
  };
};

inline unsigned long set::intersecttotest(const set &s,set &o)const{
  //  o.maxcard=maxcard;
  unsigned long test=0;
  for(unsigned int i=0;i<numulst;i++){
    *(o.v+i)=*(v+i)&*(s.v+i);
    test|=*(o.v+i);
  };
  return test;
};

inline unsigned long set::intersecttotest(const set &s,set &o,unsigned int first)const{
  //  o.maxcard=maxcard;
  unsigned long test=0;
  for(unsigned int i=first;i<numulst;i++){
    *(o.v+i)=*(v+i)&*(s.v+i);
    test|=*(o.v+i);
  };
  return test;
};


inline void set::minusto(const set &s,set &o)const{
  //  o.maxcard=maxcard;
  unsigned int i=0;
  for(;i<numulst;i++){
    *(o.v+i)=*(v+i)&(~*(s.v+i));
  };
  //  for(;i<numulst;i++){
  //    *(o.v+i)=*(v+i);
  //  };
};

inline void set::minusto(const set &s,set &o,unsigned int first)const{
  //  o.maxcard=maxcard;
  unsigned int i=first;
  for(;i<numulst;i++){
    *(o.v+i)=*(v+i)&(~*(s.v+i));
  };
  //  for(;i<numulst;i++){
  //    *(o.v+i)=*(v+i);
  //  };
};



inline void set::insert(unsigned int i){
  *(v+i/SUL)|=(((long unsigned int)1)<<(i%SUL));
};



inline bool modlexless(const set& a,const set & b){
  //returns true if a contains b or is lexicographically earlier.
  //elements are ordered in reverse order.
  for(unsigned int i=set::numulst-1;i>=0;i--){
    if(*(a.v+i)<*(b.v+i)){
      return true;
    }else if(*(a.v+i)>*(b.v+i)){
      return false;
    };
  };
  return false;
};

}
