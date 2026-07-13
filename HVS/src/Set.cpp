#include "Set.h"
#include <iostream>
using namespace std;
using namespace HVS;
#include <stdlib.h>
#include <math.h>
#include <string.h>

unsigned int set::numulst=0;
unsigned int set::maxcard=0;


unsigned int set::listSize() const {
  unsigned int ans=2;
  for(unsigned long *p=v;p<v+numulst;p++){
    for(unsigned long t=*p;t;t^=(t&-t)){
      ans++;
    };
  };  
  return ans;
};

void set::toList(unsigned int *&l) const {
  for(unsigned int i=0;i<numulst;i++){
    unsigned long a;
    for(unsigned long t=*(v+i);t;t^=a){
      a=t&-t;
      (*(l++))=getuniquepos(a)+i*SUL;
    };
  };  
  *(l++)=1;
  *(l++)=0;
};

// ostream& operator <<(ostream &out,const HVS::set &s){
//   for(unsigned int i=0;i<set::numulst;i++){
//     unsigned long a;
//     for(unsigned long t=*(s.v+i);t;t^=a){
//       a=t&-t;
//       out<<getuniquepos(a)+i*SUL<<" ";
//     };
//   };  
//   //  out<<"\n";
//   return out;
// };
// 
