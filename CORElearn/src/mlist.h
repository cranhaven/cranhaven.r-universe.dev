#if !defined(MLIST_H)
#define MLIST_H

#include <cstdlib>
#include "general.h"
#include "error.h"


template <class T> class mlistNode {
public:
	T value ;
	mlistNode<T> *next ;
	mlistNode() { next=0 ;}
	mlistNode(T &val) { value = val ; next=0 ;}
};

template <class T> class mlist {
   mlistNode<T> *pfirst, *plast ;
   void destroy(mlistNode<T> *n);
   int len(mlistNode<T> *n) ;
   
public:
   mlist() { pfirst = plast = 0 ;}
   ~mlist() { destroy() ; } 
   void destroy();
   void copy(mlist<T> &source);
   mlist<T>& operator=(mlist<T> &source) ;
   int len() { return len(pfirst) ;  }
   void add(T &value) ;
   void addEnd(T &value) ;
   mlistNode<T>* first() {  return pfirst ;   }
   mlistNode<T>* next(mlistNode<T> *it) ;
   T& getValue(mlistNode<T> *it) {	return it->value; }
   int findPos(T &e) ;
   
};

template<class T> void mlist<T>::destroy(){
	   destroy(pfirst); 
	   pfirst = plast = 0;
  }
 template<class T> void mlist<T>::destroy(mlistNode<T> *n){
	   mlistNode<T> *t ;
	   while (n) {
		   t = n ;
		   n = n->next ;
		   delete t ;
	   }
  }
 template<class T> void mlist<T>::copy(mlist<T> &source){
	   destroy() ;
	   for (mlistNode<T> *it = source.first() ; it != 0 ; it=source.next(it)) 
		   addEnd(it->value);
  }
 template<class T> mlist<T>& mlist<T>::operator=(mlist<T> &source) {
	 copy(source) ;
	 return *this ;
 }
 
 template<class T> int mlist<T>::len(mlistNode<T> *n) {
	   int ln = 0 ;
	   while (n) {
		   ++ln ;
		   n = n ->next ;
	   }
	   return ln;
  }
 template<class T> int mlist<T>::findPos(T &e) {
	   int pos = 0 ;
	   mlistNode<T> *t = pfirst ;
	   while (t) {
		   ++pos ;
		   if (t->value.compareTo(e)==0)
			   return pos ;
		   t = t ->next ;
	   }
	   return 0;
  }
 template<class T> void mlist<T>::add(T &value) {
	   if (pfirst == 0) {
		   pfirst = plast = new mlistNode<T>(value) ;
	   }
	   else {
		   mlistNode<T> *temp = new mlistNode<T>(value) ;
		   temp->next = pfirst ;
		   pfirst = temp ;
	   }
  }
 template<class T> void mlist<T>::addEnd(T &value) {
	   if (pfirst == 0) {
		   pfirst = plast = new mlistNode<T>(value) ;
	   }
	   else {
		   mlistNode<T> *temp = new mlistNode<T>(value) ;
		   plast->next = temp ;
		   plast= temp ;
	   }
  }

 template<class T> mlistNode<T>* mlist<T>::next(mlistNode<T> *it) {
	   if (it==0 || it->next==0)
	     return 0 ;
	   else return it->next ;
  }
  
#endif
