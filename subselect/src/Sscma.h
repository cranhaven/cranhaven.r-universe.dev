#ifndef SSCMA
#define SSCMA

#include <string>
#include <vector>
#include "ErrMReals.h" 

using namespace ErrMReals; 

namespace extendedleaps {


// class subsetdata;  // forward declaration

	/*  Extended Leaps special types  */

typedef  short int  vind;    /* Integer type used to index variables. Should be able to run
                                from 0 to the largest possible number of variables under comparison  */

// typedef  double     real;    // Floating point type used to represent real numbers.
typedef  ErrMReals::errmonitreal<double>	real;   // Floating point type used to represent real numbers	

enum pcskept {given,firstk}; 		 	/*  Enumeration used to identify the set of PCs considered by the GCD criterion  */
enum direction {forward,backward};	 	/*  Enumeration used to specifiy the specification of the criterion updates */
enum sscmares {optimal,limsrchbest,nomemory};	/*  Enumeration used to specifiy the result of the sscma search  */

	/*   Extended Leaps global declarations  */

/*  Constants shared by more than two files  */

const int MINIMZ   = 0;	/* Comparision criterion is to be minimized        */
const int MAXIMZ   = 1;	/* Comparision criterion is to be maximized        */


/*  Variables shared by more than two files  */
 
extern vind  p,q,fp,lp,mindim,lastvar,flst;  
extern vector<vind> actv;  
extern long unsigned ms;                                                     
extern vector<double>  lbnd,ubnd;
extern double maxtime,rtime,numtol;
extern pcskept pcsets;
extern std::string memmsg;

#ifdef COUNTING 
extern int cntg,fpcnt,fpcnt1;	/*  Floating point operation counters  */ 
#endif                

/*  Functions shared by more than two files   */

bool Leaps_Search(vind frwind0,vind bckind0,vind fvind,vind lvind,vind nvfrwd,vind nvbckwrd);	 
bool Rev_Leaps_Search(vind frwind0,vind bckind0,vind fvind,vind lvind,vind nvfrwd,vind nvbckwrd);
bool Forward_BreadthF_Search(vind frwind0,vind nvfrwd,vind fvind,vind lvind);

bool  Forward_DepthF_Search(vind frwind0,vind fvind,vind lvind,vind nvfrwd);

void crtwrksp(void);
void savfull(void);
void savfrst(void);
void msg(const std::string&);
void errmsg(const std::string&);

}

#endif
