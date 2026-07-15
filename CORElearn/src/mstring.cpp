#include <cstdlib>
#include <cstring>

#include "general.h"
#include "error.h"
#include "utils.h"
#include "mstring.h"

using namespace std ;

using namespace std ;

    void mstring::destroy () {
    	if (value) {
    		delete [] value ;
    		value = 0 ;
    	}
    }

    int mstring::len() const {
    	if (value)
    		return (int)strlen(value) ;
    	else return 0 ;
    }
    void mstring::copy(const char *cp) {
       destroy() ;
       if (cp)
         strcpy(value=new char[strlen(cp)+1], cp) ;
    }
    void mstring::copy(const mstring &cp) {
       destroy() ;
       if (cp.value)
   	      strcpy(value=new char[strlen(cp.value)+1], cp.value) ;
    }
    // copies everything from fromIdx forward
    void mstring::copyFrom(const mstring &cp, int fromIdx) {
       destroy() ;
       if (cp.value != 0 && fromIdx < cp.len())
   	     strcpy(value=new char[cp.len()+1-fromIdx], cp.value+fromIdx) ;
    }

    mstring& mstring::operator=(const mstring &Source)
    {
       copy(Source) ;
       return *this ;
    }
    mstring& mstring::operator=(const char *Source)
    {
       copy(Source) ;
       return *this ;
    }
    int mstring::compareTo(const mstring &Source) const {
    	return strcmp(value, Source.value);
    }
    void mstring::trimWhite(){
    	::trimWhite(this->value);
    }
    void mstring::append(const char *val) {
    	mstring app(val) ;
    	append(app) ;
    }
    void mstring::append(const mstring &val) {
       char *newValue = new char[this->len()+ val.len()+1];
   	   if (value)
   		   strcpy(newValue,value) ;
   	   else
   		   newValue[0]='\0';
   	   if (val.value)
   		   strcat(newValue, val.getConstValue()) ;
   	   destroy() ;
   	   value = newValue ;
    }
    char mstring::operator[] (int a) const  {
          #if defined(DEBUG)
             if ( a>len() || a<0)
                merror("mstring, operator []:","bounds check failed !") ;
          #endif
          return value[a] ;
    }
    int mstring::operator== (const mstring &Y) const
    {
      return (strcmp(value, Y.getConstValue())==0) ;
    }

    int mstring::operator== (const char *Y) const
    {
      return (strcmp(value, Y)==0) ;
    }

    char* mstring::unWrap() {
    	char *retValue = value ;
    	value = 0 ;
    	return retValue ;
    }

