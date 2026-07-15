#if !defined(MSTRING_H)
#define MSTRING_H

#include "general.h"
#include "error.h"

class mstring {
	char *value ;

public:
	mstring() {value = 0 ;}
	mstring(const char* val) { value = 0 ; copy(val);	}
	mstring(const mstring &val) {value = 0 ; copy(val) ; }
    ~mstring(){ destroy() ;  }
    void destroy() ;
    int len() const ;
    void copy(const char *val);
    void copy(const mstring &val) ;
    void copyFrom(const mstring &val, int fromIdx) ;
    mstring& operator=(const mstring &Source) ;
    mstring& operator=(const char *Source) ;
    char *getValue() {	return value ;  }
    const char* getConstValue() const {	return value ;  }
    char* unWrap() ;
    bool isDefined() { 	return (value != 0);  }
    int compareTo(const mstring &Source) const ;
    void trimWhite();
    void append(const char *val) ;
    void append(const mstring &val) ;
    char operator[] (int a) const ;
    int operator== (const mstring &Y) const ;
    int operator== (const char *Y) const ;


};

#endif
