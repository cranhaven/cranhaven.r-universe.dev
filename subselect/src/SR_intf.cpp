#include <R.h>
#include <iostream>

using namespace std; 

namespace extendedleaps {

void msg(const string& s)
{
	Rprintf("%s",s.c_str());
}

void errmsg(const string& s)
{
	Rf_error("%s",s.c_str());
}


}


