#if !defined(C45READ_H)
#define C45READ_H

#include <cstdio>

#include "general.h"
#include "contain.h"
#include "mstring.h"
#include "mlist.h"

enum c45attrType {attrDisc=0, attrNum=1, attrDiscClass=2, attrNumClass=3, attrIgnore=4,attrOrdinal=5} ;

class c45Dsc {
public:
	mstring name;
	c45attrType aType ;
	mlist<mstring> values ;

	c45Dsc() { aType=attrDisc; }
	c45Dsc(c45Dsc &source){
		copy(source);
	}
	~c45Dsc() { destroy(); }
	void copy(c45Dsc &source) {
		name=source.name ;
		aType = source.aType ;
		values = source.values ;
	}
	void destroy() {
	  name.destroy() ;
	  values.destroy() ;
	}
};

class c45Data {
public:
	marray<int> discData ;
	marray<double> numData ;
	c45Data() {}
	c45Data(int noDisc, int noNum) { create(noDisc, noNum) ; }
	void create(int noDisc, int noNum) {
		discData.create(noDisc,NAdisc);
		numData.create(noNum,NAcont);
	}
	c45Data(c45Data &source){
		copy(source);
	}
	~c45Data() { destroy(); }
	void copy(c45Data &source) {
		discData=source.discData ;
		numData = source.numData ;
	}
	void destroy() {
	  discData.destroy() ;
	  numData.destroy() ;
	}
};

class c45read {
public:
	mlist<c45Dsc> attrs ;
	mlist<c45Data> dat ;
	int noDiscreteAttr, noNumericAttr, noDataCases, classIdx ;
	booleanT isRegression, isOrdinalClass ;

	static const int MaxLineLen = 65536 ;

	c45read() {
		noDiscreteAttr = noNumericAttr = noDataCases = 0 ;
		classIdx = -1;
		isRegression = isOrdinalClass = mFALSE ;
	}
	~c45read() {
		freeC45() ;
		noDiscreteAttr = noNumericAttr = noDataCases = 0 ;
		classIdx = -1;
	}

	int readC45names(FILE *from) ;
	int readC45data(FILE *from, const char *NAstr) ;
	int readValidLine(FILE *from, char *buf);
	bool getC45nameList(char *buf, mlist<mstring> &names) ;
	int readC45costs(FILE *from, mmatrix<double> &CostMatrix) ;

	void freeC45(void);
};

#endif /*C45READ_H_*/
