#include <cstdio>    // read and write functions
#include <cstring>    // dealing with attribute and value names
#include <cstdlib>
#include <cfloat>

#include "general.h"
#include "utils.h"
#include "error.h"
#include "dataStore.h"
#include "options.h"
#include "mstring.h"
#include "mlist.h"
#include "c45read.h"

using namespace std ;

char nameSeparators[] = "," ;
char attrSeparators[] = ":" ;
char datSeparators[] = ",|" ;
char ordinalIndicator[] = "[ordered]";

void c45read::freeC45(void)
{
	dat.destroy() ;
    attrs.destroy() ;
}


int c45read::readC45names(FILE *from) {
	char buf[MaxLineLen] ;
	mlist<mstring> names ;
	//mstring nameStr ;
	attrs.destroy() ;
    c45Dsc aDsc ;
    c45Dsc classDsc;

    isRegression = mFALSE ;
    isOrdinalClass = mFALSE ;
	char *token, *aTypeStr ;
	int strIdx, dscLen ;
	bool classFound = false ;
	noDiscreteAttr = noNumericAttr = 0 ;
	classIdx = -1 ; // for data conversion
    // first line contains comma separated class values or class name
	if (!readValidLine(from, buf) || !getC45nameList(buf, names)) {
       merror("Invalid format of class in names file.",buf) ;
       return 0;
	}
	int namesLen = names.len() ;
	if (namesLen < 1)
	  return 0 ;
	else if (namesLen==1) //className
       classDsc.name.copy(names.first()->value.getValue()) ;
	else {
		classDsc.values.copy(names) ;
    	classDsc.aType = attrDiscClass ;
		isRegression = mFALSE ;
	}

	names.destroy() ;

	// next lines contain attributes' descriptions
	do {
		if (!readValidLine(from, buf)){
			if (feof(from))
				break ;
			else {
				merror("Invalid format of attributes in names file: ", buf);
				return 0 ;
			}
	    }
		strIdx = 0 ;
        token = myToken(buf, strIdx, attrSeparators );
	    if (strIdx < 0) {
			merror("Invalid format of attribute name in names file: ", token);
			return 0 ;
		}
	    aDsc.destroy() ;
	    aDsc.name.copy(token) ;
		if (!getC45nameList(&(buf[strIdx]), names)) {
          merror("Invalid format attribute description in names file",buf) ;
          return 0;
		}
		dscLen = names.len() ;
		if (dscLen < 1)
		   return 0 ;
		else if (dscLen == 1) {
			aTypeStr = names.first()->value.getValue() ;
			if (strcmp(aTypeStr, "ignore")==0 || strcmp(aTypeStr, "label")==0)
				aDsc.aType = attrIgnore ;
			else if (strcmp(aTypeStr,"continuous")==0) {
				aDsc.aType = attrNum ;
				noNumericAttr++ ;
			}
			else {
		       merror("Invalid attribute type specifier in names file",aTypeStr) ;
 	           return 0;
			}
		}
		else {
			// is ordinal
			if (strncmp(names.first()->value.getValue(),ordinalIndicator, strlen(ordinalIndicator))==0){
				aDsc.aType = attrOrdinal ;
				// remove [ordered] indicator from first value
				mstring tempStr ;
				tempStr.copyFrom(names.first()->value, (int)strlen(ordinalIndicator)) ;
				tempStr.trimWhite();
				names.first()->value = tempStr ;
			}
			else { // ordinary discrete attribute
			  aDsc.aType = attrDisc ;			  
			}
			aDsc.values = names ;
			noDiscreteAttr++ ;
		}
	    if (classDsc.name.isDefined() && strcmp(aDsc.name.getValue(), classDsc.name.getValue())==0) {
			if (aDsc.aType == attrDisc || aDsc.aType == attrOrdinal) {
              aDsc.aType = attrDiscClass ;
			  classIdx = noDiscreteAttr - 1 ;
			  isRegression = mFALSE ;
			  isOrdinalClass = mTRUE ;
			}
	        else if (aDsc.aType == attrNum) {
              aDsc.aType = attrNumClass ;
			  classIdx = noNumericAttr - 1 ;
			  isRegression = mTRUE ;
	        }
			else {
	        	merror("Class attribute cannot be ignored ",classDsc.name.getValue());
	        	return 0 ;
			}
	        if (classFound) {
	        	merror("More than one class among attributes ",classDsc.name.getValue());
	        	return 0 ;
	        }
	        else
	        	classFound = true ;
	    }
	    attrs.addEnd(aDsc);

	} while (true) ;

	if (! classDsc.name.isDefined() && classDsc.values.len() > 0) {// add last to
		classDsc.name.copy("class");
		attrs.addEnd(classDsc);
		noDiscreteAttr++ ;
		classIdx = noDiscreteAttr - 1 ;
	}
	else if (! classFound) {
    	merror("No attribute matches class name ",classDsc.name.getValue());
    	return 0 ;
    }
	return 1 ;
}


int c45read::readC45data(FILE *from, const char *NAstr) {
	char buf[MaxLineLen], ebuf[MaxLineLen];
	int strIdx ;
	c45Data item ;
	mlistNode<c45Dsc> *ait ;
	dat.destroy() ;
	noDataCases = 0 ;
	mstring valStr ;
	char *token ;
	int pos, discIdx, numIdx ;
	int lineNo = 0, linesRead = readValidLine(from, buf) ;

	while (linesRead > 0){
		lineNo++ ;
		strIdx = 0 ;
	    item.create(noDiscreteAttr, noNumericAttr) ;
		token = myToken(buf, strIdx, datSeparators);
        ait = attrs.first() ;
        discIdx = numIdx = 0 ;
		while (token && ait) {
			trimWhite(token) ;
			if (token[strlen(token)-1]=='.')
				token[strlen(token)-1]='\0' ;
            switch (ait->value.aType) {
            case attrIgnore:
            	 break ;
            case attrNum:
			case attrNumClass:
            	if (strcmp(token, NAstr)==0)
            		item.numData[numIdx] = NAcont ;
            	else
            	  sscanf(token,"%lf",&(item.numData[numIdx])) ;
            	numIdx++ ;
            	break ;
            case attrDisc:
            case attrDiscClass:
            case attrOrdinal:
            	if (strcmp(token, NAstr)==0)
            		item.discData[discIdx] = NAdisc ;
            	else {
            		valStr.copy(token) ;
            		pos = ait->value.values.findPos(valStr) ;
            		if (pos < 1){
						snprintf(ebuf,MaxLineLen, "%s in line %d",ait->value.name.getValue(),lineNo) ;
            			merror("Invalid value for attribute ", ebuf) ;
						return 0 ;
            		}
            		else {
                		item.discData[discIdx] = pos ;
            		}
            	}
				discIdx++ ;
            	break ;
            }
            ait = attrs.next(ait) ;
			token = myToken(buf, strIdx, datSeparators);
		}
        dat.addEnd(item) ;
	    ++noDataCases ;
		linesRead = readValidLine(from, buf) ;
	}
	return 1 ;
}

// read costs from file where each non zero entry is ddescribed with line:
// predicted class, true class: cost
int c45read::readC45costs(FILE *from, mmatrix<double> &CostMatrix) {
    int i  ;
	char buf[MaxLineLen], errMsg[MaxLineLen] ;
	int strIdx ;
    mlistNode<c45Dsc> *iat ;
	char *predClass, *trueClass, *cost ;
	int trueClassIdx, predClassIdx ;
	double costValue ;
	mstring predClassStr, trueClassStr ;

    // find class values
    for (iat = attrs.first(); iat != 0 ; iat = attrs.next(iat))
	   if (iat->value.aType == attrDiscClass)
		   break ;
	int noCl = 0;
	if (iat != 0)
		noCl = iat->value.values.len() ;
	else {
		merror("c45read::readc45costs", "invalid number of classes assumed");
		return -1;
	}

	// creation and initialization
	CostMatrix.create(noCl+1, noCl+1, 1.0) ;
	for (i=0 ; i <= noCl ; i++)
		CostMatrix(i, i) = CostMatrix(i,0) = CostMatrix(0, i) = 0.0 ;
	while (readValidLine(from, buf)){
		strcpy(errMsg, buf) ; // store entire line for error reporting
		strIdx = 0 ;
        predClass = myToken(buf, strIdx, "," );
		trueClass = myToken(buf, strIdx, ":" );
        cost = myToken(buf, strIdx, "|" );
	    if (predClass == 0 || trueClass ==0 || cost == 0) {
			merror("Invalid format of costs file: ", errMsg);
			return 0 ;
		}
        trimWhite(predClass) ;
        trimWhite(trueClass) ;
        trimWhite(cost) ;
		predClassStr = predClass ;
        predClassIdx = iat->value.values.findPos(predClassStr) ;
		trueClassStr = trueClass ;
        trueClassIdx = iat->value.values.findPos(trueClassStr) ;
		costValue = atof(cost) ;
		if (predClassIdx > 0 && trueClassIdx > 0 && ! isNaN(costValue))
			CostMatrix(predClassIdx, trueClassIdx) = costValue ;
		else  {
			merror("Invalid format of costs file: ", errMsg);
			return 0 ;
		}
	}
	return 1 ;
}

// returns number of lines actually read, 0 if error
int c45read::readValidLine(FILE *from, char *buf)
{
	int len ;
	char *line ;
	int noLines = 0 ;
	do {

	   line = fgets(buf,MaxLineLen,from);
	   if (line == NULL) {
		   return 0 ;
	   }
	   noLines ++ ;
	   len = (int)strlen(buf) ;
	   if (len == MaxLineLen-1)
		   merror("Too long line, possible buffer overrun",buf);
	   if (buf[strlen(buf)-1] == '\n')
		   buf[strlen(buf)-1] = '\0' ; //remove \n at end of line
	   trimWhite(buf) ;
	} while  (buf[0] == '|' || buf[0] == '#' || buf[0] == '%' || buf[0]=='\0') ; // skip empty and comments lines
	return noLines ;
}

bool c45read::getC45nameList(char *buf, mlist<mstring> &names) {
	names.destroy() ;
	mstring item ;
	int strIdx = 0 ;
	char *token ;
	int commentIdx = posCharStr('|',buf) ;
	if (commentIdx >= 0)  // truncate
		buf[commentIdx] = '\0' ;
	int dotIdx = posLastCharStr('.',buf) ;
	if (dotIdx >= 0)
       buf[dotIdx] = '\0' ;
	dotIdx = (int)strlen(buf) ;
	do {
      token = myToken(buf, strIdx, nameSeparators );
      trimWhite(token) ;
      item.copy(token);
      names.addEnd(item) ;
	} while (strIdx > 0 && (strIdx-1) != dotIdx) ;
	return true ;
}


