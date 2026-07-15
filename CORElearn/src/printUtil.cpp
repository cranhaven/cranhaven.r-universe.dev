#include <cfloat>
#include <cstdio>

#include "general.h"
#include "error.h"
#include "dataStore.h"
#include "printUtil.h"

extern estDsc estName[] ;
extern estDsc estNameReg[] ;

using namespace std ;


//************************************************************
//
//                      printEstimationHead
//                      -------------------
//
//              prints head of estimation table
//
//************************************************************
void printEstimationHead(FILE *to, dataStore *dt)
{
   fprintf(to,"\n\nidx%21s", "estimator") ;
   int i ;
   for (i=1 ; i <= dt->noAttr; i++)
     fprintf(to,"%10s ", dt->AttrDesc[i].AttributeName) ;
   fprintf(to, "\n") ;
   for (i=1 ; i <= 24+11*dt->noAttr; i++)
   fprintf(to, "-") ;
   fprintf(to, "\n") ;

}



 //************************************************************
//
//                      printEstimations
//                      ----------------
//
//        prints estimations for one split
//
//************************************************************
void printEstimations(FILE *to, int splitIdx, marray<marray<double> > &Result, dataStore *dt)
{
  int i, estIdx ;
  marray<booleanT> *estOn ;
  estDsc *estNm ;

  if (dt->isRegression) {
	  estOn = &(dt->opt->estOnReg) ;
	  estNm = estNameReg ;
  }
  else {
	  estOn = &(dt->opt->estOn) ;
	  estNm = estName ;
  }
  for (estIdx=1 ; estIdx < Result.len() ; estIdx++)
	 if ((*estOn)[estIdx])
     {
       fprintf(to, "%02d %21s", splitIdx, estNm[estIdx].brief) ;
       for (i=1 ; i <= dt->noAttr; i++)
          if (Result[estIdx][i] == -DBL_MAX)
            fprintf(to, "%10s ", "NA") ;
          else
            fprintf(to, "%10.5f ", Result[estIdx][i]) ;
       fprintf(to, "\n") ;
     }
 }



// ************************************************************
//
//                      printEstimationsInColumns
//                      -------------------------
//
//        prints estimations for one split
//
// ************************************************************
void printEstimationsInColumns(FILE *to, int splitIdx, marray<marray<double> > &Result, dataStore *dt)
{
    marray<booleanT> *estOn ;
    estDsc *estNm ;
	if (dt->isRegression) {
		estOn = &(dt->opt->estOnReg) ;
		estNm = estNameReg ;
	}
	else {
		estOn = &(dt->opt->estOn) ;
		estNm = estName ;
	}

     int estIdx, maxAttrLen = 0 ;
     for (int i=1 ; i <= dt->noAttr; i++)
        if ( (int)strlen(dt->AttrDesc[i].AttributeName) > maxAttrLen)
        	maxAttrLen = strlen(dt->AttrDesc[i].AttributeName) ;
 	 char attrHeader[] = "Attr.name" ;
	 maxAttrLen = Mmax(maxAttrLen, (int)strlen(attrHeader)) ;

	 fprintf(to, "\n\n%02d\n", splitIdx) ;
     fprintf(to, "%*s ", maxAttrLen, attrHeader) ;
     // print header
     for (estIdx=1 ; estIdx < Result.len() ; estIdx++)
       if ((*estOn)[estIdx])
         fprintf(to, "%*s ",Mmax((int)strlen(estNm[estIdx].brief),8), estNm[estIdx].brief) ;
     fprintf(to, "\n") ;

     // print rows, one for each attribute
     for (int i=1 ; i <= dt->noAttr; i++) {
        fprintf(to, "%*s ", maxAttrLen, dt->AttrDesc[i].AttributeName) ;
        for (estIdx=1 ; estIdx < Result.len() ; estIdx++)
          if ((*estOn)[estIdx]){
			   if (Result[estIdx][i] == -DBL_MAX)
                 fprintf(to, "%*s ", Mmax((int)strlen(estNm[estIdx].brief),8), "NA") ;
				else
				  fprintf(to, "%*.6f ", Mmax((int)strlen(estNm[estIdx].brief),8), Result[estIdx][i]) ;
          }
        fprintf(to, "\n") ;
     }
}
/*
// ************************************************************
//
//                      printAVestimationHead
//                      -------------------
//
//              prints head of attribute-value estimation table
//
// ************************************************************
void printAVestimationHead(FILE *to,char *methodStr, dataStore *dt)
{
   fprintf(to,"\n%s\nidx",methodStr) ;
   int i, j ;
   for (i=1 ; i < dt->noDiscrete; i++) {
	   fprintf(to,"%10s ", dt->AttrDesc[dt->DiscIdx[i]].AttributeName) ;
	   for (j=0 ; j < dt->AttrDesc[dt->DiscIdx[i]].NoValues; j++)
		   fprintf(to,"%10s ", dt->AttrDesc[dt->DiscIdx[i]].ValueName[j]) ;
   }
   fprintf(to, "\n-------------------------------------------------------------------------------------------------------\n") ;
}


// ************************************************************
//
//                      printAVestimations
//                      ----------------
//
//        prints estimations for one split
//
// ************************************************************
void printAVestimations(FILE *to, int splitIdx, marray<marray<double> > &Result, dataStore *dt)
{
  int i, j ;

  fprintf(to, "%02d ", splitIdx) ;
  for (i=1 ; i < dt->noDiscrete; i++) {
     for (j=0 ; j <= dt->AttrDesc[dt->DiscIdx[i]].NoValues; j++)
		   fprintf(to,"%10.5f ", Result[i][j]) ;
  }
  fprintf(to, "\n") ;
}

// ************************************************************
//
//                      printAVestInColumns
//                      ----------------
//
//        prints estimations for one split
//
// ************************************************************
void printAVestInColumns(FILE *to, marray<marray<double> > &Result, char *methodStr, dataStore *dt) {
  int i, j ;

  fprintf(to, "\n%10s %10s\n", "Attr.value",methodStr) ;

  for (i=1 ; i < dt->noDiscrete; i++) {
     fprintf(to,"%10s %10.5f\n", dt->AttrDesc[dt->DiscIdx[i]].AttributeName, Result[i][0]) ;
	 for (j=1 ; j <= dt->AttrDesc[dt->DiscIdx[i]].NoValues; j++)
		 fprintf(to,"%10s:%10.5f\n", dt->AttrDesc[dt->DiscIdx[i]].ValueName[j-1], Result[i][j]) ;
  }
  fprintf(to, "\n") ;
}



void printAVestIn9Columns(FILE *to, char *methodStr,
	  marray<marray<double> > &ResultCpAp,
	  marray<marray<double> > &ResultCpAn, marray<marray<double> > &ResultCpAe,
	  marray<marray<double> > &ResultCnAp,
	  marray<marray<double> > &ResultCnAn, marray<marray<double> > &ResultCnAe,
	  marray<marray<double> > &ResultCeAp,
	  marray<marray<double> > &ResultCeAn, marray<marray<double> > &ResultCeAe,
	  dataStore *dt) {
  int i, j ;

  fprintf(to, "\n%s", methodStr) ;
  fprintf(to, "\n%10s %8s %8s %8s %8s %8s %8s %8s %8s %8s\n",
	          "Attr.value","CpAp","CpAn","CpAe","CnAp","CnAn","CnAe", "CeAp","CeAn","CeAe") ;

  for (i=1 ; i < dt->noDiscrete; i++) {
     fprintf(to,"%10s %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f \n",
		 dt->AttrDesc[dt->DiscIdx[i]].AttributeName,
		 ResultCpAp[i][0], ResultCpAn[i][0], ResultCpAe[i][0],
		 ResultCnAp[i][0], ResultCnAn[i][0], ResultCnAe[i][0],
		 ResultCeAp[i][0], ResultCeAn[i][0], ResultCeAe[i][0]) ;
	 for (j=1 ; j <= dt->AttrDesc[dt->DiscIdx[i]].NoValues; j++)
		 fprintf(to,"%10s:%8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n",
		 dt->AttrDesc[dt->DiscIdx[i]].ValueName[j-1],
		 ResultCpAp[i][j], ResultCpAn[i][j], ResultCpAe[i][j],
		 ResultCnAp[i][j], ResultCnAn[i][j], ResultCnAe[i][j],
		 ResultCeAp[i][j], ResultCeAn[i][j], ResultCeAe[i][j]) ;
  }
  fprintf(to, "\n") ;

  fprintf(to, "\nVisualization data:\n%10s %10s %10s %10s\n",
	          "Attr_Value","reinfPos","reinfNeg","anchor") ;

  double reinfPos, reinfNeg, anchor, denom ;
  for (i=1 ; i < dt->noDiscrete; i++) {
	 for (j=0 ; j <= dt->AttrDesc[dt->DiscIdx[i]].NoValues; j++) {
	     if (j==0)
			fprintf(to,"%10s ", dt->AttrDesc[dt->DiscIdx[i]].AttributeName) ;
		 else fprintf(to,"%10s:", dt->AttrDesc[dt->DiscIdx[i]].ValueName[j-1]) ;
 	     denom = ResultCpAp[i][j] + ResultCnAp[i][j] + ResultCeAp[i][j] ;
		 if (denom > 0)
			reinfPos = (ResultCpAp[i][j]-ResultCnAp[i][j]) / denom ;
		 else reinfPos = 0.0 ;
         denom = ResultCpAn[i][j] + ResultCnAn[i][j]+ ResultCeAn[i][j] ;
		 if (denom > 0)
			reinfNeg = (ResultCnAn[i][j]-ResultCpAn[i][j]) / denom ;
		 else reinfNeg = 0.0 ;
         denom = ResultCpAe[i][j] + ResultCnAe[i][j]+ ResultCeAe[i][j] ;
		 if (denom > 0)
			anchor = (ResultCeAe[i][j] -ResultCpAe[i][j] - ResultCnAe[i][j]) / denom ;
		 else anchor = 0.0 ;
		 fprintf(to,"%8.3f %8.3f %8.3f\n", reinfPos, reinfNeg, anchor) ;
	 }
  }
  fprintf(to, "\n") ;

}
*/


void printAVest(FILE *to, marray<marray<double> > &reinfPos,
	  marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor, dataStore *dt)  {
  int i, j ;

  fprintf(to, "%10s, %10s, %10s, %10s, %5s\n", "AttrValue","reinfPos","reinfNeg","anchor","noAV") ;

  marray<marray<int> > noAV ;
  dt->countAV(noAV) ;

  for (i=1 ; i < dt->noDiscrete; i++) {
	 for (j=0 ; j <= dt->AttrDesc[dt->DiscIdx[i]].NoValues; j++) {
		 if (j==0) {
			fprintf(to,"%10s, ", dt->AttrDesc[dt->DiscIdx[i]].AttributeName) ;
			noAV[i][0] = dt->NoTrainCases - noAV[i][0] ; // all except missing
		 }
		 else fprintf(to,"%10s, ", dt->AttrDesc[dt->DiscIdx[i]].ValueName[j-1]) ;
		 fprintf(to,"%10.4f, %10.4f, %10.4f, %5d\n", reinfPos[i][j], reinfNeg[i][j], anchor[i][j], noAV[i][j]) ;
	 }
  }
  fprintf(to, "\n") ;
}


const char* stats[] = {"Median","Q1","Q3","Low","High","Mean","StdDev","p-value","Exp"} ;

void printAVestRnd(FILE *to, mmatrix<marray<double> > &reinfPosRnd,
	  mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd,
	  dataStore *dt)  {
  int i, j, m, s ;
  int noMethods = 3 ;
  const char* methods[] = {"reinfPosRnd","reinfNegRnd","anchorRnd"};
  char buf[MaxNameLen] ;
  marray<double>  *rndResult=0 ;
  fprintf(to, "%17s", "AttributeValue") ;
  for (m=0 ; m < noMethods ; ++m){
	  for (s=0 ; s < noOEstats ; ++s) {
		  snprintf(buf, MaxNameLen, "%s%s",methods[m],stats[s]) ;
		  fprintf(to, ", %17s",buf) ; // 17 is length of the longest name
	  }
  }
  fprintf(to,"\n") ;
  for (i=1 ; i < dt->noDiscrete; i++) {
	 for (j=0 ; j <= dt->AttrDesc[dt->DiscIdx[i]].NoValues; j++) {
		 if (j==0)
			fprintf(to,"%17s", dt->AttrDesc[dt->DiscIdx[i]].AttributeName) ;
		 else
			fprintf(to,"%17s", dt->AttrDesc[dt->DiscIdx[i]].ValueName[j-1]) ;
         for (m=0 ; m < noMethods ; ++m){
			 if (m == 0)
				 rndResult = &reinfPosRnd(i,j) ;
			 else if (m == 1)
				 rndResult = &reinfNegRnd(i,j) ;
			 else if (m==2)
				 rndResult = &anchorRnd(i,j) ;
	         for (s=0 ; s < noOEstats ; ++s) {
        		 fprintf(to,", %17.4f", (*rndResult)[s] ) ;
			 }
		 }
		 fprintf(to, "\n") ;
	 }
  }
}

// prints evaluations for a single instance
void printOrdEvalInst(FILE *to, int instance, marray<double> &reinfPos, marray<double> &reinfNeg,
		              marray<double> &anchor, dataStore *dt)  {
  int i,maxNameLen=strlen(dt->AttrDesc[0].AttributeName), maxValLen=strlen(dt->AttrDesc[0].ValueName[dt->DiscData(instance,0)-1]) ;
  // put class name and value into header
  for (i=1 ; i < dt->noDiscrete; i++) {
	  maxNameLen = Mmax(maxNameLen, (int)strlen(dt->AttrDesc[dt->DiscIdx[i]].AttributeName));
	  if (dt->DiscData(instance,i)!=NAdisc)
	     maxValLen = Mmax(maxValLen, (int)strlen(dt->AttrDesc[dt->DiscIdx[i]].ValueName[dt->DiscData(instance,i)-1]));
  }
  fprintf(to, "%*s, %*s, %6s, %6s, %6s\n", maxNameLen, dt->AttrDesc[0].AttributeName, maxValLen,  dt->AttrDesc[0].ValueName[dt->DiscData(instance,0)-1],"impPos","impNeg","impAnc") ;
  // each line contains reinforcement factors for one attribute
  for (i=1 ; i < dt->noDiscrete; i++) {
	 fprintf(to,"%*s, %*s, ", maxNameLen, dt->AttrDesc[i].AttributeName, maxValLen, ((dt->DiscData(instance,i)==NAdisc)?dt->opt->NAstring.getConstValue():dt->AttrDesc[dt->DiscIdx[i]].ValueName[dt->DiscData(instance,i)-1])) ;
	 fprintf(to,"%6.4f, %6.4f, %6.4f\n", reinfPos[i], reinfNeg[i], anchor[i]) ;
  }
  //fprintf(to, "\n") ;
}
//prints random normalizations for a single instance
void printOrdEvalInstRnd(FILE *to, int instance, marray<marray<double> > &reinfPosRnd,  marray<marray<double> > &reinfNegRnd, marray<marray<double> > &anchorRnd, dataStore *dt)  {
	int i, m, s ;
	int noMethods = 3 ;
	const char* methods[] = {"reinfPosRnd","reinfNegRnd","anchorRnd"};
	char buf[MaxNameLen] ;
	marray<double>  *rndResult=0 ;
	fprintf(to, "%17s", "AttributeValue") ;
	for (m=0 ; m < noMethods ; ++m){
		for (s=0 ; s < noOEstats ; ++s) {
			snprintf(buf, MaxNameLen, "%s%s", methods[m], stats[s]) ;
			fprintf(to, ", %17s", buf) ; // 17 is length of the longest name
		}
	}
	fprintf(to,"\n") ;
	for (i=1 ; i < dt->noDiscrete; i++) {
		fprintf(to,"%17s", ((NAdisc==dt->DiscData(instance,i))?dt->opt->NAstring.getConstValue():dt->AttrDesc[dt->DiscIdx[i]].ValueName[dt->DiscData(instance,i)-1])) ;
		for (m=0 ; m < noMethods ; ++m){
			if (m == 0)
				rndResult = &reinfPosRnd[i] ;
			else if (m == 1)
				rndResult = &reinfNegRnd[i] ;
			else if (m==2)
				rndResult = &anchorRnd[i] ;
			for (s=0 ; s < noOEstats ; ++s)
				fprintf(to,", %17.4f", (*rndResult)[s] ) ;
		}
		fprintf(to, "\n") ;
	}
}


/*
void printOrdClEst(FILE *to, marray<marray<double> > &reinfPos,
	  marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor,
	  dataStore *dt)  {
  int i, j=0 ;

  fprintf(to, "%10s, %10s, %10s, %10s, %5s\n", "AttrValue","reinfPos","reinfNeg","anchor","noAV") ;

  marray<int> noNA ;
  dt->countNA(noNA) ;

  for (i=0 ; i < dt->noNumeric; i++) {
		 fprintf(to,"%10s, %10.4f, %10.4f, %10.4f, %5d\n", dt->AttrDesc[dt->ContIdx[i]].AttributeName, reinfPos[i][j], reinfNeg[i][j], anchor[i][j], dt->NoTrainCases - noNA[i]) ;
  }
  fprintf(to, "\n") ;
}


void printOrdClEstRnd(FILE *to, mmatrix<marray<double> > &reinfPosRnd,
	  mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd,
	  dataStore *dt){
  int i, j=0, m, s ;
  int noMethods = 3;
  char* methods[] = {"reinfPosRnd","reinfNegRnd","anchorRnd"};
  char* stats[] = {"Median","Q1","Q3","Low","High","Mean","StdDev","Exp"} ;
  char buf[MaxNameLen] ;
  marray<double>  *rndResult ;
  fprintf(to, "%17s", "AttributeValue") ;
  for (m=0 ; m < noMethods ; ++m){
	  for (s=0 ; s < noOEstats ; ++s) {
		  snprintf(buf, MaxNameLen, "%s%s",methods[m],stats[s]) ;
		  fprintf(to, ", %17s",buf) ; // 17 is length of the longest name
	  }
  }
  fprintf(to,"\n") ;
  for (i=0 ; i < dt->noNumeric; i++) {
   		 fprintf(to,"%17s", dt->AttrDesc[dt->ContIdx[i]].AttributeName) ;
         for (m=0 ; m < noMethods ; ++m){
			 if (m == 0)
				 rndResult = &reinfPosRnd(i,j) ;
			 else if (m == 1)
				 rndResult = &reinfNegRnd(i,j) ;
			 else if (m==2)
				 rndResult = &anchorRnd(i,j) ;
	         for (s=0 ; s < noOEstats ; ++s) {
        		 fprintf(to,", %17.4f", (*rndResult)[s] ) ;
			 }
		 }
		 fprintf(to, "\n") ;
  }
}

*/
