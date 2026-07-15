#if !defined(PRINTUTIL_H)
#define PRINTUTIL_H

void printEstimationHead(FILE *to, dataStore *dt) ;
void printEstimations(FILE *to, int splitIdx, marray<marray<double> > &Result, dataStore *dt) ;
void printEstimationsInColumns(FILE *to, int splitIdx, marray<marray<double> > &Result, dataStore *dt);
//void printAVestimationHead(FILE *to,char *methodStr, dataStore *dt) ;
//void printAVestimations(FILE *to, int splitIdx, marray<marray<double> > &Result, dataStore *dt);
//void printAVestInColumns(FILE *to, marray<marray<double> > &Result, char *methodStr, dataStore *dt) ;
//void printAVestIn9Columns(FILE *to, char *methodStr,
//	  marray<marray<double> > &ResultCpAp, marray<marray<double> > &ResultCpAn, marray<marray<double> > &ResultCpAe,
//	  marray<marray<double> > &ResultCnAp, marray<marray<double> > &ResultCnAn, marray<marray<double> > &ResultCnAe,
//	  marray<marray<double> > &ResultCeAp, marray<marray<double> > &ResultCeAn, marray<marray<double> > &ResultCeAe,
//	  dataStore *dt) ;
void printAVest(FILE *to, marray<marray<double> > &reinfPos,
	  marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor, dataStore *dt)  ;
void printAVestRnd(FILE *to, mmatrix<marray<double> > &reinfPosRnd,
	  mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd,
	  dataStore *dt)  ;
/*
 * void printOrdClEst(FILE *to, marray<marray<double> > &reinfPos,
	  marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor,
	  dataStore *dt)  ;
void printOrdClEstRnd(FILE *to, mmatrix<marray<double> > &reinfPosRnd,
	  mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd,
	  dataStore *dt) ;
*/
void printOrdEvalInst(FILE *to, int instance, marray<double> &reinfPos, marray<double> &reinfNeg, marray<double> &anchor, dataStore *dt)  ;
void printOrdEvalInstRnd(FILE *to, int instance, marray<marray<double> > &reinfPosRnd,  marray<marray<double> > &reinfNegRnd, marray<marray<double> > &anchorRnd, dataStore *dt)  ;

#endif
