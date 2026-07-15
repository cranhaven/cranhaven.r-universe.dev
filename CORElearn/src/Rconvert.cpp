#include <cfloat>

#include "general.h"
#include "error.h"
#include "dataStore.h"
#include "ftree.h"
#include "regtree.h"
#include "utils.h"

using namespace std ;

int featureTree::predictR(marray<int> &predictedClass, marray<double> &predictedProb) {
	int i, j;
	// initialize return arrays
	for (i=0; i < NoPredict ; i++) {
		predictedClass[i] = NAdisc ;
		for (j=0; j < noClasses ; j++)
			predictedProb[i+ j * NoPredict] = -1.0;
	}

	marray<double> probDist(noClasses+1); // to hold probability distribution
	marray<int> DSet(NoPredict); // indexes of predicted instances
	for (i=0; i < NoPredict ; i++)
		// predict all instances
		DSet[i] = i ;

	// set the prediction data
	dData = &DiscPredictData ;
	nData = &NumPredictData ;

	for (i=0; i < NoPredict ; i++) {
		probDist.init(0.0) ;

		if (learnRF) {
			if (opt->rfkNearestEqual>0)
				rfNearCheck(DSet[i], probDist) ;
			else if (noClasses==2&& opt->rfRegType==1)
				rfCheckReg(DSet[i], probDist) ;
			else
				rfCheck(DSet[i], probDist) ;
		} else
			check(root, DSet[i], probDist) ;

		// predict class with minimal conditional risk,
		// in case of uniform costs this is the class with maximal predicted probability
		double minRisk= DBL_MAX, cRisk;
		int cMin = 0, cPredicted, cTrue;
		for (cPredicted=1; cPredicted <= noClasses; cPredicted++) {
			cRisk = 0.0;
			for (cTrue=1; cTrue <= noClasses ; cTrue++)
				cRisk += probDist[cTrue] * CostMatrix(cTrue, cPredicted) ;
			if (cRisk < minRisk) {
				minRisk = cRisk ;
				cMin = cPredicted ;
			}
		}
		predictedClass[i] = cMin ;

		// copy to output
		for (j=1; j<=noClasses ; j++)
			predictedProb[i + (j-1)*NoPredict] = probDist[j];
	}
	// set the prediction data
	dData = &DiscData ;
	nData = &NumData ;

	return 1;
}

int regressionTree::predictRreg(marray<double> &predicted) {
	int i;
	// initialize return arrays
	for (i=0; i < NoPredict ; i++) {
		predicted[i] = NAcont ;
	}

	marray<int> DSet(NoPredict); // indexes of predicted instances
	for (i=0; i < NoPredict ; i++)
		// predict all instances
		DSet[i] = i ;

	// set the prediction data
	dData = &DiscPredictData ;
	nData = &NumPredictData ;

	for (i=0; i < NoPredict ; i++) {
  	   predicted[i] = check(root, DSet[i]) ;
	}

	// set the prediction data
	dData = &DiscData ;
	nData = &NumData ;

	return 1;
}


const char attrValSeparatorsFromR[] = "\x1F" ;

int dataStore::dscFromR(int noDiscreteAttr, marray<int> &noDiscreteValues,
		int noNumericAttr, marray<char* > &discAttrNames,
		marray<char* > &discValNames, marray<char* >  &numAttrNames) {

	int iA, iV;
	NoOriginalAttr = noAttr = noDiscreteAttr + noNumericAttr -1;

	noNumeric = 0;
	noDiscrete = 0;

	ContIdx.create(noAttr+1, -1) ;
	DiscIdx.create(noAttr+1, -1) ;
	AttrDesc.create(noAttr+1) ;
	char buf[MaxNameLen] ;
	mlist<mstring> valList ;
	mlistNode<mstring> *vlIter=0 ;

	for (iA=0; iA <= noAttr ; iA++) {
		// first copy discrete
		if (noDiscrete < noDiscreteAttr && !(iA==0 && isRegression)) {
		    if (discAttrNames[noDiscrete]) {
		    	strcpy(buf,discAttrNames[noDiscrete]);
		    	tokenizedList(discValNames[noDiscrete],valList,attrValSeparatorsFromR) ;
		    	vlIter = valList.first() ;
		    }
		    else
			    snprintf(buf, MaxNameLen, "D%d", noDiscrete) ;
			strcpy(AttrDesc[iA].AttributeName=new char[strlen(buf)+1], buf);
			AttrDesc[iA].continuous = mFALSE ; // should be discrete
			AttrDesc[iA].ValueName.create(noDiscreteValues[noDiscrete]) ;
			AttrDesc[iA].valueProbability.create(noDiscreteValues[noDiscrete]+1) ;
			for (iV=1; iV <= noDiscreteValues[noDiscrete]; iV++) {
				if (discAttrNames[noDiscrete]) {
					strcpy(buf,vlIter->value.getValue());
					vlIter = valList.next(vlIter) ;
				}
				else
				   snprintf(buf, MaxNameLen, "V%d", iV) ;
				strcpy(AttrDesc[iA].ValueName[iV-1]=new char[strlen(buf)+1], buf) ;
			}
			AttrDesc[iA].NoValues = noDiscreteValues[noDiscrete];
			DiscIdx[noDiscrete] = iA ;
			AttrDesc[iA].tablePlace = noDiscrete ;
			noDiscrete ++;
		} else {
		    if (numAttrNames[noNumeric])
		    	strcpy(buf, numAttrNames[noNumeric]);
		    else
		      snprintf(buf, MaxNameLen, "N%d", noNumeric) ;
			strcpy(AttrDesc[iA].AttributeName=new char[strlen(buf)+1], buf);
			AttrDesc[iA].continuous = mTRUE ;
			AttrDesc[iA].NoValues = 0;
			AttrDesc[iA].tablePlace = noNumeric;
			AttrDesc[iA].userDefinedDistance = mFALSE ;
			AttrDesc[iA].EqualDistance = AttrDesc[iA].DifferentDistance = -1.0;
			ContIdx[noNumeric] = iA ;
			noNumeric ++;
		}
	}
	if (isRegression)
		noClasses = 0 ;
	else
		noClasses =  AttrDesc[0].NoValues ;
	if (noNumeric!=noNumericAttr || noDiscrete!=noDiscreteAttr) {
		merror("dscFromData", "invalid conversion") ;
		return 0;
	}

	return 1;
}

void dataStore::dataFromR(int noInst, marray<int> &discreteData,
		marray<double> &numericData, booleanT isTrain) {
	int i, j;

	mmatrix<int> *dscData;
	mmatrix<double> *numData;

	if (isTrain) { // fill the first set of data
		NoCases= noInst ;
		dscData = &DiscData ;
		numData = &NumData ;
	} else {
		NoPredict = noInst ;
		dscData = &DiscPredictData ;
		numData = &NumPredictData ;
	}
	if (noDiscrete)
		dscData->create(noInst, noDiscrete) ;
	if (noNumeric)
		numData->create(noInst, noNumeric) ;

	for (i=0; i< noInst ; i++) {
		for (j=0; j < noDiscrete ; j++) {
			(*dscData)(i, j)=discreteData[i + j*noInst];
			if ((*dscData)(i, j)<0 || (*dscData)(i, j)> AttrDesc[DiscIdx[j]].NoValues) {
				merror("Invalid data detected for attribute", AttrDesc[DiscIdx[j]].AttributeName ) ;
				(*dscData)(i, j) = NAdisc ;
			}
		}
		for (j=0; j < noNumeric ; j++) {
			(*numData)(i, j)=numericData[i + j*noInst];
             #if defined(DEBUG)
			   if (! isNAcont((*numData)(i, j)) && isNaN((*numData)(i, j)))
				   merror("Invalid data, NaN present, which are not NA","");
             #endif
		}
	}
}

void dataStore::costsFromR(marray<double> &costs) {
	int i, j;

	CostMatrix.create(noClasses+1, noClasses+1, 0.0) ;
	for (i=1; i <= noClasses; i++)
		for (j=1; j <= noClasses; j++)
			CostMatrix(i, j) = costs[i-1 +(j-1)*noClasses];
}


#if defined(R_PORT)

/*****************************************************************/
/**                      exportSizes                            **/
/*****************************************************************/
SEXP featureTree::exportSizes(void)
{
	SEXP out;
	int i ;
	if (forest.defined()) {
		PROTECT(out = Rf_allocVector(INTSXP, opt->rfNoTrees));
		for (i=0 ; i < opt->rfNoTrees; i++)
			//rfWriteTree(fout,2,i) ;
			INTEGER(out)[i] = getSize(forest[i].t.root);
		UNPROTECT(1);
		return(out);
	}
	return(NULL);
}


/*****************************************************************/
/**                  exportSumOverLeaves                        **/
/*****************************************************************/
SEXP featureTree::exportSumOverLeaves(void)
{
	SEXP out;
	int i ;
	if (forest.defined()) {
		PROTECT(out = Rf_allocVector(INTSXP, opt->rfNoTrees));
		for (i=0 ; i < opt->rfNoTrees; i++)
			//rfWriteTree(fout,2,i) ;
			INTEGER(out)[i] = getSumOverLeaves(forest[i].t.root, 0);
		UNPROTECT(1);
		return(out);
	}
	return(NULL);
}


//************************************************************
//
//                           RF2R
//                           ---------
//
//                     converts forest to R's recursive list
//
//************************************************************
SEXP featureTree::RF2R()
{
	SEXP out, aux, names, tree, treeNames, treeAux;
	int i ;

	if (forest.defined()) {
		// create output vector "out" of length 8
		PROTECT(out = Rf_allocVector(VECSXP, 8));

		// modelType
		PROTECT(aux = Rf_allocVector(STRSXP, 1));
		SET_STRING_ELT(aux, 0, Rf_mkChar("randomForest"));
		SET_VECTOR_ELT(out, 0, aux);

		// rfNoTrees
		PROTECT(aux = Rf_allocVector(INTSXP, 1));
		INTEGER(aux)[0] = opt->rfNoTrees;
		SET_VECTOR_ELT(out, 1, aux);

		// noClasses
		PROTECT(aux = Rf_allocVector(INTSXP, 1));
		INTEGER(aux)[0] = noClasses;
		SET_VECTOR_ELT(out, 2, aux);

		// noAttr
		PROTECT(aux = Rf_allocVector(INTSXP, 1));
		INTEGER(aux)[0] = noAttr;
		SET_VECTOR_ELT(out, 3, aux);

		// noNumeric
		PROTECT(aux = Rf_allocVector(INTSXP, 1));
		INTEGER(aux)[0] = noNumeric;
		SET_VECTOR_ELT(out, 4, aux);

		// noDiscrete
		PROTECT(aux = Rf_allocVector(INTSXP, 1));
		INTEGER(aux)[0] = noDiscrete-1;
		SET_VECTOR_ELT(out, 5, aux);

		// discNoValues
		PROTECT(aux = Rf_allocVector(INTSXP, noDiscrete-1));
		for (i=1 ; i < noDiscrete; i++)
			INTEGER(aux)[i-1] = AttrDesc[DiscIdx[i]].NoValues;
		SET_VECTOR_ELT(out, 6, aux);

		// trees
		PROTECT(aux = Rf_allocVector(VECSXP, opt->rfNoTrees));
		for (i=0 ; i < opt->rfNoTrees; i++) {
			PROTECT(tree = Rf_allocVector(VECSXP, 2));

			// treeIdx
			PROTECT(treeAux = Rf_allocVector(INTSXP, 1));
			INTEGER(treeAux)[0] = i;
			SET_VECTOR_ELT(tree, 0, treeAux);

			// structure
			treeAux = RFtree2R(forest[i].t.root);
			SET_VECTOR_ELT(tree, 1, treeAux);

			PROTECT(treeNames = Rf_allocVector(STRSXP, 2));
			SET_STRING_ELT(treeNames, 0, Rf_mkChar("treeIdx"));
			SET_STRING_ELT(treeNames, 1, Rf_mkChar("structure"));
			Rf_setAttrib(tree, R_NamesSymbol, treeNames);

			SET_VECTOR_ELT(aux, i, tree);

			UNPROTECT(3) ; // is this correct?
		}
		SET_VECTOR_ELT(out, 7, aux);

		// names attribute
		PROTECT(names = Rf_allocVector(STRSXP, 8));
		SET_STRING_ELT(names, 0, Rf_mkChar("modelType"));
		SET_STRING_ELT(names, 1, Rf_mkChar("rfNoTrees"));
		SET_STRING_ELT(names, 2, Rf_mkChar("noClasses"));
		SET_STRING_ELT(names, 3, Rf_mkChar("noAttr"));
		SET_STRING_ELT(names, 4, Rf_mkChar("noNumeric"));
		SET_STRING_ELT(names, 5, Rf_mkChar("noDiscrete"));
		SET_STRING_ELT(names, 6, Rf_mkChar("discNoValues"));
		SET_STRING_ELT(names, 7, Rf_mkChar("trees"));
		Rf_setAttrib(out, R_NamesSymbol, names);

		UNPROTECT(10) ;
		return out ;
	}
	return NULL ;
}

//************************************************************
//
//                           RFtree2R
//                           ---------
//
//         converts tree structure to R's recursive list
//
//************************************************************
SEXP featureTree::RFtree2R(binnode *branch){
	SEXP out, aux, names ;

    if (branch->Identification == leaf)  {
				PROTECT(out = Rf_allocVector(VECSXP, 3));
				// nodeId
				PROTECT(aux = Rf_allocVector(STRSXP, 1));
				SET_STRING_ELT(aux, 0, Rf_mkChar("leaf"));
				SET_VECTOR_ELT(out, 0, aux);

				// classify
				PROTECT(aux = Rf_allocVector(REALSXP, noClasses));
				for (int i=1 ; i <= noClasses ; i++)
					REAL(aux)[i-1] = branch->Classify[i] ;
				SET_VECTOR_ELT(out, 1, aux);

				// weight
				PROTECT(aux = Rf_allocVector(REALSXP, 1));
				REAL(aux)[0] = branch->weight ;
				SET_VECTOR_ELT(out, 2, aux);

				// names attribute
				PROTECT(names = Rf_allocVector(STRSXP, 3));
				SET_STRING_ELT(names, 0, Rf_mkChar("nodeId"));
				SET_STRING_ELT(names, 1, Rf_mkChar("classify"));
				SET_STRING_ELT(names, 2, Rf_mkChar("weight"));
				Rf_setAttrib(out, R_NamesSymbol, names);

				UNPROTECT(5) ;// is this correct?
				return out ;
    }
    else if (branch->Identification == continuousAttribute) {
				PROTECT(out = Rf_allocVector(VECSXP, 6));

				// nodeId
				PROTECT(aux = Rf_allocVector(STRSXP, 1));
				SET_STRING_ELT(aux, 0, Rf_mkChar("numericSplit"));
				SET_VECTOR_ELT(out, 0, aux);

				// attr
				PROTECT(aux = Rf_allocVector(INTSXP, 1));
				INTEGER(aux)[0] = branch->Construct.root->attrIdx+1;
				SET_VECTOR_ELT(out, 1, aux);


				// split
				PROTECT(aux = Rf_allocVector(REALSXP, 1));
				REAL(aux)[0] = branch->Construct.splitValue ;
				SET_VECTOR_ELT(out, 2, aux);

				// NAdefault
				PROTECT(aux = Rf_allocVector(STRSXP, 1));
	        	if (branch->NAnumValue[branch->Construct.root->attrIdx] <= branch->Construct.splitValue)
  				   SET_STRING_ELT(aux, 0, Rf_mkChar("left"));
	        	else
  				   SET_STRING_ELT(aux, 0, Rf_mkChar("right"));
				SET_VECTOR_ELT(out, 3, aux);

				// leftTree
				aux = RFtree2R(branch->left);
				SET_VECTOR_ELT(out, 4, aux);

				// rightTree
				aux = RFtree2R(branch->right);
				SET_VECTOR_ELT(out, 5, aux);

				// names attribute
				PROTECT(names = Rf_allocVector(STRSXP, 6));
				SET_STRING_ELT(names, 0, Rf_mkChar("nodeId"));
				SET_STRING_ELT(names, 1, Rf_mkChar("attr"));
				SET_STRING_ELT(names, 2, Rf_mkChar("split"));
				SET_STRING_ELT(names, 3, Rf_mkChar("NAdefault"));
				SET_STRING_ELT(names, 4, Rf_mkChar("leftTree"));
				SET_STRING_ELT(names, 5, Rf_mkChar("rightTree"));
				Rf_setAttrib(out, R_NamesSymbol, names);

				UNPROTECT(6) ; // is this correct?
				return out ;
    }
    else if (branch->Identification == discreteAttribute) {
     			PROTECT(out = Rf_allocVector(VECSXP, 6));

				// nodeId
				PROTECT(aux = Rf_allocVector(STRSXP, 1));
				SET_STRING_ELT(aux, 0, Rf_mkChar("discreteSplit"));
				SET_VECTOR_ELT(out, 0, aux);

				// attr
				PROTECT(aux = Rf_allocVector(INTSXP, 1));
				INTEGER(aux)[0] = branch->Construct.root->attrIdx;
				SET_VECTOR_ELT(out, 1, aux);


				// leftValues
				int noLeft = 0, iLeft=0 ;
				for (int i=1 ; i <= AttrDesc[DiscIdx[branch->Construct.root->attrIdx]].NoValues; i++)
					if (branch->Construct.leftValues[i])
						noLeft ++ ;
				PROTECT(aux = Rf_allocVector(INTSXP, noLeft));
				for (int i=1 ; i <= AttrDesc[DiscIdx[branch->Construct.root->attrIdx]].NoValues; i++)
					if (branch->Construct.leftValues[i])
				       INTEGER(aux)[iLeft++] = i ;
				SET_VECTOR_ELT(out, 2, aux);

				// NAdefault
				PROTECT(aux = Rf_allocVector(STRSXP, 1));
				if (branch->Construct.leftValues[branch->NAdiscValue[branch->Construct.root->attrIdx]])
  				   SET_STRING_ELT(aux, 0, Rf_mkChar("left"));
	        	else
  				   SET_STRING_ELT(aux, 0, Rf_mkChar("right"));
				SET_VECTOR_ELT(out, 3, aux);

				// leftTree
				aux = RFtree2R(branch->left);
				SET_VECTOR_ELT(out, 4, aux);

				// rightTree
				aux = RFtree2R(branch->right);
				SET_VECTOR_ELT(out, 5, aux);

				// names attribute
				PROTECT(names = Rf_allocVector(STRSXP, 6));
				SET_STRING_ELT(names, 0, Rf_mkChar("nodeId"));
				SET_STRING_ELT(names, 1, Rf_mkChar("attr"));
				SET_STRING_ELT(names, 2, Rf_mkChar("leftValues"));
				SET_STRING_ELT(names, 3, Rf_mkChar("NAdefault"));
				SET_STRING_ELT(names, 4, Rf_mkChar("leftTree"));
				SET_STRING_ELT(names, 5, Rf_mkChar("rightTree"));
				Rf_setAttrib(out, R_NamesSymbol, names);

				UNPROTECT(6) ; // is this correct?
				return out ;
    }
    else return NULL ;
}




#endif
