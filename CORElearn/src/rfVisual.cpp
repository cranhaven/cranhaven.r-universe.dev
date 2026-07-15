#include <cmath>
#include <cfloat>

#include "general.h"
#include "error.h"
#include "dataStore.h"
#include "ftree.h"
#include "regtree.h"
#include "utils.h"
#include "binnode.h"
#include "binnodeReg.h"

using namespace std ;

#if defined(R_PORT)
SEXP featureTree::T2Rpart()
{
        int nProtected = 0;
        /*
         * number of leaf node TODO: try noLeaves if present-it is the same
         */
        int treeDepth = this->getSize(this->root);
        /*
         * all nodes (leaf + inner node - binary tree!)
         */
        int line = 2*treeDepth-1;

        /*
         * frame data
         */
        const int varNames = 9;
        int varRealName = varNames-1;
        SEXP outMatrix;
        PROTECT(outMatrix = Rf_allocVector(VECSXP, line*varRealName));
        nProtected++;
        /*
         * frame names
         */
        SEXP outVector;
        PROTECT(outVector = Rf_allocVector(STRSXP, varNames));
        nProtected++;
        /*
         * two colons [sequential index, attribute index]
         */
        SEXP outVectorLabels;
        PROTECT(outVectorLabels = Rf_allocVector(STRSXP, line*3));
        nProtected++;

        //frame names
        SET_STRING_ELT(outVector, 0, Rf_mkChar("var"));
        SET_STRING_ELT(outVector, 1, Rf_mkChar("n"));
        SET_STRING_ELT(outVector, 2, Rf_mkChar("wt"));
        SET_STRING_ELT(outVector, 3, Rf_mkChar("dev"));
        SET_STRING_ELT(outVector, 4, Rf_mkChar("yval"));
        SET_STRING_ELT(outVector, 5, Rf_mkChar("complexity"));
        SET_STRING_ELT(outVector, 6, Rf_mkChar("ncompete"));
        SET_STRING_ELT(outVector, 7, Rf_mkChar("nsurrogate"));
        SET_STRING_ELT(outVector, 8, Rf_mkChar("yval2"));

        
        /*
         * constant number of column names in split table
         */
        const int splitsNames = 6;
        SEXP outSplitVector;
        PROTECT(outSplitVector = Rf_allocVector(STRSXP, splitsNames-1));
        nProtected++;

        /*
         * not leaf nodes. Only inner nodes have an entry in the split matrix
         */
        SEXP outSplitMatrix;
        PROTECT(outSplitMatrix = Rf_allocVector(VECSXP, (treeDepth-1)*splitsNames));
        nProtected++;

        //split names
        SET_STRING_ELT(outSplitVector, 0, Rf_mkChar("count"));
        SET_STRING_ELT(outSplitVector, 1, Rf_mkChar("ncat"));
        SET_STRING_ELT(outSplitVector, 2, Rf_mkChar("improve"));
        SET_STRING_ELT(outSplitVector, 3, Rf_mkChar("index"));
        SET_STRING_ELT(outSplitVector, 4, Rf_mkChar("adj"));
        
        /*
         * csplit array for discrete attributes.
         * rows: for each inner node == possible discrete split
         * column: max levels == max number of values of each attr.
         */
        int currCsplitMax = 0;
        for(int csplitColumn = 0; csplitColumn < this->AttrDesc.len(); csplitColumn++){
                if(currCsplitMax < this->AttrDesc[csplitColumn].NoValues){
                        currCsplitMax = this->AttrDesc[csplitColumn].NoValues;
                }
        }
        /*
         * internal nodes 2*x-1-x = x-1
         * x == all leaf nodes
         */
        const int csplitArrayLen = (treeDepth-1)*currCsplitMax;
        SEXP csplitArray;
        PROTECT(csplitArray = Rf_allocVector(INTSXP, csplitArrayLen));
        nProtected++;
        /*
         * initialize csplit array to 3.
         * 1 is attr. goes to the left
         * 2 not in the split
         * 3 is attr. goes to the right
         * Later we will set it to 1/3 if attr-value is present in the tree
         */
        for(int initCsplit=0; initCsplit < csplitArrayLen; initCsplit++){
                INTEGER(csplitArray)[initCsplit] = 2;
        }
        
        /*
         * current position in csplit table
         */
        int csplitIndex = 1;

        
        /*
         * two columns for each attribute
         * 1. discrete index of current attribute
         * 2. current attribute name
         */
        const int attributeLabelsColumn = 2;
        int attrLabelsSize = attributeLabelsColumn*(treeDepth-1);
        SEXP attributeLabels;
        PROTECT(attributeLabels = Rf_allocVector(STRSXP, attrLabelsSize));
        nProtected++;

        /*
         * final function output
         */
        SEXP out;
        PROTECT(out = Rf_allocVector(VECSXP, 14));
        nProtected++;

        /*
         * auxiliary variables
         */
        SEXP aux;
        PROTECT(aux = Rf_allocVector(REALSXP, 1));
        nProtected++;

        SEXP auxLen;
        PROTECT(auxLen = Rf_allocVector(INTSXP, 1));
        nProtected++;

        
        /*
         * example distribution per node
         */
        SEXP exampleDistr;
        PROTECT(exampleDistr = Rf_allocVector(REALSXP, line*noClasses));
        nProtected++;

        Pbinnode *aIntNode = new Pbinnode[line];

        int *aIntLabel = new int[line];

        binnode *aNode = this->root;

        aIntNode[0] = aNode;
        aIntNode[1] = NULL;
        aIntLabel[0] = 1;

        int addedCount = 1;
        int noLeafCount = -1;
        int noAttributeCount = -1;
        char aStr[128];
        bool nodeChanged;
        for(int i =0; i < line; i++){
                aNode = aIntNode[i];
                nodeChanged = FALSE;
                //n, wt
                aux = Rf_allocVector(REALSXP, 1);
                REAL(aux)[0] = aNode->weight;
                SET_VECTOR_ELT(outMatrix, i*varRealName, aux);
                SET_VECTOR_ELT(outMatrix, i*varRealName+1, aux);
                //dev
                aux = Rf_allocVector(REALSXP, 1);
                double deviation = 0;
                double deviationPerClass = 0.0;
                for(int k = 1; k <= noClasses; k++){
                        deviationPerClass = aNode->Classify[k];
                        if(aNode->majorClass != k)
                        {
                                        deviation += deviationPerClass;
                        }
                        REAL(exampleDistr)[i*noClasses+(k-1)] = (int)(deviationPerClass);
                }
                REAL(aux)[0] = (int)(deviation);
                SET_VECTOR_ELT(outMatrix, i*varRealName+2, aux);
                //yval, yval2
                aux = Rf_allocVector(REALSXP, 1);
                int classValue = aNode->majorClass;
                REAL(aux)[0] = classValue;
                SET_VECTOR_ELT(outMatrix, i*varRealName+3, aux);
                SET_VECTOR_ELT(outMatrix, i*varRealName+7, aux);
                //complexity
                aux = Rf_allocVector(REALSXP, 1);
                REAL(aux)[0] = 0.5;
                SET_VECTOR_ELT(outMatrix, i*varRealName+4, aux);
                if(aNode->Identification != leaf){
                        if(aNode->left != NULL){
                                addedCount = i+1;
                                aIntNode[addedCount] = aNode->left;
                                aIntLabel[addedCount] = 2*aIntLabel[i]+0;
                                addedCount++;
                                
                        }
                        if(aNode->right != NULL){
                                /*
                                 * between the left and right node we need space
                                 * for the entire left subtree:
                                 * 1+(2*n-1), n is the number of leaf node
                         */
                                addedCount = i+2*this->getSize(aNode->left);
                                aIntNode[addedCount] = aNode->right;
                                aIntLabel[addedCount] = 2*aIntLabel[i]+1;
                                addedCount++;
                                
                        }
                        /*
                         * sequence index of a line
                         */
                        snprintf(aStr, 128, "%d", aIntLabel[i]);
                        SET_STRING_ELT(outVectorLabels, i, Rf_mkChar(aStr));
                }
                else{
                        snprintf(aStr, 128, "%d", aIntLabel[i]);
                        /*
                         * outVectorLabels must be of type STRSXP, 
                         * but sometimes it changes to CLOSXP - closure
                         */
                        SET_STRING_ELT(outVectorLabels, i, Rf_mkChar(aStr));
                        SET_STRING_ELT(outVectorLabels, i+line, Rf_mkChar("<leaf>"));
                        char *desc = aNode->Model.descriptionString();
                        SET_STRING_ELT(outVectorLabels, i+2*line, Rf_mkChar(desc));
                        delete [] desc ;
                }
                /*
                 * ncompete, nsurrogate
                 * because odfcumsum and !is.leaf in labels.rpart
                 */
                aux = Rf_allocVector(REALSXP, 1);
                REAL(aux)[0] = 0;
                SET_VECTOR_ELT(outMatrix, i*varRealName+5, aux);
                SET_VECTOR_ELT(outMatrix, i*varRealName+6, aux);
                if(aNode->Identification != leaf){
                        noLeafCount++;
                        /*
                         * splits:count
                         */
                        int atPos = noLeafCount*splitsNames+1;
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = aNode->left->weight;
                        SET_VECTOR_ELT(outSplitMatrix, atPos, aux);
                }

                attribute *anAttribute = NULL;
                int anAttributeIndex = -1;
                if(aNode->Identification == continuousAttribute){
                        anAttributeIndex = this->ContIdx[aNode->Construct.root->attrIdx];
                        anAttribute = &this->AttrDesc[anAttributeIndex];

                        /*
                         * splits:"ncat"
                         */
                        int atPos = noLeafCount*splitsNames;
                        int direction = -1;
                        if(nodeChanged == TRUE){
                                direction = 1;
                        }
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = direction;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+2, aux);
                        /*
                         * splits:"improve, adj"
                         */
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = 0;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+3, aux);
                        SET_VECTOR_ELT(outSplitMatrix, atPos+5, aux);
                        /*
                         * splits:"index"
                         */
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = aNode->Construct.splitValue;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+4, aux);
                }
                if(aNode->Identification == discreteAttribute){
                        anAttributeIndex = this->DiscIdx[aNode->Construct.root->attrIdx];
                        anAttribute = &this->AttrDesc[anAttributeIndex];

                        //splits:"ncat"
                        int atPos = noLeafCount*splitsNames;
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = anAttribute->NoValues;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+2, aux);

                        //splits:"improve, adj"
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = 0;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+3, aux);
                        SET_VECTOR_ELT(outSplitMatrix, atPos+5, aux);

                        //splits:"index, to je index v csplit tabeli!"
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = csplitIndex;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+4, aux);
                        //csplit index
                        int csplitCurrentColumn = (csplitIndex-1)*currCsplitMax;

                        int leftValue = 1;
                        int rigthValue = 3;
                        if(nodeChanged == TRUE){
                                leftValue = 3;
                                rigthValue = 1;
                        }

                        for(int aValue=0; aValue < aNode->Construct.leftValues.len()-1; aValue++){
                                atPos = csplitCurrentColumn+aValue;
                                if(aNode->Construct.leftValues[aValue+1] == mTRUE){
                                        INTEGER(csplitArray)[atPos] = leftValue;
                                }
                                else{
                                        /*
                                         * if not in left values than it must be in the right
                                         */
                                        INTEGER(csplitArray)[atPos] = rigthValue;
                                }
                        }
                        
                        csplitIndex++;
                }
                /*
                 * if not a leaf add it.
                 */
                if(anAttribute != NULL){
                        /*
                         * var line
                         */
                        snprintf(aStr, 128, "%d", anAttributeIndex);
                        SET_STRING_ELT(outVectorLabels, i+line, Rf_mkChar(aStr));
                        /*
                         * splits:"id"
                         */
                        int atPos = noLeafCount*splitsNames;
                        auxLen = Rf_allocVector(INTSXP, 1);
                        INTEGER(auxLen)[0] = anAttributeIndex;
                        SET_VECTOR_ELT(outSplitMatrix, atPos, auxLen);

                        noAttributeCount++;
                        snprintf(aStr, 128, "%d", anAttributeIndex);
                        SET_STRING_ELT(attributeLabels, noAttributeCount*2, Rf_mkChar(aStr));
                        snprintf(aStr, 128, "%s", anAttribute->AttributeName);
                        SET_STRING_ELT(attributeLabels, noAttributeCount*2+1, Rf_mkChar(aStr));
                }
        }
        SET_VECTOR_ELT(out, 0, outMatrix);
        SET_VECTOR_ELT(out, 1, outVector);
        SET_VECTOR_ELT(out, 2, outVectorLabels);
        SET_VECTOR_ELT(out, 5, outSplitMatrix);
        SET_VECTOR_ELT(out, 6, outSplitVector);
        SET_VECTOR_ELT(out, 9, csplitArray);
        SET_VECTOR_ELT(out, 11, attributeLabels);
        

        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = line;
        SET_VECTOR_ELT(out, 3, auxLen);
        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = varNames-1;
        SET_VECTOR_ELT(out, 4, auxLen);
        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = treeDepth-1;
        SET_VECTOR_ELT(out, 7, auxLen);
        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = splitsNames;
        SET_VECTOR_ELT(out, 8, auxLen);
        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = currCsplitMax;
        SET_VECTOR_ELT(out, 10, auxLen);
        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = attributeLabelsColumn;
        SET_VECTOR_ELT(out, 12, auxLen);

        
        
        SET_VECTOR_ELT(out, 13, exampleDistr);
        
        UNPROTECT(nProtected);
        delete [] aIntNode ;
        delete [] aIntLabel ;

        return out;
}
SEXP regressionTree::T2Rpart()
{
        int nProtected = 0;
        /*
         * number of leaf node TODO: try noLeaves if present-it is the same
         */
        int treeDepth = this->getSize(this->root);
        /*
         * all nodes (leaf + inner node - binary tree!)
         */
        int line = 2*treeDepth-1;

        /*
         * frame data, razlozi zakaj ne uporabljas matriko...
         */
        const int varNames = 9;
        int varRealName = varNames-1;
        SEXP outMatrix;
        PROTECT(outMatrix = Rf_allocVector(VECSXP, line*varRealName));
        nProtected++;
        /*
         * frame names
         */
        SEXP outVector;
        PROTECT(outVector = Rf_allocVector(STRSXP, varNames));
        nProtected++;
        /*
         * two colons [sequential index, attribute index]
         */
        SEXP outVectorLabels;
        PROTECT(outVectorLabels = Rf_allocVector(STRSXP, line*3));
        nProtected++;

        //frame names
        SET_STRING_ELT(outVector, 0, Rf_mkChar("var"));
        SET_STRING_ELT(outVector, 1, Rf_mkChar("n"));
        SET_STRING_ELT(outVector, 2, Rf_mkChar("wt"));
        SET_STRING_ELT(outVector, 3, Rf_mkChar("dev"));
        SET_STRING_ELT(outVector, 4, Rf_mkChar("yval"));
        SET_STRING_ELT(outVector, 5, Rf_mkChar("complexity"));
        SET_STRING_ELT(outVector, 6, Rf_mkChar("ncompete"));
        SET_STRING_ELT(outVector, 7, Rf_mkChar("nsurrogate"));
        SET_STRING_ELT(outVector, 8, Rf_mkChar("yval2"));

        
        /*
         * constant number of column names in split table
         */
        const int splitsNames = 6;
        SEXP outSplitVector;
        PROTECT(outSplitVector = Rf_allocVector(STRSXP, splitsNames-1));
        nProtected++;

        /*
         * not leaf nodes. Only inner nodes have an entry in the split matrix
         */
        SEXP outSplitMatrix;
        PROTECT(outSplitMatrix = Rf_allocVector(VECSXP, (treeDepth-1)*splitsNames));
        nProtected++;

        //split names
        SET_STRING_ELT(outSplitVector, 0, Rf_mkChar("count"));
        SET_STRING_ELT(outSplitVector, 1, Rf_mkChar("ncat"));
        SET_STRING_ELT(outSplitVector, 2, Rf_mkChar("improve"));
        SET_STRING_ELT(outSplitVector, 3, Rf_mkChar("index"));
        SET_STRING_ELT(outSplitVector, 4, Rf_mkChar("adj"));
        
        /*
         * csplit array for descrete attributes.
         * rows: for each inner node == possible discrete split
         * column: max levels == max number of values of each attr.
         */
        int currCsplitMax = 0;
        for(int csplitColumn = 0; csplitColumn < this->AttrDesc.len(); csplitColumn++){
                if(currCsplitMax < this->AttrDesc[csplitColumn].NoValues){
                        currCsplitMax = this->AttrDesc[csplitColumn].NoValues;
                }
        }
        /*
         * internal nodes 2*x-1-x = x-1
         * x == all leaf nodes
         */
        const int csplitArrayLen = (treeDepth-1)*currCsplitMax;
        SEXP csplitArray;
        PROTECT(csplitArray = Rf_allocVector(INTSXP, csplitArrayLen));
        nProtected++;
        /*
         * initailize csplit array to 3.
         * 1 is attr. goes to the left
         * 2 not in the split
         * 3 is attr. goes to the right
         * Later we will set it to 1/3 if attr-value is present in the tree
         */
        for(int initCsplit=0; initCsplit < csplitArrayLen; initCsplit++){
                INTEGER(csplitArray)[initCsplit] = 2;
        }
        
        /*
         * current position in csplit table
         */
        int csplitIndex = 1;

        
        /*
         * two columns for each attribute
         * 1. discrete index of current attribute
         * 2. current attribute name
         */
        const int attributeLabelsColumn = 2;
        int attrLabelsSize = attributeLabelsColumn*(treeDepth-1);
        SEXP attributeLabels;
        PROTECT(attributeLabels = Rf_allocVector(STRSXP, attrLabelsSize));
        nProtected++;

        /*
         * final function output
         */
        SEXP out;
        PROTECT(out = Rf_allocVector(VECSXP, 14));
        nProtected++;

        /*
         * auxeliary variables
         */
        SEXP aux;
        PROTECT(aux = Rf_allocVector(REALSXP, 1));
        nProtected++;

        SEXP auxLen;
        PROTECT(auxLen = Rf_allocVector(INTSXP, 1));
        nProtected++;

        PbinnodeReg *aIntNode = new PbinnodeReg[line];
        int *aIntLabel = new int[line];

        binnodeReg *aNode = this->root;

        aIntNode[0] = aNode;
        aIntNode[1] = NULL;
        aIntLabel[0] = 1;

        int addedCount = 1;
        int noLeafCount = -1;
        int noAttributeCount = -1;
        char aStr[128];
        bool nodeChanged;
        for(int i =0; i < line; i++){
                aNode = aIntNode[i];
                nodeChanged = FALSE;
                //n, wt
                aux = Rf_allocVector(REALSXP, 1);
                REAL(aux)[0] = aNode->weight;
                SET_VECTOR_ELT(outMatrix, i*varRealName, aux);
                SET_VECTOR_ELT(outMatrix, i*varRealName+1, aux);
                //dev
                aux = Rf_allocVector(REALSXP, 1);
                double deviation = 0;
                deviation = aNode->MSE;
                REAL(aux)[0] = (int)(deviation);
                SET_VECTOR_ELT(outMatrix, i*varRealName+2, aux);
                //yval, yval2
                aux = Rf_allocVector(REALSXP, 1);
                double classValue = aNode->averageClassValue;
                REAL(aux)[0] = classValue;
                SET_VECTOR_ELT(outMatrix, i*varRealName+3, aux);
                SET_VECTOR_ELT(outMatrix, i*varRealName+7, aux);
                //complexity
                aux = Rf_allocVector(REALSXP, 1);
                REAL(aux)[0] = 0.5;
                SET_VECTOR_ELT(outMatrix, i*varRealName+4, aux);
                if(aNode->Identification != leaf){
                        if(aNode->left != NULL){
                                addedCount = i+1;
                                aIntNode[addedCount] = aNode->left;
                                aIntLabel[addedCount] = 2*aIntLabel[i]+0;
                                addedCount++;
                                
                        }
                        if(aNode->right != NULL){
                                /*
                                 * between the left and right node we need space
                                 * for the entire left subtree:
                                 * 1+(2*n-1), n is the number of leaf node
                         */
                                addedCount = i+2*this->getSize(aNode->left);
                                aIntNode[addedCount] = aNode->right;
                                aIntLabel[addedCount] = 2*aIntLabel[i]+1;
                                addedCount++;
                                
                        }
                        /*
                         * sequence index of a line
                         */
                        snprintf(aStr, 128, "%d", aIntLabel[i]);
                        SET_STRING_ELT(outVectorLabels, i, Rf_mkChar(aStr));
                }
                else{
                        snprintf(aStr, 128, "%d", aIntLabel[i]);
                        /*
                         * outVectorLabels must be of type STRSXP, 
                         * but sometimes it changes to CLOSXP - closure
                         */
                        SET_STRING_ELT(outVectorLabels, i, Rf_mkChar(aStr));
                        SET_STRING_ELT(outVectorLabels, i+line, Rf_mkChar("<leaf>"));
                        char *desc = aNode->Model.descriptionString();
                        SET_STRING_ELT(outVectorLabels, i+2*line, Rf_mkChar(desc));
                        delete [] desc ;
                }
                /*
                 * ncompete, nsurrogate
                 * because odfcumsum and !is.leaf in labels.rpart
                 */
                aux = Rf_allocVector(REALSXP, 1);
                REAL(aux)[0] = 0;
                SET_VECTOR_ELT(outMatrix, i*varRealName+5, aux);
                SET_VECTOR_ELT(outMatrix, i*varRealName+6, aux);
                if(aNode->Identification != leaf){
                        noLeafCount++;
                        /*
                         * splits:count
                         */
                        int atPos = noLeafCount*splitsNames+1;
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = aNode->left->weight;
                        SET_VECTOR_ELT(outSplitMatrix, atPos, aux);
                }

                attribute *anAttribute = NULL;
                int anAttributeIndex = -1;
                if(aNode->Identification == continuousAttribute){
                        anAttributeIndex = this->ContIdx[aNode->Construct.root->attrIdx];
                        anAttribute = &this->AttrDesc[anAttributeIndex];

                        /*
                         * splits:"ncat"
                         */
                        int atPos = noLeafCount*splitsNames;
                        int direction = -1;
                        if(nodeChanged == TRUE){
                                direction = 1;
                        }
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = direction;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+2, aux);
                        /*
                         * splits:"improve, adj"
                         */
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = 0;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+3, aux);
                        SET_VECTOR_ELT(outSplitMatrix, atPos+5, aux);
                        /*
                         * splits:"index"
                         */
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = aNode->Construct.splitValue;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+4, aux);
                }
                if(aNode->Identification == discreteAttribute){
                        anAttributeIndex = this->DiscIdx[aNode->Construct.root->attrIdx];
                        anAttribute = &this->AttrDesc[anAttributeIndex];

                        //splits:"ncat"
                        int atPos = noLeafCount*splitsNames;
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = anAttribute->NoValues;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+2, aux);

                        //splits:"improve, adj"
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = 0;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+3, aux);
                        SET_VECTOR_ELT(outSplitMatrix, atPos+5, aux);

                        //splits:"index, to je index v csplit tabeli!"
                        aux = Rf_allocVector(REALSXP, 1);
                        REAL(aux)[0] = csplitIndex;
                        SET_VECTOR_ELT(outSplitMatrix, atPos+4, aux);
                        //csplit index
                        int csplitCurrentColumn = (csplitIndex-1)*currCsplitMax;

                        int leftValue = 1;
                        int rigthValue = 3;
                        if(nodeChanged == TRUE){
                                leftValue = 3;
                                rigthValue = 1;
                        }
                        /*
                         * no idea why aValue+1
                         */
                        for(int aValue=0; aValue < aNode->Construct.leftValues.len()-1; aValue++){
                                atPos = csplitCurrentColumn+aValue;
                                if(aNode->Construct.leftValues[aValue+1] == mTRUE){
                                        INTEGER(csplitArray)[atPos] = leftValue;
                                }
                                else{
                                        /*
                                         * if not in left values than it must be in the right
                                         */
                                        INTEGER(csplitArray)[atPos] = rigthValue;
                                }
                        }
                        
                        csplitIndex++;
                }
                /*
                 * if not a leaf add it.
                 */
                if(anAttribute != NULL){
                        /*
                         * var line
                         */
                        snprintf(aStr, 128, "%d", anAttributeIndex);
                        SET_STRING_ELT(outVectorLabels, i+line, Rf_mkChar(aStr));
                        /*
                         * splits:"id"
                         */
                        int atPos = noLeafCount*splitsNames;
                        auxLen = Rf_allocVector(INTSXP, 1);
                        INTEGER(auxLen)[0] = anAttributeIndex;
                        SET_VECTOR_ELT(outSplitMatrix, atPos, auxLen);

                        noAttributeCount++;
                        snprintf(aStr, 128, "%d", anAttributeIndex);
                        SET_STRING_ELT(attributeLabels, noAttributeCount*2, Rf_mkChar(aStr));
                        snprintf(aStr, 128, "%s", anAttribute->AttributeName);
                        SET_STRING_ELT(attributeLabels, noAttributeCount*2+1, Rf_mkChar(aStr));
                }
        }
        SET_VECTOR_ELT(out, 0, outMatrix);
        SET_VECTOR_ELT(out, 1, outVector);
        SET_VECTOR_ELT(out, 2, outVectorLabels);
        SET_VECTOR_ELT(out, 5, outSplitMatrix);
        SET_VECTOR_ELT(out, 6, outSplitVector);
        SET_VECTOR_ELT(out, 9, csplitArray);
        SET_VECTOR_ELT(out, 11, attributeLabels);
        

        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = line;
        SET_VECTOR_ELT(out, 3, auxLen);
        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = varNames-1;
        SET_VECTOR_ELT(out, 4, auxLen);
        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = treeDepth-1;
        SET_VECTOR_ELT(out, 7, auxLen);
        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = splitsNames;
        SET_VECTOR_ELT(out, 8, auxLen);
        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = currCsplitMax;
        SET_VECTOR_ELT(out, 10, auxLen);
        auxLen = Rf_allocVector(INTSXP, 1);
        INTEGER(auxLen)[0] = attributeLabelsColumn;
        SET_VECTOR_ELT(out, 12, auxLen);

        
        UNPROTECT(nProtected);
        delete [] aIntNode ;
        delete [] aIntLabel ;

        return out;
}
// clear DTrain array in all the nodes
void featureTree::rfClearDTrain(binnode *branch) {
	if (branch) {
		branch->DTrain.clear() ;
	    rfClearDTrain(branch->left) ;
	    rfClearDTrain(branch->right) ;
	}
}
// store cases passing through nodes in DTrain array
void featureTree::rfMarkCaseInTree(binnode *branch, int caseIdx) {
	if (branch) {
		branch->DTrain.addEndAutoResize(caseIdx) ;

  	    switch (branch->Identification)  {
		        case leaf: break ;
		        case continuousAttribute:
					  {
						double contValue = branch->Construct.continuousValue(*dData, *nData, caseIdx) ;
		                if (isNAcont(contValue))
							contValue = branch->NAnumValue[branch->Construct.root->attrIdx] ;
		                if (contValue <= branch->Construct.splitValue)
		                   rfMarkCaseInTree(branch->left, caseIdx) ;
		                else
		                   rfMarkCaseInTree(branch->right, caseIdx) ;
					  }
					  break ;
				case discreteAttribute:
					   {
		                int discValue = branch->Construct.discreteValue(*dData, *nData, caseIdx) ;
		                if (discValue == NAdisc)
							discValue = branch->NAdiscValue[branch->Construct.root->attrIdx] ;
		                if (branch->Construct.leftValues[discValue])
		                    rfMarkCaseInTree(branch->left, caseIdx) ;
		                else
		                    rfMarkCaseInTree(branch->right, caseIdx) ;
					   }
					   break ;
		        default:
		                merror("featureTree::rfMarkCaseInTree", "invalid branch identification") ;
						return ;
		   }
	}
}

void featureTree::rfLeafCooccurence(binnode *branch,  int outDim, SEXP out) {
	if (branch) {

  	    switch (branch->Identification)  {
		        case leaf:
		        	int i, j ;
		        	for (i=0 ; i < branch->DTrain.filled(); i++)
			        	for (j = i+1 ; j < branch->DTrain.filled(); j++) {
		        		   REAL(out)[ branch->DTrain[i]*outDim + branch->DTrain[j] ] += 1.0 ;
	        		       REAL(out)[ branch->DTrain[j]*outDim + branch->DTrain[i] ] += 1.0 ;
			        	}
		        	break ;
		        case continuousAttribute:
				case discreteAttribute:
				    rfLeafCooccurence(branch->left, outDim, out) ;
		            rfLeafCooccurence(branch->right, outDim, out) ;
					break ;
		        default:
		                merror("featureTree::rrfLeafCooccurence", "invalid branch identification") ;
						return ;
		   }
	}
}



//   Breiman's proximity measure:
//   number of trees where instances are in the same leaf
SEXP featureTree::proximity(bool distance)
{
        int nProtected = 0;

        SEXP out;
        PROTECT(out = Rf_allocMatrix(REALSXP, NoCases, NoCases));
        nProtected++;

        // initialize proximity matrix
        int i, j ;
        for (i=0 ; i < NoCases ; i++) {
            for (j=0 ; j < NoCases ; j++)
            	 REAL(out)[i*NoCases+j] = 0.0 ;
        }

        // make a copy of the forest
        marray<forestTree> forestCopy(forest) ;
        // classify all the cases with all the trees and
        // use DTrain in each node to store cases reaching that node
        for (int iT=0 ; iT < opt->rfNoTrees ; iT++){
        	rfClearDTrain(forestCopy[iT].t.root) ;
            // put all the cases down the tree
            for(int caseIdx = 0; caseIdx < NoCases; caseIdx++)
            	rfMarkCaseInTree(forestCopy[iT].t.root, caseIdx) ;
        }

        // fill in the proximity matrix with the cooccurence data from tree leaves
        for (int iT=0 ; iT < opt->rfNoTrees ; iT++)
           rfLeafCooccurence(forestCopy[iT].t.root, NoCases, out) ;

        for (i=0 ; i < NoCases ; i++) {
            for (j=0 ; j < NoCases ; j++)
           	    REAL(out)[i*NoCases+j] /= opt->rfNoTrees ;
        	REAL(out)[i*NoCases+i] = 1.0 ; // diagonal
        }

        UNPROTECT(nProtected);
        return out;
}

SEXP featureTree::proximityM(bool distance)
{
        int nProtected = 0;

        SEXP out;
        PROTECT(out = Rf_allocMatrix(REALSXP, NoCases, NoCases));
        nProtected++;

        marray<IntSortRec> near(NoCases) ;
        //   use Breiman's proximity measure:
        //   number of trees where instances are in the same leaf
        marray<double> distr(noClasses+1) ;
        for(int caseIdx = 0; caseIdx < NoCases; caseIdx++){
                for (int i=0 ; i < NoCases ; i++){
                        near[i].key = 0 ;
                        near[i].value = i ;
                }
                for (int iT=0 ; iT < opt->rfNoTrees ; iT++){
                        rfFindNearInTree(forest[iT].t.root, caseIdx, near) ;
                }
                for (int i=0 ; i < NoCases ; i++){
                        if(caseIdx == i){
                                /*
                                 * diagonal is 1
                                 */
                                double diag = 1.0;
                                if(distance){
                                        diag = 0.0;
                                }
                                REAL(out)[caseIdx*(NoCases+1)] = diag;
                        }
                        else{
                                REAL(out)[caseIdx*NoCases+i] = near[i].key/ 2.0 / opt->rfNoTrees; // !!! check if division  by 2 is still necessary
                        }
                }
        }
        /*
         * we need a symmetric matrix
         */
        double aValue = 0.0;
        for (int i=0 ; i < NoCases*NoCases ; i++){
                /*
                 * out[a,b]
                 */
                int a = i % NoCases;
                int b = i / NoCases ;
                if(a > b){
                        aValue = (REAL(out)[b*NoCases+a] + REAL(out)[a*NoCases+b])/2;
                        if(distance){
                                aValue = sqrt(1-aValue);
                        }
                        REAL(out)[b*NoCases+a] = aValue;
                        REAL(out)[a*NoCases+b] = aValue;
                }
        }
        UNPROTECT(nProtected);
        return out;
}
#endif
int regressionTree::getSize(binnodeReg *branch)
{
        if (branch->Identification==leaf)
                return 1;
        else
                return (getSize(branch->left) + getSize(branch->right));
}
void featureTree::oobEvaluateCluster(mmatrix<int> &oob, marray<booleanT> &cluster)
{
   marray<double> distr(noClasses+1) ;
   int iT, i, max ;
   oob.init(0) ;
   for (iT = 0 ; iT < opt->rfNoTrees ; iT++){
         for (i=0 ; i < NoTrainCases ; i++)
                if (cluster[i] && forest[iT].oob[i]){
                        max = rfTreeCheck(forest[iT].t.root, DTraining[i], distr) ;
                        oob(i, max)++ ;
                }
   }
}
void featureTree::varImportanceCluster(marray<double> &varEval, marray<booleanT> &cluster){
    marray<int> discOrig(NoCases), discTemp(NoCases) ;
        marray<double> contOrig(NoCases), contTemp(NoCases) ;
        discOrig.setFilled(NoCases) ;
        discTemp.setFilled(NoCases) ;
        contOrig.setFilled(NoCases) ;
        contTemp.setFilled(NoCases) ;
        mmatrix<int> oob(NoTrainCases, noClasses+1) ;
    marray<int> maxOther(NoTrainCases) ; // dummy placeholder
    double varMargin ; // dummy placeholder
        for (int iA = 1 ; iA <= noAttr ; iA++) {
                if (AttrDesc[iA].continuous) {
                // save original values of instances and reschuffle it
                        NumData.outColumn(AttrDesc[iA].tablePlace, contOrig);
                        contTemp.copy(contOrig);
                        contTemp.shuffle();
                        NumData.inColumn(contTemp, AttrDesc[iA].tablePlace);
                }
                else {
                        DiscData.outColumn(AttrDesc[iA].tablePlace, discOrig);
                        discTemp.copy(discOrig);
                        discTemp.shuffle();
                        DiscData.inColumn(discTemp, AttrDesc[iA].tablePlace);
                }
            // compute margin
            oobEvaluateCluster(oob, cluster) ;
            varEval[iA-1] = avgOobMargin - oobMargin(oob, maxOther, varMargin) ;

                if (AttrDesc[iA].continuous)
                        NumData.inColumn(contOrig, AttrDesc[iA].tablePlace);
                else
                        DiscData.inColumn(discOrig, AttrDesc[iA].tablePlace);

        }
}
#if defined(R_PORT)
SEXP featureTree::importance2RCluster(marray<double> &varEval, marray<booleanT> &cluster)
{
        int nProtected = 0;

        SEXP out;
        PROTECT(out = Rf_allocVector(REALSXP, noAttr));
        nProtected++;

        varImportanceCluster(varEval, cluster);

        for (int i=1 ; i <= noAttr ; i++){
                REAL(out)[i-1] = varEval[i-1];
        }

        UNPROTECT(nProtected);
        return out;
}
#endif
