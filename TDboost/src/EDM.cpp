//  TDboost by Yi Yang and Hui Zou  Copyright (C) 2012
#include "EDM.h"

CEDM::CEDM(double dAlpha)
{
    this->dAlpha = dAlpha;
}

CEDM::~CEDM()
{

}

// compute gradient function
TDboostRESULT CEDM::ComputeWorkingResponse 
(
    double *adY,
    double *adMisc,
    double *adOffset,
    double *adF, 
    double *adZ, 
    double *adWeight,
    bool *afInBag,
    unsigned long nTrain
)
{
	TDboostRESULT hr = TDboost_OK;
    unsigned long i = 0;
    double dF = 0.0;
    
    if((adY == NULL) || (adF == NULL) || (adZ == NULL) || (adWeight == NULL))
    {
        hr = TDboost_INVALIDARG;
        goto Error;
    }
    if(adOffset == NULL)
    {
        for(i=0; i<nTrain; i++)
        {
			adZ[i] = -adY[i] * exp((1.0-dAlpha)*adF[i]) + exp((2.0-dAlpha)*adF[i]);
        }
    }
    else
    {
        for(i=0; i<nTrain; i++)
        {
			dF = adF[i] + adOffset[i];
			adZ[i] = -adY[i] * exp((1.0-dAlpha)*dF) + exp((2.0-dAlpha)*dF);
        }
    }

Cleanup:
    return hr;
Error:
    goto Cleanup;
}




// compute likelihood function
double CEDM::Deviance
(
    double *adY,
    double *adMisc,
    double *adOffset, 
    double *adWeight,
    double *adF,
    unsigned long cLength
)
{
    unsigned long i=0;
    double dL = 0.0;
    double dW = 0.0;
    double dF = 0.0;
    
	if(dAlpha == 2.0){
		if(adOffset == NULL){
	      	for(i=0; i<cLength; i++){
	         	dL += adWeight[i]*(adY[i] * exp(-adF[i]) + adF[i]);
	         	dW += adWeight[i];
	      	}
	    }
		else{
	      	for(i=0; i<cLength; i++){
	         	dF = adF[i] + adOffset[i];
	         	dL += adWeight[i]*(adY[i] * exp(-dF) + dF);
	         	dW += adWeight[i];
	      	}
	    }
	}
	else{
		if(adOffset == NULL){
	      	for(i=0; i<cLength; i++){
	         	dL += adWeight[i]*(-adY[i] * exp((1.0-dAlpha)*adF[i])/(1.0-dAlpha) + exp((2.0-dAlpha)*adF[i])/(2.0-dAlpha));
	         	dW += adWeight[i];
	      	}
	    }
		else{
	      	for(i=0; i<cLength; i++){
	         	dF = adF[i] + adOffset[i];
	         	dL += adWeight[i]*(-adY[i] * exp((1.0-dAlpha)*dF)/(1.0-dAlpha) + exp((2.0-dAlpha)*dF)/(2.0-dAlpha));
	         	dW += adWeight[i];
	      	}
	    }	
	}
	
    return dL/dW;
}





// compute Initial value
TDboostRESULT CEDM::InitF
(
    double *adY,
    double *adMisc,
    double *adOffset,
    double *adWeight,
    double &dInitF, 
    unsigned long cLength
)
{
    double dSum = 0.0;
    double dDenom = 0.0;
    unsigned long i = 0;

    if(adOffset == NULL)
    {
        for(i=0; i<cLength; i++)
        {
            dSum += adWeight[i]*adY[i];
            dDenom += adWeight[i];
        }
    }
    else
    {
        for(i=0; i<cLength; i++)
        {
            dSum += adWeight[i] * adY[i] * exp((1-dAlpha) * adOffset[i]);
            dDenom += adWeight[i] * exp((2-dAlpha) * adOffset[i]);
        }
    }

    dInitF = log(dSum/dDenom);

	return TDboost_OK;

}






TDboostRESULT CEDM::FitBestConstant
(
    double *adY,
    double *adMisc,
    double *adOffset,
    double *adW,
    double *adF,
    double *adZ,
    unsigned long *aiNodeAssign,
    unsigned long nTrain,
    VEC_P_NODETERMINAL vecpTermNodes,
    unsigned long cTermNodes,
    unsigned long cMinObsInNode,
    bool *afInBag,
    double *adFadj
)
{

    TDboostRESULT hr = TDboost_OK;
    double dF = 0.0;
    unsigned long iObs = 0;
    unsigned long iNode = 0;
    vector<double> vecdNum;	
    vector<double> vecdDen;
    vector<double> vecdMax;	
    vector<double> vecdMin;
    vecdNum.resize(cTermNodes);
    vecdNum.assign(vecdNum.size(),0.0);
    vecdDen.resize(cTermNodes);
    vecdDen.assign(vecdDen.size(),0.0);
    
    vecdMax.resize(cTermNodes);
    vecdMax.assign(vecdMax.size(),-HUGE_VAL);
    vecdMin.resize(cTermNodes);
    vecdMin.assign(vecdMin.size(),HUGE_VAL);

    if(adOffset == NULL)
    {
        for(iObs=0; iObs<nTrain; iObs++)
        {
            if(afInBag[iObs])
            {
                vecdNum[aiNodeAssign[iObs]] += adW[iObs]*adY[iObs]*exp((1-dAlpha)*adF[iObs]);
                vecdDen[aiNodeAssign[iObs]] += adW[iObs]*exp((2-dAlpha)*adF[iObs]);
            }
            vecdMax[aiNodeAssign[iObs]] = 
               fmax2(adF[iObs],vecdMax[aiNodeAssign[iObs]]);
            vecdMin[aiNodeAssign[iObs]] =  
               fmin2(adF[iObs],vecdMin[aiNodeAssign[iObs]]);
        }
    }
    else
    {
        for(iObs=0; iObs<nTrain; iObs++)
        {
            if(afInBag[iObs])
            {
				dF = adOffset[iObs]+adF[iObs];
                vecdNum[aiNodeAssign[iObs]] += adW[iObs]*adY[iObs]*exp((1-dAlpha)*dF);
                vecdDen[aiNodeAssign[iObs]] += adW[iObs]*exp((2-dAlpha)*dF);
            }
        }        
    }
    for(iNode=0; iNode<cTermNodes; iNode++)
    {
        if(vecpTermNodes[iNode]!=NULL)
        {
            if(vecdNum[iNode] == 0.0)
            {
                // DEBUG: if vecdNum==0 then prediction = -Inf
                // Not sure what else to do except plug in an arbitrary
                //   negative number, -1? -10? Let's use -1, then make
                //   sure |adF| < 19 always.
                vecpTermNodes[iNode]->dPrediction = -19.0;
            }
            else if(vecdDen[iNode] == 0.0)
            {
                vecpTermNodes[iNode]->dPrediction = 0.0;
            }            
            else
            {
                vecpTermNodes[iNode]->dPrediction = 
                    log(vecdNum[iNode]/vecdDen[iNode]);
            }
            vecpTermNodes[iNode]->dPrediction = 
               fmin2(vecpTermNodes[iNode]->dPrediction,
                     19-vecdMax[iNode]);
            vecpTermNodes[iNode]->dPrediction = 
               fmax2(vecpTermNodes[iNode]->dPrediction,
                     -19-vecdMin[iNode]);
        }
    }

    return hr;
}

// compute likelihood improvement after updates
double CEDM::BagImprovement
(
    double *adY,
    double *adMisc,
    double *adOffset,
    double *adWeight,
    double *adF,
    double *adFadj,
    bool *afInBag,
    double dStepSize,
    unsigned long nTrain
)
{
    double dL = 0.0;
    double dLadj = 0.0;
	double dW = 0.0;
    unsigned long i = 0;
    double dF = 0.0;
	double ddF = 0.0;

	if(dAlpha == 2.0){
		for(i=0; i<nTrain; i++){
	        if(!afInBag[i]){
				dF = adF[i] + ((adOffset==NULL) ? 0.0 : adOffset[i]);
				ddF = dF + dStepSize*adFadj[i];
	         	dL += adWeight[i] * (adY[i] * exp(-dF) + dF);
	         	dLadj += adWeight[i] * (adY[i] * exp(-ddF) + ddF);    
	            dW += adWeight[i];
	        }
	    }
	}
	else{
		for(i=0; i<nTrain; i++){
	        if(!afInBag[i]){
				dF = adF[i] + ((adOffset==NULL) ? 0.0 : adOffset[i]);
				ddF = dF + dStepSize*adFadj[i];
	         	dL += adWeight[i] * (-adY[i] * exp((1.0-dAlpha) * dF) / (1.0-dAlpha) + exp((2.0-dAlpha)* dF) / (2.0-dAlpha));
	         	dLadj += adWeight[i] * (-adY[i] * exp((1.0-dAlpha) * ddF) / (1.0-dAlpha) + exp((2.0-dAlpha)* ddF) / (2.0-dAlpha));    
	            dW += adWeight[i];
	        }
	    }
	}
	
    return (dL-dLadj)/dW;
}
