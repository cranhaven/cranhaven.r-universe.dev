//  erboost by Yi Yang and Hui Zou  Copyright (C) 2012
#include "expectile.h"

CExpectile::CExpectile(double dAlpha)
{
    this->dAlpha = dAlpha;
}

CExpectile::~CExpectile()
{

}


erboostRESULT CExpectile::ComputeWorkingResponse
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
	erboostRESULT hr = erboost_OK;
    unsigned long i = 0;
    
    if((adY == NULL) || (adF == NULL) || (adZ == NULL) || (adWeight == NULL))
    {
        hr = erboost_INVALIDARG;
        goto Error;
    }
    
    if(adOffset == NULL)
    {
		for(i=0; i<nTrain; i++)
	    {
			adZ[i] = (adY[i] > adF[i]) ? dAlpha*(adY[i]-adF[i]) : (1.0-dAlpha)*(adY[i]-adF[i]);
	    }
    }
	else
	{
		for(i=0; i<nTrain; i++)
	    {
			adZ[i] = (adY[i] > adF[i]+adOffset[i]) ? dAlpha*(adY[i]-adOffset[i]-adF[i]) : (1.0-dAlpha)*(adY[i]-adOffset[i]-adF[i]);
	    }
	}
    
Cleanup:
    return hr;
Error:
    goto Cleanup;
}





double CExpectile::Deviance
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
    
	if(adOffset == NULL)
    {
        for(i=0; i<cLength; i++)
        {
            if(adY[i] > adF[i])
            {
                dL += adWeight[i]*dAlpha*(adY[i] - adF[i])*(adY[i] - adF[i]);
            }
            else
            {
                dL += adWeight[i]*(1.0-dAlpha)*(adY[i] - adF[i])*(adY[i] - adF[i]);
            }
            dW += adWeight[i];
        }
    }
	else
	{
        for(i=0; i<cLength; i++)
        {
            if(adY[i] > adF[i] + adOffset[i])
            {
                dL += adWeight[i]*dAlpha*(adY[i]-adOffset[i]-adF[i])*(adY[i]-adOffset[i]-adF[i]);
            }
            else
            {
                dL += adWeight[i]*(1.0-dAlpha)*(adY[i]-adOffset[i]-adF[i])*(adY[i]-adOffset[i]-adF[i]);
            }
            dW += adWeight[i];
        }
    }
	
    return dL/dW;
}






erboostRESULT CExpectile::InitF
(
    double *adY,
    double *adMisc,
    double *adOffset,
    double *adWeight,
    double &dInitF, 
    unsigned long cLength
)
{
        double dOffset=0.0;

        vector<double> Yorder;
        Yorder.resize(cLength);
        Yorder.assign(Yorder.size(),0.0);

        for(unsigned long i=0;i<cLength;i++) 
        {
				dOffset = (adOffset==NULL) ? 0.0 : adOffset[i];
                Yorder[i] = adY[i] - dOffset;
        }

        sort(Yorder.begin(),Yorder.end());
        double Deviance;
        double test;
        unsigned long begin=0;
        unsigned long end=cLength-1;

             while (end>begin+1) 
             {
                     Deviance=0;
                     test=floor(double(begin+end)/2.0);
                     for (unsigned long j=0; j<test; j++)
                     {
                             Deviance += (1.0-dAlpha)*(Yorder[j]-Yorder[test]);
                     }

                     for (unsigned long j=cLength-1;j>test;j--) 
                     {
                             Deviance += dAlpha*(Yorder[j]-Yorder[test]);
                     }

                     if(Deviance>0) 
                     {
                             begin=test;
                     }

                     else
                     {
                             end=test;
                     }
             }

             double Pnum=0;
             for (unsigned long j=0;j<(begin+1);j++) 
             {
                     Pnum += (1.0-dAlpha)*(Yorder[j]);
             }

             for (unsigned long j=cLength-1;j>(end-1);j--) 
             {
                     Pnum += dAlpha*(Yorder[j]);
             }

             if (((1.0-dAlpha)*end + dAlpha*(cLength-end))==0)
             {
                     Pnum=0.0;  
             }
             else
             {
                     Pnum=Pnum / ((1.0-dAlpha)*end + dAlpha*(cLength-end));
             }       	
	
        dInitF = Pnum;
   
        return erboost_OK;

}






erboostRESULT CExpectile::FitBestConstant
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

    erboostRESULT hr = erboost_OK;
    
    unsigned long iObs = 0;
    unsigned long iNode = 0;
    vector<vector<double> > vecadDiff;	
    vecadDiff.resize(cTermNodes);
	double dOffset;

    for(iObs=0; iObs<nTrain; iObs++)
    {
            if(afInBag[iObs])
            {
					dOffset = (adOffset==NULL) ? 0.0 : adOffset[iObs];
                    vecadDiff[aiNodeAssign[iObs]].push_back(adY[iObs]-dOffset-adF[iObs]) ;            
            }
    }
    
    
    for(iNode=0; iNode<cTermNodes; iNode++)
    {
        if(vecadDiff[iNode].size()!=0)
        {
                sort(vecadDiff[iNode].begin(),vecadDiff[iNode].end());
                double Deviance;
                double test;
                unsigned long Len = vecadDiff[iNode].size();
                unsigned long begin=0;
                unsigned long end=Len-1;

                     while (end>begin+1) 
                     {
                             Deviance=0;
                             test=floor(double(begin+end)/2.0);
                             for (unsigned long j=0; j<test; j++)
                             {
                                     Deviance += (1.0-dAlpha)*(vecadDiff[iNode][j]-vecadDiff[iNode][test]);
                             }

                             for (unsigned long j=Len-1;j>test;j--) 
                             {
                                     Deviance += dAlpha*(vecadDiff[iNode][j]-vecadDiff[iNode][test]);
                             }

                             if(Deviance>0) 
                             {
                                     begin=test;
                             }

                             else
                             {
                                     end=test;
                             }
                     }

                     double Pnum=0;
                     for (unsigned long j=0;j<(begin+1);j++) 
                     {
                             Pnum += (1.0-dAlpha)*(vecadDiff[iNode][j]);
                     }

                     for (unsigned long j=Len-1;j>(end-1);j--) 
                     {
                             Pnum += dAlpha*(vecadDiff[iNode][j]);
                     }

                     if (((1.0-dAlpha)*end + dAlpha*(Len-end))==0)
                     {
                             Pnum=0.0;  
                     }
                     else
                     {
                             Pnum=Pnum / ((1.0-dAlpha)*end + dAlpha*(Len-end));
                     }       	

                vecpTermNodes[iNode] ->dPrediction = Pnum;
                        
         }
    }	

    return hr;
}


double CExpectile::BagImprovement
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

    for(i=0; i<nTrain; i++)
    {
        if(!afInBag[i])
        {
			dF = adF[i] + ((adOffset==NULL) ? 0.0 : adOffset[i]);
			if(adY[i] > dF)
            {
                dL += adWeight[i]*dAlpha*(adY[i]-dF)*(adY[i]-dF);
            }
            else
            {
                dL += adWeight[i]*(1.0-dAlpha)*(adY[i]-dF)*(adY[i]-dF);
            }
			
			if(adY[i] > (dF+dStepSize*adFadj[i]))
			{
                dLadj += adWeight[i]*dAlpha*(adY[i]-dF-dStepSize*adFadj[i])*(adY[i]-dF-dStepSize*adFadj[i]);
            }
            else
            {
                dLadj += adWeight[i]*(1.0-dAlpha)*(adY[i]-dF-dStepSize*adFadj[i])*(adY[i]-dF-dStepSize*adFadj[i]);
            }
            dW += adWeight[i];
        }
    }

    return (dL-dLadj)/dW;
}
