#include <Rcpp.h>
#include <string>

using namespace std;
using namespace Rcpp;

void quickSort(double** number, int left, int right) {
  if(left < right) {
    double s = number[0][(int)(left+right)/2];
    int i = left - 1;
    int j = right + 1;
    
    while(1) {
      while(number[0][++i] > s) ;
      while(number[0][--j] < s) ;
      if(i >= j)
        break;
      double tempDouble1 = number[0][i];
      double tempDouble2 = number[1][i];
      number[0][i] = number[0][j];
      number[0][j] = tempDouble1;
      number[1][i] = number[1][j];
      number[1][j] = tempDouble2;
    }
    
    quickSort(number, left, i-1);
    quickSort(number, j+1, right);
  }
}

void quickSort4(double** number, int left, int right) {
  if(left < right) {
    double s = number[0][(int)(left+right)/2];
    int i = left - 1;
    int j = right + 1;
    
    while(1) {
      while(number[0][++i] < s) ;
      while(number[0][--j] > s) ;
      if(i >= j)
        break;
      double tempDouble1 = number[0][i];
      double tempDouble2 = number[1][i];
      double tempDouble3 = number[2][i];
      double tempDouble4 = number[3][i];
      number[0][i] = number[0][j];
      number[0][j] = tempDouble1;
      number[1][i] = number[1][j];
      number[1][j] = tempDouble2;
      number[2][i] = number[2][j];
      number[2][j] = tempDouble3;
      number[3][i] = number[3][j];
      number[3][j] = tempDouble4;
    }
    
    quickSort4(number, left, i-1);
    quickSort4(number, j+1, right);
  }
}

// [[Rcpp::export]]
NumericVector FindPeak(NumericVector pv, NumericVector maX, NumericVector maY, IntegerVector pointResult,double xBindBoundary,double yBindBoundary,double xFindBindBoundary,double yFindBindBoundary,double conPv)
{
  int pointLength = pv.length();
  
  double maxValue = 0;
  int maxLocation = 0;
  int maxCount = 0;
  
  double maxLa = 0;
  
  for(int i=0; i<pointLength; i++)
  {
    if(pv[i] > conPv)
      maxCount++;
    if(maY[i] > maxLa)
      maxLa = maY[i];
  }
  
  double** maxMatrix = new double*[2];
  maxMatrix[0] = new double[maxCount];
  maxMatrix[1] = new double[maxCount];
  int addMax = 0;
  for(int i=0; i<pointLength; i++)
  {
    if(pv[i] > conPv)
    {
      maxMatrix[0][addMax] = pv[i];
      maxMatrix[1][addMax] = i;
      addMax++;
    }
  }
  quickSort(maxMatrix,0,maxCount-1);
  addMax = 0;
  maxValue = maxMatrix[0][addMax];
  maxLocation = (int)maxMatrix[1][addMax];
  
  bool bExpand = true;
  
  int iFindLeft = maxLocation;
  int iFindRight = maxLocation;
  
  double xBind = 0;
  double yBind = 0;
  
  double maxTopBind = maxValue;
  double maxBottomBind = maxValue;
  double maxLeftBind = maxValue;
  double maxRightBind = maxValue;
  
  bool topExp = true;
  bool bottomExp = true;
  bool leftExp = true;
  bool rightExp = true;
  
  bool topCover = false;
  bool bottomCover = false;
  bool leftCover = false;
  bool rightCover = false;
  
  double* topDelete = new double[pointLength];
  double* bottomDelete = new double[pointLength];
  double* leftDelete = new double[pointLength];
  double* rightDelete = new double[pointLength];
  
  int* vtopDelete = new int[pointLength];
  int* vbottomDelete = new int[pointLength];
  int* vleftDelete = new int[pointLength];
  int* vrightDelete = new int[pointLength];
  
  int* peakLocation = new int[maxCount];
  
  int topDeNum = 0;
  int botDeNum = 0;
  int lefDeNum = 0;
  int rigDeNum = 0;
  
  int peakNum =0;
  
  double k_yBind = (yFindBindBoundary-5)*5/900;
  
  double oriYFindBindBoundary = yFindBindBoundary;
  
  while(addMax < maxCount)
  {
    if(maxLocation > 500) {
      yFindBindBoundary = k_yBind/(maX[maxLocation] - maX[maxLocation-500]) + 5;
    }
    else if(pointLength - maxLocation > 500) {
      yFindBindBoundary = k_yBind/(maX[maxLocation+500] - maX[maxLocation]) + 5;
    }
    if(yFindBindBoundary > oriYFindBindBoundary)
      yFindBindBoundary = oriYFindBindBoundary;
    pointResult[maxLocation] = 1;
    pv[maxLocation] = -1;
    bExpand = true;
    iFindLeft = maxLocation;
    iFindRight = maxLocation;
    peakLocation[peakNum] = maxLocation;
    
    xBind = 0;
    yBind = 0;
    
    maxTopBind = maxValue;
    maxBottomBind = maxValue;
    maxLeftBind = maxValue;
    maxRightBind = maxValue;
    
    topExp = true;
    bottomExp = true;
    leftExp = true;
    rightExp = true;
    
    topCover = false;
    bottomCover = false;
    leftCover = false;
    rightCover = false;
    
    topDeNum = 0;
    botDeNum = 0;
    lefDeNum = 0;
    rigDeNum = 0;
    
    // Find the exact top, bottom, left and right point of each kernal point.
    double locTopMax = maY[maxLocation], locBottomMin = maY[maxLocation], tmpLocTopMax = maY[maxLocation], tmpLocBottomMin = maY[maxLocation];
    double locRightMax = maX[maxLocation], locLeftMin = maX[maxLocation], tmpLocRightMax = maX[maxLocation], tmpLocLeftMin = maX[maxLocation];
    
    while(bExpand == true && iFindLeft >= 0 && iFindRight <= pointLength)
    {
      topDeNum = 0;
      botDeNum = 0;
      lefDeNum = 0;
      rigDeNum = 0;
      
      iFindLeft = maxLocation-1;
      iFindRight = maxLocation+1;
      
      double tempTopMax = 0;
      double tempBottomMax = 0;
      double tempLeftMax = 0;
      double tempRightMax = 0;
      
      if(xBind > xBindBoundary && yBind > yBindBoundary)
      {
        if(topExp == true)
        {
          topDelete[peakNum] = tmpLocTopMax - maY[maxLocation];
        }
        if(bottomExp == true)
        {
          bottomDelete[peakNum] = maY[maxLocation] - tmpLocBottomMin;
        }
        if(leftExp == true)
        {
          leftDelete[peakNum] = maX[maxLocation] - tmpLocLeftMin;
        }
        if(rightExp == true)
        {
          rightDelete[peakNum] = tmpLocRightMax - maX[maxLocation];
        }
        break;
      }
      
      // Record the exact bound point
      if(topExp == true)
      {
        locTopMax = tmpLocTopMax;
      }
      if(bottomExp == true)
      {
        locBottomMin = tmpLocBottomMin;
      }
      if(leftExp == true)
      {
        locLeftMin = tmpLocLeftMin;
      }
      if(rightExp == true)
      {
        locRightMax = tmpLocRightMax;
      }
      
      if(xBind > xBindBoundary)
      {
        yBind = yBind + yFindBindBoundary;
      }
      else if(yBind > yBindBoundary)
      {
        xBind = xBind + xFindBindBoundary;
      }
      else
      {
        xBind = xBind + xFindBindBoundary;
        yBind = yBind + yFindBindBoundary;
      }
      
      if(topCover == true)
      {
        if(topExp == true)
        {
          /*
          if((maY[maxLocation] + yBind - yFindBindBoundary) < maxLa)
          topDelete[peakNum] = yBind - yFindBindBoundary;
          else
          topDelete[peakNum] = maxLa - maY[maxLocation];
          */
          topDelete[peakNum] = locTopMax - maY[maxLocation];
        }
        topExp = false;
      }
      if(bottomCover == true)
      {
        if(bottomExp == true)
        {
          /*
          if(maY[maxLocation] - yBind + yFindBindBoundary > 0)
          bottomDelete[peakNum] = yBind - yFindBindBoundary;
          else
          bottomDelete[peakNum] = maY[maxLocation];
          */
          bottomDelete[peakNum] = maY[maxLocation] - locBottomMin;
        }
        bottomExp = false;
      }
      if(leftCover == true)
      {
        leftExp = false;
        //leftDelete[peakNum] = xBind - xFindBindBoundary;
        leftDelete[peakNum] = maX[maxLocation] - locLeftMin;
      }
      if(rightCover == true)
      {
        rightExp = false;
        //rightDelete[peakNum] = xBind - xFindBindBoundary;
        rightDelete[peakNum] = locRightMax - maX[maxLocation];
      }
      
      while(iFindLeft-1 >= 0 && maX[maxLocation] - maX[iFindLeft] < xBind)
      {
        if(maY[maxLocation] - maY[iFindLeft] > -yBind && maY[maxLocation] - maY[iFindLeft] <= -yBind + yFindBindBoundary && topExp == true)
        {
          if(pv[iFindLeft] == -1)
          {
            topCover = true;
          }
          
          if(pv[iFindLeft] > tempTopMax)
          {
            tempTopMax = pv[iFindLeft];
          }
          if(pv[iFindLeft] > maxTopBind)
          {
            topExp = false;
            for(int j=0; j<topDeNum; j++)
            {
              pv[topDelete[j+peakNum]] = vtopDelete[j];
            }
            //topDelete[peakNum] = yBind - yFindBindBoundary;
            topDelete[peakNum] = locTopMax - maY[maxLocation];
          }
          else
          {
            if(maY[iFindLeft] > tmpLocTopMax && pv[iFindLeft] != -1)
              tmpLocTopMax = maY[iFindLeft];
            topDelete[topDeNum+peakNum] = iFindLeft;
            vtopDelete[topDeNum] = pv[iFindLeft];
            pv[iFindLeft] = -1;
            topDeNum++;
          }
        }
        
        else if(maY[maxLocation] - maY[iFindLeft] < yBind && maY[maxLocation] - maY[iFindLeft] >= yBind - yFindBindBoundary && bottomExp == true)
        {
          if(pv[iFindLeft] == -1)
          {
            bottomCover = true;
          }
          
          if(pv[iFindLeft] > tempBottomMax)
          {
            tempBottomMax = pv[iFindLeft];
          }
          if(pv[iFindLeft] > maxBottomBind)
          {
            bottomExp = false;
            for(int j=0; j<botDeNum; j++)
            {
              pv[bottomDelete[j+peakNum]] = vbottomDelete[j];
            }
            //bottomDelete[peakNum] = yBind - yFindBindBoundary;
            bottomDelete[peakNum] = maY[maxLocation] - locBottomMin;
          }
          else
          {
            if(maY[iFindLeft] < tmpLocBottomMin && pv[iFindLeft] != -1)
              tmpLocBottomMin = maY[iFindLeft];
            bottomDelete[botDeNum+peakNum] = iFindLeft;
            vbottomDelete[botDeNum] = pv[iFindLeft];
            pv[iFindLeft] = -1;
            botDeNum++;
          }
        }
        
        else if(maX[maxLocation] - maX[iFindLeft] >= xBind - xFindBindBoundary && maY[maxLocation] - maY[iFindLeft] < yBind && maY[maxLocation] - maY[iFindLeft] > -yBind && leftExp == true)
        {
          if(pv[iFindLeft] == -1)
          {
            leftCover = true;
          }
          
          if(pv[iFindLeft] > tempLeftMax)
          {
            tempLeftMax = pv[iFindLeft];
          }
          if(pv[iFindLeft] > maxLeftBind)
          {
            leftExp = false;
            for(int j=0; j<lefDeNum; j++)
            {
              pv[leftDelete[j+peakNum]] = vleftDelete[j];
            }
            //leftDelete[peakNum] = xBind - xFindBindBoundary;
            leftDelete[peakNum] = maX[maxLocation] - locLeftMin;
          }
          else
          {
            if(maX[iFindLeft] < tmpLocLeftMin)
              tmpLocLeftMin = maX[iFindLeft];
            leftDelete[lefDeNum+peakNum] = iFindLeft;
            vleftDelete[lefDeNum] = pv[iFindLeft];
            pv[iFindLeft] = -1;
            lefDeNum++;
          }
        }
        iFindLeft = iFindLeft - 1;
      }
      
      while(iFindRight+1 <= pointLength && maX[iFindRight] - maX[maxLocation]  < xBind)
      {
        if(maY[maxLocation] - maY[iFindRight] > -yBind && maY[maxLocation] - maY[iFindRight] <= -yBind + yFindBindBoundary && topExp == true)
        {
          if(pv[iFindRight] == -1)
          {
            topCover = true;
          }
          
          if(pv[iFindRight] > tempTopMax)
          {
            tempTopMax = pv[iFindRight];
          }
          if(pv[iFindRight] > maxTopBind)
          {
            topExp = false;
            for(int j=0; j<topDeNum; j++)
            {
              pv[topDelete[j+peakNum]] = vtopDelete[j];
            }
            //topDelete[peakNum] = yBind - yFindBindBoundary;
            topDelete[peakNum] = locTopMax - maY[maxLocation];
          }
          else
          {
            if(maY[iFindRight] > tmpLocTopMax && pv[iFindRight] != -1)
              tmpLocTopMax = maY[iFindRight];
            topDelete[topDeNum+peakNum] = iFindRight;
            vtopDelete[topDeNum] = pv[iFindRight];
            pv[iFindRight] = -1;
            topDeNum++;
          }
        }
        else if(maY[maxLocation] - maY[iFindRight] < yBind && maY[maxLocation] - maY[iFindRight] >= yBind - yFindBindBoundary && bottomExp == true)
        {
          if(pv[iFindRight] == -1)
          {
            bottomCover = true;
          }
          
          if(pv[iFindRight] > tempBottomMax)
          {
            tempBottomMax = pv[iFindRight];
          }
          if(pv[iFindRight] > maxBottomBind)
          {
            bottomExp = false;
            for(int j=0; j<botDeNum; j++)
            {
              pv[bottomDelete[j+peakNum]] = vbottomDelete[j];
            }
            //bottomDelete[peakNum] = yBind - yFindBindBoundary;
            bottomDelete[peakNum] = maY[maxLocation] - locBottomMin;
          }
          else
          {
            if(maY[iFindRight] < tmpLocBottomMin && pv[iFindRight] != -1)
              tmpLocBottomMin = maY[iFindRight];
            bottomDelete[botDeNum+peakNum] = iFindRight;
            vbottomDelete[botDeNum] = pv[iFindRight];
            pv[iFindRight] = -1;
            botDeNum++;
          }
        }
        
        else if(maX[iFindRight] - maX[maxLocation] >= xBind - xFindBindBoundary && maY[maxLocation] - maY[iFindRight] < yBind && maY[maxLocation] - maY[iFindRight] > -yBind && rightExp == true)
        {
          if(pv[iFindRight] == -1)
          {
            rightCover = true;
          }
          
          if(pv[iFindRight] > tempRightMax)
          {
            tempRightMax = pv[iFindRight];
          }
          if(pv[iFindRight] > maxRightBind)
          {
            rightExp = false;
            for(int j=0; j<rigDeNum; j++)
            {
              pv[rightDelete[j+peakNum]] = vrightDelete[j];
            }
            //rightDelete[peakNum] = xBind - xFindBindBoundary;
            rightDelete[peakNum] = locRightMax - maX[maxLocation];
          }
          else
          {
            if(maX[iFindRight] > tmpLocRightMax)
              tmpLocRightMax = maX[iFindRight];
            rightDelete[rigDeNum+peakNum] = iFindRight;
            vrightDelete[rigDeNum] = pv[iFindRight];
            pv[iFindRight] = -1;
            rigDeNum++;
          }
        }
        iFindRight = iFindRight + 1;
      }
      
      if(tempTopMax != 0)
      {
        maxTopBind = tempTopMax;
      }
      if(tempBottomMax != 0)
      {
        maxBottomBind = tempBottomMax;
      }
      if(tempLeftMax != 0)
      {
        maxLeftBind = tempLeftMax;
      }
      if(tempRightMax != 0)
      {
        maxRightBind = tempRightMax;
      }
      
      if(topExp == false && bottomExp == false && leftExp == false && rightExp == false)
      {
        bExpand = false;
      }
    }
    
    addMax++;
    peakNum++;
    while(pv[maxMatrix[1][addMax]] < 0)
    {
      addMax++;
      if(addMax >= maxCount)
        break;
    }
    maxValue = maxMatrix[0][addMax];
    maxLocation = (int)maxMatrix[1][addMax];
  }
  
  delete(maxMatrix[0]);
  delete(maxMatrix[1]);
  delete[](maxMatrix);
  
  NumericVector returnVector(peakNum*5);
  
  for(int i=0; i<peakNum; i++)
  {
    returnVector[i] = peakLocation[i];
    returnVector[peakNum+i] = topDelete[i];
    returnVector[peakNum*2+i] = bottomDelete[i];
    returnVector[peakNum*3+i] = leftDelete[i];
    returnVector[peakNum*4+i] = rightDelete[i];
  }
  
  delete[](topDelete);
  delete[](bottomDelete);
  delete[](leftDelete);
  delete[](rightDelete);
  
  delete[](vtopDelete);
  delete[](vbottomDelete);
  delete[](vleftDelete);
  delete[](vrightDelete);
  
  delete[](peakLocation);
  
  return returnVector;
}



// [[Rcpp::export]]
NumericVector calculateDensity(NumericVector ma,NumericVector la,NumericVector inten ,IntegerVector pointEndVector, IntegerVector pointStartVector,IntegerVector pointResult,double sTempCoef,double cutWidth,double cutHeight,double xBindBoundary,double yBindBoundary,double xFindBindBoundary,double yFindBindBoundary,double pv, int iPow)
{
  int pointLength = ma.length();
  
  double** lamaSort = new double*[4];
  lamaSort[0] = new double[pointLength];
  lamaSort[1] = new double[pointLength];
  lamaSort[2] = new double[pointLength];
  lamaSort[3] = new double[pointLength];
  
  for(int i=0; i<pointLength; i++)
  {
    lamaSort[0][i] = ma[i];
    lamaSort[1][i] = la[i];
    lamaSort[2][i] = inten[i];
    lamaSort[3][i] = i;
  }
  
  quickSort4(lamaSort, 0, pointLength-1);
  NumericVector maNew = clone(ma);
  NumericVector laNew = clone(la);
  NumericVector intenNew = clone(inten);
  
  double k = 100 / (pow(lamaSort[0][pointLength-1],1.0/iPow)-pow(lamaSort[0][0],1.0/iPow));
  
  for(int i=0; i<pointLength; i++)
  {
    maNew[i] = lamaSort[0][i];
    laNew[i] = lamaSort[1][i];
    intenNew[i] = lamaSort[2][i];
  }
  
  // double  cutWidth = 0.01;
  // double  cutHeight = 50;
  int goPoint = 1;
  NumericVector tempInten =  clone(intenNew);
  
  for (int i = 0; i < pointLength; i++)
  {
    double newCutWidth = pow(pow(maNew[i],1.0/iPow) + cutWidth, iPow);
    while (goPoint < (pointLength-1) && newCutWidth >= maNew[goPoint])
    {
      goPoint = goPoint + 1;
    }
    if (goPoint > pointLength)
    {
      break;
    }
    pointEndVector[i] = goPoint;
    pointStartVector[goPoint] = i+1;
  }
  
  for (int i = 1; i < pointLength; i++)
  {
    if (pointStartVector[i] == 1 && pointStartVector[i-1] != 1)
    {
      pointStartVector[i] = pointStartVector[i-1];
    }
  }
  
  NumericVector tempIntenLog =  clone(intenNew);
  NumericVector intenNewIni = clone(inten);
  
  for (int i = 0; i < pointLength; i++) {
    tempIntenLog[i] = log10(tempInten[i] + 1);
  }
  
  double pointValue = 0;
  double pointValueLog = 0;
  double tempValue = 0;
  //New Version
  for (int i = 0; i < pointLength; i++)
  {
    pointValue = 0;
    pointValueLog = 0;
    if(pointEndVector[i] - pointStartVector[i] != 0)
    {
      double newCutWidth = pow(pow(maNew[i],1.0/iPow) + cutWidth, iPow) - maNew[i];
      double tempCoef = -log(sqrt(newCutWidth)) + sTempCoef;
      
      for (int j = pointStartVector[i]-1; j < pointEndVector[i]; ++j)
      {
        if (laNew[j] >= (laNew[i] - cutHeight) &&  laNew[j] <= (laNew[i] + cutHeight) )
        {
          tempValue = exp(tempCoef -(0.5 * (pow(((maNew[j] - maNew[i]) / sqrt(newCutWidth)),2) + pow(((laNew[j] - laNew[i]) / sqrt(cutHeight)),2))));
          pointValue += (tempValue * tempInten[j]);
          pointValueLog += (tempValue * tempIntenLog[j]);
        }
      }
    }
    intenNew[i] = pointValueLog;
    intenNewIni[i] = pointValue;
  }
  
  double dSta = pow(maNew[0],1.0/iPow);
  for(int i=0; i<pointLength; i++)
  {
    maNew[i] = (pow(maNew[i],1.0/iPow)-dSta) * k;
  }
  
  NumericVector intenTemp = clone(intenNew);
  delete(lamaSort[0]);
  delete(lamaSort[1]);
  delete(lamaSort[2]);
  
  NumericVector findPeakReturn = FindPeak(intenNew, maNew, laNew, pointResult,xBindBoundary,yBindBoundary,xFindBindBoundary,yFindBindBoundary,pv);
  
  /*
   for(int i=0; i<pointLength; i++)
   {
   if(findPeakReturn[i] == 1 || findPeakReturn[i] == 3)
   {
   findPeakReturn[lamaSort[3][i]] += 2;
   findPeakReturn[i] -= 1;
   }
   }
   */
  NumericVector finalData(findPeakReturn.length()/5*7);
  
  int peakNum = findPeakReturn.length()/5;
  for(int i=0; i<peakNum; i++)
  {
    finalData[i] = maNew[findPeakReturn[i]] / k + dSta;
    finalData[i+peakNum] = laNew[findPeakReturn[i]];
    finalData[i+peakNum*2] = findPeakReturn[peakNum+i];
    finalData[i+peakNum*3] = findPeakReturn[peakNum*2+i];
    finalData[i+peakNum*4] = intenNewIni[findPeakReturn[i]];
    finalData[i+peakNum*5] = findPeakReturn[peakNum*3+i] / k + dSta;
    finalData[i+peakNum*6] = findPeakReturn[peakNum*4+i] / k + dSta;
  }
  
  return finalData;
}

