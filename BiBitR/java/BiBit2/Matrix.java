


import java.io.IOException;

import weka.core.Instances;

/********************** CLASS MATRIX **********************
This class is used to extract the information from the arff files and to store it in a memory matrix. Also, this class implements the binarization and encoding process.
Weka's java libraries are used.
**********************************/

public class Matrix {

	public int [][] mArray=null;
	public int [][] mArray_dis=null;
	public int [][] DAux=null; 
	
	public double[] value;
	
	public int nAttributes=0;
	public int nInstances=0;
	public int nRows=0;
	public int nCols=0;
	public int extraCols=0; 
	public int patternSize=0;// Number of extra columns dedicated to the decimal representation of binary numbers
	public Matrix (Utilities_Weka weka, int size){
		
		patternSize=size;
		Instances i_Aux = null;
		

		i_Aux=weka.m_Data;
		
		
		if (i_Aux.classIndex()>0){
			
			nAttributes=i_Aux.numAttributes()-1;
		}
		else{
			nAttributes=i_Aux.numAttributes();
		}
		nInstances=i_Aux.numInstances();
		
		
		
		 	
		 	nRows=nAttributes;
		 	nCols=nInstances;
		 	int d=(nCols/size)+1; // Number of extra columns needed;
		 	System.out.println("Extra Columns Number: "+d);
		 	extraCols=d;
		 	nCols=nCols+d;
		 	mArray=new int[nAttributes][nCols];
		 	mArray_dis=new int[nAttributes][nCols];
		 	DAux=new int[nRows][3]; // 15/10/2010 CHEVI
		 	
		    for (int j = 0; j < nInstances; j++) {
	    	   value = i_Aux.instance(j).toDoubleArray();
	    	   
	    	   for (int i = 0; i < nAttributes; i++) {
	    	     if (i_Aux.attribute(i).isNumeric()){
	    	       if (!Double.isNaN(value[i])) {
	    	    	   mArray[i][j]= (int)value [i];
	    	    	 }  
	    	         else{
	    	    	   mArray[i][j]= 0;
	    	    	 }
	    	      
	    	     }
	    	  
	    	    }
		    }
		    // The matrix will has d more columns in which the decimal representation of the row wil be stored
		    for(int k=0;k<nRows;k++)
		    {
		    	for(int l=0;l<d;l++)
		    	{
		    		mArray[k][nCols-(1+l)]=-1;
		    	}
		    }
		
		 
		 
		 	
		System.out.println('\n' + "The matrix is created");
		
		value=null;
		
		
		}
	// This procedure binarizes a matrix and encodes every bit word.
	public void discretizeMatrix(int value) throws IOException
	{
       
		for(int k=0;k<nRows;k++)
	    {
			DAux[k][1]=0;
			DAux[k][2]=-1;
			int pow=0;
			int numExtraCol=extraCols;
			int numPatternSize=0;
			int contOnes=0; 
			
          	for(int l=(nInstances-1);l>=0;l--)
	    	{
          		
          		
          		if (numPatternSize >(patternSize-1))
          		{
          		
          			mArray_dis[k][(nInstances -1)+numExtraCol]=pow;
          			numPatternSize=0;
          			numExtraCol=numExtraCol-1;
          			pow=0;
          			
          		}
          		
          		if (mArray[k][l]< value)
          		
	    		{
          			mArray_dis[k][l]=0;
          			pow=pow+0;
          			
	    		}
	    		else
	    		{
	    			mArray_dis[k][l]=1;
	    			pow=pow+(int)Math.pow(2, (numPatternSize));
	    			contOnes=contOnes+1; 
	    			
	    		}
	    		
	    		
          		
          		numPatternSize=numPatternSize+1;	
          		
          		
          		
	    	}
          	mArray_dis[k][(nInstances -1)+numExtraCol]=pow;
          	
          	if (DAux[k][0]==contOnes) 
          	{	
          		DAux[k][1]=1;
          		
          	}
          	DAux[k][0]=contOnes;
          	
          	
          	
	    }
		
		

		for(int k=0;k<nRows;k++)
		{
			if (DAux[k][0]>0)
			{
			
			for(int l=k+1;l<nRows;l++)
			{
				if(DAux[l][2]==-1 && DAux[l][0]>0)
				{	
					
					boolean coincidence=true;
				
					int p=0;
					while(coincidence && p<extraCols) 
					{
						
						if (mArray_dis[l][nCols-extraCols+p]!=mArray_dis[k][nCols-extraCols+p])
							coincidence=false;
						else
							p=p+1;
					
					}
				
		
					if (coincidence)
					{
						DAux[l][2]=k;
						
					}
				}	
			}
		}
		}
		
		
	}
   	
}
