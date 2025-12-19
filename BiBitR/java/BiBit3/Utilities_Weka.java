

import java.io.FileReader;
import java.io.IOException;
import weka.core.*;
/********************** CLASS UTILITIES_WEKA **********************
This class only has a procedure which transform the input arff file into a weka's object.
**********************************/


public class Utilities_Weka {
	
	
	
	public Instances m_Data = null;
	public int numInstances=0; // Número de genes
	public int numAttributes=0; // Número de condiciones
		

	public void leeFichero(String fInput) 
	  { 
	    	   
		try {
			FileReader reader = new FileReader(fInput); 
	        m_Data = new Instances(reader);
	       
	       if (m_Data.classIndex() >0){
	        
	        	
	        m_Data.setClassIndex(m_Data.numAttributes()- 1);
	        }
	       	
	        numInstances=m_Data.numInstances();
	        numAttributes=m_Data.numAttributes();
	        
	        System.out.println("BITPAT");
	        System.out.println("============"+'\n');
	        System.out.println("ARFF file name: "+ fInput+'\n');
	        System.out.println("Number of rows: "+numAttributes+'\n');
	        System.out.println("Number of columns: "+ numInstances+'\n');
	       
	          
	        
	        reader.close();
	       
	        } 
	   catch(IOException e){System.out.println(e);} 
	  }
	
	
	
	
	public void PrintDataSet() throws IOException{
		
		System.out.println("\nDataset:\n"); 
		System.out.println(m_Data); 
		
	}
	
	
}
