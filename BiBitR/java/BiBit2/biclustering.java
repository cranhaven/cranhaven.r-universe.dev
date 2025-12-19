
import java.util.List;

/********************** CLASS BICLUSTERING **********************
This is the main class. 
**********************************/
public class biclustering {
  
	// We will work with 16 bits in the encoding phase
	public static int patternSize=16;  
	
	public static void main(String[] args) throws Exception {
		
		// Input parameters of the main procedure
		
		// PARAMETER 1: Input arff file
        String arff=args[0];
        // The class Utilities_Weka is used to process the arff input file
        Utilities_Weka weka_Aux=new Utilities_Weka(); 
        weka_Aux.leeFichero(arff);
        
        // PARAMETER 2: is the maximum value in the discretized dataset. From this value, BiBit will binarize the dataset generating max_value different ones.
        String sMax=args[1];
        Integer iMax=new Integer(sMax);
        int max=iMax.intValue();
        
        //PARAMETER 3: minimum number of rows allowed in a valid bicluster
        String xminMot=args[2];
        Integer ixminMot=new Integer(xminMot);
        int minMot=ixminMot.intValue();
        
        //PARAMETER 4: minimum number of columns allowed in a valid bicluster
        String xminGene=args[3];
        Integer ixminGene=new Integer(xminGene);
        int minGene=ixminGene.intValue();
        
        //PARAMETER 5: outputFileName
        String outFile=args[4];
        
        //PARAMETER 6: is a one-column file with the names of all the rows, following the order they appear in the dataset. It is used to print information about the elements of final biclusters
        String rowFile=args[5];
        
        //PARAMETER 7: is a one-column file with the names of all the columns, following the order they appear in the dataset. It is used to print information about the elements of final biclusters
        String colFile=args[6];
        
        //PARAMETER 8: write results? If 1, the information about the final biclusters will be written on hard-disk. If 0, the results will not be written on hard-disk.
        String Swrite=args[7];
        Integer Iwrite= new Integer(Swrite);
        int write=Iwrite.intValue();
		
		// PARAMETER 9: Noise pattern mismatch when adding rows to the bicluster.
		String Snoise=args[8];
		Double Dnoise=new Double(Snoise);
		double noise=Dnoise.doubleValue();
       
        // The information about the names of all rows and columns is stored 
        List<String> Genes=Utilities.readDataColumn(colFile);
        List<String> Rows=Utilities.readDataColumn(rowFile);
             
        //We will work with a maximum size of 16 bits
        Matrix matrix_Aux = new Matrix(weka_Aux,patternSize);        
        int pow=(int) Math.pow(2,patternSize);
		
		
		//Creating the array with the number of ones of every decimal value between 0 and 2 raise to patternSize
		int [] distance=new int[pow];
        Utilities.numberOfOnes(pow, distance);
                        
         // The following loop will analyze every binarization level
        for(int i=max;i>=1;i--)
        {
        	// The matrix is binarized following the level i
        	matrix_Aux.discretizeMatrix(i);
        	
        	long tInicial = System.currentTimeMillis(); 
        	// This is the call to the function that generates the biclusters
        	int num=Utilities.generateBiclusters(matrix_Aux,minMot,minGene,distance,outFile,i,Genes,Rows,patternSize,write,noise); //PVALUE CHANGE
        	long tFinal = System.currentTimeMillis();
   		 	long seconds = tFinal - tInicial; 
        	System.out.println("Number of biclusters at level "+i+": "+num+" in "+seconds+" milliseconds");
        	Utilities.ImprimeSalidaln(outFile, "Number of biclusters at level "+i+": "+num+" in "+seconds+" milliseconds");
        }
        
        
        
      	}
	

}
