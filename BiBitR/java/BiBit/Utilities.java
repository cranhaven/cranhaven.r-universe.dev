import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
/********************** CLASS UTILITIES **********************
This class contains all the functions and procedures that are used during the biclusters
processing.
**********************************/
public class Utilities {

	// This functions reads a one-column format file and stores the information in a List
	public static List<String> readDataColumn(String file) throws IOException
	{
		List<String> output=new ArrayList<String>();
		File f= new File (file);
		BufferedReader br= new BufferedReader (new FileReader(f));
		String cadena=null;
		while ((cadena = br.readLine()) != null){
		 output.add(cadena);
		  	
		 }
		return output;
		
		
	}
	
	// This procedures stores in an array the number of ones of all the binary numbers that
	// can be generated with the number of bits used in the encoding phase
	public static void numberOfOnes(int value, int[]v)
	{
		for(int i=0;i<value;i++)
		{
			
			String s=Integer.toString(i,2);
			int index=0;
			int cont=-1;
			while (index!=-1)
			{
				cont=cont+1;
				index=s.indexOf('1');
				s=s.substring(index+1, s.length());
								
			}
			v[i]=cont;
			
		}
		
		
	}
	// This is the procedure that extract the biclusters from each binarization level. The patterns are extracted and, for every pattern, the rows compatible with it are added to a bicluster
	public static int generateBiclusters(Matrix matrix_Aux, int minMot, int minGene, int[] v, String file, int value,List<String> Genes,List<String> Rows,int patternSize,int write) throws Exception
	{
		
		int rows=matrix_Aux.nRows;
		int cols=matrix_Aux.nCols;
		int extraCols=matrix_Aux.extraCols;
		
		String outfile=file+"_"+value+".txt";
		
		if(write==1)
		{
			ImprimeSalidaln(outfile,"NumOfRows;NumOfColumns;Rows;Columns");
		}
		SortedSet<String> patterns=new TreeSet<String>();
		int cont=0;
		for(int i=0;i<rows;i++)
		{
			
			
			if (matrix_Aux.DAux[i][0]>=minGene && matrix_Aux.DAux[i][2]==-1) 
			{
				Integer motifI=new Integer(i);
				
				for(int j=i+1;j<rows;j++)
				{
					if (matrix_Aux.DAux[j][0]>=minGene) 
					{
						Integer motifJ=new Integer(j);
						int andR[]=new int[extraCols];
						
						String prodPattern="";
						int prod=0;
						int num1=0;
						for(int k=0;k<extraCols;k++)
						{
					
							prod=matrix_Aux.mArray_dis[i][cols-extraCols+k]&matrix_Aux.mArray_dis[j][cols-extraCols+k];
							num1=num1+v[prod];
							andR[k]=prod;
							
							prodPattern=prodPattern+String.valueOf(prod);
					
						}
						
						SortedSet motifs=new TreeSet();
						
						if(num1>=minGene && !(patterns.contains(prodPattern)))
						{
					
							patterns.add(prodPattern);
							motifs.add(motifI);
							motifs.add(motifJ);
							for(int k=0;k<rows;k++)
							{
								if (k!=i && k!=j)
								{
									boolean coincidence=true;
							
									int p=0;
									while(coincidence && p<extraCols) 
									{
										int r=andR[p] & matrix_Aux.mArray_dis[k][cols-extraCols+p];
										if (r!=andR[p])
											coincidence=false;
										else
											p=p+1;
								
									}
							
					
									if (coincidence)
									{
										Integer elem=new Integer(k);
										motifs.add(elem);
									}
							
								}
							}
							if (motifs.size()>=minMot)
							{
								cont=cont+1;
								if(write==1)
								{
								ImprimeSalida(outfile,motifs.size()+";"+num1+";");
								ImprimeMotivos(outfile,motifs,Rows);
								ImprimeGenes(outfile,num1,Genes,andR,extraCols,matrix_Aux.nInstances,patternSize);
								
								}
							}
					
						}								
				}//if
			}		
		} //if 
				
	}

		return cont;
		
	}
	// This procedure stores in an array the number of 1's of every bit word of x bits, being x the number of bits used for the encoding phase
	static int giveNumberOfOnes(int prod, List s)
	{
		
		int result=0;
		int aux=prod;
		Binary bAux=new Binary(prod);
	
		int index=s.indexOf(bAux);
		
		if (index!=-1)
		{
			bAux=(Binary)s.get(index);
			result=bAux.getNumOfOnes();
		}
		else
		{
			
			while(aux>0)
			{
					
				int sqrt=(int)Math.sqrt(aux);
				int pow=(int)Math.pow(sqrt, 2);
				aux=aux-pow;
				result=result+1;
			
			}
			bAux.setNumOfOnes(result);
			s.add(bAux);
			
		}
		
	
		
		return result;
		
	}
	
	// The following procedures are used to print the results in files stored in hard-disk
	static TreeSet ImprimeGenes(String nomFich,int num,List Genes, int [] andR, int extraCols,int numInstances,int patternSize) throws IOException
	{
		
		TreeSet result=new TreeSet();
		for(int i=extraCols-1,j=0;i>=0;i--,j++)
		{
			
			String s=Integer.toString(andR[i],2);
			int numChar=s.length();
			int op=Genes.size()-numChar-(patternSize*j);
			int index=0;
			int cont=0;
			while(index<s.length())
			{
				char aux=s.charAt(index);
				if (aux=='1')
				{
					cont=cont +1;
					int pos=index+op;
					String gene=(String)Genes.get(pos);
					result.add(gene);
					if(cont==num)
						ImprimeSalida(nomFich,gene);
					else
						ImprimeSalida(nomFich,gene+", ");
					
				}
				index=index+1;
			}
			
		}
		
		ImprimeSalidaln(nomFich,"");
		return result;
	}
	static void ImprimeMotivos(String nomFich, SortedSet Motifs, List rows) throws IOException
	{
		int num=Motifs.size();
		int cont=0;
		Iterator i=Motifs.iterator();
		while(i.hasNext())
		{
			cont=cont+1;
			Integer aux=(Integer)i.next();
			String row=(String)rows.get(aux.intValue());
			if(cont==num)
				ImprimeSalida(nomFich,row);
			else
				ImprimeSalida(nomFich,row+",");
		}
		ImprimeSalida(nomFich,";");
				
	}
	
	
	static void ImprimeSalida (String nomFich, String mensaje)throws IOException{
		BufferedWriter bw = new BufferedWriter(new FileWriter(nomFich, true));
		PrintWriter salida = new PrintWriter(bw);
		salida.print(mensaje);
		salida.close();
	}
	static void ImprimeSalidach (String nomFich, char mensaje)throws IOException{
		BufferedWriter bw = new BufferedWriter(new FileWriter(nomFich, true));
		PrintWriter salida = new PrintWriter(bw);
		salida.print(mensaje);
		salida.close();
	}
	static void ImprimeSalidaln (String nomFich, String mensaje)throws IOException{
		BufferedWriter bw = new BufferedWriter(new FileWriter(nomFich, true));
		PrintWriter salida = new PrintWriter(bw);
		salida.println(mensaje);
		salida.close();
	}
}
