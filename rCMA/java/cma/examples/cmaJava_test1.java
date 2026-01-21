package cma.examples;
import java.io.*;
import java.util.Properties;

import cma.*;
import cma.fitness.IObjectiveFunction;

/**  
 * The R-call, a function to be minimized: 
 * C:\Programme\R\R-2.10.0\bin\Rterm.exe --no-restore --no-save --slave < cma_j.r > cma_j.log
 */
class TdmFunc_test1 implements IObjectiveFunction { // meaning implements methods valueOf and isFeasible
	private cmaJavaOptions cjOptions = new cmaJavaOptions();
	TdmFunc_test1(cmaJavaOptions cjOptions) {
		this.cjOptions=cjOptions;
	}
	public double valueOf (double[] x) {
		double res = 0;
		Properties sysprops   = System.getProperties();
		String os = sysprops.getProperty("os.name");	
		System.out.println(os);
		boolean IS_OS_WINDOWS = os.startsWith("Windows");
		boolean IS_OS_LINUX = os.startsWith("Linux");
        String param1 = "cma_j_test.r";
        String param2 = "cma_j_test.log";
        String callLINUX[] = {"xterm"
        		,"-exec"
        		,"R --no-restore --no-save --slave < " + param1 + " > " + param2 + " 2> cma_j_test.err"};
        String callWINDOWS = "R --no-restore --no-save --slave < " + param1 + " > " + param2 + " 2> cma_j.err";
        // it is assumed that the path to R (under Windows e.g. C:/Programme/R/R-2.10.0/bin/)  
        // is in the program search path (!)

        Process process = null;
        PrintWriter f;
        String s, last_s="", prelast_s="";
        try {
        	f = new PrintWriter(new BufferedWriter(new FileWriter("cma_j.des")));
        	for (int i=0; i<x.length; i++)
        		f.println(x[i]);
            f.close();
        } 
		catch ( FileNotFoundException e ) {
			System.err.println( "Can’t write on cma_j.des");
		}
		catch ( IOException e ) {
			System.err.println( "I/O failed." );
		}
        
        try {
        	if (IS_OS_WINDOWS)  
        		process = Runtime.getRuntime().exec(callWINDOWS);
        	else if(IS_OS_LINUX) 
        		process = Runtime.getRuntime().exec(callLINUX);
        	else 
        		process = Runtime.getRuntime().exec(callLINUX);
        	process.waitFor();
            //OutputStream stdout = process.getOutputStream(); 
            BufferedReader br =  new BufferedReader(new FileReader("cma_j_test.log"));
            while((s=br.readLine()) != null) {
            	prelast_s = last_s;
            	last_s = s;
            	System.out.println(s);
            }
            //InputStream stderr = process.getErrorStream();
        } catch(IOException e) {
        	System.out.println(e.getMessage());
        } catch(InterruptedException e) {
        	System.out.println(e.getMessage());
        } 
        
        /*
        if (!prelast_s.equals("cma_j.r successfully finished with result"))
        	throw(new RuntimeException("cma_j.r returned with error status >> check cma_j.err!"));
        */
		return 42;
	}
	public boolean isFeasible(double[] x) {
		if (cjOptions.lowerBounds != null) {
	    	if (cjOptions.lowerBounds.length != x.length)
	    		throw(new RuntimeException("dimensions x.length" + x.length + " and lowerBounds.length=" 
	    				+ cjOptions.lowerBounds.length + "do not agree"));
			for (int i=0; i<x.length; i++) 
				if (x[i]<cjOptions.lowerBounds[i]) return false;
		}
		if (cjOptions.upperBounds != null) {
	    	if (cjOptions.upperBounds.length != x.length)
	    		throw(new RuntimeException("dimensions x.length" + x.length + " and upperBounds.length=" 
	    				+ cjOptions.upperBounds.length + "do not agree"));
			for (int i=0; i<x.length; i++)
				if (x[i]>cjOptions.upperBounds[i]) return false;
		}
		return true; 
	} 
}

/** Use the CMA java tuner from TDMR  * 
 * @see CMAEvolutionStrategy
 * 

 * @author Wolfgang Konen 
 */
public class cmaJava_test1 {
	public cmaJavaOptions cjOptions = new cmaJavaOptions();
    String propertiesFileName = new String("cma_j.properties");
    Properties cjProperties = new Properties();
	public CMAEvolutionStrategy cma = new CMAEvolutionStrategy();
	public static void main(String[] args) {
		cmaJava_test1 c_j = new cmaJava_test1();
		c_j.run(args);
	}
	public void run(String[] args) {
		String propsFile = "cma_j.properties";
		if (args.length>0) propsFile = args[0];
		// set initial values
		cma.readProperties(); // read options, see file CMAEvolutionStrategy.properties
		this.readProperties(propsFile); 	// read further options from propsFile, e.g. CMAprops.txt,
								// which is written by R code (function cma_jTuner in tdmDispatchTuner.r)
		
		//cma.setDimension(11); // overwrite some loaded properties
		//cma.setInitialX(0.5); // in each dimension, also setTypicalX can be used
		//cma.setInitialStandardDeviation(0.2); // also a mandatory setting 
		cma.options.stopFitness = -Double.MAX_VALUE;       // i.e. stop never due to value of fitness function

		IObjectiveFunction fitfun = new TdmFunc_test1(cjOptions);

		// initialize cma and get fitness array to fill in later
		double[] fitness = cma.init();  // new double[cma.parameters.getPopulationSize()];

		// initial output to files
		cma.writeToDefaultFilesHeaders(0); // 0 == overwrites old files

		// iteration loop
		while(cma.stopConditions.getNumber() == 0) {

            // --- core iteration step ---
			double[][] pop = cma.samplePopulation(); // get a new population of solutions
			for(int i = 0; i < pop.length; ++i) {    // for each candidate solution i
            	// a simple way to handle constraints that define a convex feasible domain  
            	// (like box constraints, i.e. variable boundaries) via "blind re-sampling" 
            	                                       // assumes that the feasible domain is convex, the optimum is  
				while (!fitfun.isFeasible(pop[i]))     //   not located on (or very close to) the domain boundary,  
					pop[i] = cma.resampleSingle(i);    //   initialX is feasible and initialStandardDeviations are  
                                                       //   sufficiently small to prevent quasi-infinite looping here
                // compute fitness/objective value	
				fitness[i] = fitfun.valueOf(pop[i]); // fitfun.valueOf() is to be minimized
			}
			cma.updateDistribution(fitness);         // pass fitness array to update search distribution
            // --- end core iteration step ---

			// output to files and console 
			cma.writeToDefaultFiles();
			int outmod = 150;
			if (cma.getCountIter() % (15*outmod) == 1)
				cma.printlnAnnotation(); // might write file as well
			if (cma.getCountIter() % outmod == 1)
				cma.println(); 
		}
		// evaluate mean value as it is the best estimator for the optimum
		cma.setFitnessOfMeanX(fitfun.valueOf(cma.getMeanX())); // updates the best ever solution 

		// final output
		cma.writeToDefaultFiles(1);
		cma.println();
		cma.println("Terminated due to");
		for (String s : cma.stopConditions.getMessages())
			cma.println("  " + s);
		cma.println("best function value " + cma.getBestFunctionValue() 
				+ " at evaluation " + cma.getBestEvaluationNumber());
			
		// we might return cma.getBestSolution() or cma.getBestX()

	} // main  

    /** reads properties from default
     * input file cma_j.properties and
     * sets options and strategy parameter settings
     * accordingly. Options values can be changed at any time using this function. 
     */
    public Properties readProperties() {
    	return readProperties(propertiesFileName);
    }
    /** reads properties from fileName and sets strategy parameters and options
     * accordingly
     * @param fileName of properties file
     */
    public Properties readProperties(String fileName) {
        this.propertiesFileName = fileName;
        try {
            java.io.FileInputStream fis = new java.io.FileInputStream(fileName);
            cjProperties.load(fis);
			cjProperties.list( System.out );
            fis.close();
        } 
		catch ( FileNotFoundException e ) {
			System.err.println( "Can’t find " + fileName );
		}
		catch ( IOException e ) {
			System.err.println( "I/O failed." );
		}
        setFromProperties(cjProperties);
        return cjProperties;
    }

    public void setFromProperties(Properties properties) {
        String s;
        
        //cjOptions.setOptions(properties);
        if ((s = properties.getProperty("dimension")) != null) {
        	cma.setDimension(cjOptions.getFirstToken(s,1));
        }
        if ((s = properties.getProperty("initialX")) != null) {
            cma.setInitialX(cjOptions.parseDouble(cjOptions.getAllToken(s))); 
        }
        if ((s = properties.getProperty("initialStandardDeviations")) != null) {
            cma.setInitialStandardDeviations(cjOptions.parseDouble(cjOptions.getAllToken(s)));
        }
        if ((s = properties.getProperty("lowerBounds")) != null) {
            cjOptions.setLowerBounds(cjOptions.parseDouble(cjOptions.getAllToken(s)));
        }
        if ((s = properties.getProperty("upperBounds")) != null) {
            cjOptions.setUpperBounds(cjOptions.parseDouble(cjOptions.getAllToken(s)));
        }
        cma.options.stopMaxFunEvals = 5; //cjOptions.getFirstToken(properties.getProperty("stopMaxFunEvals"), cma.options.stopMaxFunEvals);
        //cma.options.stopTolFun = cjOptions.getFirstToken(properties.getProperty("stopMaxFunEvals"), cma.options.stopTolFun);
    }
} // class
