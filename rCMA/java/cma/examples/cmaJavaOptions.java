package cma.examples;

//import java.util.Properties;

/*
    Copyright 2003, 2005, 2007 Nikolaus Hansen 
    e-mail: hansen .AT. bionik.tu-berlin.de
            hansen .AT. lri.fr

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License, version 3,
    as published by the Free Software Foundation.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

 */

/** Simple container of (mostly generic) options for the
 * optimization, like the maximum number of objective
 * function evaluations, see class fields.  No explicit setting of 
 * options is needed to 
 * initialize the CMA-ES ({@link CMAEvolutionStrategy#init()}) 
 * and options of the CMA-ES can be set
 * and changed any time, either via a property file and the method
 * {@link CMAEvolutionStrategy#readProperties()}, or new values can simply be 
 * assigned to the fields of the public <code>opts</code> field of 
 * the class <code>CMAEvolutionStrategy</code> (yeah, I know, not exactly Java style).
 * 
 */
public class cmaJavaOptions implements java.io.Serializable {
        // needs to be public to make sure that a using class can excess Options.
        // Therefore, if not nested, needs to move into a separate file
        
	private static final long serialVersionUID = 2255162105325585121L;

        /** This is the only place where the reading of a new option needs to be declared 
         * 
         * @param cjProperties
         */
    public double stopTolFun = 1e-12; 
    public double[] upperBounds = null;
    public double[] lowerBounds = null;
    
    void setUpperBounds(double[] x) {
    	upperBounds = x.clone();
    }
    void setLowerBounds(double[] x) {
    	lowerBounds = x.clone();
    }
	/*
    void setOptions(Properties properties) {
        String s;
        	
            stopTolFun = getFirstToken(properties.getProperty("stopTolFun"), stopTolFun);
            if ((s = properties.getProperty("upperStandardDeviations")) != null && !s.equals(""))
                upperStandardDeviations = parseDouble(getAllToken(s));
            String s;
            diagonalCovarianceMatrix = getFirstToken(properties.getProperty("diagonalCovarianceMatrix"), diagonalCovarianceMatrix);
            if((s = properties.getProperty("stopFitness")) != null)
                stopFitness = Double.valueOf(getFirstToken(s));
            stopTolFunHist = getFirstToken(properties.getProperty("stopTolFunHist"), stopTolFunHist);
            stopTolX = getFirstToken(properties.getProperty("stopTolX"), stopTolX);
            stopTolXFactor = getFirstToken(properties.getProperty("stopTolXFactor"), stopTolXFactor);
            stopTolUpXFactor = getFirstToken(properties.getProperty("stopTolUpXFactor"), stopTolUpXFactor);
            stopMaxFunEvals = getFirstToken(properties.getProperty("stopMaxFunEvals"), stopMaxFunEvals);
            stopMaxIter = getFirstToken(properties.getProperty("stopMaxIter"), stopMaxIter);
            if ((s = properties.getProperty("lowerStandardDeviations")) != null && !s.equals(""))
                lowerStandardDeviations = parseDouble(getAllToken(s));
            outputFileNamesPrefix = properties.getProperty("outputFileNamesPrefix", outputFileNamesPrefix).split("\\s")[0];
            maxTimeFractionForEigendecomposition = 
                getFirstToken(properties.getProperty("maxTimeFractionForEigendecomposition"), 
                        maxTimeFractionForEigendecomposition);
            maxTimeFractionForWriteToDefaultFiles = 
                getFirstToken(properties.getProperty("maxTimeFractionForWriteToDefaultFiles"), 
                        maxTimeFractionForWriteToDefaultFiles);
            stopnow = "now".equals(getFirstToken(properties.getProperty("stop")));
            writeDisplayToFile = getFirstToken(properties.getProperty("writeDisplayToFile"), writeDisplayToFile);
            checkEigenSystem = getFirstToken(properties.getProperty("checkEigenSystem"), checkEigenSystem);
        }
            */

		/** Returns the double value of the first token of a string s or the default, 
		 *  if the string is null or empty. This method should become generic with respect to the
		 *  type of second argument.  
		 *  @param s string where the first token is read from
		 *  @param def double default value, in case the string is empty*/
		public Double getFirstToken(String s, Double def) {
		    if (s == null)
		        return def;
		    String[] ar = s.split("\\s+");
		    if (ar[0].equals("")) 
		        return def;
		    return Double.valueOf(ar[0]);
		}

		/** should become generic with type argument?  */
		public String getFirstToken(String s) {
		    if (s == null)
		        return ""; 
		    String[] ar = s.split(new String("\\s+"));
		    return ar[0];
		}

		/** Returns the Integer value of the first token of a string s or the default, 
		 *  if the string is null or empty. This method should become generic with respect to the
		 *  type of second argument.  
		 *  @param s string where the first token is read from
		 *  @param def Integer default value, in case the string is empty*/
		public Integer getFirstToken(String s, Integer def) {
		    if (s == null)
		        return def;
		    String[] ar = s.split("\\s+");
		    if (ar[0].equals("")) 
		        return def;
		    return Integer.valueOf(ar[0]);
		}

		//    public <T> T getFirstToken(String s, T def) {
		//        if (s == null)
		//            return def;
		//        String[] ar = s.split("\\s+");
		//        if (ar[0].equals("")) 
		//            return def;
		//        return (T)(ar[0]); /* this fails */
		//    }
		    
		    private String removeComments(String s) {
		        int i;
		        // remove trailing comments
		        i = s.indexOf("#");
		        if (i >= 0)
		            s = s.substring(0,i);
		        i = s.indexOf("!");
		        if (i >= 0)
		            s = s.substring(0,i);
		        i = s.indexOf("%");
		        if (i >= 0)
		            s = s.substring(0,i);
		        i = s.indexOf("//");
		        if (i >= 0)
		            s = s.substring(0,i);
		        return s;
		    }

		/** Returns def if s==null or empty, code dublicate, should become generic */
		public Long getFirstToken(String s, Long def) {
		    if (s == null)
		        return def;
		    String[] ar = removeComments(s).split("\\s+");
		    if (ar[0].equals("")) 
		        return def;
		    return Long.valueOf(ar[0]);
		}

		String[] getAllToken(String s) {
		    // split w.r.t. white spaces regexp \s+
		    return removeComments(s).split("\\s+");
		}

		double[] parseDouble(String[] ars) {
		    double[] ard = new double[ars.length];
		    for(int i = 0; i < ars.length; ++i) {
		        ard[i] = Double.parseDouble(ars[i]);
		    }
		    return ard;
		}
    }

