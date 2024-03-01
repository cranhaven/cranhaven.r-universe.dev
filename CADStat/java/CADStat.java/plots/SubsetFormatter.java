package CADStat.java.plots;
import static java.lang.Integer.min;
import org.neptuneinc.cadstat.utils.RUtils;
import static java.lang.Math.ceil;
import java.lang.Object;

/**
 *
 * @author David Rebhuhn
 * 
 * The following function formats data subsets in the interpreted commands
 * sent to R in such a way that prevents interpreter overflow.
 */

public class SubsetFormatter {

    public static Object[][] groupSubsets(Object[] subsets){        

        // length of each row that we want to output.
        int rowLength = 10;

        // number of rows in the final array of object arrays
        int nRows = (int) ceil(((double)(subsets.length))/rowLength);

        // output array of arrays
        Object[][] output = new Object[nRows][];
        int maxRowLength = rowLength;

        //populate the output array
        
        int offset=0;
        
        for (int k=0; k<nRows; k++){
        //** determine number of choices left
            int nLeft = subsets.length-offset;
        //** determine the number of elements a new sub-array should have, 
        //   which should be the minimum of maxRowLength and nChoices Left
            maxRowLength = min(rowLength,nLeft);
        //** create a new output row
        Object[] newRow = new Object[maxRowLength];
        //** populate it with elements
        for (int i=0; i<maxRowLength; i++){
                newRow[i] = subsets[offset+i];
            }
            
            offset = offset+maxRowLength;
            
        //** add it to the appropriate row in output
            output[k]=newRow;
        }
        //* return output        
        return output;
    }    
    
    public static String formatSubset(Object[] selectedFactorValues){
        //split subsets
            Object[][] subsetGroups = groupSubsets(selectedFactorValues);
            String[] formatted = new String[subsetGroups.length];
            for (int i=0; i<subsetGroups.length; i++){
                formatted[i]=RUtils.toString(subsetGroups[i], ",", "'");
            }                        
            String buffer = "c(";

            for (int i=0; i<formatted.length; i++){
                buffer = buffer+formatted[i];
                
                if (!(i==formatted.length-1)){
                   buffer=buffer+",";
                }
                buffer=buffer+"\n";
            }
            buffer=buffer+")";
            return buffer;
            
//            return "c(" + RUtils.toString(formatted, ",\n", "'") + ")";
    }
   

}

