import weka.core.DenseInstance;
import weka.core.Attribute;
import weka.core.Instances;
import weka.core.FastVector;
public class JavaObjectFromDataConverter {

    public static void main(String[] args) {
        //Create an array for Testing Purposes
        double[][] arr = {{1,2,3,4},{5,6,7,8},{9,10,11,12}};
        System.out.println(Instances_from_matrix(arr));
    }


    //The goal of this function is to create an Instances object because the Subspace clustering
    //algorithms can only be applied to that class. We can also not easily create this kind of object
    //in R and the end user should be able to just pass a matrix.
    public static Instances Instances_from_matrix(double[][] arr) {
        //First we wrap the rows of our matrix in Objects of the DenseInstance class (Dense means
        //no missing values in this context)
        DenseInstance[] in = new DenseInstance[arr.length];
        for(int i=0; i < arr.length;i++) {
            //Note:The 1 passed here is a weigthing Factor for the Instance.
            //Since we do not present the option of weighting to the user, we always weight
            //everything equally.

           in[i] = new DenseInstance(1,arr[i]); 
        } 

        //Next we would like to Create an Object of the class Instances, to which we can add
        //all the individual elements of the array 'in'. To do so, we need a FastVector of Attributes
        //with (apparently) names for the dimensions of the Data set. Since we do not intend to
        //use these names, we just use some meaningless names. 
        //
        //The parameter to FastVector represents the capacity that the FastVector should have.
        //
        //We use arr[0].length to get the number of dimensions of the data set.
        //
        //Note that the Version of weka we use is a very old one. With the current version as
        //of mid 2015 we would have to pass an ArrayList<Attribute> instead.
        FastVector lis = new FastVector(arr[0].length);
        for(int i=0;i < arr[0].length;i++) {
            lis.addElement(new Attribute("dummy_name"+i));//We add i because the attribute names
                                                          //Have to be unique
        }

        //Now we can create the Instances object. We have to assign it a name, so we just
        //use a dummy name and we have to assign it a capacity, which should be equal to
        //the number of rows in the matrix(= the number of Objects to perform clustering on).
        Instances insts= new Instances("dummy_name",lis,arr.length);
        //Now we add all the individual Instance Objects:
        for(int i=0;i < in.length;i++) {
            insts.add(in[i]);
        }

        //And now we can return the prepared Instances Object to be used for clustering
        return insts;
    }

    //This function turns a one-dimensional double array into a two-dimensional double
    //array with ncol columns.
    public static double[][] matrix_from_array(double[] arr,int ncol) {
        if(arr.length % ncol!=0) {
            System.out.println("Invalid data was passed to the function matrix_from_array");
        }
        int nrow=arr.length/ncol;
        double[][] res = new double[nrow][];
        for(int i=0;i< nrow;i++) {
            double[] cur = new double[ncol];
            for(int j=0;j<ncol;j++) {
               cur[j]=arr[i+(j*nrow)]; 
            }
            res[i]=cur;
        }
        return res;
    }

    /*Not in use as of now. Might be used later for very slight performance boost
    //Turns a double[] column-wise representation of an R Matrix into an object of Class 'Instances'
    //that can then be passed into the Clustering Functions.
    //
    //ncol must be the number of dimensions(=columns) that the resulting Instances should have and
    //thus arr.length must be divisible by it.
    public static Instances Instances_from_array(double[] arr,int ncol) {
        if(arr.length % ncol!=0) {
            System.out.println("Invalid data was passed to the function Instances_from_array");
        }
        int nrow=arr.length/ncol;
        //First we would like to Create an Object of the class 'Instances'. 
        //To do so, we need a 'FastVector' of 'Attribute's
        //with (apparently) names for the dimensions of the Data set. Since we do not intend to
        //use these names, we just use some meaningless names. 
        //
        //The parameter to 'FastVector' represents the capacity that the 'FastVector' should have.
        //
        //Note that the Version of weka we use is a very old one. With the current version as
        //of mid 2015 we would have to pass an ArrayList<Attribute> instead.
        FastVector lis = new FastVector(ncol);
        for(int i=0;i < ncol;i++) {
            lis.addElement(new Attribute("dummy_name"+i));//We add i because the attribute names
                                                          //Have to be unique
        }
        //Now we can create the 'Instances object'. We have to assign it a name, so we just
        //use a dummy name and we have to assign it a capacity, which should be equal to
        //the number of rows in the matrix(= the number of 'Instance' to perform clustering on).
        Instances insts= new Instances("dummy_name", lis,nrow);
        //Now we create the 'Instance's and add them to the 'Instances'. 
        for(int i=0; i < nrow;i++) {
            //Helper array in which we collect the elements of the i-th instance.
            double[] elems = new double[ncol];
            for(int j=0;j<ncol;j++) {
               //This is the way it is because arr is a matrix that was passed into this method
               //'column by column'. For this reason, the elements of one row are always ncol
               //indices apart.
               elems[j]=arr[i+(j*nrow)]; 
            }
            //Note:The 1 passed here is a weighting Factor for the 'Instance'.
            //Since we do not present the option of weighting to the user, we always weight
            //everything equally.
            insts.add(new DenseInstance(1,elems));
        }
        return insts;
    }
*/
    //Only for debugging purposes: outputs a 2d array row by row
    public static void output_2d_array(double[][] arr) {
        for(int i=0;i<arr.length;i++) {
            for(int j=0;j<arr[i].length;j++) {
                System.out.print(""+arr[i][j]+",");
            } 
            System.out.println("");
        }
    }
}
