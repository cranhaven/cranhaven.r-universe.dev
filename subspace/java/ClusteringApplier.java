import weka.core.Instances;
import weka.subspaceClusterer.Clique;
import weka.subspaceClusterer.Fires;
import weka.subspaceClusterer.P3c;
import weka.subspaceClusterer.Proclus;
import weka.subspaceClusterer.Subclu;
import weka.subspaceClusterer.SubspaceClusterer;
import i9.subspace.base.Cluster;
import java.util.List;
import java.io.*;

public class ClusteringApplier {
    public static void main(String[] args) {
        //Array for Testing purposes
        double[][] arr = {{1,2,3,4},{5,6,7,8},{9,10,11,12}};
        //int[][] clustering = clique(arr,10,1);
        //System.out.println(""+clustering);
    }

    //After one of the subspace methods is called, a Cluster[] is returned. However,
    //these arrays will not be easy to handle in R, so a user of the package should
    //be presented with S3-Objects more suitable for further processing in R.
    //However, extracting relevant information from a 'Cluster' with the 
    //rJava interface is inefficient, so we do that in Java and pass efficiently processable
    //matrices with the data back into R.
    public static boolean[][] extract_subspace(Cluster[] clus) {
       boolean[][] res = new boolean[clus.length][];
       for(int i = 0; i < clus.length; i++) {
            res[i]= clus[i].m_subspace;
       }
       return res;
    }
    
    //This function has roughly the same goal as extract_subspace.
    //
    //Here we also have to take into account that thr rJava interface can only handle
    //rectangular 2d-arrays, so we fill each of the partial arrays with "filler" indices
    //of value -2 that will be removed after this matrix has been passed into R.
    public static int[][] extract_objects(Cluster[] clus) {
        int[][] res = new int[clus.length][];
        int max=-1;
        for(int i = 0; i < clus.length;i++) {
            if(max < clus[i].m_objects.size()) {
                max=clus[i].m_objects.size();
            }
        }
        for(int i = 0; i < clus.length;i++) {
            res[i] = new int[max];
            for(int j= 0;j<max;j++) {
                res[i][j]= -2;
            }
        }
        for(int i = 0; i < clus.length;i++) {
            List<Integer> temp1=clus[i].m_objects;
            Integer[] temp2 =temp1.toArray(new Integer[temp1.size()]);
            for(int j=0; j < temp2.length;j++) {
               res[i][j]=temp2[j]; 
            }
        }
        return res;
    }


    public static Cluster[] clique(double[][] data, int xi,double tau) {
        Instances insts = JavaObjectFromDataConverter.Instances_from_matrix(data);
        Clique clus=new Clique();
        try{
            clus.setOptions(new String[]{"-XI",String.valueOf(xi),"-TAU",String.valueOf(tau)});
            clus.buildSubspaceClusterer(insts);
        } catch(Exception e) {
            System.out.println("Exception was caught while Applying Clique");
        }
        List<Cluster> result=clus.getSubspaceClustering();
        return result.toArray(new Cluster[result.size()]);
    }


    //We could process all of the Cluster[]s returned by the clustering functions in R, but that
    //would take very long. Instead we encode the contents of each 'Cluster' in an int array, so
    //that we can then Just pass one big array back into R.
    //
    //The resulting array will contain twice as many int[]s as the amount of clusters.
    //the first half of the int[]s will consist of the clustering assignments and the second half
    //will consist of the dimension vectors.
    //
    //The dimension vectors are actually boolean vectors but we treat them as integer vectors
    //where 1 is true and 0 is false because we need to pass one big integer array.
    public static int[][] cluster_array_to_int_array(i9.subspace.base.Cluster[] clus) {
       int[][] res = new int[2*clus.length][];
        for(int i=0;i < clus.length;i++) {
            List<Integer> temp1=clus[i].m_objects;
            Integer[] temp2 =temp1.toArray(new Integer[temp1.size()]);
            res[i] = integer_array_to_int_array(temp2);

            res[(res.length/2-1)+i]=bool_array_to_int_array(clus[i].m_subspace);
        } 
        return res;
    }
    private static int[] integer_array_to_int_array(Integer[] arr) {
        int[] res = new int[arr.length];
        for(int i =0;i<arr.length;i++) {
            res[i]=arr[i];//Works because autounboxing
        }
        return res;
    }
    private static int[] bool_array_to_int_array(boolean[] arr) {
        int[] res= new int[arr.length];
        for(int i=0;i< arr.length;i++) {
            if(arr[i]) {
               res[i]=1;
            }
            else{
                res[i]=0;
            }
        }
        return res;
    }
        
    public static i9.subspace.base.Cluster[] fires(double[][] data, 
                                                   double base_dbscan_epsilon,
                                                   int base_dbscan_minpts,
                                                   double pre_minimumpercent,
                                                   int graph_k,
                                                   int graph_mu,
                                                   int graph_minclu,
                                                   double graph_split,
                                                   double post_dbscan_epsilon,
                                                   int post_dbscan_minpts) {
        Instances insts = JavaObjectFromDataConverter.Instances_from_matrix(data);
        Fires clus=new Fires();
        try{
            clus.setOptions(new String[]{"-BASE_DBSCAN_EPSILON",String.valueOf(base_dbscan_epsilon),
                                         "-BASE_DBSCAN_MINPTS",String.valueOf(base_dbscan_minpts),
                                         "-PRE_MINIMUMPERCENT",String.valueOf(pre_minimumpercent),
                                         "-GRAPH_K",String.valueOf(graph_k),
                                         "-GRAPH_MU",String.valueOf(graph_mu),
                                         "-GRAPH_MINCLU",String.valueOf(graph_minclu),
                                         "-GRAPH_SPLIT",String.valueOf(graph_split),
                                         "-POST_DBSCAN_EPSILON",String.valueOf(post_dbscan_epsilon),
                                         "-POST_DBSCAN_MINPTS",String.valueOf(post_dbscan_minpts)
            });
            clus.buildSubspaceClusterer(insts);
        } catch(Exception e) {
            System.out.println("Exception was caught while Applying Fires");
        }
        List<Cluster> result=clus.getSubspaceClustering();
        return result.toArray(new Cluster[result.size()]);
    }
    public static i9.subspace.base.Cluster[] p3c(double[][] data, double chi_square_alpha,
                                                                  int poisson_threshold) {
        Instances insts = JavaObjectFromDataConverter.Instances_from_matrix(data);
        //P3c still has outputs for debugging, that we do not want, so we silence
        //System.out.
        PrintStream out =System.out;
        System.setOut(new PrintStream(new OutputStream() {
                @Override public void write(int b) throws IOException {}
        }));
        P3c clus=new P3c();
        try{
            clus.setOptions(new String[]{"-P",String.valueOf(poisson_threshold),
                                         "-A",String.valueOf(chi_square_alpha)});
            clus.buildSubspaceClusterer(insts);
        } catch(Exception e) {
            System.out.println("Exception was caught while Applying p3c");
        }
        List<Cluster> result=clus.getSubspaceClustering();
        //Turn System.out back on again
        System.setOut(out);
        return result.toArray(new Cluster[result.size()]);
    }
    public static i9.subspace.base.Cluster[] proclus(double[][] data, int k,int d) {
        Instances insts = JavaObjectFromDataConverter.Instances_from_matrix(data);
        Proclus clus=new Proclus();
        try{
            clus.setOptions(new String[]{"-K",String.valueOf(k),
                                         "-D",String.valueOf(d)});
            clus.buildSubspaceClusterer(insts);
        } catch(Exception e) {
            System.out.println("Exception was caught while Applying proclus");
        }
        List<Cluster> result=clus.getSubspaceClustering();
        return result.toArray(new Cluster[result.size()]);
    }
    public static i9.subspace.base.Cluster[] subclu(double[][] data,double epsilon,
                                                                    int min_support) {
        Instances insts = JavaObjectFromDataConverter.Instances_from_matrix(data);
        Proclus clus=new Proclus();
        try{
            clus.setOptions(new String[]{"-M",String.valueOf(min_support),
                                         "-E",String.valueOf(epsilon)});
            clus.buildSubspaceClusterer(insts);
        } catch(Exception e) {
            System.out.println("Exception was caught while Applying subclu");
        }
        List<Cluster> result=clus.getSubspaceClustering();
        return result.toArray(new Cluster[result.size()]);
    }
}
