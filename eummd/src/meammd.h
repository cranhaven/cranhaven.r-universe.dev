#ifndef GUARD_meammd_h
#define GUARD_meammd_h

std::vector<double> cpp_meammd_proj_pval_faster(double* X, double* Y, 
                                                int nX, int dX,
                                                int nY, int dY,
                                                int numperm, 
                                                int numproj,
                                                int seednum, 
                                                double beta, 
                                                int twosided, 
                                                int boundedminpval);

double cpp_meammd_dist_pval(double* X, double* Y, 
                        int nX, int dX,
                        int nY, int dY,
                        int numperm, 
                        int seednum, 
                        double beta, 
                        int pmethod,
                        int nmethod,
                        int twosided, 
                        int boundedminpval);


double cpp_meammd_proj_stat(double* X, double* Y, 
                            int nX, int dX,
                            int nY, int dY,
                            int numproj, 
                            int seednum,
                            double beta);


std::vector<double> cpp_meammd_proj_pval(double* X, double* Y, 
                        int nX, int dX,
                        int nY, int dY,
                        int numperm, 
                        int numproj,
                        int seednum, 
                        double beta);


#endif

