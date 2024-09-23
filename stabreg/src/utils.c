#include "utils.h"



double factorial(int n){
    double res = 1;
    if(n > 1){
        for(int i = 1; i != n; ++i)
            res *= (i + 1);
    }
    return res;
}



double d_abs_diff(double a, double b){
    double out = 0.0;
    if(a > b)
        out = a - b;
    else if(a < b)
        out = b - a;
    return out;
}


void set_to_zero(double* x, int x_len){
    for(int i = 0; i != x_len; ++i)
        x[i] = 0.0;
}
