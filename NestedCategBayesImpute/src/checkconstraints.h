//  checkconstraints.h
#include <Rcpp.h>
using namespace Rcpp;
#define HEAD 1
#define SPOUSE 2
#define BIOLOGICALCHILD 3
#define ADOPTEDCHILD 4
#define STEPCHILD 5
#define SIBLING 6
#define PARENT 7
#define GRANDCHILD 8
#define PARENTINLAW 9
#define CHILDINLAW 10

#define COL 3

#define GENDER 0
#define AGE 3
#define RELATE 4

#define GRAINSIZE 1000

int checkconstraints_imp_HHhead_at_group_level(int *data, int *isPossible,int hh_size, int DIM, int nHouseholds, int Parallel);
List checkconstraints_HHhead_at_group_level(IntegerMatrix data,int neededpossiblehh, int hh_size, int Parallel);

int checkconstraints_imp(int *data, int *isPossible,int hh_size, int DIM, int nHouseholds);
List checkconstraints(IntegerMatrix data,int neededpossiblehh, int hh_size);

int isValid(int *datah, int hh_size);
