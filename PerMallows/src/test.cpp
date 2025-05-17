#include "Cayley.h"
#include "Kendall.h"
#include "Hamming.h"
#include "Ulam_disk.h"
#include "Ulam.h"
#include "Newton_raphson.h"
#include "Exponential_model.h"
#include "Lap.h"
#include "Generic.h"


#include "Cayley.h"
#include "Kendall.h"
#include "Hamming.h"
#include "Ulam_disk.h"
#include "Ulam.h"
#include "Newton_raphson.h"
#include "Exponential_model.h"
#include "Lap.h"

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <Rdefines.h>


using namespace std;

int main (int argc, char *argv[]) { 
  int n = 4;
  int dist_id = HAMMING_DISTANCE;
  Generic gen;
  Exponential_model * exp_mod = gen.new_instance(dist_id, n); ;
  //ham->maximum_distance();
  //cout<<"RRRRR"<<endl;
  delete exp_mod; 
  return 0;
  
  }
