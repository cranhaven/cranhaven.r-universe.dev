#include <progress.hpp>
#include <progress_bar.hpp>

//[[Rcpp::export]]
void pbClean() {
  bool disp = false;
  int num = 1;
  Progress prog( num, disp);
  prog.cleanup();
}

//[[Rcpp::export]]
void test() {

}
