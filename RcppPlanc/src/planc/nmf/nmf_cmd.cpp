#include "parsecommandline.hpp"
#include "nmf_lib.hpp"
#include <cstdio>


int main(int argc, char *argv[]) {
//  try {
    planc::ParseCommandLine dnd;
    auto libstate = planc::nmflib();
    int status = libstate.runNMF<arma::sp_mat>(dnd.getPlancParams(argc, argv));
    fflush(stdout);
    return status;
//  } catch (const std::exception &e) {
//    INFO << "Exception with stack trace " << std::endl;
//    INFO << e.what();
//  }
}
