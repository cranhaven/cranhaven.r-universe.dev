#include "local_f2c.h"
#include <time.h>

int timer_(doublereal *ttime) {

    clock_t t = clock();
    *ttime = (doublereal) (t) / CLOCKS_PER_SEC;
    return 0;
}

