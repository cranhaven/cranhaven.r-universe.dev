#include <sys/time.h>
#include "cucubes.h"
#include "calc.h"

void nopCallback(int a, int b, int c, int d) {
}

// TODO: check sanity of input
void run_cucubes(
	int n,
	int k,
	int dimension,
	int divisions,
	int discretizations,
	int seed,
	double range,
	double pseudocount,
	double* data,
	int* decision,
	double* IGmax) {

	// TODO Query devices&kernels
	int ts[6] = {0, 0, 512, 64, 32, 8};

	LaunchConfig lc;
	lc.dim = dimension;
	lc.div = divisions;

  // TODO: check what the second alternative means
	if (lc.div > 9 || (lc.dim == 3 && lc.div == 5)) {
		lc.bf = BF_SHIFT;
	} else {
		lc.bf = BF_SPLIT;
	}

	lc.tileSize = ts[dimension];
	lc.disc = discretizations;
	lc.range = range;
	lc.pseudo = pseudocount;
	lc.seed = seed;

	// TODO: remove reduce method altogether
	lc.rm = RM_MAX;

	InputFile inf;
	inf.vars = k;
	inf.objs = n;
	inf.data = data;
	inf.decision = decision;

	Calc c(lc, inf, IGmax, nopCallback);
}

/* class Timer {
	timeval start;
public:
	void reset() {
		gettimeofday(&start, 0);
	}

	float elapsed() {
		timeval end;
		gettimeofday(&end, 0);
		float sec = end.tv_sec - start.tv_sec;
		float usec = end.tv_usec - start.tv_usec;
		return sec + (usec / 1000000.0);
	}

	float remaining(uint32_t p, uint32_t w) {
		float el = elapsed();
		return p ? el * float(w - p) / float(p) : 0.0;
	}
};*/

/*void printCallback(int a, int b, int c, int d) {
	static Timer timer[2];
	static int e[2], r[2];

	int p[2] = {a, c};
	int w[2] = {b, d};

	for (int i = 0; i < 2; i++) {
		if (p[i] <= 1) timer[i].reset();

		if (p[i] < w[i]) {
			e[i] = int(timer[i].elapsed());
			r[i] = int(timer[i].remaining(p[i], w[i]));
		}
	}

	if (a > 1 && c > 1) {
		for (int i = 0; i < 2; i++) {
			if (i == 0) REprintf("\rDiscretization:   ");
			else        REprintf(", IGmax: ");

			REprintf("%3.1lu%% [%.3dh%d%dm%d%ds | -%.3dh%d%dm%d%ds]",
				(100*p[i]/w[i]),
				e[i]/3600, ((e[i]/60)%60)/10, ((e[i]/60)%60)%10, (e[i]%60)/10, (e[i]%60)%10,
				r[i]/3600, ((r[i]/60)%60)/10, ((r[i]/60)%60)%10, (r[i]%60)/10, (r[i]%60)%10);
		}
	}
	if (c == d) REprintf("\n");
}*/
