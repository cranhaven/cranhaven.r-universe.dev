#include <R.h>

void auc_trapezoid(int *ndata, double *predictions, int *labels, double *result) {
	double *thresholds = (double*)R_alloc(*ndata, sizeof(double));
	int *index = (int*)R_alloc(*ndata, sizeof(int));
	int bkg_acc, sig_acc;
	int i;

	memcpy(thresholds, predictions, *ndata*sizeof(double)); /* class 0 predictions */
	for (i = 0; i < *ndata; i++) {
		index[i] = i;
	}
	rsort_with_index(thresholds, index, *ndata);
	*result = 0.0;
	sig_acc = bkg_acc = 0;
	for ( i = 0; i < *ndata; ) {
		int prev_sig_acc = sig_acc;
		int prev_bkg_acc = bkg_acc;
		double cur_thr = thresholds[i];
		do {
			if (labels[index[i]] == 0) {
				++bkg_acc;
			} else {
				++sig_acc;
			}
		} while (++i < *ndata && thresholds[i] == cur_thr);
		if (bkg_acc > prev_bkg_acc) {
			*result += (((double)(sig_acc+prev_sig_acc))/2) * (bkg_acc-prev_bkg_acc);
		}
	}
	*result /= bkg_acc*sig_acc;
}

