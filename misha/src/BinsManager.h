#ifndef BINSMANAGER_H_
#define BINSMANAGER_H_

#include <cmath>
#include <vector>

#include "BinFinder.h"
#include "TGLException.h"

#include <R.h>
#include <Rinternals.h>

using namespace std;

class BinsManager {
public:
	BinsManager(SEXP breaks, SEXP include_lowest);

	// returns -1 if any of the values does not fall into a bin
	int vals2idx(const vector<double> &vals) const;

	void set_dims(SEXP dim, SEXP dimnames) const;

	bool             get_include_lowest() const { return m_include_lowest; }
	unsigned         get_total_bins() const { return m_totalbins; }
	unsigned         get_num_bin_finders() const { return m_bin_finders.size(); }
	const BinFinder &get_bin_finder(int idx) const { return m_bin_finders[idx]; }

private:
	vector<BinFinder> m_bin_finders;
	vector<unsigned>  m_track_mult;
	unsigned          m_totalbins;
	bool              m_include_lowest;
};


//-------------------------------------- IMPLEMENTATION -------------------------------------------

inline int BinsManager::vals2idx(const vector<double> &vals) const
{
	int res = 0;

	for (vector<double>::const_iterator ival = vals.begin(); ival != vals.end(); ++ival) {
		if (std::isnan(*ival))
			return -1;

		int index = ival - vals.begin();
		int bin = m_bin_finders[index].val2bin(*ival);

		if (bin < 0)
			return -1;

		res += bin * m_track_mult[index];
	}

	return res;
}

#endif /* BINSMANAGER_H_ */
