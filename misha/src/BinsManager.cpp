#include "rdbutils.h"
#include "BinsManager.h"

using namespace rdb;

BinsManager::BinsManager(SEXP _breaks, SEXP _include_lowest)
{
	if (!isVector(_breaks))
		TGLError<BinsManager>("Breaks argument must be a vector");

	if (!isLogical(_include_lowest) || length(_include_lowest) != 1)
		TGLError<BinsManager>("include.lowest argument is not logical");

	unsigned num_breaks_sets = length(_breaks);

	m_include_lowest = LOGICAL(_include_lowest)[0];
	m_bin_finders.reserve(num_breaks_sets);
	m_track_mult.resize(num_breaks_sets);
	m_totalbins = 1;

	for (unsigned i = 0; i < num_breaks_sets; ++i) {
		SEXP breaks = VECTOR_ELT(_breaks, i);

		if (!isReal(breaks) && !isInteger(breaks))
			TGLError<BinsManager>("breaks[%d] is not numeric", i + 1);

		m_bin_finders.push_back(BinFinder());

		if (isInteger(breaks)) {
			vector<double> double_breaks(length(breaks));

			for (int i = 0; i < length(breaks); i++)
				double_breaks[i] = INTEGER(breaks)[i];
			m_bin_finders.back().init(double_breaks, m_include_lowest);
		} else
			m_bin_finders.back().init(REAL(breaks), length(breaks), m_include_lowest);

		m_totalbins *= m_bin_finders.back().get_numbins();
		m_track_mult[i] = !i ? 1 : m_track_mult[i - 1] * m_bin_finders[i - 1].get_numbins();
	}
}

void BinsManager::set_dims(SEXP dim, SEXP dimnames) const
{
	for (unsigned i = 0; i < get_num_bin_finders(); i++) {
		const BinFinder &bin_finder = m_bin_finders[i];
		int numbins = bin_finder.get_numbins();
		INTEGER(dim)[i] = numbins;
		SEXP dimname;
		rprotect(dimname = RSaneAllocVector(STRSXP, numbins));

		for (int j = 0; j < numbins; j++) {
			char buf[10000];

			snprintf(buf, sizeof(buf), "%c%g,%g]", j || !m_include_lowest ? '(' : '[', bin_finder.get_breaks()[j], bin_finder.get_breaks()[j + 1]);

			SET_STRING_ELT(dimname, j, mkChar(buf));
		}
		SET_VECTOR_ELT(dimnames, i, dimname);
	}
}
