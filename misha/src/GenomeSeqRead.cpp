#include <cstdint>
#include "GIntervalsBigSet1D.h"
#include "GenomeSeqFetch.h"
#include "rdbinterval.h"
#include "rdbutils.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gseqread(SEXP _intervals, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		IntervUtils iu(_envir);
		GIntervalsFetcher1D *intervals = NULL;
		iu.convert_rintervs(_intervals, &intervals, NULL);
		unique_ptr<GIntervalsFetcher1D> intervals_guard(intervals);
		intervals->sort();

		if (!intervals->size())
			return R_NilValue;

		vector<char> buf;
		SEXP answer;
		rprotect(answer = RSaneAllocVector(STRSXP, intervals->size()));

		uint64_t seqlen = 0;
		GenomeSeqFetch seqfetch;
		seqfetch.set_seqdir(string(rdb::get_groot(_envir)) + "/seq");

		for (intervals->begin_iter(); !intervals->isend(); intervals->next()) {
			seqfetch.read_interval(intervals->cur_interval(), iu.get_chromkey(), buf);
			seqlen += buf.size();
			iu.verify_max_data_size(seqlen, "Result sequence");
			buf.push_back(0);
			SET_STRING_ELT(answer, iu.get_orig_interv_idx(intervals->cur_interval()), mkChar(&*buf.begin()));
			check_interrupt();
		}
		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
