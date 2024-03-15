#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"
#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP giterator_intervals(SEXP _expr, SEXP _intervals, SEXP _iterator_policy, SEXP _band, SEXP _intervals_set_out, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_expr) || length(_expr) != 1)
			verror("Tracks expression argument must be a string");

		if (!isNull(_intervals_set_out) && (!isString(_intervals_set_out) || length(_intervals_set_out) != 1))
			verror("intervals.set.out argument is not a string");

		string intervset_out = isNull(_intervals_set_out) ? "" : CHAR(STRING_ELT(_intervals_set_out, 0));

		IntervUtils iu(_envir);
		GIntervalsFetcher1D *intervals1d = NULL;
		GIntervalsFetcher2D *intervals2d = NULL;
		iu.convert_rintervs(_intervals, &intervals1d, &intervals2d);
		unique_ptr<GIntervalsFetcher1D> intervals1d_guard(intervals1d);
		unique_ptr<GIntervalsFetcher2D> intervals2d_guard(intervals2d);
		intervals1d->sort();
		intervals1d->unify_overlaps();
		intervals2d->sort();
		intervals2d->verify_no_overlaps(iu.get_chromkey());

		SEXP answer = R_NilValue;
		TrackExprScanner scanner(iu);
		scanner.begin(_expr, intervals1d, intervals2d, _iterator_policy, _band);

		if (intervset_out.empty()) {
			if (scanner.get_iterator()->is_1d()) {
				GIntervals res_intervs;
				for (; !scanner.isend(); scanner.next()) {
					res_intervs.push_back(scanner.last_interval1d());
					iu.verify_max_data_size(res_intervs.size(), "Result");
				}
				answer = iu.convert_intervs(&res_intervs);
			} else {
				GIntervals2D res_intervs;
				for (; !scanner.isend(); scanner.next()) {
					res_intervs.push_back(scanner.last_interval2d());
					iu.verify_max_data_size(res_intervs.size(), "Result");
				}
				answer = iu.convert_intervs(&res_intervs);
			}
		} else {
			char error_prefix[1000];

			if (scanner.get_iterator()->is_1d()) {
				GIntervals res_intervals;
				vector<GIntervalsBigSet1D::ChromStat> chromstats;

				GIntervalsBigSet1D::begin_save(intervset_out.c_str(), iu, chromstats);
				for (; !scanner.isend(); scanner.next()) {
					const GInterval &interval = scanner.last_interval1d();

					if (res_intervals.empty())
						snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chrom %s",
								 intervset_out.c_str(), iu.id2chrom(interval.chromid).c_str());
					else if (res_intervals.front().chromid != interval.chromid)
						GIntervalsBigSet1D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals, iu, chromstats);

					res_intervals.push_back(interval);
					iu.verify_max_data_size(res_intervals.size(), error_prefix);
				}

				GIntervalsBigSet1D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals, iu, chromstats);
				GIntervalsBigSet1D::end_save_plain_intervals(intervset_out.c_str(), iu, chromstats);
			} else {
				GIntervals2D res_intervals;
				vector<GIntervalsBigSet2D::ChromStat> chromstats;

				GIntervalsBigSet2D::begin_save(intervset_out.c_str(), iu, chromstats);
				for (; !scanner.isend(); scanner.next()) {
					const GInterval2D &interval = scanner.last_interval2d();

					if (res_intervals.empty())
						snprintf(error_prefix, sizeof(error_prefix), "Big intervals set %s, chroms (%s, %s)",
								intervset_out.c_str(), iu.id2chrom(interval.chromid1()).c_str(), iu.id2chrom(interval.chromid2()).c_str());
					else if (!res_intervals.front().is_same_chrom(interval))
						GIntervalsBigSet2D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals, iu, chromstats);

					res_intervals.push_back(interval);
					iu.verify_max_data_size(res_intervals.size(), error_prefix);
				}

				GIntervalsBigSet2D::save_chrom_plain_intervals(intervset_out.c_str(), res_intervals, iu, chromstats);
				GIntervalsBigSet2D::end_save_plain_intervals(intervset_out.c_str(), iu, chromstats);
			}
		}

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gcheck_iterator(SEXP _iterator_policy, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		IntervUtils iu(_envir);
		GIntervals intervals1d;
		GIntervals2D intervals2d;
		GIntervals scope1d;
		GIntervals2D scope2d;
		iu.get_all_genome_intervs(scope1d);
		iu.get_all_genome_intervs(scope2d);

		TrackExprScanner scanner(iu);
		scanner.create_expr_iterator(R_NilValue, &scope1d, &scope2d, _iterator_policy, R_NilValue);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
