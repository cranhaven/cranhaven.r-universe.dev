#include <cstdint>
#include "rdbinterval.h"
#include "rdbutils.h"
#include "BinsManager.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP C_gcis_decay(SEXP _expr, SEXP _breaks, SEXP _src_intervals, SEXP _domain_intervals, SEXP _intervals,
				SEXP _include_lowest, SEXP _iterator_policy, SEXP _band, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_expr) || length(_expr) != 1)
			verror("Track argument is not a character string");

		if (!isReal(_breaks))
			verror("Breaks argument is not a number");

		if (!isLogical(_include_lowest) || length(_include_lowest) != 1)
			verror("include.lowest argument is not logical");

		BinFinder bin_finder(REAL(_breaks), length(_breaks), LOGICAL(_include_lowest)[0]);

		IntervUtils iu(_envir);
		TrackExprScanner scanner(iu);
		iu.verify_max_data_size(bin_finder.get_numbins(), "Result");
		vector<uint64_t> inter_domain_dist(bin_finder.get_numbins(), 0);
		vector<uint64_t> intra_domain_dist(bin_finder.get_numbins(), 0);

		// divide src and domain intervals to chromosomes (this will make the search faster)
		vector<GIntervals> src_intervals(iu.get_chromkey().get_num_chroms());
		vector<GIntervals> domain_intervals(iu.get_chromkey().get_num_chroms());

		{
			GIntervals tmp_intervals;
			iu.convert_rintervs(_src_intervals, &tmp_intervals, NULL, false, NULL, "Source intervals: ");
			tmp_intervals.sort();
			tmp_intervals.unify_overlaps(true);
			for (GIntervals::const_iterator iinterv = tmp_intervals.begin(); iinterv != tmp_intervals.end(); ++iinterv) 
				src_intervals[iinterv->chromid].push_back(*iinterv);
		}

		{
			GIntervals tmp_intervals;
			iu.convert_rintervs(_domain_intervals, &tmp_intervals, NULL, false, NULL, "Domain intervals: ");
			tmp_intervals.sort();
			tmp_intervals.verify_no_overlaps(iu.get_chromkey(), "Domain intervals: ");
			for (GIntervals::const_iterator iinterv = tmp_intervals.begin(); iinterv != tmp_intervals.end(); ++iinterv) 
				domain_intervals[iinterv->chromid].push_back(*iinterv);
		}

		GIntervalsFetcher2D *initial_scope = NULL;
		iu.convert_rintervs(_intervals, NULL, &initial_scope);
		unique_ptr<GIntervalsFetcher2D> initial_scope_guard(initial_scope);

		set<ChromPair> chrompairs_mask;
		GIntervals2D all_genome_intervs2d;
		iu.get_all_genome_intervs(all_genome_intervs2d);

		// exclude trans chromosomes from scope
		for (GIntervals2D::const_iterator iinterval = all_genome_intervs2d.begin(); iinterval != all_genome_intervs2d.end(); ++iinterval) {
			if (iinterval->chromid1() == iinterval->chromid2()) 
				chrompairs_mask.insert(ChromPair(iinterval->chromid1(), iinterval->chromid2()));
		}

		unique_ptr<GIntervalsFetcher2D> scope(initial_scope->create_masked_copy(chrompairs_mask));
		scope->sort();
		scope->verify_no_overlaps(iu.get_chromkey());

		if (iu.get_multitasking()) {
			if (!iu.prepare4multitasking(_expr, NULL, scope.get(), _iterator_policy))
				rreturn(R_NilValue);

			if (iu.distribute_task(2 * bin_finder.get_numbins() * sizeof(uint64_t), 0)) { // child process
				uint64_t *distribution = (uint64_t *)allocate_res(0);
				uint64_t *intra_domain_dist = distribution;
				uint64_t *inter_domain_dist = distribution + bin_finder.get_numbins();
				memset(distribution, 0, 2 * bin_finder.get_numbins() * sizeof(uint64_t));

				for (scanner.begin(_expr, NULL, iu.get_kid_intervals2d(), _iterator_policy, _band); !scanner.isend(); scanner.next()) {
					const GInterval2D &interv = scanner.last_interval2d();
					GInterval contact1(interv.chromid1(), interv.start1(), interv.end1(), 0);
					GInterval contact2(interv.chromid2(), interv.start2(), interv.end2(), 0);

					if (!src_intervals[contact1.chromid].containing_interval(contact1)) 
						continue;

					const GInterval *domain_interv1 = domain_intervals[contact1.chromid].containing_interval(contact1);
					const GInterval *domain_interv2 = domain_intervals[contact2.chromid].containing_interval(contact2);

					// distance between contacts is defined as distance between the intervals centers
					int64_t distance = llabs((contact1.start + contact1.end - contact2.start - contact2.end) / 2);
					int index = bin_finder.val2bin(distance);

					if (index >= 0) {
						if (domain_interv1 && domain_interv1 == domain_interv2)
							intra_domain_dist[index]++;
						else
							inter_domain_dist[index]++;
					}
				}
				rreturn(R_NilValue);
			} else {  // parent process
				// collect results from kids
				for (int i = 0; i < get_num_kids(); ++i) {
					uint64_t *kid_distribution = (uint64_t *)get_kid_res(i);
					uint64_t *kid_intra_domain_dist = kid_distribution;
					uint64_t *kid_inter_domain_dist = kid_distribution + bin_finder.get_numbins();

					for (uint64_t ibin = 0; ibin < bin_finder.get_numbins(); ++ibin) {
						intra_domain_dist[ibin] += kid_intra_domain_dist[ibin];
						inter_domain_dist[ibin] += kid_inter_domain_dist[ibin];
					}
				}
			}
		} else {   // no multitasking
			for (scanner.begin(_expr, NULL, scope.get(), _iterator_policy, _band); !scanner.isend(); scanner.next()) {
				const GInterval2D &interv = scanner.last_interval2d();
				GInterval contact1(interv.chromid1(), interv.start1(), interv.end1(), 0);
				GInterval contact2(interv.chromid2(), interv.start2(), interv.end2(), 0);

				if (!src_intervals[contact1.chromid].containing_interval(contact1)) 
					continue;

				const GInterval *domain_interv1 = domain_intervals[contact1.chromid].containing_interval(contact1);
				const GInterval *domain_interv2 = domain_intervals[contact2.chromid].containing_interval(contact2);

				// distance between contacts is defined as distance between the intervals centers
				int64_t distance = llabs((contact1.start + contact1.end - contact2.start - contact2.end) / 2);
				int index = bin_finder.val2bin(distance);

				if (index >= 0) {
					if (domain_interv1 && domain_interv1 == domain_interv2)
						intra_domain_dist[index]++;
					else
						inter_domain_dist[index]++;
				}
			}
		}

		// pack the answer
		SEXP answer, dim, dimnames, dimname;
		rprotect(answer = RSaneAllocVector(REALSXP, 2 * bin_finder.get_numbins()));
		double *panswer = REAL(answer);

		for (unsigned i = 0; i < bin_finder.get_numbins(); i++) {
			panswer[i] = intra_domain_dist[i];
			panswer[i + bin_finder.get_numbins()] = inter_domain_dist[i];
		}

		rprotect(dim = RSaneAllocVector(INTSXP, 2));
		rprotect(dimnames = RSaneAllocVector(VECSXP, 2));

		SEXP breaks_sets;
		rprotect(breaks_sets = RSaneAllocVector(VECSXP, 1));
		SET_VECTOR_ELT(breaks_sets, 0, _breaks);

		BinsManager bins_manager(breaks_sets, _include_lowest);
		bins_manager.set_dims(dim, dimnames);
		INTEGER(dim)[1] = 2;

		rprotect(dimname = RSaneAllocVector(STRSXP, 2));
		SET_STRING_ELT(dimname, 0, mkChar("intra"));
		SET_STRING_ELT(dimname, 1, mkChar("inter"));
		SET_VECTOR_ELT(dimnames, 1, dimname);

		setAttrib(answer, R_DimSymbol, dim);
		setAttrib(answer, R_DimNamesSymbol, dimnames);
		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}
