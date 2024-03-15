#include "port.h"

#include <memory>

#include "GenomeSeqFetch.h"
#include "GenomeTrackFixedBin.h"
#include "GenomeTrackSparse.h"
#include "PssmSet.h"

#include "TrackExpressionFixedBinIterator.h"
#include "TrackExpressionScanner.h"
#include "TrackExpressionVars.h"
#include "rdbinterval.h"
#include "rdbprogress.h"
#include "rdbutils.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gcreate_pwm_energy(SEXP _track, SEXP _pssmset, SEXP _pssmid, SEXP _prior, SEXP _iterator_policy, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");
		const char *track = CHAR(STRING_ELT(_track, 0));

		if (!isString(_pssmset) || length(_pssmset) != 1)
			verror("Pssmset argument is not a string");
		const char *pssmset = CHAR(STRING_ELT(_pssmset, 0));

		int pssmid;
		if (!(isReal(_pssmid) || isInteger(_pssmid)) || length(_pssmid) != 1)
			verror("Pssmid argument is not numeric");
		if (isReal(_pssmid)) {
			if (REAL(_pssmid)[0] != (int)REAL(_pssmid)[0])
				verror("Pssmid is not an integer");
			pssmid = (int)REAL(_pssmid)[0];
		} else
			pssmid = INTEGER(_pssmid)[0];

		if (!isReal(_prior) || length(_prior) != 1)
			verror("Prior argument is not numeric");
		double prior = REAL(_prior)[0];

		PssmSet pssm_set;
		char pssmkey_filename[PATH_MAX];
		char pssmdata_filename[PATH_MAX];
		char seqdir[PATH_MAX];
		const char *groot_str = get_groot(_envir);

		snprintf(pssmkey_filename, sizeof(pssmkey_filename), "%s/pssms/%s.key", groot_str, pssmset);
		snprintf(pssmdata_filename, sizeof(pssmdata_filename), "%s/pssms/%s.data", groot_str, pssmset);
		snprintf(seqdir, sizeof(seqdir), "%s/seq", get_groot(_envir));
		pssm_set.read(pssmkey_filename, pssmdata_filename, prior);

		DnaPSSM &pssm = pssm_set.get_pssm(pssmid);
		GenomeSeqFetch seqfetch;
		seqfetch.set_seqdir(seqdir);
		IntervUtils iu(_envir);
		GIntervals scope;
		iu.get_all_genome_intervs(scope);

		vector<char> seq;
		float energy = -1;

		string dirname = create_track_dir(_envir, track);
		GenomeTrackFixedBin fixed_bin_track;
		GenomeTrackSparse sparse_track;
		GInterval cur_interval(-1, -1, -1, -1);

		Progress_reporter progress;
		progress.init(scope.range(), 1000000);

		char filename[PATH_MAX];
		TrackExprScanner scanner(iu);
		TrackExpressionIteratorBase *expr_itr_base = scanner.create_expr_iterator(R_NilValue, &scope, NULL, _iterator_policy, R_NilValue);

		if (!expr_itr_base->is_1d()){
			verror("Iterator type %s is not supported by the function", TrackExpressionIteratorBase::TYPE_NAMES[expr_itr_base->get_type()]);
		}

		TrackExpression1DIterator *expr_itr = (TrackExpression1DIterator *)expr_itr_base;
		GIntervals::const_iterator icur_scope = scope.begin();

		for (; !expr_itr->isend(); expr_itr->next()) {
			const GInterval &last_interval = expr_itr->last_interval();

			if (cur_interval.chromid != last_interval.chromid) {
				// some of the chromosomes could be skipped by the iterator; we still must create them even if they are empty
				if (cur_interval.chromid != -1) 
					++icur_scope;
				while (icur_scope->chromid != last_interval.chromid) {
					if (expr_itr->get_type() == TrackExpressionIteratorBase::INTERVALS1D) {
						snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), iu.id2chrom(icur_scope->chromid).c_str());
						sparse_track.init_write(filename, icur_scope->chromid);
					}
					progress.report(icur_scope->end - icur_scope->start);
					++icur_scope;
					if (icur_scope == scope.end())
						verror("Failed to find chromid %d\n", last_interval.chromid);
				}

				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), iu.id2chrom(last_interval.chromid).c_str());

				if (expr_itr->get_type() == TrackExpressionIteratorBase::FIXED_BIN)
					fixed_bin_track.init_write(filename, ((TrackExpressionFixedBinIterator *)expr_itr)->get_bin_size(), last_interval.chromid);
				else if (expr_itr->get_type() == TrackExpressionIteratorBase::INTERVALS1D)
					sparse_track.init_write(filename, last_interval.chromid);
				else
					verror("Unrecognized type of iterator");
			}

			GInterval seq_interval(last_interval.chromid, last_interval.start, last_interval.end + pssm.size(), 0);

			if (seq_interval.end > icur_scope->end)
				seq_interval.end = icur_scope->end;

			if (seq_interval.end - seq_interval.start >= pssm.size()) {
				seqfetch.read_interval(seq_interval, iu.get_chromkey(), seq);
				pssm.integrate_like_seg(&*seq.begin(), &*(seq.end() - pssm.size()), energy);
			} else
				energy = numeric_limits<double>::quiet_NaN();

			if (expr_itr->get_type() == TrackExpressionIteratorBase::FIXED_BIN)
				fixed_bin_track.write_next_bin(energy);
			else
				sparse_track.write_next_interval(last_interval, energy);

			progress.report(cur_interval.chromid == last_interval.chromid ? last_interval.end - cur_interval.end : last_interval.end);
			cur_interval = last_interval;
			check_interrupt();
		}

		if (expr_itr->get_type() == TrackExpressionIteratorBase::INTERVALS1D) {
			// some of the chromosome could be skipped by the iterator; we still must create them even if they are empty
			for (++icur_scope; icur_scope != scope.end(); ++icur_scope) {
				snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), iu.id2chrom(icur_scope->chromid).c_str());
				sparse_track.init_write(filename, icur_scope->chromid);
				progress.report(icur_scope->end - icur_scope->start);
			}
		}

		progress.report_last();
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP gcreate_pwm_energy_multitask(SEXP _track, SEXP _pssmset, SEXP _pssmid, SEXP _prior, SEXP _iterator_policy, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");
		const char *track = CHAR(STRING_ELT(_track, 0));

		if (!isString(_pssmset) || length(_pssmset) != 1)
			verror("Pssmset argument is not a string");
		const char *pssmset = CHAR(STRING_ELT(_pssmset, 0));

		int pssmid;
		if (!(isReal(_pssmid) || isInteger(_pssmid)) || length(_pssmid) != 1)
			verror("Pssmid argument is not numeric");
		if (isReal(_pssmid)) {
			if (REAL(_pssmid)[0] != (int)REAL(_pssmid)[0])
				verror("Pssmid is not an integer");
			pssmid = (int)REAL(_pssmid)[0];
		} else
			pssmid = INTEGER(_pssmid)[0];

		if (!isReal(_prior) || length(_prior) != 1)
			verror("Prior argument is not numeric");
		double prior = REAL(_prior)[0];

		PssmSet pssm_set;
		char pssmkey_filename[PATH_MAX];
		char pssmdata_filename[PATH_MAX];
		char seqdir[PATH_MAX];
		const char *groot_str = get_groot(_envir);

		snprintf(pssmkey_filename, sizeof(pssmkey_filename), "%s/pssms/%s.key", groot_str, pssmset);
		snprintf(pssmdata_filename, sizeof(pssmdata_filename), "%s/pssms/%s.data", groot_str, pssmset);
		snprintf(seqdir, sizeof(seqdir), "%s/seq", get_groot(_envir));
		pssm_set.read(pssmkey_filename, pssmdata_filename, prior);

		DnaPSSM &pssm = pssm_set.get_pssm(pssmid);
		GenomeSeqFetch seqfetch;
		seqfetch.set_seqdir(seqdir);
		IntervUtils iu(_envir);
		GIntervals scope;
		iu.get_all_genome_intervs(scope);

		vector<char> seq;
		float energy = -1;

		string dirname = create_track_dir(_envir, track);

		if (!iu.prepare4multitasking(&scope, NULL))
			rreturn(R_NilValue);

		if (iu.distribute_task(0, 0)) { // child process
			GIntervals *kid_intervals1d = (GIntervals *)iu.get_kid_intervals1d();
			GenomeTrackFixedBin fixed_bin_track;
			GenomeTrackSparse sparse_track;
			GInterval cur_interval(-1, -1, -1, -1);
			char filename[PATH_MAX];

			Progress_reporter progress;
			progress.init(kid_intervals1d->range(), 1000000);

			TrackExprScanner scanner(iu);
			TrackExpressionIteratorBase *expr_itr_base = scanner.create_expr_iterator(R_NilValue, kid_intervals1d, NULL, _iterator_policy, R_NilValue);

			if (!expr_itr_base->is_1d()) {
				verror("Iterator type %s is not supported by the function", TrackExpressionIteratorBase::TYPE_NAMES[expr_itr_base->get_type()]);
			}

			TrackExpression1DIterator *expr_itr = (TrackExpression1DIterator *)expr_itr_base;
			GIntervals::const_iterator icur_scope = kid_intervals1d->begin();

			for (; !expr_itr->isend(); expr_itr->next()) {
				const GInterval &last_interval = expr_itr->last_interval();

				if (cur_interval.chromid != last_interval.chromid) {
					// some of the chromosomes could be skipped by the iterator; we still must create them even if they are empty
					if (cur_interval.chromid != -1) 
						++icur_scope;
					while (icur_scope->chromid != last_interval.chromid) {
						if (expr_itr->get_type() == TrackExpressionIteratorBase::INTERVALS1D) {
							snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), iu.id2chrom(icur_scope->chromid).c_str());
							sparse_track.init_write(filename, icur_scope->chromid);
						}
						progress.report(icur_scope->end - icur_scope->start);
						++icur_scope;
						if (icur_scope == kid_intervals1d->end())
							verror("Failed to find chromid %d\n", last_interval.chromid);
					}

					snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), iu.id2chrom(last_interval.chromid).c_str());

					if (expr_itr->get_type() == TrackExpressionIteratorBase::FIXED_BIN)
						fixed_bin_track.init_write(filename, ((TrackExpressionFixedBinIterator *)expr_itr)->get_bin_size(), last_interval.chromid);
					else if (expr_itr->get_type() == TrackExpressionIteratorBase::INTERVALS1D)
						sparse_track.init_write(filename, last_interval.chromid);
					else
						verror("Unrecognized type of iterator");
				}

				GInterval seq_interval(last_interval.chromid, last_interval.start, last_interval.end + pssm.size(), 0);

				if (seq_interval.end > icur_scope->end)
					seq_interval.end = icur_scope->end;

				if (seq_interval.end - seq_interval.start >= pssm.size()) {
					seqfetch.read_interval(seq_interval, iu.get_chromkey(), seq);
					pssm.integrate_like_seg(&*seq.begin(), &*(seq.end() - pssm.size()), energy);
				} else
					energy = numeric_limits<double>::quiet_NaN();

				if (expr_itr->get_type() == TrackExpressionIteratorBase::FIXED_BIN)
					fixed_bin_track.write_next_bin(energy);
				else
					sparse_track.write_next_interval(last_interval, energy);

				progress.report(cur_interval.chromid == last_interval.chromid ? last_interval.end - cur_interval.end : last_interval.end);
				cur_interval = last_interval;
				check_interrupt();
			}

			if (expr_itr->get_type() == TrackExpressionIteratorBase::INTERVALS1D) {
				// some of the chromosome could be skipped by the iterator; we still must create them even if they are empty
				for (++icur_scope; icur_scope != kid_intervals1d->end(); ++icur_scope) {
					snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), iu.id2chrom(icur_scope->chromid).c_str());
					sparse_track.init_write(filename, icur_scope->chromid);
					progress.report(icur_scope->end - icur_scope->start);
				}
			}

			progress.report_last();
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}
